{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ReservationAPISpec where

import Control.Monad
import Control.Monad.Trans.Free
import Control.Monad.IO.Class
import Data.List
import Data.Foldable
import Data.Functor.Sum
import Data.UUID
import Data.IORef
import Data.Time.Calendar
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Aeson (encode, decode)
import Network.HTTP.Types (methodGet, methodPost)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Test
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Test.HUnit.Base hiding (Test, Testable)
import Test.QuickCheck hiding (tables)
import Test.QuickCheck.Instances.Time ()
import Test.QuickCheck.Instances.UUID ()
import ReservationAPI
import Servant
import API

reservationAPITests :: [Test]
reservationAPITests = [
  testGroup "Remove non-overlapping reservations" [
    testProperty "returns reservation if already reserved" $ \
    (Positive sd) (AnyReservation r) -> do
      let actual = removeNonOverlappingReservations sd [r] r
      [r] === actual
    ,
    testProperty "returns no reservations that start a seating duration after the reservation" $ \
      (Positive sd) (fmap getValidReservation -> rs) (ValidReservation r) -> do
      let actual = removeNonOverlappingReservations sd rs r
      False ===
        any (\x -> addLocalTime sd (reservationDate r) < reservationDate x) actual
    ,
    testProperty "returns no reservations that end a seating duration before the reservation" $ \
      (Positive sd) (fmap getValidReservation -> rs) (ValidReservation r) -> do
      let actual = removeNonOverlappingReservations sd rs r
      False ===
        any (\x -> addLocalTime sd (reservationDate x) < reservationDate r) actual
  ]
  ,
  testGroup "Accommodate" $ [
    testProperty "rejects any reservation when restaurant has no tables" $ \
      (tables :: [Int]) q -> do
      let actual = canAccommodate [] tables q
      False === actual
    ,
    testProperty "accepts reservation when table is available" $ \
      (q :: Word) -> do
      let actual = canAccommodate [q] [] q
      True === actual
    ,
    testProperty "rejects reservation when table is already taken" $ \
      (Positive (q :: Integer)) -> do
      let reservations = [q]
      let actual = canAccommodate [q] reservations q
      False === actual
    ,
    testProperty "rejects reservation over total capacity" $ \
      (fmap getPositive -> resources :: [Int]) (Positive i) -> do
      let q = sum resources + i
      let actual = canAccommodate resources [] q
      False === actual
    ,
    testProperty "rejects any reservation when sold out" $ \
      (fmap getPositive -> resources :: [Int]) (Positive q) -> do
      let reservations = resources
      let actual = canAccommodate resources reservations q
      False === actual
    ,
    testProperty "rejects any reservation larger than the biggest table" $ \
      (fmap getPositive . getNonEmpty -> resources :: [Int]) (Positive i) -> do
      let biggestTable = maximum resources
      let actual = canAccommodate resources [] $ i + biggestTable
      False === actual
    ] ++ 
    hUnitTestToTests ("responds correctly in specific scenarios" ~: do
      (tables :: [Int], reservations, q, expected) <-
        [
          ([1, 1], [], 2, False),
          ([2, 2], [2], 2, True),
          ([3, 2], [3], 2, True),
          ([2, 4], [3], 3, False),
          ([2, 4], [3], 2, True),
          ([2, 4], [4], 3, False),
          ([2, 4], [4], 2, True),
          ([2, 4], [2], 3, True),          
          ([4, 2], [3], 3, False),
          ([4, 2], [3], 2, True),
          ([4, 2], [4], 3, False),
          ([4, 2], [4], 2, True),
          ([4, 2], [2], 3, True),
          ([4, 2], [2, 3], 1, False),
          ([4, 4, 2], [3, 2], 1, True),
          ([4, 4, 2], [3, 2], 4, True),
          ([4, 4, 2], [3, 2], 5, False)
        ]
      let actual = canAccommodate tables reservations q
      return $ expected ~=? actual
    ) ++ [
    testProperty "canAccommodateReservation returns correct result" $ \
      (fmap getValidTable -> tables) (fmap getValidReservation -> rs) (ValidReservation r) -> do
      let actual = canAccommodateReservation tables rs r

      let expected = canAccommodate
                      (tableSeats <$> tables)
                      (reservationQuantity <$> rs)
                      (reservationQuantity r)
      expected === either (const False) (const True) actual
    ]
  ,
  testGroup "Reservation JSON" $ 
    hUnitTestToTests (TestLabel "renders correctly" $ do
      let rid = fromWords 872411231 2362592316 2161598850 3450687078
      let rd = LocalTime (fromGregorian 2019 4 12) (TimeOfDay 19 0 0)

      let json = encode $ Reservation rid rd "Jo" "j@example.com" 3

      json ~?= "{\"id\":\"33fff05f-8cd2-4c3c-80d7-6182cdad4e66\",\
                \\"date\":\"2019-04-12T19:00:00\",\
                \\"name\":\"Jo\",\
                \\"email\":\"j@example.com\",\
                \\"quantity\":3}"
    ) ++ [
    testProperty "round-trips" $ \(AnyReservation r) -> do
      let json = encode r
      let actual = decode json
      Just r === actual
  ]
  ,
  testGroup "/reservations/" [
    testProperty "responds with 404 when no reservation exists" $ withApp <$> \
      rid -> do
      actual <- get $ "/reservations/" <> toASCIIBytes rid
      assertStatus 404 actual
    ,
    testProperty "responds with 200 after reservation is added" $ withApp <$> \
      (ValidReservation r) -> do
      _ <- postJSON "/reservations" $ encode r
      actual <- get $ "/reservations/" <> toASCIIBytes (reservationId r)
      assertStatus 200 actual
    ,
    testProperty "succeeds when valid reservation is POSTed" $ withApp <$> \
      (ValidReservation r) -> do
      actual <- postJSON "/reservations" $ encode r
      assertStatus 200 actual
    ,
    testProperty "fails when reservation is POSTed with the nil UUID" $ withApp <$> \
      (ValidReservation r) -> do
      let invalid = r { reservationId = nil }
      actual <- postJSON "/reservations" $ encode invalid
      assertStatus 400 actual
    ,
    testProperty "fails when reservation is POSTed with invalid quantity" $ withApp <$> \
      (ValidReservation r, NonNegative q) -> do
      let invalid = r { reservationQuantity = negate q }
      actual <- postJSON "/reservations" $ encode invalid
      assertStatus 400 actual
    ,
    testProperty "fails when past reservation is POSTed" $ withApp <$> \
      (ValidReservation r, Positive diffTime) -> do
      let invalid =
            r { reservationDate = addLocalTime (negate diffTime) now2019 }
      actual <- postJSON "/reservations" $ encode invalid
      assertStatus 400 actual
    ,
    testProperty "fails when reservation beyond table capacity is POSTed" $ withApp <$> \
      (ValidReservation r, Positive i) -> do
      let largestTableSize = maximum $ tableSeats <$> theTables
      let invalid = r { reservationQuantity = i + largestTableSize }

      actual <- postJSON "/reservations" $ encode invalid

      assertStatus 500 actual
    ,
    testProperty "succeeds fully booking the restaurant" $ withApp <$> \
      (FutureTime d, InfiniteList ids _, InfiniteList validReservations _) -> do
      let reserve (NonNilUUID rid) (ValidReservation res) (Table t) =
            res {
              reservationId = rid,
              reservationQuantity = t,
              reservationDate = d }
      let rids = take (length theTables) $ nub ids
      let rs = zipWith3 reserve rids validReservations theTables

      statuses <- traverse (postJSON "/reservations" . encode) rs

      return $ all (statusIsSuccessful . simpleStatus) statuses
    ,
    testProperty "fails when restaurant is fully booked" $ withApp <$> \
      (InfiniteList ids _, InfiniteList validReservations _, ValidReservation r) -> do
      let reserve (NonNilUUID rid) (ValidReservation res) (Table t) =
            res {
              reservationId = rid,
              reservationQuantity = t,
              reservationDate = reservationDate r }
      let rids =
            take (length theTables) $
            filter (/= (NonNilUUID $ reservationId r)) $
            nub ids
      let rs = zipWith3 reserve rids validReservations theTables
      traverse_ (postJSON "/reservations" . encode) rs

      actual <- postJSON "/reservations" $ encode $ r { reservationQuantity = 1 }

      assertStatus 500 actual
    ,
    testProperty "fails when restaurant is fully booked, but reservation is for a little later" $ withApp <$> \
      (FutureTime d, InfiniteList ids _, InfiniteList validReservations _, ValidReservation r) -> do
      let reserve (NonNilUUID rid) (ValidReservation res) (Table t) =
            res {
              reservationId = rid,
              reservationQuantity = t,
              reservationDate = d }
      let rids = filter (/= (NonNilUUID $ reservationId r)) $ nub ids
      let rs = zipWith3 reserve rids validReservations theTables
      traverse_ (postJSON "/reservations" . encode) rs

      let aLittleLater = addLocalTime (theSeatingDuration / 2) $ d
      actual <- postJSON "/reservations" $
                encode $
                r { reservationQuantity = 1, reservationDate = aLittleLater }

      assertStatus 500 actual
  ]]

newtype AnyReservation =
  AnyReservation { getAnyReservation :: Reservation } deriving (Eq, Show)

instance Arbitrary AnyReservation where
  arbitrary =
    AnyReservation <$>
    liftM5 Reservation arbitrary arbitrary arbitrary arbitrary arbitrary

newtype ValidReservation =
  ValidReservation { getValidReservation :: Reservation } deriving (Eq, Show)

instance Arbitrary ValidReservation where
  arbitrary = do
    (NonNilUUID rid) <- arbitrary
    (FutureTime d) <- arbitrary
    n <- arbitrary
    e <- arbitrary
    (QuantityWithinCapacity q) <- arbitrary
    return $ ValidReservation $ Reservation rid d n e q

newtype NonNilUUID = NonNilUUID { getNonNilUUID :: UUID } deriving (Eq, Show)

instance Arbitrary NonNilUUID where
  arbitrary = NonNilUUID <$> arbitrary `suchThat` (/= nil)

newtype FutureTime =
  FutureTime { getFutureTime :: LocalTime } deriving (Eq, Show)

instance Arbitrary FutureTime where
  arbitrary =
    (\dt -> FutureTime $ addLocalTime (getPositive dt) now2019) <$> arbitrary

newtype QuantityWithinCapacity = QuantityWithinCapacity Int deriving (Eq, Show)

instance Arbitrary QuantityWithinCapacity where
  arbitrary =
    QuantityWithinCapacity <$> choose (1, maximum (tableSeats <$> theTables))

newtype ValidTable = ValidTable { getValidTable :: Table } deriving (Eq, Show)

instance Arbitrary ValidTable where
  arbitrary = (ValidTable . Table . getPositive) <$> arbitrary

get :: BS.ByteString -> Session SResponse
get url = request $ setPath defaultRequest { requestMethod = methodGet } url

postJSON :: BS.ByteString -> LBS.ByteString -> Session SResponse
postJSON url json = srequest $ SRequest req json
  where
    req = setPath defaultRequest
            { requestMethod = methodPost
            , requestHeaders = [("Content-Type", "application/json")]} url

type DB = Map UUID Reservation

createInFake :: IORef DB -> Reservation -> IO ()
createInFake ref r = modifyIORef' ref (Map.insert (reservationId r) r)

readOneFromFake :: IORef DB -> UUID -> IO (Maybe Reservation)
readOneFromFake ref rid = Map.lookup rid <$> readIORef ref

readManyFromFake :: IORef DB -> LocalTime -> LocalTime -> IO [Reservation]
readManyFromFake ref lo hi = do
  db <- readIORef ref
  let allReservations = Map.elems db
  let inRange (Reservation _ d _ _ _) = lo <= d && d <= hi
  return $ filter inRange allReservations

runInFakeDB :: MonadIO m => IORef DB -> ReservationsInstruction (m a) -> m a
runInFakeDB ref (ReadReservation rid next) =
  liftIO (readOneFromFake ref rid) >>= next
runInFakeDB ref (ReadReservations lo hi next) =
  liftIO (readManyFromFake ref lo hi) >>= next
runInFakeDB ref (CreateReservation r next) =
  liftIO (createInFake ref r) >> next

-- This date has nothing to do with when this code was written; it's the
-- approximate date that Blade Runner takes place.
now2019 :: LocalTime
now2019 = LocalTime (fromGregorian 2019 11 8) (TimeOfDay 20 49 0)

runIn2019 :: Monad m => ClockInstruction (m a) -> m a
runIn2019 (CurrentTime next) = next now2019

runInFakeDBAndIn2019 :: MonadIO m
                     => IORef DB
                     -> ReservationsProgramT m a
                     -> m a
runInFakeDBAndIn2019 ref = iterT go
  where go (InL rins) = runInFakeDB ref rins
        go (InR cins) = runIn2019 cins

theSeatingDuration :: NominalDiffTime
theSeatingDuration = 2 * 60 * 60 -- 2 hours

theTables :: [Table]
theTables = [Table 2, Table 4, Table 4, Table 2, Table 6]

app :: IO Application
app = do
  ref <- newIORef Map.empty
  return $
    serve api $
    hoistServer api (runInFakeDBAndIn2019 ref) $
    server theSeatingDuration theTables

withApp :: Testable prop => Session prop -> Property
withApp = idempotentIOProperty . (app >>=) . runSession
