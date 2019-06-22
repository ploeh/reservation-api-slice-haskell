{-# LANGUAGE OverloadedStrings #-}
module ReservationAPISpec where

import Control.Monad
import Control.Monad.Trans.Free
import Control.Monad.IO.Class
import Data.Functor.Sum
import Data.UUID
import Data.IORef
import Data.Time.Calendar
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Aeson (encode, decode)
import Network.HTTP.Types (methodPost)
import Network.Wai.Test (SResponse)
import Test.Hspec
import Test.Hspec.Wai
import qualified Test.Hspec.Wai.QuickCheck as WQC
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()
import Test.QuickCheck.Instances.UUID ()
import ReservationAPI
import Servant
import API

newtype AnyReservation = AnyReservation Reservation deriving (Eq, Show)

instance Arbitrary AnyReservation where
  arbitrary =
    AnyReservation <$>
    liftM5 Reservation arbitrary arbitrary arbitrary arbitrary arbitrary

newtype ValidReservation = ValidReservation Reservation deriving (Eq, Show)

instance Arbitrary ValidReservation where
  arbitrary = do
    rid <- arbitrary `suchThat` (/= nil)
    d <- (\dt -> addLocalTime (getPositive dt) now2019) <$> arbitrary
    n <- arbitrary
    e <- arbitrary
    (Positive q) <- arbitrary
    return $ ValidReservation $ Reservation rid d n e q

reservationAPISpec :: Spec
reservationAPISpec = describe "Reservations" $ do
  describe "Accept" $ do
    it "rejects any reservation when restaurant has no tables" $ property $ \
      (AnyReservation r) -> do
      let actual = canAccept [] r
      actual `shouldBe` False

  describe "Reservation JSON" $ do
    it "renders correctly" $ do
      let rid = fromWords 872411231 2362592316 2161598850 3450687078
      let rd = LocalTime (fromGregorian 2019 4 12) (TimeOfDay 19 0 0)

      let json = encode $ Reservation rid rd "Jo" "j@example.com" 3

      json `shouldBe` "{\"id\":\"33fff05f-8cd2-4c3c-80d7-6182cdad4e66\",\
                      \\"date\":\"2019-04-12T19:00:00\",\
                      \\"name\":\"Jo\",\
                      \\"email\":\"j@example.com\",\
                      \\"quantity\":3}"

    it "round-trips" $ property $ \(AnyReservation r) -> do
      let json = encode r
      let actual = decode json
      actual `shouldBe` Just r

  with app $ describe "/reservations/" $ do
    it "responds with 404 when no reservation exists" $ WQC.property $ \rid ->
      get ("/reservations/" <> toASCIIBytes rid) `shouldRespondWith` 404

    it "responds with 200 after reservation is added" $ WQC.property $ \
      (ValidReservation r) -> do
      _ <- postJSON "/reservations" $ encode r
      let actual = get $ "/reservations/" <> toASCIIBytes (reservationId r)
      actual `shouldRespondWith` 200

    it "succeeds when valid reservation is POSTed" $ WQC.property $ \
      (ValidReservation r) -> do
      let actual = postJSON "/reservations" $ encode r
      actual `shouldRespondWith` 200

    it "fails when reservation is POSTed with the nil UUID" $ WQC.property $ \
      (ValidReservation r) -> do
      let invalid = r { reservationId = nil }
      let actual = postJSON "/reservations" $ encode invalid
      actual `shouldRespondWith` 400

    it "fails when reservation is POSTed with invalid quantity" $ WQC.property $ \
      (ValidReservation r) (NonNegative q) -> do
      let invalid = r { reservationQuantity = negate q }
      let actual = postJSON "/reservations" $ encode invalid
      actual `shouldRespondWith` 400

    it "fails when past reservation is POSTed" $ WQC.property $ \
      (ValidReservation r) (Positive diffTime) -> do
      let invalid =
            r { reservationDate = addLocalTime (negate diffTime) now2019 }
      let actual = postJSON "/reservations" $ encode invalid
      actual `shouldRespondWith` 400

-- Not in time 1.8.0.2
addLocalTime :: NominalDiffTime -> LocalTime -> LocalTime
addLocalTime x = utcToLocalTime utc . addUTCTime x . localTimeToUTC utc

postJSON :: BS.ByteString -> LBS.ByteString -> WaiSession SResponse
postJSON url = request methodPost url [("Content-Type", "application/json")]

type DB = Map UUID Reservation

createInFake :: IORef DB -> Reservation -> IO ()
createInFake ref r = modifyIORef' ref (Map.insert (reservationId r) r)

readFromFake :: IORef DB -> UUID -> IO (Maybe Reservation)
readFromFake ref rid = Map.lookup rid <$> readIORef ref

runInFakeDB :: MonadIO m => IORef DB -> ReservationsInstruction (m a) -> m a
runInFakeDB ref (ReadReservation rid next) =
  liftIO (readFromFake ref rid) >>= next
runInFakeDB   _ (ReadReservations _ _ next) = next []
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

app :: IO Application
app = do
  ref <- newIORef Map.empty
  return $ serve api $ hoistServer api (runInFakeDBAndIn2019 ref) $ server []
