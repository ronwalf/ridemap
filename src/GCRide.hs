{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module GCRide (
    GCRide(..),
    GCInterval(..),
    GCSampleKey(..),
    GCSample,
    readGCRide,
) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import System.Locale (defaultTimeLocale)

readGCRide :: String -> IO GCRide
readGCRide fname = do
    f <- B.readFile fname
    case decode' f of
        Just (GCRideFile ride) -> return ride
        Nothing -> fail $ "Could not decode " ++ fname


data GCRideFile = GCRideFile GCRide
instance FromJSON GCRideFile where
    parseJSON (Object v) = GCRideFile <$>
        v .: "RIDE"
    parseJSON _ = mzero

data GCRide = GCRide {
    startTime :: UTCTime,
    recIntSecs :: Int,
    deviceType :: T.Text,
    identifier :: T.Text,
    overrides :: [M.Map T.Text (M.Map T.Text T.Text)],
    tags :: M.Map T.Text T.Text,
    intervals :: [GCInterval],
    samples :: [GCSample]
    } deriving (Show, Eq)


instance FromJSON GCRide where
    parseJSON (Object v) = do
        startText <- v .: "STARTTIME"
        start <- case parseTime defaultTimeLocale "%Y/%m/%d %X %Z" (T.unpack startText) of
            Just d -> return d
            Nothing -> fail "Could not parse start time"
        recInt <- v .: "RECINTSECS"
        dtype <- v .: "DEVICETYPE"
        ident <- v .: "IDENTIFIER"
        ovrides <- v .:? "OVERRIDES" .!= []
        ts <- v .: "TAGS"
        ivals <- v .:? "INTERVALS" .!= []
        sl <- v .:? "SAMPLES" .!= []

        return $ GCRide {
            startTime = start,
            recIntSecs = recInt,
            deviceType = dtype,
            identifier = ident,
            overrides = ovrides,
            tags = ts,
            intervals = ivals,
            samples = sl
        }   
    parseJSON _ = mzero

data GCInterval = GCInterval T.Text Double Double
    deriving (Show, Eq)

instance FromJSON GCInterval where
    parseJSON (Object v) = GCInterval <$>
            v .: "NAME" <*>
            v .: "START" <*>
            v .: "STOP"
    parseJSON _ = mzero

type GCSample = M.Map GCSampleKey Double

instance (FromJSON v) => FromJSON (M.Map GCSampleKey v) where
    parseJSON (Object o) = H.foldrWithKey (\k v pm -> do
            k' <- parseJSON (String k)
            v' <- parseJSON v
            m' <- pm
            return $ M.insert k' v' m')
        (return M.empty) 
        o
    parseJSON _ = mzero

data GCSampleKey =
    Secs
    | Cad
    | HR
    | KM
    | KPH
    | NM
    | Watts
    | Alt
    | Lon
    | Lat
    | Headwind
    | Slope
    | Temp
    | LRBalance
    deriving (Eq, Ord, Show)

keyMap :: M.Map T.Text GCSampleKey
keyMap = M.fromList
    [ ("SECS", Secs)
    , ("CAD", Cad)
    , ("HR", HR)
    , ("KM", KM)
    , ("KPH", KPH)
    , ("NM", NM)
    , ("WATTS", Watts)
    , ("ALT", Alt)
    , ("LON", Lon)
    , ("LAT", Lat)
    , ("HEADWIND", Headwind)
    , ("SLOPE", Slope)
    , ("TEMP", Temp)
    , ("LRBALANCE", LRBalance)
    ]
    

instance FromJSON GCSampleKey where
    parseJSON (String key) = case M.lookup key keyMap of
        Just sk -> return sk
        Nothing -> mzero
    parseJSON _ = mzero

{-
instance Hashable GCSampleKey where
    hash Secs = 1
    hash Cad = 2
    hash HR = 4
    hash KM = 8
    hash KPH = 16
    hash NM = 32
    hash Watts = 64
    hash Alt = 128
    hash Lon = 256
    hash Lat = 512
    hash Headwind = 1024
    hash Slope = 2048
    hash Temp = 4096
    hash LRBalance = 8192
-}
