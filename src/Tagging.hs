{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Tagging (
    readTagFile, emptyTagFile,
    tag,
    Taggable(..),
    TagFile(..),
    TagData(..), emptyTagData
) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Colour as C
import qualified Data.Colour.Names as CN
import qualified Data.Colour.SRGB as SRGB
import qualified Data.Text as T
import Text.Regex.Posix

readTagFile :: String -> IO TagFile
readTagFile fname = do
    f <- B.readFile fname
    case decode' f of
        Just tf -> return tf
        Nothing -> fail $ "Could not decode " ++ fname

tag :: (Taggable a) => TagFile -> a -> [String]
tag tfile obj =
    let tl = getTags obj in
    map name $ filter (or . (\td -> map (matches td) tl)) (tags tfile)
    where
    matches :: TagData -> (String, String) -> Bool
    matches td (aname , val)
        | aname == attr td = val =~ matchRE td
        | otherwise = False
       

data TagFile = TagFile {
    tags :: [TagData],
    defaultTag :: TagData
}   

emptyTagFile :: TagFile
emptyTagFile = TagFile [] emptyTagData

instance FromJSON TagFile where
    parseJSON (Object v) = TagFile <$>
        v .: "tags" <*>
        v .: "default"
    parseJSON _ = mzero

instance ToJSON TagFile where
    toJSON td = object [
        "tags" .= tags td,
        "default" .= defaultTag td
        ]


data TagData = TagData {
    name :: String,
    attr :: String,
    matchRE :: String,
    image :: String,
    color :: C.Colour Double
}
emptyTagData :: TagData
emptyTagData = TagData "" "" "" "" CN.red

instance FromJSON TagData where
    parseJSON (Object v) = TagData <$>
        v .:? "name" .!= "" <*>
        v .:? "attr" .!= "" <*>
        v .:? "match" .!= "" <*>
        v .:? "img" .!= "" <*>
        v .:? "color" .!= CN.red
    parseJSON _ = mzero

instance ToJSON TagData where
    toJSON td = object [
        "name" .= name td,
        "attr" .= attr td,
        "match" .= matchRE td,
        "img" .= image td,
        "color" .= color td
        ]

instance FromJSON (C.Colour Double) where
    parseJSON (String t) = case SRGB.sRGB24reads (T.unpack t) of
        [(c,"")] -> pure c
        _ -> fail $ "Could not read string " ++ T.unpack t
    parseJSON _ = mzero
instance ToJSON (C.Colour Double) where
    toJSON = toJSON . SRGB.sRGB24show

class Taggable a where
    getTags :: a -> [(String, String)]
    
