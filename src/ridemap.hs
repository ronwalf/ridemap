{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
import Control.Monad (foldM, guard, when)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)
import System.Console.GetOpt
import System.Directory (createDirectory, doesDirectoryExist)
import System.Environment (getArgs)
import System.FilePath (combine)
import System.IO (hPutStrLn, stderr)


import GCRide


gCirc :: Double
gCirc = 20037508.34*2

data Options = Options {
    maxTime :: Double,
    minStep :: Double,
    resolution :: Double,
    projection :: (String, TrackPoint -> TrackPoint)
    }

defaultOpts :: Options
defaultOpts = Options {
    maxTime = 60,
    minStep = 0.5,
    resolution = 20,
    projection = ("EPSG:4326",id)
}

readOrFail :: Read a => String -> String -> IO a
readOrFail name str = case reads str of
    [(r,"")] -> return r
    _ -> fail ("Could not parse " ++ name)

options :: [OptDescr (Options -> IO Options)]
options = 
    [ Option ['r'] ["resolution"]
        (ReqArg (\n opts -> do
            r <- readOrFail "resolution" n
            return $ opts { resolution = r })
        "NUM")
        "resolution of grid in meters (at the equator)"
    , Option ['s'] ["step"]
        (ReqArg (\n opts -> do
            s <- readOrFail "minimum step size" n
            return $ opts { minStep = s })
        "NUM")
        "minimum time step when interpolating points"
    , Option ['m'] ["max"]
        (ReqArg (\n opts -> do
            m <- readOrFail "maximum recording gap" n
            return $ opts { maxTime = m })
        "NUM")
        "maximum recording gap to try and interpolate"
    , Option ['g'] ["google"] 
        (NoArg (\opts -> return $ opts { projection = ("EPSG:900913", toGM) }))
        "Use google maps projection"
    ]



main :: IO ()
main = do
    argv <- getArgs
    (opts, dir, files) <- case getOpt Permute options argv of
        (o, dir:files, []) -> do
            opts <- foldM (\opts f -> f opts) defaultOpts o
            return (opts, dir, files)
        (_,_, errs) ->
            ioError $ userError $
            concat errs
            ++ usageInfo "Usage: rideaccum [OPTION...] ouput-directory files..." options
    --elapsed <- foldM totalTime 0 files
    ensureDirectory dir
    grid <- foldM (positions opts dir) (emptyGrid (fst $ projection opts) $ hexDesc opts) files
    BL.writeFile (combine dir "grid.json") (encode grid)
    return ()
    where
        ensureDirectory :: String -> IO ()
        ensureDirectory dir = do
            exists <- doesDirectoryExist dir
            when (not exists) $ do
                createDirectory dir
            return ()

positions :: Options -> String -> HexGrid -> String -> IO HexGrid 
positions opts dir grid f = do
    ride <- readGCRide f
    let segs = segments (maxTime opts) $ map (snd $ projection opts) $ trackPoints ride
    foldM processSegment grid segs
    where
    processSegment :: HexGrid -> [TrackPoint] -> IO HexGrid
    processSegment grid' segment = do
        let path = gridPath (minStep opts) (hexDesc opts) segment
        let m = foldr (\hc m' -> M.insertWith addCells (hcPos hc) hc m') (gridCells grid') path
        BL.writeFile (combine dir $ "ride" ++ show (gridRides grid') ++ ".json") $
                encode (object ["ride" .= segment])
        return $ grid { gridCells = m, gridRides = gridRides grid' + 1 }



data HexGrid = HexGrid {
    gridProj :: !String,
    gridDesc :: !HexDesc, 
    gridCells :: !(M.Map (Int, Int) HexCell), 
    gridRides:: !Int }

emptyGrid :: String -> HexDesc -> HexGrid 
emptyGrid proj desc = HexGrid {
    gridProj = proj,
    gridDesc = desc,
    gridCells = M.empty,
    gridRides = 0
}

instance ToJSON HexGrid where
    toJSON grid = 
        let 
            hexTimes = sortBy (flip compare `on` hcTime) $ map snd $ M.toList $ gridCells grid
            (maxCellTime, totalTime) = foldr (\hc (mt,tt) -> let ht = hcTime hc in mt `seq` tt `seq` (max mt ht, ht + tt)) (0,0) hexTimes
        in
        object [ 
        "projection" .= gridProj grid,
        "desc" .= gridDesc grid, 
        "cells" .= hexTimes, 
        "maxTime" .= maxCellTime,
        "totalTime" .= totalTime,
        "rides" .= gridRides grid ]

data HexCell = HexCell {
    hcPos :: (Int, Int),
    hcTime :: !Double,
    hcCount :: !Int
}

instance ToJSON HexCell where
    toJSON cell = object [
        "pos" .= hcPos cell,
        "time" .= hcTime cell,
        "count" .= hcCount cell ]

tpToCell :: HexDesc -> TrackPoint -> Double -> HexCell
tpToCell desc tp time = HexCell {
    hcPos = hexPtoI desc tp,
    hcTime = time,
    hcCount = 1
}

addCells :: HexCell -> HexCell -> HexCell
addCells x y = -- x `seq` y `seq` 
    x {
    hcTime = hcTime x + hcTime y,
    hcCount = hcCount x + hcCount y
}


segments :: Double -> [TrackPoint] -> [[TrackPoint]]
segments _ [] = []
segments mstep (tp:tpl) =
    let
        (continuous, skip) = segSpan tp tpl
    in
    continuous : segments mstep skip
    where
    segSpan tp [] = ([tp],[])
    segSpan tp0 (tp1 : tpl)
        | secs tp1 - secs tp0 > mstep = ([tp0], tp1 : tpl)
        | otherwise = 
            let (x, y) = segSpan tp1 tpl in
            (tp0 : x, y)

gridPath :: Double -> HexDesc -> [TrackPoint] -> [HexCell]
gridPath _ _ [] = []
gridPath step desc (h : l) =
    gp' 0 h l
    where
        pos :: TrackPoint -> (Int,Int)
        pos = hexPtoI desc
        adjacent :: TrackPoint -> TrackPoint -> Bool
        adjacent tp0 tp1 =
            let 
                (x0,y0) = pos tp0
                (x1,y1) = pos tp1
            in
            if x0 == x1
                then (y0 == y1 + 1 || y0 == y1 - 1)
                else if (x0 == x1 + 1 || x0 == x1 -1)
                    then (y0 == y1 + mod (x1+1) 2 || y0 == y1 - mod x1 2)
                    else False
        gp' :: Double -> TrackPoint -> [TrackPoint] -> [HexCell]
        gp' t tp0 [] = [tpToCell desc tp0 t]
        gp' t tp0 (tp1 : tpl)
            | pos tp0 == pos tp1 = t `seq` gp' (t + elapsed) tp1 tpl
            | (adjacent tp0 tp1 && elapsed < step) = tpToCell desc tp0 (t + elapsed/2) : gp' (elapsed/2) tp1 tpl
            -- | (adjacent tp0 tp1) = (pos tp0, t + elapsed/2) : gp' (elapsed/2) tp1 tpl
            | otherwise = -- trace (show (tp0, tp1, pos tp0, pos tp1)) $ 
                gp' t tp0 (midpoint tp0 tp1 : tp1 : tpl)
            where
            elapsed = secs tp1 - secs tp0

type HexDesc = (Double, Double, Double)

hexDesc :: Options -> HexDesc
hexDesc opts = 
    let
        -- This is a pretty hacky way to find the hex parameters
        tp = TrackPoint {lat = 0, lon = resolution opts * 360 / gCirc, secs=0} 
        tp' = snd (projection opts) tp
        r = lon tp'
    in (r, r * 1.5, r * sqrt 3)

hexItoP :: HexDesc -> (Int, Int) -> TrackPoint
hexItoP (_, s, h) (i,j) = 
    TrackPoint {
        lon = s * fromIntegral i, 
        lat = (*) h $ (fromIntegral j) - 0.5 * (fromIntegral $ mod i 2),
        secs = 0
    }

hexPtoI :: HexDesc -> TrackPoint -> (Int,Int)
hexPtoI desc@(r, s, h) tp =
    let
        it = floor (lon tp / s) :: Int
        xt = (lon tp) - (fromIntegral it) * s :: Double
        yts = (lat tp) + h * (0.5 * (fromIntegral $ mod it 2)) :: Double
        jt = floor (yts/h) :: Int
        yt = yts - (fromIntegral jt) * h :: Double
    in
    --trace (show tp) $
    --trace (show (desc,it, xt/s, yts, jt, yt/h, xt < s -  r * abs (0.5 - yt/h))) $
    if xt < s -  r * abs (0.5 - yt/h)
        then (it, jt)
        else (it + 1, 
            if yt < 0.5*h
                then jt - mod it 2
                else jt + mod (it + 1) 2)


data TrackPoint = TrackPoint {
    lat :: !Double,
    lon :: !Double,
    secs :: !Double
} deriving (Show, Ord, Eq)

instance ToJSON TrackPoint where
    toJSON tp = toJSON (lon tp, lat tp, secs tp)

trackPoints :: GCRide -> [TrackPoint]
trackPoints =
    mapMaybe toTP . samples
    where
        toTP :: GCSample -> Maybe TrackPoint
        toTP m = do
            lt <- M.lookup Lat m
            ln <- M.lookup Lon m
            guard (lt /= 0 || ln /= 0)
            s <- M.lookup Secs m
            return $ TrackPoint lt ln s 

-- BADBADBAD
midpoint :: TrackPoint -> TrackPoint -> TrackPoint
midpoint tp1 tp2 = 
    TrackPoint { lat = (lat tp1 + lat tp2)/2, lon = (lon tp1 + lon tp2)/2, secs = (secs tp1 + secs tp2)/2 }

    
toGM :: TrackPoint -> TrackPoint 
toGM tp =
    let
        x = (gCirc / 360) * lon tp
        y = (*) (gCirc / (2*pi)) $ log $ tan $ (90 + lat tp) * pi / 360
    in
    tp { lon = x, lat = y }

fromGM :: TrackPoint -> TrackPoint
fromGM tp =
    let
        ln = (360 / gCirc) * lon tp
        lt = (*) (180 / pi) $
            2 * atan (exp $ 2 * (lat tp) * pi / gCirc) - pi / 2
    in
    tp {lat = lt, lon = ln, secs = 0}

