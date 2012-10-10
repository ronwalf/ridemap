{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (foldM, guard)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)


import GCRide


gCirc :: Double
gCirc = 20037508.34*2

data Options = Options {
    maxTime :: Double,
    minStep :: Double,
    resolution :: Double
    }

defaultOpts :: Options
defaultOpts = Options {
    maxTime = 60,
    minStep = 0.5,
    resolution = 20
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
    ]



main :: IO ()
main = do
    argv <- getArgs
    (opts, files) <- case getOpt Permute options argv of
        (o, files, []) -> do
            opts <- foldM (\opts f -> f opts) defaultOpts o
            return (opts, files)
        (_,_, errs) ->
            ioError $ userError $
            concat errs
            ++ usageInfo "Usage: rideaccum [OPTION...] files..." options
    --elapsed <- foldM totalTime 0 files
    pm <- foldM (positions opts) M.empty files
    hPutStrLn stderr $ "Number of positions: " ++ show (M.size pm)
    let hexTimes = sortBy (flip compare `on` hcTime) $ map snd $ M.toList pm
    let (maxCellTime, totalTime) = foldr (\hc (mt,tt) -> let ht = hcTime hc in mt `seq` tt `seq` (max mt ht, ht + tt)) (0,0) hexTimes
    --let tp = (hexItoP (hexDesc opts) $ fst $ head hexTimes) { secs = snd $ head hexTimes }

    --hPutStrLn stderr $ "Most common location: " ++ show (lon tp) ++ "," ++ show (lat tp) ++ " (" ++ show (secs tp) ++ ") seconds"
    {-
    let tp = last $
            map (\(pt,t) -> (hexItoP (hexDesc opts) pt) { secs = t} ) $
            sortBy (compare `on` snd) $ M.toList pm
    hPutStrLn stderr $ "Bounding box: " ++ show (foldr (\((i,j),_) (l,b,r,t) -> 
            let 
                tp' = (hexItoP (hexDesc opts) (i,j))
                x = lon tp'
                y = lat tp'
            in 
        (if x < l then x else l, if y < b then y else b, if x > r then x else r, if y > t then y else t))
        (lon tp, lat tp, lon tp, lat tp)
        (M.toList pm))
    -}
    {-
    hPutStrLn stderr $ "Point errors:"
    mapM_ (\x -> hPutStrLn stderr $ show x) $
        filter (\(p0, p1) -> p0 /= p1) $
        map (\(p, _) -> (p, hexPtoI (hexDesc opts) ((\tp -> tp {lon = lon tp + 0.00000001 , lat = lat tp + 0.0000001}) (hexItoP (hexDesc opts) p)))) $
        M.toList pm
    -}

    BL.putStrLn (encode $ HexGrid {
        gridDesc = hexDesc opts,
        gridCells = hexTimes,
        gridMaxTime = maxCellTime,
        gridTotalTime = totalTime,
        gridRides = [] -- rides
    })
    return ()

positions :: Options -> (M.Map (Int, Int) HexCell)-> String -> IO (M.Map (Int,Int) HexCell)
positions opts m f = do
    ride <- m `seq` readGCRide f
    --let m' = M.fromListWith addCells $ gridPath (minStep opts) (maxTime opts) (hexDesc opts) $ trackPoints ride 
    --return (M.unionWith addCells m m')
    return $ foldr (\hc m' -> M.insertWith addCells (hcPos hc) hc m') m $
        gridPath (minStep opts) (maxTime opts) (hexDesc opts) $ trackPoints ride


data HexGrid = HexGrid { 
    gridDesc :: HexDesc, 
    gridCells :: [HexCell], 
    gridMaxTime :: Double,
    gridTotalTime :: Double,
    gridRides :: [[(Double,Double)]] }
instance ToJSON HexGrid where
    toJSON grid = object [ 
        "desc" .= gridDesc grid, 
        "cells" .= gridCells grid, 
        "maxTime" .= gridMaxTime grid,
        "totalTime" .= gridTotalTime grid,
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



gridPath :: Double -> Double -> HexDesc -> [TrackPoint] -> [HexCell]
gridPath _ _ _ [] = []
gridPath step mstep desc (h : l) =
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
            {-
            pos tp0 `elem`
                [ (x1, y1) -- Same ?
                , (x1, y1 + 1) -- Above
                , (x1, y1 - 1) -- Below
                , (x1 + 1, y1 + mod (x1+1) 2) -- Above right
                , (x1 - 1, y1 + mod (x1+1) 2) -- Above left
                , (x1 + 1, y1 - mod (x1) 2) -- Below right
                , (x1 - 1, y1 - mod (x1) 2) -- Below left
                ]
            -}  
            {- -- True ||
            (x0 == x1 && (y0 == y1 - 1 || y0 == y1 + 1))
            || ((x0 == x1 - 1 || x0 == x1 + 1)
                && ((y0 == y1)
                    || (mod x0 2 == 0 && y0 == y1 + 1)
                    || (mod x0 2 == 1 && y0 == y1 - 1)))
            -}
        gp' :: Double -> TrackPoint -> [TrackPoint] -> [HexCell]
        gp' t tp0 [] = [tpToCell desc tp0 t]
        gp' t tp0 (tp1 : tpl)
            | elapsed > mstep = tpToCell desc tp0 t : gp' 0 tp1 tpl
            | pos tp0 == pos tp1 = t `seq` gp' (t + elapsed) tp1 tpl
            | (adjacent tp0 tp1 && elapsed < step) = tpToCell desc tp0 (t + elapsed/2) : gp' (elapsed/2) tp1 tpl
            -- | (adjacent tp0 tp1) = (pos tp0, t + elapsed/2) : gp' (elapsed/2) tp1 tpl
            | otherwise = -- trace (show (tp0, tp1, pos tp0, pos tp1)) $ 
                gp' t tp0 (midpoint tp0 tp1 : tp1 : tpl)
            where
            elapsed = secs tp1 - secs tp0

type HexDesc = (Double, Double, Double)

hexDesc :: Options -> HexDesc
hexDesc opts = let r = resolution opts * 360 / gCirc in (r, r * 1.5, r * sqrt 3)

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

    
{-
toGM :: TrackPoint -> (Double, Double)
toGM tp =
    let
        x = (gCirc / 180) * lon tp
        y = (*) (gCirc / pi) $ log $ tan $ (90 + lat tp) * pi / 360
    in
    (x, y)
    where

fromGM :: (Double,Double) -> TrackPoint
fromGM (x, y) =
    let
        ln = (180 / gCirc) * x
        lt = (*) (180 / pi) $
            2 * atan (exp $ y * pi / gCirc) - pi / 2
    in
    TrackPoint {lat = lt, lon = ln, secs = 0}
-}

