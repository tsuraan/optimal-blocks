module Main
where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import Data.ByteString ( ByteString )
import System.Environment ( getArgs )
import System.IO ( Handle, IOMode(..), withFile, hPutStrLn )
import Data.Hex ( hex )
import Crypto.Hash.MD5 ( hash )
import Control.Monad ( forM_ )
import Data.List ( sortBy, transpose )

import Algorithm.OptimalBlocks ( Blocks(..), OptimalBlock(..), chop' )

data Args = Args
  { argWinSz :: Int
  , argChunkSz :: Int
  , argOutHtml :: Maybe String
  } deriving ( Show )

data FileResult = FR
  { frPath :: String
  , frBlocks :: [(String, Int)]
  , frRemain :: (String, Int)
  }

stdArgs :: Args
stdArgs = Args 128 (1024*1024) Nothing

parseArgs :: [String] -> (Args, [String])
parseArgs = go stdArgs []
  where
  go :: Args -> [String] -> [String] -> (Args, [String])
  go args paths [] = (args, reverse paths)
  go args paths [path] = (args, reverse $ path:paths)
  go args paths ("-w":w:rest) =
    go (args{argWinSz=read w}) paths rest
  go args paths ("-c":c:rest) =
    go (args{argChunkSz=read c}) paths rest
  go args paths ("-o":o:rest) =
    go (args{argOutHtml=Just o}) paths rest
  go args paths (path:rest) =
    go args (path:paths) rest

doFiles :: Args -> [String] -> IO [FileResult]
doFiles args [] = do
  bs <- withFile "/dev/urandom" ReadMode (\h -> BS.hGet h (100*1024*1024))
  return [getResult args "/dev/urandom" bs]
doFiles args lst =
  mapM (\p -> getResult args p `fmap` BS.readFile p ) lst

getResult :: Args -> String -> ByteString -> FileResult
getResult args path bs =
  let blocks = chop' (argWinSz args) (argChunkSz args) bs
      remain = blocksRemain blocks
      blBs   = map fromOptimal $ blocksOptimal blocks
  in FR path
        [(BSC.unpack $ hex $ hash b, BS.length b) | b <- blBs]
        (BSC.unpack $ hex $ hash remain, BS.length remain)
  -- let avg    = fromRational ( ( toRational $ sum [BS.length b | b <- blBs])
  --                           / (toRational $ length blBs ) ) :: Float

  -- forM_ blBs $ \b -> putStrLn ("  " ++ (BSC.unpack $ hex $ hash b) ++ " " ++ (show $ BS.length b))

prtText :: Args -> [FileResult] -> IO ()
prtText _ [] = return ()
prtText args (res:rest) = do
  putStrLn $ "Analyzed " ++ frPath res
  putStrLn $ "Generated " ++ show (length $ frBlocks res) ++ " optimal blocks"
  let avg    = fromRational ( ( toRational $ sum [len | (_, len) <- frBlocks res])
                            / ( toRational $ length $ frBlocks res) ) :: Float
  putStrLn $ "Avg block size is " ++ show avg ++ " (desired was " ++ show (argChunkSz args) ++ ")"
  forM_ (frBlocks res) $ \(h, l) -> putStrLn ("  " ++ h ++ " " ++ show l)
  prtText args rest


prtHtml :: String -> Args -> [FileResult] -> IO ()
prtHtml output args results = withFile output WriteMode go
  where
  go :: Handle -> IO ()
  go h = do
    let ordered = sortBy (\a b -> compare (length $ frBlocks b)
                                          (length $ frBlocks a))
                         results
    let numCell = length results
    s "<html><body><table>"
    s " <tr>"
    forM_ ordered $ \res ->
      s $ "  <th>" ++ frPath res ++ "</th>"
    s " </tr>"
    s " <tr>"
    forM_ ordered $ \res ->
      let avg = if null (frBlocks res)
          then 0
          else fromRational ( ( toRational $ sum [len | (_, len) <- frBlocks res])
                            / ( toRational $ length $ frBlocks res) ) :: Float
      in s $ "  <td>Avg Len:" ++ show avg ++ "</td>"
    s $ "<td>Expected Avg: " ++ show (argChunkSz args) ++ "</td>"
    s " </tr>"
    loop numCell $ transpose [frBlocks o | o <- ordered]
    s " <tr>"
    forM_ ordered $ \res -> writeCell (frRemain res)
    s " </tr>"
    s "</table></body></html>"
    where
    loop _ [] = return ()
    loop nc (row:rest) = do
      s " <tr>"
      forM_ row writeCell
      s " </tr>"
      loop nc rest

    writeCell :: (String, Int) -> IO ()
    writeCell (hexHash, len) = do
      let color = "color:#" ++ take 6 hexHash
      s $ "  <td> <span style=\"" ++ color ++ "\">" ++ hexHash ++ "</span>"
      s $ "(" ++ show len ++ ")</td>"

    s :: String -> IO ()
    s = hPutStrLn h

main :: IO ()
main = do
  (args, paths) <- parseArgs `fmap` getArgs
  results <- doFiles args paths
  case argOutHtml args of
    Nothing -> prtText args results
    Just output -> prtHtml output args results

