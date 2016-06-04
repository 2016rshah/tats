{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Web.Twitter.Conduit hiding (map)
import Common

import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Data.List 

--createBox e rh = (text (show e) <> (rect e rh)) # fontSize (local rh)

generateGraph :: (Eq a) => [a] -> [a] -> Diagram B
generateGraph following followers = 
  (text (show left) <> (rect left rh)) # fontSize (local rh)
  ||| (text (show middle) <> (rect middle rh)) # fontSize (local rh)
  ||| (text (show right) <> (rect right rh)) # fontSize (local rh)
  where 
    left = fromIntegral (length (following \\ followers))
    right = fromIntegral (length (followers \\ following))
    middle = fromIntegral (length (intersect following followers))
    rh = (flip (/) 10 . sum . map (fromIntegral . length)) [following, followers]

--example :: Diagram B
--example = 
--  (text (show left)) <> (rect left rh) # fc red
--  ||| (text (show middle)) <> rect middle rh # fc blue
--  ||| (text (show right) <> rect right rh) # fc green
--  where 
--    left = fromIntegral (length ([1..10]))
--    right = fromIntegral (length ([1..5]))
--    middle = fromIntegral (length ([1..3]))
--    rh = (flip (/) 10 . sum . map (fromIntegral . length)) [[1..5], [1..6]]
--text (show (fromIntegral (length [1..5]))) <> rect 8 1
--example = (rect following rh) ||| (rect followers rh) ||| (rect b rh)

main :: IO ()
main = do
  {-
    export OAUTH_CONSUMER_KEY="..."
    export OAUTH_CONSUMER_SECRET="..."
    export OAUTH_ACCESS_TOKEN="..."
    export OAUTH_ACCESS_SECRET="..."
  -}
  twInfo <- getTWInfoFromEnv
  mgr <- newManager tlsManagerSettings
  followingIds <- fmap contents (call twInfo mgr $ friendsIds (ScreenNameParam "2016rshah"))
  followersIds <- fmap contents (call twInfo mgr $ followersIds (ScreenNameParam "2016rshah"))
  putStrLn ("Following: " ++ show (length followingIds))
  putStrLn ("Followers: " ++ show (length followersIds))
  mainWith (generateGraph followingIds followersIds)
  --mainWith (example)
  putStrLn "Success!"
