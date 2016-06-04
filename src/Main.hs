{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Web.Twitter.Conduit hiding (map)
import Common

import Control.Applicative
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine



example :: Diagram B
example = rect 100 100
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
  mainWith (example)
  putStrLn "Success!"

--{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE FlexibleContexts #-}

--module Main where

--import Common

--import Web.Twitter.Conduit
--import Web.Twitter.Types.Lens

--import Control.Lens
--import qualified Data.ByteString.Char8 as B8
--import qualified Data.Conduit as C
--import qualified Data.Conduit.List as CL
--import qualified Data.Text as T
--import qualified Data.Text.IO as T
--import System.IO (hFlush, stdout)
--import qualified Web.Authenticate.OAuth as OA

--tokens :: OAuth
--tokens = twitterOAuth
--    { oauthConsumerKey = "yKIgsYdYIeOh5ShQd2hgnAfss"
--    , oauthConsumerSecret = "bqlUSsRQTI4vcNQGyjd1DlTrv3l06qAbctcuiWkhkTMnUdCEyo"
--    }

--authorize :: OAuth -- ^ OAuth Consumer key and secret
--          -> (String -> IO String) -- ^ PIN prompt
--          -> Manager
--          -> IO Credential
--authorize oauth getPIN mgr = do
--    cred <- OA.getTemporaryCredential oauth mgr
--    let url = OA.authorizeUrl oauth cred
--    pin <- getPIN url
--    putStrLn pin
--    OA.getAccessToken oauth (OA.insert "oauth_verifier" (B8.pack pin) cred) mgr

--getTWInfo :: Manager -> IO TWInfo
--getTWInfo mgr = do
--    cred <- authorize tokens getPIN mgr
--    return $ setCredential tokens cred def
--  where
--    getPIN url = do
--        putStrLn $ "browse URL: " ++ url
--        putStr "> what was the PIN twitter provided you with? "
--        hFlush stdout
--        getLine

--main :: IO ()
--main = do
--    mgr <- newManager tlsManagerSettings
--    twInfo <- getTWInfo mgr
--    putStrLn $ "# your home timeline (up to 800 tweets):"
--    sourceWithMaxId twInfo mgr (homeTimeline & count ?~ 200)
--        C.$= CL.isolate 800
--        C.$$ CL.mapM_ $ \status -> do
--            T.putStrLn $ T.concat [ T.pack . show $ status ^. statusId
--                                  , ": "
--                                  , status ^. statusUser . userScreenName
--                                  , ": "
--                                  , status ^. statusText
--                                  ]
