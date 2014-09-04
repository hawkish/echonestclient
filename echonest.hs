{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
-- import Text.JSON
-- import Text.JSON.Generic
import Network.HTTP (getResponseBody, getRequest, simpleHTTP, urlEncode)
import Data.List (isInfixOf, or)
-- import Data.Text
import qualified Data.Text as T
import System.Environment    
import System.IO    
import System.IO.Error
import Control.Exception
import Prelude hiding (catch)
import Data.Aeson
import Data.Attoparsec hiding (try)
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad
import GHC.Generics

data Response = Response { status :: Status
                         , songs :: [Song]
                         } deriving (Eq, Show)

data Status = Status { version :: T.Text
                     , code :: Integer
                     , message :: T.Text
                   } deriving (Eq, Show)

data Song = Song { artistId :: T.Text
                 , id :: T.Text
                 , artistName :: T.Text
                 , title :: T.Text
                 } deriving (Eq, Show)


instance FromJSON Response where
         parseJSON (Object v) =
                   Response <$> ((v .: "response") >>= (.: "status"))  
                            <*> ((v .: "response") >>= (.: "songs"))
         parseJSON _ = mzero


instance FromJSON Status where
         parseJSON (Object v) =
                   Status <$> v .: "version"  
                          <*> v .: "code"  
                          <*> v .: "message" 
         parseJSON _ = mzero


instance FromJSON Song where
         parseJSON (Object v) =
                   Song <$> v .: "artist_id"
                        <*> v .: "id"
                        <*> v .: "artist_name"
                        <*> v .: "title" 
         parseJSON _ = mzero


-- Nasty bit of convolution.
output :: Show c => (Song -> c) -> [Song] -> IO()
output a b = mapM_ (\x -> print $ a x) b 


readAPIkey :: IO String  
readAPIkey = do
           content <- readFile "/Users/morten/git/haskell/echonest/apikey.txt" 
           let line = lines content
           return $ head line

          
getAPIkey :: IO String 
getAPIkey = do
    result <- try readAPIkey :: IO (Either SomeException String)
    case result of
        Left ex  -> return $ "Caught exception: " ++ show ex
        Right val -> return val


printArtist :: String -> Int -> IO ()
printArtist a b  = do
    result <- search a b "artist" 
    case result of
        Left exception -> putStrLn $ show exception
        Right val -> output artistName $ songs val
           

printTitle :: String -> Int -> IO ()
printTitle a b  = do
    result <- search a b "title" 
    case result of
        Left exception -> putStrLn $ show exception
        Right val -> output title $ songs val


search :: String -> Int -> String -> IO(Either String Response)
search a b c = do 
   json <- searchRequest a b c
   let result = eitherDecode (BS.pack json) :: Either String Response
   return result


searchRequest :: String -> Int -> String -> IO String
searchRequest a b c = do 
          apikey <- getAPIkey
          let baseURL = "http://developer.echonest.com/api/v4/song/search?api_key=" ++ apikey ++ "&format=json&results=" ++ show b ++ "&" ++ c ++ "=" 
          resp <- simpleHTTP $ getRequest $ baseURL ++ urlEncode a
          body <- getResponseBody resp
          return body

