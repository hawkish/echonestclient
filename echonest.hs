{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
-- import Text.JSON
-- import Text.JSON.Generic
import Network.HTTP (getResponseBody, getRequest, simpleHTTP, urlEncode)
import Data.List (isInfixOf, or)
import qualified Data.Text as T
import Control.Exception
import Prelude hiding (catch, lookup)
import Data.Aeson
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad
import Data.Configurator
import Data.Configurator.Types
import Data.Maybe


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

-- Could have used Generics. Decided not to.
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
printSearch :: Show c => (Song -> c) -> [Song] -> IO()
printSearch a b = mapM_ (\x -> print $ a x) b 


-- Example: 'searchByArtist "Kenny Rogers" title 10' returns 10 Kenny Rogers songtitles. 
searchByArtist :: Show c => String -> (Song -> c) -> Int -> IO ()
searchByArtist a c b  = do
    result <- search a b "artist" 
    case result of
        Left exception -> putStrLn $ show exception
        Right val -> printSearch c $ songs val
           
-- Example: 'searchByTitle "Somewhere over the rainbow" artistName 10' returns 10 artists doing that song. 
searchByTitle :: Show c => String -> (Song -> c) -> Int -> IO ()
searchByTitle a c b  = do
    result <- search a b "title" 
    case result of
        Left exception -> putStrLn $ show exception
        Right val -> printSearch artistName $ songs val


search :: String -> Int -> String -> IO(Either String Response)
search a b c = do 
   json <- searchRequest a b c
   case json of
        Left ex -> return $ Left ex
        Right val -> do
              let result = eitherDecode (BS.pack val) :: Either String Response
              return result


searchRequest :: String -> Int -> String -> IO (Either String String)
searchRequest a b c = do 
          result <- parseAPIkey
          case result of
               Left ex -> return $ Left ex
               Right val -> do
                     let baseURL = "http://developer.echonest.com/api/v4/song/search?api_key=" ++ val ++ "&format=json&results=" ++ show b ++ "&" ++ c ++ "=" 
                     resp <- simpleHTTP $ getRequest $ baseURL ++ urlEncode a
                     body <- getResponseBody resp
                     return $ Right body


parseAPIkey :: IO (Either String String) 
parseAPIkey = do
    result <- loadConfigFile
    case result of
        Left ex  -> return $ Left (show ex)
        Right val -> do
                  apikey <- lookup val "config.apikey" :: IO (Maybe String)
                  return $ Right (fromJust apikey)



loadConfigFile :: IO (Either SomeException Config)
loadConfigFile = do 
               result <- try (load [Required "/Users/morten/git/echonestclient/app.cfg"]) :: IO (Either SomeException Config)
               return result
