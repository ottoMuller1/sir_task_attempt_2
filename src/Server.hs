{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module Server ( runApp ) where

------------------------------imports------------------------------------
-- foreign imports
import Network.Wai.Handler.Warp ( run )
import Servant ( 
    ( :> ),
    Post,
    JSON,
    ReqBody ( .. ),
    Proxy ( .. ),
    Server,
    Application,
    Handler ( .. ),
    PlainText,
    serve )
import qualified Data.Text as T
import Control.Monad.IO.Class ( MonadIO ( liftIO ) )
import GHC.Generics ( Generic )
import Data.Aeson ( 
    FromJSON, 
    ToJSON,
    decode )
import qualified Network.HTTP.Simple as HTTP
import qualified Data.ByteString.Lazy.Char8 as B

------------------------------------types-----------------------------------
-- type to request
newtype RequestType = RequestType {
    text :: T.Text
} deriving Generic
instance ToJSON RequestType
instance FromJSON RequestType

-- type to response
data Grade a = Grade { 
    getGrade :: Int, 
    getMistakes :: ![ a ] 
} deriving Generic
instance FromJSON ( Grade T.Text )
instance ToJSON ( Grade T.Text )

-- type to Yandex Speller response
data Yandex = Yandex {
    code :: Int,
    pos :: Int,
    row :: Int,
    col :: Int,
    len :: Int,
    word :: T.Text,
    s :: [ T.Text ]
} deriving Generic
instance FromJSON Yandex
instance ToJSON Yandex
instance Show Yandex

----------------------------------api types---------------------------------
-- common type api for solution
type CommonApi =
    "check" :> "text" :> 
    ReqBody '[ JSON ] RequestType :>
    Post '[ JSON ] ( Grade T.Text )

-----------------------------------applications-----------------------------
-- api
commonApp :: Application
commonApp = serve proxy server
    where
    proxy :: Proxy CommonApi
    proxy = Proxy

    server :: Server CommonApi
    server = sendText
        where
        sendText :: RequestType -> Handler ( Grade T.Text )
        sendText ( RequestType txt ) = do 
            request <- HTTP.parseRequest $ T.unpack $ T.append
                ( T.pack "https://speller.yandex.net/services/spellservice.json/checkText?text=" ) $
                ( \letter -> if letter == ' ' then '+' else letter ) `T.map` txt
            response <- HTTP.httpLBS request
            case ( decode ( HTTP.getResponseBody response ) :: Maybe [ Yandex ] ) of
                Nothing -> 
                    return $ Grade 0 []
                Just body -> do
                    return $ Grade ( norm $ 5 - length body ) $ word <$> body

-----------------------------------running applications---------------------------
-- run application
runApp :: Int -> IO ()
runApp port = run port commonApp

------------------------------------features--------------------------------------
-- normalizing of answer
norm :: Int -> Int
norm n = if n <= 0 then 0 else n
