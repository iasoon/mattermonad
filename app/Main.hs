{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Concurrent             ( forkIO )
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString               as BS
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8         as S8
import qualified Data.ByteString.Lazy.Char8    as L8
import qualified Data.CaseInsensitive          as CI
import           Data.Functor                   ( (<&>) )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Data.Text.IO                  as T
import qualified Data.Yaml                     as Yaml
import           Data.Maybe                     ( fromJust )
import           Data.Either                    ( fromRight )
import           Debug.Trace
import           Lib.HTTP
import           Lib.Session
import           Lib.Types
import           Lib.Utils
import           Lib.WebSocket
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Header      ( ResponseHeaders )
import           Network.HTTP.Types.Status      ( statusCode )
import qualified Reflex                        as R
import qualified Reflex.Host.Headless          as R
import           OpenAPI.Test

main :: IO ()
main = R.runHeadlessApp botMonad

botMonad :: forall t m . R.MonadHeadlessApp t m => m (R.Event t ())
botMonad = mdo
  loginReq :: LoginReq <- liftIO $ Yaml.decodeFileThrow "credentials.yaml"
  clientHttpManager    <- liftIO $ newManager tlsManagerSettings

  let clientServerData = Server { serverHost     = "mattermost.zeus.gent"
                                , serverPort     = 80
                                , serverBasePath = ""
                                }

  loginResp <- runReaderT (makeLoginReq loginReq >>= runReq)
                          ApiClientConfig { .. }
  let token = fromJust $ getHeader loginResp "Token"

  -- (eWS, callback) <- R.newTriggerEvent
  -- liftIO $ forkIO $ connectWS clientServerData token callback

  -- let wsEvents :: R.Event t (WSEvent Value) = R.mapMaybe decode eWS
  --     (others, posted) = R.fanEither $ wsEvents <&> \WSEvent {..} ->
  --       case wsEventEvent of
  --         "posted" -> case fromJSON wsEventData of
  --           Success e -> Right (e :: EventPosted)
  --           Error   e -> trace e $ Left $ WSEvent { .. }
  --         _ -> Left $ WSEvent { .. }
  --     dms = R.ffilter ((== "D") . eventPostedChannelType) posted

  let session = SessionData { sessionClient = ApiClientConfig { .. }
                            , sessionToken  = token
                            }
  flip runReaderT session $ do
    let req = GetChannel { getChannelTeamName       = "zeus"
                         , getChannelChannelName    = "bot-monad-testkanaal"
                         , getChannelIncludeDeleted = Nothing
                         }
    channelId <- chanDataId . justDecode <$> runApiRequest req
    liftIO . putStrLn $ show channelId
    let announce = CreatePost
          { createPostPayload   = CreatePostPayload
                                    { createPostPayloadChannelId = channelId
                                    , createPostPayloadMessage   =
                                      "IK BEN DE BOT-MONAD"
                                    , createPostPayloadProps     = Nothing
                                    , createPostPayloadFileIds   = Nothing
                                    , createPostPayloadRootId    = Nothing
                                    }
          , createPostSetOnline = Nothing
          }
    resp <- runApiRequest announce
    return ()

  -- flip runReaderT session $ do
  --   me <- userId <$> runReq GetUser { getUserId = "me" }
  --   let incomingDms = R.ffilter ((/= me) . postUserId . eventPostedPost) dms
  --   R.performEvent_ $ incomingDms <&> \e -> do
  --     let chanId = postChannelId . eventPostedPost $ e
  --     runReq $ CreatePostReq { createPostChannelId = chanId
  --                            , createPostMessage   = "IK BEN DE BOT-MONAD"
  --                            }
  --     return ()
  pure R.never

justDecode :: forall a . (FromJSON a) => Response L8.ByteString -> a
justDecode = either error id . eitherDecode . responseBody
