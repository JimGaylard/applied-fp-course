{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main (runApp) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           FirstApp.Types           (ContentType (JSON, PlainText), Error (EmptyComment, EmptyTopic, InvalidRequest),
                                           RqType (AddRq, ListRq, ViewRq),
                                           mkCommentText, mkTopic,
                                           renderContentType)

-- --------------------------------------------
-- - Don't start here, go to FirstApp.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse status ct =
  responseLBS status [("ContentType", renderContentType ct)]

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 = mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 ct _ = mkResponse status404 ct ""

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 = mkResponse status400

-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest topic comment =
  AddRq <$> mkTopic topic <*> mkCommentText (decodeUtf8 $ LBS.toStrict comment)


-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.
mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest topic = ViewRq <$> mkTopic topic

mkListRequest
  :: Either Error RqType
mkListRequest = return ListRq

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse EmptyTopic = resp400 PlainText "topic in request was empty"
mkErrorResponse EmptyComment = resp400 PlainText "comment text in request was empty"
mkErrorResponse InvalidRequest = resp400 PlainText "Invalid path or body"


-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest req = do
  commentText <- strictRequestBody req
  case (pathInfo req, requestMethod req) of
    ([topic, "add"], "POST") -> return $ mkAddRequest topic commentText
    ([topic, "view"], "GET") -> return $ mkViewRequest topic
    (["list"], "GET")        -> return mkListRequest
    _                        -> return $ Left InvalidRequest


-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest (AddRq _ _) = Right (resp200 PlainText "AddRq not implemented yet")
handleRequest (ViewRq _) = Right (resp200 PlainText "ViewRq not implemented yet")
handleRequest ListRq = Right (resp200 PlainText "ViewRq not implemented yet")

-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
app
  :: Application
app req respond = do
  reqType <- mkRequest req
  case reqType >>= handleRequest of
    Left err   -> respond $ mkErrorResponse err
    Right resp -> respond resp


runApp :: IO ()
runApp = run 3000 app
