module Main where

import Data.ByteString.Lazy.UTF8
import Data.Maybe
import Network.HTTP.Conduit
import Network.URI
import Text.HTML.TagSoup

main :: IO ()
main = do
  putStrLn "hello world"


download :: String -> IO String
download url = do res <- simpleHttp url
                  return (toString res)

saveAs :: String -> Int -> IO ()
saveAs url k =
  do content <- download url
     writeFile (makeFileName k) content

makeFileName :: Int -> FilePath 
makeFileName k = "download-" ++ show k ++ ".html"

processPage :: String -> IO [String]
processPage url = do
  page <- download url
  return (links url page)

nofollow :: Tag String -> Bool
nofollow tag = fromAttrib "rel" tag == "nofollow"


links :: String -> String -> [String]
links url =
  catMaybes .
  map (canonicalize url) .
  filter (not . null) .
  map (fromAttrib "href") .
  filter (\t -> fromAttrib "rel" t /= "nofollow") .
  filter (isTagOpenName "a") .
  canonicalizeTags .
  parseTags
  

canonicalizeLink :: String -> String -> Maybe String
canonicalizeLink referer path =
  case parseURI referer of
    Nothing -> Nothing
    Just r  ->
      case parseURIReference path of
        Nothing -> Nothing
        Just p  -> Just (uriToString id (nonStrictRelativeTo p r) "")

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind  Nothing      _     = Nothing
bind (Just value) action = action value

canonicalizeLink1 :: String -> String -> Maybe String
canonicalizeLink1 referer path =
  parseURI referer                `bind`
   \r -> parseURIReference path   `bind`
    \p -> Just (uriToString id (nonStrictRelativeTo p r) "")

canon :: String -> String -> Maybe String
canon referer path =
  parseURI referer         `bind` \r ->
  parseURIReference path   `bind` \p ->
  Just (uriToString id (nonStrictRelativeTo p r) "")

canon1 :: String -> String -> Maybe String
canon1 referer path =
  parseURI referer >>= \r ->
  parseURIReference path >>= \p ->
  Just (uriToString id (nonStrictRelativeTo p r) "")

canonicalize :: String -> String -> Maybe String
canonicalize referer path = do
  r <- parseURI referer
  p <- parseURIReference path
  return (uriToString id (nonStrictRelativeTo p r) "")

process' :: String -> String -> [Maybe String]
process' url =
   map (canonicalize url) .
   filter (not . null) .
   map (fromAttrib "href") .
   filter (\t -> fromAttrib "rel" t /= "nofollow") .
   filter (isTagOpenName "a") .
   canonicalizeTags .
   parseTags