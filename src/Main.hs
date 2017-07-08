module Main (main) where
    
import qualified Data.ByteString.Lazy.UTF8 as BS
import Control.Exception.Base
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Network.HTTP.Conduit
import Network.URI
import Text.HTML.TagSoup

main :: IO ()
main = do
  results <- spider 1000 "http://haskell.org"
  mapM_ putStrLn $ printMap results 

download :: String -> IO String
download url = do res <- simpleHttp url
                  return (BS.toString res)

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


data Link = Link { linkTo   :: String
                 , linkFrom :: [String]
                 } 

visited :: String -> [Link] -> Bool
visited url = elem url . map linkTo  --Traag bij veel links!

type URL = String

spider :: Int -> URL -> IO (Map.Map URL [URL])
spider count url0 = go 0 Map.empty (Set.singleton url0)
  where
    go k seen queue0
        | k >= count = return seen
        | otherwise  =
      case Set.minView queue0 of
        Nothing -> return seen
        Just (url, queue) -> do
          -- putStrLn ("Start downloading "++url)
          page' <- safeDownload url
          case page' of 
            Left err -> 
               do putStrLn ("failed to download "++url)
                  putStrLn ("   "++showException err)
                  go k seen queue
            Right page ->
               do putStrLn (show k ++") "++url)
                  let ls       = links url page
                      newSeen  = Map.insert url ls seen
                      notSeen  = Set.fromList .
                                    filter (`Map.notMember` newSeen) $ ls
                      newQueue = queue `Set.union` notSeen
                  go (k+1) newSeen newQueue

printMap ::  (Show k , Show v) => Map.Map k v -> [String]
printMap = map printRow . Map.toList
  where
    printRow (k,v) = show k ++ ", "++show v

safeDownload :: String -> IO (Either HttpException String )
safeDownload url = try (download url)

showException :: HttpException -> String
showException e =
  case e of 
    HttpExceptionRequest _ x ->
      case x of 
        StatusCodeException resp _ 
           -> show . responseStatus $ resp
        _ -> show x
    InvalidUrlException _ reason   -> "Invalid url ("++reason++")" 
