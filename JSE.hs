{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module JSE (Config(..),
            Filter(..),
            readFilterSpec,
            Field,
            pipeline,
            Value) where

import           Blaze.ByteString.Builder (Builder, toByteStringIO)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson.Parser (json, value)
import           Data.Aeson.Encode (fromValue)
import           Data.Aeson.Types (Object, Value(..))
import           Data.Attoparsec (parse, maybeResult)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.Data (Data)
import           Data.Enumerator (Enumerator,
                                  Enumeratee,
                                  Iteratee,
                                  tryIO,
                                  continue,
                                  yield,
                                  Stream(Chunks, EOF),
                                  joinI,
                                  (=$),
                                  ($$))
import           Data.Enumerator.Binary (splitWhen)
import qualified Data.Enumerator.List as EL
import qualified Data.Map as M
import           Data.Maybe (isJust, fromJust)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           System.IO (Handle, stdin, openFile, IOMode(ReadMode))
import           Text.Regex.PCRE.Light (Regex, compile, match, caseless)

data Config = Config { returnFields :: [Text],
                       source       :: Enumerator ByteString IO (),
                       filters      :: [Filter] }

type Field = Text

data Filter = StringFilter Field Text Bool | -- JSON string
              ValueFilter Field Value | -- JSON value (not string)
              PatternFilter Field Regex deriving (Show)

pipeline :: Config -> Iteratee ByteString IO ()
pipeline Config {source         = s,
                 returnFields   = rfs,
                 filters = fs } = s $$ splitLines
                                    =$ parseLine
                                    =$ filterObjects fs
                                    =$ restrictObjects rfs
                                    =$ objectAsValue
                                    =$ encoder
                                    =$ output

splitLines :: Monad m => Enumeratee ByteString ByteString m b
splitLines = splitWhen (== nl)
  where nl = c2w8 '\n'

left =. right = \step_i -> joinI $ left $$ right step_i

parseLine :: Monad m => Enumeratee ByteString Object m b
parseLine = parseRaw =. keepSuccessful =. unwrap
  where parseRaw                     = EL.map $ maybeResult . parse json
        keepSuccessful               = EL.filter isJust
        justObject (Just (Object _)) = True
        justObject _                 = False
        unwrap                       = EL.map (\(Just (Object o)) -> o)

filterObjects :: Monad m => [Filter] -> Enumeratee Object Object m b
filterObjects fs = EL.filter (objectMatchesAny fs)

restrictObjects :: Monad m => [Text] -> Enumeratee Object Object m b
restrictObjects rfs = EL.map (restrictFields rfs)

objectAsValue :: Monad m => Enumeratee Object Value m b
objectAsValue = EL.map Object

encoder :: Monad m => Enumeratee Value Builder m b
encoder = EL.map fromValue

output :: MonadIO m => Iteratee Builder m ()
output = continue step
  where step EOF    = yield () EOF
        step (Chunks []) = continue step
        step (Chunks builders) = do
          tryIO $ mapM_ (toByteStringIO BS.putStrLn) builders
          continue step

collectMatches'' :: Monad m => Filter -> Enumeratee Object Bool m b
collectMatches'' f = EL.map (`objectMatches` f)

collectMatches' :: Monad m => Enumeratee (Maybe [ByteString]) (Maybe [ByteString]) m b
collectMatches' = EL.filter isJust

collectMatches (PatternFilter f reg) = EL.map (\obj -> fuck $ finder obj)
  where getMatch (String text) = match reg (encodeUtf8 text) []
        fuck (Nothing) = Nothing
        fuck (Just x) = getMatch x
        finder obj = M.lookup f obj

c2w8 :: Char -> Word8
c2w8 = fromIntegral . fromEnum

-- If no fields are specified, keep them all
restrictFields :: [Text] -> Object -> Object
restrictFields [] = id
restrictFields fs = M.filterWithKey flt
  where flt f _ = f `elem` fs

readFilterSpec :: Bool -> Text -> Filter
readFilterSpec cSensitive str = readFilterSpec' cSensitive field value
  where (field, value) = spanSkip ':' str

readFilterSpec' :: Bool -> Text -> Text -> Filter
readFilterSpec' cSensitive field "" = StringFilter field T.empty cSensitive
readFilterSpec' cSensitive field v  = maybe valF (const patF) $ match isRegex bsValue []
  where isRegex   = compile "^/.*/$" []
        valF      = case jsonValue of
                    (String str) -> StringFilter field str cSensitive
                    _            -> ValueFilter field jsonValue
        jsonValue = case maybeResult $ parse value $ encodeUtf8 v of
                      Just val -> val
                      -- CmdArgs eats double quotes around string values
                      Nothing  -> String v
        patF      = PatternFilter field $ compile (encodeUtf8 . stripSlashes $ v) opts
        opts      = if cSensitive then []
                    else               [caseless]
        bsValue   = encodeUtf8 v

spanSkip :: Char -> Text -> (Text, Text)
spanSkip pred xs = (left, T.tail right)
  where (left, right) = T.span (/= pred) xs


stripSlashes :: Text -> Text
stripSlashes = T.tail . T.init

objectMatchesAny :: [Filter] -> Object -> Bool
objectMatchesAny [] obj = True
objectMatchesAny fs obj = any (objectMatches obj) fs

objectMatches :: Object -> Filter -> Bool
objectMatches obj (ValueFilter f v) = maybe False (== v) $ M.lookup f obj
objectMatches obj (StringFilter f v cSensitive) = maybe False matcher $ M.lookup f obj
  where matcher (String text) = if cSensitive then text == v
                                else               CI.mk text == CI.mk v
        matcher _             = False
objectMatches obj (PatternFilter f reg) = maybe False matcher $ M.lookup f obj
  where matcher (String text) = isJust $ match reg (encodeUtf8 text) []
        matcher _             = False

getHandle :: FilePath -> IO Handle
getHandle "-" = return stdin
getHandle fp  = openFile fp ReadMode
