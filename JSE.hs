{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module JSE (Config(..),
            Filter(..),
            readFilterSpec,
            Field,
            pipeline,
            Value) where

import           Blaze.ByteString.Builder (Builder, toByteStringIO)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson (json)
import           Data.Aeson.Encode (fromValue)
import           Data.Aeson.Types (Object, Value(String, Object))
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

data Filter = ValueFilter Field Text Bool |
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

c2w8 :: Char -> Word8
c2w8 = fromIntegral . fromEnum

restrictFields :: [Text] -> Object -> Object
restrictFields fs = M.filterWithKey flt
  where flt f _ = f `elem` fs

readFilterSpec :: Bool -> Text -> Filter
readFilterSpec cSensitive str = readFilterSpec' cSensitive field value
  where (field, value) = spanSkip ':' str

readFilterSpec' :: Bool -> Text -> Text -> Filter
readFilterSpec' cSensitive f "" = ValueFilter f T.empty cSensitive
readFilterSpec' cSensitive f v  = maybe valF (const patF) $ match isRegex bsValue []
  where isRegex  = compile "^/.*/$" []
        valF     = ValueFilter f v cSensitive
        patF     = PatternFilter f $ compile (encodeUtf8 . stripSlashes $ v) opts
        opts     = if cSensitive then []
                   else               [caseless]
        bsValue  = encodeUtf8 v

spanSkip :: Char -> Text -> (Text, Text)
spanSkip pred xs = (left, T.tail right)
  where (left, right) = T.span (/= pred) xs


stripSlashes :: Text -> Text
stripSlashes = T.tail . T.init

objectMatchesAny :: [Filter] -> Object -> Bool
objectMatchesAny [] obj = True
objectMatchesAny fs obj = any (objectMatches obj) fs

objectMatches :: Object -> Filter -> Bool
objectMatches obj (ValueFilter f v cSensitive) = isJust $ fmap matcher $ M.lookup f obj
  where matcher (String text) = if cSensitive then text == v
                                else               CI.mk text == CI.mk v
        matcher _             = False
objectMatches obj (PatternFilter f reg) = isJust $ fmap matcher $ M.lookup f obj
  where matcher (String text) = isJust $ match reg (encodeUtf8 text) []
        matcher _             = False

getHandle :: FilePath -> IO Handle
getHandle "-" = return stdin
getHandle fp  = openFile fp ReadMode
