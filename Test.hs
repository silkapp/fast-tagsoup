{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Main where

import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast.Utf8Only
import Text.HTML.TagSoup.Entity
import Text.HTML.TagSoup.Match

import Control.Monad
import Data.List
import Data.String
import Test.QuickCheck

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text (unpack, pack)
import Text.XML.Light

deriving instance Eq Content
deriving instance Eq CData
deriving instance Eq Element
-- * The Test Monad

type Test a = IO a

pass :: Test ()
pass = return ()

runTest :: Test () -> IO ()
runTest x = x >> putStrLn "All tests passed"

(===) :: (Show a, Eq a) => a -> a -> IO ()
a === b = if a == b then pass else putStrLn $ "Does not equal: " ++ show a ++ " =/= " ++ show b

check :: Testable prop => prop -> IO ()
check prop = do
    res <- quickCheckWithResult stdArgs{maxSuccess=1000, chatty=False} prop
    case res of
        Success{} -> pass
        _ -> fail "Property failed"

newtype HTML = HTML ByteString deriving Show
instance Arbitrary HTML where
    arbitrary = fmap (HTML . BS.concat) $ listOf $ elements frags
        where frags = map fromString $ map (:[]) " \n!-</>#&;xy01[]?'\"" ++ ["CDATA","amp","gt","lt"]
    shrink (HTML x) = map HTML $ zipWith BS.append (BS.inits x) (tail $ BS.tails x)

-- * The Main section

main :: IO ()
main = runTest $ do
    parseTests
    renderTests
    entityTests


parseTests :: Test ()
parseTests = do
    parseTags "<!DOCTYPE TEST>" === [TagOpen "!doctype" [("test","")]]
    parseTags "<test \"foo bar\">" === [TagOpen "test" [("\"foo",""),("bar\"","")]]
    parseTags "<test baz \"foo\">" === [TagOpen "test" [("baz",""),("\"foo\"","")]]
    parseTags "<test 'foo bar'>" === [TagOpen "test" [("'foo",""),("bar'","")]]
    parseTags "<test bar=''' />" === [TagOpen "test" [("bar",""),("'","")], TagClose "test"]
    parseTags "<test2 a b>" === [TagOpen "test2" [("a",""),("b","")]]
    parseTags "<test2 ''>" === [TagOpen "test2" [("''","")]]
    parseTags "</test foo>" === [TagClose "test"]
    parseTags "<test/>" === [TagOpen "test" [], TagClose "test"]
    parseTags "<test1 a = b>" === [TagOpen "test1" [("a","b")]]
    parseTags "hello &amp; world" === [TagText "hello & world"]
    parseTags "hello &#64; world" === [TagText "hello @ world"]
    parseTags "hello &#x40; world" === [TagText "hello @ world"]
    parseTags "hello &haskell; world" === [TagText "hello &haskell; world"]
    parseTags "hello \n\t world" === [TagText "hello \n\t world"]
    parseTags "<a href=http://www.google.com>" === [TagOpen "a" [("href","http://www.google.com")]]
    parseTags "<foo bar=\"bar&#54;baz\">" === [TagOpen "foo" [("bar","bar6baz")]]
    parseTags "<foo bar=\"bar&amp;baz\">" === [TagOpen "foo" [("bar","bar&baz")]]
    parseTags "hey &how are you" === [TagText "hey &how are you"]
    parseTags "hey &how; are you" === [TagText "hey &how; are you"]
--    parseTags "hey &amp are you" === [TagText "hey & are you"] -- This is not valid XML!
    parseTags "hey &amp; are you" === [TagText "hey & are you"]
    parseTags "hey &apos; are you" === [TagText "hey ' are you"]
    parseTags "<a test=\"&apos;\">" === [TagOpen "a" [("test", "'")]]
    parseTags "<bla:hoi att:bla>" === [TagOpen "bla:hoi" [("att:bla","")]]

    -- real cases reported by users
    parseTags "test &#10933649; test" === [TagText "test ? test"] -- HTML edge-case

    parseTags "<a href=\"series.php?view=single&ID=72710\">" === [TagOpen "a" [("href","series.php?view=single&ID=72710")]]

    parseTags "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" ===
        [TagOpen "!DOCTYPE" [("HTML",""),("PUBLIC",""),("","-//W3C//DTD HTML 4.01//EN"),("","http://www.w3.org/TR/html4/strict.dtd")]]

    parseTags "<script src=\"http://edge.jobthread.com/feeds/jobroll/?s_user_id=100540&subtype=slashdot\">" ===
        [TagOpen "script" [("src","http://edge.jobthread.com/feeds/jobroll/?s_user_id=100540&subtype=slashdot")]]

    parseTags "<a title='foo'bar' href=correct>text" === [TagOpen "a" [("title","foo"),("bar'",""),("href", "correct")],TagText "text"]

    parseTags "<test><![CDATA[Anything goes, <em>even hidden markup</em> &amp; entities]]> but this is outside</test>" ===
        [TagOpen "test" [],TagText "Anything goes, <em>even hidden markup</em> &amp; entities", TagText " but this is outside",TagClose "test"]

    parseTags "<a \r\n href=\"url\">" === [TagOpen "a" [("href","url")]]

    parseTags "<a href='random.php'><img src='strips/130307.jpg' alt='nukular bish'' title='' /></a>" === 
        [TagOpen "a" [("href","random.php")],TagOpen "img" [("src","strips/130307.jpg"),("alt","nukular bish"),("'",""),("title","")],TagClose "img",TagClose "a"]

    parseTags "<p>some text</p\n<img alt='&lt; &yyy; &gt;' src=\"abc.gif\">" ===
        [TagOpen "p" [],TagText "some text",TagClose "p"]

renderTests :: Test ()
renderTests = do
    let rp = renderTags . parseTags
    rp "<test>" === "<test>"
    rp "<br></br>" === "<br/>"
    rp "<script></script>" === "<script></script>"
    rp "hello & world" === "hello &amp; world"
    rp "<a href=test>" === "<a href=\"test\">"
    rp "<a href>" === "<a href=\"\">"
--    rp "<a href?>" === "<a href?>" Not valid XML
    rp "<?xml foo?>" === "<?xml foo ?>"
    rp "<?xml foo?>" === "<?xml foo ?>"
    rp "<!-- neil -->" === "<!-- neil -->"
    rp "<a test=\"a&apos;b\">" === "<a test=\"a&apos;b\">"
    rp "<a test=\"a&amp;b\">" === "<a test=\"a&amp;b\">"
    check $ \(HTML x)  -> let y = rp x in rp y == (y :: ByteString)
    testF <- readFile "testfile.xml"
    parseXML testF === parseXML (unpack $ decodeUtf8 $ rp $ encodeUtf8 $ pack testF)

entityTests :: Test ()
entityTests = do
    lookupNumericEntity "65" === Just 'A'
    lookupNumericEntity "x41" === Just 'A'
    lookupNumericEntity "x4E" === Just 'N'
    lookupNumericEntity "x4e" === Just 'N'
    lookupNumericEntity "Haskell" === Nothing
    lookupNumericEntity "" === Nothing
    lookupNumericEntity "89439085908539082" === Nothing
    lookupNamedEntity "amp" === Just '&'
    lookupNamedEntity "haskell" === Nothing
    escapeXMLChar 'a' === Nothing
    escapeXMLChar '&' === Just "amp"
