module Test.Main where
import Data.CSVGeneric
import Prelude
import Data.Either
import Data.Generic
import Data.Array as A
import Data.Eulalie.Parser as P
import Control.Bind ((=<<), join)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Eulalie.Result (ParseResult(Success, Error))
import Data.Eulalie.Stream (stream)
import Data.Eulalie.Success (ParseSuccess)
import Data.Maybe (fromMaybe, maybe, Maybe(Nothing, Just))
import Data.String (joinWith, split)
import Data.Traversable (sequence)
import Type.Proxy (Proxy(Proxy))
newtype WithEnum = WithEnum { enum1 :: AnEnum, enum2 :: AnEnum }
data AnEnum = X | Y | Z
derive instance gerericAnEnum :: Generic AnEnum
instance showAnEnum :: Show AnEnum where
  show = gShow
derive instance gerericWithEnum :: Generic WithEnum
instance showWithEnum :: Show WithEnum where
  show = gShow

newtype WithMaybe = WithMaybe { int :: Maybe Int, ms :: Maybe Int, me :: Maybe AnEnum }
derive instance genericWithMaybe :: Generic WithMaybe
instance showWithMaybe :: Show WithMaybe where
  show = gShow
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let p = Proxy :: (Proxy Something)
  let x  = fromArray (Proxy :: Proxy Simple) [{ recLabel : "foo", recValue : \_ -> (SString "Foo!")}] 
  log $ show x
  log $ show $ toSignature (Proxy :: Proxy (Maybe Int))
  let parser = sigToSpine $ toSignature (Proxy :: Proxy Int)
  log $ show $ (join (fromSpine <$> run parser "123") :: Maybe Int) -- Just 123
  let parser' = sigToSpine $ toSignature (Proxy :: Proxy (Maybe Int))  
  log $ show $ (join (fromSpine <$> run parser' "123") :: Maybe (Maybe Int)) -- Just Just 123
  log $ show $ (join (fromSpine <$> run parser' "") :: Maybe (Maybe Int)) -- Just Nothing
  log $ show $ (join (fromSpine <$> run parser' "bleh") :: Maybe (Maybe Int)) -- Nothing
  log $ show $ sequence $ ((map join $ sequence $ map fromSpine <$> run psep "foo,bar") :: (Array ((Maybe String)))) -- Just ["foo", "bar"]
  let p' = fullParse ["foo"] (Proxy :: Proxy Simple)
  let pi = fullParse ["int"] (Proxy :: Proxy JustInt)
  log $ showResult <$> P.parse pi $ stream "123"
  let p'' = fullParse ["int", "char", "bool"] (Proxy :: Proxy Primitives)
  log $ showResult <$>  P.parse p'' $ stream "5,3,false" -- Just (Main.Simple {foo: "bleh"})
  let px = fullParse' ["foo"]  (Proxy :: Proxy Simple)
  log $ genericShowPrec 0 $ getResult (P.parse px $ stream "5se")
  let px' = fullParse' ["int", "char", "bool"]  (Proxy :: Proxy Primitives)
  log $ genericShowPrec 0 $ getResult ((P.parse px' $ stream "5,3,false") :: ParseResult GenericSpine)
  let px'' = fullParse ["int", "char", "bool"]  (Proxy :: Proxy Primitives)
  log $  showResult <$> P.parse px'' $ stream "5,3,false,a" 
  let enump = fullParse ["enum1","enum2"]  (Proxy :: Proxy WithEnum)
  log $ showResult <$> P.parse enump $ stream "Z,X"
  let mp = fullParse ["int", "ms", "me"] (Proxy :: Proxy WithMaybe)
  log $ showResult <$> P.parse mp $ stream "12,,"
  log $ showResult $ P.parse mp $ stream "8988,2,X" -- Right (WithMaybe { int : })
  log $ showResult $ P.parse mp $ stream "8988,,X" -- Right (WithMaybe { int : })
  log $ showResult $ P.parse mp $ stream ",4,X" -- Right (WithMaybe { int : })
  log $ showResult $ P.parse mp $ stream "12,,X" -- Right (WithMaybe { int : })
  log $ showResult $ P.parse mp $ stream "12,," -- Right (WithMaybe { int : })
  log $ showResult $ P.parse mp $ stream ",,," -- Right (WithMaybe { int : })
  log $ showResult $ P.parse mp $ stream "a,,X" -- Left failure
  log $ showResult $ P.parse mp $ stream "12," -- Left failure
       
getResult (Success r) = r.value
showLabels x = "reclabel : " <> x.recLabel <> "recValue : " <> (show $ force x.recValue)
mapResult f (Success r) = Success $ f r.value
mapResult _ e          = e

showResult (Success r) = show r.value
showResult (Error e) = "Expected one of:" <> show e.expected <> "at " <> show e.input


data Something = Foo | Bar
derive instance genericFoo :: Generic Something
instance showSomething :: Show Something where
  show = gShow 

newtype Simple = Simple { foo :: String }
derive instance gerericSimple :: Generic Simple
instance showSimple :: Show Simple where
  show = gShow
  
newtype JustInt = JustInt { int :: Int }
derive instance gerericJustInt :: Generic JustInt
instance showJustInt :: Show JustInt where
  show = gShow
  
newtype Primitives = Primitives { int :: Int
                                , char :: Char
                                , bool :: Boolean}

-- for sum types just pick based on which field names match the headers.

derive instance gerericPrimitives :: Generic Primitives
instance showPrimitives :: Show Primitives where
  show = gShow

