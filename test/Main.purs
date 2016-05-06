module Test.Main where
import Data.CSVGeneric
import Prelude
import Data.Traversable (sequence)
import Data.Maybe (fromMaybe, maybe, Maybe(Nothing, Just))
import Data.String (joinWith, split)
import Data.Eulalie.Result (ParseResult(Success, Error))
import Data.Eulalie.Stream (stream)
import Data.Eulalie.Success (ParseSuccess)
import Data.Generic
import Control.Bind ((=<<), join)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Eulalie.Parser as P
import Type.Proxy (Proxy(Proxy))
import Data.Array as A 
newtype WithEnum = WithEnum { enum1 :: AnEnum, enum2 :: AnEnum }
data AnEnum = X | Y | Z
derive instance gerericAnEnum :: Generic AnEnum
instance showAnEnum :: Show AnEnum where
  show = gShow
derive instance gerericWithEnum :: Generic WithEnum
instance showWithEnum :: Show WithEnum where
  show = gShow
--main :: forall e. Eff (console :: CONSOLE | e) Unit
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
  log $ show $ join $ run p' "bleh" -- Just (Main.Simple {foo: "bleh"})
  let pi = fullParse ["int"] (Proxy :: Proxy JustInt)
  log $ showResult <$> P.parse pi $ stream "123"
--  let p'' = fullParse ["int", "char", "bool"] (Proxy :: Proxy Primitives)
--  log $ showResult <$>  P.parse p'' $ stream "5,3,false" -- Just (Main.Simple {foo: "bleh"})
  let px = fullParse' ["foo"]  (Proxy :: Proxy Simple)
  log $ genericShowPrec 0 $ getResult (P.parse px $ stream "5se")
  let px' = fullParse' ["int", "char", "bool"]  (Proxy :: Proxy Primitives)
  log $ genericShowPrec 0 $ getResult ((P.parse px' $ stream "5,3,false") :: ParseResult GenericSpine)
  let px'' = fullParse ["int", "char", "bool"]  (Proxy :: Proxy Primitives)
  log $ showResult <$> P.parse px'' $ stream "5,3,false,a" 
  let enump = fullParse ["enum1","enum2"]  (Proxy :: Proxy WithEnum)
  log $ showResult <$> P.parse enump $ stream "Z,X"
       
--  log $ show $ fromJust $ getResult x2
pi = fullParse ["int"] (Proxy :: Proxy JustInt)
x2 =  P.parse pi $ stream "123"
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

genericShowPrec :: Int -> GenericSpine -> String
genericShowPrec d (SProd s arr) =
    if A.null arr
    then s
    else showParen (d > 10) $ s <> " " <> joinWith " " (map (\x -> genericShowPrec 11 (x unit)) arr)
  where showParen false x = x
        showParen true  x = "(" <> x <> ")"

genericShowPrec d (SRecord xs) = "{" <> joinWith ", " (map (\x -> x.recLabel <> ": " <> genericShowPrec 0 (x.recValue unit)) xs) <> "}"
genericShowPrec d (SBoolean x) = show x
genericShowPrec d (SInt x)     = show x
genericShowPrec d (SNumber x)  = show x
genericShowPrec d (SString x)  = show x
genericShowPrec d (SChar x)    = show x
genericShowPrec d (SArray xs)  = "[" <> joinWith ", "  (map (\x -> genericShowPrec 0 (x unit)) xs) <> "]"
