module Main where

import Data.Generic
import Prelude
import Data.Array as A
import Data.Array.Unsafe as Unsafe
import Data.Eulalie.Char as C
import Data.Eulalie.Parser as P
import Data.Eulalie.String as S
import Data.Int as Int
import Control.Alt ((<|>), alt)
import Control.Apply ((<*), (*>))
import Control.Bind ((=<<), join)
import Control.Monad (when)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Plus (empty, class Plus)
import Data.Array (zip, last)
import Data.Eulalie.Parser (Parser)
import Data.Eulalie.Result (ParseResult(Success, Error))
import Data.Eulalie.Stream (stream)
import Data.Eulalie.Success (ParseSuccess)
import Data.Foldable (fold, class Foldable, foldr)
import Data.Maybe (fromMaybe, maybe, Maybe(Nothing, Just))
import Data.Maybe.Unsafe (fromJust)
import Data.Monoid (mempty)
import Data.String (joinWith, split)
import Data.Traversable (sequence)
import Data.Tuple (snd, fst, Tuple(Tuple))
import Type.Proxy (Proxy(Proxy))

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
  
fromShow a = S.string (show a) *> (P.succeed a)


genericRead :: forall a. Generic a => String -> Proxy a -> Maybe a
genericRead s p =
  case (toSignature p) of
    (SigProd _ cs) -> oneOf (map step cs)
    _ -> Nothing
 where
  step constructor = do
    -- leave only last part of constructor ie. Main.Foo -> Foo
    let fullConstructorName = constructor.sigConstructor
    constructorName <- last <<< split "." $ fullConstructorName
    if constructorName == s
      -- this example only works for construcotrs of kind `*`
      then fromSpine $ SProd fullConstructorName []
      else Nothing
           

-- this helper will be available in purescript-foldable-traversable 1.0
oneOf :: forall f g a. (Foldable f, Plus g) => f (g a) -> g a
oneOf = foldr alt empty


data Something = Foo | Bar
derive instance genericFoo :: Generic Something
instance showSomething :: Show Something where
  show = gShow

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let p = Proxy :: (Proxy Something)
  log <<< show <<< genericRead "Foo" $ p
  log <<< show <<< genericRead "Bar" $ p
  log <<< show <<< genericRead "Baz" $ p
  let x  = fromArray (Proxy :: Proxy Simple) [{ recLabel : "foo", recValue : \_ -> (SString "Foo!")}] 

  log $ show x
--  log <<< show <<< fromSpine (SigProd "Simple" [{sigConstructor : "Simple"
--                                              , sigValues : [\_ -> (SigRecord [{ recLabel : "foo"
--                                                          , recValue : \_ -> SigString "Foo!"}])]}])

  --let x = fromSpine (SProd "Main.Simple" [\_ -> (SRecord [{ recLabel : "foo" , recValue : \_ -> (SString "Foo!")}])]) :: Maybe Simple
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
  log "fon"
  log $ show $ (fromSpine $ SProd "Main.Simple"
                [\_ -> SRecord [{ recLabel : "foo",
                                  recValue : \_ -> SString "5se"}] ]) :: Maybe Simple
  log $ show $ (fromSpine $ SProd "Main.Primitives"
                [\_ -> SRecord [{ recLabel : "bool", recValue : \_ -> SBoolean false  }
                              , { recLabel : "char", recValue : \_ -> SChar 'a'}
                       , { recLabel : "int", recValue : \_ -> SInt 3 }  ]] ) :: Maybe Primitives
  let px'' = fullParse ["int", "char", "bool"]  (Proxy :: Proxy Primitives)
  log $ showResult <$> P.parse px'' $ stream "5,3,false,a"
                                                                            
       
--  log $ show $ Primitives {int: 5, char: '3', bool: false}
--  log $ show $ fromJust $ getResult x2
-- doesn't seem to work for multiple fields 

pi = fullParse ["int"] (Proxy :: Proxy JustInt)
x2 =  P.parse pi $ stream "123"
getResult (Success r) = r.value
showLabels x = "reclabel : " <> x.recLabel <> "recValue : " <> (show $ force x.recValue)
mapResult f (Success r) = Success $ f r.value
mapResult _ e          = e
showResult (Success r) = show r.value
showResult (Error e) = "Expected one of:" <> show e.expected <> "at " <> show e.input

--fullParse :: Array String -> GenericSignature -> Parser GenericSpine
--fullParse header sig@(SigProd _ arr) = do
--parseCSV :: forall a. (Generic a) => Proxy a -> String -> Parser a
--parseCSV pa s = ?what
fullParse :: forall a. (Generic a) => Array String -> Proxy a -> Parser (Maybe a)
fullParse h x = fromSpine <$> (fullParse' h x)
fullParse' :: forall a. (Generic a) => Array String -> Proxy a -> Parser GenericSpine
fullParse' header p = do
  fields <- parse
  --pure $ SProd name $ [\_ -> SRecord fields ]
  let fields' = A.sortBy (\x y -> compare x.recLabel y.recLabel) fields
  let prod = SProd name $ [\_ -> SRecord fields' ]
  pure prod 
  where
    sig = toSignature p
    parse = fromHeader header $ parseMap sig
    name = constructorName sig
    
constructorName (SigProd _ arr)  = (Unsafe.head arr).sigConstructor
withSep :: forall a. String -> Array (Parser a) -> Parser (Array a)
withSep sep xs = sequence (start <> [last'])
  where
    start = map (\x -> x <* (S.string sep)) (Unsafe.init xs)
    last' = (Unsafe.last xs)
psep = withSep "," ([SString <$> (S.string "foo"), SString <$> S.string "bar"] :: forall a. Array (Parser a))

type ParseMap =  Array (Tuple String (Parser GenericSpine)) 
parseMap  :: GenericSignature -> Array (Tuple String (Parser GenericSpine))
parseMap (SigProd _ arr) = zip names parsers
  where
    name = (Unsafe.head arr).sigConstructor
    sr = force $ Unsafe.head (Unsafe.head arr).sigValues
    recs = (\(SigRecord recs) -> recs) sr
    names = map _.recLabel recs
    parsers = map (sigToSpine <<< force <<< _.recValue) recs
    
fromHeader :: Array String -> ParseMap -> Parser (Array Field)
fromHeader header pm = withSep "," fieldParsers
  where
    parsers = map snd $ A.sortBy f pm
    fieldParsers = A.zipWith fieldParse header parsers
    f a b = fromMaybe EQ $ compare <$> (A.elemIndex (fst a) header)  <*> (A.elemIndex (fst b) header)
    fieldParse col p = p >>= \x -> pure {recLabel : col, recValue : \_ -> x}
    
run p s = toMaybe $ P.parse p (stream s)
toMaybe x = case x of
  (Success ({value : value})) -> Just value
  _                           -> Nothing
type Field = { recLabel :: String , recValue :: (Unit -> GenericSpine) }
--(Array {sigConstructor :: String, sigValues :: (Array (Unit -> GenericSignature))})
fromArray :: forall a. Generic a => Proxy a -> Array Field -> Maybe a
fromArray p fields = case (toSignature p) of
                      (SigProd _ cs) -> oneOf (map step cs)
                      _ -> Nothing
 where
  step constructor = do
    let fullConstructorName = constructor.sigConstructor
    fromSpine $ SProd fullConstructorName [\_ -> (SRecord fields)]
    
force :: forall a. (Unit -> a) -> a
force = (_ $ unit)

toInt = do
  d <-  C.many1 C.digit
  let i = Int.fromString d
  maybe P.fail pure i
  
sigToSpine :: GenericSignature -> Parser GenericSpine
sigToSpine SigChar     =  SChar <$> P.item
sigToSpine SigString   =  SString <$> (C.many P.item)
sigToSpine SigInt      =  SInt <$> toInt
sigToSpine SigBoolean  =  SBoolean <$> (fromShow true <|> fromShow false) 
sigToSpine (SigProd "Data.Maybe.Maybe" arr) = nothingCase <|> justCase
  where
    nothingCase = do
      x <- C.many P.item
      when (x /= "") P.fail
      pure $ SProd "Data.Maybe.Nothing" []
    justCase = do
      x <- sigToSpine $ force $ Unsafe.head (Unsafe.head arr).sigValues
      pure $ SProd "Data.Maybe.Just" [\_ -> x]
sigToSpine (SigProd _ arr) = P.fail -- read case
sigToSpine (SigArray  _) = P.fail
sigToSpine (SigRecord _) = P.fail


--SArray (Array (Unit -> GenericSpine))
--SChar Char
--SString String
--SInt Int
--SBoolean Boolean
--SNumber Number
--SRecord (Array { recLabel :: String, recValue :: Unit -> GenericSpine })
--SProd String (Array (Unit -> GenericSpine))

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
