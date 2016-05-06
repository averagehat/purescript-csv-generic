module Data.CSVGeneric where 
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

  
fromShow a = S.string (show a) *> (P.succeed a)

matchEnum ::  Array DataConstructor -> Parser GenericSpine
matchEnum constructors = oneOf (map step constructors)
 where
  step :: DataConstructor -> Parser GenericSpine
  step constructor = (flip SProd []) <$> S.string constructorName 
   where
    fullConstructorName = constructor.sigConstructor
    constructorName = Unsafe.last $ split "." $ fullConstructorName

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
sigToSpine (SigProd _ arr) = matchEnum arr
sigToSpine (SigArray  _)   = P.fail
sigToSpine (SigRecord _)   = P.fail


--SArray (Array (Unit -> GenericSpine))
--SChar Char
--SString String
--SInt Int
--SBoolean Boolean
--SNumber Number
--SRecord (Array { recLabel :: String, recValue :: Unit -> GenericSpine })
--SProd String (Array (Unit -> GenericSpine))

