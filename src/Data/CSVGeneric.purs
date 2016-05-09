module Data.CSVGeneric where 
import Data.Generic
import Prelude
import Data.Array as A
import Data.Array.Unsafe as Unsafe
import Data.Eulalie.Char as C
import Data.Eulalie.Parser as P
import Data.Eulalie.String as S
import Data.Int as Int
import Data.Functor
import Data.Either
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
  step constructor = (SProd fullConstructorName []) <$ S.string constructorName 
   where
    fullConstructorName = constructor.sigConstructor
    constructorName = Unsafe.last $ split "." $ fullConstructorName

oneOf :: forall f g a. (Foldable f, Plus g) => f (g a) -> g a
oneOf = foldr alt empty 

--parseCSV pa s = ?what
eitherFromSpine x = maybe (Left $ genericShowPrec 0 x) Right $ fromSpine x
fullParse :: forall a. (Generic a) => Array String -> Proxy a -> Parser (Either String a)
fullParse h x = eitherFromSpine <$> (fullParse' h x)
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
sigToSpine SigString   =  SString <$> (C.many $ P.notChar ',')
sigToSpine SigInt      =  SInt <$> toInt
sigToSpine SigBoolean  =  SBoolean <$> (fromShow true <|> fromShow false) 
sigToSpine (SigProd "Data.Maybe.Maybe" arr) = justCase <|> nothingCase -- this ordering matters!
  where
    nothingSpine = SProd "Data.Maybe.Nothing" []
    nothingCase = (P.eof $> nothingSpine) <|> ((S.string "") $> nothingSpine) 
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
