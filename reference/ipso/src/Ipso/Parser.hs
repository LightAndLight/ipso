{-# language OverloadedLists, OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
module Ipso.Parser where

import qualified Bound
import Bound.Var (Var(..))
import Control.Applicative ((<|>), many, optional, some)
import qualified Data.Maybe as Maybe
import "text" Data.Text (Text)
import qualified "text-utf8" Data.Text as Utf8
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Text.Sage (Parser)
import Text.Parser.Char (CharParsing, char, digit, lower, noneOf, text, upper)
import Text.Parser.Combinators (between, sepBy)
import Text.Parser.Token (IdentifierStyle(..), TokenParsing, comma)
import Text.Parser.Token (braces, brackets, parens)
import qualified Text.Parser.Token as Token
import qualified Text.Parser.Token.Highlight as Highlight

import Ipso.Syntax
  ( CaseBranch(..)
  , Expr(..)
  , Pattern(..)
  , RecordPattern(..)
  , StringPart(..)
  , Type(..)
  , patternNames
  )

identStyle :: CharParsing m => IdentifierStyle m
identStyle =
  IdentifierStyle
  { _styleName = "identifier"
  , _styleStart = lower
  , _styleLetter = lower <|> upper <|> digit <|> char '_'
  , _styleReserved = [ "true", "false", "if", "then", "else", "case", "of" ]
  , _styleHighlight = Highlight.Identifier
  , _styleReservedHighlight = Highlight.ReservedIdentifier
  }

ident :: (TokenParsing m, Monad m) => m Text
ident = Token.ident identStyle

ctorStyle :: CharParsing m => IdentifierStyle m
ctorStyle =
  IdentifierStyle
  { _styleName = "constructor"
  , _styleStart = upper
  , _styleLetter = lower <|> upper <|> digit <|> char '_'
  , _styleReserved = []
  , _styleHighlight = Highlight.Constructor
  , _styleReservedHighlight = Highlight.ReservedConstructor
  }

ctor :: (TokenParsing m, Monad m) => m Text
ctor = Token.ident ctorStyle

reserved :: (TokenParsing m, Monad m) => Text -> m ()
reserved = Token.reserveText identStyle

expr :: forall a. (Text -> Maybe a) -> Parser (Expr a)
expr abstract = self
  where
    self :: Parser (Expr a)
    self =
      ifThenElse <|>
      lam <|>
      app <|>
      case_

    app :: Parser (Expr a)
    app = foldl App <$> project <*> many project

    project :: Parser (Expr a)
    project = foldl Project <$> atom <*> many (char '.' *> ident)

    atom :: Parser (Expr a)
    atom =
      bool <|>
      int <|>
      character <|>
      array <|>
      string <|>
      record <|>
      constructor <|>
      variable <|>
      parens self

    ifThenElse :: Parser (Expr a)
    ifThenElse =
      IfThenElse <$ reserved "if" <*>
      self <* reserved "then" <*>
      self <* reserved "else" <*>
      self

    recordPattern :: Parser RecordPattern
    recordPattern =
      braces $
      RecordPattern . Vector.fromList <$>
      (ident `sepBy` comma) <*>
      (True <$ comma <* text "..." <|> pure False)

    pattern_ :: Parser Pattern
    pattern_ =
      PName <$> ident <|>
      PRecord <$> recordPattern <|>
      PCtor <$> ctor <*> optional recordPattern <|>
      PUnnamed <$ char '_'

    lam :: Parser (Expr a)
    lam = do
      char '\\'
      ps <- Vector.fromList <$> some pattern_
      let pnames = ps >>= patternNames
      text "->"
      body <- expr $ \n -> B <$> Vector.elemIndex n pnames <|> F <$> abstract n
      pure $ Lam ps (Bound.toScope body)

    variable :: Parser (Expr a)
    variable =
      (\i -> maybe (Name i) Var $ abstract i) <$> ident

    bool :: Parser (Expr a)
    bool =
      BTrue <$ reserved "true" <|>
      BFalse <$ reserved "false"

    int :: Parser (Expr a)
    int =
      (\f -> Int . f . read) <$>
      (negate <$ char '-' <|> pure id) <*>
      some digit

    character :: Parser (Expr a)
    character =
      Char <$ char '\'' <*>
      (noneOf "'\\" <|> char '\\' *> (char '\'' <|> char '\\')) <* char '\''

    array :: Parser (Expr a)
    array = Array . Vector.fromList <$> brackets (self `sepBy` comma)

    stringPart :: Parser (StringPart Expr a)
    stringPart =
      StringPart . Utf8.pack <$>
        many (noneOf "\"\\$" <|> char '\\' *> (char '"' <|> char '\\' <|> char '$')) <|>
      ExprPart <$ char '$' <*> braces self

    string :: Parser (Expr a)
    string =
      String <$ char '"' <*>
      (Vector.fromList <$> many stringPart) <* char '"'

    record :: Parser (Expr a)
    record =
      braces $
      Record . Vector.fromList <$>
      sepBy ((,) <$> ident <*> optional (char '=' *> self)) comma

    constructor :: Parser (Expr a)
    constructor = Ctor <$> ctor

    caseBranch :: Parser (CaseBranch Expr a)
    caseBranch = do
      p <- pattern_
      let pnames = patternNames p
      text "->"
      body <- expr $ \n -> B <$> Vector.elemIndex n pnames <|> F <$> abstract n
      pure $ CaseBranch p (Bound.toScope body)

    case_ :: Parser (Expr a)
    case_ =
      Case <$ reserved "case" <*> self <* reserved "of" <*>
      (Vector.fromList <$> many caseBranch)

type_ :: forall a. (Text -> Maybe a) -> Parser (Type a)
type_ abstract = self
  where
    self :: Parser (Type a)
    self = arrow

    arrow :: Parser (Type a)
    arrow = foldr (TApp . TApp TArrow) <$> app <*> many app

    app :: Parser (Type a)
    app = foldl TApp <$> atom <*> many atom

    atom :: Parser (Type a)
    atom =
      variable <|>
      record <|>
      variant <|>
      parens self

    variable :: Parser (Type a)
    variable =
      (\i -> maybe (TName i) TVar $ abstract i) <$> ident

    record :: Parser (Type a)
    record =
      braces . fmap (TApp TRecord) $
      (\fields rest ->
         foldr (\(fname, fty) -> TRowCons fname fty) (Maybe.fromMaybe TRowNil rest) fields
      ) <$>
      (((,) <$> ident <* char ':' <*> self) `sepBy` comma) <*>
      optional (char '|' *> variable)

    variant :: Parser (Type a)
    variant =
      between (char '<') (char '>') . fmap (TApp TVariant) $
      (\fields rest ->
         foldr (\(fname, fty) -> TRowCons fname fty) (Maybe.fromMaybe TRowNil rest) fields
      ) <$>
      (((,) <$> ctor <* char ':' <*> self) `sepBy` comma) <*>
      optional (char '|' *> variable)
