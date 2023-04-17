{-# LANGUAGE UndecidableInstances #-}

module Language.Fennel where

import Control.Monad.Combinators
import Data.Char qualified as Char
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import GHC.Show (appPrec, appPrec1, showSpace)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec
import Prelude hiding (exponent)

data FormF a
  = HashFormF a
  | ListFormF Delimiter [a]
  | NumberFormF Text
  | QuoteFormF a
  | StringFormF Text
  | SymbolFormF Text
  | UnquoteFormF a
  deriving stock (Show)

data Delimiter
  = CurlyBracketDelimiter
  | ParenDelimiter
  | SquareBracketDelimiter
  deriving stock (Show)

newtype Form
  = Form (FormF Form)

instance Show Form where
  showsPrec p = \case
    HashForm x -> showParen (p > appPrec) (showString "HashForm " . showsPrec appPrec1 x)
    ListForm delim xs ->
      showParen (p > appPrec) (showString "ListForm " . shows delim . showSpace . showList xs)
    NumberForm x -> showParen (p > appPrec) (showString "NumberForm " . shows x)
    QuoteForm x -> showParen (p > appPrec) (showString "QuoteForm " . showsPrec appPrec1 x)
    StringForm x -> showParen (p > appPrec) (showString "StringForm " . shows x)
    SymbolForm x -> showParen (p > appPrec) (showString "SymbolForm " . shows x)
    UnquoteForm x -> showParen (p > appPrec) (showString "UnquoteForm " . showsPrec appPrec1 x)

pattern HashForm :: Form -> Form
pattern HashForm x = Form (HashFormF x)

pattern ListForm :: Delimiter -> [Form] -> Form
pattern ListForm x y = Form (ListFormF x y)

pattern NumberForm :: Text -> Form
pattern NumberForm x = Form (NumberFormF x)

pattern QuoteForm :: Form -> Form
pattern QuoteForm x = Form (QuoteFormF x)

pattern StringForm :: Text -> Form
pattern StringForm x = Form (StringFormF x)

pattern SymbolForm :: Text -> Form
pattern SymbolForm x = Form (SymbolFormF x)

pattern UnquoteForm :: Form -> Form
pattern UnquoteForm x = Form (UnquoteFormF x)

{-# COMPLETE HashForm, ListForm, NumberForm, QuoteForm, StringForm, SymbolForm, UnquoteForm #-}

type Parser a =
  Megaparsec.Parsec Void Text a

formP :: Parser Form
formP =
  Form <$> formFP formP

formFP :: Parser a -> Parser (FormF a)
formFP parser =
  choice
    [ listFormP '(' ')' ParenDelimiter,
      listFormP '{' '}' CurlyBracketDelimiter,
      listFormP '[' ']' SquareBracketDelimiter,
      quoteFormP,
      unquoteFormP,
      hashFormP,
      symbolFormP,
      numberFormP,
      stringFormP
    ]
  where
    listFormP ldelim rdelim delim = do
      _ <- Megaparsec.char ldelim
      whitespaceP
      forms <- many parser
      _ <- Megaparsec.char rdelim
      whitespaceP
      pure (ListFormF delim forms)

    quoteFormP = do
      _ <- Megaparsec.char '`'
      form <- parser
      pure (QuoteFormF form)

    unquoteFormP = do
      _ <- Megaparsec.char ','
      form <- parser
      pure (UnquoteFormF form)

    hashFormP = do
      _ <- Megaparsec.char '#'
      form <- parser
      pure (HashFormF form)

    symbolFormP = empty

    numberFormP = do
      -- Why try: if we parse a sign (+/-), then no numbers, we don't want to have committed to parsing a number
      number <- Megaparsec.try (choice [hexNumberP, decimalNumberP])
      whitespaceP
      pure (NumberFormF number)
      where
        hexNumberP =
          fail "TODO: parse hex numbers"

        decimalNumberP :: Parser Text
        decimalNumberP = do
          sign <- signP
          integral <- Megaparsec.takeWhileP (Just "digit") Char.isDigit
          fractional <-
            choice
              [ do
                  dot <- Text.singleton <$> Megaparsec.char '.'
                  num <-
                    (if Text.null integral then Megaparsec.takeWhile1P else Megaparsec.takeWhileP)
                      (Just "digit")
                      Char.isDigit
                  pure (dot <> num),
                pure Text.empty
              ]
          exponent <-
            choice
              [ do
                  e <- Text.singleton <$> choice [Megaparsec.char 'e', Megaparsec.char 'E']
                  esign <- signP
                  num <- Megaparsec.takeWhile1P (Just "digit") Char.isDigit
                  pure (e <> esign <> num),
                pure Text.empty
              ]
          pure (sign <> integral <> fractional <> exponent)

        signP :: Parser Text
        signP =
          choice
            [ Text.singleton <$> Megaparsec.char '-',
              Text.singleton <$> Megaparsec.char '+',
              pure ""
            ]

    stringFormP = empty

whitespaceP :: Parser ()
whitespaceP =
  void $
    Megaparsec.takeWhileP
      (Just "whitespace")
      \c -> c == ' ' || c == '\n' || c == '\t' || c == '\r' || c == '\v' || c == '\f'
