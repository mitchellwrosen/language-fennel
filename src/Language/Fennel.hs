module Language.Fennel where

import Control.Monad.Combinators
import Data.Char qualified as Char
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
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
  = Form (L (FormF Form))
  deriving stock (Show)

type Parser a =
  Megaparsec.Parsec Void Text a

data L a = L
  { startLine :: !Megaparsec.Pos,
    startCol :: !Megaparsec.Pos,
    endLine :: !Megaparsec.Pos,
    endCol :: !Megaparsec.Pos,
    value :: !a
  }
  deriving stock (Show)

formP :: Parser Form
formP =
  Form <$> formFP formP

formFP :: forall a. Parser a -> Parser (L (FormF a))
formFP parser = do
  whitespaceP
  Megaparsec.SourcePos _ startLine startCol <- Megaparsec.getSourcePos
  value <-
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
  Megaparsec.SourcePos _ endLine endCol <- Megaparsec.getSourcePos
  pure L {startLine, startCol, endLine, endCol, value}
  where
    listFormP :: Char -> Char -> Delimiter -> Parser (FormF a)
    listFormP ldelim rdelim delim = do
      _ <- Megaparsec.char ldelim
      whitespaceP
      forms <- many parser
      whitespaceP
      _ <- Megaparsec.char rdelim
      pure (ListFormF delim forms)

    quoteFormP :: Parser (FormF a)
    quoteFormP = do
      _ <- Megaparsec.char '`'
      form <- parser
      pure (QuoteFormF form)

    unquoteFormP :: Parser (FormF a)
    unquoteFormP = do
      _ <- Megaparsec.char ','
      form <- parser
      pure (UnquoteFormF form)

    hashFormP :: Parser (FormF a)
    hashFormP = do
      _ <- Megaparsec.char '#'
      form <- parser
      pure (HashFormF form)

    symbolFormP :: Parser (FormF a)
    symbolFormP = empty

    numberFormP :: Parser (FormF a)
    numberFormP = do
      -- Why try: if we parse a sign (+/-), then no numbers, we don't want to have committed to parsing a number
      number <- Megaparsec.try (choice [hexNumberP, decimalNumberP])
      pure (NumberFormF number)
      where
        hexNumberP =
          fail "TODO: parse hex numbers"

        decimalNumberP :: Parser Text
        decimalNumberP = do
          sign <- signP
          integral <- Megaparsec.takeWhileP (Just "digit") Char.isDigit
          fractional <-
            if Text.null integral
              then do
                dot <- Text.singleton <$> Megaparsec.char '.'
                num <- Megaparsec.takeWhile1P (Just "digit") Char.isDigit
                pure (dot <> num)
              else
                choice
                  [ do
                      dot <- Text.singleton <$> Megaparsec.char '.'
                      num <- Megaparsec.takeWhileP (Just "digit") Char.isDigit
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

    stringFormP :: Parser (FormF a)
    stringFormP = empty

whitespaceP :: Parser ()
whitespaceP =
  void $
    Megaparsec.takeWhileP
      (Just "whitespace")
      \c -> c == ' ' || c == '\n' || c == '\t' || c == '\r' || c == '\v' || c == '\f'
