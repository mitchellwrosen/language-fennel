module Language.Fennel where

import Control.Monad.Combinators
import Data.Char qualified as Char
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec
import Prelude hiding (exponent, span)

-- | A piece of syntax: 0+ line comments (the first of which may be on the same line as the preceding syntax), plus a
-- form (which may be empty only there is at least one comment).
newtype Syntax
  = Syntax (SyntaxF Syntax)
  deriving stock (Show)

data SyntaxF a = SyntaxF
  { comments :: ![Comment],
    span :: !Span,
    form :: !(Form a)
  }
  deriving stock (Show)

data Form a
  = EmptyForm
  | HashForm a
  | ListForm Delimiter [a]
  | NumberForm Text
  | QuoteForm a
  | StringForm Text
  | SymbolForm Text
  | UnquoteForm a
  deriving stock (Show)

data Delimiter
  = CurlyBracket
  | Paren
  | SquareBracket
  deriving stock (Show)

-- | A single line-comment.
data Comment = Comment
  { -- | The line the comment is on. It may appear after some other syntax on the same line.
    line :: !Megaparsec.Pos,
    -- | The comment text, minus the leading semicolon, but including any leading whitespace.
    comment :: !Text
  }
  deriving stock (Show)

-- | A span in a source file.
data Span = Span
  { start :: !Loc,
    end :: !Loc
  }
  deriving stock (Show)

-- | A location in a source file.
data Loc = Loc
  { line :: !Megaparsec.Pos,
    col :: !Megaparsec.Pos
  }
  deriving stock (Show)

type Parser a =
  Megaparsec.Parsec Void Text a

-- | A syntax parser.
syntaxP :: Parser Syntax
syntaxP =
  Syntax <$> syntaxFP syntaxP

syntaxFP :: Parser a -> Parser (SyntaxF a)
syntaxFP parser = do
  whitespaceP
  comments <- many commentP
  start <- getLoc
  -- If there are any preceding comments, then allow parsing an empty form.
  -- This allows us to parse the comments at the end of a list, as in:
  --
  --   (foo
  --     bar
  --     ; Hi I'm preceding nothing
  --   )
  form <-
    if null comments
      then formP parser
      else formP parser <|> pure EmptyForm
  end <- getLoc
  let span = Span start end
  pure SyntaxF {comments, span, form}

-- | A form parser.
formP :: forall a. Parser a -> Parser (Form a)
formP parser =
  choice
    [ listFormP '(' ')' Paren,
      listFormP '{' '}' CurlyBracket,
      listFormP '[' ']' SquareBracket,
      quoteFormP,
      unquoteFormP,
      hashFormP,
      symbolFormP,
      numberFormP,
      stringFormP
    ]
  where
    listFormP :: Char -> Char -> Delimiter -> Parser (Form a)
    listFormP ldelim rdelim delim = do
      _ <- Megaparsec.char ldelim
      whitespaceP
      forms <- many parser
      whitespaceP
      _ <- Megaparsec.char rdelim
      pure (ListForm delim forms)

    quoteFormP :: Parser (Form a)
    quoteFormP = do
      _ <- Megaparsec.char '`'
      form <- parser
      pure (QuoteForm form)

    unquoteFormP :: Parser (Form a)
    unquoteFormP = do
      _ <- Megaparsec.char ','
      form <- parser
      pure (UnquoteForm form)

    hashFormP :: Parser (Form a)
    hashFormP = do
      _ <- Megaparsec.char '#'
      form <- parser
      pure (HashForm form)

    symbolFormP :: Parser (Form a)
    symbolFormP = empty

    numberFormP :: Parser (Form a)
    numberFormP = do
      -- Why try: if we parse a sign (+/-), then no numbers, we don't want to have committed to parsing a number
      number <- Megaparsec.try (choice [hexNumberP, decimalNumberP])
      pure (NumberForm number)
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

    stringFormP :: Parser (Form a)
    stringFormP = empty

-- | A comment parser.
commentP :: Parser Comment
commentP = do
  _ <- Megaparsec.char ';'
  Megaparsec.SourcePos _ line _ <- Megaparsec.getSourcePos
  comment <- Megaparsec.takeWhileP (Just "comment") (/= '\n')
  whitespaceP
  pure Comment {line, comment}

whitespaceP :: Parser ()
whitespaceP =
  void $
    Megaparsec.takeWhileP
      (Just "whitespace")
      \c -> c == ' ' || c == '\n' || c == '\t' || c == '\r' || c == '\v' || c == '\f'

getLoc :: Parser Loc
getLoc = do
  Megaparsec.SourcePos _ line col <- Megaparsec.getSourcePos
  pure Loc {line, col}
