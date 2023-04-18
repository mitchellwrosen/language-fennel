module Language.Fennel where

import Control.Monad.Combinators
import Data.Char qualified as Char
import Data.Functor (void)
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
-- import Prettyprinter
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec
import Prelude hiding (exponent, span)

-----------------------------------------------------------------------------------------------------------------------
-- Types

-- | A piece of syntax: 0+ line comments (the first of which may be on the same line as the preceding syntax), plus a
-- form (which may be empty only there is at least one comment).
data Syntax = Syntax
  { comments :: ![Comment],
    span :: !Span,
    form :: !Form
  }
  deriving stock (Show)

data Form
  = EmptyForm
  | HashForm Syntax
  | ListForm Delimiter [Syntax]
  | NumberForm Text
  | QuoteForm Syntax
  | StringForm Text
  | SymbolForm Text
  | UnquoteForm Syntax
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

-----------------------------------------------------------------------------------------------------------------------
-- Parsing

type Parser a =
  Megaparsec.Parsec Void Text a

-- | A syntax parser.
syntaxP :: Parser Syntax
syntaxP = do
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
      then formP
      else formP <|> pure EmptyForm
  end <- getLoc
  let span = Span start end
  pure Syntax {comments, span, form}

-- | A form parser.
formP :: Parser Form
formP =
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
    listFormP :: Char -> Char -> Delimiter -> Parser Form
    listFormP ldelim rdelim delim = do
      _ <- Megaparsec.char ldelim
      whitespaceP
      forms <- many syntaxP
      whitespaceP
      _ <- Megaparsec.char rdelim
      pure (ListForm delim forms)

    quoteFormP :: Parser Form
    quoteFormP = do
      _ <- Megaparsec.char '`'
      form <- syntaxP
      pure (QuoteForm form)

    unquoteFormP :: Parser Form
    unquoteFormP = do
      _ <- Megaparsec.char ','
      form <- syntaxP
      pure (UnquoteForm form)

    hashFormP :: Parser Form
    hashFormP = do
      _ <- Megaparsec.char '#'
      form <- syntaxP
      pure (HashForm form)

    symbolFormP :: Parser Form
    symbolFormP = empty

    numberFormP :: Parser Form
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

    stringFormP :: Parser Form
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

-----------------------------------------------------------------------------------------------------------------------
-- Pretty-printing

type BlockCommentOut =
  List.NonEmpty LineCommentOut

type LineCommentOut =
  Text

data SyntaxOut = SyntaxOut
  { blockCommentsWayAbove :: ![BlockCommentOut],
    blockCommentJustAbove :: !(Maybe BlockCommentOut),
    form :: !FormOut,
    lineCommentJustAfter :: !(Maybe LineCommentOut)
  }
  deriving stock (Show)

data FormOut
  = HashFormOut FormOut
  | ListFormOut Delimiter [SyntaxOut]
  | NumberFormOut Text
  | QuoteFormOut FormOut
  | StringFormOut Text
  | SymbolFormOut Text
  | UnquoteFormOut FormOut
  deriving stock (Show)
