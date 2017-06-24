module Data.TransmissionParser (transmission) where

import Data.Char
import Data.Transmission
import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as Lexer
import Text.Megaparsec.String
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Scientific

-- Parse a message and generate a transmission from it
-- Transmissions can be either
-- FORMAT:ADDRESS:MESSAGE
-- WAIT
-- WAIT <LENGTH>
transmission :: Parser (TransmissionM w ())
transmission = do
  lineSep
  actions <- manyTill ((action <|> comment) <* lineSep) eof
  pure $ sequence_ actions
  where
    action = sendMessage <|> delay
    comment = skipLineComment $> pure ()

sendMessage :: Parser (TransmissionM w ())
sendMessage =
  lexeme $ do
    mType <- pocsagMessageType <|> flexMessageType
    void colon
    addr <- intLit
    void colon
    msg <- manyTill anyChar . lookAhead $ void newlineChar
    pure $ encode (Message mType addr msg) >>= transmit

delay :: Parser (TransmissionM w ())
delay = do
  void $ symbol "WAIT"
  getDelayTime <- option randDelayTime (pure <$> doubleLit)
  pure $ getDelayTime >>= noise >>= transmit

pocsagMessageType :: Parser MessageType
pocsagMessageType =
  lexeme $
  string "POCSAG" *>
  choice
    [ string "512" *> pure (POCSAG POCSAGBaudRate512)
    , string "1200" *> pure (POCSAG POCSAGBaudRate1200)
    , string "2400" *> pure (POCSAG POCSAGBaudRate2400)
    ]

flexMessageType :: Parser MessageType
flexMessageType = symbol "FLEX" $> FLEX

whitespaceChar :: Parser Char
whitespaceChar = satisfy $ \c -> c /= '\r' && c /= '\n' && isSpace c

newlineChar :: Parser Char
newlineChar = satisfy $ \c -> c == '\r' || c == '\n'

lineSep :: Parser ()
lineSep = lexeme $ skipMany newlineChar

skipLineComment :: Parser ()
skipLineComment = Lexer.skipLineComment "#"

spaceConsumer :: Parser ()
spaceConsumer =
  Lexer.space (void whitespaceChar) empty empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = Lexer.symbol spaceConsumer

colon :: Parser String
colon = symbol ":"

intLit :: Num b => Parser b
intLit = fromIntegral <$> lexeme Lexer.integer

doubleLit :: Parser Double
doubleLit = toRealFloat <$> lexeme Lexer.number
