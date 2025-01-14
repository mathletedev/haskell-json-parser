module Main where

import Control.Applicative
import Data.Char

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer -- NOTE: no support for floats
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

-- NOTE: no proper error reporting
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input', x) <- p input
    Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
  where
    f [] = Nothing
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing

stringP :: String -> Parser String
stringP = traverse charP

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where
    f "true" = JsonBool True
    f "false" = JsonBool False
    f _ = undefined

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (a, input') = span f input
   in Just (input', a)

guardNull :: Parser [a] -> Parser [a]
guardNull (Parser p) = Parser $ \input -> do
  (input', xs) <- p input
  if null xs
    then Nothing
    else Just (input', xs)

jsonNumber :: Parser JsonValue
jsonNumber = f <$> guardNull (spanP isDigit)
  where
    f xs = JsonNumber (read xs)

-- NOTE: no support for string escaping
stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

wsP :: Parser String
wsP = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray =
  JsonArray
    <$> ( charP '['
            *> wsP
            *> elements
            <* wsP
            <* charP ']'
        )
  where
    elements = sepBy (wsP *> charP ',' <* wsP) jsonValue

jsonObject :: Parser JsonValue
jsonObject =
  JsonObject
    <$> ( charP '{'
            *> wsP
            *> sepBy (wsP *> charP ',' <* wsP) pair
            <* wsP
            <* charP '}'
        )
  where
    pair =
      (\key _ value -> (key, value))
        <$> stringLiteral
        <*> (wsP *> charP ':' <* wsP)
        <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
  input <- readFile fileName
  return (snd <$> runParser parser input)

main :: IO ()
main = undefined
