{-# LANGUAGE ScopedTypeVariables #-}
module TspFile where

import Text.Parsec
import Data.List (intercalate)

data Tag = Tag String String deriving (Show)
data Coord = Coord Int Float Float deriving (Show)
data TspFile = TspFile [Tag] [Coord]

instance Show TspFile where
    show (TspFile tags coords) = 
        intercalate "\n" ((\(Tag key val) -> key <> " : " <> val) <$> tags)
        <> "\nNumber of coords -> " <> show (length coords)

skipWhitespace :: Parsec String st ()
skipWhitespace = try $ skipMany (char ' ')

tagP :: Parsec String st Tag
tagP = do
    skipWhitespace
    name <- many $ noneOf [' ',':','\n']
    skipWhitespace
    char ':'
    skipWhitespace
    value <- manyTill anyChar (try (char '\n'))
    skipWhitespace
    pure $ Tag name value

coordsP :: Parsec String st [Coord]
coordsP = do
    skipWhitespace
    string "NODE_COORD_SECTION"
    skipWhitespace
    endOfLine
    manyTill 
        (do
            skipWhitespace
            index <- many $ noneOf [' ']
            skipWhitespace
            first <- many $ noneOf [' ']
            skipWhitespace
            second <- many $ noneOf [' ', '\n']
            skipWhitespace
            endOfLine
            pure $ Coord (read index) (read first) (read second)
        )
        (try (string "EOF"))

fileP :: Parsec String st TspFile
fileP = do
    tags <- many (try tagP)
    TspFile tags <$> coordsP

readTspFile :: String -> IO (Either ParseError TspFile)
readTspFile filename = do
    putStrLn $ "Reading " <> filename
    contents <- readFile filename
    pure $ parse fileP filename contents

