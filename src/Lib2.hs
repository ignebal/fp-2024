{-# LANGUAGE StrictData #-}

module Lib2 (
    Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition,
    Song(..),
    parseSong,
    parseSongList
) where

import Data.Char (isDigit, isSpace)

-- Song data type
data Song = Song
    { songTitle :: String,
        songSinger :: String,
        songGenre :: String,
        songLength :: (Int, Int)
    }
    deriving (Show, Eq)

-- User command data type
data Query
    = OpenSongList [Song]
    | LikeSong Song
    | UnlikeSong Song
    deriving (Show, Eq)

-- Parsing Helpers
orElse :: Either String a -> Either String a -> Either String a
orElse (Right x) _ = Right x
orElse (Left _) y = y

-- Basic Parsers
digit :: String -> Either String Char
digit str = case str of
    (x:_) | isDigit x -> Right x
    _ -> Left "Expected a digit"

string :: String -> String -> Either String String
string expected actual =
    if take (length expected) actual == expected
        then Right (drop (length expected) actual) -- Consume the matched string!
        else Left ("Expected \"" ++ expected ++ "\"")

parseTwoDigit :: String -> Either String Int
parseTwoDigit str =
    if length str /= 2
        then Left "Expected two digits"
        else case (digit (take 1 str), digit (take 1 (drop 1 str))) of
            (Right d1, Right d2) -> case reads [d1, d2] :: [(Int, String)] of
                [(num, "")] -> Right num
                _ -> Left "Invalid number"
            (Left err, _) -> Left err
            (_, Left err) -> Left err

parseLength :: String -> Either String (Int, Int)
parseLength str =
    let parseMMSS inputStr = case break (== ':') inputStr of
            (minutesStr, ':':secondsStr) -> case (parseTwoDigit minutesStr, parseTwoDigit secondsStr) of
                (Right minutes, Right seconds) ->
                    if minutes <= 59 && seconds <= 59
                        then Right (minutes, seconds)
                        else Left "Minutes and seconds must be between 0 and 59"
                (Left minErr, _) -> Left minErr -- Return the minutes error
                (_, Left secErr) -> Left secErr -- Return the seconds error
            _ -> Left "Invalid length format (MM:SS)" -- Correct error message for wrong separator

        parseSS inputStr = case parseTwoDigit inputStr of
            Right seconds -> if seconds <= 59 then Right (0, seconds) else Left "Seconds must be between 0 and 59"
            Left err -> Left err
    in orElse (parseMMSS str) (parseSS str)

parseQuotedString :: String -> Either String String
parseQuotedString str = case string "\"" str of
    Left err -> Left err
    Right _ -> case break (== '"') (drop 1 str) of
        (content, '"':_) -> Right content
        _ -> Left "Unterminated quoted string"

skipSpaces :: String -> Either String String
skipSpaces str = Right (drop (length (takeWhile isSpace str)) str)

parseSong :: String -> Either String Song
parseSong str = case skipSpaces str of
    Left err -> Left err
    Right rest -> case parseQuotedString rest of
        Left err -> Left err
        Right title -> case skipSpaces (drop (length ('"' : title ++ "\"")) rest) of
            Left err -> Left err
            Right restAfterTitle -> case parseQuotedString restAfterTitle of
                Left err -> Left err
                Right singer -> case skipSpaces (drop (length ('"' : singer ++ "\"")) restAfterTitle) of
                    Left err -> Left err
                    Right restAfterSinger -> case parseQuotedString restAfterSinger of
                        Left err -> Left err
                        Right genre -> case skipSpaces (drop (length ('"' : genre ++ "\"")) restAfterSinger) of
                            Left err -> Left err
                            Right restAfterGenre -> case parseLength (dropWhile isSpace restAfterGenre) of
                                Left err -> Left err
                                Right len -> Right Song { songTitle = title, songSinger = singer, songGenre = genre, songLength = len }

parseListBy :: (String -> Either String a) -> Char -> String -> Either String [a]
parseListBy _ _ "" = Right []
parseListBy parser delimiter str = case break (== delimiter) str of
    (line, rest) -> case parser line of
        Left err -> Left err
        Right item -> case rest of
            "" -> Right [item]
            (_:remainingRest) -> parseListBy parser delimiter remainingRest

parseSongList :: String -> Either String [Song]
parseSongList str = parseListBy parseSong '\n' str

parseQuery :: String -> Either String Query
parseQuery str = case skipSpaces str of
    Left initialErr -> Left initialErr
    Right rest ->
        case string "OPEN" rest of
            Right _ -> case skipSpaces (drop (length "OPEN") rest) of
                Right restAfterOpen -> case parseSongList restAfterOpen of
                    Right songList -> Right (OpenSongList songList)
                    Left err -> Left err
                Left openErr -> Left ("Invalid OPEN command format: " ++ show openErr)
            Left _ ->  -- If OPEN doesn't match
                case string "LIKE" rest of
                    Right _ -> case skipSpaces (drop (length "LIKE") rest) of
                        Right restAfterLike -> case parseSong (dropWhile isSpace restAfterLike) of
                            Right song -> Right (LikeSong song)
                            Left likeParseErr -> Left ("Invalid LIKE command format: " ++ show likeParseErr)
                        Left likeErr -> Left ("Invalid LIKE command format: " ++ show likeErr)
                    Left _ -> -- If LIKE doesn't match
                        case string "UNLIKE" rest of
                            Right _ -> case skipSpaces (drop (length "UNLIKE") rest) of
                                Right restAfterUnlike -> case parseSong restAfterUnlike of
                                    Right song -> Right (UnlikeSong song)
                                    Left unlikeParseErr -> Left ("Invalid UNLIKE command format: " ++ show unlikeParseErr)
                                Left unlikeErr -> Left ("Invalid UNLIKE command format: " ++ show unlikeErr)
                            Left _ -> Left "Invalid command" -- If none match

-- Program state data type
data State = State
    { songs :: [Song] }
    deriving (Show, Eq)

-- Create an empty initial state
emptyState :: State
emptyState = State {songs = []}

-- Update program state based on user command
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = case query of
    OpenSongList _ -> Right (Just (show (songs st)), st)
    LikeSong song -> Right (Just "Song liked!", State {songs = song : songs st})
    UnlikeSong song ->
        if song `elem` songs st
            then Right (Just "Song unliked!", State {songs = filter (/= song) (songs st)})
            else Left "Song not found"