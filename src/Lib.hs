module Lib
    ( getTitle
    , getAuthor
    , getISBN
    , getPublisher
    , getYearPublished
    , getReadingDate
    ) where

import Data.Time
import Control.Applicative

prompt :: String -> IO String
prompt part = putStr part >> getLine >>=  (\line -> putStr "[y]/[n]\t" >> return line)

getTitle :: IO String
getTitle = do
        line <- prompt "Title: "
        confirmation <- getLine
        if confirmation == "y"
            then return line
            else getTitle

getAuthor :: IO (Maybe String)
getAuthor = do
        line <- prompt "Author: "
        confirmation <- getLine
        if confirmation == "y"
            then if line == "" then return Nothing else return $ Just line
            else getAuthor

getISBN :: IO (Maybe String)
getISBN = do
        line <- prompt "ISBN: "
        confirmation <- getLine
        if confirmation == "y"
            then if line == "" then return Nothing else return $ Just line
            else getAuthor

getPublisher :: IO (Maybe String)
getPublisher = do
        line <- prompt "Publisher: "
        confirmation <- getLine
        if confirmation == "y"
            then if line == "" then return Nothing else return $ Just line
            else getPublisher

getYearPublished :: IO (Maybe Day)
getYearPublished = do
    line <- prompt "Year Published(YYYY): "
    confirmation <- getLine
    if confirmation == "y"
        then return $ getDate line
        else getYearPublished

getReadingDate :: IO (Maybe Day)
getReadingDate = do
    line <- prompt "Date Read (YYYY/MM/DD): "
    confirmation <- getLine
    if confirmation == "y"
        then return $ getDate line
        else getReadingDate

getDate :: String -> Maybe Day
getDate s =
        p1 <|> p2 <|> p3
        where
            p1 = parseDateString "%Y/%-m/%-d" s
            p2 = parseDateString "%Y/%-m" s
            p3 = parseDateString "%Y" s
            parseDateString = parseTimeM True defaultTimeLocale
