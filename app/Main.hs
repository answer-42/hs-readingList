{-# LANGUAGE OverloadedStrings #-}

module Main where
-- TODO: Add edit

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Loops
import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import           Data.Maybe
import           Data.Time
import qualified Data.Vector          as V
import           Lib
import           System.Exit
import           Text.Printf
import           Text.Read            (readMaybe)

import           System.IO            (BufferMode (BlockBuffering),
                                       hSetBuffering, stdout)
import           System.IO.CodePage

data Action = Add | Delete | Edit | Print | Help | Save | Quit | Unknown -- TODO: do we use Help qnd Unknown?

parseArg :: String -> Action
parseArg "a" = Add
parseArg "d" = Delete
parseArg "e" = Edit
parseArg "p" = Print
parseArg "s" = Save
parseArg "h" = Help
parseArg "q" = Quit
parseArg _   = Unknown

printHelp :: IO ()
printHelp = do
    putStrLn "[a] ... Add book"
    putStrLn "[d] ... Delete book"
    putStrLn "[e] ... Edit book"
    putStrLn "[p] ... Print all books"
    putStrLn "[s] ... Save changes"
    putStrLn "[h] ... Show this help text"
    putStrLn "[q] ... Quit"

data Book = Book
    { title       :: !String
    , author      :: !(Maybe String)
    , isbn        :: !(Maybe String)
    , publisher   :: !(Maybe String)
    , publishDate :: !(Maybe Day)
    , readingDate :: !(Maybe Day)
    } deriving (Show)

instance FromField Day where
    parseField = getDate . show
        where getDate s =
                p1 <|> p2 <|> p3
                    where
                        p1 = parseDateString "\"%Y/%-m/%-d\"" s
                        p2 = parseDateString "\"%Y/%-m\"" s
                        p3 = parseDateString "\"%Y\"" s
                        parseDateString = parseTimeM True defaultTimeLocale

instance ToField Day where
            toField d = toField $ formatTime defaultTimeLocale "%Y/%-m/%-d" d

instance FromNamedRecord Book where
    parseNamedRecord r = Book <$>
                         r .: "Title" <*>
                         r .: "Author" <*>
                         r .: "ISBN13" <*>
                         r .: "Publisher" <*>
                         r .: "Year Published" <*>
                         r .: "Date Read"

instance ToNamedRecord Book where
    toNamedRecord (Book title author isbn pub pubDate readDate) = namedRecord [
        "Title" .= title,
        "Author" .= author,
        "ISBN13" .= isbn,
        "Publisher" .= pub,
        "Year Published" .= pubDate,
        "Date Read" .= readDate]

instance DefaultOrdered Book where
    headerOrder _ = header ["Title", "Author", "ISBN13", "Publisher", "Year Published", "Date Read"]

prettyPrint :: Book -> String
prettyPrint (Book title author isbn pub pubDate readDate) =
    printf "%-20.20s\t%-20.20s\t%-20.20s\t%-13.13s\t%-4.4s\t%-10.10s\n"
    title (get author) (get pub) (getFromISBN isbn) (getFromYear pubDate) (getFromDate readDate)
    where
        get = fromMaybe ""
        getFromISBN (Just i) = show' i
            where show' = takeWhile (/= '\"') . dropWhile (== '\"') . show
        getFromISBN Nothing = ""
        getFromYear (Just t) = formatTime defaultTimeLocale "%Y" t
        getFromYear Nothing  = ""
        getFromDate (Just t) = formatTime defaultTimeLocale "%Y/%m/%d" t
        getFromDate Nothing  = ""

promptBook :: IO Book
promptBook = do
    title <- getTitle
    author <- getAuthor
    isbn <- getISBN
    pub <- getPublisher
    pubDate <- getYearPublished
    readDate <- getReadingDate
    return $ Book title author isbn pub pubDate readDate

applyArg :: V.Vector Book -> Action -> FilePath -> IO (V.Vector Book)
applyArg db Print _ = V.imapM_ (\i el -> putStr (printf "%5i\t" i) >> (putStr . prettyPrint) el) db >> pure db
applyArg db Add _ = do
    book <- promptBook
    putStr $ prettyPrint book
    return $ V.cons book db
applyArg db Delete _ = do
    printf "Which book do you want to delete? (0-%i) " (pred $ V.length db)
    l <- getLine
    let n = readMaybe l
    if n >= (Just 0) && n < (Just $ V.length db)
      then pure (deleteN (fromMaybe 0 n) db)
      else putStrLn "Invalid input" >> pure db
    where deleteN n db = uncurry (V.++) . second (V.drop 1) $ V.splitAt n db
applyArg db Edit _ = do
    putStr "Which book do you want to edit? "
    l <- getLine
    let n = readMaybe l
    if n >= (Just 0) && n < (Just $ V.length db)
        then pure (editBook (fromMaybe 0 n) db)
        else putStrLn "Invalid input" >> pure db
    where editBook n db = db -- TODO
applyArg db Save path = do
    putStr "Do you want to save to file? [y]/[n] "
    confirmation <- getLine
    if confirmation == "y"
        then saveToFile >> putStrLn "Saved to file." >> return db
        else return db
    where saveToFile = BL.writeFile path (encodeDefaultOrderedByName (V.toList db))
applyArg _ Quit _ = exitSuccess
applyArg db Help _ = printHelp >> pure db
applyArg db Unknown _ = printHelp >> pure db

unicodeRepl :: FilePath -> IO ()
unicodeRepl path =
    let prompt db = do
                hSetBuffering stdout $ BlockBuffering $ Just 1 -- ! Needed on windows
                putStr "What do you want to do? ([h]elp) "
                l <- getLine
                applyArg db (parseArg l) path
        getCSV = do
            csvData <- BL.readFile path
            case decodeByName csvData of
                Left _       -> pure V.empty -- ? Add an error message
                Right (_, v) -> pure v
    in
        getCSV >>= iterateM_ prompt

main :: IO ()
main = withCP65001 (unicodeRepl "C:\\Users\\answe\\Dropbox\\reading_list.csv")
