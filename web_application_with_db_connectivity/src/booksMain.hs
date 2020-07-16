import BooksDatabase
import BooksNetwork
import BooksParser
import Control.Exception
import Data.Aeson
import qualified Data.HashMap as HM
import Data.Maybe
import qualified Data.Text as T
import Data.Tuple.Select
import Database.HDBC
import System.Directory
import System.Environment
import System.IO

{-|
  The main function provides ten different functionalities:

    [@create@] Creates the database @books.db@.
    [@delete@] Deletes the database @books.db@.
    [@add @@<@@author@@>@] Adds all books by author to the database.
    [@remove @@<@@author@@>@] Removes all books by author from the database.
    [@co-authors@] Adds all books by all the co-authors present in the database.
    [@books@] Displays all the books present in the database.
    [@authors@] Displays all the authors present in the database.
    [@categories@] Displays all the categories present in the database.
    [@pages @@<@@author@@>@] Displays the total number of pages written by the
    author.
    [@filter@] Provides an interactive filter based on several criteria.
-}
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["create"] -> catch createDB errorHandler
    ["delete"] -> catch deleteDB errorHandler
    ["add", author] -> catch (add author) errorHandler
    ["remove", author] ->catch (remove author) errorHandler
    ["co-authors"] -> catch (coAuthors) errorHandler
    ["books"] -> catch (listAllBooks) errorHandler
    ["authors"] -> catch (listAllAuthors) errorHandler
    ["categories"] -> catch (listAllCategories) errorHandler
    ["pages", author] -> catch (totalPages author) errorHandler
    ["filter"] -> catch (filterCriteria) errorHandler
    _ -> help

{-|
  This function takes a @String@ with the name of the author and adds all the
  books by the searched author to the database. When it finishes inserting all
  the books into the database, it displays how many books were added.
-}
add :: String -> IO ()
add author = do
  fileExists <- doesFileExist "books.db"
  if fileExists then do
    booksAdded <- addAllBooks author
    putStrLn $ "Added " ++ (show booksAdded) ++ (if (booksAdded == 1) then
               " book" else " books") ++ " to the database."
  else
    throw DBDoesNotExistException

{-|
  This function takes a @String@ with the name of the author and removed all the
  books by the specified author.
-}
remove :: String -> IO ()
remove author = do
  fileExists <- doesFileExist "books.db"
  if fileExists then
    removeAuthor author
  else
    throw DBDoesNotExistException

{-|
  This function retrieves all the authors which already exist in the database
  and adds all the books by the co-authors to the database. When it finishes
  inserting all the books into the database, it displays how many books were
  added.
-}
coAuthors :: IO ()
coAuthors = do
  fileExists <- doesFileExist "books.db"
  if fileExists then do
    allAuthors <- getAllNames "author"
    if (null allAuthors) then
      putStrLn "There are no authors in the database."
    else do
      booksAdded <- addCoAuthors allAuthors
      putStrLn $ "Added " ++ (show booksAdded) ++ " " ++
                 (if (booksAdded == 1) then "book" else "books") ++
                 " to the database."
  else
    throw DBDoesNotExistException

{-|
  This function displays a list of all the books in the database alphabetically
  sorted.
-}
listAllBooks :: IO ()
listAllBooks = do
  fileExists <- doesFileExist "books.db"
  if fileExists then do
    books <- filterBooks "SELECT isbn, title FROM book ORDER BY title" []
    if (not . isNothing $ books) then
      showBooks [fromSql . head $ x | x <- (fromJust books)] 0
    else
      putStrLn "There are no books in the database."
  else
    throw DBDoesNotExistException

{-|
  This function displays a list of all the authors that exist in the database
  alphabetically sorted. It uses the recursive function 'showAuthors', which
  takes a list of names of authors and displays them.
-}
listAllAuthors :: IO ()
listAllAuthors = do
  fileExists <- doesFileExist "books.db"
  if fileExists then do
    allAuthors <- getAllNames "author"
    showElements allAuthors "authors"
  else
    throw DBDoesNotExistException

{-|
  This function takes a @String@ with the name of a category and displays a list
  of all the books in that category alphabetically sorted by title.
-}
listAllCategories :: IO ()
listAllCategories = do
  fileExists <- doesFileExist "books.db"
  if fileExists then do
    allCategories <- getAllNames "category"
    showElements allCategories "categories"
  else
    throw DBDoesNotExistException

{-|
  This recursive function takes a list of @String@s with names of authors or
  categories, and a @String@ with the kind of elements, and displays all the
  elements. If the list is empty, it displays a messages reporting that there
  are no elements in the database.
-}
showElements :: [String] -> String -> IO ()
showElements [] kind = putStrLn $ "There are no " ++ kind ++ " in the database."
showElements [x] _ = putStrLn x
showElements (x:xs) kind = do
  showElements [x] kind
  showElements xs kind

{-|
  This function takes a @String@ with the name of the author and displays how
  many pages the author wrote with all their books. It uses the function
  'getSum', which takes a list of @SqlValue@ elements with the number of pages a
  book has and a number with the sum of all the pages, and returns that value.
-}
totalPages :: String -> IO ()
totalPages author = do
  fileExists <- doesFileExist "books.db"
  if fileExists then do
    bookPageCount <- filterBooks ("SELECT b.pageCount FROM author a, " ++
        "book b, bookAuthor ba WHERE a.name = ? AND a.name = ba.author " ++
        "AND ba.book = b.isbn") [toSql author]
    if (not . isNothing $ bookPageCount) then do
      sumPages <- getSum [head x | x <- (fromJust bookPageCount)] 0
      putStrLn $ author ++ " wrote a total of " ++ (show sumPages) ++
                 (if (sumPages == 1) then " page." else " pages.")
    else
      putStrLn $ "There were found no books by " ++ author ++ "."
  else
    throw DBDoesNotExistException
  where getSum :: [SqlValue] -> Int -> IO Int
        getSum [] sumPages = return sumPages
        getSum (x:xs) sumPages = do
          let bookPageCount = fromSql x
          if (isNothing bookPageCount) then
            getSum xs sumPages
          else
            getSum xs (sumPages + (fromJust bookPageCount))

{-|
  This function allows results to be filtered by several criteria from any book,
  author and category properties. It interactively prompts the user for input.
  After gathering all the user input, it displays only the items that match all
  the specified criteria. It displays all the books if the user does not specify
  any criteria. To skip any criterion, the user must press Enter.
-}
filterCriteria :: IO ()
filterCriteria = do
  putStrLn "(Input is case sensitive. Press Enter to skip any criterion.)"
  author <- authorQuery
  putStr "Category: "
  hFlush stdout
  category <- getLine
  yearPublished <- promptDate "Books published"
  putStr "Publisher: "
  hFlush stdout
  publisher <- getLine
  pageCount <- pageCountQuery
  putStr "\n"
  let listQueries = author ++ (categoryQuery category) ++
                    (publishedDateQuery yearPublished) ++
                    (publisherQuery publisher) ++ pageCount
  if (null listQueries) then do
    putStrLn "Results found:\n"
    listAllBooks
  else do
    let intersection = intersectionQuery listQueries "" []
    if (isNothing intersection) then do
      putStrLn "Results found:\n"
      listAllBooks
    else do
      let query = fromJust intersection
      results <- filterBooks (fst query) (snd query)
      if (isNothing results) then
        putStrLn "There were found no books that match your criteria."
      else do
        putStrLn "Results found:\n"
        showBooks [fromSql . head $ x | x <- (fromJust results)] 0

{-|
  This function gathers user input and returns a list of tuples with SQL queries
  and the corresponding SQL values. If the user does not provide an author name,
  it will display more filters such as author's gender, nationality and date of
  birth. If the user does not specify any criteria, it returns an empty list.
-}
authorQuery :: IO [(String, SqlValue)]
authorQuery = do
  putStr "Author: "
  hFlush stdout
  author <- getLine
  if (not . null $ author) then
    return [(("SELECT b.isbn FROM author a, book b, bookAuthor ba " ++
            "WHERE a.name = ? AND a.name = ba.author AND ba.book = b.isbn"),
            toSql author)]
  else do
    putStr "Male/female authors (m/f): "
    hFlush stdout
    inputGender <- getLine
    let gender = case (T.unpack . T.toLower . T.pack $ inputGender) of
      "m" -> "Male"
      "f" -> "Female"
      _ -> ""
    putStr "Authors from (country): "
    hFlush stdout
    country <- getLine
    year <- promptDate "Authors born"
    return $ (genderQuery gender) ++ (nationalityQuery country) ++
             (dateOfBirthQuery year)

{-|
  This function takes a @String@ with the author's gender, and returns a list of
  a tuple with an SQL query and the corresponding SQL value. If the input is an
  empty @String@, it returns an empty list.
-}
genderQuery :: String -> [(String, SqlValue)]
genderQuery "" = []
genderQuery x = [(("SELECT b.isbn FROM author a, book b, bookAuthor ba " ++
    "WHERE a.gender = ? AND a.name = ba.author AND ba.book = b.isbn"), toSql x)]

{-|
  This function takes a @String@ with the author's nationality, and returns a
  list of a tuple with an SQL query and the corresponding SQL value. If the
  input is an empty String, it returns an empty list.
-}
nationalityQuery :: String -> [(String, SqlValue)]
nationalityQuery "" = []
nationalityQuery x = [(("SELECT b.isbn FROM author a, book b, bookAuthor ba " ++
    "WHERE a.nationality = ? AND a.name = ba.author AND ba.book = b.isbn"),
    toSql x)]

{-|
  This function takes a @String@ with the author's date of birth, and returns a
  list of a tuple with an SQL query and the corresponding SQL value. If the user
  does not specify any criteria, it returns an empty list.
-}
dateOfBirthQuery :: Maybe (String, String) -> [(String, SqlValue)]
dateOfBirthQuery Nothing = []
dateOfBirthQuery (Just (x, y)) = if (x == "in") then
  [(("SELECT b.isbn FROM author a, book b, bookAuthor ba WHERE " ++
   "a.dateOfBirth = ? AND a.name = ba.author AND ba.book = b.isbn"), toSql y)]
else if (x == "before") then
  [(("SELECT b.isbn FROM author a, book b, bookAuthor ba WHERE " ++
   "a.dateOfBirth < ? AND a.name = ba.author AND ba.book = b.isbn"), toSql y)]
else if (x == "after") then
  [(("SELECT b.isbn FROM author a, book b, bookAuthor ba WHERE " ++
   "a.dateOfBirth > ? AND a.name = ba.author AND ba.book = b.isbn"), toSql y)]
else []

{-|
  This function takes a @String@ with a category, and returns a list of a tuple
  with an SQL query and the corresponding SQL value.
-}
categoryQuery :: String -> [(String, SqlValue)]
categoryQuery "" = []
categoryQuery x = [(("SELECT b.isbn FROM book b, bookCategory bc, category " ++
    "c WHERE c.name = ? AND c.name = bc.category AND bc.book = b.isbn"),
    toSql x)]

{-|
  This function takes a tuple with the book's year of publication, and returns a
  list of a tuple with an SQL query and the corresponding SQL value. If the user
  does not specify any criteria, it returns an empty list.
-}
publishedDateQuery :: Maybe (String, String) -> [(String, SqlValue)]
publishedDateQuery Nothing = []
publishedDateQuery (Just (x, y)) = if (x == "in") then
  [("SELECT isbn FROM book WHERE publishedDate = ?", toSql y)]
else if (x == "before") then
  [("SELECT isbn FROM book WHERE publishedDate < ?", toSql y)]
else if (x == "after") then
  [("SELECT isbn FROM book WHERE publishedDate > ?", toSql y)]
else []

{-|
  This function takes a @String@ with the book's publisher, and returns a list
  of a tuple with an SQL query and the corresponding SQL value.
-}
publisherQuery :: String -> [(String, SqlValue)]
publisherQuery "" = []
publisherQuery x = [("SELECT isbn FROM book WHERE publisher = ?", toSql x)]

{-|
  This function gathers user input and returns a list of tuples with SQL queries
  and the corresponding SQL values. First, it asks the user if they want to
  filter books with exactly, less than or more than a certain number of pages,
  and then it requests the number of pages. If the user does not specify any
  criteria, it returns an empty list.
-}
pageCountQuery :: IO [(String, SqlValue)]
pageCountQuery = do
  putStr $ "Books with...\n\t1. exactly (number of pages)\n\t2. less " ++
           "than (number of pages)\n\t3. more than (number of pages)\n" ++
           "Option number: "
  hFlush stdout
  option <- getLine
  if (option == "1") then do
    pages <- promptNumberPages "exactly"
    if (isNothing pages) then
      return []
    else
      return [("SELECT isbn FROM book WHERE pageCount = ?",
              toSql . fromJust $ pages)]
  else if (option == "2") then do
    pages <- promptNumberPages "less than"
    if (isNothing pages) then
      return []
    else
      return [("SELECT isbn FROM book WHERE pageCount < ?",
              toSql . fromJust $ pages)]
  else if (option == "3") then do
    pages <- promptNumberPages "more than"
    if (isNothing pages) then
      return []
    else
      return [("SELECT isbn FROM book WHERE pageCount > ?",
              toSql . fromJust $ pages)]
  else
    return []
  where promptNumberPages :: String -> IO (Maybe String)
        promptNumberPages option = do
            putStr $ "Books with " ++ option ++ " (number of pages): "
            hFlush stdout
            pages <- getLine
            if (null pages) then
                return Nothing
            else
                return . Just $ pages

{-|
  This function takes a @String@ with a message to be displayed, prompts the
  user for the date interval (in, before or after a certain year), and returns a
  tuple with the option chosen and a year. If the option chosen is invalid, it
  returns @Nothing@.
-}
promptDate :: String -> IO (Maybe (String, String))
promptDate message = do
      putStr $ message ++ "...\n\t1. in (year)\n\t2. before (year)\n\t" ++
               "3. after (year)\nOption number: "
      hFlush stdout
      option <- getLine
      year <- case option of "1" -> promptYear message "in"
                             "2" -> promptYear message "before"
                             "3" -> promptYear message "after"
                             _ -> return Nothing
      return year

{-|
  This function takes a @String@ with a message to be displayed and another
  @String@ with the option of the date interval (in, before of after a certain
  year), and returns a tuple with the option chosen and a year. If the year is
  not specified, it returns @Nothing@.
-}
promptYear :: String -> String -> IO (Maybe (String, String))
promptYear message option = do
  putStr $ message ++ " " ++ option ++ " (year): "
  hFlush stdout
  year <- getLine
  if (null year) then
    return Nothing
  else
    return . Just $ (option, year)

{-|
  This function takes a list of tuples with SQL queries and SQL values, a
  @String@ to collect all the queries, and a list of @SqlValue@ elements to
  collect all the SQL values, and returns a tuple with the intersection of all
  the SQL queries and a list of all the SQL values. If the list of tuples with
  SQL queries and SQL values is empty, it returns @Nothing@.
-}
intersectionQuery :: [(String, SqlValue)] -> String -> [SqlValue] ->
    Maybe (String, [SqlValue])
intersectionQuery [] _ _ = Nothing
intersectionQuery [x] query values = Just ((query ++ (fst x)),
                                           (values ++ [snd x]))
intersectionQuery (x:xs) query values = intersectionQuery xs (query ++
    (fst x) ++ "\nINTERSECT\n") (values ++ [snd x])

{-|
  This recursive function takes a list of ISBN-13 codes, and displays the title
  of the book, it's authors, date of publication, publisher, number of pages and
  categories if they are not null, and the total number of books. It uses four
  functions, 'showPublishedDate', 'showPublisher', 'showPageCount' and
  'showCategories', which display the date of publication, publisher, number of
  pages and categories of the book, respectively, if they are not null.
-}
showBooks :: [String] -> Int -> IO ()
showBooks [] totalBooks = return ()
showBooks (x:xs) totalBooks = do
  content <- getBookInfo x
  if (not . isNothing $ content) then do
    let bookInfo = fromJust content
    let bookTitle = sel1 bookInfo
    putStrLn $ "Title: '" ++ bookTitle ++ "'"
    let bookAuthors = sel2 bookInfo
    putStrLn $ "Authors: " ++ (showElementsList bookAuthors)
    showPublishedDate . sel3 $ bookInfo
    showPublisher . sel4 $ bookInfo
    showPageCount . sel5 $ bookInfo
    showCategories . sel6 $ bookInfo
    if (not . null $ xs) then
      putStrLn "-------"
    else
      putStrLn $ "\nTotal: " ++ (show $ totalBooks + 1) ++
                 (if ((totalBooks + 1) == 1) then " book" else " books")
    showBooks xs (totalBooks + 1)
  else
    showBooks xs totalBooks
  where showPublishedDate :: Maybe String -> IO ()
        showPublishedDate bookPublishedDate = do
          if (not . isNothing $ bookPublishedDate) then
            putStrLn $ "Date of publication: " ++
                       (fromJust bookPublishedDate)
          else return ()
        showPublisher :: Maybe String -> IO ()
        showPublisher bookPublisher = do
          if (not . isNothing $ bookPublisher) then
            putStrLn $ "Publisher: " ++ (fromJust bookPublisher)
          else return ()
        showPageCount :: Maybe Int -> IO ()
        showPageCount bookPageCount = do
          if (not . isNothing $ bookPageCount) then
            putStrLn $ "Pages: " ++ (show . fromJust $ bookPageCount)
          else return ()
        showCategories :: Maybe [String] -> IO ()
        showCategories bookCategories = do
          if (not ((isNothing $ bookCategories) || (null . fromJust $
                   bookCategories))) then
              putStrLn $ "Categories: " ++ (showElementsList . fromJust $
                  bookCategories)
          else return ()

{-|
  This recursive functions takes a list of names of authors, inserts all the
  books by each of the authors into the database, and returns the number of
  books added.
-}
addCoAuthors :: [String] -> IO Int
addCoAuthors [] = return 0
addCoAuthors (x:xs) = do
  putStrLn $ "Adding books by " ++ x ++ "..."
  sumHead <- addAllBooks x
  sumTail <- addCoAuthors xs
  return $ sumHead + sumTail

{-|
  This function takes a @String@ with the name of the author, retrieves the
  content from the webpage and initiates the entry of all the books by the
  searched author into the database.
-}
addAllBooks :: String -> IO Int
addAllBooks author = do
  content <- downloadURL $ formatBooksURL author 0
  let searchResult = decode content :: Maybe SearchResult
  if (isNothing searchResult ||
      (isNothing . totalItems . fromJust $ searchResult) ||
      (fromJust . totalItems . fromJust $ searchResult) == 0) then do
      putStrLn "There were found 0 books."
      return 0
  else do
    -- An estimated number of items found.
    let itemsFound = fromJust . totalItems . fromJust $ searchResult
    putStrLn "Downloading data..."
    books <- downloadAllBooks author 0 itemsFound
    validVolumes <- getValidVolumes books HM.empty
    putStrLn "Adding books to the database..."
    booksAdded <- insertBooks . HM.elems $ validVolumes
    return booksAdded

{-|
  This recursive function takes a @String@ with the name of the author, the
  index of the results page, an estimated number of items found, and returns an
  array of 'Item' elements. There can only be retrieved a maximum of 40 items
  per request, as imposed by the Google Books API.
-}
downloadAllBooks :: String -> Int -> Int -> IO [Item]
downloadAllBooks author index estimatedFound = do
  {- The content is downloaded again because the number of items found
  (totalItems) is constantly changing at every new HTTP request. -}
  content <- downloadURL $ formatBooksURL author index
  let searchResult = decode content :: Maybe SearchResult
  if (isNothing searchResult ||
      (isNothing $ totalItems . fromJust $ searchResult) ||
      index >= estimatedFound) then do
    {- If the content downloaded is invalid, the next results page is
    checked for possible valid content based on the initially estimated
    number of items found (estimatedFound). -}
    if (index < estimatedFound) then
      downloadAllBooks author (index + 40) estimatedFound
    else
      return []
  else do
    let itemsFound = fromJust . totalItems . fromJust $ searchResult
    if (index < itemsFound) then do
      putStrLn $ "Downloaded " ++ (if (index + 40 > itemsFound) then
                 (show itemsFound) else (show $ index + 40)) ++ " of " ++
                 (show itemsFound) ++ " items."
      if (isNothing . items . fromJust $ searchResult) then
        downloadAllBooks author (index + 40) estimatedFound
      else do
        books <- downloadAllBooks author (index + 40) estimatedFound
        return $ (fromJust . items . fromJust $ searchResult) ++ books
    else
      return []

{-|
  This recursive function takes a list of 'VolumeInfo' elements, inserts all the
  valid books into the table @book@ of the database, inserts all the authors who
  wrote the valid books into the table @author@ of the database, and returns the
  number of items added. It uses the function 'insertValidCategories', which
  takes a 'VolumeInfo' and the ISBN-13 of the book, and if the book is valid and
  has any categories, it inserts all the categories into the table @category@ of
  the database.
-}
insertBooks :: [VolumeInfo] -> IO Int
insertBooks [] = return 0
insertBooks (x:xs) = do
  sumHead <- do
    let bookISBN = fromJust . getISBN13 . fromJust . industryIdentifiers $ x
    bookExists <- bookAlreadyExists bookISBN
    if (bookExists) then
      return 0
    else do
      let bookTitle = fromJust . title $ x
      let bookAuthors = splitValues . fromJust . authors $ x
      insertBookRow [toSql bookISBN,
                     toSql bookTitle,
                     if (isNothing . publisher $ x) then SqlNull
                     else (toSql . fromJust . publisher $ x),
                     if (isNothing . publishedDate $ x) then SqlNull
                     else (toSql . fromJust . publishedDate $ x),
                     if (isNothing . pageCount $ x) then SqlNull
                     else (toSql ((fromJust . pageCount $ x) :: Int))]
      insertAuthors bookAuthors
      insertBookAuthors bookISBN bookAuthors
      insertValidCategories x bookISBN
      putStrLn $ "Added '" ++ bookTitle ++ "'."
      return 1
  sumTail <- insertBooks xs
  return $ sumHead + sumTail
  where insertValidCategories :: VolumeInfo -> String -> IO ()
        insertValidCategories volume isbn = do
            if (not ((isNothing . categories $ volume) ||
                     (null . fromJust . categories $ volume))) then do
              let bookCategories = fromJust . categories $ x
              insertCategories bookCategories
              insertBookCategories isbn bookCategories
            else
              return ()

{-|
  This recursive function takes a list of @String@s with the names of the
  authors of the book and inserts them into the table @author@ of the database.
-}
insertAuthors :: [String] -> IO ()
insertAuthors [] = return ()
insertAuthors (x:xs) = do
  authorExists <- authorAlreadyExists x
  if (authorExists) then
    return ()
  else do
    content <- downloadURL $ formatAuthorsURL x
    let authorInfo = decode content :: Maybe AuthorInfo
    if (isNothing authorInfo || (isNothing . property . fromJust $ authorInfo))
    then
      insertAuthorRow [toSql x, SqlNull, SqlNull, SqlNull]
    else do
      let authorProperty = fromJust . property . fromJust $ authorInfo
      dateOfBirthText <- getProperty authorProperty dateOfBirth
      genderText <- getProperty authorProperty gender
      nationalityText <- getProperty authorProperty nationality
      insertAuthorRow [toSql x,
                       if (isNothing dateOfBirthText) then SqlNull
                       else toSql . fromJust $ dateOfBirthText,
                       if (isNothing genderText) then SqlNull
                       else toSql . fromJust $ genderText,
                       if (isNothing nationalityText) then SqlNull
                       else toSql . fromJust $ nationalityText]
      insertAuthors xs

{-|
  This recursive function takes a list of @String@s with the book categories and
  inserts them into the table @category@ of the database.
-}
insertCategories :: [String] -> IO ()
insertCategories [] = return ()
insertCategories (x:xs) = do
  categoryExists <- categoryAlreadyExists x
  if (categoryExists) then
    return ()
  else do
    insertCategoryRow [toSql x]
    insertCategories xs

{-|
  This recursive function takes the ISBN-13 and a list of authors of a book, and
  inserts a row into the table @bookAuthor@ of the database for each author,
  which associates the book with its authors.
-}
insertBookAuthors :: String -> [String] -> IO ()
insertBookAuthors _ [] = return ()
insertBookAuthors bookISBN (x:xs) = do
  insertBookAuthorRow [toSql bookISBN, toSql x]
  insertBookAuthors bookISBN xs

{-|
  This recursive function takes the ISBN-13 and a list of categories of a book,
  and inserts a row into the table @bookCategory@ of the database for each
  category, which associates the book with its categories.
-}
insertBookCategories :: String -> [String] -> IO ()
insertBookCategories _ [] = return ()
insertBookCategories bookISBN (x:xs) = do
  insertBookCategoryRow [toSql bookISBN, toSql x]
  insertBookCategories bookISBN xs

{-|
  This function takes a 'Property' and a 'Property' function (@dateOfBirth@,
  @gender@ or @nationality@), and returns the corresponding value or @Nothing@
  if the property does not exist.
-}
getProperty :: Property -> (Property -> Maybe PropertyInfo) -> IO (Maybe String)
getProperty authorProperty propertyName =
    if ((isNothing . propertyName $ authorProperty) ||
        (isNothing . values . fromJust . propertyName $ authorProperty) ||
        ((length . fromJust . values . fromJust . propertyName $
          authorProperty) /= 1)) then
        return Nothing
    else
      return $ text . head $ fromJust . values . fromJust . propertyName $
               authorProperty

{-|
  This recursive function takes a list of 'Item' elements and a @HashMap@ and
  returns another @HashMap@ with pairs of only the valid volumes where the
  ISBN-13 is the key and the 'VolumeInfo' is the value of the pair. If more than
  one item has the same ISBN-13, the last item retrieved replaces any previous
  item.
-}
getValidVolumes :: [Item] -> HM.Map String VolumeInfo ->
    IO (HM.Map String VolumeInfo)
getValidVolumes [] booksMap = return booksMap
getValidVolumes (x:xs) booksMap = do
  mapHead <- do
    if ((not . isNothing . volumeInfo $ x) &&
        (validVolumeInfo . fromJust . volumeInfo $ x)) then do
      let volume = fromJust . volumeInfo $ x
      let isbn = fromJust . getISBN13 . fromJust . industryIdentifiers $
                 volume
      return $ HM.insert isbn volume booksMap
    else
      return booksMap
  getValidVolumes xs mapHead

{-|
  This function takes a 'VolumeInfo' and returns @True@ if it is valid, i.e. if
  none of the mandatory fields is @Nothing@ and if lists with mandatory fields
  are not empty.
-}
validVolumeInfo :: VolumeInfo -> Bool
validVolumeInfo volume = not ((isNothing . title $ volume) ||
    (isNothing . authors $ volume) ||
    (isNothing . industryIdentifiers $ volume) ||
    (isNothing . getISBN13 . fromJust . industryIdentifiers $ volume) ||
    (null . fromJust . authors $ volume))

{-|
  This recursive function takes a list of @String@s and returns a @String@ with
  all the elements of the list separated by a comma.
-}
showElementsList :: [String] -> String
showElementsList [] = ""
showElementsList [x] = x
showElementsList (x:xs) = x ++ ", " ++ (showElementsList xs)

{-|
  This recursive function takes a list of @String@s and returns another list of
  @String@s with all the individual authors as individual elements, for cases
  when a @String@ has more than one author separated by a comma.
-}
splitValues :: [String] -> [String]
splitValues [] = []
splitValues (x:xs) = [T.unpack $ T.strip a | a <- T.split (== ',')
                      (T.pack $ x), a /= T.empty] ++ (splitValues xs)

{-|
  This function takes an array of 'IndustryIdentifier' objects and returns the
  ISBN-13 of the book.
-}
getISBN13 :: [IndustryIdentifier] -> Maybe String
getISBN13 [] = Nothing
getISBN13 (x:xs)
  | ((fromJust . identifierType $ x) == "ISBN_13") = identifier $ x
  | otherwise = getISBN13 xs

{-|
  This function handles 'DBException' exceptions. It uses 'errorMessage', which
  takes a message with the error description and displays it.
-}
errorHandler :: DBException -> IO ()
errorHandler e = case e of
  DBAlreadyExistsException -> errorMessage "database already exists"
  DBDoesNotExistException -> errorMessage "database does not exist"
  where errorMessage :: String -> IO ()
        errorMessage message = putStrLn $ "Error: " ++ message ++ "!"

{-|
  This function displays how to use this application and displays all the
  available commands.
-}
help :: IO ()
help = putStrLn $ "Usage: books <command> \"<arguments>\"\n\n" ++
    "create               Create database books.db.\n" ++
    "delete               Delete database books.db.\n" ++
    "add \"[author]\"       Add all books by author to the database.\n" ++
    "remove \"[author]\"    Remove all books by author from the database.\n" ++
    "co-authors           Add all books by co-authors present in the " ++
    "database.\n" ++
    "books                Display all books present in the database.\n" ++
    "authors              Display all authors present in the datase.\n" ++
    "categories           Display all book categories present in the " ++
    "database.\n" ++
    "pages \"[author]\"     Display total number of pages written by " ++
    "author.\n" ++
    "filter               Filter books by several criteria."
