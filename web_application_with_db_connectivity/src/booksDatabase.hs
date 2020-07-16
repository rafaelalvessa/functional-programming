{-# LANGUAGE DeriveDataTypeable #-}

-- | This module contains functions that operate on the database @books.db@.
module BooksDatabase (authorAlreadyExists, bookAlreadyExists,
    categoryAlreadyExists, createDB, deleteDB, DBException (..), filterBooks,
    getAllNames, getBookInfo, insertAuthorRow, insertBookRow, insertCategoryRow,
    insertBookAuthorRow, insertBookCategoryRow, removeAuthor)
  where

import Control.Exception
import Data.Maybe
import Data.Typeable
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory

{-|
  A 'DBException' can be a 'DBAlreadyExistsException' exception if the database
  already exists, or a 'DBDoesNotExistException' exception if the dabatase does
  not exist.
-}
data DBException = DBAlreadyExistsException | DBDoesNotExistException
  deriving (Show, Typeable)

instance Exception DBException

{-|
  This function creates the database @books.db@ containing five tables: @book@,
  @author@, @bookAuthor@, @category@ and @bookCategory@. It throws a
  'DBAlreadyExistsException' exception if the database already exists.
-}
createDB :: IO ()
createDB = do
  fileExists <- doesFileExist "books.db"
  if fileExists then
    throw DBAlreadyExistsException
  else do
    conn <- connectSqlite3 "books.db"
    run conn ("CREATE TABLE " ++ book) []
    run conn ("CREATE TABLE " ++ author) []
    run conn ("CREATE TABLE " ++ bookAuthor) []
    run conn ("CREATE TABLE " ++ category) []
    run conn ("CREATE TABLE " ++ bookCategory) []
    commit conn
    disconnect conn
    putStrLn "Database successfully created."
  where book = "book (isbn VARCHAR(13) PRIMARY KEY, title TEXT NOT NULL, " ++
               "publisher TEXT, publishedDate DATE, pageCount INTEGER)"
        author = "author (name TEXT PRIMARY KEY, dateOfBirth DATE, gender " ++
                 "VARCHAR(6), nationality TEXT)"
        category = "category (name TEXT PRIMARY KEY)"
        bookAuthor = "bookAuthor (book VARCHAR(13), author TEXT, FOREIGN " ++
                     "KEY (book) REFERENCES book (isbn), FOREIGN KEY " ++
                     "(author) REFERENCES author (name))"
        bookCategory = "bookCategory (book VARCHAR(13), category TEXT, " ++
                       "FOREIGN KEY (book) REFERENCES book (isbn), " ++
                       "FOREIGN KEY (category) REFERENCES category (name))"

{-|
  This function takes a list of @SqlValue@ elements and inserts a book into the
  table @book@ of the database.
-}
insertBookRow :: [SqlValue] -> IO ()
insertBookRow bookData = do
  fileExists <- doesFileExist "books.db"
  if fileExists then do
      conn <- connectSqlite3 "books.db"
      run conn ("INSERT INTO book (isbn, title, publisher, publishedDate, " ++
                "pageCount) VALUES (?, ?, ?, ?, ?)") bookData
      commit conn
      disconnect conn
  else
      throw DBDoesNotExistException

{-|
  This function takes a list of @SqlValue@ elements and inserts an author into
  the table @author@ of the database.
-}
insertAuthorRow :: [SqlValue] -> IO ()
insertAuthorRow authorData = do
  fileExists <- doesFileExist "books.db"
  if fileExists then do
    conn <- connectSqlite3 "books.db"
    run conn ("INSERT INTO author (name, dateOfBirth, gender, " ++
              "nationality) VALUES (?, ?, ?, ?)") authorData
    commit conn
    disconnect conn
  else
    throw DBDoesNotExistException

{-|
  This function takes a list of @SqlValue@ elements and inserts a row into the
  table @bookAuthor@ of the database that associates the book with its authors.
-}
insertBookAuthorRow :: [SqlValue] -> IO ()
insertBookAuthorRow bookAuthorData = do
  fileExists <- doesFileExist "books.db"
  if fileExists then do
    conn <- connectSqlite3 "books.db"
    run conn ("INSERT INTO bookAuthor (book, author) VALUES (?, ?)")
        bookAuthorData
    commit conn
    disconnect conn
  else
    throw DBDoesNotExistException

{-|
  This function takes a list of @SqlValue@ elements and inserts a category into
  the table @category@ of the database.
-}
insertCategoryRow :: [SqlValue] -> IO ()
insertCategoryRow categoryData = do
  fileExists <- doesFileExist "books.db"
  if fileExists then do
    conn <- connectSqlite3 "books.db"
    run conn ("INSERT INTO category (name) VALUES (?)") categoryData
    commit conn
    disconnect conn
  else
    throw DBDoesNotExistException

{-|
  This function takes a list of @SqlValue@ elements and inserts a row into the
  table @bookCategory@ of the database that associates the book with its
  categories.
-}
insertBookCategoryRow :: [SqlValue] -> IO ()
insertBookCategoryRow bookCategoryData = do
  fileExists <- doesFileExist "books.db"
  if fileExists then do
    conn <- connectSqlite3 "books.db"
    run conn ("INSERT INTO bookCategory (book, category) VALUES (?, ?)")
        bookCategoryData
    commit conn
    disconnect conn
  else
    throw DBDoesNotExistException

{-|
  This function takes a @String@ with the ISBN-13 of a book and returns @True@
  if the book already exists in the database.
-}
bookAlreadyExists :: String -> IO Bool
bookAlreadyExists isbn = do
  fileExists <- doesFileExist "books.db"
  if fileExists then do
    conn <- connectSqlite3 "books.db"
    content <- quickQuery' conn "SELECT * FROM book WHERE isbn = ?"
        [toSql isbn]
    disconnect conn
      return . not $ content == []
  else
    throw DBDoesNotExistException

{-|
  This function takes a @String@ with the name of the author and returns @True@
  if the author already exists in the database.
-}
authorAlreadyExists :: String -> IO Bool
authorAlreadyExists author = do
  fileExists <- doesFileExist "books.db"
  if fileExists then do
    conn <- connectSqlite3 "books.db"
    content <- quickQuery' conn "SELECT * FROM author WHERE name = ?"
        [toSql author]
    disconnect conn
    return . not $ content == []
  else
    throw DBDoesNotExistException

{-|
  This function takes a @String@ with the name of the category and returns
  @True@ if the category already exists in the database.
-}
categoryAlreadyExists :: String -> IO Bool
categoryAlreadyExists categoryName = do
  fileExists <- doesFileExist "books.db"
  if fileExists then do
    conn <- connectSqlite3 "books.db"
    content <- quickQuery' conn "SELECT * FROM category WHERE name = ?"
        [toSql categoryName]
    disconnect conn
    return . not $ content == []
  else
    throw DBDoesNotExistException

{-|
  This function takes a @String@ with the name of a table, and returns a list
  with the names of all the items that exist in the specified table
  alphabetically sorted.
-}
getAllNames :: String -> IO [String]
getAllNames table = do
  conn <- connectSqlite3 "books.db"
  content <- quickQuery' conn ("SELECT name FROM " ++ table ++
                               " ORDER BY name") []
  disconnect conn
  return [fromSql . head $ x | x <- content]

{-|
  This generic function takes a @String@ with the SQL query and a list of
  @SqlValue@ elements, and returns a list of lists of @SqlValue@ elements with
  the results that match the query, or @Nothing@ if no results were found.
-}
filterBooks :: String -> [SqlValue] -> IO (Maybe [[SqlValue]])
filterBooks query values = do
  fileExists <- doesFileExist "books.db"
  if fileExists then do
    conn <- connectSqlite3 "books.db"
    result <- quickQuery' conn query values
    disconnect conn
    if (null result) then
      return Nothing
    else do
      return . Just $ result
  else
    throw DBDoesNotExistException

{-|
  This function takes a @String@ with the ISBN-13 of a book, and returns a tuple
  with the title of the book, a list with its authors, the date of publication,
  the publisher, the number of pages, and a list with its categories. It returns
  @Nothing@ if there were found no books with the specified ISBN, and any null
  elements are also replaced by @Nothing@ in the tuple.
-}
getBookInfo :: String -> IO (Maybe (String, [String], Maybe String,
    Maybe String, Maybe Int, Maybe [String]))
getBookInfo isbn = do
  fileExists <- doesFileExist "books.db"
  if fileExists then do
    bookExists <- bookAlreadyExists isbn
    if bookExists then do
      conn <- connectSqlite3 "books.db"
      bookInfo <- quickQuery' conn ("SELECT title, publishedDate, " ++
          "publisher, pageCount FROM book WHERE isbn = ?") [toSql isbn]
      if ((length bookInfo) /= 1) then do
        disconnect conn
        return Nothing
      else do
        let book = bookInfo !! 0
        listAuthors <- quickQuery' conn ("SELECT a.name FROM " ++
            "author a, book b, bookAuthor ba WHERE b.isbn = ? AND " ++
            "b.isbn = ba.book AND ba.author = a.name ORDER BY a.name")
            [toSql isbn]
        listCategories <- quickQuery' conn ("SELECT c.name FROM " ++
            "book b, bookCategory bc, category c WHERE b.isbn = ? " ++
            "AND b.isbn = bc.book AND bc.category = c.name " ++
            "ORDER BY c.name") [toSql isbn]
        disconnect conn
        return . Just $ (fromSql $ book !! 0,
                         [fromSql . head $ x | x <- listAuthors],
                         fromSql $ book !! 1,
                         fromSql $ book !! 2,
                         fromSql $ book !! 3,
                         (if (null listCategories) then Nothing
                          else Just [
                            fromSql .head $ x | x <- listCategories
                          ]))
    else
      return Nothing
  else
    throw DBDoesNotExistException

{-|
  This function takes a @String@ with the name of the author, and removes all
  the books by the specified author, their association with their authors, their
  association with their categories, and removes the author from the database.
-}
removeAuthor :: String -> IO ()
removeAuthor author = do
  fileExists <- doesFileExist "books.db"
  if fileExists then do
    authorExists <- authorAlreadyExists author
    if authorExists then do
      books <- filterBooks ("SELECT b.isbn FROM author a, book b, " ++
          "bookAuthor ba WHERE a.name = ? AND a.name = ba.author AND " ++
          "ba.book = b.isbn") [toSql author]
      if (isNothing books) then
        putStrLn $ "There were found no books by " ++ author ++ "."
      else do
        putStrLn "Removing author and their books..."
        conn <- connectSqlite3 "books.db"
        removeBooks [head x | x <- (fromJust books)]
        commit conn
        disconnect conn
        let booksRemoved = length . fromJust $ books
        putStrLn $ "Removed " ++ (show booksRemoved) ++
                   (if (booksRemoved == 1) then " book"
                    else " books") ++ " by " ++ author ++ "."
    else
        putStrLn $ "The author was not found in the database."
  else
      throw DBDoesNotExistException

{-|
  This recursive function takes a list of @SqlValue@ elements with ISBN-13 codes
  of books, and removes those books and the associations with their authors. It
  uses the function @removeBookProperties@, which takes a @Connection@, a String
  with the name of a table (@author@ or @category@), a String with a property
  (@bookAuthor@ or @bookCategory@), and a list of @SqlValue@ elements with the
  names of the authors of the book, and removes the association between the book
  and their authors, and between the book and their categories. If an author
  does not have any more books in the database, then the author is also deteled
  from the database. If there are no more books in the database with a deleted
  category, then the category is also deleted from the database.
-}
removeBooks :: [SqlValue] -> IO ()
removeBooks [] = return ()
removeBooks (x:xs) = do
  conn <- connectSqlite3 "books.db"
  bookAuthors <- quickQuery' conn ("SELECT a.name FROM author a, book b, " ++
      "bookAuthor ba WHERE ba.book = ? AND ba.author = a.name") [x]
  bookCategories <- quickQuery' conn ("SELECT c.name FROM book b, " ++
      "bookCategory bc, category c WHERE b.isbn = ? AND b.isbn = bc.book " ++
      "AND bc.category = c.name") [x]
  removeBookProperties conn "author" "bookAuthor" [head x | x <- bookAuthors]
  removeBookProperties conn "category" "bookCategory"
      [head x | x <- bookCategories]
  quickQuery' conn "DELETE FROM book WHERE isbn = ?" [x]
  commit conn
  disconnect conn
  removeBooks xs
  where removeBookProperties :: Connection -> String -> String ->
                                [SqlValue] -> IO ()
    removeBookProperties _ _ _ [] = return ()
    removeBookProperties conn table property (x:xs) = do
      quickQuery' conn ("DELETE FROM " ++ property ++ " WHERE " ++
                        table ++ " = ?") [x]
      books <- quickQuery' conn ("SELECT b.isbn FROM " ++ table ++
          " t, " ++ "book b, " ++ property ++ " p WHERE t.name = ? " ++
          "AND t.name = p." ++ table ++ " AND p.book = b.isbn") [x]
      if (null books) then do
        quickQuery' conn ("DELETE FROM " ++ table ++
                          " WHERE name = ?") [x]
        removeBookProperties conn table property xs
      else
        removeBookProperties conn table property xs

{-|
  This function deletes the database @books.db@. It throws a
  'DBDoesNotExistException' exception if the database does not exist.
-}
deleteDB :: IO ()
deleteDB = do
  fileExists <- doesFileExist "books.db"
  if fileExists then do
    removeFile "books.db"
    putStrLn "Database successfully deleted."
  else
    throw DBDoesNotExistException
