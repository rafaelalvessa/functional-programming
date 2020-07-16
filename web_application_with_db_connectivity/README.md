# Web Application with DB Connectivity

## Summary

The goal of this project is to implement a Haskell program for harvesting
information from the Web. Two examples of such programs are provided together
with this project description.

The first example provided is a basic Web crawler, a program recursively
downloads webpages, collects web links from these pages, and follows these links
to collect more pages. That is the basic principle of the Google bot, the Google
web crawler that "travels" the Internet collecting information from webpages.

The second example provided is the example given in the text book "Real World
Haskell" (Chapter 22). This is an application that visits webpages looking for
podcasts. The application then saves these podcasts into a database.

In both cases a component of "parsing" the webpage is needed, similarly to the
[JSON to XML translation using Aeson
project](../json_to_xml_translation_using_aeson). In this project the parsing
component can be simpler, as the focus is on the HTTP requests and database
connectivity.

Most web programs follow exactly this architecture: One component for web
requests, one component for parsing the information received, and one component
for saving/accessing this information on/from a database.

**Extra modules needed**: Two extra modules, which are not included in the
standard GHC package, will be needed:

- `Database.HDBC`
- `Database.HDBC.Sqlite3`
- `Network.HTTP`

`Text.XML.HaXml` will also be needed in order to run the podcast example, or if
an XML parser is needed.

## Module `HDBC` (Haskell Database Connectivity)

`HDBC` is able to connect to different SQL databases. It also includes the
database driver. In this project, Sqlite3 will be used as the database backend
and backend driver.

## Module `Network.HTTP`

The module `Network.HTTP` provides a HTTP client library, allowing programs to
make HTTP requests.

## Goal

The goal of this project is to:

- Choose a webpage or a file that is available on the web.
- Write a program that:
  - Downloads the page or file.
  - Parses the file to extract some information.
  - Stores that information on the database.
  - Uses that information to answer requests from the user.

A basic project consists of:

- A database with at least two tables.
- Each table with at least three columns/fields.
- A program split into four modules:
  - One module dealing with the HTTP request.
  - One module dealing with parsing the downloaded contents.
  - One module dealing with the database (save and retrieve).
  - One main module.
- A typical main module should implement the requests such as:
  - Initialise the database.
  - Download page/file from the web, parse it, and save obtained information on
    database.
  - And access points for functions that query the database and show some of the
    saved information.

## Sample Project 1

Extend the crawler example so that the database contains a new table of words.
For each page downloaded one should not just find the links, but find the blocks
of text in the web page. Split the text into words and save the words in the
"words" table, with a reference to the webpage the words came from. An example
query could be, given a word return a list of webpages in which this word
appears.

## Sample Project 2

In Yahoo Finance, at the end of each historical prices page one finds a link to
a CSV file containing the historical prices for any given company. For instance,
at the bottom of http://finance.yahoo.com/q/hp?s=%5EIXIC+Historical+Prices one
finds a link to a spreadsheet containing the historical prices for the NASDAQ
composite. Your application could download this file and extract the lows and
highs of the stock for each given date. A separate table could have a list of
all the different companies already downloaded (so the first table needs a
reference to the table of companies). An example function could be find the
cheapest or most expensive price of the stock over all the dates obtained.

## Ways of obtaining online data

There are two basic ways of harvesting data from the
Internet:

1. **Web scraping**. A program downloads an HTML file and parses the contents of
   that HTML for relevant information. This relies on the fact that the
   structure of the HTML will be fixed. If that's not the case your program
   might eventually fail to work.
2. **Web APIs**. It might be that the information required is already available
   in a structured way as a JSON or XML file. Such data might be obtained either
   directly from a permanent link, or more commonly it will be available through
   a Web API (such as REST). Some examples of websites that provide APIs:
   Amazon, Twitter, RottenTomatoes, last.fm.

## Project implementation

### `books-database`

This application provides ten different functionalities:

- `create`: Creates the database `books.db`.
- `delete`: Deletes the database `books.db`.
- `add [author]`: Adds all books by author to the database.
- `remove [author]`: Removes all books by author from the database.
- `co-authors`: Adds all books by all the co-authors present in the database.
- `books`: Displays all the books present in the database.
- `authors`: Displays all the authors present in the database.
- `categories`: Displays all the categories present in the database.
- `pages [author]`: Displays the total number of pages written by the author.
- `filter`: Provides an interactive filter based on several criteria.


### Running the application

```sh
$ ./books [command] [arguments]
```

#### Examples

```sh
$ ./books create
$ ./books delete
$ ./books add "Jane Austen"
$ ./books remove "Charles Dickens"
$ ./books co-authors
$ ./books books
$ ./books authors
$ ./books categories
$ ./books pages "Virginia Woolf"
$ ./books filter
```

_**Note**: Some parts of this application no longer work following Google's
deprecation of the Freebase API & database in 2016._
