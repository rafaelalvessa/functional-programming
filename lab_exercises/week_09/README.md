# Lab Exercise Week 9

## Database

### Exercise 1

Create a database `prices.db` containing the following table:

```sql
CREATE TABLE 'prices' (
    'Stock' varchar(40) NOT NULL,
    'Date' DATE NOT NULL,
    'Open' varchar(40) DEFAULT NULL,
    'High' varchar(40) DEFAULT NULL,
    'Low' varchar(40) DEFAULT NULL,
    'Close' varchar(40) DEFAULT NULL,
    'Volume' bigint(11) DEFAULT NULL,
    'Adj Close' varchar(40) DEFAULT NULL
)
```

### Exercise 2

Produce a record type that represents a row of the table.

### Exercise 3

Download a CSV file of Yahoo prices from
http://real-chart.finance.yahoo.com/table.csv?s=YHOO.

### Exercise 4

Read this into Haskell as a String, drop the first line and take the next 100.

### Exercise 5

Write a function that takes a line of the CSV file and produces an element of
the record type (using the stock name YHOO for the field stock).

### Exercise 6

Write a function that inserts a record of your stock type into your table.

### Exercise 7

Upload the (first hundred entries of) the CSV file to your table.

## HTTP

### Exercise 1

Download http://real-chart.finance.yahoo.com/table.csv?s=YHOO as a String into
Haskell.

### Exercise 2

Put the entries into a clean version of your database.

### Exercise 3

Write a function that will do this for other stocks such as: APPL, MSFT, ...

## Using the database

### Exercise 1

Write a function to access your database and calculate for how many of the last
100 days, Yahoo stock finished up.

### Exercise 2

Write a function to calculate for how many of the last 100 days Microsoft stock
did better in terms of percentage change than Yahoo.

## Running with Docker

```sh
$ docker build -t week_09 .
$ docker run -it --rm week_09 ghci week_09.hs
```
