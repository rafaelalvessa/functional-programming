# Lab Exercise Week 7

## Exercise 1

Write a short Haskell program that prompts the user to input a positive whole
number and then replies either "Congratulations, your number is even. Half of it
is..." or "Congratulations, your number is odd. Its square is..."

You may find the function `read`, which can be used to convert a string to a
piece of data, useful. But you will probably need to give it type information.

The next few exercises consist of manipulating a simple spreadsheet. The file
`sample.csv` is a CSV file containing some simple student test results. The
first line contains headings and the subsequent lines are data consisting of the
student's name and their marks on each of questions 1 and 2 on some test. For
simplicity it can be assumed that no entry is blank.

## Exercise 2

Design a datatype `Student` that is suitable for holding a single (data) line of
this spreadsheet. But see the last bullet below.

## Exercise 3

Write the following functions:

- `readStudent:: String -> Student` that takes the string corresponding to a
  line of the csv file and produces a Student.
- `tot:: Student -> Real` that gives the student's total mark.
- Make `Student` into an instance of the type class `Show` where the `Show`
  function produces a line of the CSV file now augmented by their total mark.
- Write a function that takes the contents of the CSV file as a String and
  produces a String representing a new CSV file, now containing the totals.
- Write a function that takes two arguments. The first is the name of a CSV
  file, and the second is the name of a new CSV file that includes totals.
- If your functions are not capable of handling an arbitrary number of
  questions, change them so they are.

## Exercise 4

Write a program that reads in a spreadsheet as above, and prints one with:
totals on each line, and follows the data by a blank line followed by a line
with averages for each question and total, and a further line with standard
deviations (recall that the standard deviation can be calculated as the mean of
the squares minus the square of the mean). (Hint: you may want to transpose the
'matrix' of student marks, so that for each question you have a list of marks.)

## Exercise 5

Produce different versions of the functions above using do and pipelining `>>=`.

## Running with Docker

```sh
$ docker build -t week_07 .
$ docker run -it --rm week_07 ghci week_07.hs
```
