# Lab Exercise Week 2

## Exercise 1

In the definition of `goo`, systematically move the `else` clause right one
character at a time till you get the definition to compile.

## Exercise 2

Write a function that will convert a telephone number to international format.
For simplicity, assume that you are given a valid UK phone number, possibly with
the area code bracketed and with spaces (e.g. `(01235) 678901`). Remove brackets
and spaces and prefix with `+44` (e.g. `+44 1235678901`).

## Exercise 3

A card consists of a name, address and phone number. Design a type `Card` that
implements this, and write the following functions:

- `hasname :: String -> Card -> Bool`, that returns `true` if the name on the
  card is the string given to the function.
- `lookup :: String -> Card -> Phone`, that returns the phone number on the card
  if the name on the card is the string given to the function.

## Running with Docker

```sh
$ docker build -t week_02 .
$ docker run -it --rm week_02 ghci week_02.hs
```
