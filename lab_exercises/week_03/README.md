# Lab Exercise Week 3

## Exercise 1

Define the exponentiation function `myExp a b = a^b` for non-negative integers
using recursion (both in Haskell and F#). Show how `exp 2 3` is evaluated
(unfolding) using your definition.

## Exercise 2

Show how `length [1,2,3]` and `drop 3 [1,2,3,4,5]` are evaluated (unfolding).

## Exercise 3

Define the following **library** functions in Haskell using recursion:

- Decide if all logical values in a list are `True`.

```haskell
myAnd :: [Bool] -> Bool
```

- Concatenate a list of lists (use binary concatenation of lists `xs ++ ys`).

```haskell
myConcat :: [[a]] -> [a]
```

- Produce a list with `n` identical elements.

```haskell
myReplicate :: a -> Int -> [a]
```

- Select the `n`th element of a list.

```haskell
mySelect :: [a] -> Int -> a
```

- Decide if a value is an element of a list.

```haskell
myElem :: Int -> [Int] -> Bool
```

_**Remark**: Most of these functions are in fact defined in the prelude
using other library functions, rather than recursion._

## Exercise 4

Define both in Haskell and F# a recursive function - `merge :: [Int] -> [Int] ->
[Int]` - that merges two sorted lists to give a single sorted list. For example:

```haskell
> merge [2,5,6] [1,3,4]
[1,2,3,4,5,6]
```

## Exercise 5

Using merge, define a recursive function - `msort :: [Int] -> [Int]` - that
implements merge sort, in which the empty list and singleton lists are already
sorted, and any other list is sorted by merging together the two lists that
result from sorting the two halves of the list separately.

**Hint**: first define a function `halve :: [Int] -> ([Int],[Int])` that splits
a list into two halves whose length differ by at most one.

## Exercise 6

Using the five-step process, define the library function that calculates the sum
of a list of numbers.

## Running with Docker

```sh
$ docker build -t week_03 .
$ docker run -it --rm week_03 ghci week_03.hs
```
