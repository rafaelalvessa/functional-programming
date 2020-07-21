# Lab Exercise Week 4

## Exercise 1

Implement the following standard functions from the Haskell prelude using
`foldl` and `foldr`:

- `myAnd :: [Bool] -> Bool`

`myAnd` returns the conjunction of a Boolean list. For the result to be `True`,
the list must be finite; `myAnd`, should, however return `False` if either the
list is finite, or the list is infinite, but there is a `False` value at some
finite index in it. Which version of fold do you use and why?

- `myOr :: [Bool] -> Bool`

Similar to `myAnd`: `myOr` returns the disjunction of a Boolean list. For the
result to be `False`, the list must be finite; `True`, however, results from a
`True` value at a finite index of a finite or infinite list.

- `myAny :: (a -> Bool) -> [a] -> Bool`

Applied to a predicate and a list, `myAny` determines if any element of the list
satisfies the predicate. For the result to be `False`, the list must be finite;
`True`, however, results from a `True` value for the predicate applied to an
element at a finite index of a finite or infinite list.

- `myAll :: (a -> Bool) -> [a] -> Bool`

Applied to a predicate and a list, `myAll` determines if all elements of the
list satisfy the predicate. For the result to be `True`, the list must be
finite; `False`, however, results from a `False` value for the predicate applied
to an element at a finite index of a finite or infinite list.

- `mySum :: Num a => [a] -> a`

The `mySum` function computes the sum of a finite list of numbers.

- `myProduct :: Num a => [a] -> a`

The `myProduct` function computes the product of a finite list of numbers.

- `myConcat :: [[a]] -> [a]`

Concatenate a list of lists.

- `myConcatMap :: (a -> [b]) -> [a] -> [b]`

Map a function over a list and concatenate the results.

`myMaximum :: Ord a => [a] -> a`

`myMaximum` returns the maximum value from a list, which must be non-empty,
finite, and of an ordered type. It is a special case of `maximumBy`, which
allows the programmer to supply their own comparison function.

- `myMinimum :: Ord a => [a] -> a`

`myMinimum` returns the minimum value from a list, which must be non-empty,
finite, and of an ordered type. It is a special case of `minimumBy`, which
allows the programmer to supply their own comparison function.

## Exercise 2

Which of the following are suitable for implementation using fold?

- Produce a list with `n` identical elements: `myReplicate :: a -> Int -> [a]`?
- Select the `n`th element of a list: `mySelect :: [a] -> Int -> a`?
- Decide if a value is an element of a list: `myElem :: Int -> [Int] -> Bool`?

## Exercise 3

Complete and test a recursive version of `quicksort`. Recall that `quicksort`
works as follows:


1. Split list using a key (may be first element of  list) into those elements
   less than the key, and those greater than (or equal).

2. Sort two parts using `quicksort`.

3. Append two sorted lists together.

Things to watch out for:

- Check that your algorithm works for empty lists and lists of length 1.
- Check that you always split into two shorter lists.

## Running with Docker

```sh
$ docker build -t week_04 .
$ docker run -it --rm week_04 ghci week_04.hs
```
