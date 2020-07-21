# Lab Exercise Week 5

## Exercise 1

Implement the function `lookUp :: Name -> PhoneBook -> PhoneNumber`.

Improve your implementation using the `Maybe` type constructor:
`lookUp :: Name -> PhoneBook -> Maybe PhoneNumber`.

## Exercise 2

Consider the following type of binary trees:

```haskell
type Tree = Leaf Int | Node Tree Tree
```

Let us say that a tree is balanced if the number of leaves in the left and right
sub-trees of every node differs by at most one, with leaves themselves being
trivially balanced. Define  a function `balanced :: Tree -> Bool` that decides
if a tree is balanced or not.

_**Hint**: First define a function that splits a list into two halves whose
length differs by at most one._

## Exercise 3

Define a new type class with methods for addition and subtraction. For instance:

```haskell
class NumberLike a where
    add :: a -> a -> a
    sub :: a -> a -> a
```

Think of a way to make booleans and lists (`[a]`) belong to this type class.
And implement your instantiation.

```haskell
instance NumberLike Bool where
    -- ...

instance NumberLike [a] where
    -- ...
```

## Running with Docker

```sh
$ docker build -t week_05 .
$ docker run -it --rm week_05 ghci week_05.hs
```
