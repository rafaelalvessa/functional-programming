# JSON to XML translation using Aeson

## Summary

The project consists of writing a JSON to XML translator using the Haskell
module Aeson. Your program should work on one of the many JSON datasets
available online. The core component of your program should be the function from
the JSON representation to the XML format, via an intermediate Haskell datatype.

**Aeson package**:
https://hackage.haskell.org/package/aeson-0.8.0.0/docs/Data-Aeson.html

## 1. Parsing JSON input into Haskell data type (using Aeson)

Once a particular JSON dataset has been chosen:

- Document the structure of the JSON dataset, concentrating on how the set is
  structured as a tree, and what types of information is included.
- Devise a Haskell data type that corresponds to the structure of the JSON file.
- Write a function (using Aeson) to navigate the JSON input and extract the
  strings giving the basic data.
- Convert JSON input string into an element of the corresponding Haskell data
  type.

## 2. Converting Haskell data type into XML format

Once the data is availabe as a Haskell data type:

- Write a function that converts the Haskell data type into XML format.
- Combine this with the function from Part 1 so as to provide a converter from
  JSON to XML.

## 3. Writing at least three functions to query parsed data

Three function that allows the user to query the input data must be provided.
For instance, if reading data about stock prices, the program could calculate
highest and lowest prices, average prices, etc. The program and the different
functions will be tested on `ghci`, so there is no need for a "main" function
that interacts with the user.
