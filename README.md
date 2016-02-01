# Tansu-BerkeleyDB

**WARNING: DO NOT USE THIS LIBRARY.** Like, seriously, there
are a ton of other storage libraries that aren't fly-by-night
like this one.

This is a backend for the
[`tansu`](http://github.com/aisamanra/tansu)
library that supports storing and retrieving data using the
[Berkeley DB](https://en.wikipedia.org/wiki/Berkeley_DB)
library. Be aware that Berkeley DB, and therefore this code
as well, is GPL-licensed, and thus cannot be used in commercial
products.

## Example

~~~.haskell
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)

import Database.Tansu ((=:), get, run)
import Database.Tansu.Backend.BerkeleyDb (withBerkeleyDb)

data Person = Person { name :: String, age  :: Int }
  deriving (Eq, Show, Generic, Serialize)

main :: IO ()
main = withBerkeleyDb "sample.db" $ \ db -> do
  run db $ do
    "alex"  =: Person "alex" 33
    "blake" =: Person "blake" 22

  Right age <- run db (age `fmap` get "blake")
  putStrLn $ "Blake's age is " ++ show age
~~~

## Usage

Please see the corresponding section of the
[`tansu` library page](http://github.com/aisamanra/tansu#usage).
