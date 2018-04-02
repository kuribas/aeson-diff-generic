# Welcom to `aeson-diff-generic` [![Hackage](https://img.shields.io/hackage/v/aeson-diff-generic.svg)](https://hackage.haskell.org/package/aeson-diff-generic) [![Build Status](https://travis-ci.org/kuribas/aeson-diff-generic.svg)](https://travis-ci.org/kuribas/aeson-diff-generic)

aeson-diff-generic is a haskell library that allows you to apply a
JSON patch [rfc6902](https://tools.ietf.org/html/rfc6902) document
directly to a haskell datatype.  A JSON Patch document is a sequence
of instructions to modify a JSON value.  It is suitable for use with 
the HTTP PATCH method.  

Suppose you are writing a client/server application with
auto-save. Every time the user makes a change to the document, the
client needs to tell the server about it. If the document being edited
is large, sending the entire updated document is impractical, so you
want to send a diff instead. JSON patch
[rfc6902](https://tools.ietf.org/html/rfc6902) is a standard format
for representing diffs, but it applies to JSON documents, not Haskell
values. Let's say our document is represented by a Haskell value of
type Doc: we need a Doc patch, not a json patch.

The aeson library uses GHC.Generics or template haskell to define a
default json encoding for algebraic datatypes. The aeson-diff-generic
can generate code, using options given to aeson, to interpret a json
patch as a Doc patch.

## Example

Suppose we have a datatype for which we have a ToJSON and FromJSON instance:

```haskell
{-# LANGUAGE DeriveGeneric, TemplateHaskell, OverloadedStrings #-}
import Data.Aeson
import Data.Aeson.Diff
import Data.Aeson.Diff.Generic
import GHC.Generics

data Pet = Bird | Cat | Dog | Fish
  deriving (Show, Eq, Generic)

data Person = Person
  { name :: String
  , age  :: Int
  , pet  :: Pet
  } deriving (Show, Eq, Generic)

instance ToJSON Pet where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions
  
instance FromJSON Pet where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Person where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions
  
instance FromJSON Person where
  parseJSON = genericParseJSON defaultOptions
```

We can now create a JsonPatch instance for our datatypes.  Creating
one by hand is tedious, so aeson-diff-generic gives two alternative
ways to create one: using the `FieldLens` class, or using template
haskell with the "Data.Aeson.Diff.Generic.TH" module.

## Creating instances with fieldlens.

A fieldlens maps a `Key` and onto a getter and setter into the given data.

For our Pet datatype we don't have any sub-data, so it just returns an error:

```haskell
instance FieldLens Pet where
  fieldLens _ _ = Error "Invalid Path"
```

In fact, this is the default implementation, so we can do simply:

```haskell
instance FieldLens Pet
```

For the Person datatype we map each field to the `GetSet` type:

```haskell
instance FieldLens Person where
  fieldLens (OKey field) (Person name_ age_ pet_) =
    case field of
      "name" -> pure $ GetSet name_ (\v -> pure $ Person v age_ pet_)
      "age"  -> pure $ GetSet age_ (\v -> pure $ Person name_ v pet_)
      "pet"  -> pure $ GetSet pet_ (\v -> pure $ Person name_ age_ v)
      _ -> Error "Invalid Path"
  fieldLens _ _ = Error "Invalid Path"
```

Now our `JsonPatch` instance will automatically use the `FieldLens` instance.

```haskell
instance JsonPatch Person
instance JsonPatch Pet
```

## Creating instances with fieldlens.

FieldLens still involves some boilerplate.  We can avoid that by using
the template haskell functions from "Data.Aeson.Diff.Generic.TH":

```haskell
instance JsonPatch Pet
instance FieldLens Pet

deriveJsonPatch defaultOptions ''Person
```

## applying patches

Now we can apply patches to our data:

```haskell
> joe = Person "Joe" 32 Fish
> john = Person "John" 32 Bird
> patch = Data.Aeson.Diff.diff (toJSON joe) (toJSON john)

> import qualified Data.ByteString.Lazy.Char8 as ByteString
> ByteString.putStrLn $ encode patch
[{"op":"replace","path":"/name","value":"John"},{"op":"replace","path":"/pet","value":"Bird"}]

> Success json = Data.Aeson.Diff.patch patch (toJSON joe)
> ByteString.putStrLn $ encode json
{"age":32,"name":"John","pet":"Bird"}

> Data.Aeson.Diff.Generic.patch patch joe
Success (Person {name = "John", age = 32, pet = Bird})
```

# Join in!

I am happy to receive bug reports, fixes, documentation enhancements,
and other improvements.

Please report bugs via the
[github issue tracker](http://github.com/kuribas/aeson-diff-generic/issues).

Master [git repository](http://github.com/kuribas/aeson-diff-generic):

* `git clone git://github.com/kuribas/aeson-dif-generic.git`

See what's changed in recent (and upcoming) releases:

* https://github.com/kuribas/aeson-diff-generic/blob/master/changelog.md

(You can create and contribute changes using git)


# Authors

This library is written by Kristof Bastiaensen.
