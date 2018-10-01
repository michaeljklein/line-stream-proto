# line-stream-proto

Prototype library for parsing a line-based representation of input files by
gradually transforming a monadic stream of input into an AST.

## Input representation

We repeat getting a line until there are none left (`hIsEOF`),
and represent the computation as a `Monadic` stream of
`Text` lines using `Cofree`:

```haskell
hGetLineMaybe :: Handle -> MaybeT IO Text

hGetLinesIO :: Handle -> MaybeT IO (Cofree (MaybeT IO) Text)
```

## Parsing

Several pure/monadic combinators are provided.

The input stream is then parsed in pieces:
- Empty lines
- Line numbers
- Ensuring that lines are `<= 128` characters long
- Indentation, including indented blocks
- Line comments
- String literals

Prototype parsers for the following: (some mostly in comments):
- Results with source location ranges
- Paired delimiters
- State machine based parsing

```haskell
newtype EmptyText i = EmptyText { getEmptyText :: i }

ensureLeq128 :: (MonadFail m, Show i) => i -> Int -> Text -> m (i, Int, Text)

data Indent a = NonIndented !Int a -- [number of indents deep (indent is '\t' or "  ")] value
              | Indented !Int (Indent a) -- (+1) to number of indents deep

newtype LineComment = LineComment { getLineComment :: Text }

newtype StringLit = StringLit { getStringLit :: Text }
```

