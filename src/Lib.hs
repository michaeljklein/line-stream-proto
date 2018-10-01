{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Prelude hiding (fail)
import Data.Text (Text)
import qualified Data.Text.IO as IO
import System.IO
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Comonad.Cofree
import Control.Monad.Fail
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Unsafe as U

-- withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r

-- | `Maybe` get a `Text` line from a `Handle`
hGetLineMaybe :: Handle -> MaybeT IO Text
hGetLineMaybe handle = MaybeT $ do
  hIsEOF' <- hIsEOF handle
  if hIsEOF'
     then return Nothing
     else Just <$> IO.hGetLine handle -- :: Handle -> IO Text

hGetLinesIO :: Handle -> MaybeT IO (Cofree (MaybeT IO) Text)
hGetLinesIO = unfoldMaybe . hGetLineMaybe


-- IO (Maybe (Text, IO (Maybe (Text, ..

-- MaybeT IO (Text, MaybeT IO Text

-- MaybeT IO (Cofree (MaybeT IO) Text)

-- MaybeT IO Text -> MaybeT IO (Cofree (MaybeT IO) Text)

nothingT :: Monad m => MaybeT m a
nothingT = MaybeT (return Nothing)

overMaybeT :: Monad m => (a -> m b) -> MaybeT m a -> MaybeT m b
overMaybeT f = mapMaybeT (>>= maybe (return Nothing) (fmap Just . f))

-- | `Cofree` encodes the recursion of running a `MaybeT` monadic operation
-- repeatedly.
--
-- The unfolded structure looks like:
--
-- @
-- MaybeT (m (Maybe (a :< MaybeT (m (a :< ..
-- @
--
unfoldMaybe :: Monad m => MaybeT m a -> MaybeT m (Cofree (MaybeT m) a)
unfoldMaybe x = (:< unfoldMaybe x) <$> x


-- Example: Line numbers
-- fmap (enumCofree 1) . hGetLinesIO
--   :: (Enum i, Num i) => Handle -> MaybeT IO (Cofree (MaybeT IO) (i, Text))

enumCofree :: (Enum i, Functor f) => i -> Cofree f a -> Cofree f (i, a)
enumCofree i = scanCofree (const succ) i

scanCofree :: Functor f => (a -> b -> b) -> b -> Cofree f a -> Cofree f (b, a)
scanCofree f x ~(y :< ys) = (x, y) :< (scanCofree f (f y x) <$> ys)



-- fmap enumCofree . hGetLinesIO :: Enum i => Handle -> MaybeT IO (Cofree (MaybeT IO) (i, Text))

newtype EmptyText i = EmptyText { getEmptyText :: i }

parseEmptyText :: i -> Text -> Either (EmptyText i) (i, Int, Text)
parseEmptyText line txt = if len == 0
                             then Left  (EmptyText line)
                             else Right (line, len, txt)
  where
    len = T.length txt

failLine :: (MonadFail m, Show i) => i -> Text -> String -> m a
failLine line txt err = fail $ unlines [ "Line: ", show line
                                       , show txt
                                       , err
                                       ]

ensureLeq128 :: (MonadFail m, Show i) => i -> Int -> Text -> m (i, Int, Text)
ensureLeq128 line len x = if 128 < len
                             then failLine line x . unwords $ [ "Line is", show len, " characters long (128 <)." ]
                             else return (line, len, x)





data Indent a = NonIndented !Int a -- [number of indents deep (indent is '\t' or "  ")] value
              | Indented !Int (Indent a) -- (+1) to number of indents deep

parseIndent :: (MonadFail m, Show i) => i -> Int -> Text -> m (Either (EmptyText i) (i, Int, Indent Text))
parseIndent line 1   txt | isSpace (U.unsafeHead txt) = return (Left (EmptyText line))
                         | otherwise                  = failLine line txt "There are no valid non-space single-character programs in Flock"
parseIndent line len txt | numSpaces == 0              = return (Right (line, len, NonIndented (toEnum 0) txt))
                         | odd numSpaces              = failLine line txt . unwords $ [ "Odd number of indents:", show numSpaces ]
                         | otherwise                  = return (Right (line, len, Indented 0 (loop 1 (numSpaces - 2) (U.dropWord16 2 txt))))
  where
    numSpaces = countPrefixSpaces txt
    loop dep 0          txt' = NonIndented dep txt'
    loop dep spacesLeft txt' = Indented dep (loop (dep+1) (spacesLeft-2) (U.dropWord16 2 txt'))


countPrefix :: Enum i => (Char -> Bool) -> Text -> i
countPrefix p = fst . T.foldl' folder (toEnum 0, True)
  where
    folder  (i, False) _ = (i, False)
    folder ~(i, _    ) c = if p c
                              then (succ i, True )
                              else (i     , False)

countPrefixSpaces :: Enum i => Text -> i
countPrefixSpaces = countPrefix isSpace


-- "/-- (?<LineComment>.*)$/"
newtype LineComment = LineComment { getLineComment :: Text }

parseLineComment :: Int -> Text -> Maybe LineComment
parseLineComment len txt | len < 3                    = Nothing
                         | T.take 3 txt == "-- " = Just (LineComment (T.drop 3 txt))
                         | otherwise                  = Nothing


parseEndLineComment :: Int -> Text -> Maybe (LineComment, Text)
parseEndLineComment len txt | len < 4   = Nothing
                            | otherwise = case T.splitOn " -- " txt of
                          [x,y] -> Just (LineComment y, x)
                          _     -> Nothing

newtype StringLit = StringLit { getStringLit :: Text }

-- parseStrings :: Int -> Text -> m (Maybe (NonEmpty (Either StringLit Text)))
-- parseStrings len txt | T.length str == 0 = Nothing
--                      | _
--   where
--     (preString, str) = span (/= '"') txt

-- -- /^((\s*--\s(?<line_comment>).*)|((?<expr>.*) -- (?<end_line_comment>.*)))$/
-- ' ' -- /'(?<char_lit>.)'/

-- " "
-- ()
-- []
-- {}

-- MutableByteArray
--   length _ * sizeOf _ = 128
--   (OpenPairedDelimiter, beginPos, 0|endPos)

--     OpenPairedDelimiter, len | 0
--   | Space, len /= 0
--   | Digits, len /= 0
--   | Symbols, len /= 0
--   | UpperCased, len /= 0
--   | LowerCased, len /= 0
--   | CharLit
--   | '`', '\'', '#', '-', ':', ','

--   sum len == 128

--   sizeOf len <= 6

--   sizeOf = ceil (log2 (#OpenPairedDelimiters + 5))

--   4 paired delimiters: "", (), [], {}
--     len == 0 => unmatched
--     len > 0 => indexOf OpenDelimiter + len == indexOf CloseDelimiter
--   1 Fixed-length: CharLit
--     len == 3
--   5 Positive-length: Space, Digits, Symbols, UpperCased, LowerCased, CharLit
--     len /= 0


-- Ok, want each case for length (_ : Text) = 0..128
--   For 0, small cases, there are so few options that they can be written by hand.

--   State should include:
--     Paired-delimiter stack: There are 3 paired-delimiters + one option for end
--       => 2 bits + 6 bits (for len)
--       => (#bytes / 4) for just paired delimiters
--       => (#bytes    ) for paired delimiters with length
--     Comment | Null
--     Current MutableByteArray of lexed

--   128 * (10 bits for (kind & len) + 6 bits for.. )
--    well, we can devote one bit to '\0'..
--
  --  There are 85 printable ascii characters.
  --    -10 for digits
  --    -1 for space
  --    -2 for ()
  --    -2 for []
  --    -2 for {}
  --    -1 for ""
  --    -1 for ' => 'charLit' | '\''
  --    -1 for ` => '`'
  --    -4 for , ; : #
  --    -52 alphabet characters
  --    -10 remaining characters!

--   Digits , beginDigits, endDigits
--   CharLit, beginCharLit, endCharLit

--   Digits | Spaces | () | [] | {} | "" | ' ' | '\'' | '`' | ',' | ';' | ':' | '#' | Upper | Lower | Symbols

--   16 options == 4 bits
--   12 bits for begin/end -- begin, 0 if unmatched

--   (Int#, SmallArray# 128 Addr#) -- first two bits are delimiter type, remaining six are delimiter index

--   (.. Addr#) -- Text
--   Int# -- Stack length
--   MutableSmallArray# 128 Addr# -- Stack

--   Int# -- Lex position
--   MutableByteArray -- Length (16 / sizeOf Char#), Lex results


-- Ok, so we traverse the Text, unsafe iter'ing exactly as many times as its length (unrolled)
--   Beforehand, we begin (for any using a line) with a length 128 pre-allocated stack of Addr#'s (effectively random)
--   We also begin with a Length (16 / sizeOf Char#) MutableByteArray to store the lexing results in

-- Before we've reached several characters in, all options are pre-set,
--   after which, a tight loop takes over in which the characters are classified


-- "()[]{}\"'#,:;'`"


-- ()      | 0
-- []      | 1
-- {}      | 2
-- ""      | 3
-- ' '     | 4
-- '#'     | 5
-- ','     | 6
-- ':'     | 7
-- ';'     | 8
-- '\''    | 9
-- '`'     | 10
-- Digits  | 11
-- Lower   | 12
-- Upper   | 13
-- Spaces  | 14
-- Symbols | 15


--  0   1    2    3    4        5        6
-- f c |
--   | isSpace  c = (14, dropLen isSpace) -- Spaces
--   | isDigit  c = (11, dropLen isDigit) -- Digits
--   | isLower  c = (12, dropLen isAlpha) -- Lower
--   | isUppper c = (13, dropLen isAlpha) -- Upper
--   | isRegularSymbol c = (15, dropLen isRegularSymbol) -- Symbol
--   | otherwise = case c of
--                   '#'  -> (5,  (1, tail))
--                   ','  -> (6,  (1, tail))
--                   ':'  -> (7,  (1, tail))
--                   ';'  -> (8,  (1, tail))
--                   '`'  -> (10, (1, tail))
--                   '\'' -> if remainingLength < 2
--                              then error "Unmached character literal"
--                              else if remainingText ! 1 == '\''
--                              then (4, (2, drop 2))
--                              else (9, (1, tail))
--                   if openDelimiter then add to stack
--                                    else if c == head stack
--                                    then write ending lex location (nothing, (1, tail))
--                                    else error "unmatched closed delimiter"



-- λ> sort "()[]{}\"'#,:;`"
-- "\"#'(),:;[]`{}"
--
-- λ> fromEnum <$> sort "()[]{}\"'#,:;`"
-- [34,35,39,40,41,44,58,59,91,93,96,123,125]
--
-- λ> ups $ fromEnum <$> sort "()[]{}\"'#,:;`"
-- [Right (34,35),Right (39,41),Left 44,Right (58,59),Left 91,Left 93,Left 96,Left 123,Left 125]
isRegularSymbol :: Char -> Bool
isRegularSymbol c = upsElem (fromEnum c) [Right (34,35),Right (39,41),Left 44,Right (58,59),Left 91,Left 93,Left 96,Left 123,Left 125]

upsElem :: Ord a => a -> [Either a (a, a)] -> Bool
upsElem _  [] = False
upsElem x  (Left y:zs) = x < y || if x == y
                                     then False
                                     else upsElem x zs
upsElem x ~(Right ~(y, z):ws) = x < y || if x <= z
                                            then False
                                            else upsElem x ws


ups :: (Eq a, Num a) => [a] -> [Either a (a, a)]
ups [] = []
ups [x] = [Left x]
ups (x:y:zs) | x + 1 == y = loop x y zs
             | otherwise = Left x : ups (y:zs)
  where
    loop x' y' [] = [Right (x', y')]
    loop x' y' (z':zs') | y' + 1 == z' = loop x' z' zs'
                        | otherwise   = Right (x', y') : ups (z':zs')

-- f 0 = 1
-- f n = sum [ 4 * sum [f (n - space) | space <- [1..n]]
--           -- , sum [f (n - digit) | digit <- [0..n]]
--           -- , sum [f (n - symbol) | symbol <- [0..n]]
--           -- , sum [f (n - upper) | upper <- [0..n]]
--           , f (n - 3) -- char literal
--           , 6 * f (n - 1) -- char cases
--             -- beforePaired, inPaired, afterPaired
--           , 3 * sum [f i * f j * f k | i <- [0..n-2], j <- [0..n-(i+2)], k <- [0..n-(i+j+2)]]
--           ]



-- not paired


--   PairedDelimiter stack


--   [
--   ,
--   ,
--   ]



-- Ok, so we really only need Ranges for a 16-bit word,
--   since 6 bits will be devoted to the column and the remaining 10 bits devoted to the actual line number.
--   (That's a maximum of 128 characters per line and 1024 lines per file..)





-- For example, in Flock, this data type could be deconstructed as follows:

-- -- A generic interpretation of the data type:
-- fix $ \x -> () | Word16 | Word16 & Word16 | Word16 & Word16 & x & x

-- -- A generic folder:
-- fold'Ranges :
--   (() -> a -> a) -- EmptyRange
--   -> (Word16 -> a -> a) -- SingletonRange
--   -> (Word16 -> Word16 -> a -> a) -- Range
--   -> (Word16 -> Word16 -> a -> a -> a -> a) -- Ranges
--   -> a -- seed
--   -> Ranges -> a

-- -- A generic unfolder:
-- a -> (a -> Maybe (a, ..)) -> Ranges



-- data Ranges = EmptyRage
--             | SingletonRange !Word16
--             | Range !Word16 !Word16
--             | Ranges !Word16 !Word16 !Ranges !Ranges
--             deriving (Eq, Ord, Show)

-- insert :: Word16 -> Ranges -> Ranges
-- insert x EmptyRange = SingletonRange x
-- insert x (SingletonRange y) = case compare x y of
--                                 LT -> Range x y
--                                 EQ -> SingletonRange x
--                                 GT -> Range y x

-- insert x r@(Range y z) = case compare x y of
--                            LT -> if x + 1 == y
--                                     then Range x z
--                                     else Ranges x z (SingletonRange x) r
--                            EQ -> r
--                            GT -> case compare x z of
--                                    LT -> r
--                                    EQ -> r
--                                    GT -> if z + 1 == x
--                                             then Range y x
--                                             else Ranges y x r (SingletonRange x)

-- insert x r@(Ranges y z ry rz) = case compare x y of
--                                   LT -> if x + 1 == y
--                                            then Ranges x z _ the halfway point has changed, need to re-pivot




-- init:: Ranges i -> Ranges i

-- tail:: Ranges i ->

-- insert :: i -> Ranges i -> Ranges i

-- delete:: i -> Ranges i -> Ranges i

-- inserts :: i -> i -> Ranges i -> Ranges i

-- deletes :: i -> i -> Ranges i -> Ranges i

-- union :: Ranges i -> Ranges i -> Ranges i

-- intersection :: Ranges i -> Ranges i -> Ranges i

-- diff :: Ranges i -> Ranges i -> Ranges i





-- 1..3 Word#'s, 0..2 Addr#'s
-- 0 -- emptyRange
-- 1 -- singleton range
-- 2 -- range
-- 3 -- Ranges

-- [Indicator][lower(all but empty)][upper(all but empty, singleton)][addr(ranges)][addr(ranges)]

-- data Ranges = Ranges Addr#

-- emptyRange# :: Addr#
-- emptyRange# = nullAddr#

-- singletonRange# :: Word# -> Addr#
-- singletonRange# x = 1 , x

-- -- indexWord16OffAddr# :: Addr# -> Int# -> Word#

-- range :: Word16 -> Word16 -> Addr#
-- range x y | x < y     = 2 , x , y
--           | otherwise = error $ "not (x < y): " ++ unwords [x, y]

-- ranges :: Word16 -> Word16 -> Addr# -> Addr# -> Addr#


-- EmptyRange | SingletonRange a | Range a a | Ranges (NonEmpty (a, a)) = Vector a -- min, max, min, max, min, max..

-- data Ranges i where
--   Range0 :: Ranges i
--   Range1 :: i -> Ranges i
--   Range  :: i -> i -> Ranges i
--   Ranges :: i -> i -> Ranges i -> Ranges i -> Ranges i
--   deriving (Eq, Ord, Show)



-- IO.hPutStrLn :: Handle -> Text -> IO ()

-- withFile path ReadMode $ \handle -> do

-- ReadMode
-- WriteMode

someFunc :: IO ()
-- someFunc = mapM_ (\x -> putStrLn ("x: " ++ show x) >> print f) [0..]
someFunc = putStrLn "someFunc"
