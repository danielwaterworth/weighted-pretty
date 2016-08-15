{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Text.WeightedPretty (Doc(..), pretty) where

import Data.Maybe
import Data.Word
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Search

data Doc =
  And Doc Doc |
  Or Doc Doc |
  Text T.Text |
  Indented Doc |
  Cost Word64 Doc |
  Newline

instance Monoid Doc where
  mappend = And
  mempty = Text ""

data PrinterState =
  PrinterState {
    _col :: Int,
    _maxCol :: Int,
    _indentLevel :: Int,
    _output :: B.Builder
  }
makeLenses ''PrinterState

write :: T.Text -> StateT PrinterState (Search (Sum Word64)) ()
write t = do
  col %= ((+) (T.length t))
  c <- use col
  m <- use maxCol
  if c > m then
    mzero
   else
    output %= (<> B.fromText t)

newline :: StateT PrinterState (Search (Sum Word64)) ()
newline = do
  output %= (<> "\n")
  col .= 0
  i <- use indentLevel
  write $ T.pack $ replicate i ' '

indent :: StateT PrinterState (Search (Sum Word64)) a -> StateT PrinterState (Search (Sum Word64)) a
indent op = do
  indentLevel %= ((+) 2)
  x <- op
  indentLevel %= (flip (-) 2)
  return x

render :: Doc -> StateT PrinterState (Search (Sum Word64)) ()
render (And x y) = render x >> render y
render (Or x y) = render x `mplus` render y
render (Text x) = write x
render (Indented x) = indent $ render x
render (Cost n d) = cost' (Sum n) >> render d
render Newline = newline

initial = PrinterState 0 80 0 mempty

pretty :: Doc -> Maybe T.Text
pretty =
  fmap snd .
  listToMaybe .
  runSearch .
  fmap (L.toStrict . B.toLazyText . view (_2 . output)) .
  flip runStateT initial .
  render
