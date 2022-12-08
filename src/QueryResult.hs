module QueryResult
  ( QueryResult (..)
  , toBytestring
  ) where

import qualified Data.ByteString.Char8 as B
import Data.String (fromString)
import Types (Description (..), Index (..), Tag (..), TodoItem (..))

data QueryResult
  = Added Index
  | Done
  | Found Int [TodoItem]
  deriving (Show, Eq)

toBytestring :: QueryResult -> B.ByteString
toBytestring result = case result of
  Added i     -> fromString (show $ getIndex i)
  Done        -> "done"
  Found n items ->
    fromString (show n <> " item(s) found")
    <> foldMap itemToBytestring items
    where
      itemToBytestring (TodoItem (Index index) (Description d) _ tags) =
        "\n"
        <> fromString (show index)
        <> " \""
        <> d
        <> "\""
        <> foldMap showTag tags
        where
          showTag (Tag tag) = " #" <> tag
