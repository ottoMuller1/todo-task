module Types
  ( Index (..)
  , SearchWord (..)
  , Description (..)
  , Tag (..)
  , SearchParams (..)
  , TodoItem (..)
  , MonadTodoList (..)
  , TiWord ( .. )
  ) where

import Data.ByteString.Char8 (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.String ( IsString )

newtype Index = Index { getIndex :: Word }
  deriving (Show, Eq, Enum, Bounded, Ord)

newtype Description = Description { getDescription :: ByteString }
  deriving (Show, Eq, Ord, IsString)

newtype TiWord = TiWord { getTiWord :: ByteString }
  deriving (Show, Eq, Ord, IsString, Semigroup, Monoid)

newtype Tag = Tag { getTag :: ByteString }
  deriving (Show, Eq, Ord, IsString, Semigroup, Monoid)

newtype SearchWord = SearchWord { getSearchWord :: ByteString }
  deriving (Show, Eq, Ord, IsString)

data SearchParams = SearchParams
  { spWords :: ![SearchWord]
  , spTags  :: ![Tag]
  } deriving (Show, Eq)

data TodoItem = TodoItem
  { tiIndex       :: !Index
  , tiDescription :: !Description
  , tiWords :: ![TiWord]
  , tiTags        :: ![Tag]
  } deriving (Show, Eq, Ord)

class MonadTodoList m where
  add    :: Description -> [Tag] -> m Index
  done   :: Index -> m ()
  search :: SearchParams -> m ([TodoItem], Int)
