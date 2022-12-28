module Todo
  ( TodoListM
  , runTodoList
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State.Strict (StateT ( .. ) , get, gets, modify, runStateT)
import qualified Data.ByteString.Char8 as B
import Data.Char (isLetter, toLower)
import Data.Sequence ( ( <| ), ( |> ) )
import Data.Sequence as S
import Data.Foldable ( toList )
import Data.Maybe (fromJust)
import Data.Coerce ( coerce )
--import Data.Function (on)
--import Data.List (intersect)
--import Data.List.NonEmpty (toList)

import qualified Types as T
import Types (MonadTodoList (..))

data TodoList = TodoList { getQuantity :: T.Index, getList :: S.Seq ( Maybe T.TodoItem ) }
  deriving (Show, Eq)

emptyTodoList :: TodoList
emptyTodoList = TodoList ( T.Index 0 ) S.empty

newtype TodoListM a = TodoListM { runTodoListM :: StateT TodoList IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runTodoList :: TodoListM a -> IO ()
runTodoList = void . flip runStateT emptyTodoList . runTodoListM

instance MonadTodoList TodoListM where
  add = \desc tags -> TodoListM $ StateT $ 
    \( TodoList n todoSeq ) -> do
      return $ ( n, ) $ 
        TodoList ( succ n ) $ 
        todoSeq |> 
        Just ( T.TodoItem n desc ( T.TiWord <$> B.words ( T.getDescription desc ) ) tags )

  done = \( T.Index i ) -> TodoListM $ modify $
    \( TodoList n todoSeq ) ->
    TodoList n $ S.update ( fromIntegral $ T.getIndex n ) Nothing todoSeq

  search = \( T.SearchParams words tags ) -> TodoListM $ StateT $ 
    \todoList@( TodoList _ todoSeq ) -> do
      let filtered = S.filter ( todoApproved words tags ) todoSeq
      return ( ( coerce $ toList $ fromJust <$> filtered, S.length filtered ), todoList )

todoApproved :: [ T.SearchWord ] -> [ T.Tag ] -> Maybe T.TodoItem -> Bool
todoApproved _ _ Nothing = False

todoApproved words [] ( Just ( T.TodoItem _ _ todoWords _ ) ) =
  coerce $
  flip all words $ 
  \( T.SearchWord word ) -> 
  flip any todoWords $
  \( T.TiWord todoWord ) -> word `isSubOf` todoWord

todoApproved [] tags ( Just ( T.TodoItem _ _ _ todoTags ) ) =
  coerce $
  flip all tags $ 
  \( T.Tag tag ) -> 
  flip any todoTags $
  \( T.Tag todoTag ) -> tag `isSubOf` todoTag

todoApproved words tags todoItem = 
  coerce $
  todoApproved words [] todoItem && 
  todoApproved [] tags todoItem

isSubOf :: B.ByteString -> B.ByteString -> Bool
"" `isSubOf` str = True
sub `isSubOf` "" = False
sub `isSubOf` str
  | hSub == hStr = tSub `isSubOf` tStr
  | otherwise = sub `isSubOf` tStr
  where
    hSub = B.head sub
    hStr = B.head str
    tSub = B.tail sub
    tStr = B.tail str
