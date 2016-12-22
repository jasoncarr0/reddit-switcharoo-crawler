{-# LANGUAGE FlexibleContexts #-}

module SwitchCrawler.GraphBuilder 
( GraphBuilder (..)
, emptyGraphBuilder
, mapMap
, mapQueue
, enqueue
, dequeue
, modifyQueue
, mapContains
, insertMap
) where

import Control.Monad.State.Class
import Data.List
import qualified Data.Map.Strict as M

--a GraphBuilder is a queue and a map, for use with state
--Uses a list despite the expense of appending as most of our time
--is spent on the actual polling the API
data GraphBuilder a = GraphBuilder 
    { gbqueue :: [a] 
    , gbmap :: (M.Map a [a])
    }

emptyGraphBuilder :: GraphBuilder a
emptyGraphBuilder = GraphBuilder [] M.empty

mapMap :: (M.Map a [a] -> M.Map a [a]) -> GraphBuilder a -> GraphBuilder a
mapMap f (GraphBuilder queue map) = GraphBuilder queue (f map)

mapQueue :: ([a] -> [a]) -> GraphBuilder a -> GraphBuilder a
mapQueue f (GraphBuilder queue map) = GraphBuilder (f queue) map

enqueue :: MonadState (GraphBuilder a) m => [a] -> m ()
enqueue as = modify' $ mapQueue (++ as)

dequeue :: MonadState (GraphBuilder a) m => Int -> m [a]
dequeue n = do
    gb <- get
    let (ret, queue') = splitAt n (gbqueue gb)
    put gb {gbqueue=queue'}
    return ret

modifyQueue :: MonadState (GraphBuilder a) m => ([a] -> [a]) -> m ()
modifyQueue f = modify' $ mapQueue f

mapContains :: (MonadState (GraphBuilder a) m, Eq a) => m (a -> Bool)
mapContains = do
    GraphBuilder queue map <- get
    return $ \a -> elem a $ M.keys map

insertMap :: Ord a => MonadState (GraphBuilder a) m => a -> [a] -> m ()
insertMap k v = modify' $ mapMap $ M.insert k v
