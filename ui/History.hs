{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module History where

import Control.Lens

data History a = History
  { _back    :: ![a]
  , _current :: !(Maybe a)
  , _forward :: ![a]
  }
  deriving Show
makeLenses ''History

emptyHistory :: History a
emptyHistory = History
  { _back = []
  , _current = Nothing
  , _forward = []
  }

push :: (a -> a) -> a -> History a -> History a
push adj new History{..} = History
  { _back = maybe id ((:) . adj) _current _back
  , _current = Just new
  , _forward = []
  }

moveForward :: (a -> a) -> History a -> Maybe (History a)
moveForward adj History{..} = do
  (new,next) <- uncons _forward
  pure History
    { _back = maybe id ((:) . adj) _current _back
    , _current = Just new
    , _forward = next
    }

moveBack :: (a -> a) -> History a -> Maybe (History a)
moveBack adj History{..} = do
  (new,prev) <- uncons _back
  pure History
    { _back = prev
    , _current = Just new
    , _forward = maybe id ((:) . adj) _current _forward
    }
