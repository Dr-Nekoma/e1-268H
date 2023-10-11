{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module LSMachine ( LSMachine (..)
                 , Address (..)
                 ) where

import Data.Word
import DCPU (Register)

data Address = Reg Register
             | Ram Word16
  deriving (Eq, Show)


class (Functor m, Monad m) => LSMachine m where
  load :: Address -> m Word16
  store :: Address -> Word16 -> m ()
