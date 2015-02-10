--------------------------------------------------------------------------------
--  This file is part of diplomarbeit ("Diplomarbeit Johannes Weiß").         --
--                                                                            --
--  diplomarbeit is free software: you can redistribute it and/or modify      --
--  it under the terms of the GNU General Public License as published by      --
--  the Free Software Foundation, either version 3 of the License, or         --
--  (at your option) any later version.                                       --
--                                                                            --
--  diplomarbeit is distributed in the hope that it will be useful,           --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of            --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             --
--  GNU General Public License for more details.                              --
--                                                                            --
--  You should have received a copy of the GNU General Public License         --
--  along with diplomarbeit.  If not, see <http://www.gnu.org/licenses/>.     --
--                                                                            --
--  Copyright 2012, Johannes Weiß                                             --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
module Functionality.Goliath (readExprFromFile) where

-- # STDLIB
import Data.Text (Text)
import qualified Data.Text as T

-- # SITE PACKAGES
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (($$), ($=), (=$=))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT

-- # LOCAL
import Data.ExpressionTypes (Expr(..))

-- | Parse a polynomial from a file.
readExprFromFile :: forall a. (Show a, Read a, Num a)
                 => ([Expr a] -> Expr a) -- ^ Polynomial Buuilding function
                 -> FilePath             -- ^ File
                 -> IO (Expr a)          -- ^ Polynomial read, as 'Expr'
readExprFromFile buildPoly path =
    do elems <-
        runResourceT $
            CB.sourceFile path
            $= CT.decode CT.utf8
            =$= CT.lines
            =$= CL.mapM parseElements
            $$ CL.consume
       let expr :: Expr a
           expr = buildPoly (map Literal elems)
       return $! expr
    where parseElements :: Monad m => Text -> m a
          parseElements str =
              let parsed = readsPrec 0 (T.unpack str)
               in case parsed of
                    ((v, _):_) -> return $! v
                    _ -> fail "Parse failed"
