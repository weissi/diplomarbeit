{-# LANGUAGE ScopedTypeVariables #-}
module Functionality.Goliath (readExprFromFile) where

-- # STDLIB
import Data.Text (Text)
import qualified Data.Text as T

-- # SITE PACKAGES
import Data.Conduit (($$), ($=), (=$=), runResourceT)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT

-- # LOCAL
import Data.ExpressionTypes (Expr(..))

readExprFromFile :: forall a. (Show a, Read a, Num a)
                 => ([Expr a] -> Expr a) -> FilePath -> IO (Expr a)
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
