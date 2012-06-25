{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Data.API.LinkedIn.QueryResponsePair
       ( QueryResponsePair(..)
       ) where

class QueryResponsePair q r | q -> r where
