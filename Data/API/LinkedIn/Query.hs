module Data.API.LinkedIn.Query
       ( Query(..)
       ) where

import Network.OAuth.Http.Request

class Query q where
  toPathSegments :: q -> [String]
  toQueryItems :: q -> [(String, String)]

  toPathComps :: q -> [String]
  toPathComps = (++) ["", "v1"] . toPathSegments

  toQueryStr :: q -> FieldList
  toQueryStr = fromList . toQueryItems
