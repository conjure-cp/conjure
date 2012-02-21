{-# LANGUAGE MultiParamTypeClasses #-}

module GenericOps.Coerce where

class Coerce sub sup where
    promote :: sub -> sup
    demote :: sup -> Maybe sub
