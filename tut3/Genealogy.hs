module Genealogy where

type Person = String

father :: Person -> Maybe Person
father "Albert" = Just "Berti"
father "Berti"  = Just "Conrad"
father "Beate"  = Just "Dennis"
father "Conrad" = Just "Erich"
father "Eva"    = Just "Fritz"
father _        = Nothing

mother :: Person -> Maybe Person
mother "Albert" = Just "Beate"
mother "Berti"  = Just "Claudia"
mother "Beate"  = Just "Dora"
mother "Conrad" = Just "Eva"
mother _        = Nothing

parents :: Person -> [Person]
parents p = maybeToList (father p) ++ maybeToList (mother p)
  where
    maybeToList Nothing  = []
    maybeToList (Just a) = [a]

-- powerM ::
n = 5
