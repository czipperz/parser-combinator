module Text.ParserCombinator.Pos where

data Pos = Pos String Int Int
  deriving (Eq, Show)

data Tag a = Tag Pos a
  deriving (Eq, Show)

initialPos :: String -> Pos
initialPos fileName = Pos fileName 1 1

incrementPosChar :: Pos -> Char -> Pos
incrementPosChar (Pos f l _) '\n' = Pos f (l + 1) 1
incrementPosChar (Pos f l c) _ = Pos f l (c + 1)

sourceName :: Pos -> String
sourceName (Pos f _ _) = f

sourceLine :: Pos -> Int
sourceLine (Pos _ l _) = l

sourceColumn :: Pos -> Int
sourceColumn (Pos _ _ c) = c

displayPos :: Pos -> String
displayPos (Pos f l c) = f ++ ':': show l ++ ':': show c

pos :: Tag a -> Pos
pos (Tag p _) = p

unwrapTag :: Tag a -> a
unwrapTag (Tag _ x) = x

displayTag :: Tag String -> String
displayTag (Tag p a) = displayPos p ++ ':': a

instance Ord Pos where
  Pos _ l c <= Pos _ l' c' = l < l' || (l == l' && c <= c')

instance Functor Tag where
  fmap f (Tag p x) = Tag p (f x)
