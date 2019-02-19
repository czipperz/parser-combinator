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

displayPos :: Pos -> String
displayPos (Pos f l c) = f ++ ':': show l ++ ':': show c

unwrapTag :: Tag a -> a
unwrapTag (Tag _ x) = x

displayTag :: Tag String -> String
displayTag (Tag p a) = displayPos p ++ ':': a

instance Functor Tag where
  fmap f (Tag p x) = Tag p (f x)
