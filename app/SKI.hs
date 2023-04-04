module SKI where

import Prelude hiding ((**), product,fail)
import Data.List (intercalate)

import Text.Parsec
import Text.Parsec.String

data AppExp t
  = Val t
  | App (AppExp t) (AppExp t)

instance Show t => Show (AppExp t) where
  show (Val v) = show v
  show (App m (App n k)) = show m ++ "(" ++ show (App n k) ++ ")"
  show (App m n) = show m ++ "" ++ show n

-- instance Functor AppExp where
--   fmap f (Val v) = Val (f v)
--   fmap f (App m n) = App (fmap f m) (fmap f n)

-- instance Applicative AppExp where
--   pure = Val
--   (Val f) <*> (Val a) = Val (f a)
--   (Val f) <*> (App m n) = App (Val f <*> m) (Val f <*> n)
--   (App f g) <*> m = App (f <*> m) (g <*> m)
{-------------------- SKI Combinator Language --------------------}

data SKI = S | K | I | V Char

instance Show SKI where
  show S = "S"
  show K = "K"
  show I = "I"
  show (V c) = [c]

type D = AppExp SKI

infixl 7 **
(**) :: D -> D -> D
(Val (V x)) ** a     = App (Val (V x)) a
(Val I) ** a         = a
(App (Val K) a) ** _ = a
(App (App (Val S) a) b) ** c = (a ** c) ** (b ** c)
a ** b = App a b


{-------------------- SKI Parser --------------------}

-- Parse a string into a SKI expression
skParser :: String -> D
skParser str = case parse app "" str of
               Left err -> error $ show err
               Right e -> e
  where 
        -- Parse a single SKI combinator
        combinatorTerm :: Parser SKI
        combinatorTerm = choice [char 'S' >> return S, char 'K' >> return K, char 'I' >> return I, lower >>= return . V]

        -- Parse a single application of SKI combinators
        app :: Parser D
        app = chainl1 factor (return App)

        -- Parse a factor, which is either a single combinator or an application in parentheses
        factor :: Parser D
        factor = choice [Val <$> combinatorTerm, between (char '(') (char ')') app]


skp :: String -> D
skp = skParser

{-------------------- Lambda Calculus Language --------------------}


data Term
  = TVar Char
  | TAbs Char Term
  | TApp Term Term

instance Show Term where
  show (TVar c) = [c]
  show (TAbs c t) = "(\\" ++ [c] ++ "." ++ show t ++ ")"
  show (TApp t1 (TApp t2 t3)) = show t1 ++ "(" ++ show (TApp t2 t3) ++ ")"
  show (TApp t1 t2) = show t1 ++ "" ++ show t2


{-------------------- Lambda Calculus Parser --------------------}

lcParser :: String -> Term
lcParser str = case parse term "" str of
               Left err -> error $ show err
               Right e -> e
  where 
        term :: Parser Term
        term = choice [app, abs, var]
        
        var :: Parser Term
        var = lower >>= return . TVar

        abs :: Parser Term
        abs = do
          char '\\'
          c <- lower
          char '.'
          t <- term
          return $ TAbs c t

        app :: Parser Term
        app = chainl1 factor (return TApp)

        -- Parse a factor, which is either a single combinator or an application in parentheses
        factor :: Parser Term
        factor = choice [var, abs, between (char '(') (char ')') term]



{-------------------- Lambda Calculus to SKI --------------------}

set x y f x' = if x == x' then y else f x'

-- sem :: (Char -> D) -> Term -> D
-- sem p (TVar c) = p c
-- sem p (TAbs c t) = 