
module Notes where

import Prelude hiding (product,fail,(**))
import Data.List (intercalate)

cross :: [a] -> [a] -> [[a]]
cross xs ys = [[x,y] | x <- xs, y <- ys]


cartesian :: [[a]] -> [[a]]
cartesian [] = [[]]
cartesian (xs:xss) = [x:ys | x <- xs, ys <- cartesian xss]

power :: [a] -> Int -> [[a]]
power xs n = cartesian $ map (const xs) [1..n]

class Back a where
  previous :: a -> [[a]]

initial :: Back a => a -> Bool
initial a | [[]] <- previous a = True
          | otherwise          = False

fail :: Back a => a -> Bool
fail a | [] <- previous a = True
       | otherwise        = False

data Trace a = Trace a [Trace a]

instance Show a => Show (Trace a) where
  show (Trace a []) = show a
  show (Trace a ts) = show a ++ " -| " ++ "{ " ++ intercalate "," (map show ts) ++ " }"

traces :: Back a => a -> [Trace a]
traces a | fail a = []
         | initial a = [Trace a []]
         | otherwise = do as <- previous a
                          let trs = map traces as
                          if any null trs
                            then []
                            else do tr <- concat trs
                                    return $ Trace a [tr]



instance Back Int where
  previous n | n < 0 = []
  previous 0 = [[]]
  previous n = [[n-1],[n-2]]

data Prop = PB Bool | PAnd Prop Prop | POr Prop Prop | PNot Prop

instance Show Prop where
  show (PB True) = "T"
  show (PB False) = "F"
  show (PAnd p q) = "(" ++ show p ++ " & " ++ show q ++ ")"
  show (POr (PNot p) q) = "(" ++ show p ++ " -> " ++ show q ++ ")"
  show (POr p q) = "(" ++ show p ++ " v " ++ show q ++ ")"
  show (PNot p) = "¬" ++ show p


instance Back Prop where
  previous (PB False) = []
  previous (PNot (PB True)) = []
  previous (PB True) = [[]]
  previous (PNot (PB False)) = [[]]
  previous (PAnd p q) = [[p,q]]
  previous (POr p q) = [[p],[q]]
  previous (PNot (PNot p)) = [[p]]
  -- previous (PNot p) = [[POr (PNot p) (PB False)]]
  previous (PNot (PAnd p q)) = [[PNot p],[PNot q]]
  previous (PNot (POr p q)) = [[PNot p,PNot q]]


-- Example : ¬(F & T)
ex1 :: Prop
ex1 = PNot $ PAnd (PB False) (PB True)


-- Example : F -> T
ex2 :: Prop
ex2 = POr (PNot $ PB False) (PB True)


-- Example : T -> (F -> F) & (T -> T)
ex3 :: Prop
ex3 = POr (PNot (PB True)) (PAnd (PNot (POr (PNot $ PB True) (PB False))) (POr (PNot $ PB True) (PB True)))


fix :: (t -> t) -> t
fix f = let { x = f x } in x


-- data D a = F (D a -> D a) | V a
-- instance Show a => Show (D a) where
--   show (F f) = "(fun)"
--   show (V x) = show x

-- (**) :: Show a => D a -> D a -> D a
-- (**) (F f) x = f x
-- (**) (V x) y = undefined -- error $ "Cannot apply " ++ show (V x) ++ " to " ++ show y

-- e :: D a
-- e = F (\y -> y)

-- instance Applicative D where 
--   pure 

-- data AppExp t v
--   = Val v
--   | Ter t
--   | App (AppExp t v) (AppExp t v)

-- instance (Show v,Show t) => Show (AppExp t v) where
--   show (Val v) = show v
--   show (Ter t) = show t
--   show (App m n) = "(" ++ show m ++ " " ++ show n ++ ")"
