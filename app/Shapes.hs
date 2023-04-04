module Shapes where
import Numeric (showHex)
import Data.Char (toUpper)

type Point = (Double,Double)

type OI = Double

data Color = Color { red :: OI, green :: OI, blue :: OI }



newtype Field = F (Point -> Double)

newtype Mask = M (Point -> OI)

data Shape = Shape Mask Color



{--
func myF (uv: Point) -> OI = {
  let (u,v) = uv
  sin(u) * cos(v)
}

myField = Field(f: myF)
myMask = Mask(field: myField, slice: 2.0, blur: 0.1)
myShape = Shape(mask: myMask, color: myColor)

--}
type Name = String
data Type = TOI | TColor | TPoint | TField | TMask | TShape
data Expr
  = EVar Name
  | ENum Double
  | EVec [Expr]
  | EBinOp Expr Op Expr
  | EFun Name [(Name,Expr)]

data Stmt = SLet [Name] Expr
          | SExpr Expr

data Op = Add | Sub | Mul | Div | Pow

data ProgStmt = PStmt Stmt
              | PFunc Name [(Name,Type)] Type [Stmt]

toHex :: (Integral a, Show a) => a -> String
toHex i
  | length o == 1 = '0':o
  | otherwise     = o
  where
    o = map toUpper $ showHex i ""

-- d16to8 :: Integral a => a -> Integer
d16to8 i = (255 * d16to15 + 16385) `div` 32768
  where d16to15 =  (32768 * (toInteger i) + 32768) `div` 65535

dto8 :: Double -> Integer
dto8 i = round $ 255 * i
colorToHex :: Color -> String
colorToHex (Color a b c) = "#" ++ (toHex . dto8 $ a) ++ (toHex.dto8 $ b) ++ (toHex.dto8 $ c)
{--
--}



