module Graphics.Shape
import Data.Vect

%access public export
Point : Type
Point = (Double, Double)

data Path : (n : Nat) -> Type where
  Start  : Point -> Path Z
  Append : Path k -> Point -> Path (S k)

Line : Point -> Point -> Path 1
Line from to = (Start from) `Append` to

Triangle : Path 1 -> Point -> Path 3
Triangle (Append (Start x) y) z = ((Line x y) `Append` z) `Append` x

toPoints : Path n -> Vect (n+1) Point
toPoints (Start x)          = [x]
toPoints (Append path next) = let ps = next :: toPoints path
                              in reverse ps
