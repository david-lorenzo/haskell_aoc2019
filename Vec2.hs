{-# LANGUAGE NamedFieldPuns #-}

module Vec2 where

data Vec2 a = Vec2 {x :: a, y :: a} deriving (Show, Eq)

instance Num a => Num (Vec2 a) where
  (+) v1 v2 = Vec2 {x = x v1 + x v2, y = y v1 + y v2}
  (-) v1 v2 = Vec2 {x = x v1 - x v2, y = y v1 - y v2}
  (*) v1 v2 = Vec2 {x = x v1 * x v2, y = y v1 * y v2}
  abs (Vec2 {x, y}) = Vec2 {x=abs(x), y=abs(y)}
  fromInteger v = Vec2 {x=fromInteger v, y=0}
  signum (Vec2 {x, y}) = Vec2 {x=signum x, y=signum y}


instance (Num a, Ord a) => Ord (Vec2 a) where
  (<=) (Vec2 {x=x1, y=y1}) (Vec2 {x=x2, y=y2}) =
    case (x1==x2, y1==y2) of
      (True, True) -> (x1*x1 + y1*y1) <= (x2*x2 + y2*y2)
      (True, _   ) -> y1 <= y2
      (_   , _   ) -> x1 <= x2


instance Num a => Semigroup (Vec2 a) where
  (<>) = (+)


instance Num a => Monoid (Vec2 a) where
  mempty = Vec2 0 0


sqDistance :: Num a => Vec2 a -> Vec2 a -> a
sqDistance v1 v2 =
  let xd = x v1 - x v2
      yd = y v1 - y v2
  in xd*xd + yd*yd

toList :: Vec2 a -> [a]
toList v = [x v, y v]


normVec2 :: Integral a => Vec2 a -> Vec2 a
normVec2 (Vec2 {x, y}) =
  let cd = gcd x y
  in case (x, y) of
        (0, 0) -> Vec2 0                0
        (0, _) -> Vec2 x                (y `div` abs y)
        (_, 0) -> Vec2 (x `div` abs x)  y
        _      -> Vec2 (x `div` cd)     (y `div` cd)
