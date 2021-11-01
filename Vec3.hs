{-# LANGUAGE NamedFieldPuns #-}

module Vec3 where

data Vec3 a = Vec3 {x :: a, y :: a, z :: a} deriving (Show, Eq)

instance Num a => Num (Vec3 a) where
  (+) v1 v2 = Vec3 {x = x v1 + x v2, y = y v1 + y v2, z = z v1 + z v2}
  (-) v1 v2 = Vec3 {x = x v1 - x v2, y = y v1 - y v2, z = z v1 - z v2}
  (*) v1 v2 = Vec3 {x = x v1 * x v2, y = y v1 * y v2, z = z v1 * z v2}
  abs (Vec3 {x, y, z}) = Vec3 {x=abs(x), y=abs(y), z=abs(z)}
  fromInteger v = Vec3 {x=fromInteger v, y=0, z=0}
  signum (Vec3 {x, y, z}) = Vec3 {x=signum x, y=signum y, z=signum z}


instance (Num a, Ord a) => Ord (Vec3 a) where
  (<=) (Vec3 {x=x1, y=y1}) (Vec3 {x=x2, y=y2}) =
    case (x1==x2, y1==y2) of
      (True, True) -> (x1*x1 + y1*y1) <= (x2*x2 + y2*y2)
      (True, _   ) -> y1 <= y2
      (_   , _   ) -> x1 <= x2


instance Num a => Semigroup (Vec3 a) where
  (<>) = (+)


instance Num a => Monoid (Vec3 a) where
  mempty = Vec3 0 0 0


sqDistance :: Num a => Vec3 a -> Vec3 a -> a
sqDistance v1 v2 =
  let Vec3 {x=xd, y=yd, z=zd} = v1 - v2
  in xd*xd + yd*yd + zd*zd


toList :: Vec3 a -> [a]
toList v = [x v, y v, z v]
