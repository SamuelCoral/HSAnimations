{-# LANGUAGE TemplateHaskell #-}
module Math.Types where

import Control.Lens



---
--- Conjugate numbers which should satisfy
--- x * (conj x) = abs x * abs x
---
class Num a => Conjugate a where
    conj :: a -> a
    
    conjMult :: a -> a -> a
    conjMult n1 n2 = n2 * n1 * conj n2


class ScalarMult f where
     (*.) :: Num a => a -> f a -> f a
     infixl 7 *.



data Point2D a = Point2D {
    _xp2 :: a,
    _yp2 :: a
}

$(makeLenses ''Point2D)


data Point3D a = Point3D {
    _xp3 :: a,
    _yp3 :: a,
    _zp3 :: a
}

$(makeLenses ''Point3D)



instance ScalarMult Point2D where
    f *. Point2D x y = Point2D (f*x) (f*y)

instance ScalarMult Point3D where
    f *. Point3D x y z = Point3D (f*x) (f*y) (f*z)



instance Floating a => Num (Point2D a) where
    Point2D x1 y1 + Point2D x2 y2 = Point2D (x1+x2) (y1+y2)
    Point2D x1 y1 - Point2D x2 y2 = Point2D (x1-x2) (y1-y2)
    Point2D x1 y1 * Point2D x2 y2 = Point2D (x1*x2) (y1*y2)
    abs (Point2D x y) = let a = sqrt (x*x + y*y) in Point2D a a
    signum (Point2D x y) = let a = sqrt (x*x + y*y) in Point2D (x/a) (y/a)
    fromInteger x = Point2D (fromInteger x) (fromInteger x)

instance Floating a => Num (Point3D a) where
    Point3D x1 y1 z1 + Point3D x2 y2 z2 = Point3D (x1+x2) (y1+y2) (z1+z2)
    Point3D x1 y1 z1 - Point3D x2 y2 z2 = Point3D (x1-x2) (y1-y2) (z1-z2)
    Point3D x1 y1 z1 * Point3D x2 y2 z2 = Point3D (x1*x2) (y1*y2) (z1*z2)
    abs (Point3D x y z) = let a = sqrt (x*x + y*y + z*z) in Point3D a a a
    signum (Point3D x y z) = let a = sqrt (x*x + y*y + z*z) in Point3D (x/a) (y/a) (z/a)
    fromInteger x = Point3D (fromInteger x) (fromInteger x) (fromInteger x)

