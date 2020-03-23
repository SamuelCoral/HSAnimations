{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
module Math.Quaternion where

import Control.Lens
import Math.Types


data Quaternion a = Quaternion {
    _wq :: a,
    _xq :: a,
    _yq :: a,
    _zq :: a
} deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

$(makeLenses ''Quaternion)



instance ScalarMult Quaternion where
    f *. Quaternion w x y z = Quaternion (f*w) (f*x) (f*y) (f*z)

vectorPart :: Lens' (Quaternion a) (Point3D a)
vectorPart = lens (\ (Quaternion _ x y z) -> Point3D x y z) $
    \ (Quaternion w _ _ _) (Point3D x y z) -> Quaternion w x y z

fromScalar :: Num a => a -> Quaternion a
fromScalar w = Quaternion w 0 0 0

fromPoint :: Num a => Point3D a -> Quaternion a
fromPoint (Point3D x y z) = Quaternion 0 x y z


qi :: Num a => Quaternion a
qi = Quaternion 0 1 0 0



instance Floating a => Conjugate (Quaternion a) where
    conj (Quaternion w x y z) = Quaternion w (-x) (-y) (-z)



instance Floating a => Num (Quaternion a) where
    Quaternion w1 x1 y1 z1 + Quaternion w2 x2 y2 z2 =
        Quaternion (w1 + w2) (x1 + x2) (y1 + y2) (z1 + z2)

    Quaternion w1 x1 y1 z1 - Quaternion w2 x2 y2 z2 =
        Quaternion (w1 - w2) (x1 - x2) (y1 - y2) (z1 - z2)

    Quaternion w1 x1 y1 z1 * Quaternion w2 x2 y2 z2 = Quaternion
        (w1*w2 - x1*x2 - y1*y2 - z1*z2)
        (w1*x2 + x1*w2 + y1*z2 - z1*y2)
        (w1*y2 - x1*z2 + y1*w2 + z1*x2)
        (w1*z2 + x1*y2 - y1*x2 + z1*w2)

    abs (Quaternion w x y z) =
        let r = sqrt $ w*w + x*x + y*y + z*z in Quaternion r 0 0 0

    signum (Quaternion w x y z) =
        let r = sqrt $ w*w + x*x + y*y + z*z
        in Quaternion (w/r) (x/r) (y/r) (z/r)

    fromInteger w = Quaternion (fromInteger w) 0 0 0



instance Floating a => Fractional (Quaternion a) where
    fromRational w = Quaternion (fromRational w) 0 0 0

    recip (Quaternion w x y z) =
        let r = w*w + x*x + y*y + z*z
        in Quaternion (w/r) (-x/r) (-y/r) (-z/r)

    Quaternion w1 x1 y1 z1 / Quaternion w2 x2 y2 z2 =
        let r = w2*w2 + x2*x2 + y2*y2 + z2*z2 in Quaternion
            ((w1*w2 + x1*x2 + y1*y2 + z1*z2) / r)
            ((-w1*x2 + x1*w2 - y1*z2 + z1*y2) / r)
            ((-w1*y2 + x1*z2 + y1*w2 - z1*x2) / r)
            ((-w1*z2 - x1*y2 + y1*x2 + z1*w2) / r)



instance (Eq a, Floating a) => Floating (Quaternion a) where
    pi = Quaternion pi 0 0 0

    exp (Quaternion w x y z) =
        let f = exp w
            r = sqrt $ x*x + y*y + z*z
            fr = f * sin r / r
        in  if r == 0 then Quaternion f 0 0 0
            else Quaternion (f * cos r) (fr * x) (fr * y) (fr * z)

    log (Quaternion w x y z) =
        let q = sqrt $ w*w + x*x + y*y + z*z
            v = sqrt $ x*x + y*y + z*z
            f = acos (w / q) / v
        in  if v == 0 then Quaternion (log w) 0 0 0
            else Quaternion (log q) (f * x) (f * y) (f * z)

    sin q = (exp(qi*q) - exp(-qi*q))/(2 * qi)
    cos q = (exp(qi*q) + exp(-qi*q))/2
    tan q = qi*(exp(-qi*q) - exp(qi*q))/(exp(qi*q) + exp(-qi*q))
    asin q = -qi*log(qi*q + sqrt(1-q^2))
    acos q = -qi*log(q + sqrt(q^2 - 1))
    atan q = qi/2 * log((qi + q)/(qi - q))
    sinh q = (exp(q) - exp(-q))/2
    cosh q = (exp(q) + exp(-q))/2
    tanh q = (exp(q) - exp(-q))/(exp(q) + exp(-q))
    asinh q = log(q + sqrt(q^2 + 1))
    acosh q = log(q + sqrt(q^2 - 1))
    atanh q = 1/2*log((1 + q) / (1 - q))
