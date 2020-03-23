module Math.Projections where

import Math.Types
import Math.Quaternion



project3D :: Fractional a => a -> Quaternion a -> Point2D a
project3D f (Quaternion _ x y z) = Point2D (-f * y / x) (f * z / x)

projectCurve3D :: (Floating a, Functor f) => a -> Quaternion a -> Quaternion a -> f (Quaternion a) -> f (Point2D a)
projectCurve3D factor traslation rotor =
    fmap $ \ point -> project3D factor $ conjMult point rotor + traslation
