module Math.Projections where

import Math.Types
import Math.Quaternion



project3D :: Fractional a => a -> Point3D a -> Point2D a
project3D f (Point3D x y z) = Point2D (-f * y / x) (f * z / x)

