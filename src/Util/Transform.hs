{-# LANGUAGE DeriveFunctor #-}
module Util.Transform where

data Point3D a =
    Point3D a a a
    deriving (Show, Ord, Eq, Functor)

instance Applicative Point3D where
    pure a = Point3D a a a
    (<*>) (Point3D fx fy fz) (Point3D x y z) = Point3D (fx x) (fy y) (fz z)

instance Foldable Point3D where
    foldr f acc (Point3D x y z) = f x $ f y $ f z acc

instance Num a => Num (Point3D a) where
  (+) p1 p2 = (+) <$> p1 <*> p2
  (*) p1 p2 = (*) <$> p1 <*> p2
  abs p = abs <$> p
  signum p = signum <$> p
  fromInteger = pure . fromInteger
  negate p = negate <$> p

transform1 (Point3D x y z) = Point3D x y z

transform2 (Point3D x y z) = Point3D x (-y) (-z)

transform3 (Point3D x y z) = Point3D x z (-y)

transform4 (Point3D x y z) = Point3D x (-z) y

transform5 (Point3D x y z) = Point3D (-x) y (-z)

transform6 (Point3D x y z) = Point3D (-x) (-y) z

transform7 (Point3D x y z) = Point3D (-x) z y

transform8 (Point3D x y z) = Point3D (-x) (-z) (-y)

transform9 (Point3D x y z) = Point3D y x (-z)

transform10 (Point3D x y z) = Point3D y (-x) z

transform11 (Point3D x y z) = Point3D y z x

transform12 (Point3D x y z) = Point3D y (-z) (-x)

transform13 (Point3D x y z) = Point3D (-y) x z

transform14 (Point3D x y z) = Point3D (-y) (-x) (-z)

transform15 (Point3D x y z) = Point3D (-y) z (-x)

transform16 (Point3D x y z) = Point3D (-y) (-z) x

transform17 (Point3D x y z) = Point3D z x y

transform18 (Point3D x y z) = Point3D z (-x) (-y)

transform19 (Point3D x y z) = Point3D z y (-x)

transform20 (Point3D x y z) = Point3D z (-y) x

transform21 (Point3D x y z) = Point3D (-z) x (-y)

transform22 (Point3D x y z) = Point3D (-z) (-x) y

transform23 (Point3D x y z) = Point3D (-z) y x

transform24 (Point3D x y z) = Point3D (-z) (-y) (-x)

transformations :: Num a => [Point3D a -> Point3D a]
transformations =
    [ transform1
    , transform2
    , transform3
    , transform4
    , transform5
    , transform6
    , transform7
    , transform8
    , transform9
    , transform10
    , transform11
    , transform12
    , transform13
    , transform14
    , transform15
    , transform16
    , transform17
    , transform18
    , transform19
    , transform20
    , transform21
    , transform22
    , transform23
    , transform24
    ]
