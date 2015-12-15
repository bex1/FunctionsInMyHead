module Util where

data Point = Point
  {
    x :: Int,
    y :: Int
  } deriving (Show, Eq)

pTranslate :: Point -> Point -> Point
Point x1 y1 `pTranslate` Point x2 y2 = Point (x1 + x2) (y1 + y2)

data RotationDirection = Clockwise | Counterclockwise
  deriving (Show, Enum)

rotate90 :: RotationDirection -> Point -> Point
rotate90 Clockwise (Point x y)        = Point y (-x)
rotate90 Counterclockwise (Point x y) = Point (-y) x

data SteerDirection = SteerLeft | SteerRight
  deriving (Show, Enum)

steerVector :: SteerDirection -> Point
steerVector SteerLeft  = Point (-1) 0
steerVector SteerRight = Point   1  0
