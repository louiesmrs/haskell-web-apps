module Shapes(
  Shape, Point, Vector, Transform, Colour(..), ColourDrawing,
  point, getX, getY,
  empty, circle, square, rectangle, ellipse, polygon,
  identity, translate, rotate, scale, shear, (<+>),
  inside, colourPixel, over, beside, white, red, green, blue, black, yellow )  where
import Data.Word (Word8)


-- Utilities

data Vector = Vector Double Double
              deriving Show
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

zcross (Vector a b) (Vector a' b') = a * b' - b * a'

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c

-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
              deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

getX (Vector x y) = x
getY (Vector x y) = y

-- Shapes

type Point  = Vector

point :: Double -> Double -> Point
point = vector


data Shape = Empty
           | Circle
           | Square
           | Rectangle Vector
           | Ellipse Vector
           | Polygon [Point]
             deriving Show

empty, circle, square :: Shape

empty = Empty
circle = Circle
square = Square
rectangle width height = Rectangle (Vector width height)
ellipse width height = Ellipse (Vector width height)
polygon = Polygon

-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Matrix
           | Shear Matrix
             deriving Show

identity = Identity
translate = Translate
scale = Scale
shear x y = Shear $ matrix 1 (-sin x) (sin y) 1
rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)

t0 <+> t1 = Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity                   x = x
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform (Rotate m)                 p = invert m `mult` p
transform (Shear m)                  p = invert m `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p

-- Drawings
data Colour = Colour Word8 Word8 Word8
              deriving Show

white, black, red, green, blue, yellow :: Colour
color = Colour
white = Colour 255 255 255
black = Colour 0   0   0
red   = Colour 255 0   0
green = Colour 0 255 0
blue  = Colour 0   0  255
yellow = Colour 255 255 0
dark = Colour 38 45 54

type ColourDrawing = [(Transform,Shape,Colour)]
-- interpretation function for drawings
colourPixel :: Point -> ColourDrawing -> Colour
colourPixel p drawing = determineColour p drawing dark

determineColour p [] col = col
determineColour p ((t,s,c1 ):ds) c2 =
  if inside1 p (t,s,c1)
    then determineColour p ds c1
    else determineColour p ds c2


over :: ColourDrawing -> ColourDrawing -> ColourDrawing
over ((transform1, shape1, colour1) : _) ((transform2, shape2, colour2) : _) = [(transform2 <+> scale (point 1 1), shape2, colour2), (transform2 <+> scale (point 0.25 0.25), shape1, colour1)]

beside :: ColourDrawing -> ColourDrawing -> String -> ColourDrawing
beside ((t1, s1, c1) : _) ((t2, s2, c2) : _) position = case position of
  "left"  -> [(t2 <+> translate (point x 0), s1, c1), (t2, s2, c2)]
  "right" -> [(t2 <+> translate (point y 0), s1, c1), (t2 , s2, c2)]
  "above" -> [(t2 <+> translate (point 0 z), s1, c1), (t2 , s2, c2)]
  "below" -> [(t2 <+> translate (point 0 w), s1, c1), (t2 , s2, c2)]
  _       -> error "Invalid position"
  where
    (x, y, z, w) = case s2 of
      Ellipse (Vector width height) ->
        if width > height
          then (-1 - (width / height), 1 + width / height, -2, 2)
          else (-1-(width / height), 1+(width / height), -(height/width), (height/width))
      Rectangle (Vector width height) ->
        if width > height
          then (-1 - (width / height), 1 + (width / height), -1, 1)
          else (-1- (width / height), 1 + (width / height), -(height/width), height/width)
      Square -> (-2,2,-2,2)
      Circle -> (-2,2,-2,2)
      Polygon points ->
        let (Vector xMin yMin, Vector xMax yMax) = boundingBox points
        in (-xMax-1, xMax+1, -yMax-1, yMax+1)

boundingBox :: [Point] -> (Point, Point)
boundingBox points = (Vector xMin yMin, Vector xMax yMax)
  where
    xMin = minimum $ map getX points
    xMax = maximum $ map getX points
    yMin = minimum $ map getY points
    yMax = maximum $ map getY points


inside :: Point -> ColourDrawing -> Bool
inside p = any (inside1 p)

inside1 :: Point -> (Transform, Shape, Colour) -> Bool
inside1 p (t,s,_) = insides (transform t p) s

insides :: Point -> Shape -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm  p <= 1
(Vector x y) `insides` Rectangle (Vector a b) = (sqrt (y**2) <= 1) && (sqrt (x**2) <= a/b)
(Vector x y) `insides` Ellipse (Vector a b) = (x**2/(a/b)**2) + y**2 <= 1
(Vector x y) `insides` Polygon [p1, p2] = inPoly (Vector x y) p1 p2
(Vector x y) `insides` Polygon (p1:(p2:ps)) = inPoly (Vector x y) p1 p2 && (Vector x y `insides` Polygon (p2:ps))

inPoly (Vector x y) (Vector a b) (Vector c d) = zcross (Vector (a-x) (b-y)) (Vector (c-x) (d-y)) < 0


distance :: Point -> Double
distance (Vector x y ) = sqrt ( x**2 + y**2 )

maxnorm :: Point -> Double
maxnorm (Vector x y ) = max (abs x) (abs y)

testShape = (scale (point 10 10), circle)


