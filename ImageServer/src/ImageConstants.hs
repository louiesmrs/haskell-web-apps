module ImageConstants(
  circleImage, rectangleImage, ellipseImage, polygonImage,
   shearRotatePolygon, shearRotateRectangle, scaleTranslateCircle,
   scaleTranslateEllipse, circleOverSquare, rectangleOverPolygon,
   circleLeftOfSquare, circleRightOfSquare, circleAboveOfSquare,
   circleBelowOfSquare, circleOverEllipse)  where
import Shapes

circleImage = [(scale (point 1 1) , circle, red)]
rectangleImage = [(scale (point 1 1) , rectangle 1 2,  green)]

squareImage = [(scale (point 0.25 0.25) <+> translate (point (-2) 2), square,  green)]


ellipseImage = [(scale (point 1 1) , ellipse 1 2, blue)]

polygonImage = [(scale (point 0.2 0.2), polygon [point (-2) 3.5, point 2 3.5, point 4 0, point 2 (-3.5), point (-2) (-3.5), point (-4) 0, point (-2) 3.5], yellow)]

scaleTranslateEllipse =  [(scale (point 0.75 0.75) <+> translate (point (-1.4) (-0.2)), ellipse 1 2, blue)]
scaleTranslateCircle = [(scale (point 0.75 0.75) <+> translate (point 1 (-1)), circle, red)]
shearRotatePolygon = [(scale (point 0.2 0.2) <+> rotate 270 <+> shear 180 0, polygon [point (-2) 3.5, point 2 3.5, point 4 0, point 2 (-3.5), point (-2) (-3.5), point (-4) 0, point (-2) 3.5], green)] 
shearRotateRectangle = [(scale (point 0.55 0.35) <+> rotate 180 <+> shear 0 90, rectangle 1 1, blue)]

ellipseImageRotated = [(scale (point 0.25 0.25), ellipse 2 1, blue)]


circleOverSquare = over circleImage squareImage 
circleLeftOfSquare = beside circleImage  squareImage "left" 
circleRightOfSquare = beside circleImage  squareImage "right" 
circleBelowOfSquare = beside circleImage squareImage "below" 
circleAboveOfSquare = beside circleImage  squareImage "above" 

rectangleOverPolygon = over shearRotateRectangle  shearRotatePolygon 

circleOverEllipse = over circleImage ellipseImageRotated 