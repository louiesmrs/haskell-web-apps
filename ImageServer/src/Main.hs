{-# LANGUAGE OverloadedStrings #-}
module Main(main) where
import Shapes
import ImageConstants
import Render (render,defaultWindow)
import Data.Text.Lazy (Text, append, pack)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Base64 (encodeBase64)
import Data.Map.Lazy (Map, fromList, findWithDefault)
import Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R


data ImageInfo = ImageInfo
    { imageText :: Text
    , imageRender :: ByteString
    }

textLookup :: Text -> Text
textLookup name = imageText $ findWithDefault defaultImageInfo name imageMap

imageLookup :: Text -> ByteString
imageLookup name = imageRender $ findWithDefault defaultImageInfo name imageMap

defaultImageInfo :: ImageInfo
defaultImageInfo = ImageInfo "No matching image" "No matching image"

imageMap :: Map Text ImageInfo
imageMap = fromList
    [ ("circle", ImageInfo (pack $ show circleImage) (render defaultWindow circleImage))
    , ("rectangle", ImageInfo (pack $ show rectangleImage) (render defaultWindow rectangleImage))
    , ("ellipse", ImageInfo (pack $ show ellipseImage) (render defaultWindow ellipseImage))
    , ("polygon", ImageInfo (pack $ show polygonImage) (render defaultWindow polygonImage))
    , ("shearrotatepolygon", ImageInfo (pack $ show shearRotatePolygon) (render defaultWindow shearRotatePolygon))
    , ("shearrotaterectangle", ImageInfo (pack $ show shearRotateRectangle) (render defaultWindow shearRotateRectangle))
    , ("scaletranslatecircle", ImageInfo (pack $ show scaleTranslateCircle) (render defaultWindow scaleTranslateCircle))
    , ("scaletranslateellipse", ImageInfo (pack $ show scaleTranslateEllipse) (render defaultWindow scaleTranslateEllipse))
    , ("circleoversquare", ImageInfo (pack $ show circleOverSquare) (render defaultWindow circleOverSquare))
    , ("circleleftofsquare", ImageInfo (pack $ show circleLeftOfSquare) (render defaultWindow circleLeftOfSquare))
    , ("circlerightofsquare", ImageInfo (pack $ show circleRightOfSquare) (render defaultWindow circleRightOfSquare))
    , ("circleabovesquare", ImageInfo (pack $ show circleAboveOfSquare) (render defaultWindow circleAboveOfSquare))
    , ("circlebelowsquare", ImageInfo (pack $ show circleBelowOfSquare) (render defaultWindow circleBelowOfSquare))
    , ("rectangleoverpolygon", ImageInfo (pack $ show rectangleOverPolygon) (render defaultWindow rectangleOverPolygon))
    , ("circleoverellipse", ImageInfo (pack $ show circleOverEllipse) (render defaultWindow circleOverEllipse))
    ]



main = S.scotty 3000 $ do
    S.get "/" $ do
        S.html $ R.renderHtml $ H.body $ do
            H.h1 "Image Server - Choose your drawing"
            H.ul $ do
                H.li $ H.a H.! A.href "/circle" $ "Drawing of Circle"
                H.li $ H.a H.! A.href "/rectangle" $ "Drawing of Rectangle"
                H.li $ H.a H.! A.href "/ellipse" $ "Drawing of Ellipse"
                H.li $ H.a H.! A.href "/polygon" $ "Drawing of Polygon"
                H.li $ H.a H.! A.href "/shearrotatepolygon" $ "Drawing of Polygon Sheared and Rotated"
                H.li $ H.a H.! A.href "/scaletranslatecircle" $ "Drawing of Circle Scaled and Translated"
                H.li $ H.a H.! A.href "/scaletranslateellipse" $ "Drawing of Ellipse Scaled and Translated"
                H.li $ H.a H.! A.href "/shearrotaterectangle" $ "Drawing of Rectangle Sheared and Rotated"
                H.li $ H.a H.! A.href "/circleoversquare" $ "Drawing of Circle over Square"
                H.li $ H.a H.! A.href "/circleleftofsquare" $ "Drawing of Circle to the left of Square"
                H.li $ H.a H.! A.href "/circlerightofsquare" $ "Drawing of Circle to the right of Square"
                H.li $ H.a H.! A.href "/circleabovesquare" $ "Drawing of Circle above Square"
                H.li $ H.a H.! A.href "/circlebelowsquare" $ "Drawing of Circle below Square"
                H.li $ H.a H.! A.href "/rectangleoverpolygon" $ "Drawing of Rectangle over Polygon"
                H.li $ H.a H.! A.href "/circleoverellipse" $ "Drawing of Circle over Ellipse"
    S.get "/:imagePath" $ do
        imagePath <- S.captureParam "imagePath"
        S.html $ R.renderHtml $ H.body $ do
            H.h2 "Drawing as PNG"
            H.img H.! A.src (H.toValue $ append "data:image/png;base64," $ encodeBase64 $ imageLookup imagePath)      
            H.h2 "Input to produce Drawing"
            H.textarea H.! A.rows "10" H.! A.cols "90" $ H.toHtml $ textLookup imagePath
