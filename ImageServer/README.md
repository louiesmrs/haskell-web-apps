# Project 1

In this project you will create a drawing eDSL (you can take inspiration in your design from the Shape language we used in the lectures, or from the Pan language, or neither). You will combine it with the two web eDSL languages we saw (Scotty and Blaze) to produce a web application capable of delivering images. The web app does not need to be interactive (that is, the images can be hard-coded in the app), the point is to create a DSL that could be used to specify images and integrate it into an application.
You will use the JuicyPixels library to create the images (you will receive sample code for this. The library can serve as an almost drop-in replacement for the Ascii rendering layer in the example code, though there are also more efficient ways to make use of it).
This project is overall worth 25% (the second project will be worth 35%). The project is due at midnight on the 7th of November - that’s 4 weeks from the day of release. Any extensions to the deadline must be agreed in advance, in writing.
The project tasks, in detail, are:

    Design a suitable drawing eDSL and an interpretation function that renders drawings using JuicyPixels [60% of the project marks for this].
        Provide at least the following basic shapes: Circle, Rectangle, Ellipse, Polygon (this last should be a closed convex polygon defined by a series of points listed in clockwise order).
        Provide the following set of basic affine transformations: Scale, Rotate, Shear, Translate, and functions to combine the transformations with shapes.
        Provide a way to specify colour for each shape. This can be either a simple case with a single colour, or a function which can provide gradients.
        Provide a way to join two drawings together with relative positioning, as described in the shape
    As a UI provide a Scotty application which can render some (hard-coded) sample images that demonstrate the result. The images should be returned as PNG graphics rendered using JuicyPixels. You should include the text of the DSL program that produced the image in the web page, so that the user of the web app can see how the image was produced (the idea is that a future improvement could be to allow the user to edit this text and re-render it, but you don't have to implement that). [20% of the project marks for this]