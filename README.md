# Declarative Graphics / Shapes

This code:

```haskell
picture :: Form
picture = groupBy toBottom [
  centered $ text defaultTextStyle "groupBy toRight",
  debugEnvelope $ padded 4 $ groupBy toRight [ orangeCircle, greenRectangle ],
  centered $ text defaultTextStyle "groupBy toLeft",
  debugEnvelope $ padded 4 $ groupBy toLeft [ orangeCircle, greenRectangle ] ]
  where
    thickOrangeLine = defaultLineStyle { color = (1, 0.5, 0), lineWidth = 2 }
    orangeCircle = outlined thickOrangeLine $ circle 40
    greenRectangle = outlined (solid (0, 1, 0)) $ rectangle 80 80
```

Produces this Image:

![TitleSVG](https://rawgithub.com/matheus23/DeclarativeGraphics/master/titleImage.svg)

The aim is to create a functional, declarative Shape rendering library. It also features Text rendering.

## Libraries used

* Cairo: Rendering Backend for Shapes
* Pango: Rendering Backend for Text
* Gtk: Backend for rendering to Window.

## Inspirations

* [Racket Picts](http://docs.racket-lang.org/quick/)
* API Heavily inspired by: [Elm Collages](http://library.elm-lang.org/catalog/evancz-Elm/0.12/Graphics-Collage)

## Similar Projects

* [Diagrams](http://projects.haskell.org/diagrams/)