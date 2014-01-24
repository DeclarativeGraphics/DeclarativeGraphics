module Graphics.Main where

import Graphics.FRP
import Graphics.FRPUtils
import Graphics.Prototype.TextEdit

import Graphics.GtkFRP
import Graphics.Graphics
import Graphics.GraphicsUtils
import Graphics.CairoGraphics

import Graphics.UI.Gtk hiding (Point,Circle)
import Graphics.Rendering.Cairo

import Control.Monad
import Data.IORef
import Data.List


main :: IO ()
main = do
      print frpsys

      initGUI
      window <- windowNew
      set window [windowTitle := "CairoGraphics"
                 ]

      frame <- frameNew
      containerAdd window frame
      canvas <- drawingAreaNew
      containerAdd frame canvas

      canvas `set` [widgetCanFocus := True]
      widgetModifyBg canvas StateNormal (Color 65535 65535 65535)
      widgetAddEvents canvas [PointerMotionMask, PointerMotionHintMask]

      widgetShowAll window

      frpWidget canvas frpsys

      onDestroy window mainQuit
      mainGUI


keyEdit :: FRP (Event KeyVal) (Event (TextEdit -> TextEdit))
keyEdit = eventExtract keyDir
  where
    keyDir k = case keyName k of
      "Left" -> Just teLeft
      "Right" -> Just teRight
      "BackSpace" -> Just teBackspace
      "Return" -> Just (tePut '\n')
      _ -> liftM tePut (keyToChar k)


cairoDoubleBuffer rendering = do
  pushGroup
  rendering
  popGroupToSource
  paint

data Tree a = Node a [Tree a]

nodeCircle = addStdRefs $ Canvas 80 80 [Circle 40 (40,40) |> Outlined dso (RGB 0 0 0)]

treeImage (Node (im,reft,_)    [])   = (im, reft)
treeImage (Node (im,reft,refb) subs)
  = (foldr addForm treeImg lines, reft `at` nodeHeadPos)
  where
    (treeImg, [nodeHeadPos,subtreesPos])
      = below middleAlign linesHeight [im, subtrees]
    (subtrees, subtreeRelPositions)
      = besides topAlign 3 subtreeImgs
    (subtreeImgs, subtreeRelRefs) = unzip $ map treeImage subs

    nodeHeadRef = refb `at` nodeHeadPos
    subtreeAbsPositions = map (`at` subtreesPos) subtreeRelPositions
    subtreeRefs = zipWith at subtreeRelRefs subtreeAbsPositions
    
    (x,y) `at` (offx,offy) = (x + offx, y + offy)

    lines = map (line nodeHeadRef) subtreeRefs

    line start end = OpenPath [start,end] |> Outlined dso (RGB 0 0 0)

    linesHeight = 30

recto w h = Canvas w h [ClosedPath [(0,0), (0,h), (w,h), (w,0)]
                          |> Outlined dso (RGB 0 0 0)
                       ]

wrapRect obj
  = obj |> addForm (AsForm (uncurry recto (canvasSize obj)) (0,0))

text = addStdRefs . wrapRect . fst . below leftAlign 0 . map (Text (RGB 0 0 0) 20) . lines

addStdRefs img = (img, (w/2, 0), (w/2, h))
  where
    (w,h) = canvasSize img

leaf x = Node x []
cNode = Node nodeCircle
cLeaf = leaf nodeCircle

complexTree = cNode [cNode [cLeaf], cLeaf, cNode [cLeaf, cNode [cLeaf, cLeaf], cLeaf]]
exampleTree = Node (text "above") [leaf (text "mario"), leaf (text "bg")]
exTreeStd = Node nodeCircle [leaf nodeCircle, leaf nodeCircle]

frpsys :: FRP (Event GtkEvent) (Render ())
frpsys = -- constant $ cairoRenderTL (200,200) $
         --   cCircle 20 |> cFilled (1,0,0) |> cMove (100,100)
         drawing <~ pos ~~ text ~~ areaSize
  where
    keyEditInput = keyPress >>> keyEdit

    areaSize = resizeEvent >>> size

    textInput = keyEditInput

    textEntry = foldp ($) emptyText >>> pure showTextEdit

    mouseClick = sampleOn mousePress (mousePos <<< mouseMove)
    lastClicks = mouseClick >>> lastE 2 (0,0)
    lastClick = mouseClick >>> hold (0,0)

    lastPress
      = hold Nothing <<< eventMap Just <<< keyPress
    
    text = textEntry <<< textInput
    pos = mousePos <<< mouseMove

    drawing (x,y) text (w,h)
      = cairoDoubleBuffer $ do
          selectFontFace "Source Code Pro" FontSlantNormal FontWeightNormal
          setSourceRGB 1 1 1
          rectangle 0 0 (fromIntegral w) (fromIntegral h)
          fill
          translate x y
          render $ fst $ treeImage complexTree
          -- render $ below middleAlign $ map (Text black 10) $ lines text

      where
        black = RGB 0 0 0
