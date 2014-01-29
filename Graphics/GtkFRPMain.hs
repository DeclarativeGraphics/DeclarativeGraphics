module Main where

import Diagrams.Prelude hiding ((~~))

import Graphics.FRP
import Graphics.FRPUtils
import Graphics.Prototype.TextEdit

import Graphics.GtkFRP

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


--frpsys :: FRP (Event GtkEvent) (IO ())
frpsys = constant $ circle 1 # fc yellow -- drawing <~ pos ~~ text ~~ areaSize
  where
    keyEditInput = keyPress >>> keyEdit

    areaSize = resizeEvent >>> size

    textInput = keyEditInput

    textEntry = foldp ($) emptyText >>> mapB showTextEdit

    mouseClick = sampleOn mousePress (mousePos <<< mouseMove)
    lastClicks = mouseClick >>> lastE 2 (0,0)
    lastClick = mouseClick >>> hold (0,0)

    lastPress
      = hold Nothing <<< eventMap Just <<< keyPress
    
    text = textEntry <<< textInput
    pos = mousePos <<< mouseMove

    drawing (x,y) text (w,h) =
      circle 1 # fc yellow
