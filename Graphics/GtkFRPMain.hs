module Graphics.GtkFRPMain where

import Graphics.UI.Gtk
import Graphics.GtkFRP


runGtkFRP frpWidget frpsys = do
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
