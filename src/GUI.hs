module GUI where

import Graphics.UI.WX (
    start,frameLoadRes)
import Graphics.UI.WXCore (
    windowShow)

gui :: IO ()
gui = start (do
    f <- frameLoadRes "GUI.xrc" "MainFrame" []
    windowShow f
    return ())
