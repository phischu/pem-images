module GUI where

import Graphics.UI.WX (
    start,close,
    frame,button,
    Prop((:=)),set,text,outerSize,sz,
    on,command,
    layout,widget)

import MVC (runMVC,Model,View,Controller,Managed,asPipe)
import Pipes (await,yield)
import Control.Monad.Trans.Class (lift)
import Control.Monad.State.Strict (get,modify)

import Control.Monad (forever)

model :: Model [String] () [String]
model = asPipe (forever (do
    () <- await
    lift (modify (\xs -> "hallo" : xs))
    lift get >>= yield))

wx :: Managed (View [String],Controller ())
wx = undefined {-start (do
    parentFrame <- frame [text := "Image Processing"]
    quit <- button parentFrame [text := "Quit", on command := close parentFrame,outerSize := sz 500 500]
    set parentFrame [layout := widget quit])-}

gui :: IO ()
gui = runMVC [] model wx >> return ()
