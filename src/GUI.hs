module GUI where

import Graphics.UI.WX (
    start,close,
    frame,button,singleListBox,
    Prop((:=)),set,text,items,
    on,command,
    layout,widget,row)

import MVC (
    runMVC,Model,View,Controller,asPipe,
    Managed,managed,asSink,asInput,
    spawn,Buffer(Single),atomically,send,recv,forkIO)
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
wx = managed (\k -> do
    (buttonOutput,buttonInput) <- spawn Single
    (listOutput,listInput) <- spawn Single
    forkIO (start (do
        parentFrame <- frame [text := "Image Processing"]
        but <- button parentFrame [text := "GO!", on command := atomically (send buttonOutput ()) >> return ()]
        lst <- singleListBox parentFrame [items := ["world"]]
        forkIO (forever (atomically (recv listInput) >>= maybe (return ()) (\xs -> set lst [items := xs])))
        set parentFrame [layout := row 5 [widget lst,widget but]]))
    let updateList xs = atomically (send listOutput xs) >> return ()
    k (asSink updateList,asInput buttonInput))

gui :: IO ()
gui = runMVC [] model wx >> return ()
