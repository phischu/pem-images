module GUI where

import ImageQuery (ImageQueryStatement)

import Graphics.UI.WX (
    start,close,
    frame,Frame,
    button,Button,
    fileSaveDialog,
    Prop((:=)),set,text,items,
    on,command,
    layout,widget,row)

import MVC (
    runMVC,Model,View,Controller,asPipe,
    Managed,managed,asSink,asInput,
    spawn,Buffer(Single),atomically,forkIO,
    Output,send,recv,)
import Pipes (await,yield)
import Control.Monad.State.Class (get)

import Control.Monad (forever,void)

data Program =
    Program [ImageQueryStatement] |
    Error String

data Request =
    RequestSaveProgram FilePath

data Response =
    ResponseSaveProgram FilePath [ImageQueryStatement]

gui :: IO ()
gui = runMVC (Program []) model wx >> return ()

model :: Model Program Request Response
model = asPipe (forever (do
    RequestSaveProgram filepath <- await
    Program imagequerystatements <- get
    yield (ResponseSaveProgram filepath imagequerystatements)))

wx :: Managed (View Response,Controller Request)
wx = managed (\k -> do
    (saveProgramO,saveProgramI) <- spawn Single
    (listOutput,listInput) <- spawn Single
    forkIO (start (do
        parentFrame <- frame [text := "Image Processing"]
        saveProgramButton <- createSaveProgramButton parentFrame saveProgramO
        set parentFrame [layout := row 5 [widget saveProgramButton]]))
    let updateList _ = return ()
    k (asSink updateList,asInput saveProgramI))

createSaveProgramButton :: Frame () -> Output Request -> IO (Button ())
createSaveProgramButton parentFrame saveProgramO = button parentFrame attributes where
    attributes = [text := "Save", on command := sendSaveProgramRequest]
    sendSaveProgramRequest = do
        maybeFilepath <- fileSaveDialog
            parentFrame True True "Save Image Queries"
            [("Image Query File",["*.imagequery"])] "" ""
        case maybeFilepath of
            Nothing -> return ()
            Just filepath -> do
                atomically (send saveProgramO (RequestSaveProgram filepath))
                return ()
