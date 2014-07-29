module GUI where

import ImageQuery (ImageQueryStatement)
import ImageQuery.Parser (imageQueriesParser)
import ImageQuery.Printer (imageQueriesPrinter,imageQueryStatementPrinter)
import Text.Parsec.String (parseFromFile)

import Graphics.UI.WX (
    start,close,
    frame,Frame,button,Button,
    singleListBox,SingleListBox,
    fileSaveDialog,fileOpenDialog,errorDialog,
    Prop((:=)),set,text,items,
    on,command,
    layout,widget,row,column)

import MVC (
    runMVC,Model,View,Controller,asPipe,
    Managed,managed,asSink,asInput,
    spawn,Buffer(Single),atomically,forkIO,
    Output,send,
    Input,recv)
import Pipes (await,yield)
import Control.Monad.State.Class (get,put)

import Control.Monad (forever)
import Data.Monoid (mconcat)

data Program =
    Program [ImageQueryStatement]

data Request =
    RequestSaveProgram FilePath |
    RequestLoadProgram [ImageQueryStatement]

data Response =
    ResponseSaveProgram FilePath [ImageQueryStatement] |
    ResponseProgramChanged [ImageQueryStatement]

gui :: IO ()
gui = runMVC (Program []) model wx >> return ()

model :: Model Program Request Response
model = asPipe (forever (do
    request <- await
    case request of
        RequestSaveProgram filepath -> do
            Program imagequerystatements <- get
            yield (ResponseSaveProgram filepath imagequerystatements)
        RequestLoadProgram imagequerystatements -> do
            put (Program imagequerystatements)
            yield (ResponseProgramChanged imagequerystatements)))

wx :: Managed (View Response,Controller Request)
wx = managed (\k -> do

    (saveProgramO,saveProgramI) <- spawn Single
    (loadProgramO,loadProgramI) <- spawn Single
    (programChangedO,programChangedI) <- spawn Single

    forkIO (start (do
        parentFrame <- frame [text := "Image Processing"]
        saveProgramButton <- createSaveProgramButton parentFrame saveProgramO
        loadProgramButton <- createLoadProgramButton parentFrame loadProgramO
        programListBox <- createProgramListBox parentFrame programChangedI
        let frameLayout = column 5 [
                widget programListBox,
                row 5 [
                    widget loadProgramButton,
                    widget saveProgramButton]]
        set parentFrame [layout := frameLayout]))

    let inputs = [saveProgramI,loadProgramI]
        sink (ResponseSaveProgram filepath imagequerystatements) = do
            writeFile filepath (imageQueriesPrinter imagequerystatements)
        sink (ResponseProgramChanged imagequerystatements) = do
            atomically (send programChangedO imagequerystatements)
            return ()

    k (asSink sink,asInput (mconcat inputs)))

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

createLoadProgramButton :: Frame () -> Output Request -> IO (Button ())
createLoadProgramButton parentFrame loadProgramO = button parentFrame attributes where
    attributes = [text := "Load", on command := sendLoadProgramRequest]
    sendLoadProgramRequest = do
        maybeFilepath <- fileOpenDialog
            parentFrame True True "Load Image Queries"
            [("Image Query File",["*.imagequery"])] "" ""
        case maybeFilepath of
            Nothing -> return ()
            Just filepath -> do
                parseResult <- parseFromFile imageQueriesParser filepath
                case parseResult of
                    Left message -> errorDialog parentFrame "Parse Error" (show message)
                    Right imagequerystatements -> do
                        atomically (send loadProgramO (RequestLoadProgram imagequerystatements))
                        return ()

createProgramListBox :: Frame () -> Input [ImageQueryStatement] -> IO (SingleListBox ())
createProgramListBox parentFrame programChangedI = do
    programListBox <- singleListBox parentFrame []
    forkIO (forever (do
        maybeImageQueryStatements <- atomically (recv programChangedI)
        case maybeImageQueryStatements of
            Nothing -> return ()
            Just imagequerystatements -> do
                set programListBox [items := map imageQueryStatementPrinter imagequerystatements]))
    return programListBox
