module GUI where

import Run (run)
import ImageQuery (
    ImageQueryStatement(GetImageQueryResult),
    ImageQuery(ImageOfAverage))
import ImageQuery.Parser (imageQueriesParser)
import ImageQuery.Printer (imageQueriesPrinter,imageQueryStatementPrinter)
import Text.Parsec.String (parseFromFile)

import Graphics.UI.WX (
    start,
    frame,Frame,button,Button,
    singleListBox,SingleListBox,
    fileSaveDialog,fileOpenDialog,errorDialog,
    Prop((:=)),set,text,items,sz,position,pt,selection,
    on,command,
    layout,widget,row,column,minsize)
import qualified  Graphics.UI.WX as Wx (get)

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
    RequestLoadProgram [ImageQueryStatement] |
    RequestRunProgram |
    RequestAddStatement Int ImageQueryStatement

data Response =
    ResponseSaveProgram FilePath [ImageQueryStatement] |
    ResponseProgramChanged [ImageQueryStatement] |
    ResponseRunProgram [ImageQueryStatement]

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
            yield (ResponseProgramChanged imagequerystatements)
        RequestRunProgram -> do
            Program imagequerystatements <- get
            yield (ResponseRunProgram imagequerystatements)
        RequestAddStatement index imagequerystatement -> do
            Program imagequerystatements <- get
            let (prefix,suffix) = splitAt index imagequerystatements
                imagequerystatements' = prefix ++ [imagequerystatement] ++ suffix
            put (Program imagequerystatements')
            yield (ResponseProgramChanged imagequerystatements')))

wx :: Managed (View Response,Controller Request)
wx = managed (\k -> do

    (saveProgramO,saveProgramI)       <- spawn Single
    (loadProgramO,loadProgramI)       <- spawn Single
    (runProgramO,runProgramI)         <- spawn Single
    (addStatementO,addStatementI)     <- spawn Single
    (programChangedO,programChangedI) <- spawn Single

    forkIO (start (do

        parentFrame         <- frame [text := "Image Processing",position := pt 100 100]
        saveProgramButton   <- createSaveProgramButton parentFrame saveProgramO
        loadProgramButton   <- createLoadProgramButton parentFrame loadProgramO
        runProgramButton    <- createRunProgramButton parentFrame runProgramO
        programListBox      <- createProgramListBox parentFrame programChangedI
        addStatementControl <- createAddStatementControl programListBox parentFrame addStatementO

        let frameLayout = row 5 [
                column 5 [
                    minsize (sz 500 500) (widget programListBox),
                    row 5 [
                        widget loadProgramButton,
                        widget saveProgramButton,
                        widget runProgramButton]],
                widget addStatementControl]

        set parentFrame [layout := frameLayout]))

    let inputs = [saveProgramI,loadProgramI,runProgramI,addStatementI]

        sink (ResponseSaveProgram filepath imagequerystatements) = do
            writeFile filepath (imageQueriesPrinter imagequerystatements)
        sink (ResponseProgramChanged imagequerystatements) = do
            atomically (send programChangedO imagequerystatements)
            return ()
        sink (ResponseRunProgram imagequerystatements) = do
            result <- run "data/Einbettung" imagequerystatements
            print (either id (const "Run finished!") result)

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

createRunProgramButton :: Frame () -> Output Request -> IO (Button ())
createRunProgramButton parentFrame runProgramO = button parentFrame attributes where
    attributes = [text := "Run", on command := sendRunProgramRequest]
    sendRunProgramRequest = do
        atomically (send runProgramO RequestRunProgram)
        return ()

createAddStatementControl :: SingleListBox () -> Frame () -> Output Request -> IO (Button ())
createAddStatementControl programListBox parentFrame addStatementO = button parentFrame attributes where
    attributes = [text := "AverageImage", on command := sendAverageImageRequest]
    sendAverageImageRequest = do
        index <- Wx.get programListBox selection
        atomically (send addStatementO (RequestAddStatement index (GetImageQueryResult ImageOfAverage)))
        return ()

