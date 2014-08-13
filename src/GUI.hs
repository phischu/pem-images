module GUI where

import Run (run)
import ImageQuery (
    ImageQueryStatement(GetImageQueryResult,SetImageQueryParameter),
    ImageQuery(ImageOfAverage,IslandImage,LineImage),
    Polarity(Dark,Bright),
    Orientation(Horizontal,Vertical),
    ImageQueryParameter(Channel))
import ImageQuery.Parser (imageQueriesParser)
import ImageQuery.Printer (imageQueriesPrinter,imageQueryStatementPrinter)
import Text.Parsec.String (parseFromFile)

import Graphics.UI.WX (
    start,
    frame,Frame,button,Button,
    singleListBox,SingleListBox,
    fileSaveDialog,fileOpenDialog,errorDialog,
    Prop((:=)),set,text,items,sz,position,pt,selection,text,
    on,command,
    Layout,layout,widget,row,column,minsize,
    panel,Panel,choice,entry)
import qualified  Graphics.UI.WX as Wx (get,set)

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

        parentFrame       <- frame [text := "Image Processing",position := pt 100 100]
        saveProgramButton <- createSaveProgramButton parentFrame saveProgramO
        loadProgramButton <- createLoadProgramButton parentFrame loadProgramO
        runProgramButton  <- createRunProgramButton parentFrame runProgramO
        programListBox    <- createProgramListBox parentFrame programChangedI
        addStatementPanel <- createAddStatementPanel programListBox parentFrame addStatementO

        let frameLayout = row 5 [
                column 5 [
                    minsize (sz 500 500) (widget programListBox),
                    row 5 [
                        widget loadProgramButton,
                        widget saveProgramButton,
                        widget runProgramButton]],
                widget addStatementPanel]

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
    programListBox <- singleListBox parentFrame [
        items := ["NEW STATEMENT"],
        selection := 0]
    forkIO (forever (do
        maybeImageQueryStatements <- atomically (recv programChangedI)
        case maybeImageQueryStatements of
            Nothing -> return ()
            Just imagequerystatements -> do
                index <- Wx.get programListBox selection
                set programListBox [
                    items := map imageQueryStatementPrinter imagequerystatements ++ ["NEW STATEMENT"],
                    selection := index + 1]))
    return programListBox

createRunProgramButton :: Frame () -> Output Request -> IO (Button ())
createRunProgramButton parentFrame runProgramO = button parentFrame attributes where
    attributes = [text := "Run", on command := sendRunProgramRequest]
    sendRunProgramRequest = do
        atomically (send runProgramO RequestRunProgram)
        return ()

createAddStatementPanel :: SingleListBox () -> Frame () -> Output Request -> IO (Panel ())
createAddStatementPanel programListBox parentFrame addStatementO = do
    addStatementPanel  <- panel parentFrame []
    averageImagePanel  <- createAverageImagePanel addStatementPanel programListBox addStatementO
    islandImagePanel   <- createIslandImagePanel addStatementPanel programListBox addStatementO
    lineImagePanel     <- createLineImagePanel addStatementPanel programListBox addStatementO
    channelPanel       <- createChannelPanel addStatementPanel programListBox addStatementO
    Wx.set addStatementPanel [layout := column 5 [
        widget averageImagePanel,
        widget islandImagePanel,
        widget lineImagePanel,
        widget channelPanel]]
    return addStatementPanel

createStatementPanel ::
    Panel () -> String -> IO ([Layout],IO ImageQueryStatement) ->
    SingleListBox () -> Output Request -> IO (Panel ())
createStatementPanel parentPanel buttonText createOptions programListBox addStatementO = do
    statementPanel <- panel parentPanel []
    (optionLayouts,getStatement) <- createOptions
    let sendStatement = do
            index <- Wx.get programListBox selection
            statement <- getStatement
            atomically (send addStatementO (RequestAddStatement index statement))
            return ()
    statementButton <- button statementPanel [text := buttonText, on command := sendStatement]
    Wx.set statementPanel [layout := row 5 (widget statementButton:optionLayouts)]
    return statementPanel

createAverageImagePanel :: Panel () -> SingleListBox () -> Output Request -> IO (Panel ())
createAverageImagePanel parentPanel = createStatementPanel parentPanel "Output Average Image" (do
    let getStatement = return (GetImageQueryResult ImageOfAverage)
    return ([],getStatement))

createIslandImagePanel :: Panel () -> SingleListBox () -> Output Request -> IO (Panel ())
createIslandImagePanel parentPanel = createStatementPanel parentPanel "Output Island Image" (do
    polarityChoice <- choice parentPanel [items := ["Dark","Bright"],selection := 0]
    let getStatement = do
            polaritySelection <- Wx.get polarityChoice selection
            let polarity = if polaritySelection == 0 then Dark else Bright
            return (GetImageQueryResult (IslandImage polarity))
    return ([widget polarityChoice],getStatement))

createLineImagePanel :: Panel () -> SingleListBox () -> Output Request -> IO (Panel ())
createLineImagePanel parentPanel = createStatementPanel parentPanel "Output Line Image" (do
    orientationChoice <- choice parentPanel [items := ["horizontal","vertical"],selection := 0]
    xEntry <- entry parentPanel [text := "0"]
    yEntry <- entry parentPanel [text := "0"]
    lEntry <- entry parentPanel [text := "0"]
    let getStatement = do
            orientationSelection <- Wx.get orientationChoice selection
            xText <- Wx.get xEntry text
            yText <- Wx.get yEntry text
            lText <- Wx.get lEntry text
            let orientation = if orientationSelection == 0 then Horizontal else Vertical
                x = read xText
                y = read yText
                l = read lText
            return (GetImageQueryResult (LineImage orientation x y l))
        layouts = [widget orientationChoice,widget xEntry,widget yEntry,widget lEntry]
    return (layouts,getStatement))

createChannelPanel :: Panel () -> SingleListBox () -> Output Request -> IO (Panel ())
createChannelPanel parentPanel = createStatementPanel parentPanel "Channel" (do
    channelChoice <- choice parentPanel [items := ["red","green","blue"],selection := 0]
    let getStatement = do
            channelSelection <- Wx.get channelChoice selection
            return (SetImageQueryParameter (Channel channelSelection))
    return ([widget channelChoice],getStatement))
