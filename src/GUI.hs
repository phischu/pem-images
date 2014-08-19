module GUI where

import Run (run)
import ImageQuery (
    ImageQueryStatement(GetImageQueryResult,SetImageQueryParameter),
    ImageQuery(ImageOfAverage,IslandImage,LineImage,TableQuery),
    Polarity(Dark,Bright),
    Orientation(Horizontal,Vertical),
    ImageQueryParameter(Channel,SubRect,Threshold,Smoothing),
    TableQuery(ValueInPoint,AverageAroundPoint,AverageOfImage,IslandQuery),
    IslandQuery(NumberOfIslands,AverageAreaOfIslands,AverageOutlineOfIslands))
import ImageQuery.Parser (imageQueriesParser)
import ImageQuery.Printer (imageQueriesPrinter,imageQueryStatementPrinter)
import Text.Parsec.String (parseFromFile)

import Graphics.UI.WX (
    start,
    frame,Frame,button,Button,
    singleListBox,SingleListBox,
    fileSaveDialog,fileOpenDialog,errorDialog,dirOpenDialog,
    Prop((:=)),set,text,items,sz,position,pt,selection,text,
    on,command,
    Layout,layout,widget,row,column,minsize,boxed,
    panel,Panel,choice,entry)
import qualified  Graphics.UI.WX as Wx (get,set)

import MVC (
    runMVC,Model,View,Controller,asPipe,
    Managed,managed,asSink,asInput,
    spawn,Buffer(Single),atomically,forkIO,
    Output,send,
    Input,recv)
import Pipes (await,yield)
import Control.Monad.State.Class (get,put,gets)

import Control.Monad (forever,replicateM,forM)
import Data.Monoid (mconcat)

data Program = Program {
    imageQueryStatements :: [ImageQueryStatement],
    inputPath :: InputPath}

data Request =
    RequestSaveProgram FilePath |
    RequestLoadProgram [ImageQueryStatement] |
    RequestRunProgram |
    RequestAddStatement Int ImageQueryStatement |
    RequestInputPath InputPath

data Response =
    ResponseSaveProgram FilePath [ImageQueryStatement] |
    ResponseProgramChanged [ImageQueryStatement] |
    ResponseRunProgram InputPath [ImageQueryStatement] |
    ResponseInputPath InputPath

type InputPath = FilePath

gui :: IO ()
gui = runMVC (Program [] ".") model wx >> return ()

model :: Model Program Request Response
model = asPipe (forever (do
    request <- await
    case request of
        RequestSaveProgram filepath -> do
            imagequerystatements <- gets imageQueryStatements
            yield (ResponseSaveProgram filepath imagequerystatements)
        RequestLoadProgram imagequerystatements -> do
            inputpath <- gets inputPath
            put (Program imagequerystatements inputpath)
            yield (ResponseProgramChanged imagequerystatements)
        RequestRunProgram -> do
            Program imagequerystatements inputpath <- get
            yield (ResponseRunProgram inputpath imagequerystatements)
        RequestAddStatement index imagequerystatement -> do
            imagequerystatements <- gets imageQueryStatements
            inputpath <- gets inputPath
            let (prefix,suffix) = splitAt index imagequerystatements
                imagequerystatements' = prefix ++ [imagequerystatement] ++ suffix
            put (Program imagequerystatements' inputpath)
            yield (ResponseProgramChanged imagequerystatements')
        RequestInputPath inputpath -> do
            program <- get
            put (program {inputPath = inputpath})
            yield (ResponseInputPath inputpath)))

wx :: Managed (View Response,Controller Request)
wx = managed (\k -> do

    (saveProgramO,saveProgramI)       <- spawn Single
    (loadProgramO,loadProgramI)       <- spawn Single
    (runProgramO,runProgramI)         <- spawn Single
    (addStatementO,addStatementI)     <- spawn Single
    (programChangedO,programChangedI) <- spawn Single
    (inputPathO,inputPathI)           <- spawn Single

    forkIO (start (do

        parentFrame       <- frame [text := "Image Processing",position := pt 100 100]
        saveProgramButton <- createSaveProgramButton parentFrame saveProgramO
        loadProgramButton <- createLoadProgramButton parentFrame loadProgramO
        runProgramButton  <- createRunProgramButton parentFrame runProgramO
        programListBox    <- createProgramListBox parentFrame programChangedI
        addStatementPanel <- createAddStatementPanel programListBox parentFrame addStatementO
        inputPathButton   <- createInputPathButton parentFrame inputPathO

        let frameLayout = row 5 [
                column 5 [
                    minsize (sz 500 500) (widget programListBox),
                    row 5 [
                        widget loadProgramButton,
                        widget saveProgramButton,
                        widget runProgramButton]],
                column 5 [
                    widget addStatementPanel,
                    widget inputPathButton]]

        set parentFrame [layout := frameLayout]))

    let inputs = [saveProgramI,loadProgramI,runProgramI,addStatementI,inputPathI]

        sink (ResponseSaveProgram filepath imagequerystatements) = do
            writeFile filepath (imageQueriesPrinter imagequerystatements)
        sink (ResponseProgramChanged imagequerystatements) = do
            atomically (send programChangedO imagequerystatements)
            return ()
        sink (ResponseRunProgram inputpath imagequerystatements) = do
            result <- run inputpath imagequerystatements
            putStrLn (either id (const "Run finished!") result)
        sink (ResponseInputPath inputpath) = do
            putStrLn ("input path chosen: " ++ inputpath)

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

createInputPathButton :: Frame () -> Output Request -> IO (Button ())
createInputPathButton parentFrame inputPathO =
    button parentFrame [
        text := "Choose input path",
        on command := do
            maybeFilepath <- dirOpenDialog parentFrame False "Choose input path" ""
            case maybeFilepath of
                Nothing -> return ()
                Just inputpath -> do
                    atomically (send inputPathO (RequestInputPath inputpath))
                    return ()]

createAddStatementPanel :: SingleListBox () -> Frame () -> Output Request -> IO (Panel ())
createAddStatementPanel programListBox parentFrame addStatementO = do

    addStatementPanel  <- panel parentFrame []

    let createStatementPanel (StatementControl buttonText createOptions) = do
            statementPanel <- panel addStatementPanel []
            (optionLayouts,getStatement) <- createOptions statementPanel
            let sendStatement = do
                    index <- Wx.get programListBox selection
                    statement <- getStatement
                    atomically (send addStatementO (RequestAddStatement index statement))
                    return ()
            statementButton <- button statementPanel [text := buttonText, on command := sendStatement]
            Wx.set statementPanel [layout := row 5 (widget statementButton:optionLayouts)]
            return statementPanel

    averageImagePanel       <- createStatementPanel averageImageControl
    islandImagePanel        <- createStatementPanel islandImageControl
    lineImagePanel          <- createStatementPanel lineImageControl
    channelPanel            <- createStatementPanel channelControl
    subrectPanel            <- createStatementPanel subrectControl
    thresholdPanel          <- createStatementPanel thresholdControl
    smoothingPanel          <- createStatementPanel smoothingControl
    valueInPointPanel       <- createStatementPanel valueInPointControl
    averageAroundPointPanel <- createStatementPanel averageAroundPointControl
    averageOfImagePanel     <- createStatementPanel averageOfImageControl
    islandQueryPanel        <- createStatementPanel islandQueryControl

    Wx.set addStatementPanel [layout := column 5 [
        boxed "Parameters" (column 5 [
            widget channelPanel,
            widget subrectPanel,
            widget thresholdPanel,
            widget smoothingPanel]),
        boxed "Output Image" (column 5 [
            widget averageImagePanel,
            widget islandImagePanel,
            widget lineImagePanel]),
        boxed "Table Entry" (column 5 [
            widget valueInPointPanel,
            widget averageAroundPointPanel,
            widget averageOfImagePanel,
            widget islandQueryPanel])]]

    return addStatementPanel

data StatementControl = StatementControl String (Panel () -> IO ([Layout],IO ImageQueryStatement))

averageImageControl :: StatementControl
averageImageControl = StatementControl "Average Image" (\_ -> do
    let getStatement = return (GetImageQueryResult ImageOfAverage)
    return ([],getStatement))

islandImageControl :: StatementControl
islandImageControl = StatementControl "Island Image" (\parentPanel -> do
    polarityChoice <- choice parentPanel [items := ["Dark","Bright"],selection := 0]
    let getStatement = do
            polaritySelection <- Wx.get polarityChoice selection
            let polarity = if polaritySelection == 0 then Dark else Bright
            return (GetImageQueryResult (IslandImage polarity))
    return ([widget polarityChoice],getStatement))

lineImageControl :: StatementControl
lineImageControl = StatementControl "Line Image" (\parentPanel -> do
    orientationChoice <- choice parentPanel [items := ["Horizontal","Vertical"],selection := 0]
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

channelControl :: StatementControl
channelControl = StatementControl "Channel" (\parentPanel -> do
    channelChoice <- choice parentPanel [items := ["Red","Green","Blue"],selection := 0]
    let getStatement = do
            channelSelection <- Wx.get channelChoice selection
            return (SetImageQueryParameter (Channel channelSelection))
    return ([widget channelChoice],getStatement))

subrectControl :: StatementControl
subrectControl = StatementControl "Subrect" (\parentPanel -> do
    parameterEntries <- replicateM 4 (entry parentPanel [text := "0"])
    let getStatement = do
            [x,y,w,h] <- forM parameterEntries (\parameterEntry -> Wx.get parameterEntry text >>= return . read)
            return (SetImageQueryParameter (SubRect (x,y,w,h)))
    return (map widget parameterEntries,getStatement))

thresholdControl :: StatementControl
thresholdControl = StatementControl "Threshold" (\parentPanel -> do
    thresholdEntry <- entry parentPanel [text := "0"]
    let getStatement = do
            thresholdText <- Wx.get thresholdEntry text
            return (SetImageQueryParameter (Threshold (read thresholdText)))
    return ([widget thresholdEntry],getStatement))

smoothingControl :: StatementControl
smoothingControl = StatementControl "Smoothing" (\parentPanel -> do
    smoothingEntry <- entry parentPanel [text := "0"]
    let getStatement = do
            smoothingText <- Wx.get smoothingEntry text
            return (SetImageQueryParameter (Smoothing (read smoothingText)))
    return ([widget smoothingEntry],getStatement))

valueInPointControl :: StatementControl
valueInPointControl = StatementControl "Value in Point" (\parentPanel -> do
    [xEntry,yEntry] <- replicateM 2 (entry parentPanel [text := "0"])
    let getStatement = do
            xText <- Wx.get xEntry text
            yText <- Wx.get yEntry text
            return (GetImageQueryResult (TableQuery (ValueInPoint (read xText) (read yText))))
    return (map widget [xEntry,yEntry],getStatement))

averageAroundPointControl :: StatementControl
averageAroundPointControl = StatementControl "Average Around Point" (\parentPanel -> do
    [xEntry,yEntry,rEntry] <- replicateM 3 (entry parentPanel [text := "0"])
    let getStatement = do
            xText <- Wx.get xEntry text
            yText <- Wx.get yEntry text
            rText <- Wx.get rEntry text
            return (GetImageQueryResult (TableQuery (AverageAroundPoint (read xText) (read yText) (read rText))))
    return (map widget [xEntry,yEntry,rEntry],getStatement))

averageOfImageControl :: StatementControl
averageOfImageControl = StatementControl "Average Of Image" (\_ -> do
    let getStatement = return (GetImageQueryResult (TableQuery AverageOfImage))
    return ([],getStatement))

islandQueryControl :: StatementControl
islandQueryControl = StatementControl "Island Query" (\parentPanel -> do
    polarityChoice <- choice parentPanel [items := ["Dark","Bright"],selection := 0]
    islandQueryChoice <- choice parentPanel [items := ["Number","Average Area","Average Outline"],selection := 0]
    let getStatement = do
            polaritySelection <- Wx.get polarityChoice selection
            islandQuerySelection <- Wx.get islandQueryChoice selection
            let polarity = if polaritySelection == 0 then Dark else Bright
                islandQuery = case islandQuerySelection of
                    0 -> NumberOfIslands
                    1 -> AverageAreaOfIslands
                    2 -> AverageOutlineOfIslands
            return (GetImageQueryResult (TableQuery (IslandQuery polarity islandQuery)))
    return ([widget polarityChoice,widget islandQueryChoice],getStatement))

