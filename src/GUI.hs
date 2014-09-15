module GUI where

import Run (run)
import ImageQuery (
    ImageQueryStatement(GetImageQueryResult,SetImageQueryParameter),
    ImageQuery(ImageOfAverage,IslandImage,LineImage,TableQuery,AreaHistogram),
    Polarity(Dark,Bright),
    Orientation(Horizontal,Vertical),
    Channel(Red,Green,Blue),
    ImageQueryParameter(Channel,SubRect,StencilImage,Threshold,Smoothing,Polarity),
    TableQuery(ValueInPoint,AverageAroundPoint,AverageOfImage,IslandQuery),
    IslandQuery(NumberOfIslands,AverageAreaOfIslands,AverageOutlineOfIslands),
    Power(One,OneOverTwo,ThreeOverTwo))
import ImageQuery.Parser (imageQueriesParser)
import ImageQuery.Printer (imageQueriesPrinter,imageQueryStatementPrinter)
import ImageLoading (loadImage)
import ImageProcessing (juicyToImage,binarize,chooseChannel,RGB(red))
import Text.Parsec.String (parseFromFile)

import Graphics.UI.WX (
    start,
    frame,Frame,button,Button,
    singleListBox,SingleListBox,
    fileSaveDialog,fileOpenDialog,errorDialog,dirOpenDialog,infoDialog,
    Prop((:=)),set,text,items,sz,position,pt,selection,text,
    on,command,
    Layout,layout,widget,row,column,minsize,boxed,
    panel,Panel,choice,entry,staticText,StaticText)
import qualified  Graphics.UI.WX as Wx (get,set)

import MVC (
    runMVC,Model,asPipe,
    managed,asSink,asInput,
    spawn,Buffer(Single),atomically,forkIO,
    Output,send,
    Input,recv)
import Pipes (await,yield)
import Control.Monad.State.Class (get,put,gets)

import Control.Exception (catch,SomeException)
import Control.Monad (forever,replicateM,forM,when)
import Data.Monoid (mconcat)

data Program = Program {
    imageQueryStatements :: [ImageQueryStatement],
    inputPath :: InputPath}

data Request =
    RequestSaveProgram FilePath |
    RequestLoadProgram [ImageQueryStatement] |
    RequestRunProgram |
    RequestAddStatement Int ImageQueryStatement |
    RequestInputPath InputPath |
    RequestDeleteStatement Int

data Response =
    ResponseSaveProgram FilePath [ImageQueryStatement] |
    ResponseProgramChanged [ImageQueryStatement] |
    ResponseRunProgram InputPath [ImageQueryStatement] |
    ResponseInputPath InputPath

type InputPath = FilePath

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
            yield (ResponseInputPath inputpath)
        RequestDeleteStatement index -> do
            imagequerystatements <- gets imageQueryStatements
            inputpath <- gets inputPath
            when (index < length imagequerystatements) (do
                let (prefix,suffix) = splitAt index imagequerystatements
                    imagequerystatements' = prefix ++ tail suffix
                put (Program imagequerystatements' inputpath)
                yield (ResponseProgramChanged imagequerystatements'))))

gui :: IO ()
gui = start (do

    (saveProgramO,saveProgramI)           <- spawn Single
    (loadProgramO,loadProgramI)           <- spawn Single
    (runProgramO,runProgramI)             <- spawn Single
    (addStatementO,addStatementI)         <- spawn Single
    (programChangedO,programChangedI)     <- spawn Single
    (changeInputPathO,changeInputPathI)   <- spawn Single
    (inputPathChangedO,inputPathChangedI) <- spawn Single
    (deleteStatementO,deleteStatementI)   <- spawn Single

    parentFrame           <- frame [text := "Image Processing",position := pt 100 100]
    saveProgramButton     <- createSaveProgramButton parentFrame saveProgramO
    loadProgramButton     <- createLoadProgramButton parentFrame loadProgramO
    runProgramButton      <- createRunProgramButton parentFrame runProgramO
    programListBox        <- createProgramListBox parentFrame programChangedI
    addStatementPanel     <- createAddStatementPanel programListBox parentFrame addStatementO
    inputPathButton       <- createInputPathButton parentFrame changeInputPathO
    inputPathText         <- createInputPathText parentFrame inputPathChangedI
    deleteStatementButton <- createDeleteStatementButton programListBox parentFrame deleteStatementO

    let wx = managed (\k -> let

            inputs = [saveProgramI,loadProgramI,runProgramI,addStatementI,changeInputPathI,deleteStatementI]

            sink (ResponseSaveProgram filepath imagequerystatements) = do
                writeFile filepath (imageQueriesPrinter imagequerystatements)
            sink (ResponseProgramChanged imagequerystatements) = do
                atomically (send programChangedO imagequerystatements)
                return ()
            sink (ResponseRunProgram inputpath imagequerystatements) = do
                catch (run inputpath imagequerystatements) (\e ->
                    errorDialog parentFrame "Error during run" (show (e :: SomeException)))
                infoDialog parentFrame "Run finished!" "Success!"
            sink (ResponseInputPath inputpath) = do
                atomically (send inputPathChangedO inputpath)
                return ()

            in k (asSink sink,asInput (mconcat inputs)))

        frameLayout = row 5 [
            column 5 [
                minsize (sz 500 500) (widget programListBox),
                row 5 [
                    widget deleteStatementButton,
                    widget loadProgramButton,
                    widget saveProgramButton,
                    widget runProgramButton]],
            column 5 [
                widget addStatementPanel,
                boxed "Inputs" (row 5 [
                    widget inputPathButton,
                    widget inputPathText])]]

    forkIO (runMVC (Program [] ".") model wx >> return ())

    set parentFrame [layout := frameLayout])

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
            [("Image Query File",["*.imagequery"]),("All Files",["*"])] "" ""
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

createDeleteStatementButton :: SingleListBox () -> Frame () -> Output Request -> IO (Button ())
createDeleteStatementButton programListBox parentFrame deleteStatementO = button parentFrame attributes where
    attributes = [text := "Delete statement", on command := sendDeleteStatementRequest]
    sendDeleteStatementRequest = do
        index <- Wx.get programListBox selection
        atomically (send deleteStatementO (RequestDeleteStatement index))
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

createInputPathText :: Frame () -> Input InputPath -> IO (StaticText ())
createInputPathText parentFrame inputPathChangedI = do
    inputPathText <- staticText parentFrame [text := "."]
    forkIO (forever (do
        maybeInputPath <- atomically (recv inputPathChangedI)
        case maybeInputPath of
            Nothing -> return ()
            Just inputpath -> Wx.set inputPathText [text := inputpath]))
    return inputPathText

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
    areaHistogramPanel      <- createStatementPanel areaHistogramControl
    channelPanel            <- createStatementPanel channelControl
    subrectPanel            <- createStatementPanel subrectControl
    stencilPanel            <- createStatementPanel stencilControl
    thresholdPanel          <- createStatementPanel thresholdControl
    smoothingPanel          <- createStatementPanel smoothingControl
    polarityPanel           <- createStatementPanel polarityControl
    valueInPointPanel       <- createStatementPanel valueInPointControl
    averageAroundPointPanel <- createStatementPanel averageAroundPointControl
    averageOfImagePanel     <- createStatementPanel averageOfImageControl
    islandQueryPanel        <- createStatementPanel islandQueryControl

    Wx.set addStatementPanel [layout := column 5 [
        boxed "Parameters" (column 5 [
            widget channelPanel,
            widget subrectPanel,
            widget stencilPanel,
            widget thresholdPanel,
            widget smoothingPanel,
            widget polarityPanel]),
        boxed "Output" (column 5 [
            widget averageImagePanel,
            widget islandImagePanel,
            widget lineImagePanel,
            widget areaHistogramPanel]),
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
islandImageControl = StatementControl "Island Image" (\_ -> do
    let getStatement = return (GetImageQueryResult IslandImage)
    return ([],getStatement))

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

areaHistogramControl :: StatementControl
areaHistogramControl = StatementControl "Area Histogram" (\parentPanel -> do
    binsizeEntry <- entry parentPanel [text := "1"]
    powerChoice <- choice parentPanel [items := ["One","One over two","Three over two"],selection := 0]
    let getStatement = do
            binsizeText <- Wx.get binsizeEntry text
            powerSelection <- Wx.get powerChoice selection
            let binsize = read binsizeText
                power = case powerSelection of
                    0 -> One
                    1 -> OneOverTwo
                    2 -> ThreeOverTwo
            return (GetImageQueryResult (AreaHistogram binsize power))
        layouts = [widget binsizeEntry,widget powerChoice]
    return (layouts,getStatement))

channelControl :: StatementControl
channelControl = StatementControl "Channel" (\parentPanel -> do
    channelChoice <- choice parentPanel [items := ["Red","Green","Blue"],selection := 0]
    let getStatement = do
            channelSelection <- Wx.get channelChoice selection
            let channelType = case channelSelection of
                    0 -> Red
                    1 -> Green
                    2 -> Blue
            return (SetImageQueryParameter (Channel channelType))
    return ([widget channelChoice],getStatement))

subrectControl :: StatementControl
subrectControl = StatementControl "Subrect" (\parentPanel -> do
    parameterEntries <- replicateM 4 (entry parentPanel [text := "0"])
    let getStatement = do
            [x,y,w,h] <- forM parameterEntries (\parameterEntry -> Wx.get parameterEntry text >>= return . read)
            return (SetImageQueryParameter (SubRect (x,y,w,h)))
    return (map widget parameterEntries,getStatement))

stencilControl :: StatementControl
stencilControl = StatementControl "Stencil" (\parentPanel -> do
    let getStatement = do
            maybeFilepath <- fileOpenDialog
                parentPanel True True "Stencil Image"
                [("Image Files",["*.png","*.bmp","*.gif"])] "" ""
            case maybeFilepath of
                Nothing -> return (SetImageQueryParameter (StencilImage "" Nothing))
                Just filepath -> catch (do
                    image <- loadImage filepath
                    return (SetImageQueryParameter (
                                StencilImage filepath (Just (
                                    binarize 0 (chooseChannel red (juicyToImage image)))))))
                    (\e -> do
                        errorDialog parentPanel "Loading Stencil Failed!" (show (e :: SomeException))
                        return (SetImageQueryParameter (StencilImage "" Nothing)))
    return ([],getStatement))

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

polarityControl :: StatementControl
polarityControl = StatementControl "Polarity" (\parentPanel -> do
    polarityChoice <- choice parentPanel [items := ["Dark","Bright"],selection := 0]
    let getStatement = do
            polaritySelection <- Wx.get polarityChoice selection
            let polarity = case polaritySelection of
                    0 -> Dark
                    1 -> Bright
            return (SetImageQueryParameter (Polarity polarity))
    return ([widget polarityChoice],getStatement))

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
    islandQueryChoice <- choice parentPanel [items := ["Number","Average Area","Average Outline"],selection := 0]
    let getStatement = do
            islandQuerySelection <- Wx.get islandQueryChoice selection
            let islandQuery = case islandQuerySelection of
                    0 -> NumberOfIslands
                    1 -> AverageAreaOfIslands
                    2 -> AverageOutlineOfIslands
            return (GetImageQueryResult (TableQuery (IslandQuery islandQuery)))
    return ([widget islandQueryChoice],getStatement))

