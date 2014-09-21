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
    Power(One,OneOverTwo,ThreeOverTwo),
    forStencil)
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
    Prop((:=)),set,text,items,itemCount,sz,position,pt,selection,text,
    on,command,
    Layout,layout,widget,row,column,minsize,boxed,fill,
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
import Control.Monad (forever,when)
import Control.Applicative (Applicative(pure,(<*>)),(<$>))
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
                    fill (widget inputPathText)])]]

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
                        imagequerystatements' <- forStencil imagequerystatements ((\(StencilImage stencilpath _ ) -> do
                            image <- loadImage stencilpath
                            return (StencilImage stencilpath (Just (binarize 0 (chooseChannel red (juicyToImage image)))))))
                        atomically (send loadProgramO (RequestLoadProgram imagequerystatements'))
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
                itemcount <- Wx.get programListBox itemCount
                set programListBox [
                    items := map imageQueryStatementPrinter imagequerystatements ++ ["NEW STATEMENT"],
                    selection := if length imagequerystatements >= itemcount
                        then index + 1
                        else index]))
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

    let createStatementPanel buttonText statementControl = do
            statementPanel <- panel addStatementPanel []
            (optionLayouts,getStatement) <- unStatementControl statementControl statementPanel
            let sendStatement = do
                    index <- Wx.get programListBox selection
                    statement <- getStatement
                    atomically (send addStatementO (RequestAddStatement index statement))
                    return ()
            statementButton <- button statementPanel [text := buttonText, on command := sendStatement]
            Wx.set statementPanel [layout := row 5 (widget statementButton:optionLayouts)]
            return statementPanel

    averageImagePanel       <- createStatementPanel "Average image" averageImageControl
    islandImagePanel        <- createStatementPanel "Island images" islandImageControl
    lineImagePanel          <- createStatementPanel "Line image" lineImageControl
    areaHistogramPanel      <- createStatementPanel "Area histograms" areaHistogramControl
    channelPanel            <- createStatementPanel "Channel" channelControl
    subrectPanel            <- createStatementPanel "Subrect" subrectControl
    stencilPanel            <- createStatementPanel "Stencil" stencilControl
    thresholdPanel          <- createStatementPanel "Threshold" thresholdControl
    smoothingPanel          <- createStatementPanel "Smoothing" smoothingControl
    polarityPanel           <- createStatementPanel "Polarity" polarityControl
    valueInPointPanel       <- createStatementPanel "Value in point" valueInPointControl
    averageAroundPointPanel <- createStatementPanel "Average around point" averageAroundPointControl
    averageOfImagePanel     <- createStatementPanel "Average of image" averageOfImageControl
    islandQueryPanel        <- createStatementPanel "Island query" islandQueryControl

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

data StatementControl a = StatementControl {unStatementControl :: Panel () -> IO ([Layout],IO a)}

instance Functor StatementControl where
    fmap function = StatementControl . fmap (fmap (fmap (fmap function))) . unStatementControl

instance Applicative StatementControl where
    pure value = StatementControl (const (return ([],return value)))
    (<*>) functionStatementConstrol valueStatementConstrol = StatementControl (\parentPanel -> do
        (functionLayouts,getFunction) <- unStatementControl functionStatementConstrol parentPanel
        (valueLayouts,getValue) <- unStatementControl valueStatementConstrol parentPanel
        return (functionLayouts ++ valueLayouts,do
            function <- getFunction
            value <- getValue
            return (function value)))

choiceControl :: [(String,a)] -> StatementControl a
choiceControl choices = StatementControl (\parentPanel -> do
    c <- choice parentPanel [items := map fst choices,selection := 0]
    let getChoice = do
            s <- Wx.get c selection
            return (map snd choices !! s)
    return ([widget c],getChoice))

numberControl :: (Num a) => StatementControl a
numberControl = undefined

averageImageControl :: StatementControl ImageQueryStatement
averageImageControl = pure (GetImageQueryResult ImageOfAverage)

islandImageControl :: StatementControl ImageQueryStatement
islandImageControl = pure (GetImageQueryResult IslandImage)

lineImageControl :: StatementControl ImageQueryStatement
lineImageControl =
    (\orientation x y l -> GetImageQueryResult (LineImage orientation x y l)) <$>
    choiceControl [("Horizontal",Horizontal),("Vertical",Vertical)] <*>
    numberControl <*>
    numberControl <*>
    numberControl

areaHistogramControl :: StatementControl ImageQueryStatement
areaHistogramControl = 
    (\binsize power -> GetImageQueryResult (AreaHistogram binsize power)) <$>
    numberControl <*>
    choiceControl [("One",One),("One over two",OneOverTwo),("Three over two",ThreeOverTwo)]

channelControl :: StatementControl ImageQueryStatement
channelControl =
    SetImageQueryParameter . Channel <$>
    choiceControl [("Red",Red),("Green",Green),("Blue",Blue)]

subrectControl :: StatementControl ImageQueryStatement
subrectControl =
    (\x y w h -> SetImageQueryParameter (SubRect (x,y,w,h))) <$>
    numberControl <*>
    numberControl <*>
    numberControl <*>
    numberControl

stencilControl :: StatementControl ImageQueryStatement
stencilControl = StatementControl (\parentPanel -> do
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

thresholdControl :: StatementControl ImageQueryStatement
thresholdControl =
    SetImageQueryParameter . Threshold <$>
    numberControl

smoothingControl :: StatementControl ImageQueryStatement
smoothingControl =
    SetImageQueryParameter . Smoothing <$>
    numberControl

polarityControl :: StatementControl ImageQueryStatement
polarityControl =
    SetImageQueryParameter . Polarity <$>
    choiceControl [("Dark",Dark),("Bright",Bright)]

valueInPointControl :: StatementControl ImageQueryStatement
valueInPointControl =
    (\x y -> GetImageQueryResult (TableQuery (ValueInPoint x y))) <$>
    numberControl <*>
    numberControl

averageAroundPointControl :: StatementControl ImageQueryStatement
averageAroundPointControl =
    (\x y r -> GetImageQueryResult (TableQuery (AverageAroundPoint x y r))) <$>
    numberControl <*>
    numberControl <*>
    numberControl

averageOfImageControl :: StatementControl ImageQueryStatement
averageOfImageControl = pure (GetImageQueryResult (TableQuery AverageOfImage))

islandQueryControl :: StatementControl ImageQueryStatement
islandQueryControl =
    GetImageQueryResult . TableQuery . IslandQuery <$>
    choiceControl [
        ("Number",NumberOfIslands),
        ("Average area",AverageAreaOfIslands),
        ("Average outline",AverageOutlineOfIslands)]

