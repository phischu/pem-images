module GUI where

import Run (run,numberOfImages)
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
    Layout,layout,widget,row,column,minsize,boxed,fill,label,
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
import Control.Applicative (Applicative(pure,(<*>)),(<$>),(*>))
import Data.Monoid (mconcat)

-- | The state of the application is a program to be run.
data Program = Program {
    imageQueryStatements :: [ImageQueryStatement],
    -- ^ The list of statements to be run.
    inputPath :: InputPath}
    -- ^ The path where the input images are from.

-- | User requests.
data Request =
    RequestSaveProgram FilePath |
    -- ^ Save the program to the location.
    RequestLoadProgram [ImageQueryStatement] |
    -- ^ Replace the current program with the list of statements.
    RequestRunProgram |
    -- ^ Run the current program.
    RequestAddStatement Int ImageQueryStatement |
    -- ^ Insert the statement at the position into the current list of statements.
    RequestInputPath InputPath |
    -- ^ Change the input path.
    RequestDeleteStatement Int
    -- ^ Delete the statement.

-- | Actions on the outside world.
data Response =
    ResponseSaveProgram FilePath [ImageQueryStatement] |
    -- ^ Save the list of statements to the path.
    ResponseProgramChanged [ImageQueryStatement] |
    -- ^ Update the GUI because the program has changed.
    ResponseRunProgram InputPath [ImageQueryStatement] |
    -- ^ Run the program on the input path.
    ResponseInputPath InputPath
    -- ^ Update the GUI because the input path has changed.

-- | A path to folder with image files used as input.
type InputPath = FilePath

-- | Effect of the given request on the program and responses.
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

-- | The GUI.
gui :: IO ()
gui = start (do

    -- Channels for interaction between the GUI and the pure model.
    (saveProgramO,saveProgramI)           <- spawn Single
    (loadProgramO,loadProgramI)           <- spawn Single
    (runProgramO,runProgramI)             <- spawn Single
    (addStatementO,addStatementI)         <- spawn Single
    (programChangedO,programChangedI)     <- spawn Single
    (changeInputPathO,changeInputPathI)   <- spawn Single
    (inputPathChangedO,inputPathChangedI) <- spawn Single
    (deleteStatementO,deleteStatementI)   <- spawn Single
    (progressO,progressI)                 <- spawn Single

    -- GUI elements.
    parentFrame           <- frame [text := "Image Processing",position := pt 100 100]
    saveProgramButton     <- createSaveProgramButton parentFrame saveProgramO
    loadProgramButton     <- createLoadProgramButton parentFrame loadProgramO
    runProgramButton      <- createRunProgramButton parentFrame runProgramO
    programListBox        <- createProgramListBox parentFrame programChangedI
    addStatementPanel     <- createAddStatementPanel programListBox parentFrame addStatementO
    inputPathButton       <- createInputPathButton parentFrame changeInputPathO
    inputPathText         <- createInputPathText parentFrame inputPathChangedI
    deleteStatementButton <- createDeleteStatementButton programListBox parentFrame deleteStatementO
    progressText          <- createProgressText parentFrame progressI

    -- Initialize the GUI.
    let wx = managed (\k -> let

            inputs = [saveProgramI,loadProgramI,runProgramI,addStatementI,changeInputPathI,deleteStatementI]

            sink (ResponseSaveProgram filepath imagequerystatements) = do
                writeFile filepath (imageQueriesPrinter imagequerystatements)
            sink (ResponseProgramChanged imagequerystatements) = do
                atomically (send programChangedO imagequerystatements)
                return ()
            sink (ResponseRunProgram inputpath imagequerystatements) = do
                n <- numberOfImages inputpath
                catch (run (\i -> progress i n) inputpath imagequerystatements) (\e ->
                    errorDialog parentFrame "Error during run" (show (e :: SomeException)))
                infoDialog parentFrame "Run finished!" "Success!"
            sink (ResponseInputPath inputpath) = do
                atomically (send inputPathChangedO inputpath)
                return ()

            in k (asSink sink,asInput (mconcat inputs)))
        progress i n = atomically (send progressO (i,n)) >> return ()

        frameLayout = row 5 [
            column 5 [
                minsize (sz 500 500) (widget programListBox),
                row 5 [
                    widget deleteStatementButton,
                    widget loadProgramButton,
                    widget saveProgramButton,
                    widget runProgramButton],
                widget progressText],
            column 5 [
                widget addStatementPanel,
                boxed "Inputs" (row 5 [
                    widget inputPathButton,
                    fill (widget inputPathText)])]]

    -- Fork a process with the pure model
    forkIO (runMVC (Program [] ".") model wx >> return ())

    -- Set the layout of the parent frame 'frameLayout'
    set parentFrame [layout := frameLayout])

-- | Create a button to save the program.
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

-- | Create a button to load a program.
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

-- | Create a list box to hold the current program
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

-- | Create a button to run the program.
createRunProgramButton :: Frame () -> Output Request -> IO (Button ())
createRunProgramButton parentFrame runProgramO = button parentFrame attributes where
    attributes = [text := "Run", on command := sendRunProgramRequest]
    sendRunProgramRequest = do
        atomically (send runProgramO RequestRunProgram)
        return ()

-- | Create a button to delete a statement.
createDeleteStatementButton :: SingleListBox () -> Frame () -> Output Request -> IO (Button ())
createDeleteStatementButton programListBox parentFrame deleteStatementO = button parentFrame attributes where
    attributes = [text := "Delete statement", on command := sendDeleteStatementRequest]
    sendDeleteStatementRequest = do
        index <- Wx.get programListBox selection
        atomically (send deleteStatementO (RequestDeleteStatement index))
        return ()

-- | Create a button to change the input path.
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

-- | Create a text field to contain the chosen input path.
createInputPathText :: Frame () -> Input InputPath -> IO (StaticText ())
createInputPathText parentFrame inputPathChangedI = do
    inputPathText <- staticText parentFrame [text := "."]
    forkIO (forever (do
        maybeInputPath <- atomically (recv inputPathChangedI)
        case maybeInputPath of
            Nothing -> return ()
            Just inputpath -> Wx.set inputPathText [text := inputpath]))
    return inputPathText

-- | Create a text field to contain the progress.
createProgressText :: Frame () -> Input (Int,Int) -> IO (StaticText ())
createProgressText parentFrame progressI = do
    progressText <- staticText parentFrame [text := "Waiting"]
    forkIO (forever (do
        maybeProgress <- atomically (recv progressI)
        case maybeProgress of
            Nothing -> return ()
            Just (i,n) -> Wx.set progressText [text := "Running: " ++ show i ++ "/" ++ show n]))
    return progressText

-- | Create a panel with various controls and buttons to add program statements.
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

-- | A statement control is an input field that allows to set the arguments of a statement.
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

-- | Given a list of names associated with values yields a statement control
-- that is a drop down menu with the given names.
choiceControl :: [(String,a)] -> StatementControl a
choiceControl choices = StatementControl (\parentPanel -> do
    c <- choice parentPanel [items := map fst choices,selection := 0]
    let getChoice = do
            s <- Wx.get c selection
            return (map snd choices !! s)
    return ([widget c],getChoice))

-- | An input field for a number.
numberControl :: (Read a) => StatementControl a
numberControl = StatementControl (\parentPanel -> do
    e <- entry parentPanel [text := "0"]
    let getNumber = do
            n <- Wx.get e text
            return (read n)
    return ([widget e],getNumber))

-- | Give a label to a statement control.
tag :: String -> StatementControl ()
tag name = StatementControl (\_ -> do
    return ([label name],return ()))

-- | Average image has no arguments.
averageImageControl :: StatementControl ImageQueryStatement
averageImageControl = pure (GetImageQueryResult ImageOfAverage)

-- | Island image has no arguments
islandImageControl :: StatementControl ImageQueryStatement
islandImageControl = pure (GetImageQueryResult IslandImage)

-- | Line image has the arguments "orientation", "start x", "start y" and "length".
lineImageControl :: StatementControl ImageQueryStatement
lineImageControl =
    (\orientation x y l -> GetImageQueryResult (LineImage orientation x y l)) <$>
    (tag "orientation:" *> choiceControl [("Horizontal",Horizontal),("Vertical",Vertical)]) <*>
    (tag "start x:" *> numberControl) <*>
    (tag "start y:" *> numberControl) <*>
    (tag "length:" *> numberControl)

-- | Area histogram has the arguments "bin size" and "exponent"
areaHistogramControl :: StatementControl ImageQueryStatement
areaHistogramControl = 
    (\binsize power -> GetImageQueryResult (AreaHistogram binsize power)) <$>
    (tag "bin size:" *> numberControl) <*>
    (tag "exponent" *> choiceControl [("One",One),("One over two",OneOverTwo),("Three over two",ThreeOverTwo)])

-- | Channel choice has one argument "color".
channelControl :: StatementControl ImageQueryStatement
channelControl =
    SetImageQueryParameter . Channel <$>
    choiceControl [("Red",Red),("Green",Green),("Blue",Blue)]

-- | Subrect choice has four arguments "upper left x", "upper left y", "width" and "height"
subrectControl :: StatementControl ImageQueryStatement
subrectControl =
    (\x y w h -> SetImageQueryParameter (SubRect (x,y,w,h))) <$>
    (tag "upper left x:" *> numberControl) <*>
    (tag "upper left y:" *> numberControl) <*>
    (tag "width" *> numberControl) <*>
    (tag "height" *> numberControl)

-- | Stencil choice prompts the user to choose an input image
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

-- | Threshold choice has a single number argument.
thresholdControl :: StatementControl ImageQueryStatement
thresholdControl =
    SetImageQueryParameter . Threshold <$>
    numberControl

-- | Smoothing choice has a single number argument.
smoothingControl :: StatementControl ImageQueryStatement
smoothingControl =
    SetImageQueryParameter . Smoothing <$>
    (tag "half width:" *> numberControl)

-- | Polarity is the choice between dark islands and bright islands.
polarityControl :: StatementControl ImageQueryStatement
polarityControl =
    SetImageQueryParameter . Polarity <$>
    choiceControl [("Dark",Dark),("Bright",Bright)]

-- | Query for a value at a point has two arguments "x" and "y"
valueInPointControl :: StatementControl ImageQueryStatement
valueInPointControl =
    (\x y -> GetImageQueryResult (TableQuery (ValueInPoint x y))) <$>
    (tag "x:" *> numberControl) <*>
    (tag "y:" *> numberControl)

-- | Query for the average around a point has three arguments "x", "y" and "half width".
averageAroundPointControl :: StatementControl ImageQueryStatement
averageAroundPointControl =
    (\x y r -> GetImageQueryResult (TableQuery (AverageAroundPoint x y r))) <$>
    (tag "x:" *> numberControl) <*>
    (tag "y:" *> numberControl) <*>
    (tag "half width:" *> numberControl)

-- | Average image has no arguments.
averageOfImageControl :: StatementControl ImageQueryStatement
averageOfImageControl = pure (GetImageQueryResult (TableQuery AverageOfImage))

-- | Queries concerning islands are a choice of "number", "average area" and "average outline"
islandQueryControl :: StatementControl ImageQueryStatement
islandQueryControl =
    GetImageQueryResult . TableQuery . IslandQuery <$>
    choiceControl [
        ("Number",NumberOfIslands),
        ("Average area",AverageAreaOfIslands),
        ("Average outline",AverageOutlineOfIslands)]

