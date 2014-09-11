module ImageQuery.Printer where

import ImageQuery (
    ImageQueryStatement(SetImageQueryParameter,GetImageQueryResult),
    ImageQueryParameter(Threshold,Channel,Smoothing,SubRect,StencilImage,Polarity),
    ImageQuery(TableQuery,IslandImage,ImageOfAverage,LineImage,AreaHistogram),
    TableQuery(ValueInPoint,AverageAroundPoint,AverageOfImage,IslandQuery),
    IslandQuery(NumberOfIslands,AverageAreaOfIslands,AverageOutlineOfIslands),
    Polarity(Bright,Dark),
    Orientation(Horizontal,Vertical),
    Channel(Red,Green,Blue),
    Power(One,OneOverTwo,ThreeOverTwo))

type Printer a = a -> String

imageQueriesPrinter :: Printer [ImageQueryStatement]
imageQueriesPrinter = unlines . map imageQueryStatementPrinter

imageQueryStatementPrinter :: Printer ImageQueryStatement
imageQueryStatementPrinter (SetImageQueryParameter imagequeryparameter) =
    imageQueryParameterPrinter imagequeryparameter
imageQueryStatementPrinter (GetImageQueryResult imagequery) =
    imageQueryPrinter imagequery

imageQueryParameterPrinter :: Printer ImageQueryParameter
imageQueryParameterPrinter (Threshold threshold) = "set_threshold " ++ show threshold
imageQueryParameterPrinter (Channel channel) = "set_channel " ++ channelPrinter channel
imageQueryParameterPrinter (Smoothing smoothing) = "set_smoothing " ++ show smoothing
imageQueryParameterPrinter (SubRect (x,y,w,h)) = "set_subrect " ++ numbersPrinter [x,y,w,h]
imageQueryParameterPrinter (StencilImage filepath _) = "set_stencil " ++ filepath
imageQueryParameterPrinter (Polarity polarity) = "set_polarity " ++ polarityPrinter polarity

imageQueryPrinter :: Printer ImageQuery
imageQueryPrinter (TableQuery tablequery) = tableQueryPrinter tablequery
imageQueryPrinter ImageOfAverage = "output_average_image"
imageQueryPrinter (LineImage orientation x y l) =
    "output_line_image " ++ orientationPrinter orientation ++ " " ++ numbersPrinter [x,y,l]
imageQueryPrinter IslandImage = "output_island_images"
imageQueryPrinter (AreaHistogram bins binsize power) = unwords [
    "output_area_histogram",
    show bins,
    show binsize,
    powerPrinter power]


tableQueryPrinter :: Printer TableQuery
tableQueryPrinter (ValueInPoint x y) = "table_value_in_point " ++ numbersPrinter [x,y]
tableQueryPrinter (AverageAroundPoint x y r) = "table_average_around_point " ++ numbersPrinter [x,y,r]
tableQueryPrinter AverageOfImage = "table_average_of_image"
tableQueryPrinter (IslandQuery NumberOfIslands) =
    "table_number_of_islands"
tableQueryPrinter (IslandQuery AverageAreaOfIslands) =
    "table_average_area_of_islands"
tableQueryPrinter (IslandQuery AverageOutlineOfIslands) =
    "table_average_outline_of_islands"

orientationPrinter :: Printer Orientation
orientationPrinter Horizontal = "horizontal"
orientationPrinter Vertical = "vertical"

polarityPrinter :: Printer Polarity
polarityPrinter Bright = "bright"
polarityPrinter Dark = "dark"

numbersPrinter :: Printer [Int]
numbersPrinter = unwords . map show

channelPrinter :: Printer Channel
channelPrinter Red = "red"
channelPrinter Green = "green"
channelPrinter Blue = "blue"

powerPrinter :: Printer Power
powerPrinter One = "one"
powerPrinter OneOverTwo = "one_over_two"
powerPrinter ThreeOverTwo = "three_over_two"

