module ImageQuery.Printer where

import ImageQuery (
    ImageQueryStatement(SetImageQueryParameter,GetImageQueryResult),
    ImageQueryParameter(Threshold,Channel,Smoothing,SubRect,StencilImage),
    ImageQuery(TableQuery,IslandImage,ImageOfAverage,LineImage),
    TableQuery(ValueInPoint,AverageAroundPoint,AverageOfImage,IslandQuery),
    IslandQuery(NumberOfIslands,AverageAreaOfIslands,AverageOutlineOfIslands),
    Polarity(Bright,Dark),
    Orientation(Horizontal,Vertical))

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
imageQueryParameterPrinter (Channel channel) = "set_channel " ++ show channel
imageQueryParameterPrinter (Smoothing smoothing) = "set_smoothing " ++ show smoothing
imageQueryParameterPrinter (SubRect (x,y,w,h)) = "set_subrect " ++ numbersPrinter [x,y,w,h]
imageQueryParameterPrinter (StencilImage filepath _) = "set_stencil " ++ filepath

imageQueryPrinter :: Printer ImageQuery
imageQueryPrinter (TableQuery tablequery) = tableQueryPrinter tablequery
imageQueryPrinter ImageOfAverage = "output_average_image"
imageQueryPrinter (LineImage orientation x y l) =
    "output_line_image " ++ orientationPrinter orientation ++ numbersPrinter [x,y,l]
imageQueryPrinter (IslandImage polarity) = "output_island_images " ++ polarityPrinter polarity

tableQueryPrinter :: Printer TableQuery
tableQueryPrinter (ValueInPoint x y) = "table_value_in_point " ++ numbersPrinter [x,y]
tableQueryPrinter (AverageAroundPoint x y r) = "table_average_around_point " ++ numbersPrinter [x,y,r]
tableQueryPrinter AverageOfImage = "table_average_of_image"
tableQueryPrinter (IslandQuery polarity NumberOfIslands) =
    "table_number_of_islands " ++ polarityPrinter polarity
tableQueryPrinter (IslandQuery polarity AverageAreaOfIslands) =
    "table_average_area_of_islands " ++ polarityPrinter polarity
tableQueryPrinter (IslandQuery polarity AverageOutlineOfIslands) =
    "table_average_outline_of_islands " ++ polarityPrinter polarity

orientationPrinter :: Printer Orientation
orientationPrinter Horizontal = "horizontal"
orientationPrinter Vertical = "vertical"

polarityPrinter :: Printer Polarity
polarityPrinter Bright = "bright"
polarityPrinter Dark = "dark"

numbersPrinter :: Printer [Int]
numbersPrinter = unwords . map show


