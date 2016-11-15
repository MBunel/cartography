#' @title Proportional Symbols Typo Layer
#' @name propSymbolsTypoLayer
#' @description Plot a proportional symbols layer with colors based on
#'  qualitative data.
#' @param sf an sf object, a simple feature collection. 
#' @param spdf a SpatialPointsDataFrame or a SpatialPolygonsDataFrame; if spdf 
#' is a SpatialPolygonsDataFrame symbols are plotted on centroids.
#' @param df a data frame that contains the values to plot. If df is missing 
#' spdf@data is used instead. 
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param dfid identifier field in df, default to the first column 
#' of df. (optional)
#' @param var name of the numeric field in df to plot the symbols sizes.
#' @param var2 name of the factor (or character) field in df to plot.
#' @param symbols type of symbols, one of "circle", "square" or "bar".
#' @param col a vector of colors.
#' @param inches size of the biggest symbol (radius for circles, width for
#' squares, height for bars) in inches.
#' @param fixmax value of the biggest symbol. (optional)
#' @param border color of symbols borders.
#' @param lwd width of symbols borders.
#' @param legend.var.pos position of the legend for var, one of "topleft", "top",
#' @param legend.var2.pos position of the legend for var2, one of "topleft", "top",
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright".
#' @param legend.var.title.txt title of the legend (numeric data).
#' @param legend.var2.title.txt title of the legend (factor data).
#' @param legend.title.cex size of the legend title.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.values.rnd number of decimal places of the values in
#' the legend.
#' @param legend.var.style either "c" or "e". The legend has two display
#' styles, "c" stands for compact and "e" for extended.
#' @param legend.var.frame whether to add a frame to the legend (TRUE) or
#' not (FALSE).
#' @param legend.var2.frame whether to add a frame to the legend (TRUE) or
#' not (FALSE).
#' @param legend.var2.values.order values order in the legend, a character vector 
#' that matches var modalities. Colors will be affected following this order.  
#' @param legend.var2.nodata text for "no data" values
#' @param add whether to add the layer to an existing plot (TRUE) or
#' not (FALSE).
#' @param colNA no data color. 
#' @export
#' @import sp
#' @import sf
#' @seealso \link{legendBarsSymbols}, \link{legendTypo},
#' \link{legendCirclesSymbols}, \link{legendSquaresSymbols},
#' \link{typoLayer}, \link{propSymbolsLayer}
#' @examples 
#' # Population plot on proportional symbols
#' data("nuts2006")
#' nuts0.spdf@data <- nuts0.df
#' x <- sf::st_as_sf(nuts0.spdf)
#' ## Example 1
#' plot(x, col = "grey60",border = "grey20")
#' x$typo <- c(rep("A",10),rep("B",10),rep("C",10),rep("D",4))
#' propSymbolsTypoLayer(sf = x,
#'                      var = "pop2008", var2="typo")
#' 
#' ## Example 2
#' #Countries plot
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add = FALSE)
#' x$typo <- c(rep("A",10),rep("B",10),rep("C",10),rep("D",4))
#' x$typo[1:3] <- NA
#' x$pop2008[4:6] <- NA
#' propSymbolsTypoLayer(sf = x,
#'                      var = "pop2008", var2="typo",
#'                      symbols = "circle",
#'                      legend.var.pos = "topright",
#'                      legend.var2.pos = "right",
#'                      legend.var.title.txt = "Total\npopulation (2008)",
#'                      legend.values.rnd = -3,
#'                      legend.var2.title.txt = "Category",
#'                      col = carto.pal(pal1 = "pastel.pal", 4),
#'                      legend.var2.values.order = c("A", "B", "C", "D"),
#'                      legend.var.style = "c")
#' # Layout plot
#' layoutLayer(title = "Countries Population & Color in Europe",
#'             sources = "UMS RIATE, 2015",
#'            scale = NULL,
#'             frame = TRUE,
#'             col = "black",
#'             coltitle = "white") 
propSymbolsTypoLayer <- function(sf, spdf, df, spdfid = NULL, dfid = NULL, var,
                                 inches = 0.3, fixmax = NULL, symbols = "circle",
                                 border = "grey20", lwd = 1,
                                 var2, col = NULL, colNA = "white",
                                 legend.title.cex = 0.8,
                                 legend.values.cex = 0.6,
                                 legend.var.pos = "bottomleft",
                                 legend.var.title.txt = var,
                                 legend.values.rnd = 0,
                                 legend.var.style = "c",
                                 legend.var.frame = FALSE,
                                 legend.var2.pos = "topright",
                                 legend.var2.title.txt = var2,
                                 legend.var2.values.order = NULL,
                                 legend.var2.nodata = "no data",
                                 legend.var2.frame = FALSE,
                                 add = TRUE){
  
  # spdf management
  if (missing(sf)){
    # Check missing df and NULL identifiers 
    if (missing(df)){df <- spdf@data}
    if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
    if (is.null(dfid)){dfid <- names(df)[1]}
    spdf@data <- data.frame(spdf@data, df[match(spdf[[spdfid]], df[[dfid]]),])
    x <- sf::st_as_sf(spdf)
  }
  
  # Filter and order  
  
  # surf to point or point to point
  st_geometry(x) <- st_centroid(x)
  # no NAs and no 0s
  x <- x[!is.na(x = x[[var]]) & x[[var]] != 0, ]
  # Order the dots
  x <- x[order(abs(x[[var]]), decreasing = TRUE),]
  
  
  # Color management 
  
  # modalities
  mod <- unique(x[[var2]])
  mod <- mod[!is.na(mod)]

  # check nb col vs nb mod
  col <- checkCol(col, mod)
  # check legend.var2.values.order vs mod values
  legend.var2.values.order <- checkOrder(legend.var2.values.order, mod)
  # get the colors 
  refcol <- data.frame(mod = legend.var2.values.order, 
                       col = col[1:length(legend.var2.values.order)], 
                       stringsAsFactors = FALSE)
  
  mycols <- refcol[match(x[[var2]], refcol[["mod"]]), 2]
  # for the legend  
  mycolsleg <- refcol[,2]
  rVal <- refcol[,1]
  # no data stuff
  nodata <- FALSE
  if(max(is.na(x[[var2]]) > 0)){
    nodata <- TRUE
    mycols[is.na(mycols)] <- colNA
  }
  
  
  
  # Display

  # Display prop symbols
  pSymLegParam <- pSymDisp(x = x, var = var, fixmax = fixmax, inches = inches, 
                           mycols = mycols,  border = border, lwd = lwd, 
                           add = add, symbols = symbols)
  # Display legend
  pSymLegDisp(pSymLegParam$varvect, 
              pSymLegParam$sizevect, 
              symbols,
              breakval = NULL, col = "grey", col2 = NULL,
              legend.var.pos, 
              legend.var.title.txt,
              legend.title.cex, 
              legend.values.cex,
              legend.values.rnd,
              legend.var.frame,
              legend.var.style)

  # second legend display
  if(legend.var2.pos !="n"){
    legendTypo(pos = legend.var2.pos,
               title.txt = legend.var2.title.txt,
               title.cex = legend.title.cex,
               values.cex = legend.values.cex,
               categ = rVal,
               col = mycolsleg,
               frame = legend.var2.frame,
               symbol="box",
               nodata = nodata,nodata.col = colNA,
               nodata.txt = legend.var2.nodata)
  }
}


