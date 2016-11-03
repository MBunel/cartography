#' @title Proportional Symbols Typo Layer
#' @name sf_propSymbolsTypoLayer
#' @description Plot a proportional symbols layer with colors based on
#'  qualitative data.
#' @param x SpatialPointsDataFrame or SpatialPolygonsDataFrame; if spdf
#' is a SpatialPolygonsDataFrame symbols are plotted on centroids.
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
sf_propSymbolsTypoLayer <- function(x, var,
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

  # Filter and order  
  
  # surf to point or point to point
  st_geometry(x) <- st_centroid(x)
  # no NAs and no 0s
  x <- x[!is.na(x = x[,var]) & x[,var]!=0,]
  # Order the dots
  x <- x[order(abs(x[, var]), decreasing = TRUE),]
  
  
  # Color management 
  
  # modalities
  mod <- unique(x[, var2])
  mod <- mod[!is.na(mod)]
  # check nb col vs nb mod
  col <- checkCol(col, mod)
  # check legend.var2.values.order vs mod values
  legend.var2.values.order <- checkOrder(legend.var2.values.order, mod)
  # get the colors 
  refcol <- data.frame(mod = legend.var2.values.order, 
                       col = col[1:length(legend.var2.values.order)], 
                       stringsAsFactors = FALSE)
  mycols <- refcol[match(x[, var2], refcol[,1]),2]
  # for the legend  
  mycolsleg <- refcol[,2]
  rVal <- refcol[,1]
  # no data stuff
  nodata <- FALSE
  if(max(is.na(x[,var2])>0)){
    nodata <- TRUE
    mycols[is.na(mycols)] <- colNA
  }
  
  print(mycols)
  # Display
  
  # Display prop symbols
  propSymbolsDisplay(x = x, 
                     fixmax = fixmax, 
                     var = var, 
                     symbols = symbols,
                     inches = inches, 
                     mycols = mycols, 
                     border = border, 
                     col = mycols, 
                     col2 = NULL,
                     breakval  = NULL,
                     legend.pos = legend.pos, 
                     legend.title.txt = legend.title.txt,
                     legend.title.cex = legend.title.cex,
                     legend.values.cex = legend.values.cex,
                     legend.frame = legend.frame,
                     legend.values.rnd =  legend.values.rnd,
                     legend.style = legend.style)   
  
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


