#' @title Proportional Symbols Layer
#' @name sf_propSymbolsLayer
#' @description Plot a proportional symbols layer.
#' @param x a SpatialPointsDataFrame or a SpatialPolygonsDataFrame; if spdf 
#' is a SpatialPolygonsDataFrame symbols are plotted on centroids.
#' @param var name of the numeric field in df to plot.
#' @param inches size of the biggest symbol (radius for circles, width for
#' squares, height for bars) in inches.
#' @param symbols type of symbols, one of "circle", "square" or "bar".
#' @param col color of symbols.
#' @param col2 second color of symbols (see Details).
#' @param border color of symbols borders.
#' @param lwd width of symbols borders.
#' @param breakval breaking value (see Details).
#' @param fixmax value of the biggest symbol (see Details).
#' @param legend.pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
#' legend.pos is "n" then the legend is not plotted.
#' @param legend.title.txt title of the legend.
#' @param legend.title.cex size of the legend title.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.values.rnd number of decimal places of the values 
#' displayed in the legend.
#' @param legend.style either "c" or "e". The legend has two display 
#' styles, "c" stands for compact and "e" for extended.
#' @param legend.frame boolean; whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @details The breakval parameter allows to plot symbols of two 
#' colors: the first color (col) for values superior or equal to breakval,
#' second color (col2) for values inferior to breakval.
#' 
#' Two maps with the same inches and fixmax parameters will be comparable.
#' @export
#' @seealso \link{legendBarsSymbols}, \link{legendCirclesSymbols}, 
#' \link{legendSquaresSymbols}, \link{propSymbolsChoroLayer}, 
#' \link{propSymbolsTypoLayer}
#' @import sf
#' @examples
#' data("nuts2006")
#' ## Example 1
#' # Layout plot
#' layoutLayer(title = "Countries Population in Europe",
#'             sources = "Eurostat, 2008",
#'             scale = NULL,
#'             frame = TRUE,
#'             col = "black",
#'             coltitle = "white",
#'             bg = "#D9F5FF",
#'             south = TRUE,
#'             extent = nuts0.spdf)
#' # Countries plot
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=TRUE)
#' # Population plot on proportional symbols
#' nuts0.spdf@data <- nuts0.df
#' x <- sf::st_as_sf(nuts0.spdf)
#' 
#' sf_propSymbolsLayer(x = x,
#'                     var = "pop2008", 
#'                     symbols = "square", col =  "#920000",
#'                     legend.pos = "right",
#'                     legend.title.txt = "Total\npopulation (2008)",
#'                     legend.style = "c")
#' 
#' ## Example 2
#' # Countries plot
#' plot(nuts0.spdf, col = "grey60",border = "grey20")
#' # Population plot on proportional symbols
#' sf_propSymbolsLayer(x=x,
#'                     var = "gdppps2008",
#'                     symbols = "bar", col =  "#B00EF0",
#'                     legend.pos = "right",
#'                     legend.title.txt = "GDP\nin Millions PPS (2008)",
#'                     legend.style = "e")
#' 
#' ## Example 3
#' oldpar <- par(mfrow = c(1,2), mar = c(0,0,0,0))
#' # Countries plot
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=FALSE)
#' # Population plot on proportional symbols
#' sf_propSymbolsLayer(x = x,
#'                     var = "birth_2008", 
#'                     fixmax = max(x$birth_2008),
#'                     inches = 0.2,
#'                     symbols = "circle", col =  "orange",
#'                     legend.pos = "right",
#'                     legend.title.txt = "nb of births",
#'                     legend.style = "e")
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=FALSE)
#' # Population plot on proportional symbols
#' sf_propSymbolsLayer(x = x,
#'                     var = "death_2008",
#'                     symbols = "circle", col =  "pink",
#'                     fixmax = max(x$birth_2008),
#'                     inches = 0.2,
#'                     legend.pos = "right",
#'                     legend.style = "e",
#'                     legend.title.txt = "nb of deaths")
#' par(oldpar)
#' 
#' ## Example 4
#' x$balance <- x$birth_2008-x$death_2008
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=FALSE)
#' # Population plot on proportional symbols
#' sf_propSymbolsLayer(x = x, inches = 0.3,
#'                     var = "balance",
#'                     symbols = "circle",
#'                     col = "orange", col2 = "green", breakval=0,
#'                     legend.pos = "right",
#'                     legend.style = "c",
#'                     legend.title.txt = "Natural Balance\n(2008)")
sf_propSymbolsLayer <- function(x, var,
                                inches = 0.3, fixmax = NULL, 
                                breakval = NULL,
                                symbols = "circle", 
                                col = "#E84923", col2 = "#7DC437", 
                                border = "black", lwd = 1,
                                legend.pos = "bottomleft", 
                                legend.title.txt = var,
                                legend.title.cex = 0.8, 
                                legend.values.cex = 0.6,
                                legend.values.rnd = 0,
                                legend.style = "c", 
                                legend.frame = FALSE,
                                add = TRUE){
  # Filter and order  
  
  # surf to point or point to point
  st_geometry(x) <- st_centroid(x)
  # no NAs and no 0s
  x <- x[!is.na(x = x[,var]) & x[,var]!=0,]
  # Order the dots
  x <- x[order(abs(x[, var]), decreasing = TRUE),]
  
  
  
  # Color Management
  
  # Double color management
  if (!is.null(breakval)){
    mycols <- rep(NA,nrow(x))
    mycols <- ifelse(test = x[,var] >= breakval, 
                     yes = col,
                     no = col2)
  }else{
    mycols <- rep(col, nrow(x))
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
              breakval, col, col2,
              legend.pos, 
              legend.title.txt,
              legend.title.cex, 
              legend.values.cex,
              legend.values.rnd,
              legend.frame,
              legend.style)

}
