#' @title Proportional and Choropleth Symbols Layer
#' @name sf_propSymbolsChoroLayer
#' @description Plot a proportional symbols layer with color based on a 
#' quantitative data discretization. 
#' @param x SpatialPointsDataFrame or SpatialPolygonsDataFrame; if spdf 
#' is a SpatialPolygonsDataFrame symbols are plotted on centroids.
#' @param inches size of the biggest symbol (radius for circles, width for
#' squares, height for bars) in inches.
#' @param var name of the numeric field in df to plot the symbols sizes.
#' @param var2 name of the numeric field in df to plot the colors.
#' @param breaks break points in sorted order to indicate the intervals for assigning the colors. 
#' Note that if there are nlevel colors (classes) there should be (nlevel+1) 
#' breakpoints (see \link{choroLayer} Details).
#' @param col a vector of colors. Note that if breaks is specified there must be one less 
#' colors specified than the number of break. 
#' @param nclass a targeted number of classes. If null, the number of class is 
#' automatically defined (see \link{choroLayer} Details).
#' @param method a discretization method; one of "sd", "equal", 
#' "quantile", "fisher-jenks", "q6" or "geom"  (see \link{choroLayer} Details).
#' @param symbols type of symbols, one of "circle", "square" or "bar".
#' @param fixmax value of the biggest symbol (see \link{propSymbolsLayer} Details).
#' @param border color of symbols borders.
#' @param lwd width of symbols borders.
#' @param legend.var.pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
#' legend.var.pos is "n" then the legend is not plotted.
#' @param legend.var2.pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
#' legend.var2.pos is "n" then the legend is not plotted.
#' @param legend.var.title.txt title of the legend (proportional symbols).
#' @param legend.var2.title.txt title of the legend (colors).
#' @param legend.title.cex size of the legend title.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.var.values.rnd number of decimal places of the values in 
#' the legend.
#' @param legend.var2.values.rnd number of decimal places of the values in 
#' the legend.
#' @param legend.var.style either "c" or "e". The legend has two display 
#' styles.
#' @param legend.var.frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param legend.var2.frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param legend.var2.nodata text for "no data" values
#' @param colNA no data color. 
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @export
#' @seealso \link{legendBarsSymbols}, \link{legendChoro}, 
#' \link{legendCirclesSymbols}, \link{legendSquaresSymbols}, 
#' \link{choroLayer}, \link{propSymbolsLayer}
#' @import sp
sf_propSymbolsChoroLayer <- function(x,
                                     var, 
                                     inches = 0.3, fixmax = NULL, 
                                     symbols = "circle", border = "grey20", lwd = 1,
                                     var2, 
                                     breaks = NULL,  method="quantile",  
                                     nclass= NULL, 
                                     col = NULL,
                                     colNA = "white",
                                     legend.title.cex = 0.8, 
                                     legend.values.cex = 0.6,
                                     legend.var.pos = "right",
                                     legend.var.title.txt = var, 
                                     legend.var.values.rnd = 0, 
                                     legend.var.style = "c",
                                     legend.var.frame = FALSE, 
                                     legend.var2.pos = "topright", 
                                     legend.var2.title.txt = var2,
                                     legend.var2.values.rnd = 2,  
                                     legend.var2.nodata = "no data",
                                     legend.var2.frame = FALSE,
                                     add = TRUE){
  
  # data("nuts2006")
  # nuts0.spdf@data <- nuts0.df
  # x <- sf::st_as_sf(nuts0.spdf)
  # library(sf)
  # var2 <- "gdppps2008"
  # breaks = NULL
  # col = NULL
  # nclass = NULL
  # method = "quantile"
  # var = "pop2008" 
  
  # Filter and order  
  
  # surf to point or point to point
  st_geometry(x) <- st_centroid(x)
  # no NAs and no 0s
  x <- x[!is.na(x = x[,var]) & x[,var]!=0,]
  # Order the dots
  x <- x[order(abs(x[, var]), decreasing = TRUE),]
  
  
  # Color Management
  layer <- choro(var=x[,var2]$x, distr = breaks, col = col,
                 nclass = nclass, method = method)
  mycols <- layer$colMap
  nodata <- FALSE
  if(max(is.na(x[,var2]$x)>0)){
    nodata <- TRUE
    mycols[is.na(mycols)] <- colNA
  }
  
  
  
  
  
  # if (is.null(fixmax)){
  #   fixmax <- max(dots[,var])
  # }
  # 
  # # size management
  # sizes <- sizer(dots = dots, inches = inches, var = var,
  #                fixmax = fixmax, symbols = symbols)
  # sizeMax <- max(sizes)
  # 
  # if (inches <= sizeMax){
  #   sizevect <- xinch(seq(inches, min(sizes), length.out = 4))
  #   varvect <- seq(fixmax,0,length.out = 4 )
  #   inches <- sizeMax
  # }else{
  #   mycols <- c(NA, mycols)
  #   border <- c(NA, rep(border, nrow(dots)))
  #   dots <- rbind(dots[1,],dots)
  #   dots[1,var] <- fixmax
  #   sizes <- c(inches, sizes)
  #   sizevect <- xinch(seq(inches, min(sizes), length.out = 4))
  #   varvect <- seq(fixmax, 0,length.out = 4 )
  # }
  # 
  # if (add==FALSE){
  #   sp::plot(spdf, col = NA, border = NA)
  # }
  # switch(symbols, 
  #        circle = {
  #          symbols(dots[, 2:3], circles = sizes, bg = as.vector(mycols), 
  #                  fg = border, 
  #                  lwd = lwd, add = TRUE, inches = inches, asp = 1)
  #          if(legend.var.pos!="n"){
  #            legendCirclesSymbols(pos = legend.var.pos, 
  #                                 title.txt = legend.var.title.txt,
  #                                 title.cex = legend.title.cex,
  #                                 values.cex = legend.values.cex,
  #                                 var = varvect,
  #                                 r = sizevect,
  #                                 col = "grey",
  #                                 frame = legend.var.frame,
  #                                 values.rnd =  legend.var.values.rnd,
  #                                 style = legend.var.style)
  #          }
  #        }, 
  #        square = {
  #          symbols(dots[, 2:3], squares = sizes, bg = as.vector(mycols), 
  #                  fg = border, 
  #                  lwd = lwd, add = TRUE, inches = inches, asp = 1)
  #          if(legend.var.pos!="n"){
  #            legendSquaresSymbols(pos = legend.var.pos, 
  #                                 title.txt = legend.var.title.txt,
  #                                 title.cex = legend.title.cex,
  #                                 values.cex = legend.values.cex,
  #                                 var = varvect,
  #                                 r = sizevect,
  #                                 col = "grey",
  #                                 frame = legend.var.frame,
  #                                 values.rnd =  legend.var.values.rnd,
  #                                 style = legend.var.style)
  #          }
  #        }, 
  #        bar = {
  #          tmp <- as.matrix(data.frame(width = inches/10, height = sizes))
  #          dots[,3] <- dots[,3] + yinch(sizes/2)
  #          symbols(dots[,2:3], rectangles = tmp, add = TRUE, 
  #                  bg = as.vector(mycols),
  #                  fg = border, lwd = lwd, inches = inches, asp = 1)
  #          if(legend.var.pos!="n"){
  #            legendBarsSymbols(pos = legend.var.pos, 
  #                              title.txt = legend.var.title.txt,
  #                              title.cex = legend.title.cex,
  #                              values.cex = legend.values.cex,
  #                              var = varvect,
  #                              r = sizevect,
  #                              col = "grey",
  #                              frame = legend.var.frame,
  #                              values.rnd =  legend.var.values.rnd,
  #                              style = legend.var.style)
  #          }
  #        })
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
              legend.var.values.rnd,
              legend.var.frame,
              legend.var.style)
  
  if(legend.var2.pos !="n"){
    legendChoro(pos = legend.var2.pos, 
                title.txt = legend.var2.title.txt,
                title.cex = legend.title.cex,
                values.cex = legend.values.cex,
                breaks = layer$distr, 
                col = layer$col, 
                values.rnd = legend.var2.values.rnd,
                frame = legend.var2.frame, 
                symbol="box", 
                nodata = nodata, nodata.col = colNA,
                nodata.txt = legend.var2.nodata)
  }
}


