#' @title Proportional Symbols Layer
#' @name sf_propSymbolsLayer
#' @description Plot a proportional symbols layer.
#' @param x a SpatialPointsDataFrame or a SpatialPolygonsDataFrame; if spdf 
#' is a SpatialPolygonsDataFrame symbols are plotted on centroids.
#' @param var name of the numeric field in df to plot.
#' @param inches size of the biggest symbol (radius for circles, width for
#' squares, height for bars) in inches.
#' @param symbols type of symbols, one of "circle", "square" or "bar".
#' @param k share of the map occupied by the biggest symbol (this argument
#' is deprecated; please use inches instead.).
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
#' @import sp
#' @examples
#' data("nuts2006")
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
  # nuts0.spdf@data <- nuts0.df
  # x <- st_as_sf(nuts0.spdf)
  # surf to point or point to point
  st_geometry(x) <- st_centroid(x)
  
  # 
  # dim(x)
  # 
  # var <- "death_2008"
  # no NAs and no 0s
  x <- x[!is.na(x = x[,var]) & x[,var]!=0,]

  
  # Order the dots
  x <- x[order(abs(x[, var]), decreasing = TRUE),]

  # Double color management
  if (!is.null(breakval)){
    mycols <- rep(NA,nrow(x))
    mycols <- ifelse(test = x[,var] >= breakval, 
                     yes = col,
                     no = col2)
  }else{
    mycols <- rep(col, nrow(x))
  }
  
  if (is.null(fixmax)){
    fixmax <- max(x[,var])
  }
  
  sizer <- function(dots, inches, var, fixmax, symbols){
    smax <- inches * inches * pi
    switch(symbols, 
           circle = {
             smax <- inches * inches * pi
             size <- sqrt((abs(dots[, var]) * smax  / fixmax) / pi)
           }, 
           square = {
             smax <- inches * inches
             size <- sqrt(abs(dots[, var]) * smax   / fixmax)
           }, 
           bar = {
             smax <- inches
             size <- abs(dots[,var]) * smax  / fixmax
           })
    return(size$x)
  }
  
  # compute sizes
  sizes <- sizer(dots = x, inches = inches, var = var, 
                 fixmax = fixmax, symbols = symbols)
  
  # size and values for legend, hollow circle (fixmax case)
  sizeMax <- max(sizes)
  if (inches <= sizeMax){
    sizevect <- xinch(seq(inches, min(sizes), length.out = 4))
    varvect <- seq(fixmax, 0, length.out = 4)
    inches <- sizeMax
  }else{
    mycols <- c(NA, mycols)
    border <- c(NA, rep(border, nrow(x)))
    x <- rbind(x[1,],x)
    x[1,var] <- fixmax
    sizes <- c(inches, sizes)
    sizevect <- xinch(seq(inches, min(sizes), length.out = 4))
    varvect <- seq(fixmax, 0,length.out = 4 )
  }
  
  # plot
  if (add==FALSE){
    plot(x, col = NA)
  }

  w <- t(sapply(st_geometry(x), FUN = function(X){(X[1:2])}))
  print(w)
  switch(symbols, 
         circle = {
           symbols(x = w, circles = sizes, bg = mycols, fg = border, 
                   lwd = lwd, add = TRUE, inches = inches, asp = 1)
           if(legend.pos!="n"){
             legendCirclesSymbols(pos = legend.pos, 
                                  title.txt = legend.title.txt,
                                  title.cex = legend.title.cex,
                                  values.cex = legend.values.cex,
                                  var = varvect,
                                  r = sizevect,
                                  breakval  = breakval,
                                  col = col,
                                  col2 = col2,
                                  frame = legend.frame,
                                  values.rnd =  legend.values.rnd,
                                  style = legend.style)
           }
         }, 
         square = {
           symbols(x = w, squares = sizes, bg = mycols, fg = border, 
                   lwd = lwd, add = TRUE, inches = inches, asp = 1)
           if(legend.pos!="n"){
             legendSquaresSymbols(pos = legend.pos,
                                  title.txt = legend.title.txt,
                                  title.cex = legend.title.cex,
                                  values.cex = legend.values.cex,
                                  var = varvect,
                                  r = sizevect,
                                  breakval  = breakval,
                                  col = col,
                                  col2 = col2,
                                  frame = legend.frame,
                                  values.rnd =  legend.values.rnd,
                                  style = legend.style)
           }
         }, 
         bar = {
           tmp <- as.matrix(data.frame(width = inches/10, height = sizes))
           w[,2] <- w[,2] + yinch(sizes/2)
           symbols(x = w, rectangles = tmp, add = TRUE, bg = mycols,
                   fg = border, lwd = lwd, inches = inches, asp = 1)
           if(legend.pos!="n"){
             legendBarsSymbols(pos = legend.pos, 
                               title.txt = legend.title.txt,
                               title.cex = legend.title.cex,
                               values.cex = legend.values.cex,
                               var = varvect,
                               r = sizevect,
                               breakval  = breakval,
                               col = col,
                               col2 = col2,
                               frame = legend.frame,
                               values.rnd =  legend.values.rnd,
                               style = legend.style)
           }
         })
  
}
