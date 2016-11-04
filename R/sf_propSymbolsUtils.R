pSymDisp <- function(x, var, fixmax, inches, mycols, 
                               border, lwd, add, symbols){
  
  if (is.null(fixmax)){
    fixmax <- max(x[,var])
  }
  
  smax <- inches * inches * pi
  switch(symbols, 
         circle = {
           smax <- inches * inches * pi
           size <- sqrt((abs(x[, var]) * smax  / fixmax) / pi)
         }, 
         square = {
           smax <- inches * inches
           size <- sqrt(abs(x[, var]) * smax   / fixmax)
         }, 
         bar = {
           smax <- inches
           size <- abs(x[,var]) * smax  / fixmax
         })
  
  # compute sizes
  sizes <- size$x
  
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
  if (add == FALSE){
    plot(x, col = NA)
  }
  
  # extract xy 
  xy <- t(sapply(st_geometry(x), FUN = function(X){(X[1:2])}))
  
  switch(symbols, 
         circle = {
           symbols(x = xy, circles = sizes, bg = mycols, fg = border, 
                   lwd = lwd, add = TRUE, inches = inches, asp = 1)
         }, 
         square = {
           symbols(x = xy, squares = sizes, bg = mycols, fg = border, 
                   lwd = lwd, add = TRUE, inches = inches, asp = 1)
         }, 
         bar = {
           tmp <- as.matrix(data.frame(width = inches/10, height = sizes))
           xy[,2] <- xy[,2] + yinch(sizes/2)
           symbols(x = xy, rectangles = tmp, add = TRUE, bg = mycols,
                   fg = border, lwd = lwd, inches = inches, asp = 1)
         })
  
  invisible(list(varvect = varvect, sizevect = sizevect))
}



pSymLegDisp <- function(varvect, sizevect, symbols,
                                     breakval, col, col2,
                                     legend.pos, 
                                     legend.title.txt,
                                     legend.title.cex, 
                                     legend.values.cex,
                                     legend.values.rnd,
                                     legend.frame,
                                     legend.style){
  if(legend.pos!="n"){
    switch(symbols, 
           circle = {
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
           }, 
           square = {
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
           }, 
           bar = {
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
           })
  }
}