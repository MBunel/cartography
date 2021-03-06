#' @name gradLinkTypoLayer
#' @title Graduated and Colored Links Layer
#' @description Plot a layer of colored and graduated links. Links are plotted according to discrete classes of widths. Colors depends on a discrete variable of categories.
#' @param spdf SpatialLinesDataFrame; a link layer.
#' @param df data frame with identifier(s) and a variable.
#' @param spdfid unique identifier in spdf (spdfids, spdfide, dfids and dfide are not used).
#' @param spdfids identifier of starting points in spdf (spdfid and dfid are not used).
#' @param spdfide identifier of ending points in spdf (spdfid and dfid are not used).
#' @param dfid unique identifier in df (spdfids, spdfide, dfids and dfide are not used).
#' @param dfids identifier of starting points in df (spdfid and dfid are not used).
#' @param dfide identifier of ending points in df (spdfid and dfid are not used).
#' @param var name of the variable used to plot the links widths.
#' @param var2 name of the variable used to plot the links colors.
#' @param breaks break values in sorted order to indicate the intervals for assigning the lines widths.
#' @param lwd vector of widths (classes of widths). 
#' @param col color of the links.
#' @param legend.var.pos position of the legend for var, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright".
#' @param legend.var2.pos position of the legend for var2, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright".
#' @param legend.var.title.txt title of the legend (numeric data).
#' @param legend.var2.title.txt title of the legend (factor data).
#' @param legend.title.cex size of the legend title.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.values.rnd number of decimal places of the values in 
#' the legend.
#' @param legend.var.frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param legend.var2.frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param legend.var2.nodata text for "no data" values
#' @param legend.var2.values.order values order in the legend, a character vector 
#' that matches var modalities. Colors will be affected following this order. 
#' @param colNA no data color. 
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @note Unlike most of cartography functions, identifiers fields are mandatory.
#' @import sp
#' @seealso \link{getLinkLayer}, \link{propLinkLayer}, \link{legendGradLines}, \link{gradLinkLayer}
#' @examples 
#' data("nuts2006")
#' # Create a link layer
#' twincities.spdf <- getLinkLayer(spdf = nuts2.spdf, df = twincities.df[,1:2])
#' 
#' # Plot the links - Twin cities agreements between regions
#' plot(nuts0.spdf, col = "grey60",border = "grey20")
#' 
#' # Countries of agreements
#' twincities.df$ctry <- substr(twincities.df$j,1,2)
#' 
#' # Agreements with german cities
#' twincitiesok <- twincities.df[substr(twincities.df$i,1,2)=="DE",]
#' 
#' # plot the colored and graduated links
#' gradLinkTypoLayer(spdf = twincities.spdf, df = twincitiesok,
#'                   spdfids = "i", spdfide = "j",
#'                   dfids = "i", dfide = "j",
#'                   var = "fij", breaks = c(5,10,15,20), 
#'                   lwd = c(1,4,8),
#'                   var2 = "ctry",  add = TRUE)
#' @export
gradLinkTypoLayer <- function(spdf, df, spdfid = NULL, spdfids, spdfide, 
                              dfid = NULL, dfids, dfide,
                              var, 
                              breaks = getBreaks(v = df[,var],nclass = 4,
                                                      method = "quantile"), 
                              lwd = c(1,2,4,6),
                              var2,
                              col = NULL, colNA = "white",
                              legend.title.cex = 0.8, 
                              legend.values.cex = 0.6, 
                              legend.values.rnd = 0,
                              legend.var.pos = "bottomleft",
                              legend.var.title.txt = var,
                              legend.var.frame = FALSE,
                              legend.var2.pos = "topright", 
                              legend.var2.title.txt = var2,
                              legend.var2.values.order = NULL,
                              legend.var2.nodata = "no data",
                              legend.var2.frame = FALSE,
                              add = TRUE){
  # test
  if((length(breaks)-1) != length(lwd)){
    stop("length(lwd) must be equal to length(breaks) - 1",call. = FALSE)
  }
  
  # joint
  if (is.null(spdfid)){
    spdf@data <- data.frame(df[match(x = paste(spdf@data[,spdfids],
                                               spdf@data[,spdfide]), 
                                     table = paste(df[,dfids], 
                                                   df[,dfide])),]) 
  } else {
    spdf@data <- data.frame(df[match(x = spdf@data[,spdfid], 
                                     table = df[,dfid]),]) 
  }
  
  
  spdf <- spdf[!is.na(spdf@data[,var]),]
  spdf <- spdf[spdf@data[,var]>=min(breaks) & spdf@data[,var]<=max(breaks), ]
  
  # lwd
  lwdMap <- lwd[findInterval(x = spdf@data[,var], vec = breaks, 
                             all.inside = TRUE)]
  
  # modalities
  mod <- unique(spdf@data[, var2])
  mod <- mod[!is.na(mod)]
  # check nb col vs nb mod
  col <- checkCol(col, mod)
  # check legend.var2.values.order vs mod values
  legend.var2.values.order <- checkOrder(legend.var2.values.order, mod)
  # get the colors 
  refcol <- data.frame(mod = legend.var2.values.order, 
                       col = col[1:length(legend.var2.values.order)], 
                       stringsAsFactors = FALSE)
  colvec <- refcol[match(spdf@data[,var2], refcol[,1]),2]
  # for the legend  
  mycols <- refcol[,2]
  rVal <- refcol[,1]
  
  # for NA values
  nodata <- FALSE
  if(max(is.na(df[,var2]) > 0)){
    nodata <- TRUE
    colvec[is.na(colvec)] <- colNA
  }
  
  
  # map
  plot(spdf, col = colvec ,lwd = lwdMap, add = add)
    

  
  # legend links
  if(legend.var.pos !="n"){
    legendGradLines(pos = legend.var.pos, 
                    title.txt = legend.var.title.txt, 
                    title.cex = legend.title.cex ,
                    values.cex = legend.values.cex, 
                    breaks = breaks, lwd = lwd, 
                    col = "grey20", 
                    values.rnd = legend.values.rnd,
                    frame = legend.var.frame)
  }
  # legend typo
  if(legend.var2.pos !="n"){
    legendTypo(pos = legend.var2.pos, 
               title.txt = legend.var2.title.txt,
               title.cex = legend.title.cex, 
               values.cex = legend.values.cex,
               categ = rVal, 
               col = mycols, 
               frame = legend.var2.frame, 
               symbol="line", 
               nodata = nodata,nodata.col = colNA, 
               nodata.txt = legend.var2.nodata)
  }
}



