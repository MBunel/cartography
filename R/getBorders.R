# 
# 
# data("nuts2006")
# plot(nuts0.spdf, col = "grey60",border = "grey20", add=TRUE)
# # Population plot on proportional symbols
# nuts0.spdf@data <- nuts0.df
# x <- sf::st_as_sf(nuts0.spdf)
# 
# 
# 
# 
# 
# getBorders <- function(sf, sfid = NULL){
#  
#   
#   
#   
#   library(sf)
# 
#   
#   
# 
# 
#   # spdf <- gBuffer(spdf, width = 1, byid = T)
#   # df <- data.frame(id = sapply(methods::slot(spdf, "polygons"),
#   #                              methods::slot, "ID"))
#   # row.names(df) <- df$id
#   # spdf <- SpatialPolygonsDataFrame(Sr = spdf, data = df)
#   # 
#   # sldf <- methods::as(object = spdf, Class = "SpatialLines")
#   # df <- data.frame(id = sapply(methods::slot(sldf, "lines"),
#   #                              methods::slot, "ID"))
#   # row.names(df) <- df$id
#   # sldf <- SpatialLinesDataFrame(sl = sldf, data = df)
#   
#   data("nuts2006")
#   x <- sf::st_as_sf(nuts0.spdf)
#   sfp <- x
#   sfl <- x
#   st_geometry(sfp) <- sf::st_buffer(x = x, dist = 10000)
#   st_geometry(sfl) <-  sf::st_boundary(x = x)  
# 
#   st_is_valid(sfl[1, ])
#   str(st_geometry(sfp))
#   xx <- sfp[1, ]
#   xx
#   plot(xx)
#   
#   plot(li)
#   st_is_valid(xx)
#   lj
#   xx
#   
#   l <- st_intersects(sf,sparse = TRUE)
#   
#   nbinter <- sum(unlist(lapply(X = l, FUN = length)))
#   
#   ind <- 1
#   for (i in 1:length(l)) {
#   i <- 1
#     li <- sfl[i,]
#     plot(li)
#     for (j in 1:length(l[[i]])) {
#       j <- 2
#       l[[i]][[j]]
#       lj <- sfp[l[[i]][[j]], ]
#       plot(lj, add=T, col = "#92000050")
#   st_is_valid(lj)
#       class(li)
#       class(lj)
#           Inter <- sf::st_intersection(x = li, y0 = lj)
#   plot(Inter)
#       
#       ind <- 1
#   wkt <- rep(NA, long)
#   for (i in 1:length(myl)) {
#     id1 <- names(myl[i])
#     li <- sldf[sldf$id == id1, ]
#     for (j in 1:length(myl[[i]])) {
#       id2 <- myl[[i]][[j]]
#       po <- spdf[spdf$id == id2, ]
#       Inter <- rgeos::gIntersection(li, po, byid = T)
#       if (class(Inter) != "SpatialLines") {
#         if (class(Inter) == "SpatialPoints") {
#           rm(Inter)
#         } else{
#           Inter <- Inter@lineobj
#           row.names(Inter) <- paste(id1, id2, sep = "_")
#           wkt[ind] <- rgeos::writeWKT(Inter)
#           df[ind,] <- c(paste(id1, id2, sep = "_"), id1, id2)
#           ind <- ind + 1
#         }
#       } else{
#         row.names(Inter) <- paste(id1, id2, sep = "_")
#         wkt[ind] <- rgeos::writeWKT(Inter)
#         df[ind,] <- c(paste(id1, id2, sep = "_"), id1, id2)
#         ind <- ind + 1
#       }
#     }
#   }
#   
#   
#   
#   
#   
#   l <- lower.tri(l) * l
#   gna <- function(x) {
#     y <- x[x == 1]
#     if (length(y) > 0) {
#       names(y)
#     } else{
#       NA
#     }
#   }
#   l
#   
#   myl <- as.list(apply(l, 1, gna))
#   myl <- myl[!is.na(myl)]
#   
#   long <- sum(sapply(myl, length))
#   df <- data.frame(
#     id = rep(NA, long),
#     id1 = rep(NA, long),
#     id2 = rep(NA, long)
#   )
#   
#   ind <- 1
#   wkt <- rep(NA, long)
#   for (i in 1:length(myl)) {
#     id1 <- names(myl[i])
#     li <- sldf[sldf$id == id1, ]
#     for (j in 1:length(myl[[i]])) {
#       id2 <- myl[[i]][[j]]
#       po <- spdf[spdf$id == id2, ]
#       Inter <- rgeos::gIntersection(li, po, byid = T)
#       if (class(Inter) != "SpatialLines") {
#         if (class(Inter) == "SpatialPoints") {
#           rm(Inter)
#         } else{
#           Inter <- Inter@lineobj
#           row.names(Inter) <- paste(id1, id2, sep = "_")
#           wkt[ind] <- rgeos::writeWKT(Inter)
#           df[ind,] <- c(paste(id1, id2, sep = "_"), id1, id2)
#           ind <- ind + 1
#         }
#       } else{
#         row.names(Inter) <- paste(id1, id2, sep = "_")
#         wkt[ind] <- rgeos::writeWKT(Inter)
#         df[ind,] <- c(paste(id1, id2, sep = "_"), id1, id2)
#         ind <- ind + 1
#       }
#     }
#   }
#   wkt <- wkt[!is.na(wkt)]
#   wkt <- paste("GEOMETRYCOLLECTION(",
#                paste(wkt, collapse = ","), ")", sep = "")
#   
#   b <- readWKT(wkt, id = df$id)
#   row.names(df) <- df$id
#   b <- SpatialLinesDataFrame(sl = b,
#                              data = df,
#                              match.ID = T)
#   b2 <- b
#   b2@data <- b2@data[, c(1, 3, 2)]
#   names(b2) <- c("id", "id1", "id2")
#   b2$id <- paste(b2$id1, b2$id2, sep = "_")
#   row.names(b2) <- b2$id
#   borderlines <- rbind(b, b2)
#   borderlines@proj4string <- sldf@proj4string
#   return(borderlines)
# }