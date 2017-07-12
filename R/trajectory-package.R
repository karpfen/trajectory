#' trajectory
#'
#' @name trajectory
#' @docType package
#' @importFrom geosphere distVincentyEllipsoid
#' @importFrom ggplot2 ggplot aes geom_histogram
#' @importFrom magrittr extract %>% %<>%
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom sf st_length st_coordinates st_sf st_sfc st_point
#' @importFrom sf st_bbox st_linestring
#' @importFrom tmap tm_shape tm_fill tm_borders tm_grid tm_text tm_symbols
#' @importFrom tmap tm_lines
#' @importFrom utils head tail read.table
#' @useDynLib trajectory, .registration = TRUE
NULL
