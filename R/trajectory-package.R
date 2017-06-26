#' trajectory
#'
#' @name trajectory
#' @docType package
#' @importFrom geosphere distVincentyEllipsoid
#' @importFrom ggplot2 ggplot aes geom_histogram
#' @importFrom leaflet leafletOutput colorFactor renderLeaflet leaflet
#' @importFrom leaflet addProviderTiles fitBounds leafletProxy clearControls
#' @importFrom leaflet clearShapes addPolylines addLegend addMarkers
#' @importFrom magrittr extract %>% %<>%
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom sf st_length st_coordinates st_sf st_sfc st_point
#' @importFrom sf st_bbox st_linestring
#' @importFrom shiny shinyApp bootstrapPage tags absolutePanel selectInput h4 br
#' @importFrom shiny uiOutput textOutput plotOutput renderUI observe reactive
#' @importFrom shiny sliderInput renderPlot renderText
#' @importFrom utils head tail read.table
NULL
