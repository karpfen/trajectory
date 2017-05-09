#' Plots trajectory and point data as a shiny leaflet map
#'
#' @param traj \code{sf} object containing trajectory data to display
#' @param pts \code{sf} object containing point data to display
#'
#' @export
plotMap <- function (traj, pts)
{
    if (!any (sf::st_geometry_type (traj) == "LINESTRING"))
        stop ("traj must contain geometries of type LINESTRING.")
    if (!any (sf::st_geometry_type (pts) == "POINT"))
        stop ("pts must contain geometries of type POINT.")
    mapDataTraj <<- traj
    mapDataPts <<- pts
    shiny::shinyApp (ui, server)
}

mapDataPts <- NULL
mapDataTraj <- NULL
ui <- NULL
cRamp <- subset (RColorBrewer::brewer.pal.info, category == "seq")
ui <- shiny::bootstrapPage (
    shiny::tags$style (type = "text/css", "html,
                       body{width:100%;height:100%} .checkbox,
                       .control-label{color:#FFFFFF}"),
        leaflet::leafletOutput ("map", width = "100%", height = "100%"),
        shiny::absolutePanel (top = 10, right = 10,
        shiny::selectInput ("colorscheme", "Color Scheme",
                        selected = rownames (cRamp) [1], rownames (cRamp)),
        shiny::uiOutput ("colors")
    )
)

server <- function (input, output, session)
{
    cols <- names (mapDataTraj)
    cols <- cols [!cols %in% c ("sfc", "user_id")]
    output$colors <- shiny::renderUI ({
        shiny::selectInput ("cols", "Color by:", cols)
    })

    lnColor <- function (x, colorBy) { leaflet::colorFactor (x, colorBy) } 

    output$map <- leaflet::renderLeaflet ({
        dat <- mapDataTraj
        bb <- as.vector (sf::st_bbox (dat))
        leaflet::leaflet (dat) %>%
        leaflet::addProviderTiles (leaflet::providers$CartoDB.DarkMatter) %>%
        leaflet::fitBounds (bb [1], bb [2], bb [3], bb [4])
    })

    shiny::observe ({
        datTrj <- mapDataTraj
        datPts <- mapDataPts
        proxy <- leaflet::leafletProxy ("map", data=datTrj)
        clrBy <- input$cols
        if (is.null (clrBy))
            clrBy <- names (mapDataTraj) [1]
        clrSch <- input$colorscheme
        nCol <- RColorBrewer::brewer.pal.info [clrSch,]$maxcolors
        for (i in seq_along (cols))
            datTrj [cols [i]]  <- datTrj [[cols [i]]] %>% cut (nCol)
        pal <- lnColor (clrSch, datTrj [[clrBy]])
        proxy %>% leaflet::clearControls ()
        proxy %>%
        leaflet::addPolylines (color = ~pal (datTrj[[clrBy]]),
                               group = "Trajectories") %>%
        leaflet::addCircleMarkers (stroke = FALSE, group = "Points",
                                   data = datPts, color = "#0066FF",
                                   fillOpacity = 0.7, radius = 5) %>%
        leaflet::addLegend (position = "bottomright", pal = pal,
                            values = datTrj [[clrBy]], title = clrBy) %>%
        leaflet::addLayersControl (overlayGroups = c ("Trajectories", "Points"),
                               options = leaflet::layersControlOptions
                               (collapsed = FALSE), position = "bottomright")
    })
}
