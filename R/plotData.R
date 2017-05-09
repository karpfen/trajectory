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
        shiny::uiOutput ("colors"),
        shiny::uiOutput ("range")
    )
)

server <- function (input, output, session)
{
    cols <- names (mapDataTraj)
    cols <- cols [!cols %in% c ("sfc", "user_id")]
    output$colors <- shiny::renderUI ({
        shiny::selectInput ("cols", "Attribute of Interest:", cols)
    })
    shiny::observe ({
        cl <- input$cols
        if (is.null (cl))
            cl <- cols [1]
        rngMin <- min (mapDataTraj [[cl]])
        rngMax <- max (mapDataTraj [[cl]])
        output$range <- shiny::renderUI ({
            shiny::sliderInput ("rng", "Range", rngMin, rngMax,
                                value = c (rngMin, rngMax))
        })
    })

    lnColor <- function (x, colorBy) { leaflet::colorFactor (x, colorBy) } 

    output$map <- leaflet::renderLeaflet ({
        dat <- mapDataTraj
        bb <- as.vector (sf::st_bbox (dat))
        leaflet::leaflet (dat) %>%
        leaflet::addProviderTiles (leaflet::providers$CartoDB.DarkMatter) %>%
        leaflet::fitBounds (bb [1], bb [2], bb [3], bb [4])
    })

    filtered <- shiny::reactive ({
        cl <- input$cols
        if (is.null (cl))
            cl <- cols [1]
        mapDataTraj [mapDataTraj [[cl]] >= input$rng [1] &
                     mapDataTraj [[cl]] <= input$rng [2], ]
    })

    shiny::observe ({
        datTrj <- filtered ()
        datPts <- filterPoints (datTrj, mapDataPts, "user_id")
        if (dim (datTrj) [1] > 1)
        {
            proxy <- leaflet::leafletProxy ("map", data=datTrj) %>%
                leaflet::clearShapes () %>% leaflet::clearMarkers ()
            clrBy <- input$cols
            if (is.null (clrBy))
                clrBy <- names (mapDataTraj) [1]
            clrSch <- input$colorscheme
            nCol <- RColorBrewer::brewer.pal.info [clrSch,]$maxcolors
            for (i in seq_along (cols))
                datTrj [cols [i]]  <- datTrj [[cols [i]]] %>% cut (nCol)
            pal <- lnColor (clrSch, datTrj [[clrBy]])
            proxy %>% leaflet::clearControls ()
            ptCol <- RColorBrewer::brewer.pal (3, clrSch) [1]
            proxy %>%
            leaflet::addPolylines (color = ~pal (datTrj[[clrBy]]),
                                   group = "Trajectories", opacity = 1.0,
                                   popup = popup ("Trajectory", cols,
                                                  datTrj [cols])) %>%
            leaflet::addCircleMarkers (stroke = FALSE, group = "Points",
                                       data = datPts, color = ptCol,
                                       fillOpacity = 0.7, radius = 5,
                                       popup = popup ("Point", names (datPts),
                                                      datPts)) %>%
            leaflet::addLegend (position = "bottomright", pal = pal,
                                values = datTrj [[clrBy]], title = clrBy) %>%
            leaflet::addLayersControl (overlayGroups = c ("Trajectories",
                                       "Points"), options =
                                       leaflet::layersControlOptions (collapsed=
                                       FALSE), position = "bottomright")
        }
    })
}

#' Generates text for trajectory popup fields on the graph
#'
#' @param ptitle The popup title.
#' @param pnames \code{vector} containing attribute names.
#' @param pvalues \code{vector} containing attribute values.
#'
#' @noRd
popup <- function (ptitle, pnames, pvalues)
{
    txt <- paste0 ("<b>", ptitle, "</b>")
    for (i in seq_along (pnames))
    {
        att <- pnames [i]
        val <- pvalues [[att]]
        txt %<>% paste0 ("</br><b>", att, ": </b>", val)
    }
    txt
}
