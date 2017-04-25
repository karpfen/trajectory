#' Plots trajectory and point data as a shiny leaflet map
#'
#' @param traj \code{sf} object containing trajectory data to display
#' @param pts \code{sf} object containing point data to display
#'
#' @export
plotMap <- function (traj, pts)
{
    mapDataTraj <<- traj
    if (!missing (pts))
        mapDataPts <<- pts
    shiny::shinyApp (ui, server)
}

mapDataPts <- NULL
mapDataTraj <- NULL
ui <- NULL
cRamp <- subset (RColorBrewer::brewer.pal.info, category == "seq")
if (is.null (mapDataPts))
{
    ui <- shiny::bootstrapPage (
        shiny::tags$style (type = "text/css", "html,
                           body{width:100%;height:100%} .checkbox,
                           .control-label{color:#FFFFFF}"),
            leaflet::leafletOutput ("map", width = "100%", height = "100%"),
            shiny::absolutePanel (top = 10, right = 10,
            shiny::checkboxInput ("traj", "Show trajectories", TRUE),
            shiny::selectInput ("colorscheme", "Color Scheme",
                            selected = rownames (cRamp) [1], rownames (cRamp)),
            shiny::uiOutput ("colors")
        )
    )
} else
{
    ui <- shiny::bootstrapPage (
        shiny::tags$style (type = "text/css", "html,
                           body{width:100%;height:100%} .checkbox,
                           .control-label{color:#FFFFFF}"),
            leaflet::leafletOutput ("map", width = "100%", height = "100%"),
            shiny::absolutePanel (top = 10, right = 10,
            shiny::checkboxInput ("traj", "Show trajectories", TRUE),
            shiny::checkboxInput ("pts", "Show points", TRUE),
            shiny::selectInput ("colorscheme", "Color Scheme",
                            selected = rownames (cRamp) [1], rownames (cRamp)),
            shiny::uiOutput ("colors")
        )
    )
}

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
        dat <- mapDataTraj
        proxy <- leaflet::leafletProxy ("map", data=dat)
        clrBy <- input$cols
        clrSch <- input$colorscheme
        if (input$traj & !is.null (clrBy))
        {
            nCol <- RColorBrewer::brewer.pal.info [clrSch,]$maxcolors
            for (i in seq_along (cols))
                dat [cols [i]]  <- dat [[cols [i]]] %>% cut (nCol)
            pal <- lnColor (clrSch, dat [[clrBy]])
            proxy %>% leaflet::clearControls ()
            proxy %>%
            leaflet::clearShapes () %>%
            leaflet::addPolylines (color = ~pal (dat[[clrBy]])) %>%
            leaflet::addLegend (position = "bottomright", pal = pal,
                                values = dat [[clrBy]])
        } else
        {
            proxy %>%
            leaflet::clearShapes () %>%
            leaflet::clearControls ()
        }
    })

    shiny::observe ({
        dat <- mapDataPts
        if (!is.null (dat))
        {
            proxy <- leaflet::leafletProxy ("map", data=dat)
            if (input$pts)
            {
                proxy %>%
                    leaflet::clearShapes () %>%
                    leaflet::addMarkers ()
            } else
            {
                proxy %>%
                    leaflet::clearShapes ()
            }
        }
    })
}
