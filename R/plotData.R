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

mapDataPts <- ""
mapDataTraj <- ""

ui <- shiny::bootstrapPage (
    shiny::tags$style (type = "text/css", "html, body{width:100%;height:100%}"),
        leaflet::leafletOutput ("map", width = "100%", height = "100%"),
        shiny::absolutePanel (top = 10, right = 10,
        shiny::checkboxInput ("traj", "Show trajectories", TRUE),
        shiny::checkboxInput ("pts", "Show points", TRUE)
    )
)

server <- function (input, output, session)
{
    output$map <- leaflet::renderLeaflet ({
        dat <- mapDataTraj
        bb <- as.vector (sf::st_bbox (dat))
        leaflet::leaflet (dat) %>%
        leaflet::addTiles () %>%
        leaflet::fitBounds (bb [1], bb [2], bb [3], bb [4])
    })

    shiny::observe ({
        dat <- mapDataTraj
        proxy <- leaflet::leafletProxy ("map", data=dat)
        if (input$traj)
        {
            proxy %>%
            leaflet::clearShapes () %>%
            leaflet::addPolylines ()
        } else
        {
            proxy %>%
            leaflet::clearShapes ()
        }
    })

    shiny::observe ({
        dat <- mapDataPts
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
    })
}
