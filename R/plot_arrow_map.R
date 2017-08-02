#' Plot trajectory data as a shiny leaflet arrow map
#'
#' @param traj \code{sf} object containing trajectory data to display.
#'
#' @export
plot_arrow_map <- function (traj)
{
    if (!any (sf::st_geometry_type (traj) == "LINESTRING"))
        stop ("traj must contain geometries of type LINESTRING.")
    map_data_arrows <<- line_to_point (traj)
    shiny::shinyApp (ui_arrow, server_arrow)
}

map_data_arrows <- NULL
ui_arrow <- NULL
ui_arrow <- shiny::bootstrapPage (
    shiny::tags$style (type = "text/css", "html,
                       body{width:100%;height:100%;}"),
        leaflet::leafletOutput ("map", width = "100%", height = "100%")
)

server_arrow <- function (input, output, session)
{
    output$map <- leaflet::renderLeaflet ({
        dat <- map_data_arrows
        bb <- as.vector (sf::st_bbox (dat))
        leaflet::leaflet () %>%
        leaflet::addProviderTiles (leaflet::providers$CartoDB.DarkMatter) %>%
        leaflet::addMarkers (data = dat,
                             popup = popup ("Movement", names (dat), dat)) %>%
        leaflet::addScaleBar (position = "bottomright", options =
            leaflet::scaleBarOptions(imperial = FALSE)) %>%
        leaflet::fitBounds (bb [1], bb [2], bb [3], bb [4])
    })
}

#' Take a simple feature collection of lines and replaces the line geometry
#' either with the start or end point of the lines
#'
#' @param traj \code{sf} object containing trajectory data to display.
#' @param use_start_pt If \code{TRUE}, uses the start point of the trajectories
#' as point geometries. Otherwise, it uses the end point.
#'
#' @noRd
line_to_point <- function (traj, use_start_pt = TRUE)
{
    pt_coords <- list ("POINT", length = dim (traj) [1])
    for (i in seq_len (dim (traj) [1]))
    {
        tr <- traj [i, ]
        coords <- sf::st_coordinates (tr)
        idx <- 1
        if (!use_start_pt)
            idx <- dim (coords) [1]
        coord <- coords [idx, 1:2] %>% as.numeric
        pt_coords [[i]] <- sf::st_point (coord, "XY")
    }
    sfc <- sf::st_sfc (pt_coords, crs = 4326)
    traj %<>% as.data.frame
    traj$sfc <- NULL
    traj <- sf::st_sf (sfc, traj)
}
