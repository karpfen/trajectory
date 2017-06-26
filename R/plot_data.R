#' Plots trajectory and point data as a shiny leaflet map
#'
#' @param traj \code{sf} object containing trajectory data to display
#' @param pts \code{sf} object containing point data to display
#'
#' @export
plot_map <- function (traj, pts)
{
    if (!any (sf::st_geometry_type (traj) == "LINESTRING"))
        stop ("traj must contain geometries of type LINESTRING.")
    if (!any (sf::st_geometry_type (pts) == "POINT"))
        stop ("pts must contain geometries of type POINT.")
    map_data_traj <<- traj
    map_data_pts <<- pts
    shiny::shinyApp (ui, server)
}

map_data_pts <- NULL
map_data_traj <- NULL
ui <- NULL
c_ramp <- subset (RColorBrewer::brewer.pal.info, category == "seq")
ui <- shiny::bootstrapPage (
    shiny::tags$style (type = "text/css", "html,
                       body{width:100%;height:100%;} .checkbox,
                       h4, .control-label, #num_points, #num_trajectories
                       {color:#FFFFFF;} img{border-radius: 5px;}"),
        leaflet::leafletOutput ("map", width = "100%", height = "100%"),
        shiny::absolutePanel (top = 10, right = 10,
        shiny::selectInput ("colorscheme", "Color Scheme",
                        selected = rownames (c_ramp) [1], rownames (c_ramp)),
        shiny::uiOutput ("colors"),
        shiny::uiOutput ("range"),
        shiny::h4 ("Currently Selected:"),
        shiny::textOutput ("num_points"),
        shiny::textOutput ("num_trajectories"),
        shiny::br (),
        shiny::plotOutput ("frequency")
    )
)

server <- function (input, output, session)
{
    cols <- names (map_data_traj)
    cols <- cols [!cols %in% c ("sfc", "user_id")]
    output$colors <- shiny::renderUI ({
        shiny::selectInput ("cols", "Attribute of Interest:", cols)
    })
    shiny::observe ({
        cl <- input$cols
        if (is.null (cl))
            cl <- cols [1]
        rng_min <- map_data_traj [[cl]] %>% min %>% floor 
        rng_max <- map_data_traj [[cl]] %>% max %>% ceiling 
        output$range <- shiny::renderUI ({
            shiny::sliderInput ("rng", "Range", rng_min, rng_max,
                                value = c (rng_min, rng_max))
        })
    })

    ln_color <- function (x, color_by)
    {
        leaflet::colorFactor (x, color_by)
    }

    output$map <- leaflet::renderLeaflet ({
        dat <- map_data_traj
        bb <- as.vector (sf::st_bbox (dat))
        leaflet::leaflet (dat) %>%
        leaflet::addProviderTiles (leaflet::providers$CartoDB.DarkMatter) %>%
        leaflet::fitBounds (bb [1], bb [2], bb [3], bb [4])
    })

    filtered_data <- shiny::reactive ({
        cl <- input$cols
        if (is.null (cl))
            cl <- cols [1]
        map_data_traj [map_data_traj [[cl]] >= input$rng [1] &
                     map_data_traj [[cl]] <= input$rng [2], ]
    })

    shiny::observe ({
        dat_trj <- filtered_data ()
        dat_pts <- filter_points (dat_trj, map_data_pts, "user_id")
        output$num_points <- shiny::renderText (paste ("Points:",
                                                      dim (dat_pts) [1]))
        output$num_trajectories <- shiny::renderText (paste ("Trajectories:",
                                                            dim (dat_trj) [1]))
        if (dim (dat_trj) [1] > 1)
        {
            proxy <- leaflet::leafletProxy ("map", data = dat_trj) %>%
                leaflet::clearShapes () %>% leaflet::clearMarkers ()
            clr_by <- input$cols
            if (is.null (clr_by))
                clr_by <- names (map_data_traj) [1]
            clr_sch <- input$colorscheme
            n_col <- RColorBrewer::brewer.pal.info [clr_sch, ]$maxcolors
            for (i in seq_along (cols))
                dat_trj [cols [i]]  <- dat_trj [[cols [i]]] %>% cut (n_col)
            pal <- ln_color (clr_sch, dat_trj [[clr_by]])
            proxy %>% leaflet::clearControls ()
            pt_col <- RColorBrewer::brewer.pal (3, clr_sch) [1]
            proxy %>%
            leaflet::addPolylines (color = ~pal (dat_trj[[clr_by]]),
                                   group = "Trajectories", opacity = 1.0,
                                   popup = popup ("Trajectory", cols,
                                                  dat_trj [cols])) %>%
            leaflet::addCircleMarkers (stroke = FALSE, group = "Points",
                                       data = dat_pts, color = pt_col,
                                       fillOpacity = 0.7, radius = 5,
                                       popup = popup ("Point", names (dat_pts),
                                                      dat_pts)) %>%
            leaflet::addLegend (position = "bottomleft", pal = pal,
                                values = dat_trj [[clr_by]], title = clr_by) %>%
            leaflet::addLayersControl (overlayGroups = c ("Trajectories",
                                       "Points"),
                                       options = leaflet::layersControlOptions
                                       ( collapsed = FALSE),
                                       position = "bottomleft")
        }
    })

    shiny::observe ({
        dat <- filtered_data ()
        if (dim (dat) [1] == 0)
            return (NULL)
        dat <- dat [[input$cols]]
        bns <- min (length (dat), 7)
        if (bns == 0)
            return (NULL)
        dat <- as.data.frame (dat)
        plt <- ggplot2::ggplot (data = dat, ggplot2::aes (dat)) +
            ggplot2::geom_histogram (bins = bns, col = "black",
                                     ggplot2::aes (fill = ..count..)) +
            ggplot2::xlab (input$cols) +
            ggplot2::ylab ("Count")
        output$frequency <- shiny::renderPlot (plt)
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
        val <- format (pvalues [[att]], digits = 3, nsmall = 2)
        txt %<>% paste0 ("</br><b>", att, ": </b>", val)
    }
    txt
}
