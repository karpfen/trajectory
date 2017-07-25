#' Plots trajectory and point data as a shiny leaflet map
#'
#' @param traj \code{sf} object containing trajectory data to display.
#' @param pts \code{sf} object containing point data to display.
#'
#' @export
plot_leaflet <- function (traj, pts)
{
    if (!any (sf::st_geometry_type (traj) == "LINESTRING"))
        stop ("traj must contain geometries of type LINESTRING.")
    if (!any (sf::st_geometry_type (pts) == "POINT"))
        stop ("pts must contain geometries of type POINT.")
    map_data_traj <<- traj
    map_data_pts <<- pts
    shiny::shinyApp (ui_dyn, server_dyn)
}

map_data_pts <- NULL
map_data_traj <- NULL
ui_dyn <- NULL
c_ramp <- subset (RColorBrewer::brewer.pal.info, category == "seq")
ui_dyn <- shiny::bootstrapPage (
    shiny::tags$style (type = "text/css", "html,
                       body{width:100%;height:100%;} .checkbox,
                       h4, .control-label, #num_pts, #num_trj
                       {color:#FFFFFF;} img{border-radius: 5px;}"),
        leaflet::leafletOutput ("map", width = "100%", height = "100%"),
        shiny::absolutePanel (top = 10, right = 10,
        shiny::selectInput ("colorscheme", "Color Scheme",
                        selected = rownames (c_ramp) [1], rownames (c_ramp)),
        shiny::uiOutput ("colors"),
        shiny::uiOutput ("range"),
        shiny::h4 ("Currently Selected:"),
        shiny::textOutput ("num_pts"),
        shiny::textOutput ("num_trj"),
        shiny::br (),
        shiny::plotOutput ("frequency")
    )
)

server_dyn <- function (input, output, session)
{
    cols <- names (map_data_traj)
    numeric_fields <- sapply (map_data_traj, is.numeric)
    cols %<>% magrittr::extract (numeric_fields)
    output$colors <- shiny::renderUI ({
        shiny::selectInput ("cols", "Attribute of Interest:", cols)
    })
    shiny::observe ({
        cl <- input$cols
        if (is.null (cl))
            cl <- cols [1]
        rngMin <- map_data_traj [[cl]] %>% min %>% floor 
        rngMax <- map_data_traj [[cl]] %>% max %>% ceiling 
        output$range <- shiny::renderUI ({
            shiny::sliderInput ("rng", "Range", rngMin, rngMax,
                                value = c (rngMin, rngMax))
        })
    })

    ln_color <- function (x, color_by) { leaflet::colorFactor (x, color_by) }

    output$map <- leaflet::renderLeaflet ({
        dat <- map_data_traj
        bb <- as.vector (sf::st_bbox (dat))
        leaflet::leaflet (dat) %>%
        leaflet::addProviderTiles (leaflet::providers$CartoDB.DarkMatter) %>%
        leaflet::fitBounds (bb [1], bb [2], bb [3], bb [4])
    })

    dat_filtered <- shiny::reactive ({
        cl <- input$cols
        if (is.null (cl))
            cl <- cols [1]
        map_data_traj [map_data_traj [[cl]] >= input$rng [1] &
                     map_data_traj [[cl]] <= input$rng [2], ]
    })

    shiny::observe ({
        dat_traj <- dat_filtered ()
        dat_pts <- map_data_pts
        output$num_pts <- shiny::renderText (paste ("Points:",
                                                      dim (dat_pts) [1]))
        output$num_trj <- shiny::renderText (paste ("Trajectories:",
                                                            dim (dat_traj) [1]))
        if (dim (dat_traj) [1] > 1)
        {
            proxy <- leaflet::leafletProxy ("map", data = dat_traj) %>%
                leaflet::clearShapes () %>% leaflet::clearMarkers ()
            clrBy <- input$cols
            if (is.null (clrBy))
                clrBy <- names (map_data_traj) [1]
            col_scheme <- input$colorscheme
            nCol <- RColorBrewer::brewer.pal.info [col_scheme,]$maxcolors
            for (i in seq_along (cols))
                dat_traj [cols [i]]  <- dat_traj [[cols [i]]] %>% cut (nCol)
            pal <- ln_color (col_scheme, dat_traj [[clrBy]])
            proxy %>% leaflet::clearControls ()
            ptCol <- RColorBrewer::brewer.pal (3, col_scheme) [1]
            proxy %>%
            leaflet::addPolylines (color = ~pal (dat_traj[[clrBy]]), weight = 3,
                                   group = "Trajectories", opacity = 1.0,
                                   popup = popup ("Trajectory", cols,
                                                  dat_traj [cols])) %>%
            leaflet::addCircleMarkers (stroke = FALSE, group = "Points",
                                       data = dat_pts, color = ptCol,
                                       fillOpacity = 0.7, radius = 5,
                                       popup = popup ("Point", names (dat_pts),
                                                      dat_pts)) %>%
            leaflet::addLegend (position = "bottomleft", pal = pal,
                                values = dat_traj [[clrBy]], title = clrBy) %>%
            leaflet::addScaleBar (position = "bottomright", options =
                leaflet::scaleBarOptions(imperial = FALSE)) %>%
            leaflet::addLayersControl (overlayGroups = c ("Trajectories",
                                       "Points"), options =
                                       leaflet::layersControlOptions (collapsed=
                                       FALSE), position = "bottomleft")
        }
    })

    shiny::observe ({
        dat <- dat_filtered ()
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
        # output$frequency <- shiny::renderPlot (plt)
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
        val <- format (pvalues [[i]], digits = 3, nsmall = 2)
        txt %<>% paste0 ("</br><b>", att, ": </b>", val)
    }
    txt
}
