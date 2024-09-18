#' @importFrom magrittr "%>%"
#' @export
run_app <- function(Z, X, cluster, id=NULL) {
  Z_dist <- unname(dist(Z))
  X <- unname(X)

  if (is.null(id)) {id <- 1:nrow(X)}

  tree <- get_mst(Z_dist)

  max_length <- max(igraph::E(tree)$weight)

  plotting_df <- data.frame(x=X[,1], y=X[,2], cluster, id, row=1:nrow(X))
  p <- ggplot2::ggplot(plotting_df, ggplot2::aes(x=x, y=y, color=factor(cluster), label=id, key=row)) +
    ggplot2::geom_point(size=0.3) +
    ggplot2::labs(color="Class")
  medoid_p = plot_medoid_mst(p, plotting_df, Z_dist, tree)

  ui <- bslib::page_navbar(
    title="Dimension Reduction Tool",
    theme=bslib::bs_theme(bootswatch="cosmo"),
    fillable=FALSE,
    bslib::nav_panel(
      title="Default Clusters",

      bslib::layout_sidebar(
        sidebar=bslib::sidebar(
          open="always",
          bslib::accordion(
            multiple=FALSE,
            style="--bs-accordion-btn-bg: #f2f2f2",
            bslib::accordion_panel(
              "Path Selection",
              style="background-color: #f2f2f2",
              shiny::numericInput("from", "From ID", value = 0),
              shiny::numericInput("to", "To ID", value = 0)
            ),
            bslib::accordion_panel(
              "Path Projection Settings",
              style="background-color: #f2f2f2",
              shiny::numericInput("dim", "Dimension", min=2, max=dim(Z)[2], value=2, step=1),
              shiny::sliderInput("degree", "CCA Degree", min=2, max=10, value=2, step=1),
              shiny::sliderInput("adjust", "Bandwidth Adjustment", min=0, max=5, value = 0, step = .05)
            )
          ),
          shiny::radioButtons("med_subtree1",
                       label = "Show medoid subtree?",
                       choices = c("Hide", "Show"),
                       inline = TRUE),
          shiny::uiOutput("slider")
        ),

        bslib::card(
          bslib::card_header("Low-Dimensional Embedding"),
          plotly::plotlyOutput("lowDimPlot")
        ),

        bslib::navset_card_underline(
          title="Analytical Plots",
          bslib::nav_panel("2D Path Projection", plotly::plotlyOutput("projPath")),
          bslib::nav_panel("Path Weights", shiny::plotOutput("pathWeights"))
        )
      )
    ),

    bslib::nav_panel(
      title="Custom Clusters",
      bslib::layout_sidebar(
        sidebar=bslib::sidebar(
          open="always",
          bslib::accordion(
            multiple=FALSE,
            style="--bs-accordion-btn-bg: #f2f2f2",
            bslib::accordion_panel(
              "Group Selection",
              style="background-color: #f2f2f2",
              shiny::actionButton("group1", "Submit Group 1",
                           style="color: black;
                                 background-color: white;
                                 border-color: #dee2e6;
                                 margin: 4px 0px"),
              shiny::actionButton("group2", "Submit Group 2",
                           style="color: black;
                                 background-color: white;
                                 border-color: #dee2e6;
                                 margin: 4px 0px"),
              shiny::actionButton("clear_brush", "Clear Groups",
                           style="color: black;
                                 background-color: white;
                                 border-color: #dee2e6;
                                 margin: 4px 0px"),
              shiny::numericInput("from_brush", "From ID", value = 0),
              shiny::numericInput("to_brush", "To ID", value = 0)
            ),
            bslib::accordion_panel(
              "Path Projection Settings",
              style="background-color: #f2f2f2",
              shiny::numericInput("dim_brush", "Dimension", min=2, max=dim(Z)[2], value=2, step=1),
              shiny::sliderInput("degree_brush", "CCA Degree", min=2, max=10, value=2, step=1),
              shiny::sliderInput("adjust_brush", "Bandwidth Adjustment", min=0, max=5, value = 0, step = .05),
              shiny::radioButtons("path_color_brush",
                           label="Path Projection Coloring",
                           choices=c("Original Coloring", "Group Coloring"),
                           selected="Original Coloring")
            )
          ),
          shiny::radioButtons("med_subtree2",
                       label = "Show medoid subtree?",
                       choices = c("Hide", "Show"),
                       inline = TRUE),
          shiny::uiOutput("slider_brush")
        ),

        bslib::card(
          bslib::card_header("Low-Dimensional Embedding"),
          plotly::plotlyOutput("lowDimPlot_brush")
        ),

        bslib::navset_card_underline(
          title="Analytical Plots",
          bslib::nav_panel("2D Path Projection", plotly::plotlyOutput("projPath_brush")),
          bslib::nav_panel("Path Weights", shiny::plotOutput("pathWeights_brush"))
        )
      )
    )
  )

  server <- function(input, output) {
    shortest_path <- shiny::reactive({
      sp <- tryCatch({
        get_shortest_path(tree, which(id == input$from), which(id == input$to))
      }, error = function(err) {
        NULL
      })

      sp
    })

    output$slider <- shiny::renderUI({
      max <- ifelse(is.null(shortest_path()),
                   0,
                   length(shortest_path()$vpath) - 1)

      shiny::sliderInput("slider",
                         "Path component",
                         min = 0,
                         max = max,
                         value = 0,
                         step = 1)
    })

    output$lowDimPlot <- plotly::renderPlotly({
      if (input$med_subtree1 == "Show") {
        plotly::ggplotly(medoid_p,
                         tooltip = c("x", "y", "label")) %>%
          plotly::layout(dragmode='pan')
      }
      else {
        if (is.null(shortest_path())) {
          plotly::ggplotly(p,
                           tooltip = c("x", "y", "label")) %>%
            plotly::layout(dragmode='pan')
        }
        else {
          plotly::ggplotly(add_path(p, plotting_df, shortest_path(), input$slider),
                           tooltip = c("x", "y", "label")) %>%
            plotly::layout(dragmode='pan')
        }
      }
    })

    output$projPath <- plotly::renderPlotly({
      if (is.null(shortest_path())) {
        return(plotly::plotly_empty(type="scatter", mode="markers"))
      }

      ret <- plot_2d_projection(Z, shortest_path(), cluster, id, input$dim, input$degree, input$slider, input$adjust)

      plotly::ggplotly(ret$p,
                       tooltip = c("x", "y", "label")) %>%
        plotly::layout(dragmode='pan') %>%
        plotly::add_annotations(text=paste(round(ret$var_explained, 2)),
                                xref='paper', yref='paper',
                                x=1, y=1,
                                showarrow = FALSE) %>%
        plotly::layout(showlegend = FALSE)
    })

    output$pathWeights <- shiny::renderPlot({
      if (is.null(shortest_path())) {
        return(plotly::plotly_empty(type="bar"))
      }

      plot_path_weights(shortest_path(), input$slider, max_length)
    })

    #######################

    shortest_path_brush <- shiny::reactive({
      sp <- tryCatch({
        get_shortest_path(tree,
                          which(id == input$from_brush),
                          which(id == input$to_brush))
      }, error = function(err) {
        return(NULL)
      })

      sp
    })

    rv <- shiny::reactiveValues(g1 = NULL, g2 = NULL)

    shiny::observeEvent(input$group1, {
      d <- plotly::event_data("plotly_selecting")
      rv$g1 <- as.numeric(d$key)

      if (length(rv$g1) > 0) {
        shiny::updateNumericInput(inputId="from_brush", value=id[get_medoid(Z_dist, rv$g1)])
      } else rv$g1 <- NULL
    })

    shiny::observeEvent(input$group2, {
      d <- plotly::event_data("plotly_selecting")
      rv$g2 <- as.numeric(d$key)

      if (length(rv$g2) > 0) {
        updateNumericInput(inputId="to_brush", value=id[get_medoid(Z_dist, rv$g2)])
      } else rv$g2 <- NULL
    })

    shiny::observeEvent(input$clear_brush, {
      rv$g1 <- NULL
      rv$g2 <- NULL

      shiny::updateNumericInput(inputId="from_brush", value=0)
      shiny::updateNumericInput(inputId="to_brush", value=0)
    })

    output$slider_brush <- shiny::renderUI({
      max <- ifelse(is.null(shortest_path_brush()),
                   0,
                   length(shortest_path_brush()$vpath) - 1)

      shiny::sliderInput("slider_brush",
                         "Path component",
                         min=0,
                         max=max,
                         value=0,
                         step=1)
    })

    output$lowDimPlot_brush <- plotly::renderPlotly({
      if (input$med_subtree2 == "Show") {
        plotly::ggplotly(medoid_p,
                         tooltip = c("x", "y", "label")) %>%
          plotly::layout(dragmode='pan')
      }
      else {
        if (is.null(shortest_path_brush())) {
          alpha_id <- unique(c(rv$g1, rv$g2))
          if (!is.null(alpha_id)) {
            alpha <- rep(0.3, nrow(X))
            alpha[alpha_id] <- 1
          }
          else {
            alpha <- rep(1, nrow(X))
          }

          p_brush <- ggplot2::ggplot(plotting_df, ggplot2::aes(x=x, y=y, color=factor(cluster), label=id, key=row)) +
            ggplot2::geom_point(size=0.3, alpha=alpha) +
            ggplot2::labs(color="Class")

          plotly::ggplotly(p_brush,
                           tooltip = c("x", "y", "label")) %>%
            plotly::layout(dragmode='lasso') %>%
            plotly::event_register("plotly_selecting")
        }
        else {
          alpha_id = unique(c(rv$g1, rv$g2))
          if (!is.null(alpha_id)) {
            alpha = rep(0.3, nrow(X))
            alpha[alpha_id] = 1
          }
          else {
            alpha <- rep(1, nrow(X))
          }

          p_brush <- ggplot2::ggplot(plotting_df, ggplot2::aes(x=x, y=y, color=factor(cluster), label=id, key=row)) +
            ggplot2::geom_point(size=0.5, alpha=alpha) +
            ggplot2::labs(color="Class")

          plotly::ggplotly(add_path(p_brush, plotting_df, shortest_path_brush(), input$slider_brush),
                           tooltip = c("x", "y", "label")) %>%
            plotly::layout(dragmode='lasso') %>%
            plotly::event_register("plotly_selecting")
        }
      }
    })

    output$projPath_brush <- plotly::renderPlotly({
      if (is.null(shortest_path_brush())) {
        return(plotly::plotly_empty(type="scatter", mode="markers"))
      }

      ret <- plot_2d_projection_brush(Z, shortest_path_brush(), rv$g1, rv$g2,
                                      cluster, id, input$dim_brush, input$degree_brush,
                                      input$slider_brush, input$adjust_brush,
                                      input$path_color_brush)

      plotly::ggplotly(ret$p,
                       tooltip = c("x", "y", "label")) %>%
        plotly::layout(dragmode='pan') %>%
        plotly::add_annotations(text=paste(round(ret$var_explained, 2)),
                                xref='paper', yref='paper',
                                x=1, y=1,
                                showarrow = FALSE) %>%
        {if (input$path_color_brush == "Original Coloring") plotly::layout(., showlegend = FALSE) else .}
    })

    output$pathWeights_brush <- shiny::renderPlot({
      if (is.null(shortest_path_brush())) {
        return(plotly::plotly_empty())
      }

      plot_path_weights(shortest_path_brush(), input$slider_brush, max_length)
    })
  }

  shiny::shinyApp(ui=ui, server=server)
}
