#' Launch the dimension reduction tool
#'
#' Launches the dimension reduction tool. The required inputs are the
#' high-dimensional data matrix, the low-dimensional data matrix, and the clustering.
#' Optionally, a vector of ids can be given to annotate the points.
#'
#' @param Z A numerical matrix. The high-dimensional data.
#' @param X A numerical matrix satisfying `nrow(X) = nrow(Z)` and
#' `ncol(X) = 2`.
#' @param cluster A numerical vector or factor of length `nrow(Z)`.
#' @param Z_dist A `dist` object or distance matrix. The distance matrix for the
#' high-dimensional data. By default, a distance matrix calculating using
#' Euclidean distance. Used when computing the minimum spanning tree.
#' @param id A vector of length `nrow(Z)`. If `id = NULL`, the ids will be set
#' to the indices of the points.
#' @param meta_data A data frame with a number of rows equal to `nrow(Z)`. For
#' presenting extra meta data.
#' @param col_names A vector of length `nrow(Z)`. The column names will be used
#' when viewing sub-heatmaps. If `col_names = NULL`, the column names will be
#' pulled from `Z`.
#' @param parallel A Boolean indicating whether parallel computing should be
#' used. The implementation uses [parallel::mclapply()], which is not available
#' on Windows.
#'
#' @returns None
#'
#' @examples
#' library(MASS)
#' library(Rtsne)
#'
#' # construct high-dimensional data, three clusters
#' Z1 <- mvrnorm(n=30, mu=c(0,0,0,0,0), Sigma=diag(rep(1,5)))
#' Z2 <- mvrnorm(n=30, mu=c(0,2,0,0,0), Sigma=diag(rep(1,5)))
#' Z3 <- mvrnorm(n=30, mu=c(0,0,2,0,0), Sigma=diag(rep(1,5)))
#' Z <- rbind(Z1, Z2, Z3)
#'
#' # compute low-dimensional embedding using t-SNE
#' X <- Rtsne(Z, dims=2, perplexity=20)$Y
#'
#' # compute k-means clustering
#' cluster <- kmeans(Z, centers=3, nstart=10)$cluster
#'
#' # launch tool
#' # parallel computing is not available on Windows
#' run_app(Z, X, cluster, parallel=TRUE)
#' @importFrom magrittr "%>%"
#' @export
run_app <- function(Z, X, cluster, Z_dist=dist(Z), id=NULL, meta_data=NULL, col_names=NULL, parallel=FALSE) {
  if (all(class(Z) != "matrix") | all(class(X) != "matrix")) {
    stop("Z and X must be matrices.")
  }
  if (nrow(Z) != nrow(X)) {
    stop("Z and X must have an equal number of rows.")
  }
  if (length(cluster) != nrow(Z)) {
    stop("The length of cluster must be equal to the number of rows of Z and X.")
  }
  if (!is.null(id) && length(id) != nrow(Z)) {
    stop("The length of id must be equal to the number of rows of Z and X.")
  }
  if (!is.null(col_names) && length(col_names) != ncol(Z)) {
    stop("The length of col_names must be equal to the number of columns of Z.")
  }
  if (!is.null(meta_data) && nrow(meta_data) != nrow(Z)) {
    stop("meta_data must have the same number of rows as Z.")
  }

  col_names <- if(is.null(col_names)) colnames(Z) else col_names
  Z <- unname(Z)
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
    navbar_options = list(class="bg-primary", theme="dark"),
    fillable=FALSE,
    bslib::nav_panel(
      title="Default Clusters",

      bslib::layout_sidebar(
        sidebar=bslib::sidebar(
          open="always",

          shiny::numericInput("from", "From ID", min=0, value = 0),
          shiny::numericInput("to", "To ID", min=0, value = 0),

          bslib::accordion(
            open=FALSE,
            multiple=FALSE,
            style="--bs-accordion-btn-bg: #f2f2f2",
            bslib::accordion_panel(
              "Path Projection Settings",
              style="background-color: #f2f2f2",
              shiny::numericInput("dim", "Dimension", min=2, max=dim(Z)[2], value=2, step=1),
              shiny::sliderInput("degree", "CCA Degree", min=2, max=10, value=2, step=1),
              shiny::sliderInput("adjust", "Bandwidth Adjustment", min=0, max=5, value=0, step = .05),
              shiny::radioButtons("show_all_edges",
                                  label = "Show all MST edges?",
                                  choices = c("Hide", "Show"),
                                  inline = TRUE)
            ),
            bslib::accordion_panel(
              "MST Test Settings",
              style="background-color: #f2f2f2",
              shiny::numericInput("num_sim", "Number of Simulations", min=50, 500, value=100, step=50),
              shiny::actionButton("bootstrap", "Run Test",
                                  style="color: black;
                               background-color: white;
                               border-color: #dee2e6;
                               margin: 4px 0px")
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
          plotly::plotlyOutput("lowDimPlot", width=800, height=400)
        ),

        bslib::navset_card_underline(
          title="Analytical Plots",
          id="analytical_plots",
          bslib::nav_panel("2D Path Projection", plotly::plotlyOutput("projPath", width=800, height=400)),
          bslib::nav_panel("MST Test", shiny::verbatimTextOutput("MSTtest")),
          bslib::nav_panel("Heatmap", InteractiveComplexHeatmap::InteractiveComplexHeatmapOutput("heatmap")),
          bslib::nav_panel("Meta Data",
                           shiny::uiOutput("metaDataChoice"),
                           shiny::plotOutput("metaData", width=800, height=400))
        ),
      )
    ),

    ###########################################################################

    bslib::nav_panel(
      title="Custom Clusters",
      bslib::layout_sidebar(
        sidebar=bslib::sidebar(
          open="always",

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
          shiny::numericInput("to_brush", "To ID", value = 0),

          bslib::accordion(
            open=FALSE,
            multiple=FALSE,
            style="--bs-accordion-btn-bg: #f2f2f2",
            bslib::accordion_panel(
              "Path Projection Settings",
              style="background-color: #f2f2f2",
              shiny::numericInput("dim_brush", "Dimension", min=2, max=dim(Z)[2], value=2, step=1),
              shiny::sliderInput("degree_brush", "CCA Degree", min=2, max=10, value=2, step=1),
              shiny::sliderInput("adjust_brush", "Bandwidth Adjustment", min=0, max=5, value = 0, step = .05),
              shiny::radioButtons("show_all_edges_brush",
                                  label = "Show all MST edges?",
                                  choices = c("Hide", "Show"),
                                  inline = TRUE),
              shiny::radioButtons("path_color_brush",
                           label="Path Projection Coloring",
                           choices=c("Original Coloring", "Group Coloring"),
                           selected="Original Coloring")
            ),
            bslib::accordion_panel(
              "MST Test Settings",
              style="background-color: #f2f2f2",
              shiny::numericInput("num_sim_brush", "Number of Simulations", min=50, 500, value=100, step=50),
              shiny::actionButton("bootstrap_brush", "Run Test",
                           style="color: black;
                                 background-color: white;
                                 border-color: #dee2e6;
                                 margin: 4px 0px")
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
          plotly::plotlyOutput("lowDimPlot_brush", width=800, height=400)
        ),

        bslib::navset_card_underline(
          title="Analytical Plots",
          id="analytical_plots_brush",
          bslib::nav_panel("2D Path Projection", plotly::plotlyOutput("projPath_brush", width=800, height=400)),
          bslib::nav_panel("MST Test", shiny::verbatimTextOutput("MSTtest_brush")),
          bslib::nav_panel("Heatmap", InteractiveComplexHeatmap::InteractiveComplexHeatmapOutput("heatmap_brush")),
          bslib::nav_panel("Meta Data",
                           shiny::uiOutput("metaDataButton_brush"),
                           shiny::plotOutput("metaData_brush", width=800, height=400))
        )
      )
    )
  )

  server <- function(input, output, session) {

    mst_test_vals <- shiny::reactiveValues(sim_crossings=NULL, num_crossings=NULL, endpts=NULL)

    shortest_path <- shiny::reactive({
      if(!shiny::isTruthy(input$from) | !(input$from %in% id)) return(NULL)
      if(!shiny::isTruthy(input$to) | !(input$to %in% id)) return(NULL)
      if (input$from == input$to) return(NULL)

      get_shortest_path(tree, which(id == input$from), which(id == input$to))
    })

    projected_pts <- shiny::reactive({
      if (is.null(shortest_path())) NULL else get_projection(Z, shortest_path(), cluster, input$dim, input$degree)
    })

    ht <- shiny::reactive({
      if (is.null(shortest_path())) NULL else plot_heatmap(Z, shortest_path(), cluster, col_names)
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
      if (is.null(projected_pts())) {
        return(plotly::plotly_empty(type="scatter", mode="markers"))
      }

      ret <- plot_2d_projection(tree, cluster, id, projected_pts()$projected_pts,
                                projected_pts()$ids, projected_pts()$path_ids,
                                projected_pts()$var_explained, input$degree,
                                input$slider, input$adjust, input$show_all_edges)

      plotly::ggplotly(ret$p,
                       tooltip = c("x", "y", "label")) %>%
        plotly::layout(dragmode='pan') %>%
        plotly::add_annotations(text=paste(round(ret$var_explained, 2)),
                                xref='paper', yref='paper',
                                x=1, y=1,
                                showarrow = FALSE) %>%
        plotly::layout(showlegend = FALSE)
    })

    shiny::observeEvent(input$bootstrap, {
      if (is.null(shortest_path())) {
        mst_test_vals$sim_crossings <- NULL
        mst_test_vals$num_crossings <- NULL
        mst_test_vals$endpts <- NULL
      } else {
        mst_test_vals$endpts <- get_path_endpts(shortest_path(), cluster, id)

        if (!mst_test_vals$endpts$same) {
          mst_test_vals$sim_crossings <- sim_crossings(Z, shortest_path(), cluster, input$num_sim, parallel=parallel)
          mst_test_vals$num_crossings <- count_crossings(tree, shortest_path(), cluster)
        }
      }
    })

    output$MSTtest <- shiny::renderPrint({
      if (is.null(mst_test_vals$endpts)) "Select path and click Run Test!"
      else if (mst_test_vals$endpts$from != input$from | mst_test_vals$endpts$to != input$to) "Path has changed. Please re-run the test!"
      else if (mst_test_vals$endpts$same) "To run the MST test, the path must connect samples from different classes!"
      else {
        mst_test(mst_test_vals$sim_crossings, mst_test_vals$num_crossings, mst_test_vals$endpts)
      }
    })

    shiny::observe({
      if (!is.null(ht())) {
        suppressMessages(InteractiveComplexHeatmap::makeInteractiveComplexHeatmap(input, output, session, ht(), "heatmap"))
      }
    })

    if (is.null(meta_data)) bslib::nav_remove("analytical_plots", "Meta Data")

    output$metaDataChoice <- shiny::renderUI({
      choices <- colnames(meta_data)

      shiny::radioButtons("metaDataChoice",
                          label = "Which feature?",
                          choices = choices,
                          inline = TRUE)
    })

    output$metaData <- shiny::renderPlot({
      if (is.null(projected_pts())) NULL else meta_data_plot(Z, shortest_path(), cluster, meta_data, input$metaDataChoice)
    })

    ###########################################################################

    shortest_path_brush <- shiny::reactive({
      if(!shiny::isTruthy(input$from_brush) | !(input$from_brush %in% id)) return(NULL)
      if(!shiny::isTruthy(input$to_brush) | !(input$to_brush %in% id)) return(NULL)
      if (input$from_brush == input$to_brush) return(NULL)

      get_shortest_path(tree, which(id == input$from_brush), which(id == input$to_brush))
    })

    rv <- shiny::reactiveValues(g1 = NULL, g2 = NULL)

    mst_test_vals_brush <- shiny::reactiveValues(g1=NULL, g2=NULL, sim_crossings=NULL, num_crossings=NULL)

    projected_pts_brush <- shiny::reactive({
      if (is.null(rv$g1) | is.null(rv$g2) | is.null(shortest_path_brush())) NULL else get_projection_brush(Z, shortest_path_brush(), rv$g1, rv$g2, cluster, input$dim_brush, input$degree_brush)
    })

    ht_brush <- shiny::reactive({
      if (is.null(rv$g1) | is.null(rv$g2) | is.null(shortest_path_brush())) NULL else suppressMessages(plot_heatmap_brush(Z, rv$g1, rv$g2, col_names))
    })

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
        shiny::updateNumericInput(inputId="to_brush", value=id[get_medoid(Z_dist, rv$g2)])
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
          alpha_id <- unique(c(rv$g1, rv$g2))
          if (!is.null(alpha_id)) {
            alpha <- rep(0.3, nrow(X))
            alpha[alpha_id] <- 1
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
      if (is.null(rv$g1) | is.null(rv$g2) | is.null(shortest_path_brush())) {
        return(plotly::plotly_empty(type="scatter", mode="markers"))
      }

      ret <- plot_2d_projection_brush(tree, cluster, id, rv$g1, rv$g2, projected_pts_brush()$projected_pts,
                                      projected_pts_brush()$ids, projected_pts_brush()$path_ids,
                                      projected_pts_brush()$var_explained, input$degree_brush,
                                      input$slider_brush, input$adjust_brush, input$show_all_edges_brush,
                                      input$path_color_brush)

      q = plotly::ggplotly(ret$p, tooltip = c("x", "y", "label"))

      # edit legend after conversion to plotly because ggplotly changes legend
      for (i in 1:length(q$x$data)) {
        if (q$x$data[[i]]$mode == "markers") {
          q$x$data[[i]]$name = stringr::str_extract(q$x$data[[i]]$name, "(?<=\\().+(?=(,1\\)))")
        }
        else if (q$x$data[[i]]$mode == "lines") {
          q$x$data[[i]]$showlegend = FALSE
        }
      }

      q %>%
        plotly::layout(dragmode='pan') %>%
        plotly::add_annotations(text=paste(round(ret$var_explained, 2)),
                                xref='paper', yref='paper',
                                x=1, y=1,
                                showarrow = FALSE) %>%
        plotly:: layout(legend=list(title=list(text="Group"))) %>%
        {if (input$path_color_brush == "Original Coloring") plotly::layout(., showlegend = FALSE) else .}
    })

    shiny::observeEvent(input$bootstrap_brush, {
      if (is.null(rv$g1) | is.null(rv$g2)) {
        mst_test_vals_brush$g1 <- NULL
        mst_test_vals_brush$g2 <- NULL
        mst_test_vals_brush$sim_crossings <- NULL
        mst_test_vals_brush$num_crossings <- NULL
      } else {
        mst_test_vals_brush$g1 <- rv$g1
        mst_test_vals_brush$g2 <- rv$g2

        if (length(intersect(mst_test_vals_brush$g1, mst_test_vals_brush$g2)) == 0) {
          mst_test_vals_brush$sim_crossings <- sim_crossings_brush(Z, mst_test_vals_brush$g1, mst_test_vals_brush$g2, cluster, input$num_sim_brush, parallel=parallel)
          mst_test_vals_brush$num_crossings <- count_crossings_brush(tree, mst_test_vals_brush$g1, mst_test_vals_brush$g2)
        }
      }
    })

    output$MSTtest_brush <- shiny::renderPrint({
      if (is.null(mst_test_vals_brush$g1) | is.null(mst_test_vals_brush$g2)) "Select groups and click Run Test!"
      else if (!identical(rv$g1, mst_test_vals_brush$g1) | !identical(rv$g2, mst_test_vals_brush$g2)) "Groups have changed. Please re-run the test!"
      else if (length(intersect(mst_test_vals_brush$g1, mst_test_vals_brush$g2)) != 0) "To run the MST test, the groups cannot share samples!"
      else {
        mst_test_brush(mst_test_vals_brush$sim_crossings, mst_test_vals_brush$num_crossings)
      }
    })

    shiny::observe({
      if (!is.null(ht_brush())) {
        suppressMessages(InteractiveComplexHeatmap::makeInteractiveComplexHeatmap(input, output, session, ht_brush(), "heatmap_brush"))
      }
    })

    if (is.null(meta_data)) bslib::nav_remove("analytical_plots_brush", "Meta Data")

    output$metaDataButton_brush <- shiny::renderUI({
      choices <- colnames(meta_data)

      shiny::radioButtons("metaDataChoice_brush",
                          label = "Which feature?",
                          choices = choices,
                          inline = TRUE)
    })

    output$metaData_brush <- shiny::renderPlot({
      if (is.null(rv$g1) | is.null(rv$g2)) NULL else meta_data_plot_brush(Z, rv$g1, rv$g2, meta_data, input$metaDataChoice_brush)
    })
  }

  shiny::shinyApp(ui, server)
}
