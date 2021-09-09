Overview <- R6::R6Class(
  classname = "Overview",
  inherit = View,
  public = list(
    ui = function() {
      ns <- NS(self$id)

      tagList(
        div(
          class = "justify-content-between sub-header",
          h2("Overview")
        ),
        fluidRow(
          column(
            width = 12,
            fluidRow(
              div(
                class = "col mini-container",
                htmlOutput(outputId = ns("configs")),
                h5(strong("Configurations"))
              ),
              div(
                class = "col mini-container",
                htmlOutput(outputId = ns("executions")),
                h5(strong("Target Executions"))
              ),
              div(
                class = "col mini-container",
                htmlOutput(outputId = ns("best")),
                h5(strong("Best Configuration ID"))
              )
            ),
            fluidRow(
              div(
                class = "col mini-container",
                htmlOutput(outputId = ns("iterations")),
                h5(strong("Iterations"))
              ),
              div(
                class = "col mini-container",
                htmlOutput(outputId = ns("instances")),
                h5(strong("Instances"))
              ),
              div(
                class = "col mini-container",
                htmlOutput(outputId = ns("elit")),
                h5(strong("Elite"))
              )
            ),
            fluidRow(

              box(
                title = strong("Parallel Coordinates"),
                collapsible = FALSE,
                closable = FALSE,
                width = 12,
                height = 500,
                actionButton(inputId = ns("adv_pc"),"Advanced"),
                plotlyOutput(outputId = ns("left_1"))
              )
            )
          ),
          column(
            width = 12,
            fluidRow(
              box(
                title = strong("Training Boxplot"),
                collapsible = FALSE,
                collapsed = FALSE,
                width = 6,
                plotlyOutput(outputId = ns("train_bp"))

              ),
              box(
                title = strong("Testing Boxplot"),
                collapsible = FALSE,
                collapsed = FALSE,
                width = 6,
                plotlyOutput(outputId = ns("test_bp"))
              )
            )
          ),
          column(
            width = 12,
            fluidRow(
              box(
                title = strong("Configuration Process"),
                collapsible = FALSE,
                closable = FALSE,
                width = 12,
                plotlyOutput(outputId = ns("right_1"))
              ),
              box(
                title = strong("Sampling Frequency"),
                collapsible = FALSE,
                closable = FALSE,
                width = 12,
                plotOutput(outputId = ns("right_2"))
              )
            )
          )
        )
      )
    },

    server = function(input, output, session, store) {
      ns <- session$ns

      observeEvent(input$adv_pc,{
        #updatebs4TabItems(session,ns("sidebar"),"adv_parallel_coord_view")
      })
      output$configs <- renderUI({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            ""
          )
        )

        h6(nrow(store$irace_results$allConfigurations))
      })

      output$executions <- renderUI({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            ""
          )
        )

        h6(store$irace_results$state$experimentsUsedSoFar)
      })

      output$best <- renderUI({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            ""
          )
        )

        last <- length(store$irace_results$iterationElites)
        id <- store$irace_results$iterationElites[last]
        bestConfiguration <- getConfigurationById(
          iraceResults = store$irace_results,
          ids = id
        )

        h6(bestConfiguration[[1]])
      })

      output$iterations <- renderUI({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            ""
          )
        )
        n_iterations <- length(store$irace_results$allElites)
        h6(n_iterations)
      })

      output$instances <- renderUI({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            ""
          )
        )
        n_instances <- length(store$irace_results$experiments)
        h6(n_instances)
      })

      output$elit <- renderUI({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            ""
          )
        )
        last <- length(store$irace_results$allElites)
        n_elites_end <- length(store$irace_results$allElites[[last]])
        h6(n_elites_end)
      })

      output$right_1 <- renderPlotly({
        iraceplot::plot_experiments_matrix(store$irace_results, .interactive = interactive())
      })

      output$right_2 <- renderPlot({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            ""
          )
        )

        iraceplot::sampling_frequency(store$irace_results,n = 2)

      })

      output$train_bp <- renderPlotly({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            ""
          )
        )
        p <- iraceplot::boxplot_training(irace_results = store$irace_results)
        ggplotly(p)
      })

      output$test_bp <- renderPlotly({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            ""
          )
        )
        if(!("testing" %in% names(store$irace_results))){
          print("The Rdata does not contain the testing data")
        }else{
          p <- iraceplot::boxplot_test(irace_results = store$irace_results,type = "best")
          ggplotly(p)
        }


      })

      output$left_1 <- renderPlotly({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            ""
          )
        )
        iraceplot::parallel_coord(store$irace_results)
      })
      # output$left_2 <- renderPlotly(
      #   plot_ly(z = ~volcano, type = "surface")
      # )
    }
  )
)

