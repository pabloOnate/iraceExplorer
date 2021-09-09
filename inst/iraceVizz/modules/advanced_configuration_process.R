AdvancedConfigurationProcess <- R6::R6Class(
  classname = "AdvancedConfigurationProcess",
  inherit = View,
  public = list(
    initialize = function(id) {
      super$initialize(id)
    },
    ui = function(){
      ns <- NS(self$id)
      tagList(
        div(
          class = "justify-content-between sub-header",
          h2("Configuration Process")
        ),
        fluidRow(
          box(
            width = 12,
            plotlyOutput(outputId = ns("adv_configuration_process"))
          )
        )
      )
    },
    server = function(input,output, session, store){
      ns <- session$ns
      observeEvent(
        c(
          store$irace_results
        ),
        {
          self$setup_inputs(session, store)
        },
        ignoreNULL = FALSE
      )


      output$adv_configuration_process <- renderPlotly({
        iraceplot::plot_experiments_matrix(store$irace_results,.interactive = interactive())
      })
    },
    setup_inputs = function(session, store){

    }
  )
)
