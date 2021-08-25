AdvancedSamplingFrequency <- R6::R6Class(
  classname = "AdvancedSamplingFrequency",
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
          h2("Sampling Frequency")
        ),
        fluidRow(
          box(
            width = 12,
            title = "Parameters",
            fluidRow(
              box(
                title = "Param_names",
                checkboxGroupInput(
                  inputId = ns("params"),
                  label = "",
                  choices = c()
                )
              ),
              box(
                title = "n",
                selectInput(
                  inputId = ns("n"),
                  label = "",
                  choices = c()
                )
              )
            )),
          box(
            width = 12,
            plotOutput(outputId = ns("adv_sampling_frequency"))
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
      observeEvent(input$iterations,{

      })
      output$adv_sampling_frequency <- renderPlot({
        iraceplot::sampling_frequency(store$irace_results, param_names = input$params)
      })
    },
    setup_inputs = function(session, store){
      updateCheckboxGroupInput(
        session = session,
        inputId = "params",
        choices = c(store$irace_results$parameters$names),
        inline = F
      )
    }
  )
)
