AdvancedBoxplotTrain <- R6::R6Class(
  classname = "AdvancedBoxplotTrain",
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
          h2("Training Boxplot")
        ),
        fluidRow(
          box(
            width = 12,
            title = "Parameters",
            fluidRow(
              box(
                title = "Iteration",
                selectInput(
                  inputId = ns("iteration"),
                  label = "",
                  choices = c(0,0),
                  selected = NULL
                )
              ),
              box(
                title = "RPD",
                selectInput(
                  inputId = ns("rpd"),
                  label = "",
                  choices = c("True" = T,"False" = F),
                  selected = T
                )
              )
            )),
          box(
            width = 12,
            plotlyOutput(outputId = ns("adv_training_boxplot"))
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
      observeEvent(input$iteration,{

      })
      output$adv_training_boxplot <- renderPlotly({
        p <- iraceplot::boxplot_training(store$irace_results, iteration = strtoi(input$iteration), rpd = input$rpd)
        ggplotly(p)
      })
    },
    setup_inputs = function(session, store){
      updateSelectInput(
        session = session,
        inputId = "iteration",
        choices = c(1:length(store$irace_results$allElites)),
        selected = length(length(store$irace_results$allElites))
      )
    }
  )
)
