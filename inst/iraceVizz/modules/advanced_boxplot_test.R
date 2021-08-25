AdvancedBoxplotTest <- R6::R6Class(
  classname = "AdvancedBoxplotTest",
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
          h2("Testing Boxplot")
        ),
        fluidRow(
          box(
            width = 12,
            title = "Parameters",
            fluidRow(
              box(
                title = "Type",
                selectInput(
                  inputId = ns("type"),
                  label = "",
                  choices = c("all" = "all", "ibest" = "ibest", "best" = "best"),
                  selected = "all"
                )
              ),
              box(
                title = "RPD",
                selectInput(
                  inputId = ns("rpd"),
                  label = "",
                  choices = c("True" = T, "False" = F),
                  selected = T
                )
              )
            )),
            box(
              width = 12,
              plotlyOutput(outputId = ns("adv_testing_boxplot"))
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
      observeEvent(input$type,{

      })

      output$adv_testing_boxplot <- renderPlotly({
        p <- iraceplot::boxplot_test(store$irace_results, type = input$type, rpd = input$rpd)
        ggplotly(p)
      })
    },
    setup_inputs = function(session, store){

    }
  )
)
