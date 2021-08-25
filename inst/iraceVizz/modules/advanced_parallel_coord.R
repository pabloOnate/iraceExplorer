AdvancedParallelCoord <- R6::R6Class(
  classname = "AdvancedParallelCoord",
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
          h2("Parallel Coordinate")
        ),
        fluidRow(
          fluidRow(
            box(
             title = "Parameters",
             width = 12,
             fluidRow(
               box(
                width = 2.5,
                title = "only_elite",
                radioButtons(inputId = ns("radios"),
                             label = "",
                          choices = list(
                            "FALSE" = F,
                            "TRUE" = T),
                          selected = T
                          )
             ),
             box(
               width = 2.5,
               title = "iterations",
               checkboxGroupInput(
                 inputId = ns("group"),
                 label = "",
                 choices = c(0:0),
                 inline = F
               )
             ),
             box(
               width = 3.3,
               title = "param_names",
               checkboxGroupInput(
                 inputId = ns("param"),
                 label = "",
                 choices = c(),
                 inline = F
               )
             )
            )),
            box(
              title = "",
              width = 12,
              plotlyOutput(outputId = ns("adv_parallel_coord"))
            )
          )
        )
      )
    },

    server = function(input, output, session, store){
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
      observeEvent(input$radios,{

      })
      observeEvent(input$group,{

      })
      observeEvent(input$param,{

      })

      output$value <- renderPrint({input$group})

      output$adv_parallel_coord <- renderPlotly({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            ""
          )
        )

        iraceplot::parallel_coord(
          store$irace_results,
          only_elite = input$radios,
          iterations = input$group,
          param_names = input$param)
      })
    },
    setup_inputs = function(session, store){
      updateCheckboxGroupInput(
        session = session,
        inputId = "group",
        choices = c(1:length(store$irace_results$allElites)),
        inline = F
      )
      updateCheckboxGroupInput(
        session = session,
        inputId = "param",
        choices = store$irace_results$parameters$names,
        inline = F
      )
    }


  )

)
