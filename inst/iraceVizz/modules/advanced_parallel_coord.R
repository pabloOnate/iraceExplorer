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
             ),
             box(
               title = "id_configuration",
               sliderInput(
                 inputId = ns("id_conf"),
                 label = "",
                 min = 0,
                 max = 0,
                 value = 0
               )
             ),
             box(
               title = "by_n_param",
               numericInput(
                inputId = ns("by_n"),
                label = "",
                value = 14
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


      output$adv_parallel_coord <- renderPlotly({
        shiny::validate(
          need(
            !is.null(store$irace_results),
            ""
          )
        )

        iraceplot::parallel_coord(
          irace_results =  store$irace_results,
          only_elite = input$radios,
          iterations = input$group,
          param_names = input$param,
          by_n_param = input$by_n)
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
        inline = F,
        selected = store$irace_results$parameters$names
      )
      updateSliderInput(
        session = session,
        inputId = "id_conf",
        min = 1,
        max = dim(store$irace_results$allConfigurations)[1]

      )
    }


  )

)
