Navbar <- R6::R6Class(
  classname = "Navbar",
  inherit = Component,
  public = list(
    ui = function(id) {
      ns <- NS(id)

      dashboardHeader(
        title = dashboardBrand(
          title = h2("Irace Explorer", style = "text-align:center; margin-bottom: 0;")
        ),
        fixed = TRUE,
        h4(
          textOutput(
            outputId = ns("playgroundName")
          ),
          style = "text-align: center; flex: 1 0 auto; margin-top: 5px;"
        )
      )
    },

    server = function(input, output, session, store) {
      output$playgroundName <- renderText(store$playground_name)
    }
  )
)

Body <- R6::R6Class(
  classname = "Body",
  public = list(
    overview = NULL,

    filter_view = NULL,
    performance_instance = NULL,
    performance_config = NULL,
    advanced_parallel_coord = NULL,
    advanced_boxplot_train = NULL,
    advanced_boxplot_test = NULL,
    advanced_sampling_frequency = NULL,
    advanced_configuration_process = NULL,


    initialize = function() {
      self$overview <- Overview$new("overview")

      self$filter_view <- FilterView$new("visualization_filter")
      self$performance_instance <- PerformanceInstanceView$new("visualization_by_instance")
      self$performance_config <- PerformanceConfigView$new("visualization_by_config")

      self$advanced_parallel_coord <- AdvancedParallelCoord$new("adv_parallel_coord_view")
      self$advanced_boxplot_train <- AdvancedBoxplotTrain$new("adv_boxplot_train")
      self$advanced_boxplot_test <- AdvancedBoxplotTest$new("adv_boxplot_test")
      self$advanced_sampling_frequency <- AdvancedSamplingFrequency$new("adv_sampling_frequency")
      self$advanced_configuration_process <- AdvancedConfigurationProcess$new("adv_configuration_process")
    },

    ui = function() {
      dashboardBody(
        irace_vizz_resources(),
        tabItems(
          tabItem(
            tabName = "overview",
            self$overview$ui()
          ),
          tabItem(
            tabName = "visualization_filter",
            self$filter_view$ui()
          ),
          tabItem(
            tabName = "visualization_by_config",
            self$performance_config$ui()
          ),
          tabItem(
            tabName = "visualization_by_instance",
            self$performance_instance$ui()
          ),
          tabItem(
            tabName = "adv_parallel_coord_view",
            self$advanced_parallel_coord$ui()
          ),
          tabItem(
            tabName = "adv_boxplot_train",
            self$advanced_boxplot_train$ui()
          ),
          tabItem(
            tabName = "adv_boxplot_test",
            self$advanced_boxplot_test$ui()
          ),
          tabItem(
            tabName = "adv_sampling_frequency",
            self$advanced_sampling_frequency$ui()
          ),
          tabItem(
            tabName = "adv_configuration_process",
            self$advanced_configuration_process$ui()
          )
        )
      )
    },

    setupModules = function(store, events) {
      self$overview$call(store = store)

      self$filter_view$call(store = store, events = events)
      self$performance_instance$call(store = store, events = events)
      self$performance_config$call(store = store, events = events)

      self$advanced_parallel_coord$call(store = store)
      self$advanced_boxplot_train$call(store = store)
      self$advanced_boxplot_test$call(store = store)
      self$advanced_sampling_frequency$call(store = store)
      self$advanced_configuration_process$call(store = store)
    },
    server = function(input, output, session, store){
      observe(print(ns("sidebar")))
    }
  )
)


Sidebar <- R6::R6Class(
  classname = "Sidebar",
  public = list(

    ui = function() {
      ns <- NS(self$id)

      dashboardSidebar(
        id = "dash",
        minified = FALSE,

        bs4SidebarMenu(
          id = ns("sidebar"),
          menuItem(
            text = "Overview",
            tabName = "overview",
            icon = NULL
          ),
          menuItem(
            text = strong("Performance"),
            menuSubItem(
              text = "Configuration",
              tabName = "visualization_by_config",
              icon = NULL
            ),
            menuSubItem(
              text = "Instance",
              tabName = "visualization_by_instance",
              icon = NULL
            )
          ),
          menuItem(
            text = "Filter",
            tabName = "visualization_filter",
            icon = NULL
          ),
          menuItem(
            text = strong("Advanced"),
            menuSubItem(
              text = "Paralell Cordinate",
              tabName = "adv_parallel_coord_view",
              icon = NULL
            ),
            menuSubItem(
              text = "Boxplot Train",
              tabName = "adv_boxplot_train",
              icon = NULL
            ),
            menuSubItem(
              text = "Boxplot Test",
              tabName = "adv_boxplot_test",
              icon = NULL
            ),
            menuSubItem(
              text = "Configuration Process",
              tabName = "adv_configuration_process",
              icon = NULL
            ),
            menuSubItem(
              text = "Sampling Frequency",
              tabName = "adv_sampling_frequency",
              icon = NULL
            )
          )
        ),
        bs4DashControlbar(
          skin = "light",
          selectInput(
            inputId = "controller",
            label = "Advanced",
            choices = list("overview" = "overview","adv_boxplot_train"="adv_boxplot_train-sidebar","adv_parallel_coord_view"="adv_parallel_coord_view"),
            selected = "adv_boxplot_train-sidebar"
          )

        )
      )
    },
    server = function(input, output, session, store){
      ns <- session$ns



       observeEvent(input$controller, {
         updatebs4TabItems(
           session,
           inputId = ns("sidebar"),
           selected = input$controller
         )
       }, ignoreInit = TRUE)

    }
  )
)
