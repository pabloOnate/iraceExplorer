#' Run Irace Studio
#'
#' @param port A port number that Irace Studio will listen on.
#' @export
run_irace_vizz <- function(port = NULL, ...) {
  app_dir <- app_sys("iraceVizz")

  if (app_dir == "") {
    stop("Could not find example directory. Try re-installing `Irace Studio`.", call. = FALSE)
  }

  print(with_golem_options(
    app = shiny::shinyAppDir(app_dir, options = list(display.mode = "normal", port = port, launch.browser = T)),
    golem_opts = list(...)
  ))
}

# run_app <- function(port = 4350, ...) {
#   app_dir <- app_sys("iraceStudio")
#
#   if (app_dir == "") {
#     stop("Could not find example directory. Try re-installing `Irace Studio`.", call. = FALSE)
#   }
#
#   print(with_golem_options(
#     app = shiny::shinyAppDir(app_dir, options = list(display.mode = "normal", port = port, launch.browser = T)),
#     golem_opts = list(...)
#   ))
# }
