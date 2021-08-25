#' @export
create_hidden_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path)
    if (.Platform$OS.type == "windows") {
      # Invisible directory in windows
      system2(command = "attrib", args = paste("+h", path))
    }
  }
}

#' @export
get_option <- function(name, default = NULL) {
  if (is.null(get_golem_options(name))) {
    return(default)
  }

  return(golem::get_golem_options(name))
}

#' @export
alert_error <- function(message = NULL) {
  shinyalert(
    title = "Error",
    text = message,
    type = "error",
    closeOnClickOutside = TRUE
  )
}

#' @export
check_null <- function(x, default) {
  if (is.null(x)) {
    default
  } else {
    x
  }
}
