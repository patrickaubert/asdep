#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "aidesocialedep", package = "asdep")
  if (appDir == "") {
    stop("L'exemple n'a pas été trouvé.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
