#' Une fonction auxiliaire permettant de mettre en forme des graphiques dynamique plotly
#'
#' @param ... les paramètres en input de la fonction ggplot
#'
#' @return un graphique plotly
#' @export
#'
#' @examples
ggplotlyAsdep <- function(...) {

  gdyn <- ggplotly(...) %>%
    layout(legend = list(
      orientation = 'h',
      x = 0.5,
      y = 1.1))

  # traitements pour améliorer le rendu graphique
  for (i in 1:length(gdyn$x$data)) {
    gdyn$x$data[[i]]$legendgroup <- gsub("^\\(|,[[:digit:]]*\\)$","",gdyn$x$data[[i]]$legendgroup)
    gdyn$x$data[[i]]$name <- gsub("^\\(|,[[:digit:]]*\\)$","",gdyn$x$data[[i]]$name)
  }

  return(gdyn)
}
