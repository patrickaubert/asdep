#' Une fonction auxiliaire permettant de mettre en forme des graphiques dynamique plotly
#'
#' @param ... les paramètres en input de la fonction ggplot
#'
#' @return un graphique plotly
#' @export
ggplotlyAsdep <- function(...) {

  gdyn <- ggplotly(... , tooltip = c("label","text")) %>%
    layout(legend = list(
      orientation = 'h',
      x = 0.5,
      y = 1.1))

  # traitements pour améliorer le rendu graphique
  for (i in 1:length(gdyn$x$data)) {

    # correction de type "(blabl,1)"
    gdyn$x$data[[i]]$legendgroup <- gsub("^\\(|,[[:digit:]]*\\)$","",gdyn$x$data[[i]]$legendgroup)
    gdyn$x$data[[i]]$name <- gsub("^\\(|,[[:digit:]]*\\)$","",gdyn$x$data[[i]]$name)

    # correction de type "(blabl,1,NA)"
    gdyn$x$data[[i]]$legendgroup <- gsub("^\\(|,[[:digit:]]*,NA\\)$","",gdyn$x$data[[i]]$legendgroup)
    gdyn$x$data[[i]]$name <- gsub("^\\(|,[[:digit:]]*,NA\\)$","",gdyn$x$data[[i]]$name)

    # correction des labels pour graphEvolution
    gdyn$x$data[[i]]$text <- gsub("^paste(.*)\\.\\.\\.\\:[[:space:]]*","",gdyn$x$data[[i]]$text)

  }

  return(gdyn)
}
