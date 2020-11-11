#' Une fonction auxiliaire permettant de mettre en forme des graphiques ggplot
#'
#' @param ... les paramètres en input de la fonction ggplot
#'
#' @return un graphique ggplot
#' @export
#'
#' @examples
ggplotAsdep <- function(...) {
  ggplot(...) +
    theme(legend.position="top") +
    labs(fill = "",
         colour = "",
         size = "",
         axis.title.x = element_text(size=10),
         axis.title.y = element_text(size=10)
    )
}
