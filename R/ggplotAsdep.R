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
    #theme_light() +
    theme_minimal(
      base_family = "Arial Narrow",
      base_size = 11.5) +
    theme(
      legend.position="top"
    ) +
    labs(fill = "",
         colour = "",
         size = "",
         axis.title.x = element_text(size=10),
         axis.title.y = element_text(size=10)
    )
}
