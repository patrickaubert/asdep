#' Une fonction auxiliaire qui corrige les noms de territoires mal orthographiés
#'
#' La fonction s'appuie sur la base 'nomscorrecterritoires' contenue dans le package.
#' Si un nom est reconnu comme faisant partie des noms mal-orthographiés dans cette base
#' (par exemple "La-Réunion" au lieu de "La Réunion"), il est remplacés par la forme correcte
#' d'après la base
#'
#' @param nomdep un vecteur de noms de territoires
#' @param nomcorrec un vecteur contenant les formes correctes des noms de territoires (par défaut, la variable 'TerritoireCorrect' de la base 'nomscorrecterritoires')
#' @param nommalortho un vecteur (de même taille que 'nomcorrec') contenant les formes mal-orthographiées des noms de territoires (par défaut, la variable 'TerritoireMalortho' de la base 'nomscorrecterritoires')
#'
#' @return un vecteur de la même taille que nomdep, avec les formes corrigées si un mauvaise orthographe a été détectée
#' @export
#'
#' @examples corrigeNom(nomdep=c("Alpes de Haute-Provence","Territoire-de-Belfort","Nouveau-Rhône"))
corrigeNom <- function(nomdep,
                       nomcorrec = asdep::nomscorrectsterritoires$TerritoireCorrect,
                       nommalortho = asdep::nomscorrectsterritoires$TerritoireMalortho){
  (data.frame(
    orig = nomdep,
    stringsAsFactors = FALSE
  ) %>%
    left_join(
      data.frame(
        nomcorrec = nomcorrec,
        nommalortho = nommalortho,
        stringsAsFactors = FALSE
      ),
      by = c("orig" = "nommalortho")
    ) %>%
    mutate(correc = ifelse(!is.na(nomcorrec),nomcorrec,orig))
  )$correc
}
