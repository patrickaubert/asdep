#' Une fonction auxiliaire qui retourne le champ le plus large possible pour lequel une variable est non nulle
#'
#' Si la variable est disponible pour le champ France entière, ce champ est retenu, puis dans l'ordre : France entière hors Mayotte, France métropolitaine.
#'
#' @param var un nom de variable de la table ASDEPsl
#' @param annee une année
#'
#' @return un nom de territoire
#' @export
#'
#' @examples champFrance("NbBenefAPA")
#' @examples champFrance("NbBenefAPA",2005)
champFrance <- function(var,annee=NULL,tab=ASDEPsl) {

  tab <- tab[,c(var,"TypeTerritoire","Territoire","Annee")]
  tab <- tab[!is.na(tab[,var]),]

  if (is.null(annee)) {annee<-max(tab$Annee)}
  tab <- tab   %>%     filter(Annee == annee)

  case_when(
    ("TOTAL estim\u00E9 France enti\u00E8re" %in% tab$Territoire) ~ "TOTAL estim\u00E9 France enti\u00E8re",
    ("TOTAL estim\u00E9 France enti\u00E8re (hors Mayotte)" %in% tab$Territoire) ~ "TOTAL estim\u00E9 France enti\u00E8re (hors Mayotte)",
    ("TOTAL estim\u00E9 France m\u00E9tropolitaine" %in% tab$Territoire) ~ "TOTAL estim\u00E9 France m\u00E9tropolitaine",
    TRUE ~ ""
  )
}
