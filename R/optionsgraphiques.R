#' Une fonction auxiliaires qui fournit les options d'affichage d'un graphique départemental
#'
#' Cette fonction est notamment utilisée dans les fonctions graphEvolution, graphComparaison, etc.
#'
#' @param dept le nom du département qu'on souhaite représenter
#' @param comp le nom du territoire qu'on souhaite représenter en comparaison (par défaut, "TOTAL estimé France entière (hors Mayotte)")
#' @param options un vecteur d'options issu de la fonction d'origine
#'
#' @return
#' @export
optionsgraphiques <- function(dept,
                              comp = "TOTAL estimé France entière (hors Mayotte)",
                              options = c()) {

  # choix des zones à affichier sur le graphique

  #zoneaffiche <- c()
  #if ("interdeciles" %in% options) zoneaffiche <- c(zoneaffiche,"p10","p90")
  #if ("interquartiles" %in% options) zoneaffiche <- c(zoneaffiche,"p25","p75")
  #if ("interdecilespond" %in% options) zoneaffiche <- c(zoneaffiche,"p10pond","p90pond")
  #if ("interquartilespond" %in% options) zoneaffiche <- c(zoneaffiche,"p25pond","p75pond")
  #if ("medianePM10" %in% options) zoneaffiche <- c(zoneaffiche,"p50.m10","p50.p10")
  #if ("medianePM20" %in% options) zoneaffiche <- c(zoneaffiche,"p50.m20","p50.p20")
  zoneaffiche <- unique(
    ParamGraphiquesAsdep[ParamGraphiquesAsdep$ymin %in% options,"noms"],
    ParamGraphiquesAsdep[ParamGraphiquesAsdep$ymax %in% options,"noms"]
  )

  optionszones <- intersect(options,ParamGraphiquesAsdep$noms)

  # couleurs des zones

  ParamGraphiques <- ParamGraphiquesAsdep %>%
    filter(noms %in% c("dept","comp","autres",optionszones)) %>%
    mutate(intitules = recode(intitules,
                              "Territoire de référence" = dept,
                              "Groupe de comparaison" = comp,
                              "Autres territoires" = "Autres départements"))
  rownames(ParamGraphiques) <- ParamGraphiques$noms

  couleursloc <- ParamGraphiques[c("dept","comp","autres",optionszones),"couleur"]
  names(couleursloc) <- ParamGraphiques[c("dept","comp","autres",optionszones),"intitules"]
  couleursloc <- couleursloc[names(couleursloc) != ""]

  alphasloc <- ParamGraphiques[c("dept","comp","autres",optionszones),"alpha"]
  names(alphasloc) <- ParamGraphiques[c("dept","comp","autres",optionszones),"intitules"]
  alphasloc <- alphasloc[names(alphasloc) != ""]

  return(
    list(
      zoneaffiche = zoneaffiche,
      optionszones = optionszones,
      alphas = alphasloc,
      couleurs = couleursloc
    )
  )
}
