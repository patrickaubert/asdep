#' Une fonction auxiliaire qui crée les tables nécessaires pour afficher des zones (interquartiles, etc) sur un graphique
#'
#' Cette fonction est notamment utilisée dans les fonctions graphEvolution, graphComparaison, etc.
#'
#' @param nomvariable le nom de la variable, parmi celles de la table ASDEPsl
#' @param options un vecteur d'options issu de la fonction d'origine
#' @param nbrang
#' @param tab une table avec les valeurs de l'indicateur
#' @param poidsobs une variable de pondération (par exemple si on souhaite calculer des quantiles pondérés par la population des départements). Par défaut, si denom est renseigné, cette variable denom est utilisée comme poidsobs
#'
#' @return
#' @export
selectZones <- function(nomvariable,
                        options,
                        nbrang = 1,
                        tab,
                        poidsobs) {

  # === table avec les différents éléments de distributions (quantiles, etc.), par année
  tabq <- quantileIndic(donneesQV = tab %>% filter(TypeTerritoire == "Département"),
                        var=nomvariable,  groupe="Annee",  poids=poidsobs) %>%
    mutate(Annee = as.numeric(as.character(Annee)))

  # === table pour l'affichage des zones sur les graphiques
  zonesloc <- ParamGraphiquesAsdep %>% filter(noms %in% options)

  tabq2 <- tabq %>%
    slice(rep(1:n(), each = nbrang )) %>%
    mutate(rang = 1:n())

  tabq3 <- data.frame()
  if (nrow(zonesloc)>=1) {
    for (i in 1:nrow(zonesloc)){
      tabq3 <- rbind(tabq3,
                     data.frame(
                       Annee = tabq2$Annee,
                       rang = tabq2$rang,
                       intitules = rep(zonesloc$intitules[i] , nrow(tabq2)),
                       noms = rep(zonesloc$noms[i] , nrow(tabq2)),
                       ymin = tabq2[,zonesloc$ymin[i]],
                       ymax = tabq2[,zonesloc$ymax[i]],
                       alpha = rep(zonesloc$alpha[i] , nrow(tabq2))
                     ))
    }
    tabq3 <- tabq3 %>% arrange(intitules,Annee,rang)
  }

  # === outputs de la fonction
  return(
    list(
      quantiles = tabq,
      zones = tabq3
    )
  )
}
