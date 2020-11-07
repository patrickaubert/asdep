#' Fonction produisant un graphique (format ggplot) présentant l'évolution d'un indicateur d'aide sociale au cours du temps
#'
#' Cette fonction sert à produire un graphique (au format ggplot2) représentant l'évolution au cours du temps d'un indicateur pour un département de référence, et un territoire de comparaisons (par exemple : la France entière, la région du département de référence, un groupe de département). Le tableau sous-jacent peut également être récupéré avec l'option typesortie = "tab".
#' Les options permettent d'ajouter sur le graphique une représentation des diverses zones : zone interquartile ("interquartiles"), zone interdécile ("interdeciles"), zone correspondant à la médiane +/- 10 ou 20 % ("medianePM10" et "medianePM20") ...
#'
#' @param nomvariable le nom de la variable, parmi celles de la table ASDEPsl
#' @param denom le nom d'une variable à utiliser au dénominateur de l'indicateur (si absent la variable est utilisée brute)
#' @param poidsobs une variable de pondération (par exemple si on souhaite calculer des quantiles pondérés par la population des départements). Par défaut, si denom est renseigné, cette variable denom est utilisée comme poidsobs
#' @param options un vecteur d'options du calcul (valeurs en euros courant ou constant, mensuels ou annuels, etc.) et d'options de représentation (représenter les zones interdécile, interquartile, médiane +/- 10 %, etc)
#' @param dept le nom du département qu'on souhaite représenter
#' @param comp le nom du territoire qu'on souhaite représenter en comparaison (par défaut, "TOTAL estimé France entière (hors Mayotte)")
#' @param gpecomp un vecteur de noms de départements : leur ensemble constituera le "groupe de comparaison" (si cette variable est renseignée, la variable 'comp' est automatiquement égale à "groupe de comparaison")
#' @param typesortie détermine l'objet en sortie de la fonction : un graphique si l'option "graph" est retenue (option par défaut), une table de donnée (data frame) si l'option "tab" est retenue
#' @param donnees la table de données en entrée (par défaut, la table ASDEPsl)
#' @param variables la table de métadonnées de la table de données en entrée (par défaut, la table ASDEPsl_description)
#'
#' @return un graphique (format ggplot2)
#' @export
#'
#' @examples graphEvolution(nomvariable="NbBenefAPA",dept="Vosges",comp="Grand Est",typesortie="tab")
#' @examples graphEvolution(nomvariable="NbBenefAPA",denom="pop.60.99",dept="Vosges",comp="Grand Est",options=c("interquartiles","interdeciles"))
#' @examples graphEvolution(nomvariable="NbBenefAPA",denom="pop.60.99",dept="Vosges",gpecomp=c("Meuse","Moselle"),options=c("medianePM10","medianePM20"))
graphEvolution <- function(nomvariable, denom = "", options = c(), poidsobs = c(),
                           dept, comp = "TOTAL estimé France entière (hors Mayotte)", gpecomp = c(),
                           typesortie = "graph",
                           donnees = ASDEPsl, variables = ASDEPsl_description) {

  # === Récupération des données pour l'indicateur
  if (NROW(gpecomp)>=1) {comp <- "Groupe de comparaison"}
  if (nchar(denom)>0) {poidsobs <- c(poidsobs,denom)}
  tab <- selectIndic(nomvariable = nomvariable, denom = denom, keepvar=c(poidsobs),
                     options = options, gpeDpt = gpecomp,
                     donnees = donnees, variables = variables)$var
  tabd <- tab[,c("Annee","Territoire",nomvariable)] %>%
    filter(Territoire %in% c(dept,comp))

  # === Récupération des données sur les distributions (quantiles, etc.)
  tabq <- quantileIndic(donneesQV = tab %>% filter(TypeTerritoire == "Département"),
                        var=nomvariable,  groupe="Annee",  poids=poidsobs) %>%
    mutate(Annee = as.numeric(as.character(Annee)))

  zoneaffiche <- c()
  if ("interdeciles" %in% options) zoneaffiche <- c(zoneaffiche,"p10","p90")
  if ("interquartiles" %in% options) zoneaffiche <- c(zoneaffiche,"p25","p75")
  if ("interdecilespond" %in% options) zoneaffiche <- c(zoneaffiche,"p10pond","p90pond")
  if ("interquartilespond" %in% options) zoneaffiche <- c(zoneaffiche,"p25pond","p75pond")
  if ("medianePM10" %in% options) zoneaffiche <- c(zoneaffiche,"p50.m10","p50.p10")
  if ("medianePM20" %in% options) zoneaffiche <- c(zoneaffiche,"p50.m20","p50.p20")

  # === la table pour le graphique
  tabd2 <- tabd %>% pivot_wider(id_cols=c("Annee"),names_from="Territoire",values_from=nomvariable)

  tabg1 <- tabd2 %>% left_join(tabq, by="Annee")
  tabg1 <- tabg1[, c(names(tabd2), zoneaffiche)]

  tabg2 <- tabd[,c("Annee","Territoire",nomvariable)]
  names(tabg2) <- c("Annee","Territoire","indicateur")

  # === le graphique

  couleursloc <- c("Zone interdécile" = "blue",
                   "Zone interquartile" = "blue",
                   "Zone interdécile (pondérée)" = "green",
                   "Zone interquartile (pondérée)" = "green",
                   "Médiane +/- 20 %" = "red",
                   "Médiane +/- 10 %" = "red"
                   )
  g <- ggplotAsdep() +
    geom_line(data=tabg2,aes(x=Annee,y=indicateur,colour=Territoire))

  if ("interdeciles" %in% options) {g <- g + geom_ribbon(data=tabq,aes(ymin=p10, ymax=p90, x=Annee, fill="Zone interdécile"), alpha = 0.1)}
  if ("interquartiles" %in% options) {g <- g + geom_ribbon(data=tabq,aes(ymin=p25, ymax=p75, x=Annee, fill="Zone interquartile"), alpha = 0.2)}
  if ("interdecilespond" %in% options) {g <- g + geom_ribbon(data=tabq,aes(ymin=p10pond, ymax=p90pond, x=Annee, fill="Zone interdécile (pondérée)"), alpha = 0.1)}
  if ("interquartilespond" %in% options) {g <- g + geom_ribbon(data=tabq,aes(ymin=p25pond, ymax=p75pond, x=Annee, fill="Zone interquartile (pondérée)"), alpha = 0.2)}
  if ("medianePM10" %in% options) {g <- g + geom_ribbon(data=tabq,aes(ymin=p50.m20, ymax=p50.p20, x=Annee, fill="Médiane +/- 20 %"), alpha = 0.1)}
  if ("medianePM20" %in% options) {g <- g + geom_ribbon(data=tabq,aes(ymin=p50.m10, ymax=p50.p10, x=Annee, fill="Médiane +/- 10 %"), alpha = 0.3)}

  g <- g + scale_fill_manual(values = couleursloc)

  # === objet en sortie de la fonction

  return( if (typesortie == "graph") {g} else if (typesortie == "tab") {tabg1})

}
