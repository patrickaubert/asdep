#' Fonction produisant un graphique présentant l'évolution d'un indicateur d'aide sociale au cours du temps
#'
#' Cette fonction sert à produire un graphique (au format ggplot2 ou plotly) représentant l'évolution au cours du temps d'un indicateur pour un département de référence, et un territoire de comparaisons (par exemple : la France entière, la région du département de référence, un groupe de département).
#' Le tableau sous-jacent peut également être récupéré avec l'option typesortie = "tab".
#' Les options permettent d'ajouter sur le graphique une représentation des diverses zones : zone interquartile ("interquartiles"), zone interdécile ("interdeciles"), zone correspondant à la médiane +/- 10 ou 20 % ("medianePM10" et "medianePM20") ...
#'
#' @param nomvariable le nom de la variable, parmi celles de la table ASDEPsl
#' @param denom le nom d'une variable à utiliser au dénominateur de l'indicateur (si absent la variable est utilisée brute)
#' @param poidsobs une variable de pondération (par exemple si on souhaite calculer des quantiles pondérés par la population des départements). Par défaut, si denom est renseigné, cette variable denom est utilisée comme poidsobs
#' @param options un vecteur d'options du calcul (valeurs en euros courant ou constant, mensuels ou annuels, etc.) et d'options de représentation (représenter les zones interdécile, interquartile, médiane +/- 10 %, etc)
#' @param dept le nom du département qu'on souhaite représenter
#' @param comp le nom du territoire qu'on souhaite représenter en comparaison (par défaut, "TOTAL estimé France entière (hors Mayotte)")
#' @param gpecomp un vecteur de noms de départements : leur ensemble constituera le "groupe de comparaison"
#' @param typesortie détermine l'objet en sortie de la fonction : un graphique si l'option "graph" est retenue (option par défaut), une table de donnée (data frame) si l'option "tab" est retenue. Les options "tabcomplet" (une liste de tables) et "graphdyn" (un graphique Plotly) sont également disponibles
#' @param donnees la table de données en entrée (par défaut, la table ASDEPsl)
#' @param variables la table de métadonnées de la table de données en entrée (par défaut, la table ASDEPsl_description)
#'
#' @return un graphique (format ggplot ou plotly), ou éventuellement un tableau (selon les options retenues)
#' @export
#'
#' @examples graphEvolution(nomvariable="NbBenefAPA",dept="Vosges",comp="Grand Est",typesortie="tab")
#' @examples graphEvolution(nomvariable="NbBenefAPA",denom="pop.60.99",dept="Vosges",comp="Grand Est",options=c("interquartiles","interdeciles","medianePM10"))
#' @examples graphEvolution(nomvariable="NbBenefAPA",denom="pop.60.99",dept="Vosges",gpecomp=c("Meuse","Moselle"),options=c("medianePM10","medianePM20"),typesortie="graphdyn")
#' @examples graphEvolution(nomvariable="effpersassfam",denom="popTOT",dept="Vosges",gpecomp=c(),options=c("medianePM10","medianePM20"),typesortie="tab")
graphEvolution <- function(nomvariable, denom = "", options = c(), poidsobs = c(),
                           dept, comp = "TOTAL estimé France entière (hors Mayotte)", gpecomp = c(),
                           typesortie = "graph",
                           donnees = ASDEPsl, variables = ASDEPsl_description) {


  # === Récupération des données pour l'indicateur
  #if (NROW(gpecomp)>=1) {comp <- "Groupe de comparaison"}
  if (nchar(denom)>0) {poidsobs <- unique( c(poidsobs,denom) ) }
  tabs <- selectIndic(nomvariable = nomvariable, denom = denom, keepvar=c(poidsobs),
                     options = options, gpeDpt = gpecomp,
                     donnees = donnees, variables = variables)
  tab <- tabs$var
  tabd <- tab[,c("Annee","Territoire",nomvariable)] %>%
    filter(Territoire %in% c(dept,comp))


  # === Récupération des paramètres graphiques
  optloc <- optionsgraphiques(dept = dept, comp = comp, options = options)


  # === Récupération des données sur les distributions (quantiles, etc.)
  tabzones <- selectZones(nomvariable = nomvariable,
                          options = options,
                          nbrang = 1,
                          tab = tab %>% filter(TypeTerritoire == "Département"),
                          poidsobs = poidsobs)


  # === la table pour le graphique
  tabd2 <- tabd %>% pivot_wider(id_cols=c("Annee"),names_from="Territoire",values_from=nomvariable)

  tabg1 <- tabd2 %>% left_join(tabzones$quantiles, by="Annee")
  tabg1 <- tabg1[, c(names(tabd2), optloc$zoneaffiche)]

  tabg2 <- tabd[,c("Annee","Territoire",nomvariable)]
  names(tabg2) <- c("Annee","Territoire","indicateur")

  # === production des graphiques en output

   # === le graphique, version statique (ggplot)

  gstat <- ggplotAsdep() +
    geom_line(
      data=tabg2,
      aes(x=Annee,y=indicateur,colour=Territoire,label=paste(Territoire,", ",Annee,"<br>",indicateur," ",tabs$unitevar,sep="")),size=1)
  if (NROW(optloc$optionszones)>=1) {
    gstat <- gstat + #
      geom_ribbon(
        data=tabzones$zones,
        aes(ymin=ymin, ymax=ymax, x=Annee,
            fill=intitules, alpha=intitules,
            label=paste(intitules," : entre ",ymin," et ",ymax," ",tabs$unitevar,sep="")))
  }
  gstat <- gstat +  #
    guides(size = FALSE , alpha = FALSE) +
    scale_fill_manual(values = optloc$couleurs) +
    scale_color_manual(values = optloc$couleurs) +
    scale_alpha_manual(values = optloc$alphas) +
    labs(x = "année",
         y = paste("En",tabs$unitevar,sep=" "))


  # === le graphique, version dynamique (plotly)

  gdyn <- ggplotlyAsdep(gstat)

  # === objet en sortie de la fonction

  return(
    if (typesortie == "graph") {
      gstat
    } else if (typesortie == "tab") {
      tabg1
    } else if (typesortie == "tabcomplet") {
      list("tabgraph" = tabg2, "tablarge" = tabg1, "tabquantile" = tabzones$quantiles)
    } else if (typesortie == "graphdyn") {
      gdyn
    }
  )

}
