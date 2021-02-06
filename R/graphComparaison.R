#' Fonction produisant un graphique comparant, pour une année donnée, les valeurs par département d'un indicateur d'aide sociale
#'
#' Cette fonction sert à produire un graphique (au format ggplot2 ou plotly) représentant l'évolution au cours du temps d'un indicateur pour un département de référence, et un territoire de comparaisons (par exemple : la France entière, la région du département de référence, un groupe de département).
#' Le tableau sous-jacent peut également être récupéré avec l'option typesortie = "tab".
#' Les options permettent d'ajouter sur le graphique une représentation des diverses zones : zone interquartile ("interquartiles"), zone interdécile ("interdeciles"), zone correspondant à la médiane +/- 10 ou 20 % ("medianePM10" et "medianePM20") ...
#'
#' @param nomvariable le nom de la variable, parmi celles de la table ASDEPsl
#' @param denom le nom d'une variable à utiliser au dénominateur de l'indicateur (si absent la variable est utilisée brute)
#' @param annee l'année de référence pour le calcul de l'indicateur (par défaut, l'année la plus récente pour laquelle l'indicateur 'nomvariable' est connu)
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
#' @examples graphComparaison(nomvariable="NbBenefAPA",dept="Vosges",comp="Grand Est",typesortie="tab")
#' @examples graphComparaison(nomvariable="NbBenefAPA",denom="pop.60.99",dept="Vosges",comp="Grand Est",options=c("interquartiles","interdeciles","medianePM10"))
#' @examples graphComparaison(nomvariable="NbBenefAPA",denom="pop.60.99",dept="Vosges",gpecomp=c("Meuse","Moselle"),options=c("medianePM10","medianePM20"),typesortie="graphdyn")
graphComparaison <- function(
  nomvariable, denom = "",
  annee = max(ASDEPsl[!is.na(ASDEPsl[,nomvariable]),"Annee"],na.rm=TRUE),
  options = c(), poidsobs = c(),
  dept, comp = "TOTAL estimé France entière (hors Mayotte)", gpecomp = c(),
  typesortie = "graph",
  donnees = ASDEPsl, variables = ASDEPsl_description) {

  # === Récupération des données pour l'indicateur

  #if (NROW(gpecomp)>=1) {comp <- "Groupe de comparaison"}
  if (nchar(denom)>0) {poidsobs <- c(poidsobs,denom)}

  # détermine la population de référence (pour la taille des bulles sur le graphique, celles-ci étant proportionnelles à la population)
  if (denom %in% names(PopDepartementales)) { popref<-denom
  } else {
    pop <- paste("pop.",gsub("\\-",".",ASDEPsl_description[nomvariable,"Popref.var"]),sep="")
    popref <- if (pop %in% names(PopDepartementales)) { pop } else {"popTOT"}
  }

  tabs <- selectIndic(nomvariable = nomvariable, denom = denom, keepvar=c(poidsobs,popref),
                      options = options, gpeDpt = gpecomp,
                      donnees = donnees, variables = variables)
  tab <- tabs$var %>% filter(Annee == annee)
  tab <- tab[!is.na(tab[,popref]),] %>%
    mutate(size = sqrt(tab[,popref]/40) )

  tabd <- tab[,c("TypeTerritoire","Territoire","size",nomvariable)] %>%
    filter(TypeTerritoire == "Département") %>%
    arrange(Territoire)

  # les tables pour le graphique
  tabg <- tabd[,c("Territoire","size",nomvariable)]
  names(tabg) <- c("Territoire","size","indicateur")
  tabg <- tabg  %>%
    arrange(indicateur) %>%
    mutate(rang = 1:n(),
           type = case_when(
             Territoire == dept ~ dept,
             Territoire %in% gpecomp ~ comp,
             TRUE ~ "Autres départements"))

  # === Récupération des paramètres graphiques
  optloc <- optionsgraphiques(dept = dept, comp = comp, options = options)

  # === Récupération des données sur les distributions (quantiles, etc.)
  tabzones <- selectZones(nomvariable = nomvariable,
                          options = options,
                          nbrang = nrow(tabd),
                          tab = tab %>% filter(TypeTerritoire == "Département"),
                          poidsobs = poidsobs)

  # === production des graphiques en output

  # === le graphique, version statique (ggplot)

  gstat <- ggplotAsdep() +
    geom_point(data=tabg,
               aes(x=rang,y=indicateur,
                   size=size,colour=type,alpha=type,
                   text=paste(Territoire," en ",annee," :<br>",indicateur," ",tabs$unitevar,sep="")))

  if (nrow(tabzones$zones)>=1) {
    gstat <- gstat +
      geom_ribbon(data=tabzones$zones,
                  aes(ymin=ymin, ymax=ymax, x=rang,
                      fill=intitules, alpha=intitules,
                      text=paste(intitules," : entre ",ymin," et ",ymax," ",tabs$unitevar,sep="")))
  }

  gstat <- gstat +
    guides(size = FALSE , alpha = FALSE) +
    scale_fill_manual(values = optloc$couleurs) +
    scale_color_manual(values = optloc$couleurs) +
    scale_alpha_manual(values = optloc$alphas) +
    labs(x = paste("rang (par ordre croissant) en",annee,sep=" "),
         y = paste("En",tabs$unitevar,sep=" "))

  # === le graphique, version dynamique (plotly)

  gdyn <- ggplotlyAsdep(gstat)

  # === objet en sortie de la fonction

  return(
    if (typesortie == "graph") {
      gstat
    } else if (typesortie == "tab") {
      tabg
    } else if (typesortie == "tabcomplet") {
      list("tabgraph" = tabg, "tabquantile" = tabzones$quantiles)
    } else if (typesortie == "graphdyn") {
      gdyn
    }
  )

}
