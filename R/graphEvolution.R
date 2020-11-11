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
#' @param gpecomp un vecteur de noms de départements : leur ensemble constituera le "groupe de comparaison" (si cette variable est renseignée, la variable 'comp' est automatiquement égale à "groupe de comparaison")
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
graphEvolution <- function(nomvariable, denom = "", options = c(), poidsobs = c(),
                           dept, comp = "TOTAL estimé France entière (hors Mayotte)", gpecomp = c(),
                           typesortie = "graph",
                           donnees = ASDEPsl, variables = ASDEPsl_description) {

  # === Récupération des données pour l'indicateur
  if (NROW(gpecomp)>=1) {comp <- "Groupe de comparaison"}
  if (nchar(denom)>0) {poidsobs <- c(poidsobs,denom)}
  tabs <- selectIndic(nomvariable = nomvariable, denom = denom, keepvar=c(poidsobs),
                     options = options, gpeDpt = gpecomp,
                     donnees = donnees, variables = variables)
  tab <- tabs$var
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

  # === production des graphiques en output

  # récupération des paramètres graphiques

  optionszones <- intersect(options,ParamGraphiquesAsdep$noms)

  couleursloc <- ParamGraphiquesAsdep[c("dept","comp","autres",optionszones),"couleur"]
  names(couleursloc) <- c(dept, comp, "Autres départements",ParamGraphiquesAsdep[c(optionszones),"intitules"])

  alphasloc <- ParamGraphiquesAsdep[c("dept","comp","autres",optionszones),"alpha"]
  names(alphasloc) <- c(dept, comp, "Autres départements",ParamGraphiquesAsdep[c(optionszones),"intitules"])

  # table avec les zones représentées sur le graphique

  zonesloc <- ParamGraphiquesAsdep %>% filter(noms %in% options)
  typezone <- function(tab,defzone) {
    t <- tab
  }
  tabq3 <- data.frame()
  for (i in 1:nrow(zonesloc)){
    tabq3 <- rbind(tabq3,
                   data.frame(
                     Annee = tabq$Annee,
                     intitules = rep(zonesloc$intitules[i] , nrow(tabq)),
                     noms = rep(zonesloc$noms[i] , nrow(tabq)),
                     ymin = tabq[,zonesloc$ymin[i]],
                     ymax = tabq[,zonesloc$ymax[i]],
                     alpha = rep(zonesloc$alpha[i] , nrow(tabq))
                   ))
  }

  #couleursloc <- c("Zone interdécile" = "blue",
  #                 "Zone interquartile" = "blue",
  #                 "Zone interdécile (pondérée)" = "green",
  #                 "Zone interquartile (pondérée)" = "green",
  #                 "Médiane +/- 20 %" = "red",
  #                 "Médiane +/- 10 %" = "red"
  #                 )
  #zones <- data.frame(
  #  intitules = c("Zone interdécile","Zone interquartile","Zone interdécile (pondérée)","Zone interquartile (pondérée)","Médiane +/- 20 %","Médiane +/- 10 %"),
  #  noms = c("interdeciles","interquartiles","interdecilespond","interquartilespond","medianePM20","medianePM10"),
  #  ymin = c("p10","p25","p10pond","p25pond","p50.m20","p50.m10"),
  #  ymax = c("p90","p75","p90pond","p75pond","p50.p20","p50.p10"),
  #  alpha = c(0.1, 0.2, 0.1, 0.2, 0.1, 0.2),
  #  couleur = c("blue","blue","green","green","red","red"),
  #  stringsAsFactors = FALSE
  #)
  #zonesloc <- zones %>% filter(noms %in% options)
#

  # === le graphique, version statique (ggplot)

  gstat <- ggplotAsdep() +
    geom_line(
      data=tabg2,
      aes(x=Annee,y=indicateur,colour=Territoire),size=1) + #,text=paste(Territoire,", ",Annee,"<br>",indicateur," ",tabs$unitevar,sep="")
    geom_ribbon(
      data=tabq3,
      aes(ymin=ymin, ymax=ymax, x=Annee, fill=intitules, alpha=intitules)) +  # , text=paste(intitules," : entre ",ymin," et ",ymax," ",tabs$unitevar,sep="")
    scale_fill_manual(values = couleursloc) +
    scale_color_manual(values = couleursloc) +
    scale_alpha_manual(values = alphasloc) +
    guides(size = FALSE , alpha = FALSE) +
    labs(x = "année",
         y = paste("En",tabs$unitevar,sep=" "))

  #if ("interdeciles" %in% options) {gstat <- gstat + geom_ribbon(data=tabq,aes(ymin=p10, ymax=p90, x=Annee, fill="Zone interdécile"), alpha = 0.1)}
  #if ("interquartiles" %in% options) {gstat <- gstat + geom_ribbon(data=tabq,aes(ymin=p25, ymax=p75, x=Annee, fill="Zone interquartile"), alpha = 0.2)}
  #if ("interdecilespond" %in% options) {gstat <- gstat + geom_ribbon(data=tabq,aes(ymin=p10pond, ymax=p90pond, x=Annee, fill="Zone interdécile (pondérée)"), alpha = 0.1)}
  #if ("interquartilespond" %in% options) {gstat <- gstat + geom_ribbon(data=tabq,aes(ymin=p25pond, ymax=p75pond, x=Annee, fill="Zone interquartile (pondérée)"), alpha = 0.2)}
  #if ("medianePM20" %in% options) {gstat <- gstat + geom_ribbon(data=tabq,aes(ymin=p50.m20, ymax=p50.p20, x=Annee, fill="Médiane +/- 20 %"), alpha = 0.1)}
  #if ("medianePM10" %in% options) {gstat <- gstat + geom_ribbon(data=tabq,aes(ymin=p50.m10, ymax=p50.p10, x=Annee, fill="Médiane +/- 10 %"), alpha = 0.2)}
  #for (z in 1:nrow(zonesloc)) {
  #  gstat <- gstat +
  #    geom_ribbon(
  #      #data=tabq,
  #      aes(ymin= tabq[,zonesloc[z,"ymin"]],
  #          ymax= tabq[,zonesloc[z,"ymax"]],
  #          x=tabq$Annee,
  #          fill=zonesloc[z,"intitules"]),
  #      alpha = zonesloc[z,"alpha"])
  #}

  # === le graphique, version dynamique (plotly)

  #gdyn <- ggplotly(g)
  #if ("interdeciles" %in% options){
  #  gdyn <- gdyn  %>% add_ribbons(data=tabq, x = ~Annee, ymin = ~p10, ymax = ~p90, line = list(color = 'rgba(7, 164, 181, 0.05)'),  fillcolor = 'rgba(7, 164, 181, 0.2)',
  #                                  name = "zone interdécile", text = ~paste("80 % des départements (",round(100*pond.interdec,0)," % de la population) entre ",round(p10,1)," et ",round(p90,1),", en ",Annee,sep=""), hoverinfo ="text" )
  #}

  gdyn <- ggplotlyAsdep(gstat)

  # === objet en sortie de la fonction

  return(
    if (typesortie == "graph") {
      gstat
    } else if (typesortie == "tab") {
      tabg1
    } else if (typesortie == "tabcomplet") {
      list("tabgraph" = tabg2, "tablarge" = tabg1, "tabquantile" = tabq)
    } else if (typesortie == "graphdyn") {
      gdyn
    }
  )

}
