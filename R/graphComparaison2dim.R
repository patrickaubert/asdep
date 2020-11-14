#' Fonction produisant un graphique comparant, pour une année donnée et sous la forme d'un nuage de points, les valeurs départementales de deux indicateurs d'aide sociale
#'
#' Cette fonction sert à produire un graphique (au format ggplot2 ou plotly) représentant sous la forme d'un nuage de point les valeurs de deux indicateurs, pour l'ensemble des départements français (en faisant ressortir plus spécifiquement un département de référence et des départements de comparaison)
#' Le tableau sous-jacent peut également être récupéré avec l'option typesortie = "tab".
#' Les options permettent d'ajouter sur le graphique une représentation des diverses zones : zone interquartile ("interquartiles"), zone interdécile ("interdeciles"), zone correspondant à la médiane +/- 10 ou 20 % ("medianePM10" et "medianePM20") ...
#'
#' @param nomvariable1 le nom de la variable pour le premier indicateur, parmi celles de la table ASDEPsl
#' @param denom1 le nom d'une variable à utiliser au dénominateur du premier indicateur (si absent la variable est utilisée brute)
#' @param nomvariable2 le nom de la variable pour le deuxième indicateur, parmi celles de la table ASDEPsl
#' @param denom2 le nom d'une variable à utiliser au dénominateur du deuxième indicateur (si absent la variable est utilisée brute)
#' @param denom le nom d'une variable à utiliser au dénominateur de chacun des deux indicateurs
#' @param annee l'année de référence pour le calcul de l'indicateur (par défaut, l'année la plus récente pour laquelle l'indicateur 'nomvariable' est connu)
#' @param poidsobs une variable de pondération (par exemple si on souhaite calculer des quantiles pondérés par la population des départements). Par défaut, si denom est renseigné, cette variable denom est utilisée comme poidsobs
#' @param options un vecteur d'options du calcul (valeurs en euros courant ou constant, mensuels ou annuels, etc.)
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
#' @examples graphComparaison2dim(nomvariable="NbBenefAPA",dept="Vosges",comp="Grand Est",typesortie="tab")
#' @examples graphComparaison2dim(nomvariable1="NbBenefAPADomicile",denom="pop.60.99",nomvariable2="NbBenefAPAEtab",dept="Vosges",comp="Grand Est")
#' @examples graphComparaison2dim(nomvariable="TotBenefPA",denom1="pop.60.99",nomvariable2="TotBenefPH",denom2="pop.20.64",dept="Vosges",gpecomp=c("Meuse","Moselle"),typesortie="graphdyn")
graphComparaison2dim <- function(
  nomvariable1, nomvariable2,
  denom = "", denom1 = denom,  denom2 = denom,
  annee = max(ASDEPsl[is.na(ASDEPsl[,c(nomvariable1,nomvariable2)]),"Annee"],na.rm=TRUE),
  options = c(), poidsobs = c(),
  dept, comp = "TOTAL estimé France entière (hors Mayotte)", gpecomp = c(),
  typesortie = "graph",
  donnees = ASDEPsl, variables = ASDEPsl_description) {

  # === Récupération des données pour l'indicateur

  if (NROW(gpecomp)>=1) {comp <- "Groupe de comparaison"}
  if (nchar(denom)>0) {poidsobs <- c(poidsobs,denom)}

  # détermine la population de référence (pour la taille des bulles sur le graphique, celles-ci étant proportionnelles à la population)
  if (denom1 %in% names(PopDepartementales)) { popref<-denom1
  } else {
    pop <- paste("pop.",gsub("\\-",".",ASDEPsl_description[nomvariable1,"Popref.var"]),sep="")
    popref <- if (pop %in% names(PopDepartementales)) { pop } else {"popTOT"}
  }

  # premier indicateur
  tabs1 <- selectIndic(nomvariable = nomvariable1, denom = denom1, keepvar=unique(c(poidsobs,popref)),
                      options = options, gpeDpt = gpecomp,
                      donnees = donnees, variables = variables)
  tab1 <- tabs1$var %>% filter(Annee == annee)
  tab1 <- tab1[!is.na(tab1[,popref]),] %>%
    mutate(size = sqrt(tab1[,popref]/40) )
  tabd1 <- tab1[,c("TypeTerritoire","Territoire","size",nomvariable1)] %>%
    filter(TypeTerritoire == "Département") %>%
    arrange(Territoire) %>%
    select(-TypeTerritoire)

  # deuxième indicateur
  tabs2 <- selectIndic(nomvariable = nomvariable2, denom = denom2, keepvar=unique(c(poidsobs,popref)),
                       options = options, gpeDpt = gpecomp,
                       donnees = donnees, variables = variables)
  tab2 <- tabs2$var %>% filter(Annee == annee)
  tab2 <- tab2[!is.na(tab1[,popref]),]
  tabd2 <- tab2[,c("TypeTerritoire","Territoire",nomvariable2)] %>%
    filter(TypeTerritoire == "Département") %>%
    arrange(Territoire) %>%
    select(-TypeTerritoire)

  # table avec les deux indicateurs
  tabd <- tabd1 %>% left_join(tabd2, by="Territoire")

  # === Récupération des données sur les distributions (quantiles, etc.)
  #tabq <- quantileIndic(donneesQV = tab %>% filter(TypeTerritoire == "Département"),
  #                      var=nomvariable1,  groupe="Annee",  poids=poidsobs) %>%
  #  mutate(Annee = as.numeric(as.character(Annee)))
#
  #zoneaffiche <- c()
  #if ("interdeciles" %in% options) zoneaffiche <- c(zoneaffiche,"p10","p90")
  #if ("interquartiles" %in% options) zoneaffiche <- c(zoneaffiche,"p25","p75")
  #if ("interdecilespond" %in% options) zoneaffiche <- c(zoneaffiche,"p10pond","p90pond")
  #if ("interquartilespond" %in% options) zoneaffiche <- c(zoneaffiche,"p25pond","p75pond")
  #if ("medianePM10" %in% options) zoneaffiche <- c(zoneaffiche,"p50.m10","p50.p10")
  #if ("medianePM20" %in% options) zoneaffiche <- c(zoneaffiche,"p50.m20","p50.p20")

  # === les tables pour le graphique
  tabg <- tabd[,c("Territoire","size",nomvariable1,nomvariable2)]
  names(tabg) <- c("Territoire","size","indicateur1","indicateur2")
  tabg <- tabg  %>%
    mutate(type = case_when(
             Territoire == dept ~ dept,
             Territoire %in% gpecomp ~ comp,
             TRUE ~ "Autres départements"))
  tabg2 <- tabg %>%
    mutate(commentaire = paste(Territoire," (",annee,") :<br>",indicateur1," ",tabs1$unitevar,"<br>",indicateur2," ",tabs2$unitevar,sep=""))

  #tabq2 <- tabq %>%
  #  slice(rep(1:n(), each = nrow(tabg))) %>%
  #  mutate(rang = 1:n())

  # === production des graphiques en output

  # récupération des paramètres graphiques

  optionszones <- intersect(options,ParamGraphiquesAsdep$noms)

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

  # table avec les zones représentées sur le graphique

  zonesloc <- ParamGraphiquesAsdep %>% filter(noms %in% options)
  typezone <- function(tab,defzone) {
    t <- tab
  }
  tabq3 <- data.frame()
  #for (i in 1:nrow(zonesloc)){
  #  tabq3 <- rbind(tabq3,
  #                 data.frame(
  #                   rang = tabq2$rang,
  #                   intitules = rep(zonesloc$intitules[i] , nrow(tabq2)),
  #                   noms = rep(zonesloc$noms[i] , nrow(tabq2)),
  #                   ymin = tabq2[,zonesloc$ymin[i]],
  #                   ymax = tabq2[,zonesloc$ymax[i]],
  #                   alpha = rep(zonesloc$alpha[i] , nrow(tabq2))
  #                 ))
  #}

  # === le graphique, version statique (ggplot)

  gstat <- ggplotAsdep() +
    geom_point(data=tabg2,
               aes(x=indicateur1,y=indicateur2,size=size,colour=type,alpha=type,
                   text=commentaire)) +
    #geom_ribbon(data=tabq3,
    #            aes(ymin=ymin, ymax=ymax, x=rang, fill=intitules, alpha=intitules, text=paste(intitules," : entre ",ymin," et ",ymax," ",tabs$unitevar,sep=""))) +
    guides(size = FALSE , alpha = FALSE) +
    scale_fill_manual(values = couleursloc) +
    scale_color_manual(values = couleursloc) +
    scale_alpha_manual(values = alphasloc) +
    labs(x = paste("En",tabs1$unitevar,sep=" "),
         y = paste("En",tabs2$unitevar,sep=" "))

  # === le graphique, version dynamique (plotly)

  gdyn <- ggplotlyAsdep(gstat)

  # === objet en sortie de la fonction

  return(
    if (typesortie == "graph") {
      gstat
    } else if (typesortie == "tab") {
      tabg
    } else if (typesortie == "tabcomplet") {
      list("tabgraph" = tabg, "tabquantile" = tabq)
    } else if (typesortie == "graphdyn") {
      gdyn
    }
  )

}
