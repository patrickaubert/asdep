#' Une fonction pour extraire et calculer un indicateur sur l'aide sociale
#'
#' @param nomvariable le nom de la variable, parmi celles de la table ASDEPsl
#' @param denom le nom d'une variable à utiliser au dénominateur de l'indicateur (si absent la variable est utilisée brute)
#' @param options un vecteur d'options du calcul
#' @param gpeDpt un vecteur de noms de départements : leur ensemble constituera le "groupe de comparaison"
#' @param donnees la table de données en entrée (par défaut, la table ASDEPsl)
#' @param variables la table de métadonnées de la table de données en entrée (par défaut, la table ASDEPsl_description)
#'
#' @return une liste contenant 4 éléments : 'var' = une table avec les territoires, les années et les valeurs de l'indicateur calculé; 'unitevar', 'unitevarbase' et 'nivarrond' = des métadonnées sur l'indicateur calculé
#' @export
#'
#' @examples selectIndic(nomvariable="NbBenefAPA",denom="pop.60.99")
#' @examples selectIndic(nomvariable="DepBruteAPA",denom="NbBenefAPA",options="mensuel")
#' @examples selectIndic(nomvariable="NbBenefAPA",denom="pop.60.99", gpeDpt=c("Meuse","Moselle"))
selectIndic <- function(nomvariable, denom = "",
                        options = "", gpeDpt = c(),
                        donnees = ASDEPsl, variables = ASDEPsl_description) {

  # ------------------- traitements préliminaires -------------------

  noms.variables.sortie <- c("Territoire","TypeTerritoire",nomvariable,"Annee")

  infovariable <- variables[variables$Nom.var == nomvariable,]

  # ===
  # interromp le calcul si certaines variables manquent dans la table
  if (!( ("Annee" %in% names(donnees)) & ("Territoire" %in% names(donnees)) & ("TypeTerritoire" %in% names(donnees)))) {
    return(list(var = donnees,
                unitevar = "# Attention : erreur #",
                unitevarbase = "# Attention : erreur #"))
  }

  # =====
  # liste des caractéristiques associées à chaque variables, d'après la table de métadonnées
  listetypevariables <- as.list(setNames(variables$Type.var, variables$Nom.var))

  # ===
  # cherche si un nom de variable est contenu parmi les "types", auquel cas il sera utilisé comme dénominateur de l'indicateur ("nomsvariables" est une variable globale, correspondant à un vecteur de tous les noms de variable existant)
  isdenom <- unique(intersect(names(donnees), denom))
  isdenom <- isdenom[!(isdenom %in% c("",NA))]
  if (length(isdenom)==1) {
    nomdenom <- isdenom[[1]]
    donnees$denom <- as.numeric(donnees[,c(nomdenom)])
    # si le numérateur est une dépense et le dénominateur un effectif, on remplace ce dernier par la "moyenne annuelle" (demi-somme des effectifs fin N et fin N-1)
    if ( ((listetypevariables[[nomdenom]] == "Montants")) & ((listetypevariables[[nomvariable]] %in% c("Nombres de bénéficiaires","Nombre de personnes"))) ) {
      donneeslag <- rename(donnees[,c("Annee","Territoire","denom")], denom_n_1 = denom)
      donneeslag$Annee <- donneeslag$Annee+1
      donnees <- merge(donnees, donneeslag[,c("Annee","Territoire","denom_n_1")], by=c("Annee","Territoire"), all.x=TRUE, all.y=FALSE)
      donnees$denom <- ( donnees$denom + donnees$denom_n_1 ) / 2
    }
  }

  # ===
  # on ne conserve que les données non manquantes
  nomsvarloc <- intersect( c(nomvariable, "denom", "Annee", "TypeTerritoire","Territoire"),
                           names(donnees) )
  donneesloc <- donnees[,c( nomsvarloc) ]
  #donneesloc <- donnees[complete.cases(donneesloc), ]
  donneesloc <- donnees[complete.cases(donneesloc), nomsvarloc]
  if (length(isdenom)==1) { donneesloc <- donneesloc[(donneesloc$denom>0),] }
  donneesloc[,c(nomvariable)] <- as.numeric(donneesloc[,c(nomvariable)])

  # ------------------- Unité monétaire (pour les variables en euros) -------------------
  #if ( (infovariable$Type.var == "Montants") & ("Euros de 2018" %in% typevariable) ) {
  #  donneesloc[,c(nomvariable)] <- donneesloc[,c(nomvariable)]/donneesloc$Prix2018
  #  if ( (nchar(nomdenom)>0) & (variables[(variables$Nom.var == nomdenom),"Type.var"] ==  "Montants")) {
  #    donneesloc$denom <- donneesloc$denom/donneesloc$Prix2018
  #  }
  #}

  # ------------------- Unité de temps -------------------
  if ( (infovariable$Type.var == "Montants") & (grepl("[Pp]ar mois|[Mm]ensuel",options)) ) {
    donneesloc[,c(nomvariable)] <- donneesloc[,c(nomvariable)]/12
    if ( (nchar(nomdenom)>0) & (variables[(variables$Nom.var == nomdenom),"Type.var"] ==  "Montants")) {
      donneesloc$denom <- donneesloc$denom/12
    }
  }

  # ------------------- Ajout du groupe de comparaison, si l'option est activée -------------------
  if (NROW(gpeDpt) >= 1) {
    donneescomp <- donneesloc %>%
      filter(Territoire %in% gpeDpt)
    donneescomp <- donneescomp[ , intersect( c(nomvariable, "denom", "Annee"), names(donneescomp) )]
    donneescomp <- donneescomp %>%
      group_by(Annee) %>%
      summarise_all(sum) %>%
      ungroup() %>%
      mutate(Territoire = "Groupe de comparaison",
             TypeTerritoire = "Groupe de départements")
    donneesloc <- rbind(donneesloc, donneescomp[,names(donneesloc)])
  }

  # ------------------- Dénominateur -------------------
  if ("nb" %in% denom) {
    donneesloc[,c(nomvariable)] <- as.numeric(donneesloc[,c(nomvariable)])
    unitevar <- "personnes"
  }  else if ("mont" %in% denom) {
    donneesloc[,c(nomvariable)] <- as.numeric(donneesloc[,c(nomvariable)])/1000000  # RQ : exprimé en millions d'euros
    unitevar <- "millions d'\u20AC"
  }  else if ("denom" %in% names(donneesloc)) {

    if (listetypevariables[[nomvariable]] %in% c("Nombres de bénéficiaires")) {
      donneesloc[,c(nomvariable)] <- 100* as.numeric(donneesloc[,c(nomvariable)])/donneesloc$denom
      if (nomdenom %in% noms.varpop)  {
        unitevar <- paste("% de la population",Intitulepop(nomdenom),sep="")
      }      else if (listetypevariables[[nomdenom]] == "Nombres de bénéficiaires")  {
        unitevar <- paste("% des ",variables[nomdenom,"TexteDenom"],sep="")
      }      else { unitevar <- "??" }
    }    else if (listetypevariables[[nomvariable]] %in% c("Montants")) {
      if (nomdenom %in% noms.varpop)  {
        donneesloc[,c(nomvariable)] <- donneesloc[,c(nomvariable)] / donneesloc$denom
        unitevar <- paste(infovariable$Unite.var," par habitant",Intitulepop(nomdenom),sep="")
      }      else if (listetypevariables[[nomdenom]] == "Nombres de bénéficiaires")  {
        donneesloc[,c(nomvariable)] <- donneesloc[,c(nomvariable)] / donneesloc$denom
        unitevar <- paste(infovariable$Unite.var," par ",variables[nomdenom,"TexteDenom"],sep="")
      }      else if (listetypevariables[[nomdenom]] == "Montants")  {
        donneesloc[,c(nomvariable)] <- 100 * donneesloc[,c(nomvariable)] / donneesloc$denom
        unitevar <- paste("% des ",variables[nomdenom,"TexteDenom"],sep="")
      }      else { unitevar <- paste(infovariable$Unite.var," ",infovariable$Texte.parbenef,sep="") }
    }
  }  else {
    unitevar <- infovariable$Unite.var
  }

  unitevarbase <- unitevar

  # ------------------- arrondit les résultats -------------------

  # RQ : arrondi à l'entier le plus proche pour éviter d'avoir un nombre de personnes non entier
  if ("nb" %in% denom) { niveau.d.arrondi <- 0
  }  else if (mean(donneesloc[,c(nomvariable)], na.rm=TRUE)<=5) { niveau.d.arrondi <- 2
  }  else if (mean(donneesloc[,c(nomvariable)], na.rm=TRUE)<=100) { niveau.d.arrondi <- 1
  }  else { niveau.d.arrondi <- 0 }
  donneesloc[,c(nomvariable)] <- round(donneesloc[,c(nomvariable)], niveau.d.arrondi)

  # ------------------- retourne le résultat

  return(list(var = donneesloc[,noms.variables.sortie],
              unitevar = unitevar,
              unitevarbase = unitevarbase,
              nivarrond = niveau.d.arrondi ))

}
