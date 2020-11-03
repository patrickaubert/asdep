#' Une fonction auxiliaire calculant les quantiles des distributions par département d'un indicateur
#'
#' @param donneesQV une table de données contant la variable d'intérêt (var),
#' @param var le nom de variable pour l'indicateur dont on souhaite calculer les quantiles
#' @param groupe un nom de variable pour segmenter le calcul par groupe (par exemple : "Annee")
#' @param poids une variable de pondération pour calculer des quantiles pondérés (par exemple la population : "popTOT")
#' @param liste.quantiles un vecteur contenant les quantiles qu'on veut inclure dans les résultats
#'
#' @return une table contenant les quantiles, les nombres de départements (et éventuellement part de la population) dans les zones interquartile et interdécile, ainsi que dans les zones autour de la médiane +/- 10 ou 20 %, et la variable "groupe"
#' @export
#'
#' @examples quantileIndic( donneesQV=selectIndic(nomvariable="NbBenefAPA",denom="pop.60.99", keepvar = c("pop.60.99"))$var, var="NbBenefAPA", groupe="Annee", poids="pop.60.99")
quantileIndic <- function(
  donneesQV,
  var,
  groupe,
  poids,
  liste.quantiles = c(0.05,0.10,0.25,0.5,0.75,0.9,0.95)) {

  #donnees <- donnees[(donnees$TypeTerritoire == "Département"),c("Annee","Territoire","popTOT","TotBenefPA")]
  #var="TotBenefPA"
  #groupe="Annee"
  #poids="popTOT"

  # ===
  # Création des variables d'intérêt

  donneesQV$var <- as.numeric(donneesQV[,c(var)])
  donneesQV$groupe <- as.numeric(donneesQV[,c(groupe)])
  if (!(is.na(poids))) { donneesQV$poids <- as.numeric(donneesQV[,c(poids)])
  }  else { donneesQV$poids <- rep(1,nrow(donneesQV))  }

  # ===
  # sélection des données pour l'analyse
  donneesQV <- donneesQV[(complete.cases(donneesQV[,c(var,groupe,poids)])),]
  donneesQV <- donneesQV[donneesQV$TypeTerritoire == "Département",]

  #liste.quantiles <- c(0.05,0.10,0.25,0.5,0.75,0.9,0.95)
  liste.quantiles <- unique( liste.quantiles, c(0.10,0.25,0.5,0.75,0.9) )
  liste.quantiles <- liste.quantiles[order(liste.quantiles)]
  noms.quantiles <- c( paste("p", round(100*liste.quantiles,0), sep="") )

  # === fonction auxiliaire utile
  PartEntre <- function(donneesPart, pondloc, groupeloc, val, valmin, valmax) {

    donneesPart[,c("val","valmin","valmax","groupeloc","pondloc")] <- cbind(donneesPart[,c(val)],
                                                                            donneesPart[,c(valmin)],
                                                                            donneesPart[,c(valmax)],
                                                                            donneesPart[,c(groupeloc)],
                                                                            donneesPart[,c(pondloc)]
    )

    fdivise <- function(x,y) {if ((y==0)||(is.na(x))) {return(0)} else {return(x/y)} }

    #tab   <- merge( dplyr::rename( aggregate(pondloc ~ groupe, donneesPart[( (donneesPart$val>=donneesPart$valmin) & (donneesPart$val<=donneesPart$valmax) ),], sum), num=pondloc ),
    #                dplyr::rename( aggregate(pondloc ~ groupe, donneesPart, sum), denom=pondloc ),
    #                by = "groupeloc")
    donnees.denom <- donneesPart %>%
      dplyr::group_by(groupeloc) %>%
      dplyr::summarize(denom = sum(pondloc, na.rm = TRUE))
    donnees.num <- donneesPart %>%
      dplyr::filter(val>=valmin & val<valmax) %>%
      dplyr::group_by(groupeloc) %>%
      dplyr::summarize(num = sum(pondloc, na.rm = TRUE))
    if (nrow(donnees.num)==0) { tab <- donnees.denom %>% mutate(num=0)
    } else { tab <- donnees.denom %>% left_join(donnees.num,by = "groupeloc")}

    #tab   <- dplyr::inner_join( (donneesPart[( (donneesPart$val>=donneesPart$valmin) & (donneesPart$val<=donneesPart$valmax) ),] %>% dplyr::group_by(groupeloc) %>% dplyr::summarize(num = sum(pondloc, na.rm = TRUE))) ,
    #                            (donneesPart %>% dplyr::group_by(groupeloc) %>% dplyr::summarize(denom = sum(pondloc, na.rm = TRUE))),
    #                            by = "groupeloc")

    #return(  tab$num/tab$denom)
    return(  mapply(fdivise, tab$num, tab$denom ) )
    # RQ : dans certain cas la variable est nulle pour toutes les observations (ex APA avant 2002) => on construit donc une fonction de division "élargie" pour éviter le division par 0

  } # fin de la fonction PartEntre

  # ===
  # quantiles par année, sans pondération des départements
  q1 <- do.call("rbind", tapply(donneesQV$var, donneesQV$groupe, quantile, liste.quantiles, na.rm=TRUE))
  q1 <- data.frame(names = row.names(q1), q1)
  colnames(q1) <- c(groupe, noms.quantiles)

  # quantiles par année, avec pondération des départements selon leur taille
  if (!(is.na(poids))) {
    q2 <- do.call("rbind", tapply(donneesQV$var, donneesQV$groupe, wtd.quantile, liste.quantiles, weight=donneesQV$poids, na.rm=TRUE))
    colnames(q2) <- paste(noms.quantiles,"pond",sep="")
  }

  # complément : zones autour de la médiane, part de la population dans chaque groupe

  q1$p50.m10 <- 0.9 * q1$p50
  q1$p50.p10 <- 1.1 * q1$p50
  q1$p50.m20 <- 0.8 * q1$p50
  q1$p50.p20 <- 1.2 * q1$p50

  donneesQV$nbdep <- rep(1,nrow(donneesQV))
  donneesQV <- merge(donneesQV, q1, by=groupe) #, all.x = TRUE, all.y = FALSE )
  # nb de départements dans la zone : médiane +/- 10 resp. 20 %
  q1$nbdep.p50.pm10 <- PartEntre(donneesQV, "nbdep", "groupe", "var", "p50.m10", "p50.p10")
  q1$nbdep.p50.pm20 <- PartEntre(donneesQV, "nbdep", "groupe", "var", "p50.m20", "p50.p20")
  # part de la population (variable "poids" en input) dans les zones : médiane +/- 10 resp. 20 %, et dans les zones interquartiles resp. interdéciles
  if (!(is.na(poids))) {
    q1$pond.p50.pm10 <- PartEntre(donneesQV, "poids", "groupe", "var", "p50.m10", "p50.p10")
    q1$pond.p50.pm20 <- PartEntre(donneesQV, "poids","groupe", "var", "p50.m20", "p50.p20")
    q1$pond.interquart <- PartEntre(donneesQV, "poids", "groupe", "var", "p25", "p75")
    q1$pond.interdec <- PartEntre(donneesQV, "poids", "groupe", "var", "p10", "p90")
    q1 <- cbind(q1,q2)
  }

  return( q1 )
}
