#' Une fonction extrayant le contenu d'un onglet d'un fichier Excel diffusé sur data.drees
#'
#' @param fich nom du fichier Excel
#' @param sheet nom de l'onglet
#' @param nlignetitre nombres de lignes pour les intitulés de colonnes : si une valeur est renseignée, les 'nlignetitre' premières lignes sont utilisées comme intitulés des colonnes
#'
#' @return une liste contenant un tableau de donnée (élément "tab") et des métadonnées (éléments "intitule","numtab","source","champ", etc.)
#' @export
#'
#' @examples readSheetDrees(fich="data-raw/Données mensuelles des prestations de solidarité.xlsx", sheet="Tableau 2" , nlignetitre=3)
#' @examples readSheetDrees(fich="data-raw/Les bénéficiaires de l aide sociale départementale - séries longues (1996-2018).xlsx", sheet="Tab6-pa" , nlignetitre=1)
#' @examples readSheetDrees(fich="data-raw/Minima sociaux - donnees departementales par dispositif.xlsx", sheet="Tableau 17", nlignetitre=2)
#' @examples readSheetDrees(fich="data-raw/OARSA – Principaux indicateurs de 2015 à 2018.xlsx", sheet="Tableau B10" , nlignetitre=1)
readSheetDrees <- function(fich , sheet, nlignetitre ) {

  # fich <- "data-raw/Données mensuelles des prestations de solidarité.xlsx"
  # sheet <- "Tableau 2"
  # fich <- "data-raw/Les bénéficiaires de l aide sociale départementale - séries longues (1996-2018).xlsx"
  # fich <- "data-raw/Minima sociaux - donnees departementales par dispositif.xlsx"
  # sheet <- getSheetNames(fich)[4]


  # vérifications préliminaires
  if (!(sheet %in% getSheetNames(fich))) { stop("Erreur : onglet absent du fichier") }

  # extraction des données de l'onglet
  tabcompl <- read.xlsx(fich, sheet = sheet,
                        colNames = FALSE,
                        skipEmptyRows = TRUE, skipEmptyCols = TRUE)
  if (ncol(tabcompl)<=1) { return(NULL) }
  result <- list()

  # suppression des infos inutiles
  inutile <- c("^(Retour au s|S)ommaire$","^(R|r)etour en haut de page$")
  for (i in 1:NROW(inutile)) { tabcompl <- tabcompl %>% mutate_all(function(x){ifelse(grepl(inutile[i],x),NA,x)}) }

  # séparation des données
  lignesremplies <- rowSums(!is.na(tabcompl))
  info <- tabcompl[(lignesremplies == 1),]

  # lecture et traitement de la table de données
  tab <- tabcompl[(lignesremplies > 1),]
  tab <- tab[,(colSums(is.na(tab))<nrow(tab))]
  if (!is.null(nlignetitre)) {
    # ligne des titres de colonnes : extraction et traitement
    titres <- tab[1:nlignetitre,]
    if (nlignetitre>1) {
      for (i in 1:(nlignetitre-1)) {
        rempl <- NA
        for (j in 1:ncol(titres)) {
          rempl <- ifelse(!is.na(titres[i,j]),titres[i,j],rempl)
          titres[i,j] <- ifelse(is.na(titres[i,j]),rempl,titres[i,j])
        }
      }
      titres <- t(titres %>% summarise_all(function(x){paste(x,collapse=".")}))
    }
    titres <- as.character(titres) %>% stringi::stri_trans_general("Latin-ASCII")
    # assigne les titres comme noms de colonnes
    tab <- tab[(nlignetitre+1):nrow(tab),]
    names(tab) <- titres
  }
  tab <- tab %>%
    mutate_all(function(x){ifelse(grepl("^([Nn][DdRrSsCc]|/)[[:space:]]*$",x),NA,x)}) %>%
    distinct()
  result$tab <- tab

  # enregistrement d'une 2 table de données (transposée) si des années sont détectées comme noms de colonne
  colsannee <- names(tab)[grepl("^(19|20|21)[[:digit:]]{2}[[:space:]]*(\\**|\\(([[:digit:]]|p|d|sd)\\))$",names(tab))]
  if (NROW(colsannee)>=1) {
    tablong <- tab %>%
      mutate_at(vars(colsannee),as.numeric) %>%
      pivot_longer(cols=c(colsannee),
                   names_to="annee",
                   values_to=paste0("valeur_sheet_",sheet),
                   values_drop_na = TRUE) %>%
      distinct()
    result$tablong <- tablong
  }

  # lecture des métadonnées
  info <- info[,(colSums(is.na(info))<nrow(info))]
  info <- as.vector(t(info))
  info <- info[!is.na(info)]
  info <- unique(info)

  rubriques <- data.frame(
    rubrique = c("intitule","note","source","champ","lecture"),
    txt = c("[Tt]ableau [[:alnum:]]+","[Nn]ote(s|)","[Ss]ource(s|)","[Cc]hamp","[Ll]ecture"),
    stringsAsFactors = FALSE
  )
  debut <- paste0("^(",paste(rubriques$txt,collapse="|"),")")
  left <- str_extract(info, paste0(debut,"(?=[[:space:]]*(\\:|\\-)*[[:space:]]*)"))
  right <- gsub(paste0(debut,"[[:space:]]*(\\:|\\-)*[[:space:]]*"),"",info)
  cat <- "info"
  for (i in 1:NROW(info)) {
    if (grepl(rubriques$txt[1],info[i])) { result[["tabnum"]] <- left[i] }
    for (j in 1:nrow(rubriques)) {
      if (grepl(rubriques$txt[j],left[i])) { left[i] <- rubriques$rubrique[j] }
      if (left[i] %in% rubriques[rubriques$rubrique != "intitule","rubrique"]) { cat <- left[i] }
    }
    if (is.na(left[i])) { left[i] <- cat }
  }
  infosdispo <- unique(left)
  for (k in 1:NROW(infosdispo)) {
    result[[infosdispo[k]]] <- paste(right[left==infosdispo[k]],collapse="\n")
  }

  return(result)
}
