#' Fonction extrayant le contenu d'un onglet d'un fichier Excel diffusé sur data.drees
#'
#' Cette fonction sert à extraire le contenu d'un fichier Excel de données départementales diffusé sous data.Drees.
#' Le produit est une liste contenant un élément "tab" correspondant au tableau de données,
#' et divers éléments correspondant aux métadonnées ("source","note","champ", etc.)
#' Si le tableau "tab" contient des colonnes dont les intitulés correspondent à des années,
#' un autre élément "tablong" est disponible, dans lequel les années sont transposées en ligne plutôt qu'en colonnes.
#'
#' La fonction inclut des traitements complémentaires de mise en forme pour certains fichiers Excel
#' particuliers ('options' = "ASDEPslbenef", "ASDEPsldepenses", "OARSAsl", etc.)
#'
#' @param fich nom du fichier Excel
#' @param sheet nom de l'onglet
#' @param nlignetitre nombres de lignes pour les intitulés de colonnes : si une valeur est renseignée, les 'nlignetitre' premières lignes sont utilisées comme intitulés des colonnes
#' @param options type de fichier data.drees particulier ()
#'
#' @return une liste contenant un tableau de donnée (élément "tab") et des métadonnées (éléments "intitule","numtab","source","champ", etc.)
#' @export
#'
#' @examples readSheetDrees(fich="data-raw/Données mensuelles des prestations de solidarité.xlsx", sheet="Tableau 2" , nlignetitre=3)
#' @examples readSheetDrees(fich="data-raw/Les bénéficiaires de l aide sociale départementale - séries longues (1996-2018).xlsx", sheet="Tab6-pa" , nlignetitre=1)
#' @examples readSheetDrees(fich="data-raw/Minima sociaux - donnees departementales par dispositif.xlsx", sheet="Tableau 10", nlignetitre=1)
#' @examples readSheetDrees(fich="data-raw/OARSA – Principaux indicateurs de 2015 à 2018.xlsx", sheet="Tableau B10" , nlignetitre=1)
#' @examples readSheetDrees(fich="data-raw/Le personnel départemental de l'action sociale et médico-sociale de 2014 à 2018.xlsx", sheet="eff - pers medical" , nlignetitre=1)
readSheetDrees <- function(fich , sheet, nlignetitre, options = "") {

  # fich <- "data-raw/Données mensuelles des prestations de solidarité.xlsx"
  # sheet <- "Tableau 2"
  # fich <- "data-raw/Les bénéficiaires de l aide sociale départementale - séries longues (1996-2018).xlsx"
  # fich <- "data-raw/Minima sociaux - donnees departementales par dispositif.xlsx"
  # sheet <- getSheetNames(fich)[4]

  # ========================================
  # vérifications préliminaires
  if (!(sheet %in% getSheetNames(fich))) { stop("Erreur : onglet absent du fichier") }

  # ========================================
  # extraction des données de l'onglet
  tabcompl <- read.xlsx(fich, sheet = sheet,
                        colNames = FALSE,
                        skipEmptyRows = TRUE, skipEmptyCols = TRUE)
  if (ncol(tabcompl)<=1) { return(NULL) }

  # ========================================
  # initialisation de la liste de résultats
  result <- list(
    "fichiersource" = fich,
    "ongletsource" = sheet
  )


  # ========================================
  # suppression des infos inutiles
  inutile <- c("^(Retour au s|S)ommaire$","^(R|r)etour en haut de page$")
  for (i in 1:NROW(inutile)) { tabcompl <- tabcompl %>% mutate_all(function(x){ifelse(grepl(inutile[i],x),NA,x)}) }

  # ========================================
  # séparation des données
  lignesremplies <- rowSums(!is.na(tabcompl))
  info <- tabcompl[(lignesremplies == 1),]

  # ========================================
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

  # ========================================
  # traitements complémentaires pour certains types de data.drees spécifiques
  if (options == "ASDEPslbenef") {

  } else if (options == "ASDEPsldepenses") {

  } else if (options == "OARSAsl") {

  } else if (options == "MSsl") {

  } else if (options == "PrestaSolMens") {

  }

  # ========================================
  # enregistrement d'une 2 table de données (transposée) si des années sont détectées comme noms de colonne
  patternannee <- "^(19|20|21)[[:digit:]]{2}(\\.|[[:space:]]|\\*|\\(|$)"
  #patternannee <- "^(19|20|21)[[:digit:]]{2}[[:space:]]*(\\**|\\(([[:digit:]]|p|d|sd)\\))$"
  colsannee <- names(tab)[grepl(patternannee,names(tab))]
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

  # ========================================
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

  # ========================================
  result$tab <- tab
  return(result)
}
