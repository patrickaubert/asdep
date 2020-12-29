#' Fonction extrayant le contenu d'un onglet d'un fichier Excel diffusé sur data.drees
#'
#' Cette fonction sert à extraire le contenu d'un onglet d'un fichier Excel de données départementales diffusé sous data.Drees.
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
#' @param options type de fichier data.drees particulier ("ASDEPslbenef", "ASDEPsldepenses", "OARSAsl", etc.)
#'
#' @return une liste contenant un tableau de donnée (élément "tab") et des métadonnées (éléments "intitule","numtab","source","champ", etc.)
#' @export
#'
#' @examples readSheetDrees(fich="data-raw/Données mensuelles des prestations de solidarité.xlsx", sheet="Tableau 1" , nlignetitre=2, options="PrestaSolMens")
#' @examples readSheetDrees(fich="data-raw/Données mensuelles des prestations de solidarité.xlsx", sheet="Tableau 2" , nlignetitre=3, options="PrestaSolMens")
#' @examples readSheetDrees(fich="data-raw/Les bénéficiaires de l aide sociale départementale - séries longues (1996-2018).xlsx", sheet="Tab6-pa" , options = "ASDEPslbenef", nlignetitre=1)
#' @examples readSheetDrees(fich="data-raw/Les dépenses d aide sociale départementale - séries longues (1999 -2018).xlsx", sheet="PA-tab3" , options = "ASDEPsldepenses", nlignetitre=1)
#' @examples readSheetDrees(fich="data-raw/Minima sociaux - donnees departementales par dispositif.xlsx", sheet="Tableau 10", nlignetitre=1, options="minsocsl")
#' @examples readSheetDrees(fich="data-raw/Minima sociaux - donnees departementales par dispositif.xlsx", sheet="Tableau 11", nlignetitre=2, options="minsocsl")
#' @examples readSheetDrees(fich="data-raw/OARSA – Principaux indicateurs de 2015 à 2018.xlsx", sheet="Tableau B10" , nlignetitre=1)
#' @examples readSheetDrees(fich="data-raw/Le personnel départemental de l'action sociale et médico-sociale de 2014 à 2018.xlsx", sheet="eff - pers medical" , options = "ASDEPslperso", nlignetitre=1)
readSheetDrees <- function(fich , sheet, nlignetitre = NULL, options = "") {

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
  inutile <- c("^[[:punct:][:space:]]*([Rr]etour au s|[Rr]etour s|S)ommaire$",
               "^(R|r)etour en haut de page$")
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

  options <- tolower(options)
  if (options %in% c("asdepslbenef","asdepsldepenses")) {

    # fichier Excel "bénéficiaires de l'aide sociale séries longues"
    names(tab)[grepl("^[Cc]ode(.*)[Rr][ée]gion$",names(tab))] <- "Code.region"
    names(tab)[grepl("^[Cc]ode(.*)[Dd][ée]partement$",names(tab))] <- "Code.departement"
    names(tab)[grepl("^[Dd][ée]partement($|s$)",names(tab))] <- "Territoire"

    tab <- tab %>%
      mutate_at(vars(-c("Code.region","Code.departement","Territoire")),as.numeric)

    departements <- tab %>%
      filter(grepl("^[[:digit:]][AB[:digit:]]($|[[:digit:]]$|[MDmd]$)",Code.departement)) %>%
      mutate(TypeTerritoire = "Département")
    regions <- tab  %>%
      filter(!grepl("^[[:digit:]][AB[:digit:]]($|[[:digit:]]$|[MDmd]$)",Code.departement)) %>%
      filter(grepl("^[[:digit:]]{2,3}$",Code.region)) %>%
      select(-Territoire) %>%
      rename(Territoire = Code.departement) %>%
      mutate(TypeTerritoire = "Région",
             Code.departement = NA)
    nation <- tab  %>%
      filter(!grepl("^[[:digit:]][AB[:digit:]]($|[[:digit:]]$|[MDmd]$)",Code.departement)) %>%
      filter(!grepl("^[[:digit:]]{2,3}$",Code.region)) %>%
      filter(!grepl("^[Cc]ode(.*)[Rr][ée]gion$",Code.region)) %>%
      select(-c(Territoire,Code.departement)) %>%
      rename(Territoire = Code.region) %>%
      mutate(TypeTerritoire = "France",
             Code.region = NA,
             Code.departement = NA)

    tab <- bind_rows(departements, regions, nation)

  } else if (options %in% c("asdepslperso","asdepslpersonnel","asdepslpersonnels")) {

    # Fichier Excel "personnels de l'aide sociale"
    names(tab)[grepl("^Numero de departement$",names(tab))] <- "Code.departement"
    names(tab)[grepl("^[Dd][ée]partement($|s$)",names(tab))] <- "Territoire"
    tab <- tab %>% mutate(TypeTerritoire = "Département")

  } else if (options %in% c("oarsasl")) {

  } else if (options %in% c("mssl","minsocsl","minsoc")) {

    # fichier Excel "Minima sociaux par département"
    names(tab)[grepl("^N° Dep",names(tab))] <- "Code.departement"
    names(tab)[grepl("^Libelle Dep",names(tab))] <- "Territoire"
    # noms de colonnes pour le RSA
    nomsrsa <- names(tab)[grepl("^[^\\.]*RSA[^\\.]*\\.[[:digit:]]{4}",names(tab))]
    names(tab)[names(tab) %in% nomsrsa] <- paste(gsub("^[^\\.]*RSA[^\\.]*\\.","",nomsrsa),
                                                 str_extract(nomsrsa,"^[^\\.]*RSA[^\\.]*(?=\\.)"),
                                                 sep=".")
    oknoms <- c("Code.departement","Territoire",names(tab)[grepl("^[[:digit:]]{4}",names(tab))])
    tab <- tab[ ,c(oknoms)]

    tab <- tab %>%
      mutate_at(vars(-c("Code.departement","Territoire")),function(x){ifelse(x %in% c("-","ns","nd","n"),0,x)}) %>%
      mutate_at(vars(-c("Code.departement","Territoire")),as.numeric)

    departements <- tab %>%
      filter(grepl("^[[:digit:]]+[AB[:digit:]]($|[[:digit:]]$|[MDmd]$)",Code.departement)) %>%
      mutate(TypeTerritoire = "Département")
    nation <- tab  %>%
      filter(!grepl("^[[:digit:]]+[AB[:digit:]]($|[[:digit:]]$|[MDmd]$)",Code.departement)) %>%
      select(-c(Code.departement)) %>%
      mutate(TypeTerritoire = "France",
             Code.departement = NA)

    tab <- bind_rows(departements, nation)

  } else if ((options %in% c("prestasolmens","msmens","minsocmens")) & (nlignetitre == 3)) {

    # onglets avec données par départements dans le fichier de suivi mensuel des prestations de solidarité
    names(tab)[1] <- "date"
    tab <- tab %>%
      mutate(info.date = str_extract(date,"(\\*)*$"),
             date0 = gsub("(\\*)*$","",date),
             date0 = ifelse(
               str_extract(date0,"^[[:alpha:]]+") %in% c("janv","févr","avr","juil","sept","oct","nov","déc"),
               gsub("\\-",".-",date0),
               date0
             ),
             date = case_when(
               grepl("^[[:digit:]]+$",date0) ~ format.Date(as.Date(as.numeric(date0),origin = "1899-12-30"),"%b %Y"),
               grepl("^[[:alpha:]]+\\-[[:digit:]]+$",date0) ~ format.Date(as.Date(paste0("01-",date0),"%d-%B-%y"),"%b %Y"),
               grepl("^[[:alpha:]]+\\.\\-[[:digit:]]+$",date0) ~ format.Date(as.Date(paste0("01-",date0),"%d-%b-%y"),"%b %Y"),
               TRUE ~ date0
               )
             ) %>%
      select(-date0) %>%
      pivot_longer(cols=-c("date","info.date"),names_to="noms",values_to="val")
    noms <- as.data.frame(t(as.data.frame(str_split( tab$noms,"\\.",n=3))))
    tab$Code.departement <- noms[,1]
    tab$Territoire <- noms[,2]
    tab$variable <- noms[,3]
    tab <- tab %>%
      select(-noms) %>%
      pivot_wider(id_cols=c("date","info.date","Code.departement","Territoire"),
                  names_from="variable",
                  values_from="val")

  } else if ((options %in% c("prestasolmens","msmens","minsocmens")) & (nlignetitre == 2)) {

    # onglets avec données nationales dans le fichier de suivi mensuel des prestations de solidarité
    names(tab)[1] <- "date"
    tab <- tab %>%
      mutate(info.date = str_extract(date,"(\\*)*$"),
             date0 = gsub("(\\*)*$","",date),
             date0 = ifelse(
               str_extract(date0,"^[[:alpha:]]+") %in% c("janv","févr","avr","juil","sept","oct","nov","déc"),
               gsub("\\-",".-",date0),
               date0
             ),
             date = case_when(
               grepl("^[[:digit:]]+$",date0) ~ format.Date(as.Date(as.numeric(date0),origin = "1899-12-30"),"%b %Y"),
               grepl("^[[:alpha:]]+\\-[[:digit:]]+$",date0) ~ format.Date(as.Date(paste0("01-",date0),"%d-%b-%y"),"%b %Y"),
               TRUE ~ date0
             )
      ) %>%
      select(-date0) %>%
      pivot_longer(cols=-c("date","info.date"),names_to="noms",values_to="val")
    noms <- as.data.frame(t(as.data.frame(str_split( tab$noms,"\\.",n=2))))
    tab$prestation <- noms[,1]
    tab$variable <- noms[,2]
    tab <- tab %>%
      select(-noms) %>%
      mutate(val = as.numeric(gsub("\\*|[[:space:]]|\\-","",val)),
             prestation = trimws(prestation,"both")) %>%
      pivot_wider(id_cols=c("date","info.date","prestation"),
                  names_from="variable",
                  values_from="val")
  }

  # ========================================
  # enregistrement d'une deuxième table de données (transposée) si des années sont détectées comme noms de colonne

  patternannee <- "^(19|20|21)[[:digit:]]{2}(\\.|[[:space:]]|\\*|\\(|$)"
  #patternannee <- "^(19|20|21)[[:digit:]]{2}[[:space:]]*(\\**|\\(([[:digit:]]|p|d|sd)\\))$"
  colsannee <- names(tab)[grepl(patternannee,names(tab))]
  result$containstablong <- FALSE
  if (NROW(colsannee)>=1) {
    tablong <- tab %>%
      mutate_at(vars(colsannee),as.numeric) %>%
      pivot_longer(cols=c(colsannee),
                   names_to="annee",
                   values_to="valeur",
                   values_drop_na = TRUE) %>%
      mutate(info.annee = gsub("^(19|20|21)[[:digit:]]{2}([[:space:]]|\\.)*","",annee),
             annee = as.numeric(str_extract(annee,"^(19|20|21)[[:digit:]]{2}"))) %>%
      distinct()
    result$containstablong <- (nrow(tablong)>0)
    result$tablong <- tablong
  }

  # ========================================
  # lecture des métadonnées
  info <- info[,(colSums(is.na(info))<nrow(info))]
  info <- as.vector(t(info))
  info <- info[!is.na(info)]
  info <- unique(info) %>% trimws()

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
  result$containstab <- (nrow(result$tab)>0)
  return(result)
}
