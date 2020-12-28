#' Fonction extrayant le contenu de tous les onglets d'un fichier Excel diffusé sur data.drees
#'
#' Cette fonction sert à extraire le contenu de tous les onglets d'un fichier Excel de données départementales diffusé sous data.Drees.
#' Le produit est une liste contenant un élément "tab" correspondant au tableau de données,
#' et un élément "metadonnees" correspondant au tableau des métadonnées (source,note,champ, etc.)
#' Si le tableau "tab" contient des colonnes dont les intitulés correspondent à des années,
#' un autre élément "tablong" est disponible, dans lequel les années sont transposées en ligne plutôt qu'en colonnes.
#'
#' La fonction inclut des traitements complémentaires de mise en forme pour certains fichiers Excel
#' particuliers ('options' = "ASDEPslbenef", "ASDEPsldepenses", "OARSAsl", etc.)
#'
#' @param fich nom du fichier Excel
#' @param sheetinclude vecteur des noms des onglets à inclure dans l'extraction (par défaut : tous les onglets)
#' @param sheetexclude vecteur des noms des onglets à exclure de l'extraction (par défaut : aucun onglet)
#' @param nlignetitre nombre de lignes pour les intitulés de colonne dans le fichier excel
#' @param options type de fichier data.drees particulier ("ASDEPslbenef", "ASDEPsldepenses", "OARSAsl", etc.)
#'
#' @return
#' @export
#'
#' @examples readExcelDrees(fich="data-raw/Les bénéficiaires de l aide sociale départementale - séries longues (1996-2018).xlsx",options = "ASDEPslbenef")
#' @examples readExcelDrees(fich="data-raw/Le personnel départemental de l'action sociale et médico-sociale de 2014 à 2018.xlsx",options = "ASDEPslperso")
#' @examples readExcelDrees(fich="data-raw/Les dépenses d aide sociale départementale - séries longues (1999 -2018).xlsx",options = "ASDEPsldepenses")
#' @examples readExcelDrees(fich="data-raw/Données mensuelles des prestations de solidarité.xlsx",options = "PrestaSolMens")
readExcelDrees <- function(fich , sheetinclude = NULL, sheetexclude = NULL, nlignetitre = NULL, options = "") {

  # fich <- "data-raw/Données mensuelles des prestations de solidarité.xlsx"
  # fich <- "data-raw/Les bénéficiaires de l aide sociale départementale - séries longues (1996-2018).xlsx"
  # fich <- "data-raw/Le personnel départemental de l'action sociale et médico-sociale de 2014 à 2018.xlsx"
  # fich <- "data-raw/Minima sociaux - donnees departementales par dispositif.xlsx"

  # ========================================
  # valeurs par défaut pour certains fichiers particuliers
  options <- tolower(options)
  if (options %in% c("asdepslbenef")) {

    # === fichier Excel "bénéficiaires de l'aide sociale, séries longues"

    sheetexcludespec <- c("Présentation et méthode","Sommaire","Données nationales","Tab11-ase")
    # RQ : "Tab11-ase" n'est pas lu, car il ne contient qu'une ligne, au niveau France entière (indicateur = nb de MNA)
    if (is.null(nlignetitre)) { nlignetitre <- 1 }

  } else if (options %in% c("asdepsldepenses")) {

    # === fichier Excel "Dépenses d'aide sociale, séries longues"

    sheetexcludespec <- c("Présentation et méthode","Sommaire","Données nationales","IPC")
     if (is.null(nlignetitre)) { nlignetitre <- 1 }

  } else if (options %in% c("asdepslperso")) {

    # fichier Excel "Personnels de l'aide sociale séries longues"
    sheetexcludespec <- c("Présentation et méthode " ,
                          "Sommaire",
                          "Descriptif",
                          "Données nationales",
                          "Données nationales - métro")
    if (is.null(nlignetitre)) { nlignetitre <- 1 }

  } else if (options %in% c("oarsasl")) {

  } else if (options %in% c("mssl","minsocsl")) {

  } else if (options %in% c("prestasolmens","msmens","minsocmens")) {

    # fichier Excel "données mensuelles sur les prestations de solidarité"
    # NB : on extrait ici uniquement les onglets contenant des données départementales
    # NB2 : on renonce aussi à extraire le tableau 8 (entrées initiales en GJ), dont le format est différent de celui des autres onglets
    sheetexcludespec <- c("Sommaire",
                          "Lisez-moi",
                          "Tableau 1",
                          "Tableau 8" )
    if (is.null(nlignetitre)) { nlignetitre <- 3 }

  } else {

  }
  sheetexclude <- unique(c(sheetexclude, sheetexcludespec))

  # ========================================
  # ajustement de la liste des onglets à extraire
  sheets <- getSheetNames(fich)
  if (!is.null(sheetinclude)) { sheets <- intersect(sheets, sheetinclude) }
  if (!is.null(sheetexclude)) { sheets <- sheets[!(sheets %in% sheetexclude)] }

  if (NROW(sheets) == 0) { stop("Erreur : aucun onglet correspondant") }

  # ========================================
  # extraction de tous les onglets
  titreslignes <- rep(nlignetitre, NROW(sheets))
  onglets <- lapply(
    1:NROW(sheets),
    function(i){readSheetDrees(fich = fich ,
                               sheet = sheets[i],
                               nlignetitre = titreslignes[i],
                               options = options)})
  names(onglets) <- sheets

  # ========================================
  # récupération des métadonnées => transformées en data.frame
  metadonnees <- do.call("bind_rows",
                         lapply(sheets, function(x){as.data.frame(onglets[[x]][!(names(onglets[[x]]) %in% c("tab","tablong"))])}))
  result <- list("metadonnees" = metadonnees)

  # ========================================
  # récupération et appariements des tableaux de données

  if (sum(metadonnees$containstab)>0) {
    sheetstab <- metadonnees[metadonnees$containstab==TRUE,"ongletsource"]
    tab <- do.call(
      "bind_rows",
      lapply(1:NROW(sheetstab), function(x){onglets[[sheetstab[x]]]$tab %>% mutate(sheet = sheets[x])}))
    result[["tab"]] <-tab
  }

  if (sum(metadonnees$containstablong)>0) {
    sheetstablong <- metadonnees[metadonnees$containstablong==TRUE,"ongletsource"]
    tablong <- do.call(
      "bind_rows",
      lapply(1:NROW(sheetstablong), function(x){onglets[[sheetstablong[x]]]$tablong %>% mutate(sheet = sheets[x])}))
    result[["tablong"]] <-tablong
  }

  # ========================================
  # résultats
  return(result)
}
