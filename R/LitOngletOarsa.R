#' Fonction auxiliaire extrayant les données d'un onglet du fichier Excel de résultats de l'enquête OARSA (DREES)
#'
#' Cette fonction auxiliaire sert à la création d'une table de données (format Rds) à partir des données
#' mises en ligne au format Excel sur data.drees pour la diffusion des résultats de l'enquête annuelle OARSA
#' (orientation et accompagnement des bénéficiaires du RSA).
#' Elle est adaptée au format du fichier disponible en novembre 2020.
#'
#' @param Nom.var le nom de la variable en sortie, dans la table de donnée produite
#' @param nomfich le nom du fichier Excel dans lequel se trouvent les données
#' @param nomsheet le nom de l'onglet du fichier Excel
#' @param corrigenom une fonction corrigeant les noms de département
#'
#' @return une liste de deux tables : "tab" = la table des données, "infovar" = la table des métadonnées (intitulé complet, source, unité, etc.)
#' @export
#'
#' @examples
LitOngletOarsa <- function(Nom.var,
                           nomfich = FichierSource,
                           nomsheet,
                           corrigenom = function(x){x}) {

  # valeurs des indicateurs

  vals <- read.xlsx(nomfich, sheet = nomsheet, rows = c(5:109), cols= c(2:6),
                    colNames = TRUE, skipEmptyRows = FALSE, skipEmptyCols = TRUE)
  colnames(vals)[1] <- "terr"
  Encoding(vals$terr) <- "UTF-8"
  valdep <- vals %>%
    mutate(Territoire = gsub("^[[:alnum:]]{2,3}\\-","",terr)) %>%
    select(-terr) %>%
    pivot_longer(cols=-c(Territoire), names_to="Annee",values_to = "Indic" ) %>%
    mutate(#Indic = gsub("[^[:digit:]]*","",Indic),
           #Indic = as.numeric(Indic),
           Annee = as.numeric(Annee),
           Indic  =as.numeric( ifelse(grepl("[^[:digit:]]",Indic),NA,Indic)),
           TypeTerritoire = "Département")
  names(valdep)[3] <- Nom.var
  valdep$Territoire <- unlist(sapply( valdep$Territoire, corrigenom))

  # métadonnées

  Intitule.var <- read.xlsx(nomfich, sheet = nomsheet, rows = c(2), cols= c(2), colNames = FALSE)
  Intitule.var <- gsub("^Tableau [[:alnum:]]+ \\- ","",Intitule.var)
  Intitule.var <- gsub("au 31/12 de l'année","",Intitule.var)

  Unite.var <- read.xlsx(nomfich, sheet = nomsheet, rows = c(4), cols= c(2), colNames = FALSE)
  Unite.var <- gsub("^[Ee]n ","",Unite.var)

  infovar <- data.frame(Nom.var = c(Nom.var),
                        Intitule.var = c(Intitule.var),
                        Intitulecourt.var = c(""),
                        Source.var = c("DREES, enquête OARSA"),
                        Champ.var = c("France"),
                        Note.var = c(""),
                        Unite.var = c(Unite.var),
                        Thematique.var = c("Insertion"),
                        TexteDenom = c(""),
                        ListeDenom.var = c(""),
                        ListeComposante.var = c(""),
                        Type.var = c("Parts"),
                        Popref.var = c(""),
                        stringsAsFactors = FALSE
  )

  return( list(tab = valdep, infovar = infovar))

}
