#' Bénéficiaires d'aides sociales départementales
#'
#' Une base de données issues de l'enquête annuelle 'Aide sociale' de la DREES, contenant les effectifs de bénéficiaires de diverses aides sociales versées par les départements.
#' Les noms des variables, correspondant aux différents types d'aide, sont décrits dans la table 'ASDEPslbenef_description'.
#'
#' @format Un data frame avec 2701 lignes et 49 variables:
#' \describe{
#'   \item{Annee}{année (format character)}
#'   \item{Code.region}{code numérique de la région}
#'   \item{Code.departement}{code numérique du département (ou du territoire gérant l'aide sociale)}
#'   \item{TypeTerritoire}{type de territoire ("département","région",...)}
#'   \item{Territoire}{nom complet du territoire}
#'   \item{nom.variable}{voir la table ASDEPslbenef_description}
#' }
#' @source \url{http://www.data.drees.sante.gouv.fr/}
"ASDEPslbenef"

#' Dépenses annuelles d'aides sociales départementales
#'
#' Une base de données issues de l'enquête annuelle 'Aide sociale' de la DREES, contenant les montants annuels de dépenses de diverses aides sociales versées par les départements.
#' Les noms des variables, correspondant aux différents types d'aide, sont décrits dans la table 'ASDEPsldepenses_description'.
#'
#' @format Un data frame avec 2460 lignes et 38 variables:
#' \describe{
#'   \item{Annee}{année (format character)}
#'   \item{Code.region}{code numérique de la région}
#'   \item{Code.departement}{code numérique du département (ou du territoire gérant l'aide sociale)}
#'   \item{TypeTerritoire}{type de territoire ("département","région",...)}
#'   \item{Territoire}{nom complet du territoire}
#'   \item{nom.variable}{voir la table ASDEPsldepenses_description}
#' }
#' @source \url{http://www.data.drees.sante.gouv.fr/}
"ASDEPsldepenses"

#' Personnels départementaux d'aides sociales départementales
#'
#' Une base de données issue du volet "personnels" de l'enquête annuelle 'Aide sociale' de la DREES, contenant les effectifs physiques et les ETP de personnels pour l'aide sociale ds départements.
#' Les noms des variables, correspondant aux différents types d'aide, sont décrits dans la table 'ASDEPslperso_description'.
#'
#' @format Un data frame avec 448 lignes et 20 variables:
#' \describe{
#'   \item{Annee}{année (format character)}
#'   \item{Code.region}{code numérique de la région}
#'   \item{Code.departement}{code numérique du département (ou du territoire gérant l'aide sociale)}
#'   \item{TypeTerritoire}{type de territoire ("département","région",...)}
#'   \item{Territoire}{nom complet du territoire}
#'   \item{nom.variable}{voir la table ASDEPslperso_description}
#' }
#' @source \url{http://www.data.drees.sante.gouv.fr/}
"ASDEPslperso"

#' Orientation et accompagnement des bénéficiaires du RSA
#'
#' Une base de données issues de l'enquête annuelle 'Orientation et accompagnement des bénéficiaires du RSA' de la DREES,
#' contenant des indicateurs sur les parts de bénéficiaires orientés par les départements et les délais moyens d'orientation.
#' Les noms des variables, correspondant aux différents types d'aide, sont décrits dans la table 'OARSAsl_description'.
#'
#' @format Un data frame avec 416 lignes et 23 variables:
#' \describe{
#'   \item{Annee}{année (format character)}
#'   \item{TypeTerritoire}{type de territoire ("département","région",...)}
#'   \item{Territoire}{nom complet du territoire}
#'   \item{nom.variable}{voir la table OARSAsl_description}
#' }
#' @source \url{http://www.data.drees.sante.gouv.fr/}
"OARSAsl"

#' Intitulés des variables des bases sur les Bénéficiaires d'aides sociales départementales
#'
#' Une base de métadonnées sur les variables de la base ASDEPslbenef
#'
#' @format Un data frame avec 44 lignes et 13 variables:
#' \describe{
#'   \item{nom.var}{nom de la variable (correspond aux colonnes des bases ASDEPslbenef et ASDEPsldepenses)}
#'   \item{Intitule.var}{intitulé complet de la variable}
#'   \item{Intitulecourt.var}{intitulé court de la variable}
#'   \item{Source.var}{source (= enquête Aide sociale, DREES)}
#'   \item{Champ.var}{champ de la variable}
#'   \item{Note.var}{notes explicatives sur la variable, récupérées du fichier Excel diffusé par la DREES}
#'   \item{Unite.var}{unité de la variable (personnes, €, ...)}
#'   \item{Thematique.var}{thématique (perte d'autonomie, handicap, ...)}
#'   \item{TexteDenom.var}{texte court lorsque la variable est utilisé comme dénominateur d'un ratio}
#'   \item{ListeDenom.var}{liste des noms d'autres variables (séparés par un "_") pouvant servir comme dénominateur de la variable}
#'   \item{ListeComposante.var}{listes de noms d'autres variables (séparés par un "_") dont la somme correspond à la variable}
#'   \item{Type.var}{type de variable (nombre, dépense...)}
#'   \item{Popref.var}{population de référence (par exemple : "20-59" = les 20-59 ans)}
#' }
#' @source \url{http://www.data.drees.sante.gouv.fr/}
"ASDEPslbenef_description"

#' Intitulés des variables des bases sur les dépenses d'aides sociales départementales
#'
#' Une base de métadonnées sur les variables de la base ASDEPsldepenses
#'
#' @format Un data frame avec 35 lignes et 13 variables:
#' \describe{
#'   \item{nom.var}{nom de la variable (correspond aux colonnes des bases ASDEPslbenef et ASDEPsldepenses)}
#'   \item{Intitule.var}{intitulé complet de la variable}
#'   \item{Intitulecourt.var}{intitulé court de la variable}
#'   \item{Source.var}{source (= enquête Aide sociale, DREES)}
#'   \item{Champ.var}{champ de la variable}
#'   \item{Note.var}{notes explicatives sur la variable, récupérées du fichier Excel diffusé par la DREES}
#'   \item{Unite.var}{unité de la variable (personnes, €, ...)}
#'   \item{Thematique.var}{thématique (perte d'autonomie, handicap, ...)}
#'   \item{TexteDenom.var}{texte court lorsque la variable est utilisé comme dénominateur d'un ratio}
#'   \item{ListeDenom.var}{liste des noms d'autres variables (séparés par un '_') pouvant servir comme dénominateur de la variable}
#'   \item{ListeComposante.var}{listes de noms d'autres variables (séparés par un '_') dont la somme correspond à la variable}
#'   \item{Type.var}{type de variable (nombre, dépense...)}
#'   \item{Popref.var}{population de référence (par exemple : "20-59" = les 20-59 ans)}
#' }
#' @source \url{http://www.data.drees.sante.gouv.fr/}
"ASDEPsldepenses_description"

#' Intitulés des variables des bases sur l'orientation et l'accompagnement des bénéficiaires du RSA par les départements
#'
#' Une base de métadonnées sur les variables de la base OARSAsl
#'
#' @format Un data frame avec 20 lignes et 13 variables:
#' \describe{
#'   \item{nom.var}{nom de la variable}
#'   \item{Intitule.var}{intitulé complet de la variable}
#'   \item{Intitulecourt.var}{intitulé court de la variable}
#'   \item{Source.var}{source (= enquête OARSA, DREES)}
#'   \item{Champ.var}{champ de la variable}
#'   \item{Note.var}{notes explicatives sur la variable, récupérées du fichier Excel diffusé par la DREES}
#'   \item{Unite.var}{unité de la variable (personnes, €, ...)}
#'   \item{Thematique.var}{thématique (perte d'autonomie, handicap, ...)}
#'   \item{TexteDenom.var}{texte court lorsque la variable est utilisé comme dénominateur d'un ratio}
#'   \item{ListeDenom.var}{liste des noms d'autres variables (séparés par un "_") pouvant servir comme dénominateur de la variable}
#'   \item{ListeComposante.var}{listes de noms d'autres variables (séparés par un "_") dont la somme correspond à la variable}
#'   \item{Type.var}{type de variable (nombre, dépense...)}
#'   \item{Popref.var}{population de référence (par exemple : "20-59" = les 20-59 ans)}
#' }
#' @source \url{http://www.data.drees.sante.gouv.fr/}
"OARSAsl_description"

#' Populations départementales
#'
#' Une base de données contenant les populations départementales par tranche d'âge, téléchargéex sur le site de l'Insee.
#'
#' @format Un data frame avec 2646 lignes et 12 variables
#'
#' @source \url{https://www.insee.fr/fr/statistiques/1893198}
"PopDepartementales"

#' Liste des départements et territoires
#'
#' Une base de données contenant la liste des départements (et territoires gérant l'aide sociale départementale)
#'
#' @format Un data frame avec 104 lignes et 4 variables
#' \describe{
#'   \item{NumReg}{Code numérique de la région}
#'   \item{NumDept}{Code alphnumérique du département (ou territoire)}
#'   \item{Departement}{Nom du département}
#'   \item{Region}{Nom de la région}
#' }
"departementsFR"

