#' Bénéficiaires d'aides sociales départementales
#'
#' Une base de données issues de l'enquête annuelle 'Aide sociale' de la DREES, contenant les effectifs de bénéficiaires de diverses aides sociales versées par les départements.
#' Les noms des variables, correspondant aux différents types d'aide, sont décrits dans la table 'ASDEPslbenef_description'.
#'
#' @format Un data frame avec 3234 lignes et 51 variables:
#' \describe{
#'   \item{Annee}{année (format character)}
#'   \item{Code.region}{code numérique de la région}
#'   \item{Code.departement}{code numérique du département (ou du territoire gérant l'aide sociale)}
#'   \item{TypeTerritoire}{type de territoire ("département","région",...)}
#'   \item{Territoire}{nom complet du territoire}
#'   \item{nom.variable}{voir la table ASDEPslbenef_description}
#' }
#' @source \url{https://data.drees.solidarites-sante.gouv.fr/explore/dataset/375_les-beneficiaires-de-l-aide-sociale-departementale/information/}
"ASDEPslbenef"

#' Bénéficiaires d'aides sociales départementales (données nationales uniquement)
#'
#' Une base de données issues de l'enquête annuelle 'Aide sociale' de la DREES, contenant les effectifs de bénéficiaires de diverses aides sociales versées par les départements.
#' Cette table contient uniquement les données agrégées au niveau national. En revanche, le recul historique est un peu plus long que dans la table 'ASDEPslbenef'.
#'
#' @format Un data frame avec 1159 lignes et 8 variables:
#' \describe{
#'   \item{champ_niv1}{champ des aides couvertes}
#'   \item{champ_niv2}{sous-champ des aides couvertes}
#'   \item{champ_niv3}{sous-sous-champ des aides couvertes}
#'   \item{champ_niv4}{sous-sous-sous-champ des aides couvertes}
#'   \item{annee}{Année d'observation (valeur au 31 décembre)}
#'   \item{nb_benef}{nombre de bénéficiaires en fin d'année}
#'   \item{champ}{champ géographique des observations}
#'   \item{source}{sources des données}
#' }
#' @source \url{https://data.drees.solidarites-sante.gouv.fr/explore/dataset/375_les-beneficiaires-de-l-aide-sociale-departementale/information/}
"ASDEPslbenefnat"

#' Dépenses annuelles d'aides sociales départementales
#'
#' Une base de données issues de l'enquête annuelle 'Aide sociale' de la DREES, contenant les montants annuels de dépenses de diverses aides sociales versées par les départements.
#' Les noms des variables, correspondant aux différents types d'aide, sont décrits dans la table 'ASDEPsldepenses_description'.
#'
#' @format Un data frame avec 2604 lignes et 38 variables:
#' \describe{
#'   \item{Annee}{année (format character)}
#'   \item{Code.region}{code numérique de la région}
#'   \item{Code.departement}{code numérique du département (ou du territoire gérant l'aide sociale)}
#'   \item{TypeTerritoire}{type de territoire ("département","région",...)}
#'   \item{Territoire}{nom complet du territoire}
#'   \item{nom.variable}{voir la table ASDEPsldepenses_description}
#' }
#' @source \url{https://data.drees.solidarites-sante.gouv.fr/explore/dataset/376_les-depenses-d-aide-sociale-departementale/information/}
"ASDEPsldepenses"

#' Prévalences par tranches d'âge des aides sociales départementales
#'
#' Une base de données issues de l'enquête annuelle 'Aide sociale' de la DREES, contenant les nombres de bénéficiaires et les prévalences
#' (c'est-à-dire les parts dans la population) des aides sociales départementales (APA, ASH, etc.) La base contient pour l'instant les aides
#' sociales aux personnes âgées.
#'
#' La version de la base a été compilée en mars 2024, et contient les données jusqu'au 31 décembre 2022.
#'
#' @format Un data frame avec 2889 lignes et 12 variables:
#' \describe{
#'   \item{annee}{année}
#'   \item{prestation}{nom ou type d'aide sociale}
#'   \item{gir}{catégorie de GIR ("GIR1","GIR2", etc. ou "Ensemble")}
#'   \item{lieu}{lieu de résidence au sens de la prestation ("ensemble", "domicile" ou "établissement")}
#'   \item{age}{tranche d'âge (exemple : "[70,75)" = 70 à 74 ans)}
#'   \item{nb}{nombre de bénéficiaires de la prestation dans la tranche d'âge}
#'   \item{pop}{population totale dans la tranche d'âge}
#'   \item{prevalence}{prévalence de la prestation dans la tranche d'âge}
#'   \item{part}{part des bénéficiaires de la prestation de la tranche d'âge parmi l'ensemble des bénéficiaires de cette prestation}
#'   \item{recale_gir}{indicatrice valant TRUE si les totaux tous GIR confondus ont été recalculés comme somme des effectifs par GIR}
#'   \item{decomp6age}{indicatrice valant TRUE si la tranche d'âge appartient à un découpage des âges au-delà de 60 ans en 6 tranches (quinquennal jusqu'à 85 ans, puis regroupant tous les 85 ans et plus)}
#'   \item{decomp8age}{indicatrice valant TRUE si la tranche d'âge appartient à un découpage des âges au-delà de 60 ans en 8 tranches (quinquennal jusqu'à 95 ans, puis regroupant tous les 95 ans et plus)}
#' }
#' @source \url{http://www.data.drees.sante.gouv.fr/}
"ASDEPprevalaidessoc"

#' Personnels départementaux d'aides sociales départementales
#'
#' Une base de données issue du volet "personnels" de l'enquête annuelle 'Aide sociale' de la DREES, contenant les effectifs physiques et les ETP de personnels pour l'aide sociale ds départements.
#' Les noms des variables, correspondant aux différents types d'aide, sont décrits dans la table 'ASDEPslperso_description'.
#'
#' @format Un data frame avec 697 lignes et 20 variables:
#' \describe{
#'   \item{Annee}{année (format character)}
#'   \item{Code.region}{code numérique de la région}
#'   \item{Code.departement}{code numérique du département (ou du territoire gérant l'aide sociale)}
#'   \item{TypeTerritoire}{type de territoire ("département","région",...)}
#'   \item{Territoire}{nom complet du territoire}
#'   \item{nom.variable}{voir la table ASDEPslperso_description}
#' }
#' @source \url{https://data.drees.solidarites-sante.gouv.fr/explore/dataset/3066_le-personnel-de-l-action-sociale-et-medico-sociale/information/}
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
#' @format Un data frame avec 46 lignes et 13 variables:
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
#' @format Un data frame avec 32 lignes et 13 variables:
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
#' @format Un data frame avec 4074 lignes et 12 variables
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

