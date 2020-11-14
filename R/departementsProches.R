#' Une fonction auxiliaire déterminant les départements les plus proches d'un département de référence au regard d'un indicateur donné
#'
#' Fonction qui retourne un vecteur des départements les plus proches (variable 'nb', par défaut 5 départements) du département de référence (variable 'dept') au regard d'un indicateur donné (variable 'nomvariable')
#'
#' @param dept le nom du département de référence
#' @param annee l'année de référence (par défaut, la dernière année pour laquelle la variable est disponible)
#' @param nomvariable le nom de la variable de référence (sur la bsse de laquelle les départements les plus proches seront déterminés)
#' @param denom l'éventuel dénominateur de la variable
#' @param nb le nombre de départements proches à inclure dans le vecteur en sortie
#' @param tabindic une table avec les valeurs départementales de l'indicateur (si cette table n'est pas fournie en entrée, elle est construire par la fonction)
#' @param donnees les données de base (par défaut, la table ASDEPsl contenue dans le package asdep)
#' @param variables les métadonnées de base (par défaut, la table ASDEPsl_description)
#'
#' @return un vecteur de noms de départements
#' @export departementsProches
#'
#' @examples departementsProches(dept="Meurthe-et-Moselle",nomvariable="NbBenefAPA",denom="popPA",nb=5)
departementsProches <- function(
  dept = NULL,
  nomvariable = NULL, denom = "", nb = 5,
  annee = max(ASDEPsl[is.na(ASDEPsl[,c(nomvariable1,nomvariable2)]),"Annee"],na.rm=TRUE),
  tabindic = NULL,
  donnees = ASDEPsl, variables = ASDEPsl_description) {

  # vérifier que les variables indispensables au bon fonctionnement de la fonction ont bien été renseignées
  is.vide <- function(t) { is.null(t) | is.na(t) | (t == "") }
  #is.vide <- function(t) { case_when(is.null(t)~TRUE, is.na(t)~TRUE, (t == "")~TRUE, TRUE~FALSE) }
  is.vide <- function(t) { ifelse(is.null(t),TRUE, ifelse((t == ""),TRUE, ifelse(is.na(t),TRUE, FALSE) ) ) }
  if (is.null(dept) | is.null(nomvariable)) { return( c() ) }

  # table avec les valeurs de l'indicateur
  if (is.null(tabindic)) {
    if (!(nomvariable %in% names(donnees)) | !(denom %in% names(donnees))) {return(c())}
    tabindic <- selectIndic(nomvariable = nomvariable, denom = denom,
                            donnees = donnees, variables = variables)$var
  }
  tabindic <- tabindic %>% filter(TypeTerritoire == "Département", Annee == annee)
  if (!(dept %in% tabindic$Territoire)) { return( c() ) }
  if (!(nomvariable %in% names(tabindic))) { return( c() ) }

  # détermination des départements proches
  valref <- tabindic[tabindic$Territoire == dept,nomvariable]
  tabindic$ecart <- abs( tabindic[,nomvariable] - valref )
  tabindic <- tabindic %>%
    filter(Territoire != dept) %>%
    arrange(ecart)
  proches <- tabindic$Territoire[1:min(nb,nrow(tabindic))]

  # résultat
  return( proches )
}
