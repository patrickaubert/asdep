#' Fonction produisant un graphique (format ggplot) présentant l'évolution d'un indicateur d'aide sociale au cours du temps
#'
#' Cette fonction sert à produire un graphique (au format ggplot2) représentant l'évolution au cours du temps d'un indicateur pour un département de référence, et un territoire de comparaisons (par exemple : la France entière, la région du département de référence, un groupe de département).
#' Les options permettent d'ajouter sur le graphique une représentation des diverses zones : zone interquartile, zone interdécile, zone correspondant à la médiane +/- 10 ou 20 %...
#'
#' @param nomvariable le nom de la variable, parmi celles de la table ASDEPsl
#' @param denom le nom d'une variable à utiliser au dénominateur de l'indicateur (si absent la variable est utilisée brute)
#' @param options un vecteur d'options du calcul (valeurs en euros courant ou constant, mensuels ou annuels, etc.) et d'options de représentation (représenter les zones interdécile, interquartile, médiane +/- 10 %, etc)
#' @param dept le nom du département qu'on souhaite représenter
#' @param comp le nom du territoire qu'on souhaite représenter en comparaison (par défaut, "TOTAL estimé France entière (hors Mayotte)")
#' @param gpecomp un vecteur de noms de départements : leur ensemble constituera le "groupe de comparaison" (si cette variable est renseignée, la variable 'comp' est automatiquement égale à "groupe de comparaison")
#' @param donnees la table de données en entrée (par défaut, la table ASDEPsl)
#' @param variables la table de métadonnées de la table de données en entrée (par défaut, la table ASDEPsl_description)
#'
#' @return un graphique (format ggplot2)
#' @export
#'
#' @examples
graphEvolution <- function(nomvariable, denom, options,
                           dept, comp = "TOTAL estimé France entière (hors Mayotte)", gpecomp,
                           donnees = ASDEPsl, variables = ASDEPsl_description) {

}
