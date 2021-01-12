# asdep

Ce package R contient des fonctions et des bases de données pour faciliter les analyses statistiques des aides sociales départementales, dans les domaines de la perte d'autonomie, du handicap, de l'ASE et de l'insertion. Les données sont extraites des résultats publiés par la DREES, notamment à partir de son enquête annuelle sur l'aide sociale des départements.

Le package inclut par ailleurs une application interactive permettant d'en illustrer les utilisations (application également accessible en ligne : ...).

**ATTENTION** Ce package est encore en cours de développement. La version actuelle est donc très incomplète et très préliminaire !

## Pour installer le package :

remotes::install_github("patrickaubert/asdep",ref='main')

## Documentation :

https://patrickaubert.github.io/asdep/index.html

## Exemple d'utilisation du package (Shiny app):

library(tidyverse)

library(shiny)

library(shinydashboard)

library(plotly)

library(asdep)

asdep::runExample()

## Contenu du package

Le package *asdep* contient deux types d'éléments :

* des bases de données sur les caractéristiques départementales dans le domaine social (aides sociales versées, dépenses, personnel de l'aide social, minima sociaux ...), construites à partir des données publiées par la DREES ou par l'Insee, et
* des fonctions visant à faciliter les analyses de ces données.

Plusieurs types de fonction sont inclus :

- des fonctions produisant des graphiques ou tableau de résultat, pour représenter un indicateur au niveau d'un département et le comparer au même indicateur sur d'autres territoires,
- des fonctions d'extraction des fichiers Excel diffusés par la DREES,
- des fonctions auxiliaires utiles.

### Fonctions disponibles dans le package :

La fonction *graphEvolution* produit par exemple un graphique présentant l'évolution d'un indicateur d'aide sociale au cours du temps, pour un département de référence donné, un territoire de comparaison (la France, une région, un groupe de département...). Elle permet aussi de représenter sur le graphique des éléments de distribution (par exemple, les zones interdécile ou interquartile). Le format de l'objet en sortie peut être paramétré par l'utilisateur : un graphique statique (format ggplot), un graphique dynamique (format plotly), une table de données, etc. Par exemple *graphEvolution(nomvariable="NbBenefAPA",denom="pop.60.99",dept="Vosges",gpecomp=c("Meuse","Moselle"),options=c("medianePM10","medianePM20"),typesortie="graphdyn")* produit un graphique dynammique représentant la part des bénéficiaires de l'APA dans la population de 60 ans et plus, pour le département des Vosges comparé à la moyenne des départements de la Meuse et de la Moselle, avec une représentation graphique des zones situées autour de la médiane départementale +/- 10 et +/- 20 %.

Pour construire ce graphique, des fonctions auxiliaires, également contenues dans le package, sont utilisées : la fonction *ggplotAsdep* met en forme un graphique (ggplot) selon un standard de mise en forme choisi pour le package (position de la légende, taille des titres, etc.) ;
la fonction *ggplotlyAsdep* fait de même pour les graphiques dynamiques (plotly)

### Bases de données disponibles dans le package :

* La base *ASDEPslbenef* contient les données du fichier "séries historiques sur les bénéficiaires de l'aide sociale" publié par la DREES sur son espace data.drees. La base *ASDEPslbenef_description* contient les métadonnées associées à cette base.
* Les bases *ASDEPsldepenses* et *ASDEPsldepenses_description* sont construites similairement à partir du fichier "séries historiques sur les dépenses d'aide sociale" publié par la DREES.
* Les bases *ASDEPsl* et *ASDEPsl_description* correspondent à la concaténation des bases sur les bénéficiaires et les dépenses, ainsi que des bases de données sur les populations départementales, téléchargeables sur le site de l'Insee.



