# asdep

Ce package R contient des fonctions et des bases de données pour faciliter les analyses statistiques des aides sociales départementales, dans les domaines de la perte d'autonomie, du handicap, de l'ASE et de l'insertion. Les données sont extraites des résultats publiés par la DREES, notamment à partir de son enquête annuelle sur l'aide sociale des départements.

Le package inclut par ailleurs une application interactive permettant d'en illustrer les utilisations (application également accessible en ligne : ...).

**ATTENTION** Ce package est encore en cours de développement. La version actuelle est donc très incomplète et très préliminaire !

## Pour installer le package :

remotes::install_github("patrickaubert/asdep",ref='main')

## To run the example (Shiny app):

library(tidyverse)

library(shiny)

library(shinydashboard)

library(plotly)

library(asdep)

asdep::runExample()

## Fonctions disponibles dans le package :

* La fonction *selectIndic* ...
* La fonction *listeDenominateurs* ...

## Bases de données disponibles dans le package :

* La base *ASDEPslbenef* contient les données du fichier "séries historiques sur les bénéficiaires de l'aide sociale" publié par la DREES sur son espace data.drees. La base *ASDEPslbenef_description* contient les métadonnées associées à cette base.
* Les bases *ASDEPsldepenses* et *ASDEPsldepenses_description* sont construites similairement à partir du fichier "séries historiques sur les dépenses d'aide sociale" publié par la DREES.
* Les bases *ASDEPsl* et *ASDEPsl_description* correspondent à la concaténation des bases sur les bénéficiaires et les dépenses, ainsi que des bases de données sur les populations départementales, téléchargeables sur le site de l'Insee.



