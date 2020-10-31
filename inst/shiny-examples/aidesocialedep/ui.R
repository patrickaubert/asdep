library(tidyverse)
library(dplyr)
library(shiny)
library(shinydashboard)
library(plotly)
library(asdep)


ui <- dashboardPage(

  dashboardHeader(
    title = "Aide sociale des départements",
    titleWidth = 350
  ),

  # ====================================================================
  # options de visualisation

  dashboardSidebar(
    sidebarMenu(
      menuItem("Perte d'autonomie", tabName = "dependance", icon = icon("bed")),
      menuItem("Handicap", tabName = "handicap", icon = icon("wheelchair")),
      menuItem("ASE", tabName = "ase", icon = icon("child")),
      menuItem("Insertion", tabName = "insertion", icon = icon("hands-helping")),
      menuItem("Paramétrer", tabName = "param", icon = icon("cog")),
      menuItem("Documentation", tabName = "doc", icon = icon("book")),
      menuItem("A propos", tabName = "info", icon = icon("th"))
    ) # fin sidebarMenu
  ), # fin sidebar

  # ====================================================================
  # affichage des résultats

  dashboardBody(
    tabItems(

      # === graphiques et indicateurs
      tabItem(
        tabName = "dependance",
      ),
      tabItem(
        tabName = "handicap",
      ),
      tabItem(
        tabName = "ase",
      ),
      tabItem(
        tabName = "insertion",
      ),
      tabItem(
        tabName = "param",
      ),
      tabItem(
        tabName = "doc",
      ),
      tabItem(
        tabName = "info",
      )
    ) # fin tabItems
  ) #fin body
) # fin page
