server <- function(input, output, session) {

  # ========================================================
  # mise en forme des graphiques dans l'appli

  territoireComparaison <- function(terrcomp,var) {
    switch(terrcomp,
           "france" = champFrance(var),
           "region" = "Grand Est",
           "choix" = "Groupe de comparaison")
  }
  gptDeptComparaison <- function(terrcomp,gpeDept) {
    switch(terrcomp,
           "france" = c(),
           "region" = c(),
           "choix" = c(gpeDept))
  }

  ggplotlocal <- function(...) {
    ggplot(...)
    # to be done: ajout mis en forme
  }
  ggplotlylocal <- function(...) {
    g <- ggplotly(...)
    # on extrait les ajouts moches par plotly dans les intitulés de légende
    for (i in 1:length(g$x$data)) {
      g$x$data[[i]]$legendgroup <- gsub("^\\(|,[[:digit:]]*\\)$","",g$x$data[[i]]$legendgroup)
      g$x$data[[i]]$name <- gsub("^\\(|,[[:digit:]]*\\)$","",g$x$data[[i]]$name)
    }
    return(g)
  }

  graphEvolutionAppli <- function(...) {
    graphEvolution(
      ...,
      dept=input$dep, # département choisi par l'utilisateur
      comp= territoireComparaison(input$terrcomp, nomvariable),
      gpecomp = gptDeptComparaison(input$terrcomp, input$listedepcomp),
      options=c(input$affichedispers), # zone choisie par l'utilisateur
      typesortie="graphdyn"
      )
  }

  # ========================================================
  # actualisation des widgets

  observeEvent(input$dep.plus, {
    newval <- max(1, min(NROW(listedepartements), (match(input$dep,listedepartements)+1) ))
    updateSelectInput(session, "dep",
                      label = NULL, #Département",
                      choices = listedepartements,
                      selected = listedepartements[newval] )
  })

  observeEvent(input$dep.moins, {
    newval <- max(1, min(NROW(listedepartements), (match(input$dep,listedepartements)-1) ))
    updateSelectInput(session, "dep",
                      label = NULL, #Département",
                      choices = listedepartements,
                      selected = listedepartements[newval] )
  })

  # ========================================================
  # options dynamiques




  # ========================================================
  # Graphiques

  # === Perte d'autonomie

  # -- part des bénéficiaires de l'APA dans la population de 60 ans et plus, en série temporelle
  output$partAPApop <- renderPlotly({
    graphEvolutionAppli(nomvariable="NbBenefAPA",denom="pop.60.99")
  })
  output$partAPApop2 <- renderPlotly({
    graphEvolutionAppli(nomvariable="NbBenefAPA",denom="pop.60.99",typesortie="graphdyn")
  })
  #output$partAPApop <- renderPlotly({
  #  tab <- selectIndic(
  #    nomvariable="NbBenefAPA",denom="pop.60.99",
  #    gpeDpt = c(), options="")$var %>%
  #    filter(Territoire %in% c(input$dep,"TOTAL estimé France entière (hors Mayotte)"))
  #  g <- ggplotlocal(tab , aes(x=Annee,y=NbBenefAPA,colour=Territoire) ) +
  #    geom_line()
  #  ggplotly(g)
  #})
  # -- part des bénéficiaires de l'APA à domicile dans l'ensemble des bénéficiaires de l'APA, en série temporelle
  output$partAPAdom <- renderPlotly({  graphEvolutionAppli(nomvariable="NbBenefAPADomicile",denom="NbBenefAPA")   })
  #output$partAPAdom <- renderPlotly({
  #  tab <- selectIndic(
  #    nomvariable="NbBenefAPADomicile",denom="NbBenefAPA",
  #    gpeDpt = c(), options="")$var %>%
  #    filter(Territoire %in% c(input$dep,"TOTAL estimé France entière (hors Mayotte)"))
  #  g <- ggplotlocal(tab , aes(x=Annee,y=NbBenefAPADomicile,colour=Territoire) ) +
  #    geom_line()
  #  ggplotly(g)
  #})



}
