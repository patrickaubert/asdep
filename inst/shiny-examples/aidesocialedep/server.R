server <- function(input, output, session) {

  # ========================================================
  # mise en forme des graphiques dans l'appli

  territoireComparaison <- function(terrcomp) {
    switch(terrcomp,
           "france" = "TOTAL estimé France entière (hors Mayotte)",
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
  graphEvolutionAppli <- function(...) {
    graphEvolution(
      ...,
      dept=input$dep, # département choisi par l'utilisateur
      comp= territoireComparaison(input$terrcomp),
      gpecomp = gptDeptComparaison(input$terrcomp, input$listedepcomp),
      options=c(input$affichedispers) # zone choisie par l'utilisateur
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
    ggplotly( graphEvolutionAppli(nomvariable="NbBenefAPA",denom="pop.60.99")    )
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
  output$partAPAdom <- renderPlotly({
    ggplotly( graphEvolutionAppli(nomvariable="NbBenefAPADomicile",denom="NbBenefAPA")    )
  })
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
