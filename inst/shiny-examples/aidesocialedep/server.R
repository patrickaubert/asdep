server <- function(input, output, session) {

  listedepartements <- unique(ASDEPsl[ASDEPsl$TypeTerritoire == "Département", "Territoire"])
  listedepartements <- listedepartements[order(listedepartements)]
  listedepartements <- listedepartements[!is.na(listedepartements)]

  region <- function(dep) {departementsFR[departementsFR$Departement == dep,"Region"]}
  autresdepRegion <- function(dep) {
    depreg <- departementsFR[departementsFR$Region == region(dep),]$Departement
    return(depreg[depreg != dep])
  }

  # ========================================================
  # mise en forme des graphiques dans l'appli

  territoireComparaison <- function(terrcomp,var,dep) {
    switch(terrcomp,
           "france" = champFrance(var),
           "region" = region(dep),
           "choix" = "Groupe de comparaison",
           "proche" = "Départements similaires")
  }
  gptDeptComparaison <- function(terrcomp,gpeDept,dep) {
    switch(terrcomp,
           "france" = c(),
           "region" = autresdepRegion(dep),
           "choix" = c(gpeDept),
           "proche" = c( departementsProches(
             dept = input$dep,
             nomvariable = input$varcomp, denom = input$denomvarcomp, nb = input$nbcomp,
             annee = input$anneeref) ))
  }
  output$gpecomparaison <- renderText({ paste(gptDeptComparaison(input$terrcomp, input$listedepcomp, input$dep),collapse="; ") })

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

  graphEvolutionAppli <- function(nomvariable, ...) {
    graphEvolution(
      nomvariable = nomvariable,
      ...,
      dept=input$dep, # département choisi par l'utilisateur
      comp= territoireComparaison(input$terrcomp, nomvariable,input$dep),
      gpecomp = gptDeptComparaison(input$terrcomp, input$listedepcomp, input$dep),
      options=c(input$affichedispers), # zone choisie par l'utilisateur
      typesortie="graphdyn"
      )
  }

  graphComparaisonAppli <- function(nomvariable, ...) {
    # si l'année de référence paramétrée par l'utilisateur est absente, on prend la plus proche
    anneesdispo <- unique(ASDEPsl[!is.na(ASDEPsl[,nomvariable]),"Annee"])
    anneeref <- anneesdispo[which.min(abs(anneesdispo - input$anneeref))]
    # sortie du graphique
    graphComparaison(
      nomvariable = nomvariable,
      ...,
      annee=anneeref,
      dept=input$dep, # département choisi par l'utilisateur
      comp= territoireComparaison(input$terrcomp, nomvariable, input$dep),
      gpecomp = gptDeptComparaison(input$terrcomp, input$listedepcomp, input$dep),
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

  observeEvent(input$varcomp, {
    listDenom <- listeDenominateurs(input$varcomp)
    updateSelectInput(session, "denomvarcomp",
                     choices = as.list(setNames(listDenom$denominateurs, listDenom$intitules)) )
  })


  # ========================================================
  # options dynamiques

  deptProches <- reactive({
    departementsProches(
      dept = input$dep,
      nomvariable = input$varcomp, denom = input$denomvarcomp, nb = input$nbcomp,
      annee = input$anneeref)
  })


  # ========================================================
  # Graphiques

  # === 1) Perte d'autonomie

  # -- part des bénéficiaires de l'APA dans la population de 60 ans et plus, en série temporelle
  output$partAPApopEvol <- renderPlotly({graphEvolutionAppli(nomvariable="NbBenefAPA",denom="pop.60.99")  })
  output$partAPApop <- renderPlotly({graphComparaisonAppli(nomvariable="NbBenefAPA",denom="pop.60.99")  })

  output$partAPAdompopEvol <- renderPlotly({graphEvolutionAppli(nomvariable="NbBenefAPADomicile",denom="pop.60.99")  })
  output$partAPAdompop <- renderPlotly({graphComparaisonAppli(nomvariable="NbBenefAPADomicile",denom="pop.60.99")  })

  output$partAPAetabpopEvol <- renderPlotly({graphEvolutionAppli(nomvariable="NbBenefAPAEtab",denom="pop.60.99")  })
  output$partAPAetabpop <- renderPlotly({graphComparaisonAppli(nomvariable="NbBenefAPAEtab",denom="pop.60.99")  })


  # -- part des bénéficiaires de l'APA à domicile dans l'ensemble des bénéficiaires de l'APA, en série temporelle
  output$partAPAdomEvol <- renderPlotly({  graphEvolutionAppli(nomvariable="NbBenefAPADomicile",denom="NbBenefAPA")   })
  output$partAPAdom <- renderPlotly({  graphComparaisonAppli(nomvariable="NbBenefAPADomicile",denom="NbBenefAPA")   })

  # -- montant moyen d'APA (à domicile + étab)
  output$montAPAEvol <- renderPlotly({  graphEvolutionAppli(nomvariable="DepBruteAPA",denom="NbBenefAPA")   })
  output$montAPA <- renderPlotly({  graphComparaisonAppli(nomvariable="DepBruteAPA",denom="NbBenefAPA")   })

  # -- montant moyen d'APA à domicile
  output$montAPAdomEvol <- renderPlotly({  graphEvolutionAppli(nomvariable="DepBruteAPAdom",denom="NbBenefAPADomicile")   })
  output$montAPAdom <- renderPlotly({  graphComparaisonAppli(nomvariable="DepBruteAPAdom",denom="NbBenefAPADomicile")   })

  # -- montant moyen d'APA en établissement
  output$montAPAetabEvol <- renderPlotly({  graphEvolutionAppli(nomvariable="DepBruteAPAetab",denom="NbBenefAPAEtab")   })
  output$montAPAetab <- renderPlotly({  graphComparaisonAppli(nomvariable="DepBruteAPAetab",denom="NbBenefAPAEtab")   })

  # -- ratio bénéf ASH / béné APA étab
  output$ratioASHAPAEvol <- renderPlotly({  graphEvolutionAppli(nomvariable="NbBenefASH",denom="NbBenefAPAEtab")   })
  output$ratioASHAPA <- renderPlotly({  graphComparaisonAppli(nomvariable="NbBenefASH",denom="NbBenefAPAEtab")   })

  # -- montant moyen d'ASH
  output$montASHEvol <- renderPlotly({  graphEvolutionAppli(nomvariable="DepNetteASH",denom="NbBenefASH")   })
  output$montASH <- renderPlotly({  graphComparaisonAppli(nomvariable="DepNetteASH",denom="NbBenefASH")   })

  # -- proportion bénéf aides ménages dans la population
  output$partAidesMenPAEvol <- renderPlotly({  graphEvolutionAppli(nomvariable="NbBenefAideMenagerePA",denom="pop.60.99")   })
  output$partAidesMenPA <- renderPlotly({  graphComparaisonAppli(nomvariable="NbBenefAideMenagerePA",denom="pop.60.99")   })

  # === 2) Aide sociale à l'enfance

  # -- part des enfants accueillis ou confiés à l'ASE dans la population
  output$partAccueilASEpopEvol <- renderPlotly({graphEvolutionAppli(nomvariable="TotEnfAccueillisASE",denom="pop.0.20")  })
  output$partAccueilASEpop <- renderPlotly({graphComparaisonAppli(nomvariable="TotEnfAccueillisASE",denom="pop.0.20")  })

  output$partConfiesASEpopEvol <- renderPlotly({graphEvolutionAppli(nomvariable="TotEnfConfiesASE",denom="pop.0.20")  })
  output$partConfiesASEpop <- renderPlotly({graphComparaisonAppli(nomvariable="TotEnfConfiesASE",denom="pop.0.20")  })

  # -- part des modes de placement parmi les enfants confiés
  output$partAssFamEvol <- renderPlotly({graphEvolutionAppli(nomvariable="TotEnfASEPlacesFamillesAccueil",denom="TotEnfConfiesASE")  })
  output$partAssFam <- renderPlotly({graphComparaisonAppli(nomvariable="TotEnfASEPlacesFamillesAccueil",denom="TotEnfConfiesASE")  })

  output$partEtabEvol <- renderPlotly({graphEvolutionAppli(nomvariable="TotEnfASEPlacesEtab",denom="TotEnfConfiesASE")  })
  output$partEtab <- renderPlotly({graphComparaisonAppli(nomvariable="TotEnfASEPlacesEtab",denom="TotEnfConfiesASE")  })

  # -- dépenses moyennes par mode de placement


  # -- part des bénéficiaires d'actions éducatives
  output$partAEpopEvol <- renderPlotly({graphEvolutionAppli(nomvariable="TotBenefAE",denom="pop.0.20")  })
  output$partAEpop <- renderPlotly({graphComparaisonAppli(nomvariable="TotBenefAE",denom="pop.0.20")  })

  output$partAEDpopEvol <- renderPlotly({graphEvolutionAppli(nomvariable="TotBenefAED",denom="pop.0.20")  })
  output$partAEDpop <- renderPlotly({graphComparaisonAppli(nomvariable="TotBenefAED",denom="pop.0.20")  })

  output$partAEMOpopEvol <- renderPlotly({graphEvolutionAppli(nomvariable="TotBenefAEMO",denom="pop.0.20")  })
  output$partAEMOpop <- renderPlotly({graphComparaisonAppli(nomvariable="TotBenefAEMO",denom="pop.0.20")  })

  # -- dépense d'action éducative, par bénéficiaire ou par enfant du département
  output$depAEEvol <- renderPlotly({graphEvolutionAppli(nomvariable="DepBruteAEDAEMO",denom="TotBenefAE")  })
  output$depAE <- renderPlotly({graphComparaisonAppli(nomvariable="DepBruteAEDAEMO",denom="TotBenefAE")  })

  output$depAEpopEvol <- renderPlotly({graphEvolutionAppli(nomvariable="DepBruteAEDAEMO",denom="pop.0.20")  })
  output$depAEpop <- renderPlotly({graphComparaisonAppli(nomvariable="DepBruteAEDAEMO",denom="pop.0.20")  })

  # -- dépense de prévention spécialisée, par enfant dans le département
  output$depprevspeEvol <- renderPlotly({graphEvolutionAppli(nomvariable="DepBrutePrevSpe",denom="pop.0.20")  })
  output$depprevspe <- renderPlotly({graphComparaisonAppli(nomvariable="DepBrutePrevSpe",denom="pop.0.20")  })

  # -- dépense d'allocation par enfant dans le département
  output$depallocASEEvol <- renderPlotly({graphEvolutionAppli(nomvariable="DepBruteAllocASE",denom="pop.0.20")  })
  output$depallocASE <- renderPlotly({graphComparaisonAppli(nomvariable="DepBruteAllocASE",denom="pop.0.20")  })

  # -- part des mesures judiciaires parmi l'ensemble des mesures


  # === 3) Handicap

  # -- part des bénéficiaires de la PCH et/ou de l'ACTP dans la population
  output$partPCHACTPpopEvol <- renderPlotly({graphEvolutionAppli(nomvariable="TotBenefACTPPCH",denom="popTOT")  })
  output$partPCHACTPpop <- renderPlotly({graphComparaisonAppli(nomvariable="TotBenefACTPPCH",denom="popTOT")  })

  output$partPCHpopEvol <- renderPlotly({graphEvolutionAppli(nomvariable="NbBenefPCH",denom="popTOT")  })
  output$partPCHpop <- renderPlotly({graphComparaisonAppli(nomvariable="NbBenefPCH",denom="popTOT")  })

  output$partACTPpopEvol <- renderPlotly({graphEvolutionAppli(nomvariable="NbBenefACTP",denom="popTOT")  })
  output$partACTPpop <- renderPlotly({graphComparaisonAppli(nomvariable="NbBenefACTP",denom="popTOT")  })

  # -- part des bénéficiaires de la PCH parmi l'ensemble ACTP+PCH
  output$partACTPprestaEvol <- renderPlotly({graphEvolutionAppli(nomvariable="NbBenefACTP",denom="TotBenefACTPPCH")  })
  output$partACTPpresta <- renderPlotly({graphComparaisonAppli(nomvariable="NbBenefACTP",denom="TotBenefACTPPCH")  })

  # -- part des bénéficiaires d'aide à l'accueil (ASH+accueil particulier et de jour) hors ACTP dans la population
  output$partAcchorsactppopEvol <- renderPlotly({graphEvolutionAppli(nomvariable="TotBenefPHEtab.horsACTP",denom="popTOT")  })
  output$partAcchorsactppop <- renderPlotly({graphComparaisonAppli(nomvariable="TotBenefPHEtab.horsACTP",denom="popTOT")  })

  # -- dépense moyenne par bénéficiaire ACTP et PCH
  #output$depmoyPCHACTPEvol <- renderPlotly({graphEvolutionAppli(nomvariable="",denom="TotBenefACTPPCH")  })
  #output$depmoyPCHACTP <- renderPlotly({graphComparaisonAppli(nomvariable="",denom="TotBenefACTPPCH")  })
  # !! reste à créer variable agrégée dep ACTP+PCH

  output$depmoyPCHEvol <- renderPlotly({graphEvolutionAppli(nomvariable="DepBrutePCH",denom="NbBenefPCH")  })
  output$depmoyPCH <- renderPlotly({graphComparaisonAppli(nomvariable="DepBrutePCH",denom="NbBenefPCH")  })

  output$depmoyACTPEvol <- renderPlotly({graphEvolutionAppli(nomvariable="DepBruteACTP",denom="NbBenefACTP")  })
  output$depmoyACTP <- renderPlotly({graphComparaisonAppli(nomvariable="DepBruteACTP",denom="NbBenefACTP")  })

  # -- dépense moyenne par bénéficiaire d'aide à l'accueil
  output$depmoyAccueiletabPHEvol <- renderPlotly({graphEvolutionAppli(nomvariable="DepBruteAidesAccueiletabPH",denom="NbBenefAideHebergementPH")  })
  output$depmoyAccueiletabPH <- renderPlotly({graphComparaisonAppli(nomvariable="DepBruteAidesAccueiletabPH",denom="NbBenefAideHebergementPH")  })

  # === 4) insertion

  # -- dépense moyenne nette d'allocation et d'insertion par habitant
  output$depmoyAllocInsEvol <- renderPlotly({graphEvolutionAppli(nomvariable="DepNetteInsertion",denom="popTOT")  })
  output$depmoyAllocIns <- renderPlotly({graphComparaisonAppli(nomvariable="DepNetteInsertion",denom="popTOT")  })

  # -- part des dépenses d'allocation dans le total des dépenses d'insertion
  output$partdepAllocEvol <- renderPlotly({graphEvolutionAppli(nomvariable="DepBruteRSA",denom="DepBruteInsertion")  })
  output$partdepAlloc <- renderPlotly({graphComparaisonAppli(nomvariable="DepBruteRSA",denom="DepBruteInsertion")  })

  # -- part des personnes orientées parmi les BRSA aux droits et devoirs
  output$partBRSAorientesEvol <- renderPlotly({graphEvolutionAppli(nomvariable="OarsaTabB1")  })
  output$partBRSAorientes <- renderPlotly({graphComparaisonAppli(nomvariable="OarsaTabB1")  })

  # -- part des personnes orientées vers Pôle Emploi parmi les BRSA aux droits et devoirs orientés
  output$partBRSAorientesPEEvol <- renderPlotly({graphEvolutionAppli(nomvariable="OarsaTabB5")  })
  output$partBRSAorientesPE <- renderPlotly({graphComparaisonAppli(nomvariable="OarsaTabB5")  })

  # -- part des personnes orientées vers le CD parmi les BRSA aux droits et devoirs orientés
  output$partBRSAorientesCDEvol <- renderPlotly({graphEvolutionAppli(nomvariable="OarsaTabB7")  })
  output$partBRSAorientesCD <- renderPlotly({graphComparaisonAppli(nomvariable="OarsaTabB7")  })

  # -- part des personnes ayant un CER parmi les personnes orientées hors Pôle Emploi
  output$partBRSACEREvol <- renderPlotly({graphEvolutionAppli(nomvariable="OarsaTabB9")  })
  output$partBRSACER <- renderPlotly({graphComparaisonAppli(nomvariable="OarsaTabB9")  })

  # === 5) personnels départementaux de l'aide sociale (ETP)

  # -- personnel médical
  output$persmedicalEvol <- renderPlotly({graphEvolutionAppli(nomvariable="etppersmedical",denom="popTOT")  })
  output$persmedical <- renderPlotly({graphComparaisonAppli(nomvariable="etppersmedical",denom="popTOT")  })
  # -- psychologues
  output$psychologuesEvol <- renderPlotly({graphEvolutionAppli(nomvariable="etppsychologues",denom="popTOT")  })
  output$psychologues <- renderPlotly({graphComparaisonAppli(nomvariable="etppsychologues",denom="popTOT")  })
  # -- personnel paramédical
  output$persparamedEvol <- renderPlotly({graphEvolutionAppli(nomvariable="etppersparamed",denom="popTOT")  })
  output$persparamed <- renderPlotly({graphComparaisonAppli(nomvariable="etppersparamed",denom="popTOT")  })
  # -- personnel social et éducatif
  output$perssocialeteducatifEvol <- renderPlotly({graphEvolutionAppli(nomvariable="etpperssocialeteducatif",denom="popTOT")  })
  output$perssocialeteducatif <- renderPlotly({graphComparaisonAppli(nomvariable="etpperssocialeteducatif",denom="popTOT")  })
  # -- personnel médicotechnique
  output$persmedicotechEvol <- renderPlotly({graphEvolutionAppli(nomvariable="etppersmedicotech",denom="popTOT")  })
  output$persmedicotech <- renderPlotly({graphComparaisonAppli(nomvariable="etppersmedicotech",denom="popTOT")  })
  # -- personnel adminsitratif et technique
  output$persadminettechEvol <- renderPlotly({graphEvolutionAppli(nomvariable="etppersadminettech",denom="popTOT")  })
  output$persadminettech <- renderPlotly({graphComparaisonAppli(nomvariable="etppersadminettech",denom="popTOT")  })
  # -- personnel total général hors assistants familiaux
  output$totalgeneralhorsAFEvol <- renderPlotly({graphEvolutionAppli(nomvariable="etptotalgeneralhorsAF",denom="popTOT")  })
  output$totalgeneralhorsAF <- renderPlotly({graphComparaisonAppli(nomvariable="etptotalgeneralhorsAF",denom="popTOT")  })
  # -- assistants familiaux
  output$persassfamEvol <- renderPlotly({graphEvolutionAppli(nomvariable="effpersassfam",denom="popTOT")  })
  output$persassfam <- renderPlotly({graphComparaisonAppli(nomvariable="effpersassfam",denom="popTOT")  })
  # -- personnel total général yc assistants familiaux
  output$totalgeneralycAFEvol <- renderPlotly({graphEvolutionAppli(nomvariable="efftotalgeneralycAF",denom="popTOT")  })
  output$totalgeneralycAF <- renderPlotly({graphComparaisonAppli(nomvariable="efftotalgeneralycAF",denom="popTOT")  })

  # === 6) minima sociaux

  # -- RSA et RMI
  output$rsaEvol <- renderPlotly({graphEvolutionAppli(nomvariable="RSA",denom="popTOT")  })
  output$rsa <- renderPlotly({graphComparaisonAppli(nomvariable="RSA",denom="popTOT")  })

  # -- AAH
  output$aahEvol <- renderPlotly({graphEvolutionAppli(nomvariable="AAH",denom="popTOT")  })
  output$aad <- renderPlotly({graphComparaisonAppli(nomvariable="AAH",denom="popTOT")  })

  # -- ASS
  output$assEvol <- renderPlotly({graphEvolutionAppli(nomvariable="ASS",denom="popTOT")  })
  output$asss <- renderPlotly({graphComparaisonAppli(nomvariable="ASS",denom="popTOT")  })
}
