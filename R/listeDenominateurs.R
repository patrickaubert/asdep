#' Une fonction déterminant les dénominateurs possibles d'une variable de la base ASDEPsl
#'
#' @param nomvariable le nom d'une variable de la base ASDEPsl
#' @param variables une table contenant les métadonnées sur les variables (par défaut, la table ASDEPsl_description est utilisée)
#'
#' @return une table contenant la liste des variables de la base ASDEPsl pouvant être utilisées comme dénominateurs, ainsi que la liste de leurs intitulés
#' @export
#'
#' @examples listeDenominateurs("NbBenefAPA")
#' @examples listeDenominateurs("NbBenefAPA")$denominateurs
#' @examples listeDenominateurs("NbBenefAPA")$intitules
listeDenominateurs <- function(nomvariable,
                               variables = ASDEPsl_description) {

  # =====
  # fonctions auxiliaires utiles pour déterminer la population de référence

  NommeVarpop <- function(x){
    if (grepl("[0-9][0-9]\\-[0-9][0-9]",x))  { return(paste(c("pop",as.numeric(as.vector(unlist(strsplit(x,split="-") )))),collapse=".")) }
    else {  return(x) }
  }

  Intitulepop <- function(nom){
    if (grepl("[0-9]\\.[0-9]",nom))  {
      tr <- as.vector(unlist(strsplit(nom,split="\\.") ))
      amin <- as.numeric(tr[2])
      amax <- as.numeric(tr[3])
      if (amin == 0) { return(paste(" de moins de ",(amax+1)," ans",sep="")) }
      else if (amax >= 99) { return(paste(" de ",amin," ans et plus",sep="")) }
      else { return(paste(" de ",amin," à ",amax," ans",sep="")) }
    }
    else if (nom == "popTOT") { return("") }
    else if (nom == "popPH") { return(Intitulepop("pop.20.64")) }
    else if (nom == "popASE") { return(Intitulepop("pop.00.19")) }
    else if (nom == "popPA") { return(Intitulepop("pop.60.99")) }
    else { return("")}
  }

  # =====
  # liste des caractéristiques associées à chaque variables, d'après la table de métadonnées
  listetypevariables <- as.list(setNames(variables$Type.var, variables$Nom.var))
  listedenominateursvariables <- as.list(setNames(variables$ListeDenom.var, variables$Nom.var))

  # =====
  # liste des dénominateurs possibles de la variable (suivant le type de variable)
  liste.denom <- list()

  # --- Cas 1 = la variable est de type "nombre de bénéficiaire"
  if (listetypevariables[[nomvariable]] == "Nombres de bénéficiaires") {
    select.denom <- NommeVarpop(variables[nomvariable,"Popref.var"])
    liste.denom[["Nombre de personnes"]] <- "nb"
    liste.denom[["En % de la population totale"]] <- "popTOT"
    liste.denom[[paste("En % de la population",Intitulepop(NommeVarpop(variables[nomvariable,"Popref.var"])))]] <- NommeVarpop(variables[nomvariable,"Popref.var"])

    # ajout des autres dénominateurs
    if (!(listedenominateursvariables[[nomvariable]] %in% c("",NA))) {
      autresdenom <- strsplit(listedenominateursvariables[[nomvariable]], split="_")[[1]]
      for (i in (1:length(autresdenom))) {  liste.denom[[paste("en % des ",variables[autresdenom[i],"TexteDenom"],sep="")]] <- autresdenom[i]    }
    }

  # --- Cas 2 = la variable est de type "montant"
  }  else if (listetypevariables[[nomvariable]] == "Montants") {

    select.denom <- NommeVarpop(variables[nomvariable,"Popref.var"])
    liste.denom[["Montants en \u20AC"]] <- "mont"
    liste.denom[["Par habitant"]] <- "popTOT"
    liste.denom[[paste("Par habitant",Intitulepop(NommeVarpop(variables[nomvariable,"Popref.var"])))]] <- NommeVarpop(variables[nomvariable,"Popref.var"])

    # ajout des autres dénominateurs
    if (!(listedenominateursvariables[[nomvariable]] %in% c("",NA))) {
      autresdenom <- strsplit(listedenominateursvariables[[nomvariable]], split="_")[[1]]
      select.denom <- autresdenom[1]
      for (i in (1:length(autresdenom))) {
        if (listetypevariables[[autresdenom[i]]] == "Nombres de bénéficiaires") { liste.denom[[paste("par ",variables[autresdenom[i],"TexteDenom"],sep="")]] <- autresdenom[i] }
        else if (listetypevariables[[autresdenom[i]]] == "Montants") { liste.denom[[paste("en % des ",variables[autresdenom[i],"TexteDenom"],sep="")]] <- autresdenom[i] }
      }
    }
  }

  # =====
  # résultat
  tab.denom <- data.frame(
    "denominateurs" = unlist(liste.denom),
    "intitules" = names(liste.denom),
    stringsAsFactors = FALSE
  )
  return(tab.denom)

}
