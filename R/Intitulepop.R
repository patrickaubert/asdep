#' Fonction auxiliaire associant un intitulé en clair à un nom de variable de population
#'
#' @param nom un nom de variable de population (dans la table PopDepartementales)
#'
#' @return un intitulé en clair
#' @export
#'
#' @examples Intitulepop("pop.20.59")
#' @examples Intitulepop("pop.60.99")
#' @examples Intitulepop("popTOT")
#' @examples Intitulepop("popASE")
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
