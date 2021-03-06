% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/listeDenominateurs.R
\name{listeDenominateurs}
\alias{listeDenominateurs}
\title{Une fonction déterminant les dénominateurs possibles d'une variable de la base ASDEPsl}
\usage{
listeDenominateurs(nomvariable, variables = ASDEPsl_description)
}
\arguments{
\item{nomvariable}{le nom d'une variable de la base ASDEPsl}

\item{variables}{une table contenant les métadonnées sur les variables (par défaut, la table ASDEPsl_description est utilisée)}
}
\value{
une table contenant la liste des variables de la base ASDEPsl pouvant être utilisées comme dénominateurs, ainsi que la liste de leurs intitulés
}
\description{
Une fonction déterminant les dénominateurs possibles d'une variable de la base ASDEPsl
}
\examples{
listeDenominateurs("NbBenefAPA")
listeDenominateurs("NbBenefAPA")$denominateurs
listeDenominateurs("NbBenefAPA")$intitules
}
