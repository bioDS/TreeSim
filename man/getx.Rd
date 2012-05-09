\name{getx}
\alias{getx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getx: Getting the vector of speciation times x from a phylogenetic tree
}
\description{
getx calculates the input vector x for the rate estimation functions from a phylogenetic tree (which may have polytomies)
}
\usage{
getx(datatree,sersampling)
}
\arguments{
  \item{datatree}{
phylogenetic tree
}
\item{sersampling}{sersampling = 0 if all tips are from one timepoint; 1 else}
}
\value{
\item{x}{Vector of speciation times; if sersampling = 1: second column indicates branching point (1) or sampling point (0).}
}
\author{
Tanja Stadler 
}
%\keyword{diversification rates}
%\keyword{rate shifts}
%\keyword{mass extinction}
%\keyword{maximum likelihood rates}
%\keyword{phylogeny}