\name{LTT.average.root}
\alias{LTT.average.root}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{LTT.average.root: plots the average LTT plot of a set of phylogenetic trees}
\description{
LTT.average.root plots the average LTT plot of a set of phylogenetic trees of the same age (ie. time since root). The trees may be simulated using sim.bd.age in TreeSim.
}
\usage{
LTT.average.root(trees)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trees}{list of phylogenetic trees}
}
%\details{
%todo}
\value{
\item{ltt}{Vector containing average LTT plot. Visualize with plot(ltt,type='l',col='black',log="y",xlab="time",ylab="number of species")}
}
\references{ T. Stadler: Simulating trees on a fixed number of extant species. Syst. Biol. (2011).
}
\author{Tanja Stadler }
\seealso{ sim.bd.age, sim.rateshift.taxa, sim.gsa.taxa, birthdeath.tree }
\examples{

numbsim<-10
age<-10
lambda<-0.3
mu<-0.1
K<-40

tree<- sim.bd.age(age,numbsim,lambda,mu,mrca=TRUE,complete=FALSE,K=K)
ltttrees<-LTT.average.root(tree)
plot(ltttrees,type='l',col='black',log="y",xlab="time",ylab="number of species")
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{birth-death tree}
%\keyword{ phylogenetic tree}
%\keyword{simulation}% __ONLY ONE__ keyword per line
