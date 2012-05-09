\name{LTT.plot}
\alias{LTT.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{LTT.plot: plots the lineages through time of a set of phylogenetic trees}
\description{
LTT.plot plots the LTT for a set of phylogenetic trees in black (complete or reconstructed; with or without polytomies) together with the average LTT plot in red. The trees may be simulated using any function in TreeSim, or may be empirical trees. Polytomies and non-ultrametric trees are ok. NOTE: you probably need to adopt the code such that the plot is pretty for your particular data (range of axes etc).}
\usage{
LTT.plot(trees,width,precalc,bound=10^(-12),timemax,nmax,avg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trees}{List with one or two entries. First component: list of phylogenetic trees; second component: vector with time of origins (can be empty).}
  \item{width}{Width of lines in plot}
  \item{precalc}{Default = 0. If = 1, then parse 'LTT.plot.gen(trees)' instead of 'trees' for the variable trees.}
  \item{bound}{Determines the value by which leaf times may differ in an ultrametric tree. If two tips are further apart, they are considered as sequentially sampled tips.}
  \item{timemax}{Time axis is drawn from present to timemax years in the past.}
  \item{nmax}{Axis with number of species is drawn from 1 to nmax.}
  \item{avg}{Default=FALSE. If true then the average LTT plot of all individual LTT plots is drawn in red.}
}
%\details{
%todo}
\references{ T. Stadler: Simulating trees on a fixed number of extant species. Syst. Biol. (2011) 60: 676-684.
}
\author{Tanja Stadler }
\seealso{LTT.plot.gen, sim.bd.taxa, sim.bd.age, sim.rateshift.taxa, sim.gsa.taxa, birthdeath.tree }
\examples{

numbsim<-3
age<-10
lambda<-0.3
mu<-0
K<-40

tree<- sim.bd.age(age,numbsim,lambda,mu,mrca=FALSE,complete=FALSE,K=K)
LTT.plot(c(list(tree),list(c(age,age,age))))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{birth-death tree}
%\keyword{ phylogenetic tree}
%\keyword{simulation}% __ONLY ONE__ keyword per line
