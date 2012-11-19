\name{sim.bdsky.stt}
\alias{sim.bdsky.stt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{sim.bdsky.stt: Simulating birth-death trees under a model where birth and death rates are changing in a piecewise fashion.}
\description{
sim.bdsky.stt simulates birth-death trees under a model where birth and death rates are changing in a piecewise fashion. The trees are conditioned on a fixed number of tips or a fixed age. Tips are sampled through time.}
\usage{
sim.bdsky.stt(n,lambdasky,deathsky,timesky,sampprobsky,rho=0,timestop=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{If n>0, then the simulation is stopped once n tips are sampled sequentially through time. If n=0, the simulation is stopped after time timestop.}
  \item{lambdasky}{Vector of dimension k, where k is the number of different birth rates. An individual between time (timesky[i],timesky[i+1]) has birth rate lambdasky[i].}
  \item{deathsky}{Vector of dimension k, where k is the number of different death rates. An individual between time (timesky[i],timesky[i+1]) has death rate deathsky[i].}
  \item{timesky}{Vector of dimension k, containing the times of rate shifts. timesky[1]=0 denotes the time of the first individual.}
  \item{sampprobsky}{Vector of dimension k, an individual dying during time (timesky[i],timesky[i+1]) is sampled with probability sampprobsky[i], i.e. is being included into the final tree.}
  \item{rho}{Default is rho=0. If rho>0 and timestop>0, then the process is stopped after timestop and each alive individual at time timestop is included into the final tree with probability rho.}
  \item{timestop}{Default is timestop=0, meaning the simulation is stopped once n tips are sampled. If timestop>0, then the simulation is stopped after time timestop.}
}
%\details{
%todo}
\value{
\item{out}{Phylogenetic tree with n sampled tips or a fixed age timestop.}
}
\references{ T. Stadler, D. Kuehnert, S. Bonhoeffer, A. Drummond: The birth-death skyline plot. In press, Proc. Nat. Acad. Sci., 2012.
}
\author{Tanja Stadler}
\note{ 
A large number of trees can be obtained using the R function lapply. The tree can be plotted using the R package ape function plot(collapse.singles(out)).  sim.bdsky.stt function extends the function sim.rateshift.taxa to trees which contain tips being sampled sequentially.}
\seealso{ sim.bdsky.stt.taxa }
\examples{
set.seed(1)
n<-10
lambda <- c(2,1,2)
mu <- c(1,0.5,1.5)
sampprob <-c(0.5,0.5,0.5)
times<-c(0,1,2)
numbsim<-2
trees<-lapply(rep(n,numbsim),sim.bdsky.stt,lambdasky=lambda,deathsky=mu,timesky=times,sampprobsky=sampprob,rho=0,timestop=0)
}