\name{sim.bdtypes.stt.taxa}
\alias{sim.bdtypes.stt.taxa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{sim.bdtypes.stt.taxa: Simulating multitype birth-death trees with a fixed number of tips sampled through time.}
\description{
sim.bdtypes.stt.taxa simulates trees on n tips sampled through time under a multitype birth-death process.
}
\usage{
sim.bdtypes.stt.taxa(n,lambdavector,deathvector,sampprobvector,init=-1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Number of sampled leaves}
  \item{lambdavector}{Matrix of dimension kxk, where k is the number of different states. The entry (i,j) is the rate with which an individual in state i gives rise to a new lineage of state j.}
  \item{deathvector}{Vector of dimension k, the entry i is the death rate of an individual in state i.}
  \item{sampprobvector}{Vector of dimension k, the entry i is the probability of an individual in state i being sampled upon death, i.e. being included into the final tree.}
  \item{init}{Default is -1, meaning the initial individual is in a random state (which is chosen from the equilibrium distribution of states). If init>0, then the initial state is init.}
}
%\details{
%todo}
\value{
\item{out}{Phylogenetic tree with n sampled tips. In out$states, the states for the tips are stored.}
}
\references{ T. Stadler, S. Bonhoeffer: Uncovering epidemiological dynamics in heterogeneous host populations using phylogenetic methods. In press, Phil. Trans. Roy. Soc. B, 2012.
}
\author{Tanja Stadler}
\note{ 
A large number of trees can be obtained using the R function lapply. The tree can be plotted using the R package ape function plot(collapse.singles(out)). sim.bdtypes.stt.taxa function extends the simulator in the R package diversitree to trees which contain tips being sampled sequentially.}
\seealso{ sim.bdsky.stt }
\examples{
set.seed(1)
n<-10
lambda <- rbind(c(2,1),c(3,4))
mu <- c(1,1)
sampprob <-c(0.5,0.5)
numbsim<-2
trees<-lapply(rep(n,numbsim),sim.bdtypes.stt.taxa,
lambdavector=lambda,deathvector=mu,sampprobvector=sampprob)
}