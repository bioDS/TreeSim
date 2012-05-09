\name{sim.bd.age}
\alias{sim.bd.age}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{sim.bd.age: Simulating birth-death trees of a fixed age}
\description{
sim.bd.age simulates trees conditioned on (i) the time since origin or (ii) the time since the most recent common ancestor of the extant tips under the constant rate birth-death process. The method allows for incomplete sampling: only a fixed fraction of all tips is included in the sampled tree. The method assumes constant birth and death rates, or allows for a density-dependent birth rate.}
\usage{
sim.bd.age(age, numbsim, lambda, mu, frac = 1, mrca = FALSE,
 complete = TRUE, K = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{age}{Time since origin / most recent common ancestor}
  \item{numbsim}{Number of trees to simulate}
  \item{lambda}{Speciation rate}
  \item{K}{If K=0, then lambda is constant. If K>0, density-dependent speciation is assumed, with speciation rate lambda(1-m/K) when there are m extant species.}
  \item{mu}{Extinction rate}
  \item{frac}{Sampling fraction: The actual number of tips is n/frac, but only n tips are included (incomplete sampling).}  
  \item{mrca}{If mrca=FALSE: age is time since origin. If mrca=TRUE: age is time since most recent common ancestor of the extant tips.}
  \item{complete}{If complete = TRUE, the tree with the extinct and non-sampled lineages is returned. If complete = FALSE, the extinct and non-sampled lineages are suppressed.}
}
%\details{
%todo
%}
\value{
\item{treearray }{Array of numbsim trees with the time since origin / most recent common ancestor being age. If tree goes extinct (only possible when mrca = FALSE), returns 0. If only one extant and no extinct tips, returns 1.}
}
\references{T. Stadler: Simulating trees on a fixed number of extant species. Syst. Biol. (2011) 60: 676-684. }
\author{Tanja Stadler}
%\note{%todo
%}
\seealso{ sim.bd.taxa, sim.rateshift.taxa, sim.gsa.taxa, birthdeath.tree }
\examples{
age<-2
lambda <- 2.0
mu <- 0.5
frac <-0.6
numbsim<-3

##
# Simulating trees with time age since the origin:

sim.bd.age(age,numbsim,lambda,mu,mrca=FALSE,complete=TRUE)
sim.bd.age(age,numbsim,lambda,mu,frac,mrca=FALSE,complete=FALSE)

# Simulating trees with time age since the time of the most recent common 
# ancestor of the extant and sampled species:

sim.bd.age(age,numbsim,lambda,mu,mrca=TRUE,complete=TRUE)
sim.bd.age(age,numbsim,lambda,mu,frac,mrca=TRUE,complete=FALSE)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{birth-death tree}
%\keyword{phylogenetic tree}
%\keyword{simulation}% __ONLY ONE__ keyword per line
