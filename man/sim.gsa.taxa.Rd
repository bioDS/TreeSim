\name{sim.gsa.taxa}
\alias{sim.gsa.taxa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
sim.gsa.taxa: Sampling trees on n tips from bigger trees}
\description{
sim.gsa.taxa samples trees on n tips appropriately (using the GSA approach, see references) from bigger trees simulated under the simple sampling approach (i.e. simulating until first m>>n tips are reached or the tree is extinct). This methods works for m-tip trees generated under a model where: (i) the number of tips eventually tends to zero or stays bigger than n and (ii) rate changes do not depend on the time between the change and the present - e.g. one cannot model a mass extinction event 1 million years BEFORE the present. But one can model a mass extinction event 1 million years AFTER the origin of the tree. 

Note that the number of trees in the output is significantly smaller than the number of trees in the input (in order to have correct sampling).}
\usage{
sim.gsa.taxa(treearray, n, frac = 1, sampling = 40, complete = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{treearray}{Array of trees with a fixed number of leaves}
  \item{n}{Number of tips in sampled trees}
  \item{frac}{Relevant when complete = FALSE: The actual number of tips is n/frac, but only n tips are included (incomplete sampling).
When complete = TRUE: We set frac = 1.}
  \item{sampling}{Default is 40, the higher the closer the returned trees in treearray are to the "true" distribution, but the more trees are required in treearray to reach a big sample of output trees.}
  \item{complete}{If TRUE, the tree with the extinct lineages is returned. If FALSE, the extinct lineages are suppressed.}
}
%\details{
  %~~ If necessary, more details than the description above ~~
%}
\value{
\item{treearray}{Array of sampled trees with n  extant sampled leaves}
}
\references{T. Stadler: Simulating trees on a fixed number of extant species. Submitted. 2010.

K. Hartmann, D. Wong, T. Stadler: Sampling trees from evolutionary models. To appear in Syst. Biol. 2010.}
\author{Tanja Stadler}
%\note{ %~~further notes~~ 
%~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{sim.bd.age, sim.bd.taxa, sim.rateshift.taxa, birthdeath.tree
} %~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##
# First 100 trees on 9 leaves under a birth-death process are generated.
# Then trees on 5 species are sampled from these 100 trees using the GSA
# (see references). 
# You can easily simulate trees on m species with the simple sampling
# approach (see references) under a variety of models. Then use the
# provided GSA algorithm to get a correct sample of trees on n<<m species:

m<-9
n<-5
numbsim<-100
lambda <- 2.0
mu <- 0.5

t<-sim.bd.taxa(m,numbsim,lambda,mu)
t2<-sim.gsa.taxa(t,n)
}

\keyword{tree}
\keyword{general sampling approach}% __ONLY ONE__ keyword per line
\keyword{simulation}
