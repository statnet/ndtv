\name{toy_epi_sim}
\alias{toy_epi_sim}
\docType{data}
\title{
Toy Epidemic Simulation Output from the EpiModel package
}
\description{
An example network of a trivial simulated disease process spreading over a simulated dynamic contact network among 100 individuals for 25 discrete time steps. 
}
\usage{data("toy_epi_sim")}
\format{
  The format is a \code{networkDynamic} object with attached attributes for \code{vertex.pid} (persistand ids), and dynamic attributes for \code{ndtvcol} (color corresponding to infection status) and \code{testatus} (infection status of vertices)
  
 
}
\details{
The \code{toy_epi_sim} network is example output from a basic dynamic network STERGM simulation and trivial "SI" infection simulation generated using the EpiModel package. The model had random ("edges only") edge formation and dissolution effects, with rates calculated to lead to mean edge durations of 10 time units.  The infection simulation had an infection probability of 0.8. 

 The simulation was generated with the following code: 
\preformatted{
library(EpiModel)

## Network Estimation (using a tergm model)
nw <- network.initialize(n = 100, directed = FALSE)
formation <- ~ edges
target.stats <- 50
dissolution <- ~ offset(edges)
coef.diss <- dissolution_coefs(dissolution, duration = 10)
est <- netest(nw,
              formation,
              dissolution,
              target.stats,
              coef.diss,
              verbose = FALSE)

## Epidemic simulation
param <- param.net(inf.prob = 0.8)
init <- init.net(i.num = 5)
control <- control.net(type = "SI", nsteps = 25, nsims = 1, verbose =
                         FALSE)
sim <- netsim(est, param, init, control)

## Use some of EpiModel's default coloring functions to cache colors
toy_epi_sim <- get_network(sim)
toy_epi_sim <- color_tea(toy_epi_sim)
}


}
\references{
Samuel Jenness, Steven M. Goodreau and Martina Morris (2015). EpiModel: Mathematical Modeling of Infectious
  Disease. R package version 1.1.4. \url{http://CRAN.R-project.org/package=EpiModel}
  
Statnet EpiModel Tutorial \url{http://statnet.org/EpiModel/vignette/Tutorial.pdf}  
}
\seealso{ 
See also \code{\link{short.stergm.sim}} for another basic Stergm simulation output, and \code{\link{msm.sim}} for a larger and more complex simulation without an infection process.  
}
\examples{
data(toy_epi_sim)
timeline(toy_epi_sim)
\dontrun{
# render an animation of the network
render.animation(toy_epi_sim,vertex.col='ndtvcol',displaylabels=FALSE)
ani.replay()
}


}
\keyword{datasets}
