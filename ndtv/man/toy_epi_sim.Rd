%  File man/toy_epi_sim.Rd in package ndtv, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2012-2016 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
  
Statnet EpiModel Tutorial \url{http://www.epimodel.org/}  
}
\seealso{ 
See also \code{\link{short.stergm.sim}} for another basic Stergm simulation output, and \code{\link{msm.sim}} for a larger and more complex simulation without an infection process.  
}
\examples{
data(toy_epi_sim)
timeline(toy_epi_sim)
\dontrun{


# set up layout to draw plots under timeline
layout(matrix(c(1,1,1,2,3,4),nrow=2,ncol=3,byrow=TRUE))
# plot a proximity.timeline illustrating infection spread
proximity.timeline(toy_epi_sim,vertex.col = 'ndtvcol',
                  spline.style='color.attribute',
                  mode = 'sammon',default.dist=100,
                  chain.direction='reverse')
# plot 3 static cross-sectional networks 
# (beginning, middle and end) underneath for comparison
plot(network.collapse(toy_epi_sim,at=1),vertex.col='ndtvcol',
       main='toy_epi_sim network at t=1')
plot(network.collapse(toy_epi_sim,at=17),vertex.col='ndtvcol',
       main='toy_epi_sim network at=17')
plot(network.collapse(toy_epi_sim,at=25),vertex.col='ndtvcol',
       main='toy_epi_sim network at t=25')
layout(1) # reset the layout


# render an animation of the network
render.animation(toy_epi_sim,vertex.col='ndtvcol',displaylabels=FALSE)
ani.replay()
}


}
\keyword{datasets}
