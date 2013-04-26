%  File man/stergm.sim.1.Rd in package ndtv, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2012-2013 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{stergm.sim.1}
\alias{stergm.sim.1}
\docType{data}
\title{
Very Very Basic stergm simulation output
}
\description{
Simulation from a crude stergm model based on the flobusiness network. Mostly good for testing movies 'cause it is small (16 vertices) and fast. 
}
\usage{data(stergm.sim.1)}
\format{
  A \code{networkDynamic} object containing the output of 100 steps the simulation
}
\details{
The model used to generate the sim was:
\code{require(ergm)
data("florentine")
theta.diss <- log(9)
# fit the model
stergm.fit.1 <- stergm(flobusiness, 
  formation= ~edges+gwesp(0,fixed=T),
  dissolution = ~offset(edges),
	targets="formation",
	offset.coef.diss = theta.diss,
	estimate = "EGMME"	)
stergm.sim.1 <- simulate.stergm(stergm.fit.1,
                    nsim=1, time.slices = 100)}
                    
NOTE: this is outdated syntax from old versions of statnet. 

}
\source{
See tergm package tutorials. 
}

\examples{
data(stergm.sim.1)

}
\keyword{datasets}
