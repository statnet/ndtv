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
\alias{short.stergm.sim}
\docType{data}
\title{
Very Very Basic stergm simulation output
}
\description{
Simulation from a crude stergm model based on the flobusiness network. Mostly good for testing movies 'cause it is small (16 vertices) and fast. The \code{stergm.sim.1} network is 100 simulation steps in duration. The \code{short.stergm.sim} network is an extract of the first 25 steps of \code{stergm.sim.1} -- its shorter duration makes it more suitable for quickly testing animation techniques. 
}
\usage{
data(stergm.sim.1)
data(short.stergm.sim)
}

\format{
  A \code{networkDynamic} object containing the output of 100 steps of the simulation
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
                    nsim=1, time.slices = 100)
}

However, the ergm-related output that would normally be attached to the network (toggles, etc) has been removed.

}
\source{
See tergm package tutorials. 
}

\examples{
data(stergm.sim.1)
range(get.change.times(stergm.sim.1))
data(short.stergm.sim)
range(get.change.times(short.stergm.sim))

}
\keyword{datasets}
