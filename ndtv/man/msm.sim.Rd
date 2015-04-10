%  File man/msm.sim.Rd in package ndtv, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2012-2013 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{msm.sim}
\alias{msm.sim}
\docType{data}
\title{MSM.sim : output of a stergm simulation of basic sex contact network model}
\description{
A 1000 vertex networkDynamic object that contains the output of 10 timesteps of a discrete stergm simulation of a basic sex contact network model. The model has two vertex types ('races') with different contact preferencs expressed with a \code{nodematch} parameter.   The output network object is included as an example because re-running the model can take a while.
}
\usage{data(msm.sim)}
\format{a \code{\link{networkDynamic} object}}
\details{
The model was built with the following stergm:
\preformatted{
msm.net <- network.initialize(1000, directed=F)  
msm.net \%v\% 'race' <- rep(c(1,2),500)
sm.form.constraints <- ~bd(maxout=2)
msm.form.formula <- ~edges+nodematch('race')+
	degree(2)
msm.target.stats <- c(450,375,50)
msm.diss.formula <- ~offset(edges)+offset(nodematch("race"))
msm.theta.diss <- c(2.944, -0.747)
msm.fit <- stergm(msm.net,
	formation= msm.form.formula,
	dissolution= msm.diss.formula,
	targets="formation",
	target.stats= msm.target.stats,
	offset.coef.diss = msm.theta.diss,
	form.constraints=msm.form.constraints,
	estimate = "EGMME",
	control=control.stergm(SA.plot.progress=TRUE)
)																				
msm.sim <- simulate(msm.fit,nsim=1,time.slices=100)
}
However, the tergm-related output that would normally be attached to the network (toggles, etc) has been removed.
}

\source{
statnet project stergm tutorial. 
}

\examples{
require(network)
require(networkDynamic)
data(msm.sim)
# show the network, aggregating 5 timesteps
plot(network.extract(msm.sim,onset=0,terminus=5),
  vertex.col=msm.sim\%v\%"race",vertex.cex=0.5,edge.col="gray")

# this could take 10 minutes, so don't run in example checking
\dontrun{
# use ndtv to render a movie of the momentary view of the network
render.animation(msm.sim,vertex.col=msm.sim\%v\%'race',vertex.cex=.5)
ani.replay()
saveVideo(ani.replay(),video.name="msm.simMomentary.mp4", other.opts="-b 5000k",clean=TRUE)

# another version, this time with more temporal aggregation
msm.sim <- compute.animation(msm.sim,slice.par=list(start=0,
                                                    end=10,
                                                    interval=1,
                                                    aggregate.dur=3,
                                                    rule='latest'))
# also more render more inbetween frames
render.animation(msm.sim,render.par = list(tween.frames = 15,show.times=TRUE), 
  vertex.col=msm.net\%v\%'race',vertex.cex=.5)
saveVideo(ani.replay(),video.name="msm.sim3Aggregated.mp4", other.opts="-b 5000k",clean=TRUE)
}
}
\keyword{datasets}
