%  File man/ndtv-package.Rd in package ndtv, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2012-2016 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{ndtv-package}
\alias{ndtv-package}
\alias{ndtv}
\docType{package}
\title{
Network Dynamic Temporal Visualization (ndtv)
}
\description{
Construct visualizations such as timelines and animated movies of networkDynamic objects to show changes in structure and attributes over time.
}
\details{

For version and license information, run \code{packageDescription('ndtv')}

Uses \link[network]{network} objects with dynamics encoded using \link[networkDynamic]{networkDynamic}. 

Key features:
\itemize{
  \item Compute a dynamic layout using \link{compute.animation}.
  \item Render it as a movie using \link{render.animation}.
  \item Render animations to a web page using \link{render.d3movie}
  \item Plot multiple 'stills' of a movie with \link{filmstrip}
  \item Plot a timeline of edge and vertex activity with \link{timeline}
  \item Plot network geodesic proximities as a stream graph with \link{proximity.timeline}
}


To view package vignettes and extended examples, see \code{browseVignettes(package='ndtv')}.

For a more indpeth tutorial, see \url{http://statnet.csde.washington.edu/workshops/SUNBELT/current/ndtv/ndtv_workshop.html}

The package includes several example datasets:

\itemize{
\item \code{\link{msm.sim}} output of a stergm simulation of basic sex contact network model
\item \code{\link{short.stergm.sim}} Very Very Basic stergm simulation output (25 time steps)
\item \code{\link{stergm.sim.1}}    Very Very Basic stergm simulation output (100 time steps)
\item \code{\link{toy_epi_sim}}     Toy Epidemic Simulation Output from the EpiModel package
}

}
\author{
Skye Bender-deMoll, and the Statnet Commons team
Maintainer: Skye <skyebend@uw.edu>

}
\references{
Bender-deMoll, S., Morris, M. and Moody, J. (2008) Prototype Packages for Managing and Animating Longitudinal Network Data: dynamicnetwork and rSoNIA \emph{Journal of Statistical Software} 24:7.

Hunter DR, Handcock MS, Butts CT, Goodreau SM, Morris M (2008b).  ergm: A Package to Fit, Simulate and Diagnose Exponential-Family Models for Networks.  \emph{Journal of Statistical Software}, 24(3). \url{http://www.jstatsoft.org/v24/i03/}. 


Butts CT (2008). network: A Package for Managing Relational Data in R.  \emph{Journal of Statistical Software}, 24(2). \url{http://www.jstatsoft.org/v24/i02/}. 

Carter T. Butts, Ayn Leslie-Cook, Pavel N. Krivitsky and Skye Bender-deMoll (2015). networkDynamic: Dynamic Extensions for Network Objects. R package version 0.7. \url{http://statnet.org}

Skye Bender-deMoll and McFarland, Daniel A. (2006)  The Art and Science of Dynamic Network Visualization. \emph{Journal of Social Structure. Volume 7, Number 2} \url{http://www.cmu.edu/joss/content/articles/volume7/deMollMcFarland/}
}





\keyword{ package }
\seealso{
 \code{\link[networkDynamic:networkDynamic-package]{networkDynamic}}, \code{\link{compute.animation}},\code{\link{render.animation}} for examples, and the package vignette \code{vignette(package='ndtv')}.
}

