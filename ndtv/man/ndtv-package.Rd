%  File man/ndtv-package.Rd in package ndtv, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2012-2013 Statnet Commons
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
\tabular{ll}{
Package: \tab ndtv\cr
Type: \tab Package\cr
Version: \tab 0.3\cr
Date: \tab 2013-04-19\cr
License: \tab GNU GPL 3.0\cr
}
Requires \link[network]{network} objects with dynamics encoded using \link[networkDynamic]{networkDynamic}.  Compute a layout using \link{compute.animation} and render it as a movie using \link{render.animation}. To view package vignette, run \code{browseVignettes(package='ndtv')}.
}
\author{
Skye Bender-deMoll, and the Statnet Commons team
Maintainer: Skye <skyebend@uw.edu>

}
\references{
Bender-deMoll, S., Morris, M. and Moody, J. (2008) Prototype Packages for Managing and Animating Longitudinal Network Data: dynamicnetwork and rSoNIA \emph{Journal of Statistical Software} 24:7.

Hunter DR, Handcock MS, Butts CT, Goodreau SM, Morris M (2008b).  ergm: A Package to Fit, Simulate and Diagnose Exponential-Family Models for Networks.  \emph{Journal of Statistical Software}, 24(3). \url{http://www.jstatsoft.org/v24/i03/}. 


Butts CT (2008). network: A Package for Managing Relational Data in R.  \emph{Journal of Statistical Software}, 24(2). \url{http://www.jstatsoft.org/v24/i02/}. 

Skye Bender-deMoll and McFarland, Daniel A. (2006)  The Art and Science of Dynamic Network Visualization. \emph{Journal of Social Structure. Volume 7, Number 2} \url{http://www.cmu.edu/joss/content/articles/volume7/deMollMcFarland/}
}





\keyword{ package }
\seealso{
 \code{\link[networkDynamic:networkDynamic-package]{networkDynamic}}, \code{\link{compute.animation}},\code{\link{render.animation}} for examples, and the package vignette \code{vignette(package='ndtv')}.
}

