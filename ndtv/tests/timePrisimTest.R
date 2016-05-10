#  part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2013 Statnet Commons
#######################################################################
require(ndtv)
require(testthat)

data("short.stergm.sim")
compute.animation(short.stergm.sim)
timePrism(short.stergm.sim,at=c(1,10,20),
          displaylabels=TRUE,
          label.cex=0.5)

data(toy_epi_sim)  
timePrism(toy_epi_sim,
          orientation=c('z','y','x'),
          angle=40,
          spline.v=c(7, 29, 36, 70, 82, 96),  # hilite the infected 
          spline.col='red',
          spline.lwd=2,
          box=FALSE,
          planes=TRUE,
          vertex.col='ndtvcol')