#  part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2013 Statnet Commons
#######################################################################

# crude tests for some of the basic file writing
# does not test that output is correct, just that code runs
require(testthat)
require(ndtv)
data("toy_epi_sim")

# fudge some attributes
toy_epi_sim%v%'x'<-runif(network.size(toy_epi_sim))
toy_epi_sim%v%'y'<-runif(network.size(toy_epi_sim))
toy_epi_sim%v%'z'<-runif(network.size(toy_epi_sim))
toy_epi_sim%v%'color'<-'blue'
toy_epi_sim%v%'shape'<-'rect'
toy_epi_sim%v%'label'<-LETTERS


toy_epi_sim%e%'color'<-'gray'
toy_epi_sim%e%'width'<-2
toy_epi_sim%e%'weight'<-runif(network.edgecount(toy_epi_sim))

export.pajek.net(toy_epi_sim,filename='test.net')
