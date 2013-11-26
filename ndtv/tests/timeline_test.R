#  part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2013 Statnet Commons
#######################################################################

# test the timeline functions
require(testthat)
require(ndtv)

# test on network datasets
data(windsurfers)
timeline(windsurfers)

data(McFarland_cls33_10_16_96)
timeline(cls33_10_16_96)

data(newcomb)
timeline(networkDynamic(network.list=newcomb))

data(stergm.sim.1)
timeline(stergm.sim.1)

# plot empty net
timeline(network.initialize(0))


# plot only vertices
timeline(windsurfers,plot.edge.spells=FALSE)

# plot only edges
timeline(windsurfers,plot.vertex.spells=FALSE)

# plot subset of vertices
timeline(windsurfers,v=5:10)

# plot only subset of edges
timeline(windsurfers,e=5:10)


# set colors
timeline(stergm.sim.1,vertex.col='orange',edge.col='green')

# test plotting spell bounds
slice.par<-list(start=0,end=24,interval=6, aggregate.dur=5,rule="any")
timeline(cls33_10_16_96,slice.par=slice.par)
slice.par<-list(start=0,end=24,interval=1, aggregate.dur=0,rule="any")
timeline(cls33_10_16_96,slice.par=slice.par)