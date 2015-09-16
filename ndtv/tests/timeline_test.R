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

# test vertex color expansions
timeline(stergm.sim.1,vertex.col='priorates')
timeline(stergm.sim.1,v.label.col='priorates')

# test edge colors
# show only edge spells, hilite edge id 20
set.edge.attribute(stergm.sim.1,'my_color','gray')
set.edge.attribute(stergm.sim.1,'my_color','red',e=20)
timeline(stergm.sim.1,edge.col='my_color',plot.vertex.spells=FALSE)
timeline(stergm.sim.1,edge.col=1:10)

# test label expansions
timeline(stergm.sim.1,v.label='a')
timeline(stergm.sim.1,v.label='priorates')
timeline(stergm.sim.1,e.label='a')
timeline(stergm.sim.1,e.label='my_color')


# test vertex line size (vertex.cex)
timeline(stergm.sim.1,vertex.cex=stergm.sim.1%v%'priorates'/5,plot.edge.spells = FALSE)


# test edge line size
timeline(windsurfers,edge.lwd=5,xlim=c(0,1),plot.vertex.spells = FALSE)  # this will only work with update to network
set.edge.attribute(windsurfers,'value',1:5)
timeline(windsurfers,edge.lwd='value',xlim=c(0,1),plot.vertex.spells = FALSE)
timeline(windsurfers,edge.lwd=windsurfers%e%'value',xlim=c(0,1),plot.vertex.spells = FALSE)

# test plotting spell bounds
slice.par<-list(start=0,end=24,interval=6, aggregate.dur=5,rule="any")
timeline(cls33_10_16_96,slice.par=slice.par)
slice.par<-list(start=0,end=24,interval=1, aggregate.dur=0,rule="any")
timeline(cls33_10_16_96,slice.par=slice.par)

# test setting plot bounds, bug #735
timeline(cls33_10_16_96,ylim=c(20,30))
timeline(cls33_10_16_96,xlim=c(0,10))
