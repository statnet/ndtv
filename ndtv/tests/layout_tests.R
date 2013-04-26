#  part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2013 Statnet Commons
#######################################################################

# tests for the network animation layout functions
require(ndtv)
require(testthat)
# ---- test using a dynamic attribute for layout positions ----

# check what happens if coords not defined
nd <-as.networkDynamic(network.initialize(3))
expect_error(compute.animation(nd,animation.mode='useAttribute'), 'must have layout.par')

#check when coordinates specified
nd <-network.initialize(3)
activate.vertex.attribute(nd,'x',c(1,2,3),at=0)
activate.vertex.attribute(nd,'y',c(0,0,0),at=0)
activate.vertex.attribute(nd,'x',c(2,3,4),at=1)
activate.vertex.attribute(nd,'y',c(1,1,1),at=1)
activate.vertex.attribute(nd,'x',c(3,4,5),at=2)
activate.vertex.attribute(nd,'y',c(2,2,2),at=2)

compute.animation(nd,animation.mode='useAttribute',layout.par=list(x='x',y='y'))
expect_equal(unlist(get.vertex.attribute(nd,'x.active',unlist=FALSE)[[1]][[1]]),1:3)


# check what happens if coords missing for time period
nd <-network.initialize(3)
activate.vertex.attribute(nd,'x',c(1,2,3),at=0)
activate.vertex.attribute(nd,'y',c(0,0,0),at=0)
activate.vertex.attribute(nd,'x',c(2,3,4),at=3)
activate.vertex.attribute(nd,'y',c(1,1,1),at=3)
activate.vertex.attribute(nd,'x',c(3,4,5),at=4)
activate.vertex.attribute(nd,'y',c(2,2,2),at=4)
expect_error(compute.animation(nd,animation.mode='useAttribute',layout.par=list(x='x',y='y')),'coordinates must be numeric')

# ------ test layout.distance function ----
test<-network.initialize(4)
add.edges(test,tail=1:2,head=2:3)
expect_equal(layout.distance(test),matrix(c(0,1,2,2,1,0,1,2,2,1,0,2,2,2,2,0),ncol=4,byrow=TRUE), info='test layout distance')
expect_equal(layout.distance(test,.5),matrix(c(0,1,2,.5, 1,0,1,.5, 2,1,0,.5, .5,.5,.5,0),ncol=4,byrow=TRUE),info='test inf replace value for layout distance')

set.edge.attribute(test,'weight',c(3,6))
expect_equal(layout.distance(test,weight.attr='weight'),matrix(c(0, 6, 9, 2, 6, 0, 3, 2, 9, 3, 0, 2, 2, 2, 2, 0),ncol=4,byro=TRUE),info='test layout distance with weights')

expect_equal(layout.distance(test,weight.attr='weight',weight.dist=TRUE),matrix(c(0, 3, 9, 2, 3, 0, 6, 2, 9, 6, 0, 2, 2, 2, 2, 0),ncol=4,byro=TRUE),info='test layout distance with weights with weights as distance')

# ----- test layout centering ----

pos<-matrix(c(1:5,1:5),ncol=2,byrow=FALSE)
