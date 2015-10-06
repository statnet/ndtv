#  File tests/networkAnimationTest.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2003-2012 Statnet Commons
#######################################################################
#test some auxillury functions for calculating  layouts etc.
require(ndtv)
require(testthat)


# ----- java tests ----

# does the check for java crash?
ndtv:::check.java()

# ----- ffmpeg tests -----
# does the check for ffmpeg crash?
try(ndtv:::check.ffmpeg()) # this fails if no ffmpeg, so would stop the test on cran
ndtv:::install.ffmpeg()

# ---- layout.distance -----
mat <-matrix(c(1,2,0,0, 0,0,0,0, 0,3,0,0, 0,0,0,0),ncol=4,byrow=TRUE)
net <- as.network(mat)
symat <-as.sociomatrix(layout.distance(net))
if (!all(symat == matrix(c(0,1,2,2, 1,0,1,2, 2,1,0,2, 2,2,2,0),ncol=4,byrow=TRUE))){
  stop("layout.distance did not return symetric geodedesic distance matrix as expected")
}

# does default dist work
symat <-as.sociomatrix(layout.distance(net,default.dist=20))
if (!all(symat == matrix(c(0,1,2,20, 1,0,1,20, 2,1,0,20, 20,20,20,0),ncol=4,byrow=TRUE))){
  stop("layout.distance did not return symetric geodedesic distance with default.dist as expected")
}


# test using a function to specifiy a graphic plotting property
test<-network.initialize(5)
add.edges.active(test,onset=0,terminus=10,tail=1:4,head=2:5)
activate.vertices(test,v=5,onset=4,terminus=10)
activate.vertex.attribute(test,'status',0,onset=0,terminus=10)
activate.vertex.attribute(test,'status',1,onset=1,terminus=10,v=1)
activate.vertex.attribute(test,'status',1,onset=2,terminus=10,v=2)
activate.vertex.attribute(test,'status',1,onset=3,terminus=10,v=3)
activate.vertex.attribute(test,'status',1,onset=4,terminus=10,v=4)
compute.animation(test)
render.animation(test,vertex.col=function(slice){ifelse(slice%v%'status'==1,'red','blue')})







