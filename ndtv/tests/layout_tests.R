#  part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2014 Statnet Commons
#######################################################################

# tests for the network animation layout functions
require(ndtv)
require(testthat)
# ---- test using a dynamic attribute for layout positions ----

# check what happens if coords not defined
nd <-as.networkDynamic(network.initialize(3))
expect_error(compute.animation(nd,animation.mode='useAttribute'), 'unable to find vertex attribute x containing x')

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

# test symetrization
test_that("symetrization of directed network according to weight.dist",{
  test<-network.initialize(2,directed=TRUE)
  test[1,2]<-1
  test[2,1]<-1
  set.edge.attribute(test,'count',value=c(1,2))
  expect_equal(as.vector(layout.distance(test,weight.attr='count',weight.dist=TRUE)),c(0,1,1,0))
  expect_equal(as.vector(layout.distance(test,weight.attr='count',weight.dist=FALSE)),c(0,2,2,0))
})

# ----- test layout centering ----

pos<-matrix(c(1:5,1:5),ncol=2,byrow=FALSE)

# center on 0,0
expect_equal(layout.center(pos,xlim=c(0,0),ylim=c(0,0)),pos-3)

# center in 100 px square
expect_equal(layout.center(pos,xlim=c(0,100),ylim=c(0,100)),matrix(c(48, 49, 50, 51, 52, 48, 49, 50, 51, 52),ncol=2))


# ----- test layout zooming ------

pos<-matrix(c(1:5,1:5),ncol=2,byrow=FALSE)

# factor 0 does nothing
expect_equal(ndtv:::layout.zoomfactor(pos,0),pos)

# factor 10 (scale at origin)
expect_equal(ndtv:::layout.zoomfactor(pos,10),pos*10)

# factor 10, scale at 1,1
expect_equal(ndtv:::layout.zoomfactor(pos,10,center=c(1,1)),(pos*10)-9)

# zoom on set of vertices
expect_equal(ndtv:::layout.zoomfactor(pos,2,v=2:3),matrix(c(-0.5, 1.5, 3.5, 5.5, 7.5, -0.5, 1.5, 3.5, 5.5, 7.5),ncol=2))

# ----- graphviz tests -----

# graphviz layout test
# set up a basic tree structure
tree<-network.initialize(21)
add.edges.active(tree,head=1,tail=2,onset=0,terminus=5)
add.edges.active(tree,head=c(2,2),tail=c(3,4),onset=1,terminus=5)
add.edges.active(tree,head=c(3,3,3,4,4),tail=5:9,onset=2,terminus=5)
add.edges.active(tree,head=c(6,6,8,9,9),tail=10:14,onset=3,terminus=5)
add.edges.active(tree,head=c(10,10,12,12,12,14,14),tail=15:21,onset=4,terminus=5)

# the tests below are contional on graphviz being installed on the system
if (ndtv:::check.graphviz()){
#plot.network(tree,coord=network.layout.animate.Graphviz(tree))
coords<-network.layout.animate.Graphviz(tree)
expect_equal(nrow(coords),21)
expect_equal(ncol(coords),2)

# check verbose
network.layout.animate.Graphviz(tree,verbose=FALSE)

# check using layout par to set differnet engines
network.layout.animate.Graphviz(tree,layout.par=list(gv.engine='dot'))

# check passing in gv.args
network.layout.animate.Graphviz(tree,layout.par=list(gv.engine='dot',gv.args='-Grankdir=LR'))

# check passing bad gv.args
expect_error(network.layout.animate.Graphviz(tree,layout.par=list(gv.args='-Xhelloworld'),verbose=FALSE),regexp = 'Unable to parse coordinates')

# test passing a weighted network into neato
wtest<-network.initialize(5)
add.edges.active(wtest,tail = 1,head=2,onset=0,terminus=10)
add.edges.active(wtest,tail = 2,head=3,onset=1,terminus=10)
add.edges.active(wtest,tail = 3,head=4,onset=2,terminus=10)
activate.edge.attribute(wtest,'len',0.5,onset=0,terminus=5,e = 1)
activate.edge.attribute(wtest,'len',2.5,onset=5,terminus=10,e=1)
activate.edge.attribute(wtest,'len',2,onset=1,terminus=5,e = 2)
activate.edge.attribute(wtest,'len',1,onset=5,terminus=10,e=2)
activate.edge.attribute(wtest,'len',5,onset=2,terminus=5,e = 3)
activate.edge.attribute(wtest,'len',0.1,onset=5,terminus=10,e=3)

# test rendering using the gv 'len' attrigute
compute.animation(wtest,animation.mode = 'Graphviz',layout.par=list(gv.len.mode='gv.edge.len',gv.edge.attrs='len'))
#render.animation(wtest,edge.lwd='len')

# test passing in matrix of distances
compute.animation(wtest,animation.mode = 'Graphviz',weight.attr='len',layout.par=list(gv.engine='neato',gv.len.mode='ndtv.distance.matrix'))

# test using default dist to generate a matrix
compute.animation(wtest,animation.mode = 'Graphviz',default.dist=5)

# test bad weight mode
expect_error(compute.animation(wtest,animation.mode = 'Graphviz',layout.par=list(gv.len.mode='bla')))

}  else {
# graphivz not installed, so check fallback to kk
warning("graphviz layout tests skipped because graphviz not installed on system")
expect_warning(coords<-network.layout.animate.Graphviz(tree),'KamadaKawai')
expect_equal(nrow(coords),21)
expect_equal(ncol(coords),2)

}

# ------------- MDSJ tests -------

# these need to be optionally enabled because they won't run if java and mdsj library not installed
if (!is.null(ndtv:::check.mdsj()) ){
  wtest<-network.initialize(5)
  add.edges.active(wtest,tail = 1,head=2,onset=0,terminus=10)
  add.edges.active(wtest,tail = 2,head=3,onset=1,terminus=10)
  add.edges.active(wtest,tail = 3,head=4,onset=2,terminus=10)
  activate.edge.attribute(wtest,'len',0.5,onset=0,terminus=5,e = 1)
  activate.edge.attribute(wtest,'len',2.5,onset=5,terminus=10,e=1)
  activate.edge.attribute(wtest,'len',2,onset=1,terminus=5,e = 2)
  activate.edge.attribute(wtest,'len',1,onset=5,terminus=10,e=2)
  activate.edge.attribute(wtest,'len',5,onset=2,terminus=5,e = 3)
  activate.edge.attribute(wtest,'len',0.1,onset=5,terminus=10,e=3)
  
  compute.animation(wtest,animation.mode = 'MDSJ',weight.attr='len')

 # dests for changing dimensions of MDSJ layout
test<-network.initialize(10)
dim2mat<-network.layout.animate.MDSJ(test,layout.par=list(dimensions=2))
expect_equal(dim(dim2mat),c(10,2))
dim1mat<-network.layout.animate.MDSJ(test,layout.par=list(dimensions=1))
expect_equal(dim(dim1mat),c(10,1))
}




