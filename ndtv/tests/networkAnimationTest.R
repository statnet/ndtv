#  File tests/networkAnimationTest.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2003-2012 Statnet Commons
#######################################################################
#test some functions for calculating animated layouts and exporting movies.
require(ndtv)
require(testthat)

# hard to write tests since most require a human looking at the output
# most overall tests are included as man examples or in the packae vignette
# this file should be mostly unit tests for functionality


# ----- compute.animation ------

# does it crash on basic example
data(newcomb)
dynNew <- networkDynamic(network.list=newcomb)
dyn<-dynNew
slice.par<-list(start=0,end=5,interval=1, aggregate.dur=1,rule=NULL)
set.seed(123)
compute.animation(dyn,slice.par=slice.par,verbose=FALSE)


# modify network argument in place?
if(!all(c("animation.x.active","animation.y.active")%in%list.vertex.attributes(dyn))){
  stop("compute.animation appears to have not correctly modified its argument in place")
}

# does it store slice.par
par<-get.network.attribute(dyn,'slice.par')
if (!all(names(par)==c("start","end","interval","aggregate.dur","rule" ))){
  stop("compute.animation did not store slice parameters correctly in slice.par")
}

# does it store animation.x and animation.y
x<-get.vertex.attribute(dyn,"animation.x.active")
y<-get.vertex.attribute(dyn,"animation.y.active")
if(any(is.na(x)) | any(is.na(y))){
  stop('compute.animation did not store animation.x and animation.y vertex attributes')
}

# does it give same results from same seed
x1 <-get.vertex.attribute.active(dyn,"animation.x",at=1)
set.seed(123)
dyn <-compute.animation(dyn,slice.par=slice.par,verbose=FALSE)
x2 <-get.vertex.attribute.active(dyn,"animation.x",at=1)
if (!all(x1==x2)){
  stop("compute.animation did not generate the same results fromt he same seed")
}

# does aggregate.dur change results
set.seed(123)
slice.par<-list(start=0,end=2,interval=1, aggregate.dur=1,rule="any")
dyn <-compute.animation(dyn,slice.par=slice.par,verbose=FALSE)
x1 <-get.vertex.attribute.active(dyn,"animation.x",onset=1,terminus=2)
slice.par<-list(start=0,end=2,interval=1, aggregate.dur=2,rule="any")
set.seed(123)
dyn <-compute.animation(dyn,slice.par=slice.par,verbose=FALSE)
x2 <-get.vertex.attribute.active(dyn,"animation.x",onset=1,terminus=2)
if (any(x1==x2)){
  stop("compute.animation did not give different results with a different aggreagate.dur")
}

# does aggregation rule change results
set.seed(123)
slice.par<-list(start=0,end=2,interval=1, aggregate.dur=2,rule="any")
dyn <-compute.animation(dyn,slice.par=slice.par,verbose=FALSE)
x1 <-get.vertex.attribute.active(dyn,"animation.x",at=1)
slice.par<-list(start=0,end=2,interval=1, aggregate.dur=2,rule="all")
set.seed(123)
dyn <-compute.animation(dyn,slice.par=slice.par,verbose=FALSE)
x2 <-get.vertex.attribute.active(dyn,"animation.x",at=1)
if (any(x1==x2)){
  stop("compute.animation did not give different results with a different aggregation rule")
}

# does default.dist change results
data(stergm.sim.1)
set.seed(123)
slice.par<-list(start=0,end=2,interval=1, aggregate.dur=1)
stergm.sim.1 <-compute.animation(stergm.sim.1,slice.par=slice.par,verbose=FALSE)
x1 <-get.vertex.attribute.active(stergm.sim.1,"animation.x",at=1)
set.seed(123)
stergm.sim.1 <-compute.animation(stergm.sim.1,slice.par=slice.par,default.dist=100,verbose=FALSE)
x2 <-get.vertex.attribute.active(stergm.sim.1,"animation.x",at=1)
if (any(x1==x2)){
  stop("compute.animation did not give different results with a different default.dist")
}


# check with incomplete slice.par
expect_error(compute.animation(stergm.sim.1,slice.par=list()),"the 'slice.par' argument to compute.animation must include")
expect_error(compute.animation(stergm.sim.1,slice.par=list(start=1)),"the 'slice.par' argument to compute.animation must include")
expect_error(compute.animation(stergm.sim.1,slice.par=list(start=1,end=5,interval=1)),"the 'slice.par' argument to compute.animation must include")

#----- render animation ------
# does it crash on basic example
# shorten time range so wont dake as long
dyn<-network.extract(dynNew,onset=0,terminus=3,trim.spells=TRUE)
render.animation(dyn,verbose=FALSE)

# does replay work 
ani.replay()

# does increasing tween produce different results

# does show.time crash if changed from default
render.par<-list(tween.frames=2,show.time=FALSE)
render.animation(dyn,render.par=render.par,verbose=FALSE)

# does show.stats crashed if changed from default
render.par<-list(tween.frames=2,show.time=TRUE,show.stats=FALSE)
render.animation(dyn,render.par=render.par,verbose=FALSE)

# can it render arbitrary plotting comands?
render.par=list(tween.frames=2,show.time=TRUE,show.stats=NULL,extraPlotCmds=expression(text(0,0,"SOME TEXT ON THE PLOT",col='blue')))
render.animation(dyn,render.par=render.par,verbose=FALSE)

# test workaround for 0-coord label bug #322
test<-network.initialize(2)
activate.vertex.attribute(test,'x',0,onset=0,terminus=2)
activate.vertex.attribute(test,'y',0,onset=0,terminus=2)
compute.animation(test, animation.mode='useAttribute',layout.par = list(x = "x", y = "y"),slice.par=list(start=0,end=1,interval=1,aggregate.dur=0))
render.animation(test,displaylabels=TRUE)

# try labels moving on edges, but only 5 edges
set.edge.attribute(dyn,"eLabel",1:network.edgecount(dyn))
render.par=list(tween.frames=10,show.time=TRUE,show.stats=NULL)
render.animation(dyn,render.par=render.par,verbose=FALSE,edge.col='gray',edge.label='eLabel',edge.label.cex=0.7,edge.label.col='blue')

# single vertex matrix collapse bug #332
compute.animation(as.networkDynamic(network.initialize(1)))

# test animation of single vertex
test<-network.initialize(3)
deactivate.vertices(test)
activate.vertices(test,v=2,onset=0,terminus=3)
compute.animation(test)
render.animation(test)

# test specifying xlim and ylim
render.animation(test,xlim=c(-1,1),ylim=c(-1,1))

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



# ----- kk layout ------

# ----- mdsj layout -----
# ndtv:::install.mdsj()





