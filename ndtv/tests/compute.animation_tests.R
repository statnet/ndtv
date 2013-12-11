#  File tests/compute.animation_Tests.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2003-2013 Statnet Commons
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
dynNew <- networkDynamic(network.list=newcomb[1:5])
dyn<-dynNew
slice.par<-list(start=0,end=5,interval=1, aggregate.dur=1,rule='any')
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
slice.par<-list(start=0,end=2,interval=1, aggregate.dur=1,rule='latest')
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


# does reverse chaining option crash things
dyn<-dynNew
slice.par<-list(start=0,end=2,interval=1, aggregate.dur=1,rule='latest')
compute.animation(dyn,slice.par=slice.par,verbose=FALSE,chain.direction='reverse')
expect_error(compute.animation(dyn,slice.par=slice.par,verbose=FALSE,chain.direction='sideways'), "'arg' should be one of")
