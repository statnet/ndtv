#  File tests/render.animation_test.R in package ndtv, part of the Statnet suite
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
data(newcomb)
dynNew <- networkDynamic(network.list=newcomb[1:5])

#----- render animation ------
# does it crash on basic example
# shorten time range so wont dake as long
dyn<-network.extract(dynNew,onset=0,terminus=3,trim.spells=TRUE) 
render.animation(dyn,verbose=FALSE)

# does replay work 
ani.replay()

# can it run and record plots without creating alist of animations
saveVideo(render.animation(dyn,verbose=FALSE,render.cache='none'))


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
compute.animation(test, animation.mode='useAttribute',layout.par = list(x = "x", y = "y"),slice.par=list(start=0,end=1,interval=1,aggregate.dur=0,rule='latest'))
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