#  part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2013 Statnet Commons
#######################################################################

#test drawing networks with weighted edges

require(ndtv)
test <- network.initialize(5,directed=F)
test[1,2]<-1
test[3,2]<-1
test[1,3]<-1
test[1,4]<-1
test[5,1]<-1
test[4,5]<-1
activate.edge.attribute(test,'weight',value=1,onset=0,terminus=5)
activate.edge.attribute(test,'weight',value=2,onset=1,terminus=2,e=2)
activate.edge.attribute(test,'weight',value=3,onset=2,terminus=3,e=2)
activate.edge.attribute(test,'weight',value=10,onset=3,terminus=4,e=2)
activate.edge.attribute(test,'weight',value=0.8,onset=1,terminus=2,e=5)
activate.edge.attribute(test,'weight',value=0.5,onset=2,terminus=3,e=5)
activate.edge.attribute(test,'weight',value=0.2,onset=3,terminus=4,e=5)


# test drawing them with different widths
slice.par<-list(start=0,end=3,interval=1, aggregate.dur=1,rule="latest")
test <-compute.animation(test,slice.par=slice.par,animation.mode="kamadakawai")
render.animation(test,render.par=list(tween.frames=10,show.time=T),edge.lwd='weight',edge.label='weight')

# test using default weights created by network.collapse
test <- network.initialize(5,directed=F)
test[1,2]<-1
test[3,2]<-1
test[1,3]<-1
test[1,4]<-1
test[5,1]<-1
test[4,5]<-1
activate.edges(test,onset=c(1,3,5),terminus=c(2,4,6),e=c(1,1,1))
activate.edges(test,onset=0,terminus=6,e=2:6)
slice.par<-list(start=-3,end=6,interval=1, aggregate.dur=3,rule="latest")
test <-compute.animation(test,slice.par=slice.par,animation.mode="MDSJ",weight.attr='activity.duration')
render.animation(test,edge.label='activity.duration')


#test a network with edge weights as distances
test <- network.initialize(5,directed=F)
test[1,2]<-1
test[3,2]<-1
test[1,3]<-1
test[1,4]<-1
test[5,1]<-1
test[4,5]<-1
test <-activate.edge.attribute(test,'weight',value=1,onset=0,terminus=1,e=c(1,2,3)) 
test <- activate.edge.attribute(test,'weight',value=3,onset=0,terminus=1,e=c(4,5,6))
test <- activate.edge.attribute(test,'weight',value=3,onset=1,terminus=2,e=c(1,2,3))
test <- activate.edge.attribute(test,'weight',value=1,onset=1,terminus=2,e=c(4,5,6))
slice.par<-list(start=-1,end=3,interval=1, aggregate.dur=1,rule="latest")
test <-compute.animation(test,slice.par=slice.par,animation.mode="MDSJ",weight.attr='weight',weight.dist=TRUE)
render.animation(test,edge.label='weight',render.par=list(tween.frames=10))

#test a network with edge weights as similarities
slice.par<-list(start=-1,end=3,interval=1, aggregate.dur=1,rule="latest")
test <-compute.animation(test,slice.par=slice.par,animation.mode="MDSJ",weight.attr='weight',weight.dist=FALSE)
render.animation(test,edge.label='weight',render.par=list(tween.frames=10))

#test symetrizing an asymetric network
test <- network.initialize(5,directed=T)
test[1,2]<-1
test[3,2]<-1
test[1,3]<-1
test[1,4]<-1
test[5,1]<-1
test[4,5]<-1
test[2,1]<-1
test[2,3]<-1
test[3,1]<-1
test[4,1]<-1
test[1,5]<-1
test[5,4]<-1
test <-activate.edge.attribute(test,'weight',value=1,onset=0,terminus=1,e=c(1,2,3)) 
test <- activate.edge.attribute(test,'weight',value=3,onset=0,terminus=1,e=c(4,5,6))
test <- activate.edge.attribute(test,'weight',value=3,onset=1,terminus=2,e=c(1,2,3))
test <- activate.edge.attribute(test,'weight',value=1,onset=1,terminus=2,e=c(4,5,6))
test <- activate.edge.attribute(test,'weight',value=5, onset=0,terminus=4,e=7:12)
slice.par<-list(start=-1,end=3,interval=1, aggregate.dur=1,rule="latest")
test <-compute.animation(test,slice.par=slice.par,animation.mode="MDSJ",weight.attr='weight',weight.dist=FALSE)
render.animation(test,edge.label='weight',render.par=list(tween.frames=20))
layout.distance(network.collapse(test,at=3),weight.attr='weight')


data(McFarland_cls33_10_16_96)
slice.par<-list(start=0,end=30,interval=1, aggregate.dur=2.5,rule="latest")
cls33_10_16_96 <-compute.animation(cls33_10_16_96,slice.par=slice.par,animation.mode="MDSJ",weight.attr='activity.count',default.dist=5)
#saveVideo(render.animation(cls33_10_16_96,edge.lwd='activity.count',edge.label='activity.count',edge.col='gray',render.cache='none'))


