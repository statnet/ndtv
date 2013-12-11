# some tests tfor the proximity.timeline function
require(ndtv)

test<-network.initialize(10)
add.edges.active(test,head=1:9,tail=2:10,onset=0,terminus=10)
add.edges.active(test,head=1,tail=3,onset=1,terminus=2)
proximity.timeline(test,start=0,end=4)

data(McFarland_cls33_10_16_96)
proximity.timeline(cls33_10_16_96,onsets=seq(0,45,0.5),termini=seq(2.5,47.5,0.5),vertex.cex=(cls33_10_16_96%v%'type'=='instructor')*4+1,labels.at=85)


#data(msm.sim)
#proximity.timeline(msm.sim,start=0,end=10,vertex.col=ifelse(msm.sim%v%'race'==1,rgb(0,.5,0,0.2),rgb(0,0,.5,0.2)))

# sampson monestary
# require(ergm)
#data(samplk)
#sampdyn<-networkDynamic(network.list=list(samplk1,samplk2,samplk3))
#proximity.timeline(sampdyn,labels.at=1,vertex.col='group')

# windsurfers
data(windsurfers)
proximity.timeline(windsurfers,start=20,end=31,vertex.col=ifelse(windsurfers%v%'group1','#000055ff','#55555555'),grid=F,splines=.5)

#data(newcomb)
#newDyn<-networkDynamic(network.list=newcomb,onsets=c(0:7,9:14),termini=c(1:8,10:15))
