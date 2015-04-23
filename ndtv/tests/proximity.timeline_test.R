# some tests tfor the proximity.timeline function
require(ndtv)
require(testthat)

test<-network.initialize(10)
add.edges.active(test,head=1:9,tail=2:10,onset=0,terminus=10)
add.edges.active(test,head=1,tail=3,onset=1,terminus=2)
proximity.timeline(test)

data(McFarland_cls33_10_16_96)
proximity.timeline(cls33_10_16_96,onsets=seq(0,45,5),termini=seq(2.5,47.5,5),vertex.cex=(cls33_10_16_96%v%'type'=='instructor')*4+1,labels.at=45)

# test algorithm types
# isoMDS is default, so already tested
proximity.timeline(cls33_10_16_96,onsets=seq(0,45,5),termini=seq(2.5,47.5,5),mode='sammon')
proximity.timeline(cls33_10_16_96,onsets=seq(0,45,5),termini=seq(2.5,47.5,5),mode='cmdscale')
proximity.timeline(cls33_10_16_96,onsets=seq(0,45,5),termini=seq(2.5,47.5,5),mode='gvNeato')
# only check MDSJ if allready installed
has.mdsj<-ndtv:::check.mdsj(ask=FALSE)
if(!is.null(has.mdsj)){
proximity.timeline(cls33_10_16_96,onsets=seq(0,45,5),termini=seq(2.5,47.5,5),mode='MDSJ')
}

# test grid option and verbose options
proximity.timeline(cls33_10_16_96,onsets=seq(0,10,5),termini=seq(2.5,12.5,5),grid=FALSE,verbose=FALSE)

# this is too slow for automated test
#data(msm.sim)
#proximity.timeline(msm.sim,start=0,end=10,mode='sammon',vertex.col=ifelse(msm.sim%v%'race'==1,rgb(0,.5,0,0.2),rgb(0,0,.5,0.2)))

# sampson monestary
#require(ergm)
#data(samplk)
#sampdyn<-networkDynamic(network.list=list(samplk1,samplk2,samplk3))
#proximity.timeline(sampdyn,labels.at=2.5,vertex.col='group',mode='sammon',default.dist=1)

# tests for chunks missing data
# windsurfers  
data(windsurfers)
proximity.timeline(windsurfers,start=20,end=31,vertex.col=ifelse(windsurfers%v%'group1','#000055ff','#55555555'),grid=F,splines=.5,mode='sammon')

# check the various spline styles # spline.style=c('default','inactive.ghost','inactive.gaps','inactive.ignore','color.attribute')
proximity.timeline(windsurfers,start=20,end=31,vertex.col=ifelse(windsurfers%v%'group1','#000055ff','#55555555'),grid=F,splines=.5,mode='sammon',spline.style='inactive.ghost')
proximity.timeline(windsurfers,start=20,end=31,vertex.col=ifelse(windsurfers%v%'group1','#000055ff','#55555555'),grid=F,splines=.5,mode='sammon',spline.style='inactive.gaps')
proximity.timeline(windsurfers,start=20,end=31,vertex.col=ifelse(windsurfers%v%'group1','#000055ff','#55555555'),grid=F,splines=.5,mode='sammon',spline.style='inactive.ignore')
proximity.timeline(windsurfers,start=20,end=31,vertex.col=ifelse(windsurfers%v%'group1','#000055ff','#55555555'),grid=F,splines=.5,mode='sammon',spline.style='color.attribute')

expect_error(proximity.timeline(windsurfers,start=20,end=31,draw.inactive='default'),regexp ='argument has been deprecated')




#data(newcomb)
#newDyn<-networkDynamic(network.list=newcomb,onsets=c(0:7,9:14),termini=c(1:8,10:15))

# test changing color attributes
test<-network.initialize(10,directed=FALSE)
activate.vertex.attribute(test,'color','gray',onset=-1,terminus=100)
add.edges.active(test,tail=c(9,7,3),head=c(7,3,9),onset=0,terminus=6)
activate.vertex.attribute(test,'color','red',onset=1,terminus=100,v=5)
add.edges.active(test,tail=5,head=6,onset=2,terminus=5)
activate.vertex.attribute(test,'color','red',onset=3,terminus=100,v=6)
add.edges.active(test,tail=6,head=1,onset=4,terminus=100)
activate.vertex.attribute(test,'color','red',onset=5,terminus=100,v=1)

# test tea color attribute
proximity.timeline(test,vertex.col='color',start=0,end=10,mode='sammon',spline.style='color.attribute',default.dist=20,labels.at=10)

# test tea color but not using color.attribute splines
expect_error(proximity.timeline(test,vertex.col='color',start=0,end=10,default.dist=20,labels.at=10),regexp = "can only be used with spline.style='color.attribute'")

# test the static color attributes
proximity.timeline(test,vertex.col=1:10,start=0,end=10,mode='sammon',default.dist=20,labels.at=10) # should be a sort of rainbow

proximity.timeline(test,vertex.col='blue',start=0,end=10,mode='sammon',default.dist=20,labels.at=10) #all should be blue

# test functional color attribute
expect_error(proximity.timeline(test,vertex.col=function(net){ifelse(net%v%'color'=='red','yellow','green')},start=0,end=10,mode='sammon',default.dist=20,labels.at=10))

proximity.timeline(test,vertex.col=function(slice){ifelse(slice%v%'color'=='red','yellow','green')},start=0,end=10,mode='sammon',default.dist=20,labels.at=10,spline.style='color.attribute')

# test chain directions
proximity.timeline(test,start=0,end=10,default.dist=20,labels.at=10,chain.direction='forward')
proximity.timeline(test,start=0,end=10,default.dist=20,labels.at=10,chain.direction='reverse')

# test labels
# pass in vector of label values
proximity.timeline(test,start=0,end=10,labels.at=1,label=LETTERS[1:10])
test%v%'mylabel'<-LETTERS[10:20]
proximity.timeline(test,start=0,end=10,labels.at=1,label='mylabel')
# test showing labels at multiple times
proximity.timeline(test,start=0,end=10,labels.at=c(1,5,10),label='mylabel')

# test vertex.cex
proximity.timeline(test,start=0,end=10,vertex.cex=10)
proximity.timeline(test,start=0,end=10,vertex.cex=1:10)
test%v%'myVal'<-1:10
proximity.timeline(test,start=0,end=10,vertex.cex='myVal')






