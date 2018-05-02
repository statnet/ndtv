#  File tests/d3_animation_tests.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2014 Statnet Commons
#######################################################################

# tests for d3 animation functionality
library(ndtv)
require(testthat)

# flip this to true if you actually want it to open a gajillion browser windows
showInBrowser=FALSE

# create really trivial network for testing
test<-network.initialize(3)
activate.vertices(test,v=1,onset=1,terminus=3)
activate.vertices(test,v=3:3,onset=0,terminus=5)
add.edges.active(test,tail=2,head=3,onset=1,terminus=5)

# test output formats
render.d3movie(test,filename=tempfile(fileext = '.html'),launchBrowser = showInBrowser)
render.d3movie(test,filename=tempfile(fileext = '.json'),output.mode='JSON')

# test browser launch
render.d3movie(test,filename=tempfile(fileext = '.json'),output.mode='JSON',launchBrowser = showInBrowser)

# test xlab
render.d3movie(test,filename=tempfile(fileext = '.html'),xlab='hello, xlabel',launchBrowser=showInBrowser)

# test main
render.d3movie(test,filename=tempfile(fileext = '.html'),main='hello, main',launchBrowser=showInBrowser)

# test displaylabels
render.d3movie(test,filename=tempfile(fileext = '.html'),displaylabels=FALSE,launchBrowser=showInBrowser)

# test use arrows
render.d3movie(test,filename=tempfile(fileext = '.html'),usearrows=FALSE,launchBrowser=showInBrowser)

# test bg
render.d3movie(test,filename=tempfile(fileext = '.html'),bg='red',launchBrowser=showInBrowser)

# test vertex.cex
render.d3movie(test,filename=tempfile(fileext = '.html'),vertex.cex=5,launchBrowser=showInBrowser)

# test vertex label
render.d3movie(test,filename=tempfile(fileext = '.html'),label="we all have the same label",launchBrowser=showInBrowser)

# test label cex
render.d3movie(test,filename=tempfile(fileext = '.html'),label.cex=c(0.5,2,4),launchBrowser=showInBrowser,displaylabels=TRUE)

# test vertex label color
render.d3movie(test,filename=tempfile(fileext = '.html'),label.col='red',launchBrowser=showInBrowser,displaylabels=TRUE)

# test vertex color
render.d3movie(test,filename=tempfile(fileext = 'html'),vertex.col='blue',launchBrowser=showInBrowser)

# test vertex sides
render.d3movie(test,filename=tempfile(fileext = 'html'),vertex.sides=3,launchBrowser=showInBrowser)

# vertex rotation
render.d3movie(test,filename=tempfile(fileext = 'html'),vertex.sides=3,vertex.rot=c(0,90,180),launchBrowser=showInBrowser)

# vertex border color
render.d3movie(test,filename=tempfile(fileext = 'html'),vertex.border='green',launchBrowser=showInBrowser)

# vertex border width
render.d3movie(test,filename=tempfile(fileext = 'html'),vertex.lwd=5,launchBrowser=showInBrowser)

# test edge color
render.d3movie(test,filename=tempfile(fileext = 'html'),edge.col='blue',launchBrowser=showInBrowser)

# test edge width
render.d3movie(test,filename=tempfile(fileext = 'html'),edge.lwd=10,launchBrowser=showInBrowser)

# test transparent edge color
render.d3movie(test,filename=tempfile(fileext = 'html'),edge.col='#CCCCCC22',edge.lwd=10,launchBrowser=showInBrowser)


# test static network
testStatic<-network.initialize(4)
testStatic[1,2:3]<-1
render.d3movie(testStatic)

# test passing in coord to static network
testStatic<-network.initialize(4)
testStatic[1,2:3]<-1
render.d3movie(testStatic,coord=matrix(1:8,ncol=2))

# test rendering network of size zero (issue #24)
test<-network.initialize(0)
activate.vertices(test)
activate.vertex.attribute(test,'foo',1,onset=0,terminus=1)
render.d3movie(test,vertex.cex='foo')

# test rendering of edge attribute for network with edge not active # 25
test<-network.initialize(2)
add.edges.active(test,1,2,onset=0,terminus=2)
activate.edge.attribute(test,'weight',1, onset=0,terminus=2)
compute.animation(test,slice.par = list(start=3,end=4,interval=1,aggregate.dur=1,rule='earliest'))
render.d3movie(test,edge.lwd='weight')

# test rendering of edge attribute for network with attribute not active
# (should give error because do 'default' attribute is defined)
test<-network.initialize(2)
add.edges.active(test,1,2,onset=0,terminus=2)
activate.edge.attribute(test,'weight',1, onset=1,terminus=2)
expect_error( render.d3movie(test,edge.lwd='weight'),regexp = 'had illegal missing values for')

#errors on empty slice with vertex.cex specified
#https://github.com/statnet/ndtv/issues/24
test<-network.initialize(2)
add.edges.active(test,1,2,onset=0,terminus=1)
activate.edge.attribute(test,'edge.col','blue',onset = 0,terminus=1)
render.d3movie(test,edge.col='edge.col')



