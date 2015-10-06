library(ndtv)
data("short.stergm.sim")
data("toy_epi_sim")
data("windsurfers")
require(testthat)

compute.animation(short.stergm.sim,slice.par = list(start=0,end=25,interval=1, aggregate.dur=5,rule='latest'))
render.animation(short.stergm.sim,edge.col=effectFun('edgeAgeColor',fade.dur=5,start.color = 'red',end.color='blue'))

# test function wrapper
effFun<-effectFun('edgeAgeColor',fade.dur=5)
expect_true(is.function(effFun))
# check argument substitution
expect_equal(names(formals(effFun)),c("net", "onset", "fade.dur", "start.color", "end.color", "na.color" ))

# check for warning for non matching arg
expect_warning(effectFun('edgeAgeColor',fade.dur=5, foo=6),regexp = 'does not match with any arguments')

# test for color interpolation
test<-network.initialize(2)
add.edges.active(test,tail=1,head=2,onset=0,terminus=10)
expect_equal(effect.edgeAgeColor(test,start.color = '#00000000',end.color = "#FFFFFFFF",fade.dur=10,onset=5),"#80808080")

# visual test of edge age coloring
plot(short.stergm.sim,edge.col=effect.edgeAgeColor(short.stergm.sim,
                                                   fade.dur=25,
                                                   start.color = 'red',
                                                   end.color='blue', 
                                                   onset=24),
     edge.label=edges.age.at(short.stergm.sim,24),
     edge.lwd=5)

# visual test of vertex age coloring
plot(windsurfers,vertex.col=effect.vertexAgeColor(windsurfers,
                                                   fade.dur=5,
                                                   start.color = 'red',
                                                   end.color='blue', 
                                                   onset=11),
     label=vertices.age.at(windsurfers,20),
     vertex.cex=2)
