# tests for transmission timeline
require(testthat)
require(ndtv)
#require(tsna)
#data(moodyContactSim)
#v10path<-tPath(moodyContactSim,v=10,start=0)
#v11path<-tPath(moodyContactSim,v=11,start=0)
#v12path<-tPath(moodyContactSim,v=12,start=0)

# creating v10 path via structure for now so don't bring in tsna dep
v10path<-structure(list(tdist = c(583, 494, 634, 40, 712, 701, 224, 719,  674, 0, 749, 621, 453, 665, 709, 575), previous = c(16, 13, 13,  10, 13, 16, 10, 13, 1, 0, 8, 1, 4, 4, 2, 2), gsteps = c(5, 3,  3, 1, 3, 5, 1, 3, 6, 0, 4, 6, 2, 2, 4, 4), start = 0, end = Inf,      direction = "fwd", type = "earliest.arrive"), .Names = c("tdist",  "previous", "gsteps", "start", "end", "direction", "type"), class = c("tPath",  "list"))

transmissionTimeline(v10path)
transmissionTimeline(v10path,displaylabels = TRUE)
transmissionTimeline(v10path,displaylabels = TRUE,label='a')
transmissionTimeline(v10path,displaylabels = TRUE,label.col='blue')
transmissionTimeline(v10path,displaylabels = TRUE,label.cex=2)
transmissionTimeline(v10path,vertex.col='blue')
transmissionTimeline(v10path,vertex.border='blue')
transmissionTimeline(v10path,vertex.lwd=1:3)
transmissionTimeline(v10path,vertex.sides = c(3,4,50))
transmissionTimeline(v10path,edge.col='blue')
transmissionTimeline(v10path,edge.lwd=5)
transmissionTimeline(v10path,edge.lty=2)
transmissionTimeline(v10path,xlab='hello',ylab='world',main='diffusion tree timeline plot')




data("toy_epi_sim")

expect_error(transmissionTimeline(toy_epi_sim,time.attr='foo'),regexp = 'is not a vertex attribute of the network')
# check for appropriate error if input not a tree
expect_error(transmissionTimeline(toy_epi_sim,time.attr='testatus.active'),regexp = 'network does not appear to be a tree')

# test for case where no transmissions occur and jitter is active
toy_v50path<-structure(list(tdist = c(Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf,  Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf,  Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf,  Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf,  Inf, Inf, 0, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf,  Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf,  Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf,  Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf,  Inf), previous = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0), gsteps = c(Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf,  Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf,  Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf,  Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf,  Inf, 0, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf,  Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf,  Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf,  Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf ), start = 1, end = Inf, direction = "fwd", type = "earliest.arrive"), .Names = c("tdist",  "previous", "gsteps", "start", "end", "direction", "type"), class = c("tPath",  "list"))
transmissionTimeline(toy_v50path,jitter=TRUE)
