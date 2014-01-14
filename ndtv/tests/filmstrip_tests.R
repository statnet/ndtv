#  part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2013 Statnet Commons
#######################################################################

require(ndtv)
require(testthat)

# ------- tests for filmstrip funciton -----
data(stergm.sim.1)

#try a default
filmstrip(stergm.sim.1)

# check that par was reset back to original
expect_equal(par()$mfcol,c(1,1))

# try setting mfcol and exporting a wide version to a pdf
pdf('filmstripTest.pdf',width=7,height=2)
filmstrip(stergm.sim.1,frames=5,mfrow=c(1,5),displaylabels=FALSE)
dev.off()


# try pre-computed coords

data(stergm.sim.1)
compute.animation(stergm.sim.1,slice.par=list(start=5,end=15,interval=1, aggregate.dur=1,rule='latest'))
filmstrip(stergm.sim.1,frames=4)

# try overriding pre-computed
compute.animation(stergm.sim.1,slice.par=list(start=5,end=15,interval=5, aggregate.dur=1,rule='latest'))
