#  File R/animate.interpolation.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2016 Statnet Commons
#######################################################################
#functions for interpolating node positions 

# basic linear interpolation
coord.interp.linear <- function(coords1,coords2,step,num.steps){
  return (coords1 + ((coords2-coords1)*(step/num.steps)))
}

# a smooth near sine interpolation
coord.interp.smoothstep <- function(coords1,coords2,step,num.steps){
  t <-step/num.steps
  return (coords1 + ((coords2-coords1)*(t^2 * (3-2*t))))
}


#more info on other fun easing/tween equations for snapping and bouncing: http://code.google.com/p/tweener/source/browse/trunk/as3/caurina/transitions/Equations.as