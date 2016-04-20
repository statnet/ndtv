#  Part of the statnet package, http://statnetproject.org
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) in
#    http://statnetproject.org/attribution
#
#  Copyright 2012-2016 the statnet development team
######################################################################
.onAttach <- function(lib, pkg){
  sm <- statnetStartupMessage("ndtv",c("statnet"),FALSE)
  if(!is.null(sm)) packageStartupMessage(sm)
}

