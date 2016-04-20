#  File R/export.matrix.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2016 Statnet Commons
#######################################################################

# this function was copied directly from R package MASS version 7.3-16 (GPL 3).  Included here to remove the dependency on MASS
# Venables, W. N. and Ripley, B. D. (2002) _Modern Applied Statistics with S._ Fourth edition.  Springer.

# export a matrix as a (normally) space delimted text file
write.matrix <- function (x, file = "", sep = " ", blocksize) {
  x <- as.matrix(x)
  p <- ncol(x)
  cn <- colnames(x)
  if (!missing(blocksize) && blocksize > 0L) {
    cat(cn, file = file, sep = c(rep(sep, p - 1L), "\n"))
    nlines <- 0
    nr <- nrow(x)
    while (nlines < nr) {
      nb <- min(blocksize, nr - nlines)
      cat(format(t(x[nlines + (1L:nb), ])), file = file, 
          append = TRUE, sep = c(rep(sep, p - 1L), "\n"))
      nlines <- nlines + nb
    }
  }
  else cat(c(cn, format(t(x))), file = file, sep = c(rep(sep,  p - 1L), "\n"))
}
