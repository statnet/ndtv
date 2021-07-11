# ndtv: Network Dynamic Temporal Visualizations


<img align="right" src="https://statnet.org/nme/movie.gif"> 
The R package ndtv renders dynamic network data from [networkDynamic objects](https://cran.r-project.org/web/packages/networkDynamic/index.html networkDynamic) as movies, interactive HTML5 animations, or other static representations of changing relational structures and attributes. 

by Skye Bender-deMoll, Martina Morris, and the http://statnet.org team.  This work was supported by grant R01HD68395 from the National Institute of Health

<img src='https://travis-ci.org/statnet/ndtv.svg?branch=master'> <img src='http://cranlogs.r-pkg.org/badges/ndtv'>

## Install

Released versions of the package are on CRAN  at https://cran.r-project.org/web/packages/ndtv/index.html

install in R with 
```
install.packages('ndtv')
```

A network movie example

```
library(ndtv) # load the package
data(short.stergm.sim)  # load an example dynamic network dataset
render.d3movie(short.stergm.sim) # create a HTML5 animation 
```



## Docs and Examples

The package vignette is at: https://github.com/statnet/ndtv/blob/master/ndtv/vignettes/ndtv.pdf

A longer tutorial with more background is at: http://statnet.org/Workshops/ndtv_workshop.html

## Citation and License

This software is distributed under the GPL-3 license.  It is free, open source, and has the attribution requirements (GPL Section 7) at
http://statnet.org/attribution

To cite package ‘ndtv’ in publications use:

  Skye Bender-deMoll (2016). ndtv: Network Dynamic Temporal Visualizations. R package version
  0.10. http://statnet.org

