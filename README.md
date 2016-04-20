# ndtv: Network Dynamic Temporal Visualizations
an R package to render dynamic network data from 'networkDynamic' objects as movies, interactive animations, or other representations of changing relational structures and attributes. 

by Skye Bender-deMoll, Martina Morris, and the http://statnet.org team.  This work was supported by grant R01HD68395 from the National Institute
of Health

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

The package vignette is at: https://cran.r-project.org/web/packages/ndtv/vignettes/ndtv.pdf

A longer tutorial with more background is at: http://statnet.csde.washington.edu/workshops/SUNBELT/current/ndtv/ndtv_workshop.html

## Citation and License

This software is distributed under the GPL-3 license.  It is free, open source, and has the attribution requirements (GPL Section 7) at
http://statnet.org/attribution

To cite package ‘ndtv’ in publications use:

  Skye Bender-deMoll (2016). ndtv: Network Dynamic Temporal Visualizations. R package version
  0.10. http://statnet.org

