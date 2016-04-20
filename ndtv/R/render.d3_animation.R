#  File R/render.d3_animation.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2014-2016 Statnet Commons
#######################################################################

## set ndtv-d3 version number as an option for debugging
options('ndtv-d3-version'="ndtv_0.7_release")  # git describe --tags

#go through the sets of coordinates attached to the network
#compute interpolation frames, and actually draw it out
#optionally save it directly to a file
render.d3movie <- function(net, filename=tempfile(fileext = '.html'), 
                           render.par=list(tween.frames=10,
                                           show.time=TRUE,
                                           show.stats=NULL,
                                           extraPlotCmds=NULL,
                                           initial.coords=0),
                           plot.par=list(bg='white'),
                           d3.options, 
                           output.mode=c('HTML','JSON','inline','htmlWidget'),
                           script.type=c('embedded','remoteSrc'),
                           launchBrowser=TRUE,
                           verbose=TRUE,...){
  if (!is.network(net)){
    stop("render.d3movie requires the first argument to be a network object")
  }
  
  # give error if network is hypergraphic
  if (is.hyper(net)){
    stop('render.d3movie does not yet support hypergraphic networks')
  }
  
  
  # check output modes
  output.mode<-match.arg(output.mode)
  script.type=match.arg(script.type)
  
  # if outputmode is inline, need to disable verbose
  if (output.mode=='inline'){
    verbose<-FALSE
  }
  
  # check render.par params
  if (is.null(render.par)){
    stop("render.d3movie is missing the 'render.par' argument (a list of rendering parameters).")
  }
  if (is.null(render.par[['tween.frames']])){
    render.par[['tween.frames']]<-10 
  }
  if (is.null(render.par[['show.time']])){
    render.par[['show.time']]<-TRUE
  }
  if (is.null(render.par[['initial.coords']])){
    render.par[['initial.coords']]<-matrix(0,ncol=2,nrow=network.size(net))
  }
  
  if (missing(d3.options)){
    d3.options<-list()
  }
  
  # if it is a non-dynamic network, disable the slider and play controls by default
  # convert to network dynamic before calling compute animation
  if(!is.networkDynamic(net)){
    message('input network is not networkDynamic object and does not have temporal info so output animation controls disabled by default')
    if(is.null(d3.options[['slider']])){
      d3.options[['slider']]<-FALSE
    }
    if(is.null(d3.options[['playControls']])){
      d3.options[['playControls']]<-FALSE
    }
    # turn off the time plot
    render.par[['show.time']]<-FALSE
    net<-as.networkDynamic(net)
    # set a dummy slice.par to avoid warning
    net%n%'slice.par'<-list(start=0,end=0,interval=0, aggregate.dur=0,rule='latest')
    # if 'coord' plotting argument is included, use it instead of doing a layout computation
    if (!is.null(list(...)[['coord']])){
      coord<-list(...)[['coord']]
      activate.vertex.attribute(net,'animation.x',coord[,1,drop=FALSE],at=0)
      activate.vertex.attribute(net,'animation.y',coord[,2,drop=FALSE],at=0)
    }
    
    
  }
  
  # set the ndtv version.  This is not read by javascript, but is included to make the
  # json library convert it correctly
  d3.options[['ndtv.version']]<-paste(packageDescription("ndtv")[['Version']],packageDescription("ndtv")[['Date']],sep=',')
  
  
  # check d3 params
  knownD3Params<-c(
    'ndtv.version',
    'animationDuration',
    'enterExitAnimationFactor',
    'labelOffset',
    'nodeSizeFactor',
    'dataChooser',
    'dataChooserDir',
    'playControls',
    'slider',
    'animateOnLoad',
    'margin',
    'debugFrameInfo',        
    'durationControl')
  if (!all(names(d3.options)%in%knownD3Params)){
    warning('unknown element(s) in d3.options argument: ',paste(names(d3.options)[!names(d3.options)%in%knownD3Params],collapse=', '))
  }
  
  #check if coordinates have already been computed
  if (!all(c("animation.x.active","animation.y.active") %in% list.vertex.attributes(net))){
    net <- compute.animation(net,verbose=verbose)
  }
  
  # only do device stuff checking if not on null device
  # because a call to par() create a device and break things on servers that
  # don't have a device (Shiny)
  # make sure background color is not transparent unless set that way explicitly
  # and copy the devices background color if not set by user
  
  if (is.null(plot.par[['bg']])){
    if (dev.cur()!=1){
      if(par("bg")=="transparent"){
        plot.par[['bg']]<-'white'
      } else {
        plot.par[['bg']]<-par("bg")
      }
    } else {
      plot.par[['bg']]<-'white'
    }
  } 
  
  #figure out what the slicing parameters were
  slice.par <- get.network.attribute(net,"slice.par")
  if (is.null(slice.par)){
    stop("render.d3movie can not locate the 'slice.par' list of parameters in the input network object")
  } 
  
  # cache plotting arguments 
  plot_params<-list(...)
  # also append any args included via plot.par
  plot_params<-c(plot_params,plot.par)
  
  # check network plot commands to give warnings about missing
  nonSupportedPlotArgs=c(
  'attrname',  
  'thresh',
  'displayisolates',
  'interactive',
  'ylab',
  'pad',
  'label.pad',
  'boxed.labels',
  'label.pos',
  'label.bg',
  'arrowhead.cex',
  'loop.cex',
  'label.border',
  'edge.lty',
  'label.lty',
  'vertex.lty',
  'edge.label',
  'edge.label.cex',
  'edge.label.col',                               
  'label.lwd',
  'edge.len',
  'edge.curve',
  'edge.steps',
  'loop.steps',
  'uselen',
  'usecurve',
  'suppress.axes',
  'vertices.last',
  'new' )
  if (any(names(plot_params)%in%nonSupportedPlotArgs)){
    warning('plot.network arguments(s) not yet supported by the ndtv-d3 player: ',paste(names(plot_params)[names(plot_params)%in%nonSupportedPlotArgs],collapse=', '))
  }
  
  # define some defaults for ploting args
  # label defaults to vertex names
  if(is.null(plot_params[['label']])){
    plot_params[['label']]<-function(slice){network.vertex.names(slice)}
  }
  # xlab defaults to time
  if(is.null(plot_params[['xlab']]) & render.par[['show.time']]){
    plot_params[['xlab']] <- function(onset,terminus){ifelse(onset==terminus,paste("t=",onset,sep=''),paste("t=",onset,"-",terminus,sep=''))}
  }
  # but if show stats, use that instead 
  # TODO: deprecate show.stats in favor of passing in directly for evaluation?
  if(!is.null(render.par[['show.stats']]) && render.par[['show.stats']]!=FALSE){
    # evaluate a eqn string giving the stats formual
    # TODO: this requires that tergm be loaded! give informative warning if not
    if(render.par[['show.time']]){
      # include the time string in the summary
      plot_params[['xlab']] <- eval(parse(text=paste("function(slice,onset,terminus){stats<-summary.statistics.network(slice",render.par[['show.stats']],")\n return(paste('t=',onset,'-',terminus,' ',paste(rbind(names(stats),stats),collapse=':'),sep='')) }",sep='')))
    } else {
      plot_params[['xlab']] <- eval(parse(text=paste("function(slice){stats<-summary.statistics.network(slice",render.par[['show.stats']],")\n return(paste(rbind(names(stats),stats),collapse=':')) }",sep='')))
    }
  }
  
  #disable jitter by default because it messes things up
  if(is.null(plot_params[['jitter']])){
    plot_params[['jitter']]<-FALSE
  }
  
  # if network is undirected, disable arrows
  if(!is.directed(net)){
    plot_params[['usearrows']]<-FALSE
  }
  
  #TODO: how are we doing interpolation?
  interp.fun<-coord.interp.smoothstep
  #interp.fun<-coord.interp.linear
  
  # copy the network object so we don't modify original
  outnet<-network.copy(net)
  # create a list object to contain rendering informaton
  render<-list()
  
  # compute lists of times that networks will be collapsed
  starts <- seq(from=slice.par[['start']],to=slice.par[['end']],by=slice.par[['interval']])
  ends <- seq(from=slice.par[['start']]+slice.par[['aggregate.dur']],to=slice.par[['end']]+slice.par[['aggregate.dur']],by=slice.par[['interval']])
  
  #compute coordinate ranges to know how to scale plots
  xmin <- min(aggregate.vertex.attribute.active(net,"animation.x",min),na.rm=TRUE)
  xmax <- max(aggregate.vertex.attribute.active(net,"animation.x",max),na.rm=TRUE)
  ymin <- min(aggregate.vertex.attribute.active(net,"animation.y",min),na.rm=TRUE)
  ymax <- max(aggregate.vertex.attribute.active(net,"animation.y",max),na.rm=TRUE)
  if (is.null(plot_params[['xlim']])){
    # deal with Inf or NA
    if(is.na(xmin) | is.infinite(xmin)){
      xmin<--1
    } 
    if(is.na(xmax) | is.infinite(xmax)){
      xmax<-1
    }
    # deal with case of only one coord, so no range
    if(xmin==xmax){
      
      xmax<-xmin+1
      xmin<-xmin-1
    }
    plot_params[['xlim']]<-c(xmin,xmax)
  }
  if(is.null(plot_params[['ylim']])){
    # deal with Inf or NA
    if(is.na(ymin) | is.infinite(ymin)){
      ymin<--1
    } 
    if(is.na(ymax) | is.infinite(ymax)){
      ymax<-1
    }
    # deal with case of only one coord, so no range
    if(ymin==ymax){
      ymax<-ymin+1
      ymin<-ymin-1
    }
    plot_params[['ylim']]<-c(ymin,ymax)
  }
  
  #set up default coords.  If not specified, default will be zero
  if(is.numeric(render.par[['initial.coords']])){
    coords<-matrix(render.par[['initial.coords']],ncol=2,nrow=network.size(net))
  }
  
  #compute some starting coords  
  slice <- network.collapse(net,starts[1],ends[1],rule=slice.par[['rule']],rm.time.info=FALSE) 
  activev <- is.active(net,starts[1],ends[1],v=seq_len(network.size(net)),rule=if(slice.par[['rule']]!='all'){'any'})
  
  # start from the coords of the first slice
  if (length(slice)>0 & network.size(slice)>0){ 
    coords[activev,1] <-get.vertex.attribute(slice,"animation.x")
    coords[activev,2] <-get.vertex.attribute(slice,"animation.y")
    #need to update plot params with slice-specific values
  }# end slice > 0 block

  #move through frames to render them out
  for(s in 1:length(starts)){
    if (verbose){message(paste("caching",render.par[['tween.frames']],"properties for slice",s-1))}
    slice <- network.collapse(net,starts[s],ends[s],rule=slice.par[['rule']],rm.time.info=FALSE)
    activev <- is.active(net,starts[s],ends[s],v=seq_len(network.size(net)),rule=if(slice.par[['rule']]!='all'){'any'})
    activeE<-valid.eids(net)[is.active(net,starts[s],ends[s],e=valid.eids(net),rule=if(slice.par[['rule']]!='all'){'any'})]
    
    #need to update plot params with slice-specific values
    evald_params<-.evaluate_plot_params(plot_params=plot_params,net=net,slice=slice,s=s,onset=starts[s],terminus=ends[s])
    
    
    #TODO:what if we want to include innactive nodes
    # set up arguments: these are only included in case the users function needs them
    coords[activev,1]<-get.vertex.attribute(slice,"animation.x")
    coords[activev,2]<-get.vertex.attribute(slice,"animation.y")
    plot_args<-list(coord=coords[activev,,drop=FALSE])
    plot_args<-c(plot_args,evald_params)
    # call the plotting function with appropriate args
    render<-cachePlotValues(slice,render,plot_args,onset=starts[s],terminus=ends[s],
                            vertices=which(activev),edges=activeE)
    # check if user has passed in extra plotting commands that need to be rendered
    if (!is.null(render.par[['extraPlotCmds']])){
      warning("extraPlotCmds not supported by d3 render")
      # TODO: could render this as an SVG and attach it? but would be slow. 
      #eval(render.par$extraPlotCmds)
    }
      
  }
  # prepare structures for JSON export
  
  # remove animation.x and .y vars, as that info is now cached in coords.active
  delete.vertex.attribute(outnet,"animation.x.active")
  delete.vertex.attribute(outnet,"animation.y.active")
  class(outnet)<-'list'  
  
  
  # create the list object to export
  out<-list(render=render,network=outnet)
  if (output.mode=='JSON'){
    outf<-file(description = filename)
    cat(minify(toJSON(out,pretty=FALSE)),file = outf)
    close(outf)
    if (verbose){
      message('wrote animation JSON representation to ',filename)
    }
  } else if (output.mode=='HTML'){
    
    renderD3Html(filename,out,d3.options,scriptType=script.type)
    
    if (verbose){
      message('wrote animation HTML file to ',filename)
    }
    # try to construct an appropriate local url for the file
    # in order to load it into the browser
    if (launchBrowser){
    if(file.exists(filename)){
        # filename may be relative, so expand to full path for url
        animationUrl<-normalizePath(filename)
        #if we are on windows, need to conver the local file backslashes to forward slashes
        if (.Platform[['file.sep']]=='\\'){
          animationUrl<-gsub('\\\\','/',animationUrl)
        }
        animationUrl<-paste('file://',animationUrl,sep='')
        if (verbose){
          message('opening local URL ',animationUrl,' in web browser')
        }
        browseURL(animationUrl)
      } else if (verbose){
        message('unable to generate correct local URL for animation HTML to launch in web browser')
      }
    }
  } else if(output.mode=='inline'){
    # render it out to a tempfile, and then load it back into the browser in an iFrame
    cacheFile<-tempfile(fileext = '.html')
    renderD3Html(cacheFile,out,d3.options,scriptType=script.type)
    # emit the html for markdown, etc
    cat('<iframe style="width: 100%; height: 500px;" src="data:text/html;base64,')
    cache64<-tempfile(fileext = '.base64')
    base64::encode(input=cacheFile,output=cache64)
    cat(readChar(cache64,1e6))
    cat('"></iframe>') 
  } else if(output.mode=='htmlWidget'){
    # convert the animation data ('out') and the options into JSON and pass it to the widget creation function
    #ndtvAnimationWidget(minify(toJSON(out,pretty=FALSE)),minify(toJSON(d3.options,pretty=FALSE,auto_unbox = TRUE)))
    # for some reason this needs the ndtv::: prefix, I guess 
    #ndtv:::ndtvAnimationWidget(out,d3.options)
    requireNamespace('htmlwidgets')
    return(ndtvAnimationWidget(out,d3.options))
  }
  
  
}

# renders out just the script includes in multiple possible ways
renderD3ScriptIncludes<-function(filename,ndtvD3BaseUrl,scriptType='embedded'){
  if(scriptType=='embedded'){  
    # include the scripts embedded inside the page
    cat("<!-- css for styling the d3.slider lib -->
      <style type='text/css'>",file=filename,append=TRUE)
    file.append(filename,file.path(ndtvD3BaseUrl,"src/lib/d3.slider.css"))
    cat("</style>
      <!-- css for styling the ndtv-d3 render and components -->
      <style type='text/css'>",file=filename,append=TRUE)
    file.append(filename,file.path(ndtvD3BaseUrl,"src/css/styles.css"))
    cat("</style>
    <!-- minimized d3.js library -->
      <script>",file=filename,append=TRUE)
  file.append(filename,file.path(ndtvD3BaseUrl,"src/lib/d3/d3.min.js"))
  cat("</script>
      <!-- minimized jquery js library -->
      <script>",file=filename,append=TRUE)
  file.append(filename,file.path(ndtvD3BaseUrl,"src/lib/jquery/dist/jquery.min.js"))
  cat("</script>
      <!-- d3.slider.js library -->
      <script>",file=filename,append=TRUE)
  file.append(filename,file.path(ndtvD3BaseUrl,"src/lib/d3.slider.js"))
  cat("</script>
      <!-- ndtv-d3 js code -->
      <script>",file=filename,append=TRUE)
  file.append(filename,(file.path(ndtvD3BaseUrl,"src/js/ndtv-d3.js")))
  cat("</script>",file=filename,append=TRUE)
  } else if (scriptType=='localSrc'){
    # include links to scripts in their local scripts
    stop('local script links not yet implemented')
  } else if (scriptType=='remoteSrc'){
    # include links for remote web urls for scripts
    cat("<!-- css for styling the d3.slider lib -->
      <link rel='stylesheet' href='http://statnet.github.io/ndtv-d3/src/lib/d3.slider.css' />
      <!-- css for styling the ndtv-d3 render and components -->
      <link rel='stylesheet' href='http://statnet.github.io/ndtv-d3/src/css/styles.css' />
      <!-- minimized d3.js library -->
      <script src='http://d3js.org/d3.v3.min.js' charset='utf-8'></script>
      <!-- minimized jquery js library -->
      <script src='http://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js'></script>
      <!-- d3.slider.js library -->
      <script src='http://statnet.github.io/ndtv-d3/src/lib/d3.slider.js'></script>
      <!-- ndtv-d3 js code -->
      <script src='http://statnet.github.io/ndtv-d3/src/js/ndtv-d3.js'></script>",file=filename,append=TRUE)
  } else {
    stop('unknown script setup option, scripts not included ')
  }
}

# print out the JSON data nd javascript initialization variables
renderD3HtmlNetworkContent<-function(filename,out,d3.options,ndtvD3BaseUrl){
  cat("<script>
    //INIT GRAPH DATA HERE
  var graphData = ",file=filename,append=TRUE)
  cat(minify(toJSON(out,pretty=FALSE)),file=filename,append=TRUE)
  cat(";
  var options = ", file=filename,append=TRUE)
  cat(minify(toJSON(d3.options,pretty=FALSE,auto_unbox = TRUE)),file=filename,append=TRUE)
  cat(";
  //END GRAPH DATA INIT
  
  //Insert init JS Here
  $(function() {
    options.graphData = graphData;
    var graph = new ndtv_d3(options);        
  })
  </script>", file=filename,append=TRUE)
}


# creates and html file with all the scripts embeded
renderD3Html<-function(filename,out,d3.options,scriptType='embedded'){
  # find the url of the various script files to be included
  #ndtvD3BaseUrl<-"/home/skyebend/SNA_health/statnet_commons/ndtv/branches/webviz/javascript/ndtv-d3"
  ndtvD3BaseUrl<-file.path(path.package('ndtv'),'javascript/ndtv-d3')
  # compile all the necessary code and data into a single html page
  # including the JSON data for the render and graph as an inline object
  # TODO: this is implemented in a very fragile way, should figure out a way to do it with a template file
  cat("<!DOCTYPE html>
<html lang='en'>
  <head>
    <meta charset='utf-8'>
      ",file=filename)
  # print out the script includes
  renderD3ScriptIncludes(filename,ndtvD3BaseUrl,scriptType=scriptType)
  cat("</head>
  <body>",file=filename,append=TRUE)
     # print out the JSON data nd javascript initialization variables
     renderD3HtmlNetworkContent(filename,out,d3.options,ndtvD3BaseUrl)
  cat("</body>
</html>",file=filename,append=TRUE)  
}

# creates a representation of the rendered network properties
# appropriate for the time slice and appends it to the list of 
# such properties describing the network's dynamics
cachePlotValues<-function(slice,renderList,plotArgs,onset,terminus,vertices,edges){
  sliceList<-list(start=onset,end=terminus)
  # attach a list element indicating which vertices and edges will be active
  # need to convert the ids into a named list so that they will become a hash in javascript
  lapply(seq_len(length(vertices)),function(index){assign(as.character(vertices[index]),index-1)})
  vertHash<-list()
  for(index in seq_len(length(vertices))){
    vertHash[[as.character(vertices[index])]]<-index-1
  }
  edgeHash<-list()
  for(index in seq_len(length(edges))){
    edgeHash[[as.character(edges[index])]]<-index-1
  }
  data<-list(active=list(nodes=vertHash,edges=edgeHash)
  )
  for(arg in names(plotArgs)){
    if(arg != 'x'){ #skip the network object
      dataVals<-plotArgs[[arg]]
      # expand any network attributes as plot.network.default would
      dataVals<-plotArgs.network(x=slice,argName=arg,argValue=dataVals)
      # any color-related elements need to be translated to rgba spec for html
      if (arg%in%c('vertex.col','label.col','vertex.border','label.border', 'label.bg','edge.col','edge.label.col','bg')){
        # if the value '0' is present, it needs to be translated to background color
        # trust that this has been set to a sensible default earlier
        dataVals[as.character(dataVals)=="0"]<-plotArgs[['bg']]
        dataVals<-col2rgbaString(dataVals)
      }
      # copy the data element into the array
      data[[arg]]<-dataVals
    }
    # need to calculate and store the label positions
  }
  sliceList[['data']]<-data
  renderList[[length(renderList)+1]]<-sliceList
  return(renderList)
}

# the functions col2hex and rgb2hex were copied from the package spatstat under the terms of GNU GPL
# Adrian Baddeley Adrian.Baddeley@uwa.edu.au http://www.maths.uwa.edu.au/~adrian/, Rolf Turner r.turner@auckland.ac.nz and Ege Rubak rubak@math.aau.dk.

# convert an R color to a html hex color code
col2hex <- function(x, alpha=FALSE) 
{
  apply(col2rgb(x,alpha=alpha), 2, rgb2hex,alpha=alpha)
}

# convert an rgb color object to html hex color code
# note that the kind with the 4th pair for alpha transparency isn't actually supported by browsers
rgb2hex<- function(v,alpha=FALSE) 
{
  stopifnot(is.numeric(v))
  if (is.matrix(v)) {
    stopifnot((!alpha & ncol(v) != 3) | (alpha & ncol(v) != 4)) # 4th column for transparency
  }
  else {
    if ((!alpha & length(v) != 3) | (alpha & length(v) != 4)) 
      stop("v should be a vector of length 3 or 4 or a matrix with 3 or 4 columns")
    v <- matrix(v, ncol = length(v))
  }
  if (!alpha){
    # this version for non-transparent
    out <- rgb(v[, 1], v[, 2], v[, 3], maxColorValue = 255)
  } else {
    # this version if there is a 4th column for alpha (transparency)
    out <- rgb(v[, 1], v[, 2], v[, 3], v[,4], maxColorValue = 255)
  }
  return(out)
}

# create valid html color strings with transparency channel
# rgba(0,255,0,0.3)

col2rgbaString <-function(x){
  rgbvals<-col2rgb(x,alpha=TRUE)
  apply(rgbvals,2,function(col){paste('rgba(',col[1],',',col[2],',',col[3],',',col[4]/255,')',sep='')})
}
