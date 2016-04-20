#  File R/specialEffects.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2016 Statnet Commons
#######################################################################

# special effects for use in plotting and movies

effectFun<-function(name,...){
  effect.fun<-get(paste('effect',name,sep='.'))
  # if any of the arguments in ... match with arguments of the function, swap 'em in
  topargs<-list(...)
  funargs<-formals(effect.fun)
  for(argname in names(topargs)){
    if (argname%in%names(funargs)){
      funargs[[argname]]<-topargs[[argname]]
    } else {
      warning("argument '",argname,"' does not match with any arguments to function ",name )
    }
  }
  formals(effect.fun)<-funargs
  
  return(effect.fun)
}

# effect to alter colors based on age of edges

effect.edgeAgeColor<-function(net,onset,fade.dur,start.color='#000000FF',end.color='#00000000',na.color='#CCCCCC55'){
  # these probably apply just to plots of entire network time range
  ages<-edges.age.at(net,at=onset)
  colors <-sapply(ages, function(age){
    if(is.na(age)){ return(na.color)} # non active edges should flip to default color
    startrgb<-col2rgb(start.color,alpha=TRUE)/255
    endrgb<-col2rgb(end.color,alpha=TRUE)/255
    interp<-max(((fade.dur-age)/fade.dur),0)
    rgba<-endrgb+(startrgb-endrgb)*interp
    return(rgb(rgba[1,1],rgba[2,1],rgba[3,1],rgba[4,1]))
  })
  return(colors)
}

effect.vertexAgeColor<-function(net,onset,fade.dur,start.color='#000000FF',end.color='#00000000',na.color='#CCCCCC55'){
  # these probably apply just to plots of entire network time range
  ages<-vertices.age.at(net,at=onset)
  colors <-sapply(ages, function(age){
    if(is.na(age)){ return(na.color)} # non active edges should flip to default color
    startrgb<-col2rgb(start.color,alpha=TRUE)/255
    endrgb<-col2rgb(end.color,alpha=TRUE)/255
    interp<-max(((fade.dur-age)/fade.dur),0)
    rgba<-endrgb+(startrgb-endrgb)*interp
    return(rgb(rgba[1,1],rgba[2,1],rgba[3,1],rgba[4,1]))
  })
  return(colors)
}


# effect to pan to follow a specific vertex
effect.xlimFollowVertex<-function(slice,v,zoom){
  
}

