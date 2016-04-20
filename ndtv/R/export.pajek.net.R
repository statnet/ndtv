#  File R/export.pajek.net.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2016 Statnet Commons
#######################################################################
export.pajek.net <-
function(net,filename)
{ 
  
  # list of optional vertex attributes and the Pajek codes for them
vattr.dict <- list(color="ic")  # add others...
eattr.dict <- list(color="c")  # add others...
# cribbed code from write.table
  file <- file(filename,"wb")
  on.exit(close(file))

  ## first write the vertices

  # columns in order:
  #  vertex.names id x y z shape [optional attrs]
  # cases:
  #  id can be included (in "") or not
  #  x and y must be included if there are optional attrs
  #  optional attrs are key-value pairs
  #  if there's nothing but vertex.names (and these have to be
  #   integers from 1 up) just skip the list entirely

  # get list of all vertex attrs the network has
  Vattrs <- list.vertex.attributes(net)

  #cat("creating columns\n")
  columns <- data.frame(network.vertex.names(net))
    if ("label" %in% Vattrs) {
    columns <- cbind(columns, as.character(get.vertex.attribute(net,"label")))
                     #paste("\"",get.vertex.attribute(net,"id"),"\"",sep=""))
  } else if ("id" %in% Vattrs) {  # maybe include ids
    columns <- cbind(columns, as.character(get.vertex.attribute(net,"id")))
                     #paste("\"",get.vertex.attribute(net,"id"),"\"",sep=""))
  }

  # make list of possible optional attributes
  optattrs <- Vattrs[!Vattrs %in% c("vertex.names","id","x","y","z","shape")]
  # and restrict to the ones that have Pajek names on the list
  optattrs <- optattrs[match(names(vattr.dict),optattrs)]
  #cat("Vattrs:\n")
  #print(Vattrs)
  #cat("optattrs:\n")
  #print(optattrs)

  # condition for including x and y
  if ((!is.na(optattrs) && length(optattrs)>0) || "x" %in% Vattrs || "y" %in% Vattrs) {
    #cat("trying x\n")
    if ("x" %in% Vattrs)
      columns <- cbind(columns,get.vertex.attribute(net,"x"))
    else
      columns <- cbind(columns,c(0))
    #cat("trying y\n")
    if ("y" %in% Vattrs)
      columns <- cbind(columns,get.vertex.attribute(net,"y"))
    else
      columns <- cbind(columns,c(0))
  }
  # do z if it's there
  if ("z" %in% Vattrs) {
    #cat("trying z\n")
    columns <- cbind(columns,get.vertex.attribute(net,"z"))
  }
  # likewise shape
  if ("shape" %in% Vattrs) {
    #cat("trying shape\n")
    columns <- cbind(columns,get.vertex.attribute(net,"shape"))
  }

  # now do the optional attributes
  if (!is.na(optattrs))
    for (oan in optattrs)
      if (!is.na(oan)) {
        #cat("trying attr: ",oan,"\n")
        columns <- cbind(columns,vattr.dict[[oan]],get.vertex.attribute(net,oan))
      }
  
  #finally, if we only have node indicies, repeat them as a label
  if (ncol(columns) == 1) {
    columns <- cbind(columns,columns) 
  }
  # write the vertices
  #  have to go to extra lengths to make sure the newlines are DOS style
  cat(paste("*Vertices ",network.size(net),"\015\012",sep=""),file=file)
  
  #cat("writing vertices\n")
  write.table(columns,file=file,row.names=FALSE,col.names=FALSE,
                quote=c(2),eol="\015\012")
   

  # then do edges
  # check if there are any edges
  if (network.edgecount(net) > 0){
    Eattrs <- list.edge.attributes(net)

    # make list of possible optional attributes
    optattrs <- Eattrs[!Eattrs %in% c("weight")]
    # and restrict to the ones that have Pajek names on the list
    optattrs <- optattrs[match(names(vattr.dict),optattrs)]

    columns <- as.matrix.network(net,matrix.type="edgelist")

    if ("weight" %in% Eattrs)
      columns <- cbind(columns,get.edge.attribute(net$mel,"weight"))

    # now do the optional attributes
    if (!is.na(optattrs))
      for (oan in optattrs)
        if (!is.na(oan)) {
          #cat("trying attr: ",oan,"\n")
          columns <- cbind(columns,eattr.dict[[oan]],get.edge.attribute(net$mel,oan))
        }

    #cat("writing edges\n")
    if (is.directed(net))
      cat("*Arcs\015\012",file=file)
    else
      cat("*Edges\015\012",file=file)
    write.table(columns ,file=file,
              row.names=FALSE,col.names=FALSE,quote=FALSE,eol="\015\012")
   }           
}
