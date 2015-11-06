HTMLWidgets.widget({

  name: 'ndtvAnimationWidget',

  type: 'output',
  

  initialize: function(el, width, height) {
    
    // check if the div already exists (if this is a re-render)
    if (!d3.select('#ndtvAnimation').empty()){
      // remove its contents
      d3.select('#ndtvAnimation').selectAll("*").remove()
    } else {
      // append the div into the DOM that ndtv_d3 will renderinto
      d3.select(el).append('div').attr('id','ndtvAnimation').style('width',width+'px').style('height',height+'px');
    }
    return{};
  },

  renderValue: function(el, x, instance) {
      // remove previous contents of the div (re render)
      d3.select('#ndtvAnimation').selectAll("*").remove()
      // copy the graph options into the options array
      var options = x.animationOptions;
      options.graphData = x.graphData;
      // create the new ndtv instance (this will load the svg into the div id named)
      graph = new ndtv_d3(options, '#ndtvAnimation');
      return(graph);
  },

  resize: function(el, width, height, instance) {
    d3.select('#ndtvAnimation').style('width',width+'px').style('height',height+'px');
    graph.resizeGraph();  

  }

});
