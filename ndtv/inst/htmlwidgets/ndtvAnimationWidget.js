HTMLWidgets.widget({

  name: 'ndtvAnimationWidget',

  type: 'output',
  

  initialize: function(el, width, height) {
    // get the DOM id of parent widget so can use it to construct a unique id for the animation
    ndtvId=d3.select(el).attr('id')+'_ndtvAnimation';
    // check if the ndtvAnimation div already exists as a child(if this is a re-render)
    if (!d3.select('#'+ndtvId).empty()){
      // remove its contents
      d3.select('#'+ndtvId).selectAll("*").remove();
    } else {
      // append the div into the DOM that ndtv_d3 will renderinto
      d3.select(el).append('div').attr('id',ndtvId).style('width',width+'px').style('height',height+'px');
    }
    return{};
  },

  renderValue: function(el, x, instance) {
      // get the DOM id of parent widget so can use it to construct a unique id for the animation
      ndtvId=d3.select(el).attr('id')+'_ndtvAnimation';
      // remove previous contents of the div (re render)
      d3.select('#'+ndtvId).selectAll("*").remove();
      // copy the graph options into the options array
      var options = x.animationOptions;
      options.graphData = x.graphData;
      // create the new ndtv instance (this will load the svg into the div with id ndtvId)
      graph = new ndtv_d3(options, '#'+ndtvId);
      return(graph);
  },

  resize: function(el, width, height, instance) {
    // get the DOM id of parent widget so can use it to construct a unique id for the animation
    ndtvId=d3.select(el).attr('id')+'_ndtvAnimation';
    d3.select('#'+ndtvId).style('width',width+'px').style('height',height+'px');
    graph.resizeGraph();  

  }

});
