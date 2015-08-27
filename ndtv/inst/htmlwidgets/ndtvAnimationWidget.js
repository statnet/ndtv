HTMLWidgets.widget({

  name: 'ndtvAnimationWidget',

  type: 'output',
  

  initialize: function(el, width, height) {
    // append the div into the DOM that ndtv_d3 will renderinto
    d3.select(el).append('div').attr('id','ndtvAnimation').attr("width", width).attr("height", height);
    return{};
  },

  renderValue: function(el, x, instance) {
       var options = x.animationOptions;
      options.graphData = x.graphData;  
      graph = new ndtv_d3(options, '#ndtvAnimation');
      return(graph);
  },

  resize: function(el, width, height, instance) {

  }

});
