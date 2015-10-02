/**
ndtv-d3 is a d3-based HTML5 network animation player for the ndtv package (http://cran.r-project.org/web/packages/ndtv/index.html)

The ndtv-d3 library was created by Greg Michalec and Skye Bender-deMoll for the statnet project http://statnet.org funded by NICHD grant R01HD068395.

This software is distributed under the GPL-3 license (http://choosealicense.com/licenses/gpl-3.0/).  It is free, open source, and has the attribution requirements (GPL Section 7) at http://statnet.org/attribution:

a. you agree to retain in ndtv-d3 and any modifications to ndtv-d3 the copyright, author attribution and URL information as provided at a http://statnetproject.org/attribution.

b. you agree that ndtv-d3 and any modifications to ndtv-d3 will, when used, display the attribution:

    Based on 'statnet' project software (http://statnetproject.org). For license and citation information see http://statnetproject.org/attribution

Copyright 2014 Statnet Commons http://statnet.org

To cite this project, please use:

Greg Michalec, Skye Bender-deMoll, Martina Morris (2014) 'ndtv-d3: an HTML5 network animation player for the ndtv package' The statnet project. http://statnet.org
@module
*/


(function (root, factory) {
  /** @class */
  root.ndtv_d3 = factory();
}(this, function() {
  "use strict";
 
  /**
  * Public options to control visualization functionality
  * @constant {object}
  * @global
  * @default
  */
  var default_options = {
    animationDuration: 800,       //Duration of each step animation during play or step actions, in milliseconds
    enterExitAnimationFactor: 0,  //Percentage (0-1) of total step animation time that enter/exit animations should take
    labelOffset: {                //pixel offset of labels
      x: 12,
      y: 0
    },
    baseFontSize: '14',           //Font size, in pixels, for labels with cex value of 1
    nodeSizeFactor: 0.01,         //Percentage (0-1) of viewport size that a node of size 1 will be rendered at
    dataChooser: false,           //show a select box for choosing different graphs?
    dataChooserDir: 'data/',      //web path to dir containing data json files
    playControls: true,           //show the player controls
    slider: true,                 //show the slider control
    menu: true,                   //show a menu in upper-right
    animateOnLoad: false,         //play the graph animation on page load
    margin: {                     //graph render area margins
      x: 20,
      y: 10
    },
    graphData: null,              //graph data, either as JSON object or URL to json file
    debugFrameInfo: false,        //Show the slice info in corner
    durationControl: true,  //Show a control to change duration speed
  };

  /**
  * Supported NDTV network properties and their default values
  * @constant {object}
  * @global
  * @default
  */
  var ndtvProperties = {
    graph: {
      xlab: null,                     // label caption below the render, on the xaxis
      main: null,                     // main headline above the render
      displaylabels: false ,          // should vertex labels be displayed
      bg: '#fff',                     // background color
      usearrows: true,                // should arrows be drawn on edges?
      xlim: null,                     // range of x values                     
      ylim: null,                     // range of y values  
      edgeOffset: 0,                  // offset of edge length to vertex borders  
      tooltipOffset: 0,               // offset of tooltips to vertex borders 
    }, 
    node: {
      coord: null,                    // coordinates for nodes
      label: null,                    // labels for vertices
      'label.col': '#000',            // color of node label
      'label.cex': 1,                 // label font size scale factor
      'vertex.cex': 1,                // vertex (node) expansion scale factor
      'vertex.col': '#F00',           // node fill color
      'vertex.sides': 50,             // number of sides for vertex polygon (shape)
      'vertex.rot': 0,                // rotation for vertex polygon
      'vertex.tooltip': '',           // vertex tooltip value
      'vertex.border': '#000',        // color of vertex border stroke
      'vertex.lwd': 1,                // width of vertex border stroke
      'vertex.css.class': null,       // css class name applied to node
      'vertex.label.css.class': null, // css class name applied to node label
      'vertex.css.style': null,       // css inline-style applied to node (UNIMPLIMENTED)
      'vertex.label.css.style': null, // css inline style applied to node label (UNIMPLEMENTED)
      'image': null,                  // background image for vertex
    },
    edge: {
      'edge.lwd': 1,                  // width of edge stroke
      'edge.col': '#000',             // edge stroke color
      'edge.tooltip': null,           // edge tooltip value
      'edge.css.class': null,         // css class name applied to edge
      'edge.label.css.class': null,   // css class name applied to edge label
      'edge.css.style': null,         // css inline-style applied to edge (UNIMPLIMENTED)
      'edge.label.css.style': null,   // css inline style applied to edge label (UNIMPLEMENTED)
    }
  }
  

  /**
  * Initialize a new ndtv-d3 instance
  * @constructs ndtv_d3
  * @param {object} - An object of default options overrides
  * @param {string|HTMLElement} - A CSS selector string or DOM element reference specifying the target dom element the network should be initialized to
  */
  var n3 = function(options, target) {
    var n3 = this;
    
    var globals = {
      svg: null,
      xScale: null,
      yScale: null,
      minTime: null,
      interval: null,
      maxTime: null,
      animate: null,
      baseNodeSize: null,
      currTime: 0,
      graph: null,
      timeIndex:null,
      domTarget:null,
      slider:null,
      nodeCoords: {},
      options: {}
    
}
    //initialize class globals
    $.extend(true, n3, globals);

    //replace defaults with user-specified options
    $.extend(true, n3.options, default_options);
    $.extend(true, n3.options, options);

    if (!target) {
      target = d3.select('body').append('div').style({width: '100%', height: '100%'}).node();
      d3.selectAll('html, body').classed({'ndtv-fullscreen': true})
    }
    n3.domTarget = d3.select(target);
    n3.domTarget.classed({'ndtv-d3-container': true});
    n3.SVGSetup(n3.domTarget);
    if (n3.options.playControls || n3.options.slider) {
      n3.domTarget.append('div').attr('class', 'controls');
    }
    if (n3.options.dataChooser) { n3.createDataChooser(); }
    if (n3.options.playControls) { n3.createPlayControls(); }
    if (n3.options.slider) { n3.createSliderControl(); }
    if (n3.options.menu) { n3.createMenu(); }


    n3.tooltip = n3.domTarget.select('.graph').append('div').attr('class', 'tooltip');
    n3.frameInfoDiv = n3.domTarget.select('.graph').append('div').attr('class', 'frameInfo')
    if (n3.options.debugFrameInfo) { n3.frameInfoDiv.style('display', 'block'); }
    if(n3.options.graphData) { n3.loadData(n3.options.graphData); }
  }

  /**
  * Initialize the SVG element and related DOM elements and listeners
  * @param {D3Selection} - DOM element to insert svg into
  */
  n3.prototype.SVGSetup = function(domTarget) {
    var n3 = this;

    $(domTarget).resize(function(n) { 
      n3.resizeGraph(n);
    });
    $(window).resize(function(n) { 
      n3.resizeGraph(n);
    });
 
    domTarget
      .append('div').attr('class', 'graph')
      .append("svg:svg")
      .append("defs")

    var svg = domTarget.select('svg')
      .append('g')

    var dragEvent;
    var rect = svg.append("rect")
      .attr('class', 'background')
      .style("fill", "none")
      .style("pointer-events", "all")
      .on('mousedown', function() { 
        dragEvent = d3.event;
      })
      .on('mouseup', function() { 
        if (Math.abs(dragEvent.pageX - d3.event.pageX) < 5 && Math.abs(dragEvent.pageY - d3.event.pageY) < 5) {
          n3.hideTooltip();
          n3.unSelectNetwork();
        }
      })

    n3.container = svg.append("g")
      .attr('class', 'container')
    n3.container.append('g').attr('class', 'edges');
    n3.container.append('g').attr('class', 'nodes');
    n3.container.append('g').attr('class', 'labels');
    n3.container.append('rect').attr('class', 'screen');
    n3.container.append('g').attr('class', 'edges_selected');
    n3.container.append('g').attr('class', 'nodes_selected');
    n3.container.append('g').attr('class', 'labels_selected');

    svg.append('g').attr('class', 'main').append('text');
    svg.append('g').attr('class', 'xlab').append('text');

    svg.on('mousedown', function() {
      svg.classed({'dragging': true})
    })
    svg.on('mouseup', function() {
      svg.classed({'dragging': false})
    })

    n3.zoom = d3.behavior.zoom()
      .scaleExtent([.5, 10])
      .on("zoom", function zoomed() {
        n3.container.attr("transform", "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")");
        n3.ctm = n3.container.node().getScreenCTM();
        n3.moveTooltip();
      })
    svg.call(n3.zoom)
  }

  /** sets positioning on svg elements based on current DOM container size and sets data scaling factors accordingly */
  n3.prototype.updateDimensions = function() {
    var n3 = this;
    var div_width = n3.domTarget.node().offsetWidth
    var div_height = n3.domTarget.node().offsetHeight - $(n3.domTarget.select('.controls').node()).outerHeight(true);

    var xlab = n3.timeIndex[n3.currTime].renderData.graph.xlab;
    var main = n3.timeIndex[n3.currTime].renderData.graph.main;
    var xlabSize = parseFloat(window.getComputedStyle(n3.domTarget.select('.xlab').node(), null).getPropertyValue('font-size'));
    var mainSize = parseFloat(window.getComputedStyle(n3.domTarget.select('.main').node(), null).getPropertyValue('font-size'));
    var mainMargin = 0;
    var xlabMargin = 0;

    if (xlab) {
      xlabMargin = xlabSize*(xlab.length+1)*1.2;
    } 
    if (main) {
      mainMargin = mainSize*(main.length+1)*1.2;
    } 
    
    var margin = {
      x: n3.options.margin.x,
      y: n3.options.margin.y
    }
    if (div_width > div_height) { 
      margin.x = (div_width - div_height)/2
    } else {
      margin.y = (div_height - div_width)/2
    }

    var width = div_width - (margin.x*2);
    var height = div_height - (margin.y*2);

    n3.domTarget.selectAll('.graph>svg, .background, .screen')
      .attr({
        width: width + margin.x * 2,
        height: height + margin.y * 2
      })
    
    //reset height including main & xlab for graph container translation
    height = height - mainMargin - xlabMargin;

    n3.container.attr("transform", "translate(" + margin.x + "," + (margin.y+mainMargin) + ")");

    var center = margin.x + width/2;
    n3.domTarget.select('.xlab').attr('transform', "translate("+center+","+(div_height-margin.y)+")")
    n3.domTarget.select('.main').attr('transform', "translate("+center+","+(margin.y+mainSize)+")")

    var pixelSpace = height > width ? width : height;
    n3.baseNodeSize = pixelSpace * n3.options.nodeSizeFactor;

    //set the X and Y scales
    n3.xScale = d3.scale.linear()
      .domain([n3.timeIndex[0].renderData.graph.xlim[0],n3.timeIndex[0].renderData.graph.xlim[1]])
      .range([0, pixelSpace]);

    n3.yScale = d3.scale.linear()
      .domain([n3.timeIndex[0].renderData.graph.ylim[0],n3.timeIndex[0].renderData.graph.ylim[1]])
      .range([pixelSpace, 0]);

    //reset zoom translate based on margins
    n3.zoom.translate([margin.x, margin.y+mainMargin])

    //Cache height and offset to use for tooltip movement
    n3.height = n3.domTarget.select('.graph').node().offsetHeight
    n3.offset = $(n3.domTarget.select('.graph').node()).offset();
    n3.ctm = n3.container.node().getScreenCTM();
  }
  
  /** creates the optional dataChooser element to be used for slecting among multiple JSON files for debugging */
  n3.prototype.createDataChooser = function() {
    var n3 = this;

    var div = n3.domTarget.append('div').attr('class', 'data_chooser_container')
    div.append('select').attr('class', 'data_chooser')
    div.append('a').attr({'class': 'video_link', 'target': '_blank'}).html('Video');

    var setVidLink = function(url) {
      div.select('.video_link').attr('href', url.replace('.json', '.mp4'))
    }
    $.get(n3.options.dataChooserDir, function(data){
      div.select('.data_chooser').on('change', function() {
        var url = $(this).val();
        n3.loadData(url);
        setVidLink(url)
      })
      var matches = data.match(/<td><a href="[^"]*"/g);
      $.each(matches, function(i, m) {
        var url = m.match(/href="([^"]*)"/)[1];
        if (url.match(/.json$/)) {
          div.select('.data_chooser').append('option').attr('value', n3.options.dataChooserDir+url).html(url);
        }
        if (i == 1) {
          setVidLink(url);
        }
      })
    })
  }
  
  /** creates the optional menu element to be used for controlling settings and displaying 'about' link */
  n3.prototype.createMenu = function() {
    var n3 = this;
    if (d3.select('#ndtv-svg-menu-icons').empty()) {
      $('body').prepend(
      '<svg id="ndtv-svg-menu-icons" display="none" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="176" height="32" viewBox="0 0 176 32">'+
      '  <defs>'+
      '    <g id="icon-list"><path d="M25.6 14.4h-19.2c-0.883 0-1.6 0.717-1.6 1.6s0.717 1.6 1.6 1.6h19.2c0.885 0 1.6-0.717 1.6-1.6s-0.715-1.6-1.6-1.6zM6.4 11.2h19.2c0.885 0 1.6-0.717 1.6-1.6s-0.715-1.6-1.6-1.6h-19.2c-0.883 0-1.6 0.717-1.6 1.6s0.717 1.6 1.6 1.6zM25.6 20.8h-19.2c-0.883 0-1.6 0.715-1.6 1.6s0.717 1.6 1.6 1.6h19.2c0.885 0 1.6-0.715 1.6-1.6s-0.715-1.6-1.6-1.6z"></path></g>'+
      '  </defs>'+
      '</svg>');
    }
    var div = n3.domTarget.select('.graph').append('div').attr('class', 'ndtv-menu-container');
    div.html(
      "<div class='ndtv-menu-icon'>"+
      " <svg class='icon menu-control' viewBox='0 0 32 32'><use xlink:href='#icon-list'></use></svg>"+
      "</div>"+
      "<div class='ndtv-menu'></div>"
    )
    var menu = n3.domTarget.select('.ndtv-menu');

    if (n3.options.durationControl) { 
      var durationControl = menu.append('div').attr('class', 'menu-item durationControlContainer');
      durationControl.append('span').attr('class', 'menu-label').html('Animation Duration');
      var durationSlider = d3.slider().min(0).max(8).axis(new d3.svg.axis().ticks(5)).value(n3.options.animationDuration/1000);
      durationControl.append('div').attr('class', 'durationControl').call(durationSlider)
      durationSlider.on('slide', function(evt, value){
        n3.options.animationDuration = value*1000;
        if(n3.options.slider) {
          n3.slider.animate(n3.options.animationDuration)
        }
      })
    }

    menu.append("div").attr('class', 'menu-item').html("<a href='https://github.com/michalgm/ndtv-d3/blob/master/README.md' target='_blank'>About NDTV-D3</a></div>");
    n3.domTarget.select('.ndtv-menu-icon').on('click', function() {
      $(menu.node()).fadeToggle(200);
      $(this).toggleClass('menu-active')
    })
  }

  /** creates the optional play controls div using svg icons and defines the attached events */
  n3.prototype.createPlayControls = function() {
    var n3 = this;

    //define SVG icons to be used in the play controller
    if (d3.select('#ndtv-svg-icons').empty()) {
      $('body').prepend(
      '<svg id="ndtv-svg-icons" display="none" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="176" height="32" viewBox="0 0 176 32">'+
      '  <defs>'+
      '    <g id="icon-play"><path class="path1" d="M26.717 15.179l-13.698-8.486c-0.998-0.654-1.814-0.171-1.814 1.072v16.474c0 1.243 0.818 1.725 1.814 1.070l13.699-8.486c0 0 0.486-0.342 0.486-0.822-0.002-0.478-0.488-0.821-0.488-0.821z"></path></g>'+
      '    <g id="icon-pause"><path class="path1" d="M21.6 4.8c-1.59 0-2.88 0.49-2.88 2.080v18.24c0 1.59 1.29 2.080 2.88 2.080s2.88-0.49 2.88-2.080v-18.24c0-1.59-1.29-2.080-2.88-2.080zM10.4 4.8c-1.59 0-2.88 0.49-2.88 2.080v18.24c0 1.59 1.29 2.080 2.88 2.080s2.88-0.49 2.88-2.080v-18.24c0-1.59-1.29-2.080-2.88-2.080z"></path></g>'+
      '    <g id="icon-first"><path class="path1" d="M11.976 16c0 0.413 0.419 0.707 0.419 0.707l11.64 7.31c0.862 0.565 1.565 0.149 1.565-0.92v-14.195c0-1.070-0.702-1.486-1.565-0.922l-11.64 7.312c0 0.002-0.419 0.294-0.419 0.707zM6.4 8.571v14.858c0 1.421 0.979 1.856 2.4 1.856s2.4-0.435 2.4-1.854v-14.859c0-1.422-0.979-1.858-2.4-1.858s-2.4 0.437-2.4 1.858z"></path></g>'+
      '  </defs>'+
      '</svg>');
    }
    var div = n3.domTarget.select('.controls').append('div').attr('class', 'play-control-container');
    div.html(
      "<div class='step-back-control'><svg class='icon' width='32' height='32' viewBox='0 0 32 32'><use xlink:href='#icon-first'></use></svg></div>"+
      "<div class='play-back-control'><svg class='icon' width='32' height='32' viewBox='0 0 32 32'><g transform='rotate(180, 16, 16)'><use xlink:href='#icon-play'></use></g></svg></div>"+
      "<div class='pause-control'><svg class='icon' width='32' height='32' viewBox='0 0 32 32'><use xlink:href='#icon-pause'></use></svg></div>"+
      "<div class='play-forward-control'><svg class='icon' width='32' height='32' viewBox='0 0 32 32'><use xlink:href='#icon-play'></use></svg></div>"+
      "<div class='step-forward-control'><svg class='icon' width='32' height='32' viewBox='0 0 32 32'><g transform='rotate(180, 16, 16)'><use xlink:href='#icon-first'></use></g></svg></div>"
    );

    div.select('.step-back-control').on('click', function() { n3.stepAnimation(1); });
    div.select('.play-back-control').on('click', function() { n3.playAnimation(1); });
    div.select('.pause-control').on('click', function() { n3.endAnimation(); });
    div.select('.play-forward-control').on('click', function() { n3.playAnimation(); });
    div.select('.step-forward-control').on('click', function() { n3.stepAnimation(); });
  }
  
  /** creates the time slider controls and defines attached events */
  n3.prototype.createSliderControl = function() {
    var n3 = this;
    n3.domTarget.select('.controls').append('div').attr('class', 'slider-control-container').append('div').attr('class', 'slider');
  }
 
  /** load and process the JSON formatted data
  * @param {url|JSON} - either a NDTV-generated JSON object, or a URL path to file containing JSON data
  */
  n3.prototype.loadData = function(graphData) {
    var n3 = this;
    n3.endAnimation();
    n3.currTime = 0;
    n3.selectedNetwork = null;
    n3.selected = null;
    
    var processData = function(data) {
      console.time('loadData');
      n3.graph = data.network;
      n3.timeIndex = data.render;
      if (n3.options.dataChooser && ! $.isPlainObject(graphData)) {
        $(n3.domTarget.select('.data_chooser').node()).val(graphData);
        n3.domTarget.select('.video_link').attr('href', graphData.replace('.json', '.mp4'))
      }
      n3.container.selectAll('.edge, .label, .node').remove();

      n3.nodeCoords = {};

      $.each(n3.graph.val, function(i, n) {
        if (! $.isEmptyObject(n)) {
          n.id = i+1;
        }
        n3.nodeCoords[n.id] = {
          coord: [0,0],
          active: false,
          size: 0,
          points: []
        }
      })
      $.each(n3.graph.mel, function(i, e) {
        if (! $.isEmptyObject(e)) {
          e.id = i+1;
        }
      })
      var sliceInfo = n3.graph.gal['slice.par'];
      
      n3.minTime = sliceInfo.start[0];
      n3.maxTime = sliceInfo.end[0];
      n3.interval = sliceInfo.interval[0];
      var valIndex = {};
      $.each(n3.timeIndex, function(i, t){
        valIndex[t.start] = i;
        t.renderData = n3.generateSliceRenderData(i);
        delete t.data; //remove redundant data that we've stored in renderData
      })      

      n3.updateDimensions();

      if(n3.options.slider) {
        var sliderDiv = n3.domTarget.select('.slider');

        sliderDiv.html('');

        n3.slider = d3.slider().axis(true).step(n3.interval);
        n3.slider.margin(35)
        n3.slider.min(n3.minTime)
        n3.slider.max(n3.maxTime+sliceInfo['aggregate.dur'][0])
        n3.slider.animate(n3.options.animationDuration)
        n3.slider.value(n3.minTime)
        n3.slider.interval(sliceInfo['aggregate.dur'][0])
        n3.slider.on('slide', function(ext, value) {
          //Check to see if event originated from slider control or elsewhere
          var event = d3.event;
          if (event.type == 'drag' || d3.select(event.currentTarget).classed('d3-slider')) {
            n3.endAnimation();
            n3.animateGraph(n3.currTime, valIndex[value], true);
          }
        })
        
        n3.slider.on('slideend', function() {
          n3.slider.animate(n3.options.animationDuration);
        })
        sliderDiv.on('mousedown', function(e) { 
          n3.slider.animate(0);
        })

        sliderDiv.call(n3.slider);
      }
      console.timeEnd('loadData');
      if (n3.options.animateOnLoad) {
        n3.playAnimation();
      } else {
        n3.updateGraph(n3.options.animationDuration);
      }
    };

    if($.isPlainObject(graphData)) {
      processData(graphData)
    } else {
      $.getJSON(graphData, function(data) {
        processData(data);
      });
    }
  }

  var color = d3.scale.category20();

  /** For a given time slice, process timeIndex data and generate render data, filling in defaults as necessary
  * @param {integer} - the time index to process
  * @private
  */
  n3.prototype.generateSliceRenderData = function(time) {
    var n3 = this;

    var sliceRenderData = {
      graph: {},
      node: {},
      edge: {}
    };

    var data = n3.timeIndex[time].data;

    $.each(['graph', 'node', 'edge'], function(i, type) {
      var sourceList = [];
      if (type == 'graph') {
        sourceList = [n3.graph.gal];
      } else if (type == 'node') {
        sourceList = n3.graph.val;
      } else {
        sourceList = n3.graph.mel;
      }

      $.each(sourceList, function(i, item) {
        var id =0;
        var propertyIndex;
        var itemProperties = {};

        if (type != 'graph') {
          id = item.id;
          propertyIndex = data.active[type+'s'][id];
        }

        if (type == 'graph' || propertyIndex !== undefined) {
          itemProperties.id = id;
          $.each(ndtvProperties[type], function(property, def) {
            var lookup = propertyIndex;
            var value = def;

            //If the property list has only one value, we apply it to all items
            if(typeof data[property] !== 'undefined' && data[property].length == 1) {
              lookup = 0;
            }
            if (typeof data[property] !== 'undefined' && data[property][lookup] !== undefined) {
              value = data[property][lookup];
              if (value && (property == 'main' || property == 'xlab')) {
                value = value.split('\n');
              } else if (! value && property == 'coord') {
                console.log('missing coordinates for node '+id+ ' at time '+time+' ('+n3.timeIndex[time].start+'-'+n3.timeIndex[time].end+')');
              }
            } else if (type == 'graph' && typeof data[property] !== 'undefined') { //graph properties get applied directly
              value = data[property];
            } else if (property == 'label' && sliceRenderData.graph.displaylabels) {
              value = id;
            }
            if (value && $.type(value) === 'string' && value.match('rgba')) {
              var rgba = value.match(/^rgba\((.*), ?(.*)\)$/);
              if (rgba) {
                value = "rgb("+rgba[1]+")";
                var opacityProp = property+".stroke-opacity";
                if (property == 'vertex.col' || property == 'bg' || property == 'label.col') {
                  opacityProp = property+".fill-opacity";
                }
                itemProperties[opacityProp] = rgba[2];
              }
            }
            // if (property == 'edge.lwd') {
              // value = id*0.2;
            // }
            // if (property == 'edge.col' || property == 'vertex.col' || property == 'label.col') {
            //   value = type == 'edge' ? color(time) : color(time+1);
            // }
            itemProperties[property] = value;
          })

          if (type == 'edge') {
            $.each(['inl', 'outl'], function(i, direction) {
              var otherdir = direction == 'inl' ? 'outl' : 'inl';
              itemProperties[direction] = {
                id: item[direction][0]
              }
              sliceRenderData.node[item[direction][0]].links[item[otherdir][0]] = id;
            });      
          } else if (type == 'node') {
            itemProperties.links = {};
          }
          if (type == 'graph') {
            sliceRenderData[type] = itemProperties;
          } else {
            sliceRenderData[type][id] = itemProperties;
          }
        } else if (type == 'node') {
          n3.nodeCoords[id].active = false;
        }
      })
    })
    return sliceRenderData;
  }

  /** updates renderdata node coordinates based on current state of graph, and updates node state tracker
  * @param {integer} - the time index to process
  * @private
  */
  n3.prototype.updateSliceRenderData = function(time) {
    var n3 = this;

    var prevNodeCoords = $.extend({}, n3.nodeCoords);
    var data = n3.timeIndex[time].renderData;
    
    $.each(n3.nodeCoords, function(id, nodeCoord) {
      var node = data.node[id];
      if (node) {
        if (!node.coord) {
          node.renderCoord = nodeCoord.coord;
        } else {
          node.renderCoord = node.coord;
        }
        n3.nodeCoords[id] = {
          coord: node.renderCoord,
          active: true,
          size: node['vertex.cex'],
          prevPoints: prevNodeCoords[id].points,
          //FIXME - this could break if a node is ever centered at 0,0
          prevCoord: prevNodeCoords[id].coord[0] && prevNodeCoords[id].coord[1] ? prevNodeCoords[id].coord : null
        }
      } else {
        n3.nodeCoords[id].active = false;
      }
    });
    $.each(data.edge, function(id, edge){
      $.each(['inl', 'outl'], function(i, direction){
        var nodeid = edge[direction].id;
        var prevCoord = prevNodeCoords[nodeid];
        var coords = n3.nodeCoords[nodeid] || prevCoord;
        edge[direction] = {
          id: nodeid,
          coords: coords,
          prevPoints:prevCoord.points,
          //If the node is newly active, use the current coordinates for the start values
          startCoords: ! prevCoord.active ? coords : prevCoord
        }
      })
    })
    return data;
  }

  /** create a path descriptions on the given data selection
  * @param {object} - the D3 data selection
  * @param {NDTV_D3}
  * @param {boolean} - If true, positions end of line offset of node radius to accomodate arrowhead 
  * @param {boolean} - If true, draws path using current node positions (before animation begins)
  * @private
  */
  n3.prototype.drawEdge = function(selection, n3, usearrows, start) {
    selection.attr({
      d: function(d) {
        var type = start ? 'startCoords' : 'coords';

        var startNode = d.outl[type];
        var endNode = d.inl[type];
        var startCoords = [n3.xScale(startNode.coord[0]), n3.yScale(startNode.coord[1])];
        var endCoords = [n3.xScale(endNode.coord[0]), n3.yScale(endNode.coord[1])];

        if(usearrows || ndtvProperties.graph.edgeOffset) {
          var arrowOffset = usearrows ? scaleArrowheads(d)*d['edge.lwd'] : 0;
          startCoords = findNodeIntersection(n3, d.outl.id, endCoords, ndtvProperties.graph.edgeOffset, start)
          endCoords = findNodeIntersection(n3, d.inl.id, startCoords, arrowOffset+ndtvProperties.graph.edgeOffset, start)
        }
        d['currentCoords'] = [startCoords, endCoords];
        return 'M '+startCoords[0].toFixed(1)+' '+startCoords[1].toFixed(1)+' L '+endCoords[0].toFixed(1)+' '+endCoords[1].toFixed(1);     
      }
    })
  }

  /** initializes a D3 line drawing function
  * @private */
  var drawLine = function() {
    return d3.svg.line()
      .x(function(d){return d[0];})
      .y(function(d){return d[1];})
  }
  
  /** offset a line between two given points by the specified amount. returns the new target point
  * @param {point} - the origin point
  * @param {point} - the target point
  * @param {number} - the offset amount (as length of line)
  * @private */
  var offsetLine = function(pointA, pointB, offset) {
    pointA = pointA.map(parseFloat);
    pointB = pointB.map(parseFloat);

    var xlen = pointB[0] - pointA[0];
    var ylen = pointB[1] - pointA[1];

    // Determine hypotenuse length
    var hlen = Math.sqrt(Math.pow(xlen,2) + Math.pow(ylen,2));

    // Determine the ratio between the shortened value and the full hypotenuse.
    var ratio = (hlen - offset) / hlen;

    //If the ratio is invalid, just use the original coordinates
    if ($.isNumeric(ratio)) { 
      return [
        pointA[0] + (xlen * ratio),
        pointA[1] + (ylen * ratio)
      ];
    } else {
      return pointB;
    }
  }

  var scaleArrowheads = function(d) {
    return 6-2.5*Math.atan(d['edge.lwd']*.6)
  }
  /** find the point at which a line drawn from a point to the center of a node intersects with the nodes border.
  * returns the coordinate, optionally applying an offset
  * @param {n3} n3 - the n3 object
  * @param {id} id - the node id
  * @param {point} point - the point of origin for the line
  * @param { number} offset - the offset amount to apply
  * @param {boolean} usePrev - should we use the current slice coordinates, or those of the previous state?
  * @private */
  var findNodeIntersection = function(n3, id, point, offset, usePrev) {
    offset = offset || 0;
    var nodeData = n3.nodeCoords[id];
    var center = usePrev && nodeData.prevCoord ? nodeData.prevCoord.slice() : nodeData.coord.slice();
    var intersection = [
      n3.xScale(center[0]),
      n3.yScale(center[1])
    ];

    if (nodeData.points) {
      var points = usePrev && nodeData.prevPoints.length ? nodeData.prevPoints : nodeData.points;
      intersection = findPolygonIntersection(points, intersection, point);
    } else {
      offset += nodeData.size * n3.baseNodeSize;
    }
    return offsetLine(point, intersection, offset);
  }
 
  /** find the point of intesection of two lines - adapted from http://paulbourke.net/geometry/pointlineplane/ 
  * @private */
  var findLineIntersection = function(a, b, c, d) {
    var denominator = (d[1] - c[1]) * (b[0] - a[0]) - (d[0] - c[0]) * (b[1] - a[1]);
    var precision = 100000;

    var one = ( (d[0] - c[0]) * (a[1] - c[1]) - (d[1] - c[1]) * (a[0] - c[0]) ) / denominator;
    var two = ( (b[0] - a[0]) * (a[1] - c[1]) - (b[1] - a[1]) * (a[0] - c[0]) ) / denominator;
    one = Math.round(one * precision) / precision;
    two = Math.round(two * precision) / precision;

    if (one <= 1 && one >= 0 && two <= 1 && two >= 0) {
      var x = a[0] + one * (b[0] - a[0]);
      var y = a[1] + one * (b[1] - a[1]); 
      return [x,y];
    }
  }

  /** find the point at which a line intersect a polygon
  * @private */
  var findPolygonIntersection = function(points, centerA, centerB) {
    var intersection = [];
    for(var i = 0; i<points.length-1; i++) {
      var res = findLineIntersection(points[i], points[i+1], centerA, centerB);
      if (res) {
        intersection = res;
        break;
      }
    }
    if (! intersection.length) {
      intersection = findLineIntersection(points[points.length-1], points[0], centerA, centerB);
    }
    if (! intersection) {
      console.log("unable to find intersection! using target point", points, centerA, centerB);

      //debugging code
      // var color = d3.scale.category10().domain(d3.range(0,10))(graph2.currTime);
      // d3.select('.container').append('path').attr('d', "M "+centerA[0] + ' '+centerA[1] + ' L '+ centerB[0] + ' '+centerB[1]).style({'stroke': color})
      // for(i = 0; i<points.length-1; i++) {
      //   d3.select('.container').append('path').attr('d', "M "+points[i][0] + ' '+points[i][1] + ' L '+ points[i+1][0] + ' '+points[i+1][1]).style({'stroke': color})
      // }
      // d3.select('.container').append('path').attr('d', "M "+points[points.length-1][0] + ' '+points[points.length-1][1] + ' L '+ points[0][0] + ' '+points[0][1]).style({'stroke': color})
      return centerB; //FIXME - this should be enabled for production

    }
    return intersection;
  }

  /** creates a polygon-shaped path attribute for given node selection
  * @param {D3selection}
  * @param {NDTV_D3}
  * @private
  */
  n3.prototype.drawPolygonNode = function(selection, n3){
    // console.profile('polymath')
    // console.log(renderData.n)
    selection.attr({
      points: function(d, i) { 
        var sides = d['vertex.sides'];
        var size = d['vertex.cex'] * n3.baseNodeSize;
        var coords = d.renderCoord;
        var rotation = d['vertex.rot'];
        var centerX = n3.xScale(coords[0]);
        var centerY = n3.yScale(coords[1]);

        var rot = rotation * 2 * Math.PI/360
        var base = 1/sides * 2 * Math.PI;
        n3.nodeCoords[d.id].points = [];

        for (var i = 1; i <= sides; i++) {
            var ang = i * base + rot;
            var x = centerX + size * Math.cos(ang);
            var y = centerY + size * Math.sin(ang);
            n3.nodeCoords[d.id].points.push([x, y]);
        }
        return n3.nodeCoords[d.id].points.map(function(p) { return p[0].toFixed(1)+','+p[1].toFixed(1); }).join(' ');
      },
    })
    // console.profileEnd('polymath')
  }

  /** creates circle attributes for given node selection
  * @param {D3selection}
  * @param {NDTV_D3}
  * @private
  */
  n3.prototype.drawCircleNode = function(selection, n3){
    selection.attr({
      cx: function(d, i) { return n3.xScale(d.renderCoord[0]).toFixed(1); },
      cy: function(d, i) { return n3.yScale(d.renderCoord[1]).toFixed(1); },
      r: function(d, i) { return (d['vertex.cex'] * n3.baseNodeSize).toFixed(1); },
    })
  }

  /** positions the node labels
  * @param {D3selection}
  * @param {NDTV_D3}
  * @private
  */
  n3.prototype.drawNodeLabel = function(selection, n3){
    selection.attr({
      x: function(d, i) { return (n3.xScale(d.renderCoord[0])+n3.options.labelOffset.x).toFixed(1); },
      y: function(d, i) { return (n3.yScale(d.renderCoord[1])+n3.options.labelOffset.y).toFixed(1); },
    })
  }

  /** highlights the currently selected network */
  n3.prototype.updateSelectedNetwork = function() {
    var n3 = this;
    var node = n3.selectedNetwork;
    $.each(['node_group', 'edge', 'label'], function(i, classname) {
      var selection = n3.container.selectAll('.'+classname)
      var type = classname == 'node_group' ? 'node' : classname;
      var unselectedTargetClass = '.'+type+'s';
      var selectedTargetClass = '.'+type+'s_selected';
      var unselectedTarget = n3.container.select(unselectedTargetClass).node();
      var selectedTarget = n3.container.select(selectedTargetClass).node();

      selection.each(function(d){
        var target = unselectedTarget;
        var targetClass = unselectedTargetClass;
        if (type == 'edge') {
          if (node && (d.inl.id == node.id || d.outl.id == node.id)) {
            target = selectedTarget;
            targetClass = selectedTargetClass;
          }
        } else {
          if (node && (node.links[d.id] !== undefined || d.id == node.id)) {
            target = selectedTarget;
            targetClass = selectedTargetClass;
          }
        }
        // console.log(targetClass);
        if (! $(this).parent(targetClass).length) {
          // console.log(n3.currTime)
          $(target).append(this);
        }
      })    
    })    
  }

  /** unhighlights the currently selected network */
  n3.prototype.unSelectNetwork = function() {
    var n3 = this;
    n3.selectedNetwork = null;
    n3.container.select('.screen').classed({'network-selected': false});
    n3.updateSelectedNetwork();
  }

  /** render the graph to reflect the state at currTime, transitioning elements over a given duration
  * @param {milliseconds} - the amount of time the transition animation should take
  */
  n3.prototype.updateGraph = function(duration) {
    var n3 = this;
    // console.profile('update '+n3.currTime);

    var renderData = n3.updateSliceRenderData(n3.currTime);
    n3.frameInfoDiv.html(n3.currTime+': '+n3.timeIndex[n3.currTime].start + '-'+n3.timeIndex[n3.currTime].end)

    var enterExitDuration = duration * n3.options.enterExitAnimationFactor;
    var updateDuration = duration * (1-n3.options.enterExitAnimationFactor);

    $.each(['main', 'xlab'], function(i, type){
      var text = renderData.graph[type];
      var target = n3.domTarget.select('.'+type+' text');
      target.selectAll('*').remove();

      if (text) {
        $.each(text, function(i, t){
          target.append('tspan').attr({
            'dy': (i ? '1.2em' : 0),
            'x': 0,
          }).text(t);
        })
      }
    });

    n3.domTarget.selectAll('.background, .screen').transition()
      .style({fill: renderData.graph['bg'], 'fill-opacity': renderData.graph['bg.fill-opacity']});

    var showInfo = function(d) {
      if(! n3.selected || n3.selected.id !== d.id) {
        n3.selected = d;
        n3.moveTooltip();
      } else {
        n3.hideTooltip();
      }
    }

    //update selected item
    if (n3.selected) {
      n3.selected = n3.selected.inl ? renderData.edge[n3.selected.id] : renderData.node[n3.selected.id];
      if (! n3.selected) {
        n3.hideTooltip();
      } 
    }
    if (n3.selectedNetwork) {
      n3.selectedNetwork = renderData.node[n3.selectedNetwork.id];
    }

    /** apply styles and atrributes to nodes
    * @private
    */
    var styleNodes = function(selection) {
      selection.style({
        // 'fill': function(d, i) {return d['vertex.col']; },
        fill: function(d) {
          if (d.image) { 
            return 'url(#image_'+parseInt(d.id)+')'; 
          } else {
            return d['vertex.col'];
          }
        },

        'fill-opacity': function(d, i) {return d['vertex.col.fill-opacity']; },
        'stroke-width': function(d) {return d['vertex.lwd']; },
        'stroke': function(d) {return d['vertex.border']; },
        'stroke-opacity': function(d) {return d['vertex.border.stroke-opacity']; },
      })
      selection.filter('circle').call(n3.drawCircleNode, n3)
      selection.filter('polygon').call(n3.drawPolygonNode, n3)
    }

    /** set attributes & transitions to be applied to new nodes
    * @private
    */
    var createNodes = function(selection) {
      selection
        .attr({
          class: function(d) { return 'node node_'+d.id+' '+(d['vertex.css.class'] || ''); },
          opacity: 0,
          // fill: function(d) { return 'url(#image_'+d.id+')'; }
        })
        .call(styleNodes)
        .on('click', showInfo)
        .on('dblclick', function(d) {
          if (! n3.selectedNetwork || d.id != n3.selectedNetwork.id) {
            n3.selectedNetwork = d;
          } else {
            n3.selectedNetwork = null;
          }
          n3.updateSelectedNetwork()
          n3.container.select('.screen').classed({'network-selected': n3.selectedNetwork})
          n3.selected = n3.selectedNetwork;
          n3.moveTooltip();
          d3.event.stopPropagation();
        })
        .on('mouseover', function(d) {
          d3.select(this)
            .transition()
            .duration(200)
            .style('stroke-width', parseFloat(d['vertex.lwd'])+5);
        })
        .on('mouseout', function(d) {
          d3.select(this)
            .transition()
            .duration(200)
            .style('stroke-width', d['vertex.lwd']);
        })
        .transition()
        .duration(enterExitDuration)
        .attr('opacity', 1)

    }

    var images = n3.domTarget.select('defs').selectAll('.node-image').data(d3.values(renderData.node).filter(function(d) { return d.image; }), function(n) { return n.id; })
      images.enter()
        .append('pattern').attr({
        id: function(d) { return 'image_'+d.id; },
        class: 'node-image',
        viewBox:"0 0 100 100",
        // preserveAspectRatio:"none", 
        // 'patternUnits': 'userSpaceOnUse',
        width: 10,
        height:10
      }).append('image').attr({
        'xlink:href': function(d) { return d.image; },
        width: 10,
        height:10        
      })

      images.select('image').transition()
        .delay(enterExitDuration)
        .duration(updateDuration)
        .attr({
          'xlink:href': function(d) { return d.image; }
        });

      images.exit()
        .transition()
        .duration(enterExitDuration)
        .remove(); 

    var nodes = n3.container.selectAll('.node').data(d3.values(renderData.node), function(e) { return e.id; })

      var node_groups = nodes.enter().append('g').classed({'node_group' : true});
      node_groups.filter(function(d) { return d['vertex.sides'] != 50; }).append('polygon').call(createNodes);
      node_groups.filter(function(d) { return d['vertex.sides'] == 50; }).append('circle').call(createNodes);

      if (!enterExitDuration) {nodes.attr({opacity: 1}); }

      nodes.filter('.node').transition()
        .delay(enterExitDuration)
        .duration(updateDuration)
        .attr('opacity', 1)
        .call(styleNodes)

      nodes.exit()
        .transition()
        .duration(enterExitDuration)
        .attr('opacity', 0)
        .remove(); 

    if (renderData.graph.usearrows) {
      var markers = n3.domTarget.select('defs').selectAll('.arrowhead').data(d3.values(renderData.edge), function(e) { return e.id})
        markers.enter().append('marker').attr({
          id: function(d) { return 'arrowhead_'+d.id; },
          class: 'arrowhead',
          viewBox: "0 -1 2 2",
          markerWidth: scaleArrowheads,
          markerHeight: scaleArrowheads,
          orient: "auto",
        }).append("svg:path")
          .attr({
            d: "M0,-1L2,0L0,1",
            fill: 'green'
          });

        markers.select('path').transition()
          .delay(enterExitDuration)
          .duration(updateDuration)
          .attr({
            fill: function(d) { return d['edge.col']; },
            'fill-opacity': function(d) { return d['edge.col.stroke-opacity']; }
          })

        markers.exit().selectAll('path')
          .attr({
            fill: 'red'
          })
        
        markers.exit().transition()
          .delay(duration)
          .duration(0)
          .remove()
    }

    var lines = n3.container.selectAll('.edge').data(d3.values(renderData.edge), function(e) { return e.id})
      lines.enter().append('path')
        .attr({
          class: function(d) { return 'edge edge_'+d.id+' '+(d['edge.css.class'] || ''); },     
          opacity: 0,
          "marker-end": function(d) { if(renderData.graph.usearrows) { return "url(#arrowhead_"+d.id+")"; }}
        })
        .style({
          'stroke': 'green',
          'stroke-width': function(d) { return d['edge.lwd']; }
        })
        .call(n3.drawEdge, n3, renderData.graph.usearrows, 1)
        .on('click', showInfo)
        .on('mouseover', function(d) {
          var edge = d3.select(this);
          var line = 'M '+d.currentCoords[0][0].toFixed(1)+' '+d.currentCoords[0][1].toFixed(1)+' L '+d.currentCoords[1][0].toFixed(1)+' '+d.currentCoords[1][1].toFixed(1);
          if (renderData.graph.usearrows) {
            var offsetPoints = offsetLine([d.currentCoords[0][0], d.currentCoords[0][1]], [d.currentCoords[1][0], d.currentCoords[1][1]], (scaleArrowheads(d)*5));
            line = 'M '+d.currentCoords[0][0].toFixed(1)+' '+d.currentCoords[0][1].toFixed(1)+' L '+offsetPoints[0].toFixed(1)+' '+offsetPoints[1].toFixed(1);     
          }
          edge.transition()
            .duration(200)
            .style('stroke-width', parseFloat(d['edge.lwd'])+5)
            .attr('d', line);
        })
        .on('mouseout', function(d) {
          d3.select(this).transition()
            .duration(200)
            .style('stroke-width', d['edge.lwd'])
            .call(n3.drawEdge, n3, renderData.graph.usearrows, false)
        })
        .transition()
        .duration(enterExitDuration)
        .attr({opacity: 1})

      if (!enterExitDuration) {lines.attr({opacity: 1}); }
      
      lines.transition()
        .delay(enterExitDuration)
        .duration(updateDuration)
        .style({
          'stroke': function(d) { return d['edge.col']},
          'stroke-opacity': function(d) { return d['edge.col.stroke-opacity']; },
          'stroke-width': function(d) { return d['edge.lwd']; },
        })
        .call(n3.drawEdge, n3, renderData.graph.usearrows)
        .attr({opacity: 1})

      lines.exit()
        .style('stroke', 'red')
        .transition()
        .duration(enterExitDuration)
        .attr('opacity', 0)          
        .remove();

    var labels = n3.container.select('.labels').selectAll('text').data(d3.values(renderData.node), function(e) { return e.id});
      labels.enter().append('text').filter(function(d) { return renderData.graph.displaylabels; })
        .attr({
          class: function(d) { return 'label label_'+d.id+ ' '+ (d['vertex.label.css.class'] || ''); },
          opacity: 0
        })
        .call(n3.drawNodeLabel, n3)
        .text(function(d, i) { return d.label; })
        .style({
          'fill': function(d) {return d['label.col']; },
          'fill-opacity': function(d) {return d['label.col.fill-opacity']; },
          'font-size': function(d) { return n3.options.baseFontSize * d['label.cex']+'px';}
        })
        .transition()
        .delay(0)
        .duration(enterExitDuration)
        .attr({opacity: 1})

      if (!enterExitDuration) {labels.attr({opacity: 1}); }

      labels.transition().filter(function(d) { return renderData.graph.displaylabels; })
        .delay(enterExitDuration)
        .duration(updateDuration)
        .call(n3.drawNodeLabel, n3)
        //.attr({opacity: 1})
        .text(function(d, i) { return d.label; })
        .style({
          'fill': function(d) {return d['label.col']; },
          'fill-opacity': function(d) {return d['label.col.fill-opacity']; },
        })

      labels.exit()
        .transition()
        .duration(enterExitDuration)
        .attr('opacity', 0)
        .remove();

      n3.updateSelectedNetwork();
  
      var start = Date.now();
      d3.timer(function() {
        if (n3.selected !== undefined) {
          n3.moveTooltip();
        }
        return Date.now() >= start +duration; 
      })
      // console.profileEnd('update '+n3.currTime);
  }

  /** resizes graph and other display elements to fill the target viewport */
  n3.prototype.resizeGraph = function() {
    var n3 = this;
    n3.updateDimensions();


    n3.container.selectAll('circle.node').call(n3.drawCircleNode, n3)
    n3.container.selectAll('polygon.node').call(n3.drawPolygonNode, n3)

    n3.container.selectAll('.edge').call(n3.drawEdge, n3, n3.timeIndex[n3.currTime].renderData.graph.usearrows)

    n3.container.select('.labels').selectAll('text').call(n3.drawNodeLabel, n3)

    n3.moveTooltip();
    //redraw the slider control 
    if (n3.options.slider) {
      var sliderDiv = n3.domTarget.select('.slider');
      sliderDiv.html('');
      sliderDiv.call(n3.slider);
    } 
  }
  
  /** graph animation controller
  * @param {integer} - render the graph to the state at this timeslice index
  * @param {integer} - function will recursively call itself until time equals this value
  * @param {boolean} - should the graph update immediately, or animate?
  */
  n3.prototype.animateGraph = function(time, endTime, immediate) {
    var n3 = this;
    if (time > n3.timeIndex.length -1 || time < 0) { return; }

    var duration = immediate ? 0 : n3.options.animationDuration;
    endTime = endTime === undefined ? n3.timeIndex.length -1 : endTime;
    var nextTime;
    if (time == endTime) {
      nextTime = time;
    } else if (endTime > time) {
      nextTime = time +1;
    } else {
      nextTime = time -1;
    }

    n3.currTime = time == n3.currTime ? nextTime : time;
    //console.log(n3.currTime + ' '+time+' '+endTime+ ' '+nextTime+ ' '+n3.prevTime)
    if(! immediate && n3.options.slider) {
      n3.slider.value(n3.timeIndex[n3.currTime].start[0]);
    }
    n3.updateGraph(duration);
    if (n3.currTime != endTime) {
      n3.animate = setTimeout(function(){
        n3.animateGraph(nextTime, endTime, immediate);
      }, duration)
    }
  }

  /** redraw the info popover //FIXME - needs renamed */
  n3.prototype.moveTooltip = function() {
    // console.profile('moveTooltip');
    var n3 = this;
    if (n3.selected) {
      var item = n3.selected;
      var type = item.inl ? 'edge' : 'node';
      var nodeDOM = n3.container.select('.'+type+'_'+item.id).node();
      if (!nodeDOM) {
        n3.hideTooltip();
      } else {
        var coords = n3.convertCoords(item);
        var property = 'vertex.tooltip';
        if (type == 'edge') {
          property = 'edge.tooltip';
        }
        var html = n3.selected[property] || type+" id: "+n3.selected.id;
        n3.tooltip.style({
          display: 'block',
          bottom: coords[1]+'px',
          left: coords[0]+'px',
        }).html(html)
      }
    } else {
      n3.hideTooltip();
    }
    // console.profileEnd('moveTooltip');
  }

  /** get center point of edge or node, in DOM pixels */
  n3.prototype.convertCoords = function(item) {
    var n3 = this;
    var type = item.inl ? 'edge' : 'node';
    var nodeDOM = n3.container.select('.'+type+'_'+item.id).node();
    var ctm = n3.ctm;
    var x, y;
    var bbox = nodeDOM.getBBox();
    var center = {
      x: bbox.x + bbox.width/2,
      y: bbox.y + bbox.height/2
    }
    if (type == 'node') {
      if (nodeDOM.tagName == 'polygon') {
        var points = $('polygon.node_'+item.id).attr('points').split(' ').map(function(p) { 
          return p.split(',').map(parseFloat);
        })
        var point = findPolygonIntersection(points, [bbox.x + bbox.width, bbox.y], [center.x, center.y])
        x = point[0];
        y = point[1];
      } else {
        var size = parseFloat(nodeDOM.getAttribute('r'));
        var angle = -Math.PI/4;
        x = center.x + size * Math.cos(angle);
        y = center.y + size * Math.sin(angle);
      }
    } else {
      x = center.x;
      y = center.y;
    }
    // var offsetCoords = offsetLine( [center.x, center.y],[x, y],ndtvProperties.graph.tooltipOffset)
    // x = offsetCoords[0];
    // y = offsetCoords[1];

    var left = (x*ctm.a) + ctm.e - n3.offset.left - $(window).scrollLeft() +1;
    var bottom = n3.height -(y*ctm.d)-ctm.f + n3.offset.top - $(window).scrollTop() +1;
    return [left, bottom];
  }

  /** hide the tooltip and unset the selected global */
  n3.prototype.hideTooltip = function() {
    var n3 = this;
    n3.selected = null;
    n3.tooltip.style('display', 'none');
  }

  /** stop the current animation cycle
  * @param {boolean} - if true, immediate halt all active transitions (otherwise, let animation continue to next time slice)
  */
  n3.prototype.endAnimation = function(noHalt){
    var n3 = this;
    clearTimeout(n3.animate);
    if (! noHalt) {
      n3.domTarget.selectAll('.node, .edge, .label, .d3-slider-handle').transition().duration(0)
    }
  }

  /** step the animation by one time slice
  * @param {boolean} - if true, go to previous time slice, else go forward
  */
  n3.prototype.stepAnimation = function(reverse) {
    var n3 = this;

    n3.endAnimation(1);
    if (reverse) {
      n3.animateGraph(n3.currTime-1, n3.currTime-1); 
    } else {
      n3.animateGraph(n3.currTime+1, n3.currTime+1); 
    }
  }

  /** animate the graph over all time slices, starting at current slice
  * @param {boolean} - if true, animate slices backwards until beginning of time index, other play until end
  */
  n3.prototype.playAnimation = function(reverse) {
    var n3 = this;

    n3.endAnimation(1);
    if (reverse) { 
      n3.animateGraph(n3.currTime-1, 0); 
    } else {
      n3.animateGraph(n3.currTime+1); 
    }
  }
  return n3;
}));
