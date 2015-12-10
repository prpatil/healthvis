HTMLWidgets.widget({

	name: 'dist', 
	
	type: 'output',
	
	initialize: function(el, width, height){
	
		var buffer=width/10;
		var grid = d3.select(el)
		  .append('svg')
		  .attr('width', width+buffer)
		  .attr('height', height+buffer)
		  .attr('class', 'chart');

		return {
			grid: grid,
			width: width,
			height: height
		};

	},
	
	renderValue: function(el, x, instance){
	
		this.width=instance.width;
		this.height=instance.height;
		this.buffer=instance.width/10;
		this.formdata = [];

		this.distMats = null;
		this.permutations = null;
		this.clustNames = null;
		this.distNames = null;
		this.min = null;
		this.max = null;
		this.rownames = null;

		this.clustMethod = null;
		this.distMetric = null;
		
		this.rownames = x.settings.rownames;
		this.distMats = x.data;
		this.permutations = x.settings.permutations;
		this.clustNames = x.settings.clustNames;
		this.distNames = x.settings.distNames;
		this.min = x.settings.min;
		this.max = x.settings.max;
		this.colors = x.settings.colors;
		
		console.log(x.data);

		this.clustMethod = x.settings.defaultClust;
		this.distMetric = x.settings.defaultDist;

		this.margin = {
			top: this.buffer,
			left: this.buffer,
			bottom: 0,
			right: 0
		}
		
		instance.grid.select('.table').remove();
		instance.grid.select('.rows').remove();
		instance.grid.select('.cols').remove();
		var table = instance.grid.append('g').attr('class', 'table');
		var rows = instance.grid.append('g').attr('class', 'rows');
		var cols = instance.grid.append('g').attr('class', 'cols');

		var n = Math.floor(Math.sqrt(this.distMats[this.distMetric].length));
		var cellWidth = this.width / n;
		var cellHeight = this.height / n;
		var max = this.max[this.distMetric];
		var min = this.min[this.distMetric];
		var mean = (max + min) * 0.5;
		var margin = this.margin;
		var colorMap = this.colorize(min, mean, max, this.colors);
		var ordering = this.clustMethod == 'none' ? d3.range(0, n) : this.permutations[this.clustMethod][this.distMetric];
		table.selectAll('.cell')
			 .data(this.distMats[this.distMetric])
			 .enter()
			 .append('rect')
			 .attr('class', 'cell')
			 .attr('x', function(d, i) {
				var col = ordering[i % n];
				return col * cellWidth + margin.left;
			 })
			.attr('y', function(d, i) {
				var row = ordering[Math.floor(i / n)];
				return row * cellHeight + margin.top;
			})
			.attr('width', cellWidth)
			.attr('height', cellHeight)
			.style('fill', function(d) { return colorMap(d); });

		rows.selectAll('.row')
			.data(this.rownames)
			.enter()
			.append('text')
			.attr('class', 'row')
			.attr('x', margin.left - 3)
			.attr('y', function(d,i){
			var row = ordering[i];
				return row * cellHeight + margin.top;
			})
			.attr('dy', '.7em')
			.attr('text-anchor', 'end')
			.text(function(d,i){return d;})
			.style('font-size', 6+'px');

		cols.selectAll('.column')
			.data(this.rownames)
			.enter()
			.append('text')
			.attr('x', function(d,i){return margin.left + ordering[i]*cellWidth;})
			.attr('y', margin.top - 3)
			.attr('dy', '.7em')
			.attr('text-anchor', 'start')
			.attr('transform', function(d,i){return 'rotate(-90,' + (margin.left + ordering[i]*cellWidth) + "," + (margin.top - 3) +")";})
				.text(function(d) { return d; })
				.style('font-size', 6+'px');
			
		var _this = this;
		
		Shiny.addCustomMessageHandler("handler", function(formdata){	
		
			for (var i = 0; i < formdata.length; ++i) {
				switch(formdata[i].name) {
				case 'Clustering metric':
					_this.clustMethod = formdata[i].value;
				break;
				case 'Distance metrics':
					_this.distMetric = formdata[i].value;
				break;
				}
			}
			
			_this.renderValue(el, x, instance);
		
		});

	},
	
	colorize: function(min, median, max, colors){

		var colorScale = d3.scale.linear()
			 .domain([min, median, max])
			 .range([colors[0], colors[1], colors[2]]);

		return(colorScale)
	}	
});


/*

function colorize(min, median, max, colors){

	var colorScale = d3.scale.linear()
	     .domain([min, median, max])
	     .range([colors[0], colors[1], colors[2]]);

	return(colorScale)
}


function HealthvisDist() {

    this.width=550;
    this.height=550;
    this.buffer=50;
    this.formdata = [];

    this.grid = null;
    this.distMats = null;
    this.permutations = null;
    this.clustNames = null;
    this.distNames = null;
    this.min = null;
    this.max = null;
    this.rownames = null;

    this.clustMethod = null;
    this.distMetric = null;

    this.init = function(elementId, d3Params) {
        var dimensions = healthvis.getDimensions(this.width, this.height);

        this.width = dimensions.width;
        this.height = dimensions.height;

        this.grid = d3.select('#main')
          .append('svg')
          .attr('width', this.width+this.buffer)
          .attr('height', this.height+this.buffer)
          .attr('class', 'chart');

      this.rownames = d3Params.rownames;
      this.distMats = d3Params.distMats;
      this.permutations = d3Params.permutations;
      this.clustNames = d3Params.clustNames;
      this.distNames = d3Params.distNames;
      this.min = d3Params.min;
      this.max = d3Params.max;
      this.colors = d3Params.colors;

      this.clustMethod = d3Params.defaultClust;
      this.distMetric = d3Params.defaultDist;

      this.margin = {
        top: 50,
        left: 50,
        bottom: 0,
        right: 0
		}
     };

    this.visualize = function() {
      this.grid.select('.table').remove();
      this.grid.select('.rows').remove();
      this.grid.select('.cols').remove();
      var table = this.grid.append('g').attr('class', 'table');
      var rows = this.grid.append('g').attr('class', 'rows');
      var cols = this.grid.append('g').attr('class', 'cols');

      var n = Math.floor(Math.sqrt(this.distMats[this.distMetric].length));
      var cellWidth = this.width / n;
      var cellHeight = this.height / n;
      var max = this.max[this.distMetric];
      var min = this.min[this.distMetric];
      var mean = (max + min) * 0.5;
      var margin = this.margin;
      var colorMap = colorize(min, mean, max, this.colors);
      var ordering = this.clustMethod == 'none' ? d3.range(0, n) : this.permutations[this.clustMethod][this.distMetric];
      table.selectAll('.cell')
        .data(this.distMats[this.distMetric])
        .enter()
        .append('rect')
        .attr('class', 'cell')
        .attr('x', function(d, i) {
          var col = ordering[i % n];
          return col * cellWidth + margin.left;
        })
        .attr('y', function(d, i) {
          var row = ordering[Math.floor(i / n)];
          return row * cellHeight + margin.top;
        })
        .attr('width', cellWidth)
        .attr('height', cellHeight)
        .style('fill', function(d) { return colorMap(d); });

      rows.selectAll('.row')
        .data(this.rownames)
        .enter()
        .append('text')
        .attr('class', 'row')
        .attr('x', margin.left - 3)
        .attr('y', function(d,i){
          var row = ordering[i];
          return row * cellHeight + margin.top;
        })
        .attr('dy', '.7em')
        .attr('text-anchor', 'end')
        .text(function(d,i){return d;})
        .style('font-size', 6+'px');

      cols.selectAll('.column')
        .data(this.rownames)
        .enter()
        .append('text')
        .attr('x', function(d,i){return margin.left + ordering[i]*cellWidth;})
        .attr('y', margin.top - 3)
        .attr('dy', '.7em')
        .attr('text-anchor', 'start')
        .attr('transform', function(d,i){return 'rotate(-90,' + (margin.left + ordering[i]*cellWidth) + "," + (margin.top - 3) +")";})
        .text(function(d) { return d; })
        .style('font-size', 6+'px');
    };


    this.update = function(formdata) {
      for (var i = 0; i < formdata.length; ++i) {
        switch(formdata[i].name) {
          case 'Clustering metric':
            this.clustMethod = formdata[i].value;
            break;
          case 'Distance metrics':
            this.distMetric = formdata[i].value;
            break;
        }
      }
      this.visualize();
    };
}

healthvis.register(new HealthvisDist());*/