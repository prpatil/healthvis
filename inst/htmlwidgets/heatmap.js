HTMLWidgets.widget({

	name: 'heatmap', 
	
	type: 'output',
	
	initialize: function(el, width, height){
		
		var buffer = width/13;	
		
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
		this.buffer=this.width/13;
		this.formdata = [];
		this.input = null;
		this.data = null;
		this.rownames = null;
		this.colnames = null;
		this.ord_input = null;
		this.ordnames = null;
		this.ordering = null;
		this.colors = null;
		this.nsubj = null;
		this.nobs = null;
		this.ncov = null;
		this.medians = null;
		this.heatcol = null;
		this.heatrow = null;
		this.init_rank = null;
		
		this.input = x.data;
		this.rownames = x.settings.rownames;
		this.colnames = x.settings.colnames;
		this.ord_input = x.settings.ordering;
		this.ordnames = x.settings.ordnames;
		this.colors = x.settings.colors;
		this.nsubj = x.settings.nsubj;
		this.nobs = x.settings.nobs;
		this.ncov = x.settings.ncov;
		this.medians = x.settings.medians; // Median of each column

		this.data = x.data;
		this.ordering = new Array(this.ncov);
		this.heatcol = new Array()
		
		console.log(this.data);

		this.boxw = this.width/this.data.length;
		this.boxh = this.height/this.data[0].length;

		var x=this.buffer;
		var curScale = null;
		
		// Get positions for boxes as well as colors (BY COLUMN)
		for(i=0; i < this.nobs; i++){
			curScale = this.colorize(this.data[i], this.medians[i], this.colors);
			this.heatcol.push(new Array());
			for(j=0; j < this.nsubj; j++){
				this.heatcol[i].push({
					xpos: x,
					ypos: j,
					color: curScale(this.data[i][j]),
				});
			}
			x+=this.boxw;
		}
	
		// Now make it row-wise so we can move rows - we will use this!

		this.heatrow = new Array(this.nsubj);
		for(i=0; i < this.nsubj; i++){
			this.heatrow[i] = new Array(this.nobs);
			for(j=0; j < this.nobs; j++){
				this.heatrow[i][j] = this.heatcol[j][i];	
			}
		}

		// Make 2-D associative array of subjects by covariates (for reordering)
		for(i=0; i < this.ordnames.length; i++){
			this.ordering[this.ordnames[i]] = new Array(this.ncov);
			for(j=0; j < this.nsubj; j++){
				// -1 so that the indices match up (R starts at 1)
				//this.ordering[this.ordnames[i]][j] = this.ord_input[i*this.nsubj+j]-1;	
				this.ordering[this.ordnames[i]][j] = this.ord_input[j][i]-1;
			}
		}

		var y = this.buffer;

		// Creat an initial ranking for the unsorted data
		this.init_rank = new Array(this.nsubj);
		this.true_pos = new Array(this.nsubj);
		for(i=0; i < this.nsubj; i++){
			this.init_rank[i] = i;
			this.true_pos[i] = y;
			y+=this.boxh;
		}
		
		// now visualize
		
		var cn = this.colnames;
		var rn = this.rownames;
		var buff = this.buffer;
		var boxw = this.boxw;
		var boxh = this.boxh;

		// These are rough - change when we have a better way of setting plot dimensions
		var roff = this.nsubj/10;
		var coff = this.nobs/2;

		var ymap = d3.scale.ordinal().domain(this.init_rank).range(this.true_pos);
		var rmap = d3.scale.ordinal().domain(this.init_rank).range(rn);

		this.col = instance.grid.selectAll(".column")
				   .data(this.heatcol)
				   .enter().append("svg:g")
				   .attr("class", "column");

		this.row = instance.grid.selectAll(".row")
					.data(this.heatrow)
					.enter().append("svg:g")
					.attr("class", "row");

		var width = this.width;
		// Change the 2.5 and 20 - they are rough and hardcoded
		this.col.append("text")
			.attr("x", function(d,i){return i*boxw+buff+boxw/2.5;})
			.attr("y", this.buffer-width/65)
			.attr("dy", ".7em")
			.attr("text-anchor", "start")
			.attr("transform", function(d,i){return "rotate(-90," + (i*boxw+buff+boxw/2.5) + "," + (buff-width/65) +")";})
			.text(function(d,i){return cn[i];})
			.style("font-size", 8+"px");


		this.rtext = this.row.append("text")
			.attr("x", this.buffer-width/65)
			.attr("y", function(d,i){return i*boxh+buff+boxh/5;})
			.attr("dy", ".7em")
			.attr("text-anchor", "end")
			.text(function(d,i){return rmap(i);})
			.style("font-size", 8+"px");


		this.cells = this.row.selectAll(".cell")
					.data(function(d){return d;})
					.enter().append("svg:rect")
					.attr("class", "cell")
					.attr("x", function(d){return d.xpos;})
					.attr("y", function(d){return ymap(d.ypos);})
					.attr("width", this.boxw)
					.attr("height", this.boxh)
					.style("fill", function(d){return d.color;})
					
		// Code to update
		this.instance = instance;
		var _this = this;
		
		Shiny.addCustomMessageHandler("handler", function(formdata){	
			// Always take [0] because there is only one input
			var val = formdata[0];
			var ymap_new = null;
			var rnames = _this.rownames;
			var cur_ord = null;
			
			// Makes new ordinal scales based on selection. In the future, make scales for
			// all options first, then keep them in an assoc array and just choose!
			if(val == "None"){
				cur_ord = _this.init_rank;
				ymap_new = d3.scale.ordinal().domain(_this.init_rank).range(_this.true_pos);
			} else {
				cur_ord = _this.ordering[val];
				ymap_new = d3.scale.ordinal().domain(cur_ord).range(_this.true_pos);
			}

			// Remaps the row position
			//_this.instance.grid.transition().duration(2000).selectAll(".row")
			//	.selectAll(".cell").attr("y", function(d){return ymap_new(d.ypos);});
			
			_this.instance.grid.transition().duration(2000).selectAll(".cell").attr("y", function(d){return ymap_new(d.ypos);});

			// Remaps row labels
			_this.rtext.transition().delay(1800).text(function(d,i){return rnames[cur_ord[i]];});
		});
	
	},
	
	// This function will take a set of 3 "increasing" colors and
	// return a color scale that fills in intensities between the 
	// colors. For use in turning each column of the observation
	// matrix into a heatmap.
	
	colorize: function(v, median, colors){

		var colorScale = d3.scale.linear()
			.domain([Math.min.apply(Math, v), median, Math.max.apply(Math, v)])
			.range([colors[0], colors[1], colors[2]]);

		return(colorScale)
	}

});

/*
function HealthvisHeatmap() {	
    this.width=650;
    this.height=450;
    this.buffer=this.width/13;
    this.formdata = [];
    this.input = null;
    this.data = null;
    this.rownames = null;
    this.colnames = null;
    this.ord_input = null;
    this.ordnames = null;
    this.ordering = null;
    this.colors = null;
    this.nsubj = null;
    this.nobs = null;
    this.ncov = null;
    this.medians = null;
    this.heatcol = null;
    this.heatrow = null;
    this.init_rank = null;

    this.init = function(elementId, d3Params) {
	
		var dimensions = healthvis.getDimensions(this.width, this.height);
				
		this.width = dimensions.width;
		this.height = dimensions.height;
		this.buffer=this.width/13;	
		
        this.grid = d3.select('#main')
            .append('svg')
            .attr('width', this.width+this.buffer)
            .attr('height', this.height+this.buffer)
	    .attr('class', 'chart');

	this.input = d3Params.data;
	this.rownames = d3Params.rownames;
	this.colnames = d3Params.colnames;
	this.ord_input = d3Params.ordering;
	this.ordnames = d3Params.ordnames;
	this.colors = d3Params.colors;
	this.nsubj = d3Params.nsubj;
	this.nobs = d3Params.nobs;
	this.ncov = d3Params.ncov;
	this.medians = d3Params.medians; // Median of each column


	this.data = new Array(this.nobs);
	this.ordering = new Array(this.ncov);
	this.heatcol = new Array()

	// Make 2-D array of subjects by observations (for heatmap)
	for(var i=0; i < this.nobs; i++){
		this.data[i] = new Array(this.nsubj);
		for(var j=0; j < this.nsubj; j++){
			this.data[i][j] = this.input[i*this.nsubj+j];	
		}
	}

	// Make a 2-D array of heat colors, xpos, ypos

	this.boxw = this.width/this.data.length;
	this.boxh = this.height/this.data[0].length;


	var x=this.buffer;
	
	var curScale = null;

	// Get positions for boxes as well as colors (BY COLUMN)
	for(i=0; i < this.nobs; i++){
		curScale = colorize(this.data[i], this.medians[i], this.colors);
		this.heatcol.push(new Array());
		for(j=0; j < this.nsubj; j++){
			this.heatcol[i].push({
					    xpos: x,
			  		    ypos: j,
					    color: curScale(this.data[i][j]),
					    });
		}
		x+=this.boxw;
	}


	// Now make it row-wise so we can move rows - we will use this!

	this.heatrow = new Array(this.nsubj);
	for(i=0; i < this.nsubj; i++){
		this.heatrow[i] = new Array(this.nobs);
		for(j=0; j < this.nobs; j++){
			this.heatrow[i][j] = this.heatcol[j][i];	
		}
	}

	// Make 2-D associative array of subjects by covariates (for reordering)
	for(i=0; i < this.ordnames.length; i++){
		this.ordering[this.ordnames[i]] = new Array(this.ncov);
		for(j=0; j < this.nsubj; j++){
			// -1 so that the indices match up (R starts at 1)
			this.ordering[this.ordnames[i]][j] = this.ord_input[i*this.nsubj+j]-1;	
		}
	}	

	var y = this.buffer;

	// Creat an initial ranking for the unsorted data
	this.init_rank = new Array(this.nsubj);
	this.true_pos = new Array(this.nsubj);
	for(i=0; i < this.nsubj; i++){
		this.init_rank[i] = i;
		this.true_pos[i] = y;
		y+=this.boxh;
	}

     };

    this.visualize = function() {
	
	var cn = this.colnames;
	var rn = this.rownames;
	var buff = this.buffer;
	var boxw = this.boxw;
	var boxh = this.boxh;

	// These are rough - change when we have a better way of setting plot dimensions
	var roff = this.nsubj/10;
	var coff = this.nobs/2;

	var ymap = d3.scale.ordinal().domain(this.init_rank).range(this.true_pos);
	var rmap = d3.scale.ordinal().domain(this.init_rank).range(rn);

	this.col = this.grid.selectAll(".column")
			   .data(this.heatcol)
			   .enter().append("svg:g")
			   .attr("class", "column");

	this.row = this.grid.selectAll(".row")
			    .data(this.heatrow)
			    .enter().append("svg:g")
			    .attr("class", "row");

	var width = this.width;
	// Change the 2.5 and 20 - they are rough and hardcoded
	this.col.append("text")
		.attr("x", function(d,i){return i*boxw+buff+boxw/2.5;})
		.attr("y", this.buffer-width/65)
		.attr("dy", ".7em")
		.attr("text-anchor", "start")
		.attr("transform", function(d,i){return "rotate(-90," + (i*boxw+buff+boxw/2.5) + "," + (buff-width/65) +")";})
		.text(function(d,i){return cn[i];})
		.style("font-size", 8+"px");


	this.rtext = this.row.append("text")
		.attr("x", this.buffer-width/65)
		.attr("y", function(d,i){return i*boxh+buff+boxh/5;})
		.attr("dy", ".7em")
		.attr("text-anchor", "end")
		.text(function(d,i){return rmap(i);})
		.style("font-size", 8+"px");


	this.cells = this.row.selectAll(".cell")
			    .data(function(d){return d;})
			    .enter().append("svg:rect")
			    .attr("class", "cell")
			    .attr("x", function(d){return d.xpos;})
			    .attr("y", function(d){return ymap(d.ypos);})
			    .attr("width", this.boxw)
			    .attr("height", this.boxh)
			    .style("fill", function(d){return d.color;})
//			    .style("stroke", "#555"); // For box outlines, uncomment

    };


    this.update = function(formdata) {
	// Always take [0] because there is only one input
	var val = formdata[0].value;
	var ymap_new = null;
	var rnames = this.rownames;
	var cur_ord = null;
	
	// Makes new ordinal scales based on selection. In the future, make scales for
	// all options first, then keep them in an assoc array and just choose!
	if(val == "None"){
		cur_ord = this.init_rank;
		ymap_new = d3.scale.ordinal().domain(this.init_rank).range(this.true_pos);
	} else {
		cur_ord = this.ordering[val]
		ymap_new = d3.scale.ordinal().domain(cur_ord).range(this.true_pos);
	}

	var t = this.grid.transition().duration(2000);
	
	// Remaps the row position
	t.selectAll(".row")
		.selectAll(".cell").attr("y", function(d){return ymap_new(d.ypos);});

	// Remaps row labels
	this.rtext.transition().delay(1800).text(function(d,i){return rnames[cur_ord[i]];});
	
    };
}

healthvis.register(new HealthvisHeatmap());

*/