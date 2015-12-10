HTMLWidgets.widget({

	name: 'icon_array', 
	
	type: 'output',
	
	initialize: function(el, width, height){
	
		this.w = width;
		this.h = height;
		this.legw = this.w/3.5;
		this.legh = this.h/2.5;
	
        var grid = d3.select(el)
            .append('svg')
            .attr('width', this.w)
            .attr('height', this.h)
	    .attr('class', 'chart');
		
		return {
			grid: grid,
			width: width,
			height: height
		};


	},
	
	renderValue: function(el, x, instance){
	
		this.color_array = null;
		this.init_color = null;
		this.group_colors = null;
		this.group_names = null;
		this.formdata = [];
		
		this.w = instance.width;
		this.h = instance.height;
		this.legw = this.w/3.5;
		this.legh = this.h/2.5;
		this.y = d3.scale.linear().domain([0,100]).range([this.h*0.98,this.h*0.02]);
	
		this.flag = x.settings.obj_flag;
		this.color_array = x.settings.color_array;
		this.init_color = this.color_array[0];
		this.group_colors = x.settings.group_colors;
		this.group_names = x.settings.group_names;
		this.rows = x.settings.rows;
		this.cols = x.settings.cols;
		this.cats = x.settings.cats;
		this.vtype = x.settings.vtype;
		
		this.pcts = new Array(this.rows+1); // Add 1 for the baseline category (asumed to be this.pcts[0])

		this.covar = this.init_covar(this.cats);

		this.coefs = x.settings.coefs;

        this.data = [];

		// These things should all depend on plot dimensions
        var cellWidth = this.w/23;
        var cellHeight = this.h/13;
        var start = this.w/100; //70
        var xpos = start+this.w/28;
        var ypos = start;
        var xBuffer = cellWidth+start;
        var yBuffer = cellHeight+start;
        var count = 0;

		// Initialize an object giving position and color info
		// for each rect.
        for(var i=0; i < 10; i++){
			this.data.push(new Array());
			for(var j=0; j < 10; j++){
				this.data[i].push({
					width: cellWidth,
					height: cellHeight,
					x: xpos,
					y: ypos,
					color: this.color_array[count]
				});
				xpos += xBuffer;
				count += 1;
			}
           xpos = start+this.w/28;
           ypos += yBuffer;
        }
		
		// now visualize
		
		this.row = instance.grid.selectAll('.row')
                  .data(this.data)
                .enter().append('svg:g')
                  .attr('class', 'row');

		this.col = this.row.selectAll('.cell')
                 .data(function (d) { return d; })
                .enter().append('svg:rect')
                 .attr('class', 'cell')
                 .attr('x', function(d) { return d.x; })
                 .attr('y', function(d) { return d.y; })
                 .attr('width', function(d) { return d.width; })
                 .attr('height', function(d) { return d.height; })
		 .style('fill', function(d,i) { return d.color; });
	
		var yAxis = d3.svg.axis().scale(this.y).ticks(10).orient('left');
	
		instance.grid.append('svg:g')
                .attr('class', 'y axis')
                .attr('transform', 'translate('+this.w/24+',0)')
                .call(yAxis);

		// Add legend

		var legend = instance.grid.append('g')
		  .attr('class', 'legend')
		  .attr('x', this.w*(2/3))
		  .attr('y', this.h/4)
		  .attr('height', this.legh)
		  .attr('width', this.legw);
		  
		var lh = this.legh;

		legend.selectAll('rect')
			.data(this.group_colors).enter().append('rect')
			.attr('x', this.w*(2/3))
			.attr('y', function(d,i){return i*(lh/15)+lh/2.2;})
			.attr('width', this.legw/20)
			.attr('height', this.legh/20)
			.style('fill', function(d) { return d; });

		var group_names = this.group_names;

		legend.selectAll('text')
			.data(this.group_names).enter().append('text')
			.attr('x', this.w*(2/3)+(1/35)*this.w)
			.attr('y', function(d,i){return i*(lh/15)+lh/2;})
			.text(function(d) { return d; });
			
		// update code
		this.instance = instance;
		var _this = this;
		
		Shiny.addCustomMessageHandler("handler", function(formdata){	
		
			var nums = null;
			
			if(this.flag == 0){	
				for (var j=0; j<this.group_colors.length; j++) {
					_this.formdata[j] = parseFloat(formdata[j].value);
				}
				nums = _this.formdata;

			} else {
				_this.covar = _this.init_covar(_this.cats); // Reset everything
				
				// Set the covariates correctly
				for (var j=0; j<formdata.length; j++) {
					if(_this.vtype[j] == "factor"){
						_this.covar[(formdata[j].name+formdata[j].value)] = 1;
					} else {
						_this.covar[formdata[j].name] = parseFloat(formdata[j].value);
					}
				}
				
				_this.pcts = _this.update_covar(_this.covar, _this.coefs, _this.pcts.length, _this.rows, _this.cols);

				nums = _this.pcts;
			}

		// Reset colors based on new numbers for each covariate
			var sum=0;
			var col_tmp = _this.color_array;

			for(var k = 0; k < nums.length; k++){
				for(var m = sum; m < (sum + nums[k]); m++){
					col_tmp[m] = _this.group_colors[k];
				}
				sum += nums[k];		
			}

			if(sum > 100){
				alert("Inputs total >100...figure will update, but it may not be how you want.");
			}

			// Reverse, since we need to go from bottom to top
			_this.color_array = col_tmp.reverse();

			// Set new colors in data object
			var count=0;
			for(var i=0; i < 10; i++){
				for(var j=0; j <10; j++){
					_this.data[i][j].color = _this.color_array[count];
					count += 1;
				}
			}

			// Here is the transition: fill new colors
			_this.col.transition().style('fill', function(d) { return d.color; });
		});
	
	},

	// Create an initial set of covariates
	// where everything is set to 0 except
	// for the intercept.

	init_covar: function(cats){
		var covar = new Array(cats.length);

		for(var i = 0; i < cats.length; i++){	
			if(cats[i] == "(Intercept)"){
				covar[cats[i]] = 1;
			} else {
				covar[cats[i]] = 0;
			}
		}
		
		return covar;
	},

	// This is an implementation of the estimation of
	// response probabilities for a multinomial logit.
	// See Agresti's Categorical Data Analysis 7.1.3
	// (pg. 271 in the 2002 edition)

	update_covar: function(covar, coef, len, rows, cols){
		var tmp_pct = new Array(len);
		var pct_tot = 0;
		var tot = 0;
		var keys = Object.keys(covar);

		// Get the total for the denominator
		for(var i=0; i < rows; i++){
			var subtot1 = 0;
			var k = 0;
			for(var j=0; j < cols; j++){
				subtot1 = subtot1 + coef[i][j]*covar[keys[k]];			// used to be [j][i]
				k++;
			}
			tot = tot + Math.exp(subtot1);
		}

		// Get each numerator
		for(var i=0; i < rows; i++){
			var subtot2 = 0;
			var m = 0;
			for(var j=0; j < cols; j++){
				subtot2 = subtot2 + coef[i][j]*covar[keys[m]];	//used to be [j][i]
				m++;
			}

			tmp_pct[i+1] = Math.exp(subtot2)/(1+tot); // Skip the baseline for now
			pct_tot = pct_tot + tmp_pct[i+1];
		}

		tmp_pct[0] = 1 - pct_tot;

		for(var q=0; q < tmp_pct.length; q++){
			tmp_pct[q] = Math.round(tmp_pct[q]*100);
		}

		return tmp_pct;
	}
});
