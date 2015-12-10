HTMLWidgets.widget({

	name: 'accuracy_table',
	
	type: 'output',
	
	initialize: function(el, width, height){
		var svg = d3.select(el)
            .append('svg:svg')
            .attr('width', width)
            .attr('height', height);
	
		return {
			svg: svg,
			width: width,
			height: height
		};

	},
	
	renderValue: function(el, x, instance){
		this.w = instance.width;
		this.h = instance.height;
		
		this.tmph = this.h*0.6;
		this.tmpw = this.w*0.9;
		this.padding = this.h/100;
		this.lg_font_size = this.h/10;
		this.md_font_size = this.h/20
		this.sm_font_size = this.h/35;
		
		this.colors = x.settings.colors;
		
		this.rdim = [];
		this.rectTxt = [];
		this.rectCornTxt = [];
		this.text = [];
		var rectTmp = ["True +", "False +", "False -", "True -"];
		var textTmp = ["There are 250 people with disease who test positive.", "There are 250 people without disease who test positive.", "There are 250 people with disease who test negative.", "There are 250 people without disease who test negative."];
			
		for(var i =0; i <2; i++){
			for(var j=0; j <2; j++){
				this.rdim.push({"x": this.padding+(this.tmpw/2)*j+this.padding*j, "y": this.padding+(this.tmph/2)*i+this.padding*i, "h": this.tmph/2, "w": this.tmpw/2, "col": this.colors[Math.abs(i-j)]});
				this.rectTxt.push({"x":this.padding+(this.tmpw/2)*j+(this.tmpw/5), "y": this.padding+(this.tmph/2)*i+(this.tmph/3.5), "text":"250", "font_size": this.lg_font_size});
				this.rectCornTxt.push({"x": 2*this.padding+(this.tmpw/2)*j+this.padding*j, "y": 4*this.padding+(this.tmph/2)*i+this.padding*i, "text":rectTmp[2*i+j], "font_size": this.sm_font_size});
				this.text.push({"x":this.padding, "y": 1.2*this.tmph+this.md_font_size*(2*i+j), "text":textTmp[(2*i+j)], "col": this.colors[Math.abs(i-j)]});
			}
		}
		
		this.rectVis = instance.svg.selectAll('rect')
					 .data(this.rdim).enter().append('rect')
					 .attr('x', function(d){return d.x;})
					 .attr('y', function(d){return d.y;})
					 .attr('height', function(d){return d.h;})
					 .attr('width', function(d){return d.w;})
					 .style('fill', function(d){return d.col;});

		this.rectTxtVis = instance.svg.selectAll()
						.data(this.rectTxt).enter().append('text')
						.attr('x', function(d){return d.x;})
						.attr('y', function(d){return d.y;})
						.attr('text-anchor', 'left')
						.style('fill', 'white')
						.style('font-size',function(d){return d.font_size+'px';})
						.text(function(d){return d.text;});
					 
		this.rectCornTxtVis = instance.svg.selectAll()
						.data(this.rectCornTxt).enter().append('text')
						.attr('x', function(d){return d.x;})
						.attr('y', function(d){return d.y;})
						.attr('text-anchor', 'left')
						.style('fill', 'white')
						.style('font-size',function(d){return d.font_size+'px';})
						.text(function(d){return d.text;});
					 
		this.textVis = instance.svg.selectAll()
						.data(this.text).enter().append('text')
						.attr('x', function(d){return d.x;})
						.attr('y', function(d){return d.y;})
						.attr('text-anchor', 'left')
						.style('fill', function(d){return d.col;})
						.style('font-size',this.md_font_size+'px')
						.text(function(d){return d.text;});
		
		// Update code
		var _this = this;
		Shiny.addCustomMessageHandler("handler", function(newcov){
			var res_arr = [newcov[0]*newcov[2], (1-newcov[0])*newcov[2], (1-newcov[1])*(1-newcov[2]), newcov[1]*(1-newcov[2])];
			var tot_area = _this.tmph*_this.tmpw;
			var l_wd = _this.tmpw*(res_arr[0]+res_arr[2]);
			var nh_tp = tot_area*res_arr[0]/l_wd;
			var nh_fn = tot_area*res_arr[1]/(_this.tmpw-l_wd);
			var nx_tp_txt = (l_wd+2*_this.padding)/2 + _this.padding;
			var ny_tp_txt = (nh_tp+2*_this.padding)/2 + _this.padding;
			var ny_fn_txt = (nh_fn+2*_this.padding)/2 + _this.padding;

			var new_dim = [];
			var tmp_txt = ["with disease who test positive.", "with disease who test negative.", "without disease who test positive.", "without disease who test negative."];
			var new_txt = [];
			var idx = null;

			for(var i = 0; i < 2; i++){
				for(var j = 0; j < 2; j++){
					idx = 2*i+j;
					_this.rdim[idx].x = _this.padding+j*(_this.padding+l_wd);
					_this.rdim[idx].y = _this.padding+i*_this.padding+Math.max(i-j, 0)*nh_tp + i*j*nh_fn;
					_this.rdim[idx].h = j*nh_fn + (1-j)*nh_tp+Math.max(i-j, 0)*(_this.tmph-2*nh_tp) + i*j*(_this.tmph-2*nh_fn);
					_this.rdim[idx].w = l_wd + j*(_this.tmpw-2*l_wd);
			
					_this.rectTxt[idx].x = _this.rdim[idx].x + _this.rdim[idx].w/3;
					_this.rectTxt[idx].y = _this.rdim[idx].y + _this.rdim[idx].h/2;
					_this.rectTxt[idx].text = Math.round(res_arr[idx]*1000);
					_this.rectTxt[idx].font_size = _this.font_choose(_this.rdim[idx].h, _this.rdim[idx].w, _this.sm_font_size);
		
					_this.rectCornTxt[idx].x = _this.rdim[idx].x + Math.max(_this.rdim[idx].w/50, 2*_this.padding);
					_this.rectCornTxt[idx].y = _this.rdim[idx].y + Math.max(_this.rdim[idx].h/10, 4*_this.padding);
					_this.rectCornTxt[idx].font_size = _this.name_choose(_this.rdim[idx].h, _this.sm_font_size);
		
					_this.text[idx].text = "There are "+Math.round(res_arr[idx]*1000)+" people "+tmp_txt[idx];
				}
			}

			_this.rectVis.transition()
						.attr("x", function(d){return d.x;})
						.attr("y", function(d){return d.y;})
						.attr("height", function(d){return d.h;})
						.attr("width", function(d){return d.w;});

			_this.rectTxtVis.transition()
						.attr("x", function(d){return d.x;})
						.attr("y", function(d){return d.y;})
						.attr("text-anchor", "midde")
						.style("font-size", function(d){return d.font_size+'px';})
						.text(function(d){return d.text;});
				 
			_this.rectCornTxtVis.transition()
							.attr("x", function(d){return d.x;})
							.attr("y", function(d){return d.y;})
							.style("font-size", function(d){return d.font_size+'px';})

				 
			_this.textVis.transition().text(function(d){return d.text;});	
		});
	},
	
	resize: function(el, width, height, instance){
		instance.svg.transition()
					.attr("height", height)
					.attr("width", width);
	},
	
	font_choose: function(l,w,fs){
        if(l < 10){
            return(0);
        } else {
            return(Math.max(0.001*l*w, fs));
        }
    },

    name_choose: function(l,fs){
        if(l < 10){
            return(0);
        } else {
            return(fs);
        }
    }

});