var convertDataForD3 = function(obj) {
    var res = [];
    var len = obj[Object.keys(obj)[0]].length;
    for(var i = 0; i < len; i++) {
        var temp = {};
        for(var key in obj)
            temp[key] = obj[key][i];
        res.push(temp);
    }
    return(res);
}

var paintPoint = function(ctx, d, xScale, yScale, r) {
    ctx.beginPath();
    ctx.arc(xScale(d.x), yScale(d.y), r, 0, 2 * Math.PI);
    ctx.fill();
};



var gatePlot = new Shiny.OutputBinding();
$.extend(gatePlot, {
    find: function(scope) {
        var ret = $(scope).find('.shiny-gateplot');
        return(ret);
    },

    renderValue: function(el, data) {
        var plotData = convertDataForD3({x: data.x, y: data.y});
        //top; left; bottom; right
        var margins = [15, 15, 15, 15];
        var width = 300 - margins[1] - margins[3];
        var height = 250 - margins[0] - margins[2];

        var canvas = d3.select(el)
                .select("canvas")
                .attr("width", width + margins[1] + margins[3])
                .attr("height", height + margins[0] + margins[2])
                .style("padding", margins.join("px ") + "px");

        var svg = d3.select(el)
            .append("svg")
            .attr("width", width + margins[1] + margins[3])
            .attr("height", height + margins[0] + margins[2])
            .append("svg:g")
            .attr("transform", "translate(" + margins[3] + "," + margins[0] + ")");
        
        var ctx = canvas.node().getContext('2d');
        
        var xScale = d3.scaleLinear()
            .range([0, width])
            .domain(d3.extent(plotData, function(d) { return(d.x); }));
        
        var yScale = d3.scaleLinear()
            .range([height, 0])
            .domain(d3.extent(plotData, function(d) { return(d.y); }));

        var xAxisG = svg.append("g")
                    .attr("class", "xAxis")
                    .attr("transform", "translate(0, "  + height + ")");
        var yAxisG = svg.append("g")
                    .attr("class", "yAxis");

        var xAxis = d3.axisBottom(xScale);
        var yAxis = d3.axisLeft(yScale);

        var brushed = function() {
            var sel = d3.event.selection;
            var xLim = [sel[0][0], sel[1][0]].map(xScale.invert);
            //This is necessary because the range of the y scale is inverted
            var yLim = [sel[1][1], sel[0][1]].map(yScale.invert);
            console.log(data);
            console.log(xLim);
            console.log(yLim);
            var shinyData = {
                xLim: xLim,
                yLim: yLim,
                xAxisName: data.xAxisName,
                yAxisName: data.yAxisName,
                file: data.file
            }
            Shiny.onInputChange("normalizerui_gate_selected", shinyData);
        }

        var brush = svg.append("g")
            .attr("class", "brush")
            .call(d3.brush().on("end", brushed));

        xAxisG.call(xAxis);
        yAxisG.call(yAxis);

        plotData.forEach(function(d, i, a) {
            paintPoint(ctx, d, xScale, yScale, 2);
        });

    }

});

Shiny.outputBindings.register(gatePlot, 'gateplot');