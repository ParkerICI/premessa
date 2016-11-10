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
    ctx.fillStyle = d.color;
    ctx.arc(xScale(d.x), yScale(d.y), r, 0, 2 * Math.PI);
    ctx.fill();
};

  var doPlot = function(ctx, plotData, xScale, yScale) {
    var offScreen = document.createElement('canvas');
    var r = 2;
    var d = r * 2;
    //Spacing between the circles to avoid anti-aliasing problems
    var pad = 10;
    colors = ['black', 'red'];

    offScreen.width = (d + pad) * colors.length;
    offScreen.height = d;

    var offCtx = offScreen.getContext('2d');

    colors.forEach(function(c, i) {
        offCtx.fillStyle = c;
        offCtx.beginPath();
        offCtx.arc((i * (d + pad)) + r, r, r, 0, 2 * Math.PI);
        offCtx.closePath();
        offCtx.fill();
        
    });

    plotData.forEach(function(datum, i, a) {
        ctx.drawImage(offScreen, datum.color * (d + pad), 0, d, d, xScale(datum.x) - r, yScale(datum.y) - r, d, d);
    });

}

var gatePlot = new Shiny.OutputBinding();
$.extend(gatePlot, {
    find: function(scope) {
        var ret = $(scope).find('.shiny-gateplot');
        return(ret);
    },

    renderValue: function(el, data) {
        console.log(data);
        data.color = data.color.map(function(d) { return(d == "black" ? 0 : 1); });
        var plotData = convertDataForD3({x: data.x, y: data.y, color: data.color});
        //top; right; bottom; left
        var margins = [20, 20, 20, 20];
        var width = 300 - margins[1] - margins[3];
        var height = 250 - margins[0] - margins[2];

        var canvas = d3.select(el)
                .select("canvas")
                .attr("width", width + margins[1] + margins[3])
                .attr("height", height + margins[0] + margins[2])
                .style("padding", margins.join("px ") + "px");

        var ctx = canvas.node().getContext('2d');
        
        //Clear existing graph
        d3.select(el).select("svg").remove();
        ctx.clearRect(0, 0, canvas.node().width, canvas.node().height);

        var svg = d3.select(el)
            .append("svg")
            .attr("width", width + margins[1] + margins[3])
            .attr("height", height + margins[0] + margins[2])
            .append("svg:g")
            .attr("transform", "translate(" + margins[3] + "," + margins[0] + ")");
        
        
        var xScale = d3.scaleLinear()
            .range([0, width])
            .domain(d3.extent(plotData, function(d) { return(d.x); })).nice();
        
        var yScale = d3.scaleLinear()
            .range([height, 0])
            .domain(d3.extent(plotData, function(d) { return(d.y); })).nice();

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
            
            var shinyData = {
                xLim: xLim,
                yLim: yLim,
                xAxisName: data.xAxisName,
                yAxisName: data.yAxisName,
                file: data.file
            }

            console.log(shinyData);
            Shiny.onInputChange("normalizerui_gate_selected", shinyData);
        }

        var brush = d3.brush();



        var brushG = svg.append("g")
            .attr("class", "brush")
            .call(brush);
        
        var gates = data.channelGates;
        brush.move(brushG, [[xScale(gates.x[0]), yScale(gates.y[1])], 
                            [xScale(gates.x[1]), yScale(gates.y[0])]]);
        brush.on("end", brushed);

        xAxisG.call(xAxis);
        yAxisG.call(yAxis);


      
        doPlot(ctx, plotData, xScale, yScale);

    }

});

Shiny.outputBindings.register(gatePlot, 'gateplot');