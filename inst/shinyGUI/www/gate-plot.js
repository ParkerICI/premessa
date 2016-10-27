console.log("GINOPINO");

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
        console.log(scope);
        var ret = $(scope).find('.shiny-gateplot');
        console.log(ret);
        return $(scope).find('.shiny-gateplot');
    },

    renderValue: function(el, data) {
        data = convertDataForD3(data);
        console.log(data);

        var canvas = d3.select(el).select("canvas")
        var width = 800;
        var height = 600;

        canvas.attr("width", width)
            .attr("height", height);
        
        var ctx = canvas.node().getContext('2d');
        
        var xScale = d3.scaleLinear()
            .range([0, width])
            .domain(d3.extent(data, function(d) { return(d.x); }));
        
        var yScale = d3.scaleLinear()
            .range([0, height])
            .domain(d3.extent(data, function(d) { return(d.y); }));

        data.forEach(function(d, i, a) {
            paintPoint(ctx, d, xScale, yScale);
        });

    }

});

Shiny.outputBindings.register(gatePlot, 'gateplot');