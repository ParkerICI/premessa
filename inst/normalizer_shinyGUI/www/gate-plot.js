



class GatePlotOutputBinding extends Shiny.OutputBinding {
    find(scope) {
        let ret = $(scope).find('.shiny-gateplot')
        return(ret);
    }
    
    renderValue(el, data) {
        data.color = data.color.map(d => d == "black" ? 0 : 1)
        let plotData = GatePlotOutputBinding.convertDataForD3({x: data.x, y: data.y, color: data.color})
        GatePlotOutputBinding.draw(data, plotData, el)
        
    }

    static draw(data, plotData, domEl) {
        //top; right; bottom; left
        let margins = [50, 20, 50, 30]

        let elWidth = domEl.clientWidth
        let elHeight = (window.innerHeight - domEl.offsetTop) * 0.3

        let width = elWidth - margins[1] - margins[3]
        let height = elHeight - margins[0] - margins[2]

        let canvas = d3.select(domEl)
                .select("canvas")
                .attr("width", width + margins[1] + margins[3])
                .attr("height", height + margins[0] + margins[2])
                .style("padding", margins.join("px ") + "px")

        let ctx = canvas.node().getContext('2d')
        
        //Clear existing graph
        d3.select(domEl).select("svg").remove()
        ctx.clearRect(0, 0, canvas.node().width, canvas.node().height)

        let svg = d3.select(domEl)
            .append("svg")
            .attr("width", width + margins[1] + margins[3])
            .attr("height", height + margins[0] + margins[2])
            .append("svg:g")
            .attr("transform", "translate(" + margins[3] + "," + margins[0] + ")")
        
        let xExtent = d3.extent(plotData, d => d.x)
        let xScale = d3.scaleLinear()
            .range([0, width])
            .domain([xExtent[0] - 0.5, xExtent[1] + 0.5])
        
        let yExtent = d3.extent(plotData, d => d.y)
        let yScale = d3.scaleLinear()
            .range([height, 0])
            .domain([yExtent[0] - 0.5, yExtent[1] + 0.5])

        let xAxisG = svg.append("g")
                    .attr("class", "xAxis")
                    .attr("transform", "translate(0, "  + height + ")")
        let yAxisG = svg.append("g")
                    .attr("class", "yAxis")

        let xAxis = d3.axisBottom(xScale).ticks(7)
        let yAxis = d3.axisLeft(yScale).ticks(7)

        let brushed = () => {
            let sel = d3.event.selection;
            let xLim = [sel[0][0], sel[1][0]].map(xScale.invert)
            //This is necessary because the range of the y scale is inverted
            let yLim = [sel[1][1], sel[0][1]].map(yScale.invert)
            
            let shinyData = {
                xLim: xLim,
                yLim: yLim,
                xAxisName: data.xAxisName,
                yAxisName: data.yAxisName,
                file: data.file
            }

            console.log(shinyData);
            Shiny.onInputChange("normalizerui_gate_selected", shinyData)
        }

        let brush = d3.brush()



        let brushG = svg.append("g")
            .attr("class", "brush")
            .call(brush)
        
        let gates = data.channelGates
        brush.move(brushG, [[xScale(gates.x[0]), yScale(gates.y[1])], 
                            [xScale(gates.x[1]), yScale(gates.y[0])]])
        brush.on("end", brushed)

        xAxisG.call(xAxis)
        yAxisG.call(yAxis)
        
        svg.append("text")             
            .attr("transform", "translate(" + (width / 2) + " ," + 
                            (height + margins[0]) + ")")
            .style("text-anchor", "middle")
            .text(data.xAxisName)
        
        svg.append("text")             
            .attr("transform", "translate(" + -(margins[3] / 1.5) + ", " + (height / 2)+ ") rotate(-90)")
            .style("text-anchor", "middle")
            .text(data.yAxisName);

        GatePlotOutputBinding.doPlot(ctx, plotData, xScale, yScale);

    }

    static doPlot(ctx, plotData, xScale, yScale) {
        let offScreen = document.createElement('canvas')
        let r = 2
        let d = r * 2
        //Spacing between the circles to avoid anti-aliasing problems
        let pad = 10
        let colors = ['black', 'red']
    
        offScreen.width = (d + pad) * colors.length
        offScreen.height = d
    
        let offCtx = offScreen.getContext('2d')
    
        colors.forEach((c, i) => {
            offCtx.fillStyle = c
            offCtx.beginPath()
            offCtx.arc((i * (d + pad)) + r, r, r, 0, 2 * Math.PI)
            offCtx.closePath()
            offCtx.fill()
            
        })
    
        plotData.forEach((datum, i, a) => {
            ctx.drawImage(offScreen, datum.color * (d + pad), 0, d, d, xScale(datum.x) - r, yScale(datum.y) - r, d, d)
        })
    
    }


    static convertDataForD3(obj) {
        let res = []
        let len = obj[Object.keys(obj)[0]].length
        for(let i = 0; i < len; i++) {
            let temp = {}
            for(let key in obj)
                temp[key] = obj[key][i]
            res.push(temp)
        }
        return(res)
    }
    
}

let gatePlotOutputBinding = new GatePlotOutputBinding()

Shiny.outputBindings.register(gatePlotOutputBinding, 'gateplot')

/*
window.onresize = () => {
    let plots = document.getElementsByClassName("shiny-gateplot")
    let event = new Event("resize")

    for(let i = 0; i < plots.length; i++) {
        let el = plots[i]
        el.dispatchEvent(event)
    }
}*/