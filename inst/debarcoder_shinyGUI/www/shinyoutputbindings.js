Shiny.addCustomMessageHandler("plot_loading",
    value => {
        document.getElementById("debarcoderui_plot_data_spinner").style.visibility = "visible"
        document.getElementById("debarcoderui_plot_data_text").style.visibility = "hidden"
    }
)

$(document).on('shiny:value', event => {

    if(event.target.id == "debarcoderui_plot1" || event.target.id == "debarcoderui_plot2") {

        document.getElementById("debarcoderui_plot_data_spinner").style.visibility = "hidden"
        document.getElementById("debarcoderui_plot_data_text").style.visibility = "visible"
    }
})