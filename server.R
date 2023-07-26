#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("r.describe.R")
source("charts.R")


# Define server logic required to draw a histogram
function(input, output, session) {
  df <- to.factor(mtcars)

  stat_react <- shiny::reactive({
    switch(
      input$stat_input,
      "data frame" = df,
      "describe" = t(describe(df)),
      "str" = dtype.df(df)
    )
  })

  plot_react <- shiny::reactive({
    # Get plot type id
    plot_type_input <- input$plot_type_input

    # X input id
    cat_feature_id <- input$cat_feature_id

    # X input id
    continuous_id <- input$continuous_id

    # X input id
    y <- input$y

    # X input id
    hue <- input$hue

    switch(
      plot_type_input,
      "bar"=bar_chart(df, cat_feature_id),
      "correlation bar"=bar_chart(df, cat_feature_id, y=y),
      "points"=points_chart(df, continuous_id, y=y, hue=hue)
    )
  })

  output$stat_output <- DT::renderDT({
    stat_react()
  })

  output$plot_output <- shiny::renderPlot({
    plot_react()
  })




}
