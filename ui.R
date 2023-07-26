# Bless the name of the lord God

# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(tidyverse)


get_all_categorical <- function (df, threshold=20){

  # A list to store feature
  feature <- c()

  for (col in colnames(df)){
    if (length(unique(df[[col]])) <= threshold){
      feature <- append(feature, col)
    }
  }
  return(feature)
}

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Motor Trend Car Road Tests"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          tabsetPanel(

            # Data explorer tab
              tabPanel(
              title = "Data description",
              h2(strong("Dataset Desciption", style = "font-size: 16px")),
              h3(strong("About The Dataset", style = "font-size: 14px")),
              p("The data was extracted from the 1974 _Motor Trend_ US magazine,
         and comprises fuel consumption and 10 aspects of automobile design
         and performance for 32 automobiles (1973-74 models)"),

              h3(strong("Feature descriptions", style = "font-size: 14px")),
              p("A data frame with 32 observations on 11 (numeric) variables"),
              div(
                p(span("mpg:", style = "font-weight: bold"), "Miles/(US) gallon"),
                p(span("cyl:", style = "font-weight: bold"), "Number of cylinders"),
                p(span("disp:", style = "font-weight: bold"), "Displacement (cu.in.)"),
                p(span("hp:", style = "font-weight: bold"), "Gross horsepower"),
                p(span("drat:", style = "font-weight: bold"), "Rear axle ratio"),
                p(span("wt:", style = "font-weight: bold"), "Weight (1000 lbs)"),
                p(span("qsec:", style = "font-weight: bold"), "1/4 mile time"),
                p(span("vs:", style = "font-weight: bold"), "Engine (0 = V-shaped, 1 = straight)"),
                p(span("am:", style = "font-weight: bold"), "Transmission (0 = automatic, 1 = manual)"),
                p(span("gear:", style = "font-weight: bold"), "Number of forward gears"),
                p(span("carb:", style = "font-weight: bold"), "Number of carburetors"),
              )
            ),

            tabPanel(
              title = "Data exploratory",
              h2(strong("Data Exploratory", style = "font-size: 16px")),
              h3("Statistic"),
              selectInput(
                inputId = "stat_input",
                choices = c("data frame", "describe", "str"),
                label = "Stat"
              ),
              h3("Visualization"),
              selectInput(
                inputId = "plot_type_input",
                choices = c("bar", "correlation bar", "points", "histogram", "boxplot"),
                label = "Chart Type"
              ),
              selectInput(
                inputId = "cat_feature_id",
                choices = get_all_categorical(mtcars),
                label = "Categroical Feature"
              ),
              selectInput(
                inputId = "continuous_id",
                choices = colnames(mtcars %>% select(-get_all_categorical(mtcars))),
                label = "Continuous Feature"
              ),
              selectInput(
                inputId = "y",
                choices = colnames(mtcars %>% select(-get_all_categorical(mtcars))),
                label = "Y"
              ),
              selectInput(
                inputId = "hue",
                choices = colnames(mtcars),
                label = "Hue"
              )
            )
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel(
              title = "Statistic",
              DTOutput(outputId = "stat_output")
            ),
            tabPanel(
              title = "Visualization",
              shiny::plotOutput("plot_output")
            )

          )
        )
    )
)
