library(tidyverse)
source("r.describe.R")


bar_chart <- function (data, x, y=NULL){
  if (is.null(y)){
    ggplot2::ggplot(
      data,
      ggplot2::aes(x=.data[[x]],
                   fill=.data[[x]])) +
      ggplot2::geom_bar()
  }
  else{
    ggplot2::ggplot(data,
      ggplot2::aes(
        x=.data[[x]],
        y=.data[[y]],
        fill=.data[[x]]
      )) +
      ggplot2::geom_col()
  }

}

points_chart <- function (data, x, y, hue){
  ggplot2::ggplot(data,
      ggplot2::aes(
        x=.data[[x]],
        y=.data[[y]],
        color=.data[[hue]]
      )) +
      ggplot2::geom_point()
}