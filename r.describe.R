# Bless Be The Lord God

library(tidyverse)
library(foreach)

n.unique <- function (x){
  n_unique <- length(unique(x))
  ifelse(
    is.factor(x),
    n_unique,
    0
  )
}

min.num <- function (x){
  ifelse(
    !is.factor(x),
    round(min(x), 2),
    0
  )
}


lower.threshold <- function (x){
  ifelse(
    !is.factor(x),
    round(quantile(x, 0.25),2),
    0
  )
}


upper.threshold <- function (x){
  ifelse(
    !is.factor(x),
    round(quantile(x, 0.75),2),
    0
  )
}

median.num <- function (x){
  ifelse(
    !is.factor(x),
    round(median(x), 2),
    0
  )
}

mean.num <- function (x){
  ifelse(
    !is.factor(x),
    round(mean(x), 2),
    0
  )
}


max.num <- function (x){
  ifelse(
    !is.factor(x),
    round(max(x), 2),
    0
  )
}




column_describe <- function(data, column){
  c(
    min.num(data[[column]]),
    lower.threshold(data[[column]]),
    median.num(data[[column]]),
    mean.num(data[[column]]),
    upper.threshold(data[[column]]),
    max.num(data[[column]]),
    n.unique(data[[column]])
  )
}

describe <- function(data){
  # Get the column names
  columns <- colnames(data)

  empty_list <- list()

  for (col in columns){
    empty_list[[col]] <- column_describe(data, col)
  }

  # Create the data frame
  df <- data.frame(empty_list)

  # Rename the row names
  rownames(df) <- c("min", "25%", "median", "mean", "75%", "max", "feq")

  return(df)
}

dtype.df <- function (df){
  # Get the column names of the df
  column_names <- colnames(df)

  classes <- foreach(column_name=column_names) %do% {
    class(df[[column_name]])
  }

  return(data.frame(columns = column_names, dtypes = unlist(classes)))
}

to.factor <- function (df, threshold = 10){
  # A list to store feature
  feature.list <- list()

  for (col in colnames(df)){
    if (length(unique(df[[col]])) <= threshold){
      feature.list[[col]] <- as.factor(df[[col]])
    }
    else{
      feature.list[[col]] <- df[[col]]
    }
  }

  # Construct the data frame.
  dataframe <- data.frame(feature.list)

  # Change the row names
  rownames(dataframe) <- rownames(df)

  return(dataframe)
}