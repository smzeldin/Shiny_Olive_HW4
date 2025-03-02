#' Perform K-Means Clustering
#
#'
#' This function performs K-Means clustering on two specified columns from the olive dataset.
#' @export
#' @param data The dataset to perform clustering on (data frame).
#' @param x The x variable for clustering (a column name in the dataset).
#' @param y The y variable for clustering (a column name in the dataset).
#' @param k The number of clusters (default is 3).
#' @return A list with cluster assignments and cluster centers.
#' @export
perform_kmeans <- function(data, x, y, k = 3) {
  selectedData <- data[, c(x, y)]  # Select the two columns
  kmeans_result <- kmeans(selectedData, centers = k)
  return(kmeans_result)
}

