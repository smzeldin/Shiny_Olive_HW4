
#' Plot K-Means Clustering Results
#'
#' This function will plot the outcome of K-Means clustering.
#' @export
#' @param data The dataset used for clustering.
#' @param kmeans_result The result from `perform_kmeans`.
#' @param x The x variable for clustering (a column name in the dataset).
#' @param y The y variable for clustering (a column name in the dataset).
#' @return A scatter plot showing the K-Means clusters.
#' @export
#' 
plot_kmeans <- function(data, kmeans_result, x, y) {
  cluster_colors <- as.factor(kmeans_result$cluster)
  
  plot(data[, c(x, y)], 
       col = cluster_colors, 
       pch = 20, cex = 2, 
       xlab = x, ylab = y, 
       main = "K-Means Clustering of Olive Oil Composition")
  
  points(kmeans_result$centers, pch = 4, cex = 4, lwd = 4) 
  
  legend("topright", legend = paste("Cluster", 1:length(unique(kmeans_result$cluster))),
         col = 1:length(unique(kmeans_result$cluster)), pch = 20)
}

