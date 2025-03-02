#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dslabs)

# Load dataset
data("olive")

# Convert region to a factor for coloring
olive$region <- as.factor(olive$region)

ui <- fluidPage(
  
  titlePanel("Olive Oil Fatty Acids Clustering"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('xcol', 'X Variable', choices = names(olive)[3:10]),  # Select numerical columns
      selectInput('ycol', 'Y Variable', choices = names(olive)[3:10], 
                  selected = names(olive)[4]),
      numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
    ),
    
    mainPanel(
      plotOutput('plot1'),
      tableOutput('cluster_summary')  # Show how clusters map to regions
    )
  )
)

server <- function(input, output, session) {
  
  selectedData <- reactive({
    olive[, c(input$xcol, input$ycol)]  # Only numeric columns
  })
  
  clusters <- reactive({
    kmeans(selectedData(), centers = input$clusters)
  })
  
  output$plot1 <- renderPlot({
    # Create a color palette that includes blue and other colors for the clusters
    cluster_colors <- rainbow(input$clusters)  # Generate a palette of colors (including blue)
    
    # Assign colors to the data points
    cluster_color_assignment <- cluster_colors[clusters()$cluster]
    
    plot(selectedData(),
         col = cluster_color_assignment,  # Color by k-means cluster with new palette
         pch = 20, cex = 2, 
         xlab = input$xcol, ylab = input$ycol, 
         main = "K-Means Clustering of Olive Oil Composition")
    
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4, col = "black")  # Cluster centers in black
    
    # Legend for clusters
    legend("topright", legend = paste("Cluster", 1:input$clusters),
           col = cluster_colors, pch = 20)
  })
  
  # Table to compare clusters with regions
  output$cluster_summary <- renderTable({
    data.frame(Cluster = clusters()$cluster, Region = olive$region) |>
      dplyr::count(Cluster, Region) |>  # Count how many samples per cluster-region
      tidyr::spread(Cluster, n, fill = 0)  # Make it a table
  })
}

shinyApp(ui = ui, server = server)

library(testthat)

test_that("perform_kmeans runs correctly", {
  data("olive", package = "dslabs")
  result <- perform_kmeans(olive, "palmitic", "stearic", k = 3)
  
  expect_type(result, "list")
  expect_true("cluster" %in% names(result))
  expect_true("centers" %in% names(result))
  expect_equal(length(result$cluster), nrow(olive))
})

test_that("plot_kmeans does not throw an error", {
  data("olive", package = "dslabs")
  result <- perform_kmeans(olive, "palmitic", "stearic", k = 3)
  
  expect_silent(plot_kmeans(olive, result, "palmitic", "stearic"))
})

shinyApp(ui = ui, server = server)
