#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("San Miguel Island Northern Elephant Seal Population Dynamics"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         numericInput("rmax",
                     expression(r_max),
                     value = NA),
         numericInput("K",
                      "K",
                      value = NA)        
      ),
      
      # Show a plot of the data with the logistic curve
      mainPanel(
         plotOutput("overlayPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$overlayPlot <- renderPlot({
      
     
     
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}


# A logistic curve function
logistic_growth <- function(x, r, K, N0){
  
  K*N0*exp(r*x)/(K + N0*(exp(r*x) - 1))
  
}

# Read data from GitHub


# Add column for annual avg rate of increase (from paper)
nes$lambda <- with(nes, ifelse(Year < 1980, 1.197, ifelse(Year < 1991, 1.067, 1.005)))

# Add column for multipler to derive total pop size
nes$multiplier <- with(nes, ifelse(Year < 1991, 4.4, 4.31))

# Estimate total population size
nes$Population <- nes$Births * nes$multiplier

# Run the application 
shinyApp(ui = ui, server = server)

