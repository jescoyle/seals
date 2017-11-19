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
                     HTML("r<sub>max</sub>"),
                     value = NA),
         numericInput("K",
                      "K",
                      value = NA)        
      ),
      
      # Show a plot of the data with the logistic curve
      mainPanel(
        p("These data come from the article:"),
        p("Lowry et al. 2014. Abundance, Distribution, and Population Growth of the Northern Elephant Seal (Mirounga angustirostris) in the United States from 1991 to 2010. Aquatic Mammals 40: 20-31, DOI: ", a("10.1578/AM.40.1.2014.20", href = "http://dx.doi.org/10.1578/AM.40.1.2014.20")),
        p("The population breeding at San Miguel Island in the Channel Islands, California is estimated from the number of pups born each year using parameters provided in the paper."),
        plotOutput("overlayPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$overlayPlot <- renderPlot({
     
     # Make plot
     plot(I(Population/1000) ~ Year, data = nes, type = 'n', las = 1, 
          ylab = "Est. Population (thousands)")
     
     # Add gridlines
     #abline(v = seq(1958, 2010, 2), col = 'grey')
     #abline(h = seq(0, 70, 2), col = 'grey')
     #abline(v = seq(1960, 2010, 10), col = 'grey20')
     #abline(h = seq(0, 70, 10), col = 'grey20')
     
     # Add points
     points(I(Population/1000) ~ Year, data = nes, type = 'o', pch = 16)
     
     my_curve <- function(x) logistic_growth(x - 1984, 
                                             r = input$rmax, 
                                             K = input$K, 
                                             N0 = nes[nes$Year == 1984, 'Population']
                                             ) / 1000 # divide by 1000 to put on same scale as plot
     
     curve(my_curve, from = 1958, to = 2010, add = T, col = 'orange', lwd=3)
     
  })
   
}

# A logistic curve function
logistic_growth <- function(x, r, K, N0){
  
  K*N0*exp(r*x)/(K + N0*(exp(r*x) - 1))
  
}

# Read data from GitHub
repo_url <- "https://raw.githubusercontent.com/jescoyle/seals/master/"
nes <- read.csv(file.path(repo_url, "data/san_miguel_seal_births_Lowry_et_al_2014.csv"))

# Add column for multipler to derive total pop size
nes$multiplier <- with(nes, ifelse(Year < 1991, 4.4, 4.31))

# Estimate total population size
nes$Population <- nes$Births * nes$multiplier

# Run the application 
shinyApp(ui = ui, server = server)

