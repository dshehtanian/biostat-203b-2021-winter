#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

icudata <- readRDS("./icu_cohort.rds")
    
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ICU Cohort Data Summmary"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("var", 
                        label = "Choose a variable to display",
                        choices = c("Heart Rate", "Respiratory Rate"),
                        selected = "Heart Rate"),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 100,
                        value = 30)
  
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        data1 <- switch(input$var, 
                       "Heart Rate" = icudata$heart_rate,
                       "Respiratory Rate" = icudata$respiratory_rate
                       )
        x <- data1
        bins <- 100
        # draw the histogram with the specified number of bins
        hist(data1, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
