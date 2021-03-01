#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)

icudata <- readRDS("./icu_cohort.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("ICU Cohort Data Summary"),
  tabsetPanel(
    tabPanel("Demographic Data", 
             sidebarLayout(
              sidebarPanel(
                selectInput("var2", 
                            label = "Choose a Demographic Data to display",
                            choices = c("First Care Unit", "Last Care Unit","Admission Type",
                                        "Admission Location", "Discharge Location", 
                                        "Insurance", "Language", "Marital Status", 
                                        "Ethnicity", "Gender"),
                            selected = "Gender")),
                mainPanel(plotOutput("demoPlot"),
                          verbatimTextOutput("demoSum"))
              )
    )),
  tabsetPanel(
    tabPanel("Lab and Chart Data", 
              sidebarLayout(
                sidebarPanel(
                  selectInput("var", 
                              label = "Choose Lab/Chart Data to Display",
                              choices = c("Heart Rate", 
                                          "Non-Invasive Blood Pressure - Systolic",
                                          "Non-Invasive Blood Pressure - Mean",
                                          "Respiratory Rate", "Temperature (F)",
                                          "Arterial Blood Pressure - Systolic",
                                          "Arterial Blood Pressure - Mean",
                                          "Bicarbonate", "Calcium", "Chloride",
                                          "Creatinine", "Glucose", "Magnesium",
                                          "Potassium", "Sodium", "Hematocrit", 
                                          "White Blood Cell", "Lactate", "Ethnicity"),
                              selected = "Heart Rate"),
                  selectInput("gtype",
                              label = "How do you want to display the data?",
                              choices = c("Histogram", "Boxplot"), 
                              selected = "Histogram"),
                  sliderInput("bins",
                              "Number of bins:",
                              min = 1,
                              max = 200,
                              value = 50)
                ),
              
                mainPanel(plotOutput("distPlot"),
                          verbatimTextOutput("distSum"))
                ))))
  


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    data1 <- switch(input$var, 
                    "Heart Rate" = icudata$heart_rate,
                    "Non-Invasive Blood Pressure - Systolic" = 
                      icudata$non_invasive_blood_pressure_systolic,
                    "Non-Invasive Blood Pressure - Mean" = 
                      icudata$non_invasive_blood_pressure_mean,
                    "Respiratory Rate" = icudata$respiratory_rate,
                    "Temperature (F)" = icudata$temperature_fahrenheit,
                    "Arterial Blood Pressure - Systolic" = 
                      icudata$arterial_blood_pressure_systolic,
                    "Arterial Blood Pressure - Mean" = 
                      icudata$arterial_blood_pressure_mean,
                    "Bicarbonate" = icudata$bicarbonate, 
                    "Calcium" = icudata$calcium, 
                    "Chloride" = icudata$chloride,
                    "Creatinine" = icudata$creatinine, 
                    "Glucose" = icudata$glucose, 
                    "Magnesium" = icudata$magnesium,
                    "Potassium" = icudata$potassium, 
                    "Sodium" = icudata$sodium, 
                    "Hematocrit" = icudata$hematocrit, 
                    "White Blood Cell" = icudata$wbc,
                    "Lactate" = icudata$lactate
    )
    x <- data.frame(data1)
    names(x) <- c("obs_val")
    
    # draw the histogram with the specified number of bins
    if ("Histogram" %in% input$gtype) {
      bins <- seq(min(data1, na.rm = TRUE),max(data1, na.rm = TRUE), 
                  length.out = input$bins+1)
      hist(data1, breaks = bins, col = 'steelblue', border = 'white', 
           xlab = str_c(input$var))
    }
    if ("Boxplot" %in% input$gtype) {
      x %>% ggplot(aes(x = obs_val)) + geom_boxplot() + 
        labs(x = str_c(input$var)) 
    }
    
    
  })
  output$distSum <- renderPrint({
    data1 <- switch(input$var, 
                    "Heart Rate" = icudata$heart_rate,
                    "Non-Invasive Blood Pressure - Systolic" = 
                      icudata$non_invasive_blood_pressure_systolic,
                    "Non-Invasive Blood Pressure - Mean" = 
                      icudata$non_invasive_blood_pressure_mean,
                    "Respiratory Rate" = icudata$respiratory_rate,
                    "Temperature (F)" = icudata$temperature_fahrenheit,
                    "Arterial Blood Pressure - Systolic" = 
                      icudata$arterial_blood_pressure_systolic,
                    "Arterial Blood Pressure - Mean" = 
                      icudata$arterial_blood_pressure_mean,
                    "Bicarbonate" = icudata$bicarbonate, 
                    "Calcium" = icudata$calcium, 
                    "Chloride" = icudata$chloride,
                    "Creatinine" = icudata$creatinine, 
                    "Glucose" = icudata$glucose, 
                    "Magnesium" = icudata$magnesium,
                    "Potassium" = icudata$potassium, 
                    "Sodium" = icudata$sodium, 
                    "Hematocrit" = icudata$hematocrit, 
                    "White Blood Cell" = icudata$wbc,
                    "Lactate" = icudata$lactate
    )
    x <- data.frame(data1)
    names(x) <- c("obs_val")
    
    summarise(x,Mean = mean(obs_val, na.rm = TRUE), St.Dev. = 
                sd(obs_val, na.rm = TRUE),  Min = min(obs_val, na.rm = TRUE), 
              Q1 = quantile(obs_val, 0.25, na.rm = TRUE), Median = 
                quantile(obs_val, 0.5, na.rm = TRUE), 
              Q3 = quantile(obs_val, 0.75, na.rm = TRUE), 
              Max = max(obs_val, na.rm = TRUE))
    
  })
  output$demoPlot <- renderPlot({
    data2 <- switch(input$var2, "First Care Unit" = icudata$first_careunit,
                    "Last Care Unit" = icudata$last_careunit,
                    "Admission Type" = icudata$admission_type,
                    "Admission Location" = icudata$admission_location,
                    "Discharge Location" = icudata$discharge_location, 
                    "Insurance" = icudata$insurance, 
                    "Language" = icudata$language, 
                    "Marital Status" = icudata$marital_status, 
                    "Ethnicity" = icudata$ethnicity, 
                    "Gender" = icudata$gender)
    
    y <- data.frame(data2)
    names(y) <- c("obs_val")
    y %>% ggplot(aes(x = obs_val)) + geom_bar(mapping = aes(fill = obs_val)) + 
      labs(x = str_c(input$var2)) + coord_flip() 
  })  

  output$demoSum <- renderPlot({
    data2 <- switch(input$var2, "First Care Unit" = icudata$first_careunit,
                    "Last Care Unit" = icudata$last_careunit,
                    "Admission Type" = icudata$admission_type,
                    "Admission Location" = icudata$admission_location,
                    "Discharge Location" = icudata$discharge_location, 
                    "Insurance" = icudata$insurance, 
                    "Language" = icudata$language, 
                    "Marital Status" = icudata$marital_status, 
                    "Ethnicity" = icudata$ethnicity, 
                    "Gender" = icudata$gender)  
    y <- data.table(data2)
    tables(y)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
