# ===============================================
# Fill in the following fields
# ===============================================
# Title: STAT 133 Project 2
# Description: Saving Rate Calculator using Shiny app
# Author: Joy Lin
# Date: 4/8/2022


# ===============================================
# Required packages
# ===============================================
library(shiny)
library(tidyverse)
library(grid)
# etc



# ===============================================
# Define User-Interface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Saving Rate Calculator"),
  
  fluidRow(
    # Input(s) for annual-income
    column(3,
           numericInput(inputId = "income", 
                        label = "Annual Income ($)", 
                        value = 50000),
    ),
    
    # Input(s) for target-amount
    column(3,
           numericInput(inputId = "target", 
                        label = "Target Amount ($)", 
                        value = 1000000),
    ),
    
    # Input(s) for current-age
    column(3,
           sliderInput(inputId = "age", 
                       label = "Current Age (years)", 
                       min = 0, 
                       max = 100, 
                       value = 25)
    ),
    
    # Input(s) for rate-of-return
    column(3,
           sliderInput(inputId = "return", 
                       label = "Rate of return (%)", 
                       min = 0, 
                       max = 100, 
                       value = 5)
    )
  ),
  
  hr(),
  h4('Number of Years to Reach a Target Amount for Various Saving Rates'),
  plotOutput('plot1'),
  
  hr(),
  h4('Percentage Contribution v.s. Percentage Growth for Various Saving Rates'),
  plotOutput('plot2'),
  
  hr(),
  h4('Outputs for Various Savings Rates'),
  DT::dataTableOutput('table')
)



# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # you may need to create reactive objects
  # (e.g. data frame to be used for graphing purposes)
  s = seq(5, 100, 5)
  ac = reactive(input$income * (s/100))
  t = reactive(log((input$return) / 100 * input$target / ac() + 1) / log(1 + input$return / 100))
  t1 = reactive(t() + input$age)
  tc = reactive(ac() * t())
  tg = reactive(input$target - tc())
  pc = reactive(tc() / input$target * 100)
  pg = reactive((input$target - tc()) / input$target * 100)
  
  dat <- reactive({
    data.frame(
      saving_rate = s,
      annual_contribution = ac(),
      total_contribution = round(tc(), 1),
      total_grow = round(tg(), 1),
      percent_contribution = round(pc(), 2), 
      percent_growth = round(pg(), 2),
      number_of_years = round(t(), 2),
      age_at_target = round(t1(),1)
    )
  })
  
    
  # code for plot-1
  # (e.g. uses reactive data frame for graphing purposes)
  output$plot1 <- renderPlot({
    ggplot(data = dat(), aes(x = saving_rate, y = number_of_years)) +
      geom_bar(stat="identity") +
      labs(x = "savings rate (%)",
           y = "years to reach target amount") +
      geom_text(aes(label = number_of_years), 
                color = "white", 
                size = 4, 
                vjust = 1.5) +
      scale_x_continuous(breaks = seq(0, 100, 5)) +
      theme_classic()
  })
  

  # code for plot-2
  # (e.g. uses reactive data frame for graphing purposes)
  output$plot2 <- renderPlot({
    # replace the code below with your code!!!
    ggplot(data = dat()) +
      geom_point(aes(x = saving_rate, 
                     y = percent_contribution),
                 color = "orange") +
      geom_line(aes(x = saving_rate, 
                     y = percent_contribution),
                color = "orange") +
      geom_point(aes(x = saving_rate, 
                     y = percent_growth),
                 color = "blue") +
      geom_line(aes(x = saving_rate, 
                     y = percent_growth),
                color = "blue") +
      labs(x = "Saving Rates (%)",
           y = "Percent Contrib & Growth (%)") +
      annotation_custom(grobTree(textGrob("Percent Contrib (%)", x=0.755,  y=0.75, hjust=0,
                                          gp=gpar(col="orange", fontsize=12, fontface="italic")))) +
      annotation_custom(grobTree(textGrob("Percent Growth (%)", x=0.755,  y=0.4, hjust=0,
                                          gp=gpar(col="blue", fontsize=12, fontface="italic")))) +
      scale_x_continuous(breaks = seq(0, 100, 5)) +
      ylim(0, 100) +
      theme_classic()
  })

    
  # code for statistics
  output$table <- DT::renderDataTable({
    DT::datatable(dat(), 
                  colnames = c("Savings Rate (%)", 
                               "Annual Contrib ($)", 
                               "Total Contribution ($)", 
                               "Total Growth ($)", 
                               "Percent Contrib (%)", 
                               "Percent Growth (%)", 
                               "Number of Years", 
                               "Age at Target"))
    })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

