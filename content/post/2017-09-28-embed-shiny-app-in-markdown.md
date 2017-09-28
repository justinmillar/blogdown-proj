---
title: Embed Shiny app in Markdown
author: Justin Millar
date: '2017-09-28'
slug: embed-shiny-app-in-markdown
categories: []
tags: []
---

Seeing if I can embed [this Shiny app](https://jjmillar.shinyapps.io/msat-cost-graph/) using iframe:

<iframe src="https://jjmillar.shinyapps.io/msat-cost-graph" style="border: none; width: 100%; height: 900px"></iframe>

Seems to have worked! Unfortnately the sidebar and main panels seem to stack regardless of the width, still looking for a solution for this. Maybe there is a way to scale iframes?


If you're interested, here is the code for creating the app:

```
# Shiny app for cost tool graph

library(shiny)
library(tidyverse)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Comparison of presumptive treatments and test-than-treat across prevalences"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("sn",
                  "RDT Sensitivity",
                  min = 0, 
                  max = 1,
                  step = 0.05,
                  value = 0.9),
      
      sliderInput("sp",
                  "RDT Specificity",
                  min = 0,
                  max = 1,
                  step = 0.05,
                  value = 0.85),
      
      # Input for treatment
      numericInput("treat", "Cost of treatment:", min = 0.00, value = 6.00),
      
      # Input for treatment
      numericInput("rdt", "Cost of RDT:", min = 0.00, value = 3.00),
      
      # Input for false-positive
      numericInput("falsePos", "Cost of false positive:", min = 0.00, value = 0.00),
      
      # Input for false-negative
      numericInput("falseNeg", "Cost of false negative:", min = 0.00, value = 0.00), 
      p("Note: this application assumes the same unit of value (i.e. U.S. dollar) is used for each input")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("costPlot"), 
      p("This graph compares presumptive treatment (black) and test-then-treat (red) 
        intervention strategies as a function rapid diagonstic test (RDT) preformance 
        (sensitivity and specificity) and related costs (treatment, RDT, misdiagonsis)."),
      p("Whichever line is lowest for a particular prevalence value is estimated to be 
        the most economically efficent."), 
      p("A separate application for comparing these intervention strategies using observed 
        data on prevalence, sensitivity and specificity from five west African countries 
        is availible here:", 
        a("https://jjmillar.shinyapps.io/msat-cost-map/",
          href = "https://jjmillar.shinyapps.io/msat-cost-map/"))
      )
      )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$costPlot <- renderPlotly({
    prev <- seq(0,1,0.01)
    
    # Calcute costs
    costPresump = input$treat + input$falsePos*(1 - prev)
    
    costTNT = input$rdt + 
      input$treat*(input$sn*prev + (1 - input$sp)*(1 - prev)) + 
      input$falsePos*((1 - prev)*(1 - input$sp)) + 
      input$falseNeg*(prev * (1 - input$sn))
    
    tmp <- data.frame(Prevalence = prev, Cost = costPresump, Type = "Presumptive")
    tmp1 <- data.frame(Prevalence = prev, Cost = costTNT, Type = "Test-then-treat")
    dt <- rbind(tmp, tmp1)
    
    # Plot
    p <- ggplot(data = dt, aes(x = Prevalence, y = Cost, color = Type)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("black", "red")) +
      ylab("Estimated Cost") +
      theme_minimal() +
      theme(axis.line = element_line("black"))
    
    ggplotly(p = p)
    
  })
}
```