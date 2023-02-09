#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(ggthemes)
library(shiny)
library(dplyr)
library(tidyverse)
library (plotly)
library(lubridate)


db <- read_csv("./data-clean/quality.csv")
db <- db %>%
  mutate(production_date = lubridate::dmy(production_date))
db <- db %>% 
  rename("Mean_Daily_Production" = "mean_prod_sl",
         "Missing_Obs" = "missing_obs", "Percentage_Constant_4"="percent_constant_four",
         "Percentage_Constant_8"="percent_constant_eight", "Standard_Deviation"="std",
         "Percentage_Duplicates"="dup", "Percentage_All_8_hrs"="percent_all_eight",
         "Percentage_First_4_hrs"="percent_first_four", "Percentage_Last_4_hrs"="percent_last_four")

# Define server logic required to draw a histogram
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  #output missing plot
  output$missing <- renderPlotly({
    db %>% 
      filter(production_date >= input$dates[1]) %>%
      filter(production_date <= input$dates[2]) %>%
      filter(unit_code == input$select) %>%
      gather(key = Database, value = Percent,
             Missing_Obs, Percentage_All_8_hrs) %>%
      ggplot(aes(x=production_date, y = Percent, fill = Database, colour = Database)) + 
      geom_line() + scale_fill_brewer(palette="Accent") +theme_classic() + 
      labs(x = "Date", y = "Percent") +
      ggtitle("Percentage of Missing Observations vs Empcodes with all 8 hrs Productivity Data")
  })
  
  #output constant plot
  output$constant <- renderPlotly({
    db %>% 
      filter(production_date >= input$dates[1]) %>%
      filter(production_date <= input$dates[2]) %>%
      filter(unit_code == input$select) %>%
      gather(key = Database, value = Percent,
             Percentage_Constant_4, Percentage_Constant_8) %>%
      ggplot(aes(x=production_date, y = Percent, fill = Database, colour = Database)) + 
      geom_line() + scale_fill_brewer(palette="Accent") +theme_classic() + 
      labs(x = "Date", y = "Percent") +
      ggtitle("Percentage of Empcodes with Constant 4 vs 8 hrs Productivity Data")
  })
  
  #output first4 vs last4
  output$first_vs_last <- renderPlotly({
    db %>% 
      filter(production_date >= input$dates[1]) %>%
      filter(production_date <= input$dates[2]) %>%
      filter(unit_code == input$select) %>%
      gather(key = Database, value = Percent,
             Percentage_First_4_hrs, Percentage_Last_4_hrs) %>%
      ggplot(aes(x=production_date, y = Percent, fill = Database, colour = Database)) + 
      geom_line() + scale_fill_brewer(palette="Accent") +theme_classic() + 
      labs(x = "Date", y = "Percent") +
      ggtitle("Percentage of Empcodes with Only First 4 vs Last 4 hrs Productivity Data")
  })
  
  #output prod mean & sd
  output$prod <- renderPlotly({
    db %>% 
      filter(production_date >= input$dates[1]) %>%
      filter(production_date <= input$dates[2]) %>%
      filter(unit_code == input$select) %>%
      gather(key = Database, value = Quantity,
             Mean_Daily_Production, Standard_Deviation) %>%
      ggplot(aes(x=production_date, y = Quantity, fill = Database, colour = Database)) + 
      geom_line() + scale_fill_brewer(palette="Accent") +theme_classic() + 
      labs(x = "Date", y = "Percent") +
      ggtitle("Mean Daily Production vs Standard Deviation of Productivity Data")
  })
  
  #output duplicate empcodes
  output$duplicate <- renderPlotly({
    db %>% 
      filter(production_date >= input$dates[1]) %>%
      filter(production_date <= input$dates[2]) %>%
      filter(unit_code == input$select) %>%
      ggplot(aes(x=production_date, y=Percentage_Duplicates, group = 1)) +
      geom_line() + scale_fill_brewer(palette="Accent") +
      theme_classic() + labs(x = "Date", y = "Percent") +
      ggtitle("Percentage of Duplicate Employee IDs")
  })
  
  #download missing plot
  output$missing_d <- downloadHandler(
    filename = function() {
      stringr::str_glue("Missing_{input$select}_{input$dates[1]}_{input$dates[2]}.png")
    },
    
    content = function(file) {
      ggsave(file, 
             db %>% 
               filter(production_date >= input$dates[1]) %>%
               filter(production_date <= input$dates[2]) %>%
               filter(unit_code == input$select) %>%
               gather(key = Database, value = Percent,
                      Missing_Obs, Percentage_All_8_hrs) %>%
               ggplot(aes(x=production_date, y = Percent, fill = Database, colour = Database)) + 
               geom_line() + scale_fill_brewer(palette="Accent") +theme_classic() + 
               labs(x = "Date", y = "Percent") +
               ggtitle("Percentage of Missing Observations vs Empcodes with all 8 hrs Productivity Data"),
             width = 8, height = 5, dpi = 300)
    }
    
  )
  
  #download constant plot
  output$constant_d <- downloadHandler(
    filename = function() {
      stringr::str_glue("Constant_4_vs_8_hrs_{input$select}_{input$dates[1]}_{input$dates[2]}.png")
    },
    
    content = function(file) {
      ggsave(file, 
             db %>% 
               filter(production_date >= input$dates[1]) %>%
               filter(production_date <= input$dates[2]) %>%
               filter(unit_code == input$select) %>%
               gather(key = Database, value = Percent,
                      Percentage_Constant_4, Percentage_Constant_8) %>%
               ggplot(aes(x=production_date, y = Percent, fill = Database, colour = Database)) + 
               geom_line() + scale_fill_brewer(palette="Accent") +theme_classic() + 
               labs(x = "Date", y = "Percent") +
               ggtitle("Percentage of Empcodes with Constant 4 vs 8 hrs Productivity Data"),
             width = 8, height = 5, dpi = 300)
    }
    
  )
  
  #download first4 vs last4
  output$first_vs_last_d <- downloadHandler(
    filename = function() {
      stringr::str_glue("First4_vs_Last4_{input$select}_{input$dates[1]}_{input$dates[2]}.png")
    },
    
    content = function(file) {
      ggsave(file, 
             db %>% 
               filter(production_date >= input$dates[1]) %>%
               filter(production_date <= input$dates[2]) %>%
               filter(unit_code == input$select) %>%
               gather(key = Database, value = Percent,
                      Percentage_First_4_hrs, Percentage_Last_4_hrs) %>%
               ggplot(aes(x=production_date, y = Percent, fill = Database, colour = Database)) + 
               geom_line() + scale_fill_brewer(palette="Accent") +theme_classic() + 
               labs(x = "Date", y = "Percent") +
               ggtitle("Percentage of Empcodes with Only First 4 vs Last 4 hrs Productivity Data"),
             width = 8, height = 5, dpi = 300)
    }
    
  )
  
  #download prod mean and sd
  output$prod_d <- downloadHandler(
    filename = function() {
      stringr::str_glue("Productivity_Mean_SD_{input$select}_{input$dates[1]}_{input$dates[2]}.png")
    },
    
    content = function(file) {
      ggsave(file, 
             db %>% 
               filter(production_date >= input$dates[1]) %>%
               filter(production_date <= input$dates[2]) %>%
               filter(unit_code == input$select) %>%
               gather(key = Database, value = Quantity,
                      Mean_Daily_Production, Standard_Deviation) %>%
               ggplot(aes(x=production_date, y = Quantity, fill = Database, colour = Database)) + 
               geom_line() + scale_fill_brewer(palette="Accent") +theme_classic() + 
               labs(x = "Date", y = "Percent") +
               ggtitle("Mean Daily Production vs Standard Deviation of Productivity Data"),
             width = 8, height = 5, dpi = 300)
    }
    
  )
  
  #download duplicate plot
  output$duplicate_d <- downloadHandler(
    filename = function() {
      stringr::str_glue("Duplicate_Emp_ID_{input$select}_{input$dates[1]}_{input$dates[2]}.png")
    },
    
    content = function(file) {
      ggsave(file, 
             db %>% 
               filter(production_date >= input$dates[1]) %>%
               filter(production_date <= input$dates[2]) %>%
               filter(unit_code == input$select) %>%
               ggplot(aes(x=production_date, y=Percentage_Duplicates, group = 1)) +
               geom_line() + scale_fill_brewer(palette="Accent") +
               theme_classic() + labs(x = "Date", y = "Percent") +
               ggtitle("Percentage of Duplicate Employee IDs"),
             width = 8, height = 5, dpi = 300)
    }
    
  )
  
  # render data table to ui
  output$raw_data <- DT::renderDT(
    db %>%
      filter(unit_code == input$select)%>% 
      filter(production_date >= input$dates[1]) %>%
      filter(production_date <= input$dates[2])
  )
  
  output$download_quality_data <- downloadHandler(
    filename = function() {
      stringr::str_glue("Coverage_Quality_{input$select}_{input$dates[1]}_{input$dates[2]}.csv")
    },
    
    content = function(file) {
      write.csv(db %>%
                  filter(unit_code == input$select)%>% 
                  filter(production_date >= input$dates[1]) %>%
                  filter(production_date <= input$dates[2]), file, row.names = FALSE)
    }
    
  )
}

