library(shinythemes)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
#install.packages("ggridges")
library(ggridges)
library(DT)
library(stringr)

library(readr)
Portugal_bank <- read_csv("Portugal_bank.csv")


#############################################################

ui <- dashboardPage(
  dashboardHeader(title = "Portugal Dataset", titleWidth = 290),
  dashboardSidebar(selectInput("y", label = "Subscription", choices = c("yes", "no", "both"),
                               selected = "both"),
                   selectInput("contact", label = "Means of contact", choices = c("telephone", "cellular", "both"),
                               selected = "both")),
  dashboardBody(
    fluidRow(box(plotOutput("portugal_density")), box(plotOutput("portugal_bar"))),
    fluidRow(box(plotOutput("portugal_box")), box(plotOutput("portugal_col"))),
    fluidRow(
      tabBox(
        id = "tabset1", height = "250px",
        tabPanel("Subscription vs Contact", "As we can see on the Subscription vs Contact barplot,
                 using cellular is most favorable way to attract clients than telephone"),
        tabPanel("Boxplot", "Age, campaign and Duration - Boxplot")
      ),
      tabBox(
        side = "right", height = "250px",
        selected = "Tab3",
        tabPanel("Subscription and Age", "As we can see most people who said yes
                 for the subscription are middle aged people"),
        tabPanel("Variables", "Subscription vs Age, Duration and Campaign
                 Age:implies the Age of the Client,
                 Duration:last contact duration, in seconds,
                 Campaign: number of contacts performed during this campaign."),
        
        tabPanel("Introduction", "In this project, a data from Portugal
                 bank institution was used to predict which features affect the marketing campaign.
                 The dataset is obtained from Kaggle and its originally provided by the University of
                 California, school of information and Computer Science machine learning repository.
                 To see the data, you can shed or select part of the Campaign vs age graph below and 
                 download part of the data with all the 21 variables")
      )
      
      
    ),
    
    fluidPage(
      h3("Drag part of the graph and 
         download"),
      plotOutput("plot", brush = "user_brush"),
      dataTableOutput("table"),
      downloadButton(outputId = "mydownload", label = "Download Table")
    )
    
  )
  
  
  
  
)






server<-function(input, output) {
  
  Portugal_bank <- read_csv("Portugal_bank.csv")
  
 
  output$portugal_density <- renderPlot({
    Portugal_bank %>% 
      select(-c(2:10,13:20)) %>% 
      pivot_longer(-y) %>% 
      group_by(name) %>% 
      mutate(value = scale(value)) %>%
      ungroup() %>% 
      filter(str_detect(y, if_else(input$y == "both", "", input$y))) %>% 
      ggplot(aes(x=value, y=name, fill = y)) + 
      geom_density_ridges(alpha = 0.7) +
      ggtitle("Subscription vs age,
          duration and campaign") +
      theme(axis.text = element_text(size = 10),
            axis.title = element_text(size = 13),
            plot.title = element_text(size = 15, face = "bold"),
            legend.position = "top")
    
  })
  
  output$portugal_box <- renderPlot({
    Portugal_bank %>% 
      select(-c(2:10,13:20)) %>% 
      pivot_longer(-y) %>% 
      group_by(name) %>% 
      mutate(value = scale(value)) %>%
      ungroup() %>% 
      ggplot(aes(x=name, y = value, color = y)) + 
      geom_boxplot() +
      ggtitle("Boxpot: Age, Campaign and Duration") +
      theme(axis.text = element_text(size = 10),
            axis.title = element_text(size = 13),
            plot.title = element_text(size = 15, face = "bold"),
            legend.position = "top")
    
  })
  
  output$portugal_bar<-renderPlot({
    Portugal_bank %>% 
      ggplot(aes(y=age, x=y)) + 
      geom_point(position = "jitter", color = "blue", alpha = 1/5) + 
      ggtitle("Subscription vs Age") +
      theme(axis.text = element_text(size = 10),
            axis.title = element_text(size = 13),
            plot.title = element_text(size = 15, face = "bold"))
  })
  
  output$portugal_col<-renderPlot({
    Portugal_bank %>% 
      count(y, contact) %>% 
      group_by(y) %>% 
      mutate(n = n/sum(n)) %>% 
      ungroup() %>% 
      filter(str_detect(contact, if_else(input$contact == "both", "", input$contact))) %>% 
      filter(str_detect(y, if_else(input$y == "both", "", input$y))) %>% 
      ggplot(aes(x=contact, y = n, fill = y)) + 
      geom_col() + 
      scale_y_continuous(labels = scales::label_percent()) +
      ggtitle("Subscription vs Contact") +
      theme(axis.text = element_text(size = 10),
            axis.title = element_text(size = 13),
            plot.title = element_text(size = 15, face = "bold"),
            legend.position = "top")
  })
  
  output$plot<-renderPlot({
    Portugal_bank %>% 
      ggplot(aes(x=age, y=campaign)) + 
      geom_point() + 
      scale_y_continuous(labels = scales::label_percent()) +
      ggtitle("Campaign vs Age") +
      theme(axis.text = element_text(size = 10),
            axis.title = element_text(size = 13),
            plot.title = element_text(size = 15, face = "bold"),
            legend.position = "top")
  })
  
  diam<-reactive({
    user_brush<-input$user_brush
    sel<-brushedPoints(Portugal_bank, user_brush)
    return(sel)
  })
  
  output$table <- DT::renderDataTable(DT::datatable(diam()))
  output$mydownload <- downloadHandler(
    filename = "plotextract.csv",
    content = function(file) {
      write.csv(diam(), file)
    })
}



shinyApp(ui=ui, server=server)





