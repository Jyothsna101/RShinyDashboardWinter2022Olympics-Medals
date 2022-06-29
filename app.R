#Load Libraries 
library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(scales)
library(psych)
library(tidyr)
library(plyr)
library(ggfortify)
library(reshape2)

#Load the data set
medals <- read.csv("/Users/jyothsnaakula/Documents/6070/Data/medals.csv")
View(medals)
str(medals)
glimpse(medals)
v<- gsub(x=medals$medal_date,pattern="00:00:00.0",replacement="",fixed=T)
v
w <- gsub(x=v,pattern="2022-02-",replacement="",fixed=T)
w

ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(title = "MEDALS INFORMATION", titleWidth = 1200),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                      fluidRow(
                        
                        #ValueBoxes
                        valueBoxOutput("total_gold_medals"),
                        valueBoxOutput("total_silver_medals"),
                        valueBoxOutput("total_bronze_medals"),
                        box(
                          title = "MedalCode vs MedalType",
                          status = "primary"
                          ,solidHeader = TRUE
                          ,collapsible = TRUE
                          ,plotOutput("medal_type",height = "500px")
                        ),
                        box(
                          title = "Medal vs AtheleteSex",
                          status = "primary"
                          ,solidHeader = TRUE
                          ,collapsible = TRUE
                          ,plotOutput("medal_sex",height = "500px")
                        ),
                        box(
                          title = "Medal vs Country",
                          status = "primary"
                          ,solidHeader = TRUE
                          ,collapsible = TRUE
                          ,plotOutput("medal_country", height = "500px")
                        ),
                        box(
                          title = "Medal vs Discipline",
                          status = "primary"
                          ,solidHeader = TRUE
                          ,collapsible = TRUE
                          ,plotOutput("medal_discipline", height = "500px")
                        )
                      )))

server <- function(input, output) {
  
  output$total_gold_medals <- renderValueBox({
    valueBox(formatC(sum(medals$medal_code==1,na.rm=TRUE), big.mark = ","),
             "Total Gold Medals for Winter Olympics 2022", 
             icon = icon("users", lib = "glyphicon"),
             color = "blue")
  })
  output$total_silver_medals <- renderValueBox({
    valueBox(formatC(sum(medals$medal_code==2,na.rm=TRUE), big.mark=','),
             'Total Silver Medals for Winter Olympics 2022',
             color = "blue")
  })
  
  output$total_bronze_medals <- renderValueBox({
    valueBox(formatC(sum(medals$medal_code==3,na.rm=TRUE), big.mark = ","),
             'Total Bronze Medals for Winter Olympics 2022',
             color = "blue")
  })
  
  #Plot for Medal code & date using medal type as color 
  output$medal_type <- renderPlot({
    ggplot(data = medals, aes(x = medal_code, y = v, color= medal_type)) +
      geom_boxplot()+
      xlab("Medal Code") +
      ylab("Medal Date") +
      theme_grey()
  })
  
  #Plot for Medal code & date using athlete_sex as color 
  output$medal_sex <- renderPlot({
    ggplot(data = medals, aes(x = medal_code, y = v, color=athlete_sex)) +
      geom_bar(stat = 'identity',fill="#FFFFB3", alpha=.6, width=.4) +
      xlab("Medal Code") +
      ylab("Medal Date") +
      theme_grey()
  })
  
  #Plot for Medal code & date using country as color 
  output$medal_country <- renderPlot({
    ggplot(data = medals, aes(x = medal_code, y = v, color = country))+
     geom_point(alpha=0.5,size=6) +
      xlab("Medal Code") +
      ylab("Medal Date") +
      theme_grey()
  })
  
  #Plot for Medal code & date using discipline_code as color 
  output$medal_discipline <- renderPlot({
     ggplot(data = medals, aes(x=medal_code, y = v,color = discipline_code)) +
      #geom_histogram(stat = 'identity')+
      geom_point(shape = 18,alpha=0.5,size=6)+
      xlab("Medal Code") +
      ylab("Medal Date") +
      theme_grey()
  })
}

#Running the Application
shinyApp(ui = ui, server = server)
