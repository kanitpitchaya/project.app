library(shiny)
library(shinythemes)
library(tidyverse)
library(summarytools)
library(ggplot2)
library(dplyr)
library(readxl)
library(stats)
library(factoextra)
library(randomForest)
library(plotly)
Sys.setlocale(locale = "Thai")

data<-read.csv("D:\\2.64\\Shiny\\63-2.csv", encoding = "UTF-8")
tree<-read_rds("D:\\2.64\\Shiny\\modeltree.RDS")
model1<-read_rds("D:\\2.64\\Shiny\\modelRan3.RDS")
plot.dat<-read_excel("D:\\2.64\\Shiny\\tree.xlsx")
contentdata<-read.csv("D:\\2.64\\Shiny\\content.csv.", encoding = "UTF-8")
glimpse(data)
glimpse(contentdata)
data[is.na(data)] = 0
data<-data%>%mutate(total= rowSums(data[5:10]))
names(data)[1]<-paste("No.")
data$student_ID<-as.character(data$student_ID)
Nscore<-data[c(1,2,5:11)]
score<-data[5:10]
k<-data[c(1,11)]
km<-kmeans(k, centers = 2 )

ui <- fluidPage(theme = shinytheme("cosmo"),
                navbarPage(
                  "score report app",
                  tabPanel("personal data",
                           sidebarPanel(
                             tags$h3("Please fill data"),
                             selectInput(inputId="var",
                                         choices=Nscore$No.,
                                         label=c("student number")),
                             numericInput("mid", "midterm score",
                                          min = 0, max = 25,
                                          value = 3),
                             numericInput("tot", "total score",
                                        min = 0, max = 100,
                                        value = 3),
                           ), # sidebarPanel
                  
                  
                           mainPanel(
                             h1("score report"),
                             tableOutput("value"),
                             textOutput("tree"),
                             plotOutput("plot1"),
                             textOutput("Rand"),
                             tableOutput("content"), 
                            
                           ) # mainPanel
                ), # Navbar 1, tabPanel
                
                tabPanel("Overview",
                         mainPanel(
                           
                           fluidRow(
                             h1("descirptive"),
                             br(), #line spacing
                             column(5, 
                             tableOutput("descrip"),        
                             ),
                             column(8, 
                               
                           plotOutput("km"),
                         )        
                  ), # end of fluidR
                           
                           
                         )    
                         
                ),
                
                
) # navbarPage

) # fluidPage

server <- function(input, output) {
  
  test.dat<-reactive({
    data.frame("midtrem"=input$mid, stringsAsFactors = F)                                
  })
  
  tt<-reactive({
    (as.character(predict(tree, test.dat())))})
  
  output$tree<- renderText({
    (paste(tt())) 
    
  })
  
  
  output$plot1<-renderPlot({
    if (input$mid>15.12) { 
    barplot(plot.dat$percent2, names.arg=c("fail","pass"),
              col=c("#B85C38","#368B85"),
              cex.names=2,
              cex.axis=1.5,
              cex.lab=1.5,
              ylim=c(0,100), main = "opportunity to pass the course")
    }
    else
      if
    (input$mid<=15.11) {
      barplot(plot.dat$percent1, names.arg=c("fail","pass"),
              col=c("#B85C38","#368B85"),
              cex.names=2,
              cex.axis=1.5,
              cex.lab=1.5,
              ylim=c(0,100), main = "opportunity to pass the course")
    }
  })
  
  

  test.data<-reactive({
    data.frame("total"=input$tot, stringsAsFactors = F)                                
  })
  
  rran<-reactive({
    (as.character(predict(model1, test.data())))})
  
  output$Rand<- renderText({
    (paste("predict grades;", rran())) 
    
  })
  
  output$content<- renderTable({
    if (input$tot<55) { 
      contentdata$D
     }
    else
      if
    (input$tot>=55 & input$tot<75) {
      contentdata$C
    }
    else
      if
    (input$tot>=75 & input$tot<85) {
      contentdata$B
    }
    else
      if
    (input$tot>=85) {
      contentdata$X.U.FEFF.A
    }
    })
  
 output$descrip <- renderTable({
    
    m <- descr(score, 
               stats = c("mean", "sd", "min", "max"),
               transpose = FALSE) 
    class(m) <-"matrix"
    m %>% as_tibble(rownames="Statistic")
  })
  
  
  output$km <- renderPlot({
   
  fviz_cluster(km, data=k, pointsize = 1, choose.vars = c("No.", "total"), stand = FALSE, 
               ellipse.type = "norm",  palette = "jco", ggtheme = theme_minimal())
  })
  
  
  
  
  output$value <- renderTable({
    
    if( input$var=="0") {
      Nscore[1,]
    }
    else
      if
    (input$var=="1") {
      Nscore[2,]
    }
    else
      if(input$var=="2") {
        Nscore[3,]
      }
    else
      if(input$var=="3") {
        Nscore[4,]
      }
    else
      if(input$var=="4") {
        Nscore[5,]
      }
    else
      if
    (input$var=="5") {
      Nscore[6,]
    }
    else
      if(input$var=="6") {
        Nscore[7,]
      }
    else
      if(input$var=="7") {
        Nscore[8,]
      }
    else
      if(input$var=="8") {
        Nscore[9,]
      }
    else
      if(input$var=="9") {
        Nscore[10,]
      }
    else
      if
    (input$var=="10") {
      Nscore[11,]
    }
    else
      if(input$var=="11") {
        Nscore[12,]
      }
    else
      if(input$var=="12") {
        Nscore[13,]
      }
    else
      if(input$var=="13") {
        Nscore[14,]
      }
    else
      if
    (input$var=="14") {
      Nscore[15,]
    }
    else
      if(input$var=="15") {
        Nscore[16,]
      }
    else
      if(input$var=="16") {
        Nscore[17,]
      }
    else
      if(input$var=="17") {
        Nscore[18,]
      }
    else
      if(input$var=="18") {
        Nscore[19,]
      }
    else
      if(input$var=="19") {
        Nscore[20,]
      }
    else
      if(input$var=="20") {
        Nscore[21,]
      }
    else
      if
    (input$var=="21") {
      Nscore[22,]
    }
    else
      if(input$var=="22") {
        Nscore[23,]
      }
    else
      if(input$var=="23") {
        Nscore[24,]
      }
    else
      if(input$var=="24") {
        Nscore[25,]
      }
    else
      if
    (input$var=="25") {
      Nscore[26,]
    }
    else
      if(input$var=="26") {
        Nscore[27,]
      }
    else
      if(input$var=="28") {
        Nscore[29,]
      }
    else
      if(input$var=="29") {
        Nscore[30,]
      }
    else
      if
    (input$var=="30") {
      Nscore[31,]
    }
    else
      if(input$var=="31") {
        Nscore[32,]
      }
    else
      if(input$var=="32") {
        Nscore[33,]
      }
    else
      if(input$var=="42") {
        Nscore[33,]
      }
    
  })
  
  
  
  
  
} # server

# Create Shiny object

shinyApp(ui = ui, server = server)


