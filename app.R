library(shiny)              #loading the libraries
library(shinydashboard)     
library(shinythemes)
library(scales)
ui<-fluidPage(                                   #Designing thhe user interface
     theme = shinytheme("flatly"),
    
     titlePanel( title = h4("Correlation and Clustering", align="center")),                  #TitlePanel
     
     sidebarLayout(                                                                      #sidebarLayout
    
     sidebarPanel(
       
        selectInput("var1", "Select the X variable",list("Miles/Gallon"=1,"Number of cylinders"=2,"Horsepower"=4, "Weight"=6, "Type of Transmission"=9)),     #select input panel
        br(),
        selectInput("var2", "Select the Y variable", list("Miles/Gallon"=1,"Number of cylinders"=2,"Horsepower"=4, "Weight"=6, "Type of Transmission"=9)),
        br(),
       numericInput("clusters","select the number of clusters",value = 3,min = 1, max = 7),
      
       br(),
        radioButtons("var3", "Select The File Type", choices = list("png","pdf"))                              #initalizing the radio buttons
  
       ),
     mainPanel(tabsetPanel(type='tab',                                    #initalising the tabs
                                 
                                        tabPanel("plot", plotOutput("scatter", click = "plot_click"), verbatimTextOutput("info", placeholder = TRUE),verbatimTextOutput("correlation", placeholder = TRUE)),
                                        
                         
                                     tabPanel("Data", tableOutput("table")),
                                    tabPanel("Structure", verbatimTextOutput("str")),
                                    tabPanel("summary of x variable", verbatimTextOutput("summary")),
                                    tabPanel("summary of y variable", verbatimTextOutput("summary1"))
  
                            
                          ),
            
            downloadButton(outputId = "down", label = "Download")             #creating the downlaod button
  
     )        #sidebarpanel
     )       #fluidpage
)

server<-function(input, output)                     #server side code
{
  output$info<-renderText({
  
    paste("x=",input$plot_click$x, "\ny=", input$plot_click$y) #Calculating the points when clicking on it
   
   
    
    })
  
 # mtcars<-mtcars[,-c(10:11)]
  #mtcars<-mtcars[,-7]
#mtcars<-mtcars[,-3]
 # mtcars<-mtcars[,-4]
  output$correlation<-renderText({
    paste("correlation=",  cor(mtcars[,as.numeric(input$var1)], mtcars[, as.numeric(input$var2)]))   #calculating correlation
  })
  
 output$str<-renderPrint({
   str(mtcars)                                             #Printing the structure of dataset
   })
 
 
 output$summary<-renderPrint({                          #Printing the summary of X variable
   summary(mtcars[, as.numeric(input$var1)])
   
 })
 output$summary1<-renderPrint({                #printing the summary of Y variable
   summary(mtcars[, as.numeric(input$var2)])
 })

  output$table<-renderTable({           #Printing data of x and y variable in the form of Table
    
    
   head(mtcars[,c(as.numeric(input$var1),as.numeric(input$var2))])
   
    
 })
  x<-reactive({               #Reactive function computing x variable 
    mtcars[, as.numeric(input$var1)]      
    
  })
  
  
  y<-reactive({           #Reactive function computing y variable
    mtcars[,as.numeric(input$var2)]
  })
  

  selectedData <- reactive({            #Selecting x and y variables for clustering
    mtcars[, c(as.numeric(input$var1),as.numeric(input$var2))]
  })
  
  clusters <- reactive({              #Performing K means
    kmeans(selectedData(), input$clusters)
  })
  
  
  
 
  l<-list("mpg"=1,"cyl"=2,"","hp"=4,"","wt"=6,"","","am"=9,"","")
  t<-names(l)

   output$scatter<- renderPlot({          #Creating a plot with clusters
     
     palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
               "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
     par(mar = c(5.1, 4.1, 0, 1))
     plot(selectedData(),
          col = clusters()$cluster,
          pch = 20, cex = 3)
     points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
     
     
  
   })
  
   
  output$down <-downloadHandler(           #downlaod handler
    
    filename =function()
       {
   paste("mtcars",input$var3, sep = ".")
     },
     content =function(file)
       {
       if(input$var3 == "png")
         png(file)
       else
        pdf(file)
       
         plot(x(),y())
         
         dev.off()
     }
  )
}


shinyApp(ui=ui, server=server)       #connecting the ui and server
