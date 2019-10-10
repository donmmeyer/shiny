library(shiny)

library(ggplot2)
library(dplyr)
library(DT)

setwd("C:\\R_programs\\shiny\\dcm1")


pw <- read.csv("partworth1.csv",header=TRUE)
str(pw)

# Define UI for the application
ui <- fluidPage(
  tags$style(my_css),
  # Add a sidebar layout to the application
  sidebarLayout(
    # Add a sidebar panel around the text and inputs
    sidebarPanel(
      h3("Intramuscle"),
      selectInput("admin1", "Person Administering", 
        choices = c("Healthcare professional"  = 0
                    ,"Self"                    = 1
                    ,"Community Health Worker" = 2 )),
      selectInput("timefert1", "Time to Fertility", 
                  choices = c("4 months"  = 0
                              ,"12 months"  = 1 )),

      selectInput("pain1", "Pain", 
                  choices = c("Pain present, but can be easily ignored"  = 0
                              ,"Pain present, cannot be ignored, interferes with concentration"  = 1
                              ,"Pain present, cannot be ignored but does not interfere with everyday activities" = 2 )),    
      selectInput("skin1", "Skin Reaction", 
                  choices = c("Little to no visible sign"  = 0
                              ,"Redness and/or swelling and/or stinging for several days"  = 1
                              )),     
      selectInput("access1", "Access", 
                  choices = c("Pharmacy"  = 0
                              ,"Health post/health care center"  = 1
                              ,"Community health care worker"  = 2
                  )),     
      selectInput("freq1", "Frequency of Injection", 
                  choices = c("Every 3 months"  = 0
                              ,"Every 6 months"  = 1
                  )),           
      selectInput("loc1", "Location of Injection", 
                  choices = c("Upper Arm"  = 0
                              ,"Abdomem"  = 1
                              ,"Thigh"  = 2
                  )),           
      h3("Subcutaneous"),
      selectInput("admin2", "Person Administering", 
                  choices = c("Healthcare professional"  = 0
                              ,"Self"                    = 1
                              ,"Community Health Worker" = 2 )),
      selectInput("timefert2", "Time to Fertility", 
                  choices = c("4 months"  = 0
                              ,"12 months"  = 1 )),
      
      selectInput("pain2", "Pain", 
                  choices = c("Pain present, but can be easily ignored"  = 0
                              ,"Pain present, cannot be ignored, interferes with concentration"  = 1
                              ,"Pain present, cannot be ignored but does not interfere with everyday activities" = 2 )),    
      selectInput("skin2", "Skin Reaction", 
                  choices = c("Little to no visible sign"  = 0
                              ,"Redness and/or swelling and/or stinging for several days"  = 1
                  )),     
      selectInput("access2", "Access", 
                  choices = c("Pharmacy"  = 0
                              ,"Health post/health care center"  = 1
                              ,"Community health care worker"  = 2
                  )),     
      selectInput("freq2", "Frequency of Injection", 
                  choices = c("Every 3 months"  = 0
                              ,"Every 6 months"  = 1
                  )),           
      selectInput("loc2", "Location of Injection", 
                  choices = c("Upper Arm"  = 0
                              ,"Abdomem"  = 1
                              ,"Thigh"  = 2
                  ))           
      
      
                )
      ,
    mainPanel(
      tableOutput("mytable"),
      verbatimTextOutput('values'),
      plotOutput("plot") 
       
  )
    
    
       # Add a main panel around the plot and table
 #   tabPanel(
 #     title = "Table",
#     DT::dataTableOutput("table")
#    )
  )
)
#varmark<-c(TRUE,  TRUE,  FALSE,   FALSE,    FALSE,   FALSE,    FALSE, FALSE, FALSE, FALSE, FALSE,   FALSE,  FALSE, FALSE,FALSE)
#         CASE_ID,inject_m,inject_f,Person_1, Person_2,time_fert,pain_1,pain_2,skin_1,skin_2,access_1,access_2,freq, loc_1,loc_2
#           1        2

# Define the server logic
server <- function(input, output) {


 summed_data <- reactive({ 
    resp_n = 288
    varmark<-c(TRUE,  TRUE,  FALSE,   FALSE,    FALSE,   FALSE,    FALSE, FALSE, FALSE, FALSE, FALSE,   FALSE,  FALSE, FALSE,FALSE)
    if (input$admin1 == 1) { varmark[4] <- TRUE } 
    else if (input$admin1 == 2) { varmark[5] <- TRUE }

    if (input$timefert1 == 1) { varmark[6] <- TRUE } 

    if (input$pain1 == 1) { varmark[7] <- TRUE } 
    else if (input$pain1 == 2) { varmark[8] <- TRUE }  

    if (input$skin1 == 1) { varmark[9] <- TRUE } 
    else if (input$skin1 == 2) { varmark[10] <- TRUE }      

  if (input$access1 == 1) { varmark[11] <- TRUE } 
  else if (input$access1 == 2) { varmark[12] <- TRUE }      

  if (input$freq1 == 1) { varmark[13] <- TRUE } 

  if (input$loc1 == 1) { varmark[14] <- TRUE } 
  else if (input$loc1 == 2) { varmark[15] <- TRUE } 
  
  data <-pw[1:resp_n,varmark]
  data <- cbind(data,0)  # need to add a column of 0s to make rowSums to work
  data$tsum1 <- exp(rowSums(data[,2:ncol(data)]))

  varmark<-c(TRUE,  FALSE,  TRUE,   FALSE,    FALSE,   FALSE,    FALSE, FALSE, FALSE, FALSE, FALSE,   FALSE,  FALSE, FALSE,FALSE)
  if (input$admin2 == 1) { varmark[4] <- TRUE } 
  else if (input$admin2 == 2) { varmark[5] <- TRUE }
  
  if (input$timefert2 == 1) { varmark[6] <- TRUE } 
  
  if (input$pain2 == 1) { varmark[7] <- TRUE } 
  else if (input$pain2 == 2) { varmark[8] <- TRUE }  
  
  if (input$skin2 == 1) { varmark[9] <- TRUE } 
  else if (input$skin2 == 2) { varmark[10] <- TRUE }      
  
  if (input$access2 == 1) { varmark[11] <- TRUE } 
  else if (input$access2 == 2) { varmark[12] <- TRUE }      
  
  if (input$freq2 == 1) { varmark[13] <- TRUE } 
  
  if (input$loc2 == 1) { varmark[14] <- TRUE } 
  else if (input$loc2 == 2) { varmark[15] <- TRUE } 

  
  data1 <-pw[1:resp_n,varmark]
  data1 <- cbind(data1,0)  # need to add a column of 0s to make rowSums to work
  data$tsum2 <- exp(rowSums(data1[,2:ncol(data1)]))
  
  data$m_sh <- data$tsum1/(data$tsum1 + data$tsum2 + 1)
  data$f_sh <- data$tsum2/(data$tsum1 + data$tsum2 + 1)
  data$oth <- 1 /(data$tsum1 + data$tsum2 + 1)
  
  mnum <- mean(data$m_sh)  * resp_n +59
  fnum <- mean(data$f_sh) * resp_n + 164
  onum <- mean(data$oth)  * resp_n   + 200
  
  intramuscular = 100 * mnum / (mnum + fnum + onum)
  subcutaneous = 100 * fnum / (mnum + fnum + onum)
  
  
  Type <- c(rep("Intramuscular",3),rep("Subcutaneous",3))
  
  Category  <- rep(c("Reference","Share","Difference"),2)
  
  Share <- c( 13.3,intramuscular,intramuscular - 13.3,46.5,subcutaneous,subcutaneous - 46.5)
  
  data <- data.frame(Type,Category,Share)
  
  data
                      
  })
 
tab_data <- reactive({ 
  data <- summed_data() 
 
   data <- data[data$Category != "Difference",]
   
     
    })
   

  output$values <- renderPrint({  tab_data()})  

  output$mytable <- renderTable({ summed_data()},digits = 1)  
  
 output$plot <- renderPlot({
   p <- ggplot(tab_data(), aes(fill=Bar, y=Share, x=Type)) + 
    geom_bar(position="dodge", stat="identity") + coord_flip() + scale_fill_grey() + theme_bw()
   p
  })  
  
 }




# Run the application
shinyApp(ui = ui, server = server)
