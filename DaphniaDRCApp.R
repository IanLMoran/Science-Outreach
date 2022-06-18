library(tidyverse)
library(drc)
library(shiny)

##Example data and plot

dose <- c(0,1.7,2.1,2.8,4.3,8.5)
response <- c(10,9,8,3,1,0)


exdata1<- data.frame(dose,response)



n <- c(rep(10,6))


ex.m1 <- drm(response*10~dose,
          weights = n,
          data=exdata1,
          fct=LL2.4(fixed = c(NA,NA,100,NA)),
          type = "binomial")

oak.mort <- 9

plot(ex.m1)
abline(90,0, col = "blue")
text(7.5,(0.3+oak.mort)*10,labels = "Oak Creek", col = "blue")

exp(round(ex.m1$coefficients[3],1))


###Creating RShiny app
#Inputs: Doses, Responses
#Outputs: Plot

#Add elements as arguments to fluidpage


ui <- fluidPage(
  titlePanel("Plot Dose Response Curves for Daphnia Data"),
  fluidRow(
    column(3,
           wellPanel(
      numericInput(inputId = "c1",
                   label = "Concentration in Treatment #1",
                   value = 0,
                   min = 0,
                   max = 10,
                   step = 1),
      numericInput(inputId = "c2",
                   label = "Concentration in Treatment #2",
                   value = 1.7,
                   min = 0,
                   max = 100,
                   step = 1),
      numericInput(inputId = "c3",
                   label = "Concentration in Treatment #3",
                   value = 2.1,
                   min = 0,
                   max = 100,
                   step = 1),
      numericInput(inputId = "c4",
                   label = "Concentration in Treatment #4",
                   value = 2.8,
                   min = 0,
                   max = 100,
                   step = 1),
      numericInput(inputId = "c5",
                   label = "Concentration in Treatment #5",
                   value = 4.3,
                   min = 0,
                   max = 10,
                   step = 1),
      numericInput(inputId = "c6",
                   label = "Concentration in Treatment #6",
                   value = 8.5,
                   min = 0,
                   max = 100,
                   step = 1),
           ),
      ),
    column(3,
           wellPanel(
           numericInput(inputId = "r1",
                        label = "# Living Daphnia in Treatent #1",
                        value = 10,
                        min = 0,
                        max = 10,
                        step = 1),
           numericInput(inputId = "r2",
                        label = "# Living Daphnia in Treatent #2",
                        value = 9,
                        min = 0,
                        max = 10,
                        step = 1),
           numericInput(inputId = "r3",
                        label = "# Living Daphnia in Treatent #3",
                        value = 8,
                        min = 0,
                        max = 10,
                        step = 1),
           numericInput(inputId = "r4",
                        label = "# Living Daphnia in Treatent #4",
                        value = 3,
                        min = 0,
                        max = 10,
                        step = 1),
           numericInput(inputId = "r5",
                        label = "# Living Daphnia in Treatent #5",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = 1),
           numericInput(inputId = "r6",
                        label = "# Living Daphnia in Treatent #6",
                        value = 0,
                        min = 0,
                        max = 10,
                        step = 1),
           numericInput(inputId = "stream.mort",
                        label = "# Living Daphnia in Treatment #7 (Oak Creek Water)",
                        value = 9,
                        min = 0,
                        max = 10,
                        step = 1)
           ),
    ),
    column(6,
           verbatimTextOutput("narrative"),
           textInput(inputId = "usertitle",
                     label = "Create a title for your plot",
                     value = "ex) Ammonia induced mortality in Daphnia"),
      plotOutput("drc"),
      verbatimTextOutput("EC1text"),
      textOutput("EC1"),
      )
  )
)


#Tell server how to how to assemble inputs into outputs
##Save objects to output$ ex. output$drc
##Build objects with render() ex. renderPlot() (convert R instruction to HTML for shiny)
### {} passed to renderPlot contain R code to create plot
##Use input values with input$ (inputs from user ex. value on slider bar)
###this within renderplot allows user to change values used to generate plot ex variable to plot
server <- function(input,output) {
  
  #build a reactive objects for data
  output$drc <- renderPlot({
    dose1 <- c(input$c1,input$c2,input$c3,input$c4,input$c5,input$c6)
    response1 <- c(input$r1,input$r2,input$r3,input$r4,input$r5,input$r6)
    
    data1 <- data.frame(dose1,response1)
    
    n=c(rep(10,6))
    
    m1 <- drm(response1*10~dose1,
              weights=n,
              data=data1,
              fct=LL.4(fixed = c(NA,NA,100,NA)),
              type="binomial")

    
    plot(m1,
         log = "",
         main=input$usertitle,
         xlim = c(0,10),
         ylim = c(0,100),
         xlab = "Dose (mg/L)",
         ylab = "Percent Survival (%)")
    abline(input$stream.mort*10,0,col = "blue")
    text(7.5,(0.3+input$stream.mort)*10,labels = "Oak Creek", col = "blue")
  })
  
  output$EC1text <- renderText({paste0("LC50 for chemical A (Black dose response curve)")})

  ###
  ## In order for output$EC1 to be reactive, the inputs must be repeated and embedded in this code chunk as well
  ####
  output$EC1 <- renderText({
    dose1.1 <- c(input$c1,input$c2,input$c3,input$c4,input$c5,input$c6,input$c7)
    response1 <- c(input$r1,input$r2,input$r3,input$r4,input$r5,input$r6,input$r7)
    
    data1 <- data.frame(dose1.1,response1)
    
    n=c(rep(input$n,7))
    
    m1 <- drm(response1*10~dose1.1,
              weights=n,
              data=data1,
              fct=LL.4(fixed = c(NA,NA,100,NA)),
              type="binomial")
    
    paste0(round(m1$coefficients[3],1)," mg/L")
    })

  
  output$narrative <- renderText({paste0("Input daphnia data:\n
You can use this program to input data from your group's experiment\n
Enter concentrations for each treatment in units of mg ammonia per liter water (mg/L)\n

Enter the number of surviving daphnia at the end of the experiment\n
Finally, enter a title for your plot\n
\n
Results: The LC50 (Lethal concentration) represents the modeled concentration required to reduce survival by 50%.\n
Lower LC50 values mean that less chemical is required to cause 50% reduction in survival.\n\
Therefor, a chemical with a lower LC50 is more toxic.")})
} 



shinyApp(ui = ui, server = server)


#Use R file and put that file and any supporting files into a directory (file folder)
#Use Shinyapps.io visit website and upload to their servers
# 33 minutes in discusses uploading

#To update app see :https://stackoverflow.com/questions/43981572/update-shiny-app-data-without-republishing-app
#Save working version as app.R and place in directory
#in console, run library(rsconnect) followed by rsconnect::deployApp("R:\\EMT\\Anderson-Lab\\Share\\Moran, Ian\\TEAMTox\\Outreach Tools\\Dapnia DRC\\Daphnia DRC App")
#Then type Y

#Find application at https://ianlmoran.shinyapps.io/daphnia_drc_app/