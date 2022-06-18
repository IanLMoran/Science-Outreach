library(tidyverse)
library(drc)
library(shiny)

###Example data and plot

dose <- c(0,25,50,75)
response <- c(9,7,2,0)

data <- data.frame(dose,response)
ggplot(data,aes(dose,response))+
  geom_point()

n=c(rep(10,4))

ex.m1 <- drm(response/10~dose,
          weights=n,
          data=data,
          fct=LL.4(),
          type="binomial")

plot(ex.m1)

##Example data and plot for 2 chemical DRC

exdose1 <- c(0,25,50,75)
exdose2 <- c(0,30,60,90)

exresponse1 <- c(10,7,3,0)
exresponse2 <- c(10,8,4,0)

exdata1<- data.frame(exdose1,exresponse1)
exdata2<- data.frame(exdose2,exresponse2)


n <- c(rep(10,4))


ex.m1 <- drm(exresponse1/10~exdose1,
          weights = n,
          data=exdata1,
          fct=LL.4(),
          type = "binomial")

ex.m2 <- drm(exresponse2/10~exdose2,
          weights = n,
          data = exdata2,
          fct = LL.4(),
          type = "binomial")

plot(ex.m1)
plot(ex.m2,add=TRUE)


###Creating RShiny app
#Inputs: Doses, Responses
#Outputs: Plot

#Add elements as arguments to fluidpage


ui <- fluidPage(
  titlePanel("Plot Dose Response Curves for Germination Data"),
  fluidRow(
    column(3,
           wellPanel(
      numericInput(inputId = "c1",
                   label = "Concentration Bag #1 (Control)",
                   value = 0,
                   min = 0,
                   max = 10,
                   step = 1),
      numericInput(inputId = "c2",
                   label = "Concentration Bag #2 (Chemical A)",
                   value = 20,
                   min = 0,
                   max = 100,
                   step = 1),
      numericInput(inputId = "c3",
                   label = "Concentration Bag #3 (Chemical A)",
                   value = 40,
                   min = 0,
                   max = 100,
                   step = 1),
      numericInput(inputId = "c4",
                   label = "Concentration Bag #4 (Chemical A)",
                   value = 80,
                   min = 0,
                   max = 100,
                   step = 1),
           ),
      numericInput(inputId = "c5",
                   label = "Concentration Bag #5 (Chemical B)",
                   value = 30,
                   min = 0,
                   max = 10,
                   step = 1),
      numericInput(inputId = "c6",
                   label = "Concentration Bag #6 (Chemical B)",
                   value = 60,
                   min = 0,
                   max = 100,
                   step = 1),
      numericInput(inputId = "c7",
                   label = "Concentration Bag #7 (Chemical B)",
                   value = 90,
                   min = 0,
                   max = 100,
                   step = 1),
      ),
    column(3,
           wellPanel(
           numericInput(inputId = "r1",
                        label = "# Germinated Bag #1 (Control)",
                        value = 10,
                        min = 0,
                        max = 10,
                        step = 1),
           numericInput(inputId = "r2",
                        label = "# Germinated Bag #2 (Chemical A)",
                        value = 7,
                        min = 0,
                        max = 10,
                        step = 1),
           numericInput(inputId = "r3",
                        label = "# Germinated Bag #3 (Chemical A)",
                        value = 2,
                        min = 0,
                        max = 10,
                        step = 1),
           numericInput(inputId = "r4",
                        label = "# Germinated Bag #4 (Chemical A)",
                        value = 0,
                        min = 0,
                        max = 10,
                        step = 1),
           ),
           numericInput(inputId = "r5",
                        label = "# Germinated Bag #5 (Chemical B)",
                        value = 9,
                        min = 0,
                        max = 10,
                        step = 1),
           numericInput(inputId = "r6",
                        label = "# Germinated Bag #6 (Chemical B)",
                        value = 4,
                        min = 0,
                        max = 10,
                        step = 1),
           numericInput(inputId = "r7",
                        label = "# Germinated Bag #7 (Chemical B)",
                        value = 2,
                        min = 0,
                        max = 10,
                        step = 1),
    ),
    column(6,
           verbatimTextOutput("narrative"),
           textInput(inputId = "usertitle",
                     label = "Create a title for your plot",
                     value = "ex) Coffee Inhibits Mung Bean Germination"),
      #numericInput(inputId = "n",
      #             label = "# Seeds in each bag",
      #             value = 10,
      #             min = 0,
      #             max = 100,
      #             step = 1),
      plotOutput("drc"),
      verbatimTextOutput("EC1text"),
      textOutput("EC1"),
      verbatimTextOutput("EC2text"),
      textOutput("EC2"),
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
    dose1 <- c(input$c1,input$c2,input$c3,input$c4)
    response1 <- c(input$r1,input$r2,input$r3,input$r4)
    
    data1 <- data.frame(dose1,response1)
    
    dose2 <- c(input$c1,input$c5,input$c6,input$c7)
    response2 <- c(input$r1,input$r5,input$r6,input$r7)
    
    data2 <- data.frame(dose2,response2)
    
    #n=c(rep(input$n,4))
    #Fix n at 10
    n=c(rep(10,4))
    
    m1 <- drm(response1*10~dose1,
              weights=n,
              data=data1,
              fct=LL.4(),
              type="binomial")
    
    m2 <- drm(response2*10~dose2,
              weights=n,
              data=data2,
              fct=LL.4(),
              type="binomial")
    
    plot(m1,
         log = "",
         main=input$usertitle,
         xlim = c(0,100),
         ylim = c(0,100),
         xlab = "Dose (Percent by volume)",
         ylab = "Percent Germination (%)")
    axis(side=1, at=c(0,10,20,30,40,50,60,70,80,90,100))
    
    plot(m2,add=TRUE, col = "red")
  })
  
  output$EC1text <- renderText({paste0("EC50 for chemical A (Black dose response curve)")})
  output$EC2text <- renderText({paste0("EC50 for chemical B (Red dose response curve)")})

  ###
  ## In order for output$EC1 to be reactive, the inputs must be repeated and embeded in this code chunk as well
  ####
  output$EC1 <- renderText({
    dose1.1 <- c(input$c1,input$c2,input$c3,input$c4)
    response1 <- c(input$r1,input$r2,input$r3,input$r4)
    
    data1 <- data.frame(dose1.1,response1)
    
    n=c(rep(input$n,4))
    
    m1 <- drm(response1*10~dose1.1,
              weights=n,
              data=data1,
              fct=LL.4(),
              type="binomial")
    
    round(m1$coefficients[4],1)
    })
  
  output$EC2 <- renderText({
    dose2 <- c(input$c1,input$c5,input$c6,input$c7)
    response2 <- c(input$r1,input$r5,input$r6,input$r7)
    
    data2 <- data.frame(dose2,response2)
    
    n=c(rep(input$n,4))
    
    m2 <- drm(response2*10~dose2,
              weights=n,
              data=data2,
              fct=LL.4(),
              type="binomial")
    round(m2$coefficients[4],1)
    })
  
  output$narrative <- renderText({paste0("Input germination data:\n
You can use this program to input data for one test chemical (bags 1-4)\n
or two test chemicals (bags 1-7) to compare the toxicity of two products\n
Enter concentrations for each bag as the percentage of test chemical by volume\n
 (ex 1 tsp coffee in 4 tsp total (coffee + water) = 25% coffee)\n
Enter total # of seeds germinated by day 3 for each bag\n
Finally, enter a title for your plot\n
\n
Results: The EC50 (Effective concentration) represents the modeled concentration required to reduce germination by 50%.\n
Lower EC50 values mean that less chemical is required to cause 50% reduction in germination.\n\
Therefor, a chemical with a lower EC50 is more toxic")})
} 



shinyApp(ui = ui, server = server)


#Use R file and put that file and any supporting files into a directory (file folder)
#Use Shinyapps.io visit website and upload to their servers
# 33 minutes in discusses uploading

#To update app see :https://stackoverflow.com/questions/43981572/update-shiny-app-data-without-republishing-app
#Save working version as app.R and place in directory
#in console, run library(rsconnect) followed by rsconnect::deployApp("R:\\EMT\\Anderson-Lab\\Share\\Moran, Ian\\TEAMTox\\Outreach Tools\\Germination DRC App")
#Then type Y

#Find application at https://ianlmoran.shinyapps.io/GerminationDRCApp/