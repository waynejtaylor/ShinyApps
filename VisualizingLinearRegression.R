library(shiny)
library(ggplot2)

#Simulate data
set.seed(1)
n = 25
beta = c(-2,1)
X    = cbind(1,runif(n,1,10))
y    = X%*%beta + rnorm(n)

#Best fit line
betahat = solve(crossprod(X))%*%crossprod(X,y)
reshat  = y - X%*%betahat
x       = X[,2]

#ggplot data
ggDat     = data.frame(y=y,X=x)
ggDatPlot = ggplot(ggDat) + geom_point(aes(x=X,y=y)) + theme_bw(20)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
   # Application title
   titlePanel("Visualizing Sum of Squared Residuals"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         
        sliderInput("b0",
                     "Intercept:",
                     min   = round(betahat[1]-5),
                     max   = round(betahat[1]+5),
                     value = betahat[1],
                     step  = .05),
         
         sliderInput("b1",
                     "Slope:",
                     min   = round(betahat[2]-5),
                     max   = round(betahat[2]+5),
                     value = betahat[2],
                     step  = .05),
        
        checkboxInput("inc_bfl",
                      label = "Include best fit line",
                      value = FALSE),
        
        actionButton("fit_line", "Reset to best fit line")
      ),
      
      # Display the output
      mainPanel(
        htmlOutput("intro"),
        fluidRow(
          column(3,
            h4(textOutput("outTitle")),
            h5(htmlOutput("out"))
          ),
          column(3,
            h4(textOutput("bfTitle")),
            h5(htmlOutput("bfOut"))
          )
        ),
        plotOutput("scatterPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

   output$scatterPlot = renderPlot({
     res =  y - X%*%c(input$b0,input$b1)
     ggOut = ggDatPlot + 
       geom_rect(xmin = x, xmax = x-res, ymin=y-res, ymax=y, alpha=.15, fill="dodgerblue") +
       geom_abline(intercept = input$b0,slope = input$b1, size = 1)
     
     if(input$inc_bfl){
       print(ggOut + geom_abline(intercept = betahat[1],slope = betahat[2],color = "red",linetype = 2,size = 1))
     } else {
       print(ggOut)
     }
     
  })
   
   output$intro  = renderPrint({
     HTML(paste("The goal of fitting a regression model is to minimize the sum of the squared residuals (SSE), or the squared difference
                between the predicted and actual values. The blue shaded areas represent the total value of the squared residuals based on the slider values to the left.",'<br/>',
                "Try different intercept and slope values and compare them to the best fit line."
                ))
   })
   
   output$outTitle  = renderText({
     paste("Your line:")
   })
   
   output$out  = renderPrint({
      res = y - X%*%c(input$b0,input$b1)
      HTML(paste("SSE: ", round(sum(res^2),2), '<br/>',
                 "Intercept: ", round(input$b0,2),'<br/>',
                 "Slope: ", round(input$b1,2),'<br/>'))
   })
   
   output$bfTitle  = renderText({
     if(input$inc_bfl) paste("Best fit line:")
   })
   
   output$bfOut  = renderText({
     if(input$inc_bfl) {
       HTML(paste("SSE: ", round(sum(reshat^2),2), '<br/>',
                  "Intercept: ", round(betahat[1],2),'<br/>',
                  "Slope: ", round(betahat[2],2),'<br/>'))
       }
   })
   
   observeEvent(input$fit_line, {
     updateSliderInput(session,"b0",value=betahat[1])
     updateSliderInput(session,"b1",value=betahat[2])
     updateCheckboxInput(session,"inc_bfl",value=TRUE)
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)