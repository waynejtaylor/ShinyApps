library(shiny)
library(ggplot2)

#In this section, the code is executed each time the app is launched
nstart = 50
Mstart = 1
Mmax   = 200
ggDatC = data.frame(y = c(NA,NA),phat = c(NA,NA),CIlb = c(NA,NA),CIub = c(NA,NA), coverPind = c(FALSE,TRUE))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
   # Application title
   titlePanel("Confidence Intervals"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         
        sliderInput("conf",
                    "Confidence Level",
                    min   = .5,
                    max   = 1,
                    value = .95,
                    step  = .01),
        
        checkboxInput("show_p",
                      label = "Show the true value p? (dashed blue line)",
                      value = FALSE),
        
        sliderInput("n",
                     "Sample Size: n",
                     min   = 50,
                     max   = 1000,
                     value = nstart,
                     step  = 50),
        
        sliderInput("M",
                    "Number of samples",
                    min   = 1,
                    max   = Mmax,
                    value = Mstart,
                    step  = 1),
        

        
        actionButton("resample", "Generate new p")
      ),
      
      # Display the output
      mainPanel(
        plotOutput("outGraphCI"),
        # fluidRow(
        #   column(6,
        #     h5(htmlOutput("outCI"))
        #   )
        # ),
        htmlOutput("intro")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    #In this section, the code is executed each time the app is loaded in a browser
    newP = eventReactive(input$resample, {
      runif(1,.2,.8)
    },ignoreNULL =  FALSE)
    
    newN = reactive({
      p      = newP()
      rbinom(Mmax,input$n,p)
    })
    
    output$outGraphCI = renderPlot({
      
      p      = newP()
      XmanyA = newN()
      phatsA = XmanyA/input$n
      
      Xmany = XmanyA[1:input$M]
      phats = phatsA[1:input$M]
      
      #Get CI
      SEphats    = sqrt(phats*(1-phats)/input$n)
      confidence = input$conf
      alpha      = 1-confidence
      zscore     = qnorm(1-alpha/2)
      CIlb       = phats - zscore*SEphats
      CIub       = phats + zscore*SEphats
      coverPind  = CIlb < p & p < CIub
      coveragePercent = sum(coverPind)/input$M
      
      ggDat = data.frame(y = seq(0,1,length.out = input$M),phat = phats,CIlb = CIlb,CIub = CIub, coverPind = coverPind)
      
      #when true p is unknown, we will not know if the confidence interval covers p
      if(!input$show_p) ggDat$coverPind = TRUE
      
      ggDat = rbind(ggDatC,ggDat)
      
      ggOut = ggplot(ggDat,aes(x=phat,y=y,color=coverPind)) + geom_point() + xlim(0,1) + 
        geom_segment(aes(x = CIlb, y = y, xend = CIub, yend = y)) +
        theme_bw(20) + ylab("") + xlab("Proportion") + 
        theme(panel.grid.minor=element_blank(),
              panel.grid.major=element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
      
      if(input$show_p) {
        ggOut = ggOut + geom_vline(xintercept = p, color = "dodgerblue", alpha = .8, linetype = "dashed") +
        scale_color_manual(values=c("red","black"),
                           name="Does the CI \n cover the \n population p?",
                           breaks=c(FALSE,TRUE),
                           labels=c("No","Yes"))
      } else {
        ggOut = ggOut + scale_color_manual(values=c("black","black"),
                                   name="Does the CI \n cover the \n population p?",
                                   breaks=c(TRUE),
                                   labels=c("We don't know"))
      }
      
      suppressWarnings(print(ggOut)) #suppresses warning from missing color rows
   })
  
   output$intro  = renderPrint({
     HTML(paste("Using our sample observations, we can construct confidence intervals.
                A confidence interval gives us a plausible ranges for the unknown p based
                on the observed phat. It is a range of values that we ``think`` includes the true value p.",'<br/>','<br/>',
                
                "For example, about 95% of random samples will generate a phat that is within
                about 1.96 standard errors of p.",'<br/>','<br/>',
                
                "Remember, this represents the variation in the sample statistic phat and NOT the variation
                in the sample data or the true value p (which is a fixed number).",'<br/>','<br/>',
                
                "Using the sliders on the left, try this:",'<br/>','<br/>',
                
                "-Notice what happens when we increase the confidence level. Higher confidence levels
                result in a wider range of values. Of course, you can be 100% confident that a proportion is between
                0 and 1.",'<br/>','<br/>',
                
                "-What happens when you increase the sample size n? You'll see that the confidence interval shrinks.
                IMPORTANT: if the sample size changes a new sample is redrawn, so focus on the lines coming off the point,
                not the location of the point if you are adjusting this slider.",'<br/>','<br/>',
                
                "-As you increase the number of samples M we can see what happens IF you had observed sampled the n-sized data M times from
                a population with fixed p. The size of the confidence interval remains the same but of course the observed phat will
                depend on the individual sample.",'<br/>','<br/>',
                
                "-Finally, check the box to see what the true underlying p is. With many samples, you can see that the confidence intervals
                of about 95% of them contain the true value p. The true value of p is never known from one sample, but the confidence intervals
                inform us of where it might be based on the observed phat and the size of the sample.",'<br/>','<br/>'
                
                ))
   })
   
   # output$outCI = renderPrint({
   #       p      = newP()
   #       XmanyA = newN()
   #       phatsA = XmanyA/input$n
   #       
   #       Xmany = XmanyA[1:input$M]
   #       phats = phatsA[1:input$M]
   #   
   #       #Get CI
   #       SEphats    = sqrt(phats*(1-phats)/input$n)
   #       confidence = input$conf
   #       alpha      = 1-confidence
   #       zscore     = qnorm(1-alpha/2)
   #       CIlb       = phats - zscore*SEphats
   #       CIub       = phats + zscore*SEphats
   #       coverPind  = CIlb < p & p < CIub
   #       coveragePercent = sum(coverPind)/input$M
   #   
   #      HTML(paste(
   #        "SE(phat): ", round(SEphats,3), '<br/>',
   #        "Confidence Level: ", confidence*100, "%", '<br/>',
   #        "Alpha: ", alpha, '<br/>',
   #        "Z-score: ", round(zscore,2),sep=""))
   # })
}

# Run the application 
shinyApp(ui = ui, server = server)