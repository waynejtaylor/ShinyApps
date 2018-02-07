library(shiny)
library(ggplot2)

#Create base plot
z = seq(-3,3,length.out = 1001)
y = dnorm(z)
zdat = data.frame(z=z,y=y)
zdatPlotPhat    = zdat
zdatPlotAlpha   = zdat
zPlot = ggplot(data = zdat, aes(z,y)) + geom_line(size = 1) + theme_bw(20) +
  scale_x_continuous("z-statistic",breaks=c(0,round(qnorm(c(.025,.975)),2)))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
   # Application title
   titlePanel("Two-Tailed Hypothesis Testing on Proportions"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         
        sliderInput("n",
                     "Sample Size: n",
                     min   = 30,
                     max   = 200,
                     value = 50,
                     step  = 1),
         
        checkboxInput("inc_pval",
                      label = "Show P-value region? (in blue)",
                      value = TRUE),
        
         sliderInput("phat",
                     "Sample Proportion (phat)",
                     min   = .2,
                     max   = .8,
                     value = .66,
                     step  = .01),
        
        checkboxInput("inc_alpha",
                      label = "Show significance region? (in red)",
                      value = TRUE),
        
        sliderInput("alpha",
                    "Alpha (a)",
                    min   = 0,
                    max   = .3,
                    value = .05,
                    step  = .01),
        
        sliderInput("p0",
                    "Null Hypothesis (p0)",
                    min   = .4,
                    max   = .6,
                    value = .5,
                    step  = .01),
        
        actionButton("reset", "Reset all slider values")
      ),
      
      # Display the output
      mainPanel(
        plotOutput("outGraphp"),
        fluidRow(
          column(6,
            h5(htmlOutput("outZScore")),
            h4(htmlOutput("decision"))
          )
        ),
        htmlOutput("intro")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$outGraphp = renderPlot({
      
      phatsd     = sqrt(input$p0*(1-input$p0)/input$n)
      phatzscore = (input$phat - input$p0)/phatsd
      
      
      #phats = seq(p0 - 3*phatsd,p0 + 3*phatsd,length.out = 1001)
      phats = seq(.2,.8,length.out = 1001)
      yp = dnorm(phats,mean=input$p0,sd=phatsd)
      pdat = data.frame(p=phats,y=yp)
      pdatPlotPhat    = pdat
      pdatPlotAlpha   = pdat
      pstar           = input$p0 + qnorm(c(input$alpha/2,1-input$alpha/2))*phatsd
      pdiff  = abs(input$phat- input$p0)
      pdiffs = c(input$p0 - pdiff, input$p0 + pdiff)
      pdatPlotPhat$y[phats >  pdiffs[1]& phats < pdiffs[2]]  = 0
      pdatPlotAlpha$y[phats > pstar[1] & phats < pstar[2]] = 0
      
      breakpoints = round(c(input$p0,range(phats),pdiffs,pstar),2)
      
      pPlot = ggplot(data = pdat, aes(p,yp)) + geom_line(size = 1) + theme_bw(20) +
        scale_x_continuous("phat",breaks=breakpoints) + ggtitle("Sampling distribution of phat")
      
      ggOutp = pPlot +
        geom_vline(xintercept = pdiffs, color = "dodgerblue") +
        geom_vline(xintercept = pstar, color = "red") + 
        geom_vline(xintercept = input$p0,linetype = "dashed") + 
        theme(axis.text.x  = element_text(angle=270,vjust = .5),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title.x=element_blank())
      
      if(input$inc_pval)  ggOutp = ggOutp + geom_ribbon(data=pdatPlotPhat ,aes(ymin=0,ymax=y),fill="dodgerblue",alpha=.4)
      if(input$inc_alpha) ggOutp = ggOutp + geom_ribbon(data=pdatPlotAlpha,aes(ymin=0,ymax=y),fill="red",alpha=.4)
      
      print(ggOutp)
    })
  
    observeEvent(input$reset, {
      updateSliderInput(session,"n",    value=50)
      updateSliderInput(session,"phat", value=.58)
      updateSliderInput(session,"alpha",value=.05)
      updateSliderInput(session,"p0",   value=.5)
    })
    
    # output$outGraphz = renderPlot({
    #   
    #   phatsd     = sqrt(input$p0*(1-input$p0)/input$n)
    #   phatzscore = (input$phat - input$p0)/phatsd
    #   zstar      = qnorm(1-input$alpha)
    #   zdatPlotPhat$y[z<=phatzscore]  = 0
    #   zdatPlotAlpha$y[zdat$z<=zstar] = 0
    #   
    #   ggOutz = zPlot +  
    #     geom_ribbon(data=zdatPlotPhat ,aes(ymin=0,ymax=y),fill="dodgerblue",alpha=.4) +
    #     geom_ribbon(data=zdatPlotAlpha,aes(ymin=0,ymax=y),fill="red",alpha=.4)
    #   print(ggOutz)
    # })
   
   output$intro  = renderPrint({
     HTML(paste("Given a null hypothesis p0 and the sample size n, we can produce a sampling distribution of phat.
                This is the distribution of phats we would expect to see based on many samples of size n (the black curve).
                Of course, we don't observe many samples, we only observe one sample and one estimate of phat.",'<br/>','<br/>',
                
                "When conducting a hypothesis test on a sample proportion phat, we are trying
                to determine whether or not the observed phat is unusual if the null hypothesis is true.
                If the observed phat appears to be unusual, we reject the null hypothesis.",'<br/>','<br/>',
                
                "The BLUE shaded area represents the P-value. This is the probability of observing the phat we saw in the sample
                or a phat that is even more extreme. When this area is small (that is, the p-value is low), it means that the observed phat is very surprising given
                our null hypothesis.",'<br/>','<br/>',
                
                "The RED shaded area represents the significance level of the test, denoted by alpha. An alpha of .05 is simply capturing the
                5% contained in the tails of the sampling distribution. The alpha allows us to quantify a cutoff point
                for when to be ''surprised'' by the one phat we do observe. In other words, if the phat falls in the red area, we are surprised by the result.
                An alpha of .05 is most common, but other common values are .1 and .01.",'<br/>','<br/>',
                
                "Now just compare the verticle blue lines with the verticle red lines. If the blue lines are further out than the red lines
                we would reject the null hypothesis: the observed phat is more extreme than our cutoff point.",'<br/>','<br/>',
                
                "Try the following:",'<br/>','<br/>',
                
                "-Notice how increasing the sample size will ''tighten up'' the sampling distribution. When n becomes 200 a phat of .58 is surprising
                but when n = 50 it is not surprising.",'<br/>','<br/>',
                
                "-Notice how increasing the observed phat will increase the P-value and move the blue lines closer to the red lines. With more extreme
                phats, it is more likely that we will reject the null hypothesis.",'<br/>','<br/>',
                
                "-Notice how when we increase alpha the red lines move in towards the null hypothesis. Increasing the alpha can be thought of as decreasing
                the confidence in the hypothesis test. There is a point where alpha will be large enough that a phat of .58 is rejected (.26 when n = 50).
                Obviously, the risk of reducing the confidence is that we are more likely to reject the null hypothesis when it is in fact true.",'<br/>','<br/>',
                
                "The bottom line is that we will never know the true value of the null hypothesis. But by using the properties of the sampling distribution
                we can make decisions based on this uncertainty in addition to quantifying our degree of confidence."
                
                # "In this example, let's pretend we are blah blah",
                # 
                # "Our decision is based on whether the poll reveals that significantly more or less than half
                # of the respondents vote one way or the other. Any outcome that is not different that .5 means
                # we will keep things as",
                # 
                # "What we will do is take the vote, tally the results, and ask the question: if in the population the true proportion of people
                # who vote yes is .5, then is the proportion of yes votes in the sample surprising?",
                # 
                # "To set up the test, we first set our null hypothesis p0 to .5. Which is interpreted as: the true proportion of the population that would vote yes is .5",
                # 
                # "We then ask n people to respond and after tallying the results we find that the proportion of people that say yes is phat.",
                # 
                # "Now we get to the main question: If the true population proportion is p0, is the outcome from this one sample surprising?",
                # 
                # "Remember, there will always be variability in the phat we collect because the sampling process is random",
                # 
                # "However, we know that we can characterize the shape of the sampling distribution using the following",
                # 
                # "phat ~ N(p,sqrt(p*(1-p)/n))",
                # 
                # "When we set up the null hypthesis, we assume that the null hypthosis is in fact true, so we treat p0 as p",
                # 
                # "Now, we can compare phat to p by figuring out how many SD's it is away from p0",
                # 
                # "If it is highly unlikely that phat would be observed then we can reject the null hypothesis",
                # 
                # "The P-value measures the likelihood that we would observe the phat that we observed or a phat that is more extreme. This is the blue shaded area",
                # 
                # "Now that we know the P-value, we need a cutoff point for our decision. This is where the alpha comes into play",
                # 
                # "The alpha is the significance of the test. The most common value of alpha is .05, which corresponds to 95% confidence in the test.",
                # 
                # "The interpretation is that ",
                # 
                # "Try different things."
                ))
   })
   
   output$outZScore = renderPrint({
        phatsd     = sqrt(input$p0*(1-input$p0)/input$n)
        phatzscore = (input$phat - input$p0)/phatsd
        pstar      = input$p0 + qnorm(c(input$alpha/2,1-input$alpha/2))*phatsd
        # if(abs(phatzscore)>qnorm(1-input$alpha/2)){
        #   decision = "Reject the null hypothesis"
        # } else {
        #   decision = "Fail to reject the null hypothesis"
        # }
        HTML(paste(
          # "n: ", input$n, '<br/>',
          # "phat: ", input$phat, '<br/>',
          # "alpha: ", input$alpha, '<br/>',
          # "p0: ", input$p0, '<br/>',
          "SD(phat): ", round(phatsd,2), '<br/>',
          "Z-score: ", round(phatzscore,2), '<br/>',
          "P-value: ", round(2*pnorm(-abs(phatzscore)),3), '<br/>',
          "Cutoff value for phat: ", round(pstar[1],2), "and", round(pstar[2],2), '<br/>'
          ))
    })
   
   output$decision = renderPrint({
     phatsd     = sqrt(input$p0*(1-input$p0)/input$n)
     phatzscore = (input$phat - input$p0)/phatsd
     if(abs(phatzscore)>qnorm(1-input$alpha/2)){
       decision = "Reject the null hypothesis"
     } else {
       decision = "Fail to reject the null hypothesis"
     }
     HTML(paste("Decision: ", decision, '<br/>'))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)