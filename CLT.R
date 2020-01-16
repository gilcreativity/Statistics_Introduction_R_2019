# Reference: http://blog.anilz.net/2016/08/central-limit-theorem-clt-simulation_8.html

library(shiny) #loading shiny package

#user interface code [start]
ui <- fluidPage(
  h1("Central Limit Theorem - CLT"), #heading
  
  #sidebar code [start]
  sidebarLayout(
    
    sidebarPanel(
      #sidebar input codes
      sliderInput("pop_size",
                  label = "Population Size",
                  value = 2000, min = 1000, max = 100000),
      textInput("smpl_size",
                label = "Enter Sample Size",
                value = 50),
      selectInput("dist", label = "Distribution ",
                  choices = c("Uniform" = "unif", "Normal" = "norm", "Exponential"="exp"), 
                  selected = "norm"),
      sliderInput("smpl_iterate",
                  label = "Sampling Iteration",
                  value = 200, min = 100, max = 10000)
    ),
    #sidebar code [end]
    
    #main panel code [start]
    mainPanel(
      tabsetPanel(
        
        #plot tab
        tabPanel("Plot",
                 plotOutput("plot_pop"), #plotting histogram and density plot for population
                 plotOutput("plot_smpl_mean")), #plotting histogram and density plot for sample
        
        #data tab
        tabPanel("Data", 
                 h4("Population"), #heading for the population summary and descriptive statistics
                 verbatimTextOutput("pop_summary"), #rendering population summary statistics
                 verbatimTextOutput("pop_structure"), 
                 h4("Sample"), #heading for the sample mean summary and descriptive statistics
                 verbatimTextOutput("smpl_mean_summary"), #rendering sample summary statistics
                 verbatimTextOutput("smpl_mean_structure"))
      )
    )
  )
  #main panel code [end]
)
#user interface code [end]

#server side code [start]
server <- function(input, output, session){
  #defining population function for storing population data
  population <- reactive({ #here reactive() function executes the code inside {} and render data to population object
    if (input$dist == "norm") {rnorm(input$pop_size)} #checking whether we have chosen Normal distribution
    else if (input$dist == "unif") {runif(input$pop_size)} #checking whether we have chosen Uniform distribution
    else if (input$dist == "exp") {rexp(input$pop_size)} #checking whether we have chosen Exponential distribution
  })
  
  #defining sample mean function for storing sample mean data
  smpl_mean <- reactive({ #here reactive() function executes the code inside {} and render data to smpl_mean object
    for (i in 1:input$smpl_iterate) {
      if (i==1) {
        smpl_mean <- c(mean(sample(population(), input$smpl_size, replace = TRUE ))) #creating object for the first time
      } else {
        smpl_mean <- c(smpl_mean,mean(sample(population(), input$smpl_size, replace = TRUE ))) #apending data to existing smpl_mean object
      }
    }
    smpl_mean #printing smpl_mean object in order to return via reactive function to main smpl_mean object
  })
  
  
  #rendering summary statistics and data information of population and sample mean data
  output$pop_summary <- renderPrint({summary(population())})
  output$pop_structure <- renderPrint({str(population())})
  output$smpl_mean_summary <- renderPrint({summary(smpl_mean())})
  output$smpl_mean_structure <- renderPrint({str(smpl_mean())})
  
  
  #rendering plots
  output$plot_pop <-renderPlot({
    plot(density(population()),axes=FALSE,xlab="",ylab="",main="") #density plot without axes, title, lable
    par(new=TRUE) #telling R to overlap next plot to existing plot
    hist(population(), main="Population histogram and density plot", xlab = "") #ploting histogram
    abline(v = mean(population()), col = "blue", lwd = 2) #ploting straight vertical blue line for mean
  })
  
  #simlar code as above for sample plot
  output$plot_smpl_mean <-renderPlot({
    plot(density(smpl_mean()),axes=FALSE,xlab="",ylab="",main="")
    par(new=TRUE)
    hist(smpl_mean(), main="Sample mean histogram and density plot", xlab = "")
    abline(v = mean(smpl_mean()), col = "blue", lwd = 2)
  })
  
}

shinyApp(ui, server)
