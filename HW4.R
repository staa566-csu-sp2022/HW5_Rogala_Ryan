#---------------------------#
#load packages from library. 
#---------------------------#
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(kableExtra)
library(datasets)
library(knitr)
library(reactable)

#read in data
df <- mtcars %>% mutate(cyl=as.factor(cyl), 
                        wt_class=as.factor(as.character((
                          if_else(wt <= 2.5, "Light", 
                                  if_else(wt <= 3.5, "Medium",
                                          "Heavy"))))),
                        vs = as.factor(vs),
                        gear=as.integer(gear),
                        carb=as.integer(carb),
                        vs = factor(vs, labels = c("V-shape", "Strait")),
                        am = factor(am, labels = c("Automatic", "Manual")))

#---------------------------#
#Set up UI
#---------------------------#
ui <- dashboardPage(
  # format
  skin="blue",

  # set title
  dashboardHeader(
    title="Automobile Performance"
    ),
  
  # set sidebar filter
  dashboardSidebar(
        selectInput(inputId="in_wt_class", 
                    label="Select weight class:",
                    choices = c(unique(as.character(df$wt_class))),
                    multiple = T,
                    selected = c("Light","Medium","Heavy")
                   )
    ),
  
  #set body
  dashboardBody(
      box(plotlyOutput("plot1"), width=500, collapsible = T, collapsed = F),
      box(tableOutput("tab1"), width=500, collapsible = T, collapsed = F)
  )
)

#---------------------------#
# set up server function
#---------------------------#
server <- function(input, output) {
  
  #create point plot.
  output$plot1 <- renderPlotly({
    plot1 <- 
      ggplot() + 
      geom_point(data=df %>% filter(wt_class %in% input$in_wt_class),
                 aes(x=wt, y=mpg, col=wt_class), alpha=0.7, size=3) + 
      geom_smooth(data=df, 
                  aes(x=wt, y=mpg), 
                  method="gam", 
                  col="light grey",
                  se=F, 
                  formula = y ~ splines::bs(x, 3)) +
      xlab("Weight (1,000 lbs)") +
      ylab("Miles per gallon") +
      ggtitle("Impact of automobile weight on fuel efficiency") +
      labs(color="Weight Class")
    
    #convert to plotly 
    plot1 <- ggplotly(plot1)
    
  })

  
  # Create table 
  output$tab1 <- renderTable({

    #get summary stats for table. 
    tab1 <-
    df %>% group_by(wt_class, vs) %>% 
    summarise(
      n=n(),
      avg_mpg=round(mean(mpg), 1),
      avg_wt=round(mean(wt*1000)),
      avg_hp=round(mean(hp)),
      avg_disp=round(mean(disp)),
      avg_qsec=round(mean(qsec), 2)) %>% 
      data.frame()
        
    colnames(tab1) <-c(
                      "Weight Class",
                      "Engine",
                      "Observations",
                      "Avg MPG",
                      "Avg Weight",
                      "Avg HP",
                      "Avg Displacment",
                      "Avg 1/4 Mile Time")
    
    tab1
  })
}



shinyApp(ui, server)
