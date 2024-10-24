library(shiny)
library(tidyverse)
library(plotly)

d1 <- readRDS('./ABCs_st_1998_2021.rds') %>%
  rename(agec = "Age.Group..years.",
         year=Year,
         st=IPD.Serotype,
         N_IPD = Frequency.Count) %>%
  mutate( st= if_else(st=='16','16F', st),
          agec1 = if_else(agec %in% c("Age <2","Age 2-4") ,1,2 ),
          agec=gsub('Age ', '', agec),
          agec2 = if_else( agec %in% c('<2','2-4'), '<5',
                           if_else( agec %in% c('5-17','18-49'), '5-49',
                                    if_else( agec %in% c('50-64','65+'), '50+',NA))),
          agec2 = factor(agec2, levels=c('<5','5-49','50+'), labels=c('<5 years', '5-49 years', '50+ years') )
  ) %>%
  group_by(st,agec2, year) %>%
  summarize(N_IPD=sum(N_IPD)) %>%
  ungroup()

all.sts <- unique(d1$st)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("US Serotype Trends"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("select.st",
                        "Select serotype:",
                        all.sts,
                        selected='19F')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("stPlot"),
           
           # Add a footer for the data source
           fluidRow(
             tags$div(
               style = "margin-top: 20px; font-size: 12px;",
               "Data Source: ",
               tags$a(href = "https://data.cdc.gov/Public-Health-Surveillance/1998-2022-Serotype-Data-for-Invasive-Pneumococcal-/qvzb-qs6p", 
                      "These data are made available by the CDCs Active Bacterial Core Surveillance network.", target = "_blank")
               
           
        )
    )
)
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$stPlot <- renderPlotly({
      p1 <- d1 %>%
        filter(st %in% c(input$select.st)) %>%
        ggplot(aes(x=year, y=N_IPD))+
        geom_line()+
        facet_wrap(~agec2, scales ='free', nrow=1) +
        theme_classic()+
        theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust=1)) +
        geom_vline(xintercept=c(1999.5, 2009.5), lty=2, color='gray')+
        ylim(0,NA) +
        ggtitle(paste0('Trends in IPD caused by serotype ', input$select.st, ' in the US (unadjusted for population)'))
      ggplotly(p1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
