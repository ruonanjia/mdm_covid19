library(shinydashboard)
library(ggplot2)
library(zipcode) # install only available through archive
library(maps)
library(ggpubr)
# library("PerformanceAnalytics") # for chart correlation
library(usmap) # for plotting data us maps
library(scales) # for muted() function


##### Load data ####

setwd("/Users/jiaruonan/Desktop/mdm_covid19/mdm_covid19_data")
load("data_all_444sub_rename.Rda")
load("data_all_444sub_rename_header.Rda")
load("data_all_444sub_attitude.Rda")

# combine data
data.all <- merge(data.all, data.attitude, by = "mTurkCode")
data.all$age <- as.numeric(as.character(data.all$age)) + 17 # correct coding
rm(data.attitude)

# preprocessing
# Separate young and old, compare with 60 yr
data.all$is.young <- 1
data.all$is.young[data.all$age >= 60] = 0 
data.all$is.young <- as.factor(data.all$is.young)

# clean zipcode
data.all$zip_postal_code <- as.numeric(as.character(data.all$zip_postal_code))
data(zipcode)
data.all$zipcode<- clean.zipcodes(data.all$zip_postal_code)

##### Build dashboard ####
# visuzlizing age, gender, counts on map


ui <- dashboardPage(
  dashboardHeader(title = "Decision Making"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Demographics", tabName = "demograph", icon = icon("database")),
      menuItem("Attitudes", tabName = "att", icon = icon("database"))
    ),
    div(style = "padding-left: 15px; padding-top: 40px;",
        # p(class = "small", "Made with ",
        #   a("R", href = "http://www.r-project.org/"),
        #   ", ",
        #   a("Shiny", href = "http://shiny.rstudio.com/"),
        #   ", ",
        #   a("shinydashboard", href = "http://rstudio.github.io/shinydashboard/"),
        #   ", ",
        #   a("ggplot2", href = "http://ggplot2.org/"),
        #   ", & leaflet",
        #   a("(1)", href = "http://leafletjs.com/"),
        #   " ",
        #   a("(2)", href = "http://rstudio.github.io/leaflet/")
        # 
        # 
        # ),
        # p(class = "small", "Data courtesy of ",
        #   a("PiLR Health", href="http://www.pilrhealth.com/")
        # ),
        # p(class = "small",
        #   a("Source code", href = "https://github.com/rstudio/webinars/tree/master/2015-04")
        # )
    )
  ),
  
  
  dashboardBody(
    
    tabItem(tabName="demograph",
            # Boxes need to be put in a row (or column)
            fluidRow(
              box(plotOutput("plot_age", height = 250)),
              
              box(plotOutput("plot_count_state", height = 250))
              
              # box(
              #   title = "Controls",
              #   sliderInput("slider", "Number of observations:", 1, 100, 50)
              
            )
            
        ),
    
    tabItem(tabName="att",
            # Boxes need to be put in a row (or column)
            fluidRow(
              box(plotOutput("plot_ambig_att", height = 250)),
              
              box(plotOutput("plot_risk_att", height = 250))
              
              # box(
              #   title = "Controls",
              #   sliderInput("slider", "Number of observations:", 1, 100, 50)
              
            )
            
    )
  )
)

server <- function(input, output) {
  
  output$plot_age <- renderPlot({
    # data <- histdata[seq_len(input$slider)]
    # hist(data)
    # age
    ggplot(data.all, aes(x=age)) +
      geom_histogram(position = "identity", bins = 20) +
      labs(x = "Age") +
      ggtitle("Age Distribution")
  })
  
  output$plot_count_state <- renderPlot({
    # location of county by zipcode
    data.zipcode<-aggregate(data.frame(count=data.all$mTurkCode),list(zip=data.all$zipcode),length)
    geo<- merge(data.zipcode, zipcode, by='zip')
    
    # location of state by zipcodes
    state_all <- unique(geo$state)
    geo_state <- data.frame(state = state_all)

    state_match <- match(geo$state, state_all)
    for (idx in 1:length(state_all)) {
      geo_state$count[idx] <- sum(geo$count[geo$state==state_all[idx]])
    }
    
    # plot
    plot_usmap(data=geo_state, regions="states",values="count") +
      ggtitle("Count of participants") +
      labs(fill = "Count") +
      theme(legend.position = "right")
  })
}

shinyApp(ui, server)







