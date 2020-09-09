rm(list = ls())

##### Library ####
library(shinydashboard)
library(ggplot2)
library(zipcode) # install only available through archive
library(maps)
library(ggpubr)
# library("PerformanceAnalytics") # for chart correlation
library(usmap) # for plotting data us maps
library(scales) # for muted() function
library(tidyverse)

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

##### Task behavior ####
mask_med <- data.all$error.med < 0.5 
mask_mon <- data.all$error.mon < 0.5

mask_med_young <- data.all$error.med < 0.5 & data.all$is.young == 1  
mask_mon_young <- data.all$error.mon < 0.5 & data.all$is.young == 1

mask_med_old <- data.all$error.med < 0.5 & data.all$is.young == 0
mask_mon_old <- data.all$error.mon < 0.5 & data.all$is.young == 0


##### PCA ####
# select items
ambig_pca <- data.all %>% select(mTurkCode, 
                                 # news
                                 ambig1_news_perday, ambig1_news_before, ambig1_track_deaths,
                                 # how well-informed
                                 ambig1_gov_local, ambig1_you, ambig1_gov_national, ambig1_scientists, ambig1_health_worker, ambig1_economists, ambig1_family_friend,
                                 # reaction
                                 ambig1_react_nation, ambig1_react_state, ambig1_react_science, ambig1_react_worker, ambig1_react_econ, ambig1_react_you,
                                 #health or economics
                                 ambig2_health_impact, ambig2_economics,
                                 # current situation
                                 ambig3_current_covi1, ambig3_current_covi2,
                                 # how likely will be infected
                                 ambig3_you_infect, ambig3_family_infect,
                                 # how much is your daily life affected
                                 ambig3_daily_affect,
                                 # how would you feel if you got infected
                                 ambig3_feel_infect
)

# correct factor levels and turn into integer
ambig_pca$mTurkCode <- as.factor(as.character(ambig_pca$mTurkCode))

ambig_pca$ambig1_news_perday <- as.integer(as.character(ambig_pca$ambig1_news_perday))
ambig_pca$ambig1_news_before <- as.integer(as.character(ambig_pca$ambig1_news_before))
ambig_pca$ambig1_track_deaths <- as.integer(as.character(ambig_pca$ambig1_track_deaths))

ambig_pca$ambig1_gov_local <- as.integer(as.character(ambig_pca$ambig1_gov_local))
ambig_pca$ambig1_you <- as.integer(as.character(ambig_pca$ambig1_you))
ambig_pca$ambig1_gov_national <- as.integer(as.character(ambig_pca$ambig1_gov_national))
ambig_pca$ambig1_scientists <- as.integer(as.character(ambig_pca$ambig1_scientists))
ambig_pca$ambig1_health_worker <- as.integer(as.character(ambig_pca$ambig1_health_worker))
ambig_pca$ambig1_economists <- as.integer(as.character(ambig_pca$ambig1_economists))
ambig_pca$ambig1_family_friend <- as.integer(as.character(ambig_pca$ambig1_family_friend))


ambig_pca$ambig1_react_nation <- as.integer(as.character(ambig_pca$ambig1_react_nation))
ambig_pca$ambig1_react_state <- as.integer(as.character(ambig_pca$ambig1_react_state))
ambig_pca$ambig1_react_science <- as.integer(as.character(ambig_pca$ambig1_react_science))
ambig_pca$ambig1_react_worker <- as.integer(as.character(ambig_pca$ambig1_react_worker))
ambig_pca$ambig1_react_econ <- as.integer(as.character(ambig_pca$ambig1_react_econ))
ambig_pca$ambig1_react_you <- as.integer(as.character(ambig_pca$ambig1_react_you))


ambig_pca$ambig2_health_impact <- as.integer(as.character(ambig_pca$ambig2_health_impact))
ambig_pca$ambig2_economics <- as.integer(as.character(ambig_pca$ambig2_economics))

ambig_pca$ambig3_current_covi1 <- as.integer(as.character(ambig_pca$ambig3_current_covi1))
ambig_pca$ambig3_current_covi2 <- as.integer(as.character(ambig_pca$ambig3_current_covi2))

ambig_pca$ambig3_you_infect <- as.integer(as.character(ambig_pca$ambig3_you_infect))
ambig_pca$ambig3_family_infect <- as.integer(as.character(ambig_pca$ambig3_family_infect))

ambig_pca$ambig3_daily_affect <- as.integer(as.character(ambig_pca$ambig3_daily_affect))
ambig_pca$ambig3_feel_infect <- as.integer(as.character(ambig_pca$ambig3_feel_infect))

# correct scale
# 1-6, news
ambig_pca$ambig1_news_perday <- 7-ambig_pca$ambig1_news_perday
ambig_pca$ambig1_news_before <- 7-ambig_pca$ambig1_news_before

# 1-7, current situation
ambig_pca$ambig3_current_covi1 <- 8-ambig_pca$ambig3_current_covi1
ambig_pca$ambig3_current_covi2 <- 8-ambig_pca$ambig3_current_covi2

# 2-8, compare with 5 (most uncertain)
ambig_pca$ambig3_you_infect[ambig_pca$ambig3_you_infect == 9 | ambig_pca$ambig3_you_infect == 1] <- NA 
ambig_pca$ambig3_family_infect[ambig_pca$ambig3_family_infect == 9 | ambig_pca$ambig3_family_infect == 1] <- NA 

ambig_pca$ambig3_you_infect[ambig_pca$ambig3_you_infect <= 5 & !is.na(ambig_pca$ambig3_you_infect)] <- ambig_pca$ambig3_you_infect[ambig_pca$ambig3_you_infect <= 5 & !is.na(ambig_pca$ambig3_you_infect)] - 1

ambig_pca$ambig3_you_infect[ambig_pca$ambig3_you_infect > 5 & !is.na(ambig_pca$ambig3_you_infect)] <- 9-ambig_pca$ambig3_you_infect[ambig_pca$ambig3_you_infect > 5 & !is.na(ambig_pca$ambig3_you_infect)]

ambig_pca$ambig3_family_infect[ambig_pca$ambig3_family_infect <= 5 & !is.na(ambig_pca$ambig3_family_infect)] <- ambig_pca$ambig3_family_infect[ambig_pca$ambig3_family_infect <= 5 & !is.na(ambig_pca$ambig3_family_infect)] - 1

ambig_pca$ambig3_family_infect[ambig_pca$ambig3_family_infect > 5 & !is.na(ambig_pca$ambig3_family_infect)] <- 9-ambig_pca$ambig3_family_infect[ambig_pca$ambig3_family_infect > 5 & !is.na(ambig_pca$ambig3_family_infect)]

# 1-7 likert scale, 1(extremely) ~ 7(not at all)
ambig_pca$ambig3_daily_affect <- 8 - ambig_pca$ambig3_daily_affect

# 1~5 scale, 1(Extremely unwell) ~ 5(not at all unwell)
ambig_pca$ambig3_feel_infect <- 6 - ambig_pca$ambig3_feel_infect

# no state questions
array2pca_raw <- ambig_pca %>% select(# news
  ambig1_news_perday, ambig1_news_before, ambig1_track_deaths,
  # how well-informed
  ambig1_you, ambig1_gov_national, ambig1_scientists, ambig1_health_worker, ambig1_economists, ambig1_family_friend,
  # reaction
  ambig1_react_nation, ambig1_react_science, ambig1_react_worker, ambig1_react_econ, ambig1_react_you,
  #health or economics
  ambig2_health_impact, ambig2_economics,
  # current situation
  ambig3_current_covi1,
  # how likely will be infected
  ambig3_you_infect, ambig3_family_infect,
  # how much is your daily life affected
  ambig3_daily_affect,
  # how would you feel if you got infected
  ambig3_feel_infect
)

# no na
array2pca <- array2pca_raw[!is.na(rowSums(array2pca_raw)),]

# z score
zscored <- scale(array2pca, center = TRUE, scale = TRUE)

# PCA
pcaresult <- prcomp(zscored, retx = TRUE, center = TRUE, scale. = TRUE)

# calculate variace from eigenvalues
eigs <- pcaresult$sdev^2
var_explain <- eigs/sum(eigs)
plot(var_explain, type = "b")
comp_id = c(1:ncol(array2pca))
var_plot <- data.frame(comp_id, var_explain)

# cumulative variance
for (i in c(1:nrow(var_plot))) {
  var_plot$var_cumul[i] <- sum(var_plot$var_explain[1:i])
}

# labels for plotting
item_lab <- c("news_perday", "news_before", "track_deaths",
              "inform_you", "inform_gov",  "inform_scientist", "inform_health_worker", "inform_econ", "inform_family_friend",
              "react_gov", "react_scientist", "react_health_worker", "react_econ", "react_you",
              "impact_health", "impact_econ", 
              "current_situation", 
              "infect_you", "infect_family_friend",
              "affect_daily", "infect_feel")

# loadings
pcaload <- pcaresult$rotation[,]
load_plot <- data.frame(pcaload)
load_plot$comp_id = comp_id

# score
score = pcaresult$x
score_tb <- as.data.frame.array(score)
score_tb$mTurkCode <- ambig_pca$mTurkCode[!is.na(rowSums(array2pca_raw))]
score_tb$is.young <- data.all$is.young[!is.na(rowSums(array2pca_raw))]
data_pca <- merge(data.all, score_tb, by = intersect(colnames(data.all), colnames(score_tb)))

# mask of subject selection based on choice error
mask_pca_med <- data_pca$error.med < 0.5
mask_pca_mon <- data_pca$error.mon < 0.5

##### Build UI ####
# visuzlizing age, gender, counts on map

ui <- dashboardPage(
  dashboardHeader(title = "Decision Making"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Demographics", tabName = "demograph", icon = icon("database")),
      menuItem("Task behavior", tabName = "att", icon = icon("chart-bar")),
      menuItem("PCA of Ambiguity Survey", tabName = "pca", icon=icon("chart-bar"))
    )
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName="demograph",
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(plotOutput("plot_age", height = 250)),
                
                box(plotOutput("plot_count_state", height = 250))
                
                # box(
                #   title = "Compnent number",
                #   sliderInput("slider", "Number of observations:", 1, 100, 50)
                #   )
                
              )
              
      ),
      
      tabItem(tabName = "att",
              fluidPage(
                box(selectInput("att1_name", "Choose the 1st task behavior:", 
                            list("Ambiguity Attitude Medical","Risk Attitude Medical",
                                 "Ambiguity Attitude Monetary", "Risk Attitude Monetary"),
                            selected = "Ambiguity Attitude Medical"
                ), height=80),
                
                box(selectInput("att2_name", "Choose the 2nd task behavior:", 
                                list("Ambiguity Attitude Medical","Risk Attitude Medical",
                                     "Ambiguity Attitude Monetary", "Risk Attitude Monetary"),
                                selected = "Ambiguity Attitude Monetary"
                ), height=80),
                
                box(title = "Distribution of the 1st uncertainty attitudes",
                    plotOutput("plot_att1_distrib", height = 250)),
                
                box(title = "Distribution of the 2nd uncertainty attitudes",
                    plotOutput("plot_att2_distrib", height = 250)),
                
                box(title = "Correlation between uncertainty attitudes",
                    plotOutput("plot_att_corr", height = 250)),
                
                box(title = "Correlation between uncertainty attitudes, separated by age",
                    plotOutput("plot_att_corr_age", height = 250))
                
              
              )
              ),
      
      tabItem(tabName="pca",
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(plotOutput("plot_var", height = 250)),
                
                box(plotOutput("plot_cum_var", height = 250)),
                
                box(selectInput("comp_name_load", "Choose a component to visualize its loadings:", 
                              list('PC1', 'PC2', 'PC3', 'PC4', 'PC5',
                                   'PC6', 'PC7', 'PC8', 'PC9', 'PC10',
                                   'PC11', 'PC12', 'PC13', 'PC14', 'PC15',
                                   'PC16', 'PC17', 'PC18', 'PC19', 'PC20', 'PC21')
                              ),
                    height=80
                ),
                
                box(selectInput("comp_name_distrib", "Choose a component to visualize its distribution:", 
                                list('PC1', 'PC2', 'PC3', 'PC4', 'PC5',
                                     'PC6', 'PC7', 'PC8', 'PC9', 'PC10',
                                     'PC11', 'PC12', 'PC13', 'PC14', 'PC15',
                                     'PC16', 'PC17', 'PC18', 'PC19', 'PC20', 'PC21')
                ),
                height=80
                ),
                
                box(plotOutput("plot_loading", height=350)),
                box(plotOutput("plot_score_distrib", height=350)),
                
                box(selectInput("pc2plot", "Choose a component to visualize its correlation with a task measure:", 
                                list('PC1', 'PC2', 'PC3', 'PC4', 'PC5',
                                     'PC6', 'PC7', 'PC8', 'PC9', 'PC10',
                                     'PC11', 'PC12', 'PC13', 'PC14', 'PC15',
                                     'PC16', 'PC17', 'PC18', 'PC19', 'PC20', 'PC21')
                ),
                height=80
                ),
                
                box(selectInput("att_name", "Choose a task measure to visualize correlation with a component:", 
                                list("Ambiguity Attitude Medical","Risk Attitude Medical",
                                     "Ambiguity Attitude Monetary", "Risk Attitude Monetary"),
                                selected = "Ambiguity Attitude Medical"
                ), height=80),
                
                box(plotOutput("plot_score_att", height=350)),
                box(plotOutput("plot_score_att_age", height=350))
                
              )
      )      
      
      
      
    )
  )
)

##### Build server ####
server <- function(input, output) {
  
  output$plot_age <- renderPlot({

    # age
    ggplot(data.all, aes(x=age, fill=is.young, color=is.young)) +
      geom_histogram(position = "stack", bins = 20, alpha=0.6) +
      scale_fill_discrete(name="Age", breaks=c(1,0), labels=c('Younger (below 60)','Older (above and including 60)')) +
      scale_color_discrete(name="Age", breaks=c(1,0), labels=c('Younger (below 60)','Older (above and including 60)')) +
      labs(x = "Age") +
      ggtitle("Age Distribution") +
      theme(legend.text = element_text(size=10),
            legend.title = element_text(size=9),
            legend.background = element_blank(),
            legend.position = c(0.3, 0.8))
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
  
  output$plot_att1_distrib <- renderPlot({
    var1 = case_when(
      input$att1_name == "Ambiguity Attitude Medical" ~ "ambig_corr.med",
      input$att1_name == "Ambiguity Attitude Monetary" ~ "ambig_corr.mon",
      input$att1_name == "Risk Attitude Medical" ~ "risk.med",
      input$att1_name == "Risk Attitude Monetary" ~ "risk.mon"
    )
    
    ggplot(data.all[mask_med & mask_mon,], aes(x=eval(parse(text=var1)), fill=is.young, color=is.young)) + 
      geom_histogram(position="identity", alpha=0.6) +
      scale_fill_discrete(name="Age", breaks=c(1,0), labels=c('Younger (below 60)','Older (above and including 60)')) +
      scale_color_discrete(name="Age", breaks=c(1,0), labels=c('Younger (below 60)','Older (above and including 60)')) +
      xlab(input$att1_name) + ylab("Count of participants") +
      theme(legend.text = element_text(size=10),
            legend.title = element_text(size=9),
            legend.background = element_blank(),
            legend.position = c(0.3, 0.8))
    
  })
  
  output$plot_att2_distrib <- renderPlot({
    var2 = case_when(
      input$att2_name == "Ambiguity Attitude Medical" ~ "ambig_corr.med",
      input$att2_name == "Ambiguity Attitude Monetary" ~ "ambig_corr.mon",
      input$att2_name == "Risk Attitude Medical" ~ "risk.med",
      input$att2_name == "Risk Attitude Monetary" ~ "risk.mon"
    )
    
    ggplot(data.all[mask_med & mask_mon,], aes(x=eval(parse(text=var2)), fill=is.young, color=is.young)) + 
      geom_histogram(position="identity", alpha=0.6) +
      scale_fill_discrete(name="Age", breaks=c(1,0), labels=c('Younger (below 60)','Older (above and including 60)')) +
      scale_color_discrete(name="Age", breaks=c(1,0), labels=c('Younger (below 60)','Older (above and including 60)')) +
      xlab(input$att2_name) + ylab("Count of participants") +
      theme(legend.text = element_text(size=10),
            legend.title = element_text(size=9),
            legend.background = element_blank(),
            legend.position = c(0.3, 0.8))
    
  })
  
  output$plot_att_corr <- renderPlot({
    # select variables
    att_name=c(input$att1_name,input$att2_name)
    
    var = case_when(
      att_name == "Ambiguity Attitude Medical" ~ "ambig_corr.med",
      att_name == "Ambiguity Attitude Monetary" ~ "ambig_corr.mon",
      att_name == "Risk Attitude Medical" ~ "risk.med",
      att_name == "Risk Attitude Monetary" ~ "risk.mon"
    )
    
    
    ggplot(data.all[mask_med & mask_mon,], aes(x=eval(parse(text=var[1])), y=eval(parse(text=var[2])))) + 
      geom_point()+
      geom_smooth(method="lm") +
      xlab(input$att1_name) + ylab(input$att2_name)
    
  })
  
  output$plot_att_corr_age <- renderPlot({
    # select variables
    att_name=c(input$att1_name,input$att2_name)
    
    var = case_when(
      att_name == "Ambiguity Attitude Medical" ~ "ambig_corr.med",
      att_name == "Ambiguity Attitude Monetary" ~ "ambig_corr.mon",
      att_name == "Risk Attitude Medical" ~ "risk.med",
      att_name == "Risk Attitude Monetary" ~ "risk.mon"
    )
    
    
    ggplot(data.all[mask_med & mask_mon,], aes(x=eval(parse(text=var[1])), y=eval(parse(text=var[2])), color=is.young)) + 
      geom_point()+
      scale_fill_discrete(name="Age", breaks=c(1,0), labels=c('Younger (below 60)','Older (above and including 60)')) +
      scale_color_discrete(name="Age", breaks=c(1,0), labels=c('Younger (below 60)','Older (above and including 60)')) +
      geom_smooth(method="lm") +
      xlab(input$att1_name) + ylab(input$att2_name) +
      theme(legend.text = element_text(size=10),
            legend.title = element_text(size=9),
            legend.background = element_blank(),
            legend.position = c(0.3, 0.8))
  })
  
  output$plot_var <- renderPlot({
    
    # plot variance explained
    ggplot(var_plot, aes(x = comp_id, y = var_explain)) +
      geom_point() + geom_line() +
      scale_x_continuous(breaks = c(1:ncol(array2pca))) +
      theme_classic() +
      labs(x="Number of components", y="Variance explained")+
      ggtitle("Variance explained")
    
  })  
  
  output$plot_cum_var <- renderPlot({
    # plot cumulative variance explained
    ggplot(var_plot, aes(x = comp_id, y = var_cumul)) +
      geom_point(size = 3) + geom_line() +
      scale_x_continuous(breaks = c(1:ncol(array2pca))) +
      theme_classic() +
      theme(text = element_text(size=12), axis.line = element_line(size = 1),
            axis.ticks = element_line(colour = "black", size = 1),
            axis.text = element_text(colour = "black", size = 10)) +
      labs(x="Number of components", y="Variance explained")+
      ggtitle("Cumulative variance explained")
    
  })
  
  
  output$plot_loading <- renderPlot({
    
    ggplot(load_plot, aes(x = comp_id, y = eval(parse(text=input$comp_name_load)))) +
      geom_bar(stat = "identity") +
      scale_x_continuous(breaks = c(1:ncol(array2pca)), labels = item_lab) +
      # scale_y_continuous(limits = c(-0.4,1), breaks = seq(-0.4,0.85,0.2)) +
      theme_classic() +
      theme(text = element_text(size=12), axis.line = element_line(size = 1),
            axis.ticks = element_line(colour = "black", size = 1),
            axis.text.x = element_text(angle = 90, hjust=1),
            axis.text = element_text(colour = "black", size = 10)) +
      labs(y="Loadings", x="") +
      ggtitle(input$comp_name_load)
    
  })
  
  output$plot_score_distrib <- renderPlot({
    # plot PC distribution
    ggplot(score_tb, aes(x = eval(parse(text=input$comp_name_distrib)), color=is.young, fill=is.young)) +
      geom_histogram(position = "identity", alpha = 0.5, size =1, bins = 25) +
      scale_fill_discrete(name="Age", breaks=c(1,0), labels=c('Younger (below 60)','Older (above and including 60)')) +
      scale_color_discrete(name="Age", breaks=c(1,0), labels=c('Younger (below 60)','Older (above and including 60)')) +
      theme_classic() +
      theme(text = element_text(size=12), axis.line = element_line(size = 1),
            axis.ticks = element_line(colour = "black", size = 1),
            axis.text = element_text(colour = "black", size = 10),
            legend.position = c(0.25, 0.9),
            legend.text = element_text(size=10),
            legend.title = element_text(size=10),
            legend.background = element_blank()) +
      labs(x=input$comp_name_distrib, y="Count of participant")
  })
  
  
  output$plot_score_att <- renderPlot({
    # select attitude
    var = case_when(
      input$att_name == "Ambiguity Attitude Medical" ~ "ambig_corr.med",
      input$att_name == "Ambiguity Attitude Monetary" ~ "ambig_corr.mon",
      input$att_name == "Risk Attitude Medical" ~ "risk.med",
      input$att_name == "Risk Attitude Monetary" ~ "risk.mon"
    )
    
    ggplot(data_pca[mask_pca_med & mask_pca_mon,], aes(x=eval(parse(text = input$pc2plot)), y=eval(parse(text = var)))) + 
      geom_point()+
      geom_smooth(method="lm") + 
      xlab(input$pc2plot) + ylab(input$att_name)
  })
  
  output$plot_score_att_age <- renderPlot({
    # select attitude
    var = case_when(
      input$att_name == "Ambiguity Attitude Medical" ~ "ambig_corr.med",
      input$att_name == "Ambiguity Attitude Monetary" ~ "ambig_corr.mon",
      input$att_name == "Risk Attitude Medical" ~ "risk.med",
      input$att_name == "Risk Attitude Monetary" ~ "risk.mon"
    )
    
    ggplot(data_pca[mask_pca_med & mask_pca_mon,], aes(x=eval(parse(text = input$pc2plot)), y=eval(parse(text = var)), color = is.young)) + 
      geom_point()+
      geom_smooth(method="lm") + 
      scale_fill_discrete(name="Age", breaks=c(1,0), labels=c('Younger (below 60)','Older (above and including 60)')) +
      scale_color_discrete(name="Age", breaks=c(1,0), labels=c('Younger (below 60)','Older (above and including 60)')) +
      xlab(input$pc2plot) + ylab(input$att_name) +
      theme(legend.text = element_text(size=10),
            legend.title = element_text(size=9),
            legend.background = element_blank(),
            legend.position = c(0.3, 0.9))
  })
  
}

##### Run app ####
shinyApp(ui, server)







