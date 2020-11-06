
library(readr)
library(dplyr)
library(ggplot2)
library(janitor)
library(tidyr)
library(shiny)
library(janitor)
library(data.table)
library(DT)
library(plotly)
library(lubridate)



# Data ####

data <- read_csv("data.csv")

# Data cleaning and correct parsing of campaign variable 
kantar_data <- data %>%
              clean_names() %>%
              mutate_at(3, as.factor)
# Date parsing
kantar_data$date <- dmy(kantar_data$date_week)



# Kantar App Integration ####

ui <- fluidPage(
  # Logo
  img(src = "k3.png", height = 75, width = 150, align="left"),
  br(),
  br(),
  br(),
  titlePanel("Advertising Effect"),
  theme= shinythemes::shinytheme("flatly"),
  sidebarLayout(
    sidebarPanel(
      sliderInput('RF', 'Select the Retention Factor', min = 0, max =1, step=0.1, value=0.5)),
    mainPanel(
      tabsetPanel(  
        tabPanel("Model", plotlyOutput('model_fit')), # defined in line 100
        tabPanel("Summary", verbatimTextOutput("model_summary"))), #defined in line 101
      tabPanel("Table",DTOutput("campaign_efficiency")) #defined in line 105
    )
  )
)


server <- function(input, output, session){
  
  # Adstock function 
  

  adstock <- function(RF){
    ad <- NULL #initializing adstock storage vector
    # if(!is.numeric(RF)){
    #   stop("Your Retention Factor must be numeric and take values in [0,1]")
    # }
    for ( n in 1:dim(kantar_data)[1]){
      if(n==1) { 
        ad[n]<- kantar_data$media_spend_usd[n]} else {
          ad[n] <- kantar_data$media_spend_usd[n]+ (RF * ad[n-1]) 
        }
    }
    return(ad)  
  }
  
  
  # Adstock Model Fit 
  
  plot_adstock <- function() {
    
    # Adstock regression model
    mod1 <- lm(search_volume ~ media_campaign + adstock(input$RF), data=kantar_data)
    kantar_data$fitted_values <- mod1$fitted.values
    
    # Non interactive model plot (argument for plotly; line 146)
    kantar_data %>%
      mutate(fitted_values = round(fitted_values,2)) %>%
      rename(Date = date,
             Search_Volume = search_volume,
             Model_Value = fitted_values ) %>%
      ggplot() +
      geom_point(aes(x = Date, y = Search_Volume), color="navyblue", alpha = 0.4, size =0.4)+
      geom_line(aes(x = Date, y = Model_Value, group=1), color="#09557f" , alpha= 0.6, size = 0.6)+
      scale_x_date(expand = c(0, 100))+
      labs(title ="Adstock Model ",
           x="", y = "Search Volume",
           fill="") +
      guides(fill=F)+
      theme_minimal()
    }
  
  # Summary of Adstock model
  model_fit <- function(){
    adstock_1<- adstock(input$RF)
    mod1 <- lm(search_volume ~ media_campaign + adstock_1, data=kantar_data)
    summary(mod1)
  }
  
  # Model results table per campaign 
  campaign_table <- function() {
    
    adstock_1<- adstock(input$RF)
    mod1 <- lm(search_volume ~ media_campaign + adstock_1, data=kantar_data)
    mod_info <- summary(mod1)
    kantar_data$adst<- adstock(input$RF)
    kantar_data$fitted_values <- mod1$fitted.values
    
    # Model results per brand's advertising campaigns
    
    # Average change per campaign per unit change in search volume
    camp1 <- mod_info$coefficient[1]+ mod_info$coefficient[4]
    camp2 <- mod_info$coefficient[1]+ mod_info$coefficient[2] + mod_info$coefficient[4]
    camp3 <- mod_info$coefficient[1]+ mod_info$coefficient[3] + mod_info$coefficient[4]
    
    ktable1 <- data.table(Campaign = c(1,2,3),rbind(camp1,camp2,camp3))
    ktable1 <- ktable1[, V1:= round(V1,2)]
    
    # Efficiency per campaign in terms of search_volume/adstock throughout the years
    ktable2 <- data.table(kantar_data)
    ktable3 <- ktable2[,.(ratio_vol_spend = round(sum(fitted_values)*100/sum(adst),2)), by= media_campaign]
    
    ktable4 <- ktable2[,.(total_spend = format(round(sum(adst),2),big.mark=",")), by= media_campaign]
    ktable5 <- ktable2[,.(total_volume = format(sum(fitted_values), big.mark=",")), by= media_campaign]
    ktable4<- cbind(ktable4,ktable5)
    ktable4 <- ktable4[,-c(1,3)]
    
    # Assembling final table with previous information
    ktable <- cbind(ktable1,ktable4)
    ktable <- cbind(ktable, ktable3)
    ktable[,media_campaign:=NULL] #deleting column
    
    setnames(ktable, 
             c("V1", "total_spend","total_volume","ratio_vol_spend"),
             c("Average Google Search Volume", 
               "Total Adstock",
               "Total Google Search Volume", 
               "Efficiency (%)"))
  }
  
  output$model_fit <- renderPlotly({
    plot_adstock()
  })
  
  output$model_summary <- renderPrint({
    model_fit()
  })
  
  output$campaign_efficiency<- renderDT({
    campaign_table()}, options = list(dom = 't')) #removing search tool
}
shinyApp(ui = ui, server = server)



