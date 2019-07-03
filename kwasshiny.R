#install.packages("shiny")
#install.packages("shinythemes")
#install.packages("expss")
library(readxl)
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(tidyverse)
library(data.table)
library(expss)
require(latex2exp)

# Load data
trend_data <- read_excel("____.xlsx")
tab_1 <- readRDS("/Users/sritarunit/esrd_search_trends/tab_1.RDS")
tab_1$Adj <- round(as.numeric(as.character(tab_1$Adj)), digits = 3)
tab_1$Unadj <- round(as.numeric(as.character(tab_1$Unadj)), digits = 3)
tab_2 <- tab_1
tab_2$Search_Terms <- gsub("_", " ",tab_2$Search_Terms)
tab_2$Search_Terms <- map_chr(tab_2$Search_Terms,function(term){sprintf("%s%s%s","'",term, "'")})

corr_data <- readRDS("/Users/sritarunit/esrd_search_trends/corr_data.RDS")
colnames(corr_data)[2] <- "ESRD_search_term"

# Define UI
ui <- fluidPage(theme = shinytheme("superhero"), titlePanel("Google Trend Index"),
                sidebarLayout(sidebarPanel(selectInput(inputId = "type", label = strong("Symptom Keyword"),choices=list("Kidney failure"= "Kidney_failure","Blood in urine"= "Blood_in_urine","Dialysis" ="Dialysis",        
                                                                                                                        "Swollen feet"="Swollen_feet","Kidney damage"="Kidney_damage","protein in urine"= "protein_in_urine","ESRD search term" = "ESRD_search_term"),selected = "Kidney failure")
                                           # Output: Description, lineplot, and reference
                                           ),
                              mainPanel(
                                tabPanel("Plots",plotOutput(outputId = "lineplot", width = "100%")),
                                tabPanel("Tables",dataTableOutput(outputId = "correlation_tab")),
                                textOutput(outputId = "desc"),
                                tags$a(href = "https://trends.google.com/trends/explore?q=ESRD&geo=US", "Source: Google Trends", target = "_blank")))
                
                
)

colnames(trend_data) <- gsub(" ","_",colnames(trend_data))


# Define server function
server <- function(input, output) {
  # Create scatterplot object the plotOutput function is expecting
  output$correlation_tab <- renderDataTable({
    return(tab_2)
    }, escape = FALSE)
  output$lineplot <- renderPlot({
    color = "#434343"
    setNames(state.abb, state.name)[c(trend_data$Region)]
    print(as.formula(sprintf("%s ~ ESRD_INCIDENCE",input$type)))
    m <- lm(as.formula(sprintf("%s ~ ESRD_INCIDENCE",input$type)),data = trend_data)
    a2 <- round(cor.test(corr_data[,"ESRD_INCIDENCE"],corr_data[,input$type],method ="spearman")[4]$estimate, digits =4)
    ggplot(trend_data, aes(x=ESRD_INCIDENCE, y=unlist(trend_data[,input$type]))) + ylab(input$type) + ylim(40,100) + geom_point(alpha = 0.05) + geom_text(label=setNames(state.abb, state.name)[c(trend_data$Region)], position = position_dodge(width=0.01),  size=3.5) + geom_smooth(method = "lm", se = TRUE) + annotate("text", x = 245, y = 90, label = sprintf("%s",TeX('$\\rho$')), color="blue", size = 5, parse=TRUE) + annotate("text", x = 275, y = 90, label = sprintf("\"= %s \"",round(a2, digits=3)), color="blue", size = 5, parse=TRUE)
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)