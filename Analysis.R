install.packages(c("shiny", "ggplot2", "lubridate", "dplyr", "DT", "readxl"))
library(shiny)
library(ggplot2)                 
library(lubridate)
library(dplyr)
library(DT)
library(readxl)
data <- read_excel("C:/Users/titi0/OneDrive/Documents/FAU/NeuroCort/cortisol_data.xlsx", col_names = F)
colnames(data) <- c("timestamp", "CortisolLevel")
print(head(data$timestamp))
data$timestamp <- ymd_hms(data$timestamp)
print(head(data$timestamp))
data<- na.omit(data)
data <- data[rev(1:nrow(data)),]
print(head(data))
threshold_high <- 15
feedback <- function(cortisol_level){
  if (cortisol_level > threshold_high){
    feedback_types <- c("Feeling stressed? Try listening to some music!", "Cortisol levels are high! Try taking 5 deep breaths!", "High cortisol levels! Try meditating for 5 minutes!")
    return(sample(feedback_types,1))
  } else {
    return("Your levels are good! Good job!")
  }
}
data$FeedbackonLevel <- sapply(data$CortisolLevel, function(x) as.character(feedback(x)))
print(data)

home <- fluidPage(
  titlePanel(tags$h1("Your Cortisol Levels", style = "font-size: 36px; font-weight: bold; color: cadetblue;")),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      textOutput("Suggestions"),
      tags$div(
        style = "backgorund-color: darkturquoise; padding: 10px; text-align: center; color: blue; font-weight:bold;",
        "Destress"
      )
    ),
    
    mainPanel(
      width = 8,
      DT::dataTableOutput("Cortisol_Levels"),
      plotOutput("CortisolPlot")
    )
  )
)
server <- function(input, output) {
  
  output$Cortisol_Levels <- DT::renderDataTable({
    data$timestamp <- format(data$timestamp, "%Y-%m-%d %H:%M:%S")
    datatable(data, options = list(pageLength = 10, scrollY = "300px"))
  })
  
  output$Suggestions <- renderText({
    latest_level <- tail(data$CortisolLevel,1)
    feedback_message <- feedback(latest_level)
    paste("Your latest cortisol level is", round(latest_level, 2), "\nFeedback:", feedback_message)
  })
  output$CortisolPlot <- renderPlot({
    ggplot(data, aes(x=as.POSIXct(timestamp), y=CortisolLevel)) +
      geom_line(color= "darkturquoise") +
      geom_point(color="darkturquoise", size = 3) +
      labs(title = "Your Cortisol Levels During the Week",
           x = "Time",
           y = "Cortisol Level (ug/dL)") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
        )
  })
}
shinyApp(ui = home, server = server)
