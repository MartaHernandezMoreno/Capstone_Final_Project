library(shiny)

shinyUI(pageWithSidebar(
    titlePanel("Data Science Capstone - Next Word Predictor"),
    sidebarPanel(
        h4("Input a phrase with at least two words and press 'Predict' to see the top 5 candidate words:"),	
        textInput(inputId="user_input", label = ""),
        submitButton("Predict")
    ),
    mainPanel(
        h4("Prediction 1"),
        textOutput("guess1"),
        h4("Prediction 2"),
        textOutput("guess2"),
        h4("Prediction 3"),
        textOutput("guess3"),
        h4("Prediction 4"),
        textOutput("guess4"),
        h4("Prediction 5"),
        textOutput("guess5")
        )
                  
))