## ui.R file for my Shiny App
## My github repo: https://github.com/circulaic/coursera-datasci-capstone
## Created by Genevieve Beart, 12 August 2021

suppressWarnings(library(shiny))
suppressWarnings(library(markdown))
suppressWarnings(library(shinythemes))

shinyUI(navbarPage(theme = shinytheme("united"),"Data Science Capstone",
       tabPanel("Next Word Prediction",
                HTML("<strong>Author: Genevieve Beart</strong>"),
                br(),
                HTML("<strong>Date: 12 August 2021</strong>"),
                br(),
                HTML("<h2>Coursera Capstone - Predict Next Word</h2>"),
                #####img(src = "./headers.png"),
                # Sidebar
                sidebarLayout(
                    sidebarPanel(
                        helpText("Enter a string of text to predict the next word"),
                        textInput("inputString", "Enter a partial sentence here:",value = ""),
                        br(),
                        br(),
                        br(),
                        br()
                    ),
                    mainPanel(
                      h4(strong("Sentence Input:")),
                      tags$style(type='text/css', '#text1 {background-color: rgba(255,204,153,0.40); color: black;}'), 
                      textOutput('text1'),
                      br(),
                      h4(strong("Predicted Next Word:")),
                      verbatimTextOutput("prediction"),
                      strong("Note:"),
                      ("Please wait up to 10-15 seconds for processing - input and predicted next word will be greyed out while computing next word.")
                    )
                )
                
       ),
       tabPanel("About",
                mainPanel(
                  HTML("<strong>Author: Genevieve Beart</strong>"),
                  br(),
                  HTML("<strong>Date: 12 August 2021</strong>"),
                  br(),
                  HTML("<h2>Coursera Capstone - Predict Next Word</h2>"),
                  ##img(src = "./headers.png"),
                   includeMarkdown("about.md")
                )
       )
)
)