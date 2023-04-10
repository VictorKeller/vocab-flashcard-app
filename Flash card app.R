##### Flash card app #####



##### PREP #####
library(shiny)


### load main list
df <- read.csv("main list.csv", skip = 1, stringsAsFactors = F)

### fix special characters
fix_special_chars <- function(data) {
  
  for (variable in names(data)) {
    
    data[[variable]] <- gsub("\xfc", "ü", data[[variable]])
    data[[variable]] <- gsub("<fc>", "ü", data[[variable]])
    data[[variable]] <- gsub("<e4>", "ä", data[[variable]])
    data[[variable]] <- gsub("\xe4", "ä", data[[variable]])
    data[[variable]] <- gsub("\xc4", "Ä", data[[variable]])
    data[[variable]] <- gsub("\xf6", "ö", data[[variable]])
    data[[variable]] <- gsub("<f6>", "ö", data[[variable]])
    data[[variable]] <- gsub("<d6>", "Ö", data[[variable]])
    data[[variable]] <- gsub("<df>", "ß", data[[variable]])
    
  }
  
  return(data)
  
}


df <- fix_special_chars(df)


###########





##### UI #####
ui <- fluidPage(
  
  
  tags$head(
    tags$style(HTML("
      /* this will affect all the pre elements */
      pre {
        color: black;
        background-color: #ffffff;
        font-size: 20px;
      }
      /* this will affect only the pre elements under the class myclass */
      .myclass pre {
        color: black;
        background-color: #ffffff;
        font-size: 15px;
        border: 0px;
      }"))
  ),
  
  
  fluidRow(
    
    column(4, 
           textInput("input_text", "Enter command:"),
           style = "padding-top: 20px"),
    
    column(2, 
           checkboxGroupInput("phrase_or_word", "", choices = list("Phrases" = "phrase", "Words" = "word"), selected = c("phrase", "word"))),
    
    column(6, 
           sliderInput("frequency", "Priority of words/phrases", 1, 100, value = c(1, 5), width = "100%"),
           style = "padding-top: 20px")
    
    ),
  
  
  fluidRow(
    
    column(7, offset = 3,
           verbatimTextOutput("card"),
           style = "padding: 70px")
    
    ),
  
  fluidRow(
    
    column(6,
           div(class = "myclass", verbatimTextOutput("commands")),
           style = "padding: 20px")
    
  )
  
  
)


############





##### SERVER #####
server <- function(input, output, session) {
  
  commands <- reactiveValues(command = "")
  
  langs <- reactiveValues(lang = "")
  
  stim <- reactiveValues(stimulus = "")
  
  flip <- reactiveValues(flop = F)
  
  
  
  
  observeEvent(req(input$input_text != ""), {
    
    
    commands$command <- input$input_text
    
    
    if (input$input_text %in% c("e", "g")) {
      
      temp <- df[df$phrase_or_word %in% input$phrase_or_word & df$frequency %in% input$frequency[1]:input$frequency[2], ]
      
      stim$stimulus <- sample(1:nrow(temp), 1)
      
      langs$lang <- input$input_text
      
    } else {
      
      langs$lang <- ifelse(langs$lang == "e", "g", "e")
      
    }
    
    
    updateTextInput(session, "input_text", "Enter command:", "")
    
    
  })
  
  
  
  output$card <- renderText({
    
    
    
    if (commands$command == "e") {
      
      temp <- df[df$phrase_or_word %in% input$phrase_or_word & df$frequency %in% input$frequency[1]:input$frequency[2], ]
      
      return(temp$english[stim$stimulus])
      
      
    } else if (commands$command == "g") {
      
      
      temp <- df[df$phrase_or_word %in% input$phrase_or_word & df$frequency %in% input$frequency[1]:input$frequency[2], ]
      
      return(temp$german[stim$stimulus])
      
      
    } else {
      
      
      if (langs$lang == "g") {
        
        temp <- df[df$phrase_or_word %in% input$phrase_or_word & df$frequency %in% input$frequency[1]:input$frequency[2], ]
        
        return(temp$german[stim$stimulus])
        
      } else if (langs$lang == "e") {
        
        temp <- df[df$phrase_or_word %in% input$phrase_or_word & df$frequency %in% input$frequency[1]:input$frequency[2], ]
        
        return(temp$english[stim$stimulus])
        
      }
      
      
    }
    
    
    
  })
  
  
  
  
  output$commands <- renderText({
    
    return("Commands: \ne - Random English word; \ng - Random German word; \nany other key - flip card")
    
  })
  
  
  
  
  output$url <- renderUI({
    
    temp <- df[df$phrase_or_word %in% input$phrase_or_word & df$frequency %in% input$frequency[1]:input$frequency[2], ]
    word_phrase <- ifelse(langs$lang == "e", temp$english[stim$stimulus], temp$german[stim$stimulus])
    
    url <- a("Pronunciation", href = paste0("https://translate.google.com/?sl=en&tl=pt&text=", word_phrase, "%0A&op=translate"))
    HTML(paste(url))
  })
  
  
  
  
}





###############

shinyApp(ui, server)


