# Test App 3


# Need to strip everything down to isolate how to fix the multiple selection issue.




# Building this off of https://github.com/daattali/shiny-server/blob/master/mimic-google-form/app.R
# http://shinyapps.dreamrs.fr/shinyWidgets/

# Todo
# DONE - Get rid of extra junk
# - Add questions
# - re-format the layout
# - Add visuals


# What's more intuitive? Asking "how comfortable are you with X" and then under it, "in the last month, have you done X" -OR- go through all the comfort qs then all the behavior qs?




# Packages ----------------------------------------------------------------
library(tidyverse)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(janitor)

# Prep --------------------------------------------------------------------
# which fields get saved 
fieldsAll <- c(
               "behavior_personal",
               "behavior_mask"
               
)



# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# save the results to a file
saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  
  write.csv(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = TRUE)
}



# directory where responses get stored
responsesDir <- file.path("responses")

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

# usernames that are admins
adminUsers <- c("admin", "prof")




# Shiny -------------------------------------------------------------------
shinyApp(
  
  
  # UI ----------------------------------------------------------------------
  ui = fluidPage(
    theme = "style.css",
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    title = "Mimicking a Google Form with a Shiny app",
    
    div(id = "header",
        h1("Mimicking a Google Form with a Shiny app"),
        h4("This app is a supplement to my",
           a(href = "http://deanattali.com/2015/06/14/mimicking-google-form-shiny/",
             "blog post on the topic")
        ),
        strong( 
          span("Created by "),
          a("Dean Attali", href = "http://deanattali.com"),
          HTML("&bull;"),
          span("Code"),
          a("on GitHub", href = "https://github.com/daattali/shiny-server/tree/master/mimic-google-form"),
          HTML("&bull;"),
          a("More apps", href = "http://daattali.com/shiny/"), "by Dean")
    ),
    
    fluidRow(
      column(12,
             div(
               id = "form",
               
               # Section 2: behavior
               div(class = "spacer"),
               h4("Part 2: What Have You Done?"),
               div("When was the last time you did any of the following?"),

               div(class = "questiontext", "I have recieved personal services..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_personal", NULL, c("in the last month", "in the last quarter", "since February", "since before February"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
               
               div(class = "questiontext", "Please check off all that apply to your mask-wearing behaviors:"),
               div(class = "rowquestionmask",
                   div(class = "columnnotcentered",
                       # "Please check off all that apply to your mask-wearing behaviors:",
                       checkboxGroupInput("behavior_mask", "", 
                                          c("I wear a mask when outdoors and there are few to no people near me.", 
                                            "I wear a mask when outdoors and am close to people.",
                                            "I wear a mask when indoors and there are few to no people near me.",
                                            "I wear a mask when indoors and am close to other people.",
                                            "I never wear a mask."), inline = FALSE, width = "500px")
                   )
               ),
               
               
               actionButton("submit", "Submit", class = "btn-primary"),

             ),

      )
    ),
  ),
  
  
  # Server ------------------------------------------------------------------
  server = function(input, output, session) {
    
    # Enable the Submit button when all mandatory fields are filled out

    
    # Gather all the form inputs (and add timestamp)
    formData <- reactive({
      # data <- sapply(fieldsAll, function(x) input[[x]]) 
      data <- sapply(fieldsAll, function(x) input[[x]]) %>% as_data_frame()
      data <- data %>%
        pivot_longer(names_to = "names", values_to = "value", cols = everything()) %>% 
        distinct() %>%
        mutate(new_value = case_when(names == "behavior_mask" ~ "1", TRUE ~ value)) %>% 
        mutate(names = case_when(names == "behavior_mask" ~ paste0("Mask Behavior: ", value),
                                 TRUE ~ names)) %>% 
        select(-value) %>% 
        rename(value = new_value) %>% 
        pivot_wider(names_from = "names", values_from = "value") %>% 
        clean_names() %>% 
        mutate_at(vars(matches("mask_behavior")), as.numeric) %>% 
        bind_cols(tibble(timestamp = epochTime()))
      
        
        
      # data <- c(data, timestamp = epochTime())
      # Tweaks so that multiple choice options save ok.
      # data <- data %>% 
      #   pivot_longer(names_to = "names", values_to = "value", cols = -timestamp) %>% 
      #   distinct() %>%
      #   mutate(new_value = case_when(names == "behavior_mask" ~ "1", TRUE ~ value)) %>% 
      #   mutate(names = case_when(names == "behavior_mask" ~ paste0("Mask Behavior: ", value),
      #                            TRUE ~ names)) %>% 
      #   select(-value) %>% 
      #   rename(value = new_value) %>% 
      #   pivot_wider(names_from = "names", values_from = "value") %>% 
      #   clean_names() %>% 
      #   mutate_at(vars(matches("mask_behavior")), as.numeric)
      
      # data <- t(data)
      data
    })    
    
    # When the Submit button is clicked, submit the response
    observeEvent(input$submit, {
      
      # User-experience stuff
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      # Save the data (show an error message in case of error)
      tryCatch({
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })
    })
    
    # submit another response
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })
        
  }
)