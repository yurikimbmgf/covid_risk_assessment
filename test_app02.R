# Test App 2
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
fieldsAll <- c("comfort_grocery",
               "comfort_plane",
               "comfort_restaurant_in",
               "comfort_restaurant_out",
               "comfort_friends_in",
               "comfort_friends_out",
               "comfort_event_in",
               "comfort_event_out",
               "comfort_medical",
               "comfort_personal",
               ###################
               "behavior_grocery",
               "behavior_plane",
               "behavior_restaurant_in",
               "behavior_restaurant_out",
               "behavior_friends_in",
               "behavior_friends_out",
               "behavior_event_in",
               "behavior_event_out",
               "behavior_medical",
               "behavior_personal",
               "behavior_mask",
               "feelings_catch",
               "feelings_spread",
               "feelings_precaution",
               "feelings_severity",
               "about_state",
               "about_age",
               "about_gender",
               "about_job",
               "about_health"
)


# which fields are mandatory
# fieldsMandatory <- c("name", "favourite_pkg")
fieldsMandatory <- c("comfort_grocery", "comfort_plane")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

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

# load all responses into a data.frame
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  #data <- dplyr::rbind_all(data)
  data <- do.call(rbind, data)
  data
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



# State Dropdown
states_list <- c("Alabama",
                 "Alaska",
                 "Arizona",
                 "Arkansas",
                 "California",
                 "Colorado",
                 "Connecticut",
                 "Delaware",
                 "District of Columbia",
                 "Florida",
                 "Georgia",
                 "Hawaii",
                 "Idaho",
                 "Illinois",
                 "Indiana",
                 "Iowa",
                 "Kansas",
                 "Kentucky",
                 "Louisiana",
                 "Maine",
                 "Maryland",
                 "Massachusetts",
                 "Michigan",
                 "Minnesota",
                 "Mississippi",
                 "Missouri",
                 "Montana",
                 "Nebraska",
                 "Nevada",
                 "New Hampshire",
                 "New Jersey",
                 "New Mexico",
                 "New York",
                 "North Carolina",
                 "North Dakota",
                 "Ohio",
                 "Oklahoma",
                 "Oregon",
                 "Pennsylvania",
                 "Rhode Island",
                 "South Carolina",
                 "South Dakota",
                 "Tennessee",
                 "Texas",
                 "Utah",
                 "Vermont",
                 "Virginia",
                 "Washington",
                 "West Virginia",
                 "Wisconsin",
                 "Wyoming")



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
               
               # Section 1: comfort
               h4("Part 1: Are You Comfortable?"),
               div(strong("Please rate your comfort level for the following:")),
               div(class = "questiontext", "Grocery shopping in-store"),
               div(class = "rowquestion",
                   div(class = "columnlabel", "Very Uncomfortable"),
                   div(class = "column",
                       radioGroupButtons("comfort_grocery", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                   div(class = "columnlabel", "Very Comfortable")
                   ),
               
               div(class = "questiontext", "Take a 3+ hour plane ride"),
               div(class = "rowquestion",
                   div(class = "columnlabel", "Very Uncomfortable"),
                   div(class = "column",
                       radioGroupButtons("comfort_plane", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                   div(class = "columnlabel", "Very Comfortable")
               ),
               # 
               # div(class = "questiontext", "Flying on an airplane for 3+ hours"),
               # div(class = "rowquestion",
               #     div(class = "columnlabel", "Very Uncomfortable"),
               #     div(class = "column",
               #         radioGroupButtons("comfort_plane", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               #     div(class = "columnlabel", "Very Comfortable")
               # ),
               # 
               div(class = "questiontext", "Eating at an restaurant INDOORS"),
               div(class = "rowquestion",
                   div(class = "columnlabel", "Very Uncomfortable"),
                   div(class = "column",
                       radioGroupButtons("comfort_restaurant_in", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                   div(class = "columnlabel", "Very Comfortable")
               ),
               
               div(class = "questiontext", "Eating at an restaurant OUTDOORS"),
               div(class = "rowquestion",
                   div(class = "columnlabel", "Very Uncomfortable"),
                   div(class = "column",
                       radioGroupButtons("comfort_restaurant_out", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                   div(class = "columnlabel", "Very Comfortable")
               ),
               
               div(class = "questiontext", "Seeing friends/family INDOORS"),
               div(class = "rowquestion",
                   div(class = "columnlabel", "Very Uncomfortable"),
                   div(class = "column",
                       radioGroupButtons("comfort_friends_in", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                   div(class = "columnlabel", "Very Comfortable")
               ),
               
               div(class = "questiontext", "Seeing friends/family OUTDOORS"),
               div(class = "rowquestion",
                   div(class = "columnlabel", "Very Uncomfortable"),
                   div(class = "column",
                       radioGroupButtons("comfort_friends_out", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                   div(class = "columnlabel", "Very Comfortable")
               ),
               
               div(class = "questiontext", "Attending large events (30+ people) that take place partially or fully indoors (weddings, parties, etc.)"),
               div(class = "rowquestion",
                   div(class = "columnlabel", "Very Uncomfortable"),
                   div(class = "column",
                       radioGroupButtons("comfort_event_in", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                   div(class = "columnlabel", "Very Comfortable")
               ),
               
               div(class = "questiontext", "Attending large events (30+ people) that take place fully outdoors (weddings, parties, etc.)"),
               div(class = "rowquestion",
                   div(class = "columnlabel", "Very Uncomfortable"),
                   div(class = "column",
                       radioGroupButtons("comfort_event_out", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                   div(class = "columnlabel", "Very Comfortable")
               ),
               
               div(class = "questiontext", "Receiving elective medical treatments (dental cleanings, check-ups, etc.)"),
               div(class = "rowquestion",
                   div(class = "columnlabel", "Very Uncomfortable"),
                   div(class = "column",
                       radioGroupButtons("comfort_medical", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                   div(class = "columnlabel", "Very Comfortable")
               ),
               
               div(class = "questiontext", "Receiving personal services (haircuts, manicures, etc.)"),
               div(class = "rowquestion",
                   div(class = "columnlabel", "Very Uncomfortable"),
                   div(class = "column",
                       radioGroupButtons("comfort_personal", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                   div(class = "columnlabel", "Very Comfortable")
               ),
               
               

               # Section 2: behavior
               div(class = "spacer"),
               h4("Part 2: What Have You Done?"),
               div("When was the last time you did any of the following?"),
               
               div(class = "questiontext", "I have done grocery shopping in-store..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_grocery", NULL, c("in the last month", "in the last quarter", "since February", "since before February"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
               div(class = "questiontext", "I have taken a 3+ hour plane ride..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_plane", NULL, c("in the last month", "in the last quarter", "since February", "since before February"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
               div(class = "questiontext", "I have eaten at a restaurant INDOORS..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_restaurant_in", NULL, c("in the last month", "in the last quarter", "since February", "since before February"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
               div(class = "questiontext", "I have eaten at a restaurant OUTDOORS..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_restaurant_out", NULL, c("in the last month", "in the last quarter", "since February", "since before February"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
               div(class = "questiontext", "I have seen family/friends INDOORS..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_friends_in", NULL, c("in the last month", "in the last quarter", "since February", "since before February"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
               div(class = "questiontext", "I have seen family/friends OUTDOORS..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_friends_out", NULL, c("in the last month", "in the last quarter", "since February", "since before February"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
               div(class = "questiontext", "I have attended a large event that is partially or fully INDOORS..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_event_in", NULL, c("in the last month", "in the last quarter", "since February", "since before February"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
               div(class = "questiontext", "I have attended a large event that is partially or fully OUTDOORS..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_event_out", NULL, c("in the last month", "in the last quarter", "since February", "since before February"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
               div(class = "questiontext", "I have recieved elective medical treatments..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_medical", NULL, c("in the last month", "in the last quarter", "since February", "since before February"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
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
               
               # Section 3: Feelings about COVID-19
               div(class = "spacer"),
               h4("Part 3: How do you feel about your feelings?"),
               div("Please rate how much you agree with the following statements. 1 = Very much disagree; 4 = Very much agree"),
               
               div(class = "questiontext", "I am worried about catching COVID-19"),
               div(class = "rowquestion",
                   div(class = "columnlabel", "Very Much Disagree"),
                   div(class = "column",
                       radioGroupButtons("feelings_catch", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                   div(class = "columnlabel", "Very Much Agree")
               ),
               
               div(class = "questiontext", "I am worried about being a spreader of COVID-19"),
               div(class = "rowquestion",
                   div(class = "columnlabel", "Very Much Disagree"),
                   div(class = "column",
                       radioGroupButtons("feelings_spread", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                   div(class = "columnlabel", "Very Much Agree")
               ),
               
               div(class = "questiontext", "I am taking the appropriate amount of precautions around COVID-19."),
               div(class = "rowquestion",
                   div(class = "columnlabel", "Very Much Disagree"),
                   div(class = "column",
                       radioGroupButtons("feelings_precaution", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                   div(class = "columnlabel", "Very Much Agree")
               ),
               
               div(class = "questiontext", "I think COVID-19 needs to be taken seriously."),
               div(class = "rowquestion",
                   div(class = "columnlabel", "Very Much Disagree"),
                   div(class = "column",
                       radioGroupButtons("feelings_severity", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                   div(class = "columnlabel", "Very Much Agree")
               ),
               
               # Section 4: About You
               div(class = "spacer"),
               h4("Part 4: Tell me about yourself"),
               div("A couple of demographics questions"),
               # selectInput("about_state", "Where do you live?",
               #             states_list),
               selectInput("about_age", "How old are you?",
                           c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")),
               selectInput("about_gender", "To which gender identity do you most identify?",
                           c("Female", "Male", "Transgender", "Gender Variant / Non-Conforming")),
               radioButtons("about_job", "I am a frontline worker.", c("Yes", "No"), inline = TRUE),
               radioButtons("about_health", "I have a health condition that makes me a high-risk of a serious illness from COVID-19.", c("Yes", "No"), inline = TRUE),
               
               
               
               actionButton("submit", "Submit", class = "btn-primary"),
               
               shinyjs::hidden(
                 span(id = "submit_msg", "Submitting..."),
                 div(id = "error",
                     div(br(), tags$b("Error: "), span(id = "error_msg"))
                 )
               )
             ),
             
             shinyjs::hidden(
               div(
                 id = "thankyou_msg",
                 h3("Thanks, your response was submitted successfully!"),
                 actionLink("submit_another", "Submit another response")
               )
             )
      )
    ),
    fluidRow(
      column(12,
             uiOutput("adminPanelContainer")
      )
    )
  ),


# Server ------------------------------------------------------------------
  server = function(input, output, session) {
    
    # Enable the Submit button when all mandatory fields are filled out
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
    
    # Gather all the form inputs (and add timestamp)
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, timestamp = epochTime())
      data <- t(data)
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
    
    # render the admin panel
    output$adminPanelContainer <- renderUI({
      if (!isAdmin()) return()
      
      div(
        id = "adminPanel",
        h2("Previous responses (only visible to admins)"),
        downloadButton("downloadBtn", "Download responses"), br(), br(),
        DT::dataTableOutput("responsesTable"), br(),
        "* There were over 2000 responses by Dec 4 2017, so all data prior to that date was deleted as a cleanup"
      )
    })
    
    # determine if current user is admin
    isAdmin <- reactive({
      is.null(session$user) || session$user %in% adminUsers
    })    
    
    # Show the responses in the admin table
    output$responsesTable <- DT::renderDataTable({
      data <- loadData()
      data$timestamp <- as.POSIXct(data$timestamp, origin="1970-01-01")
      DT::datatable(
        data,
        rownames = FALSE,
        options = list(searching = FALSE, lengthChange = FALSE)
      )
    })
    
    # Allow user to download responses
    output$downloadBtn <- downloadHandler(
      filename = function() { 
        sprintf("mimic-google-form_%s.csv", humanTime())
      },
      content = function(file) {
        write.csv(loadData(), file, row.names = FALSE)
      }
    )    
  }
)