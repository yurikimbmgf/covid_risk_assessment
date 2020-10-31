# Test App 2
# Building this off of https://github.com/daattali/shiny-server/blob/master/mimic-google-form/app.R
# http://shinyapps.dreamrs.fr/shinyWidgets/
# https://stackoverflow.com/questions/49488228/how-to-show-spinning-wheel-or-busy-icon-while-waiting-in-shiny

# Todo
# Need to make it so Google Drive is the storage.....
# Make it pretty? (Font colors, fonts themselves using webkit)
# fix spacing between sections
# Tweaks (making them all say OUTDOORS; Not February but March; Footer font size)
# Tweak ("I wear a face mask any time I am INDOORS" - excluding your home)

# Packages ----------------------------------------------------------------
library(tidyverse)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(janitor)
library(shinycssloaders)
# library(googlesheets4)
# library(googledrive)
library(rdrop2)



# setting up dropbox ------------------------------------------------------
drop_auth()
token <- drop_auth()
saveRDS(token, file = "token.rds")
drop_dir('covid_survey_responses')
# rdrop2::drop_read_csv('covid_survey_responses/dummy.csv')

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
               # "behavior_mask",
               "mask_indoor",
               "mask_outdoor",
               "feelings_catch",
               "feelings_spread",
               "feelings_precaution",
               "feelings_severity",
               "about_age",
               "about_gender",
               "about_job",
               "about_health"
)


# which fields are mandatory
# fieldsMandatory <- c("name", "favourite_pkg")
fieldsMandatory <- c("comfort_grocery",
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
                     # "behavior_mask",
                     "mask_indoor",
                     "mask_outdoor",
                     "feelings_catch",
                     "feelings_spread",
                     "feelings_precaution",
                     "feelings_severity",
                     "about_age",
                     "about_gender",
                     "about_job",
                     "about_health")

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
# saveData <- function(data) {
#   fileName <- sprintf("%s_%s.csv",
#                       humanTime(),
#                       digest::digest(data))
#   
#   write.csv(x = data, file = file.path(responsesDir, fileName),
#             row.names = FALSE, quote = TRUE)
# }

saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  write.csv(x = data, file = data)
  drop_upload(data, path = "drop_test")
  }
# rdrop2::drop_read_csv('covid_survey_responses/dummy.csv')


# load all responses into a data.frame
# loadData <- function() {
#   files <- list.files(file.path(responsesDir), full.names = TRUE)
#   data <- lapply(files, read.csv, stringsAsFactors = FALSE)
#   #data <- dplyr::rbind_all(data)
#   data <- do.call(rbind, data)
#   data
# }


loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(paste0("covid_survey_responses/", drop_dir('covid_survey_responses')$name), drop_read_csv)
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
   #header { background: #fff; border-bottom: 1px solid #ddd;  padding: 15px 15px 10px; }
  "
# margin: -20px -15px 0;


# usernames that are admins
# adminUsers <- c("admin", "prof")



# Functions to transform the responses to visuals
func_visual_1to4 <- function(df_name, variable_name, question_text, var1_rename, var2_rename) {
  df <- tabyl(df_name, variable_name) %>% 
    rename(choice = variable_name) %>% 
    mutate(bar_color = case_when(choice == df_name %>% filter(timestamp == max(timestamp)) %>% select(variable_name) %>% as.numeric() ~ "#e88f00",
                                 TRUE ~ "#124d47")) %>% 
    mutate(your_selection = case_when(choice == df_name %>% filter(timestamp == max(timestamp)) %>% select(variable_name) %>% as.numeric() ~ paste0(percent(percent, accuracy = 1), " (Your pick)"),
                                      TRUE ~ percent(percent, accuracy = 1))) %>% 
    mutate(choice = case_when(choice == 1 ~ var1_rename, 
                              choice == 2 ~ "2",
                              choice == 3 ~ "3",
                              choice == 4 ~ var2_rename))
  
  ggplot(data = df,
         aes(x = choice, 
             y = percent,
             fill = bar_color,
             color = bar_color,
             label = your_selection)) +
    geom_bar(stat = "identity") +
    geom_text(vjust = -1) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(label = percent) +
    theme_fivethirtyeight() +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = question_text,
         caption = paste0("(Total Responses: ", sum(df$n), ")")
    ) +
    theme(plot.title = element_text(size=12),
          plot.background = element_rect(fill = "#fdfdfd"),
          panel.grid.major.x = element_blank(),
          panel.background = element_rect(fill = "#fdfdfd"),
          plot.caption = element_text(hjust = 0, face = "italic"))
  
}


func_visual_behavior <- function(df_name, variable_name, question_text) {
  df <- tabyl(df_name, variable_name) %>% 
    rename(choice = variable_name) %>% 
    mutate(bar_color = case_when(choice == df_name %>% filter(timestamp == max(timestamp)) %>% select(variable_name) %>% as.character() ~ "#e88f00",
                                 TRUE ~ "#124d47")) %>% 
    mutate(your_selection = case_when(choice == df_name %>% filter(timestamp == max(timestamp)) %>% select(variable_name) %>% as.character() ~ paste0(percent(percent, accuracy = 1), " (Your pick)"),
                                      TRUE ~ percent(percent, accuracy = 1))) %>% 
    mutate(choice = fct_relevel(choice, "in the last month", "in the last quarter", "since February", "since before February"))
  
  ggplot(data = df,
         aes(x = choice, 
             y = percent,
             fill = bar_color,
             color = bar_color,
             label = your_selection)) +
    geom_bar(stat = "identity") +
    geom_text(vjust = -1) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(label = percent) +
    theme_fivethirtyeight() +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = question_text,
         caption = paste0("(Total Responses: ", sum(df$n), ")" )
    ) +
    theme(plot.title = element_text(size=12),
          plot.background = element_rect(fill = "#fdfdfd"),
          panel.grid.major.x = element_blank(),
          panel.background = element_rect(fill = "#fdfdfd"),
          plot.caption = element_text(hjust = 0, face = "italic"))
  
}




# Visuals without bar colors ----------------------------------------------
func_nohighlight_visual_1to4 <- function(df_name, variable_name, question_text, var1_rename, var2_rename) {
  df <- tabyl(df_name, variable_name) %>% 
    rename(choice = variable_name) %>% 
    mutate(bar_color = "#124d47") %>% 
    mutate(your_selection = percent(percent, accuracy = 1)) %>% 
    mutate(choice = case_when(choice == 1 ~ var1_rename, 
                              choice == 2 ~ "2",
                              choice == 3 ~ "3",
                              choice == 4 ~ var2_rename))
  
  ggplot(data = df,
         aes(x = choice, 
             y = percent,
             fill = bar_color,
             color = bar_color,
             label = your_selection)) +
    geom_bar(stat = "identity") +
    geom_text(vjust = -1) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(label = percent) +
    theme_fivethirtyeight() +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = question_text,
         caption = paste0("(Total Responses: ", sum(df$n), ")")
    ) +
    theme(plot.title = element_text(size=12),
          plot.background = element_rect(fill = "#fdfdfd"),
          panel.grid.major.x = element_blank(),
          panel.background = element_rect(fill = "#fdfdfd"),
          plot.caption = element_text(hjust = 0, face = "italic"))
  
}
func_nohighlight_visual_behavior <- function(df_name, variable_name, question_text) {
  df <- tabyl(df_name, variable_name) %>% 
    rename(choice = variable_name) %>% 
    mutate(bar_color = "#124d47") %>% 
    mutate(your_selection = percent(percent, accuracy = 1)) %>% 
    mutate(choice = fct_relevel(choice, "in the last month", "in the last quarter", "since February", "since before February"))
  
  ggplot(data = df,
         aes(x = choice, 
             y = percent,
             fill = bar_color,
             color = bar_color,
             label = your_selection)) +
    geom_bar(stat = "identity") +
    geom_text(vjust = -1) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(label = percent) +
    theme_fivethirtyeight() +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = question_text,
         caption = paste0("(Total Responses: ", sum(df$n), ")" )
    ) +
    theme(plot.title = element_text(size=12),
          plot.caption = element_text(hjust = 0, face = "italic"))
}



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
    title = "COVID-19 Risk Tolerance Survey",
    
    div(id = "header",
        h1("COVID-19 Risk Tolerance Survey"),
        h4("Please take the short survey below about your comfort level, behaviors, and feelings about your actions.",
           br(),
           "After you submit your answers, you'll how your responses compare to others who have taken the survey!",
           br(),
           strong("Please note that all answers are required!"),
           br(),
           br(),
           "Thanks for taking part in this little experiment!"
        ),
        div(
          "(Have you already taken the survey and just want to see the results?", 
          actionLink("skip_button", "Click Here"),
          
          ")"
        )
        
    ),
    
    fluidRow(
      column(12,
             div(
               id = "form",
               
               # Section 1: comfort
               h4("Part 1: Are You Comfortable?"),
               div(strong("Please rate your comfort level for the following:")),
               div(class = "questiontext", "Grocery shopping in-store"),
               # div(class = "questiontext", labelMandatory("Grocery shopping in-store")),
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
               
               
               # Section 2: behavior
               div(class = "spacer"),
               h4("Part 3: What's On Your Face?"),
               div("Please describe your face masking wearing habits?"),
               
               div(class = "questiontext", "I wear a face mask any time I am INDOORS"),
               div(class = "rowquestion",
                   div(class = "columnlabel", "I never wear a face mask"),
                   div(class = "column",
                       radioGroupButtons("mask_indoor", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                   div(class = "columnlabel", "I wear a face mask 100% of the time")
               ),
               div(class = "questiontext", "I wear a face mask any time I am OUTDOORS"),
               div(class = "rowquestion",
                   div(class = "columnlabel", "I never wear a face mask"),
                   div(class = "column",
                       radioGroupButtons("mask_outdoor", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                   div(class = "columnlabel", "I wear a face mask 100% of the time")
               ),
               
               # Section 4: Feelings about COVID-19
               div(class = "spacer"),
               h4("Part 4: How Ya Feelin'?"),
               div("Please rate how much you agree with the following statements."),
               
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
               
               # Section 3: About You
               div(class = "spacer"),
               h4("Part 5: Who Are You?"),
               div("A couple of demographics questions. NOTE: This data is not shared publicly."),
               selectInput("about_age", "How old are you?",
                           c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+", "Prefer Not To Say")),
               selectInput("about_gender", "To which gender identity do you most identify?",
                           c("Female", "Male", "Transgender", "Gender Variant / Non-Conforming", "Prefer Not To Say")),
               radioButtons("about_job", "I am a frontline worker.", c("Yes", "No", "Prefer Not To Say"), inline = TRUE),
               radioButtons("about_health", "I have a health condition that makes me a high-risk of a serious illness from COVID-19.", c("Yes", "No", "Prefer Not To Say"), inline = TRUE),
               
               
               
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
                 h3("Thanks for sharing! See how you compare to others who took this survey below!")
               )
             )
      )
    ),
    fluidRow(
      column(12,
             uiOutput("adminPanelContainer")
      )
    ),
    fluidRow(
      hr(),
      div( 
        span("Much of this code was taken from "),
        a("Dean Attali's Mimic Google Form", href = "https://deanattali.com/2015/06/14/mimicking-google-form-shiny/"),
        span("project."),
        br(),
        span("Created by"),
        a("Yuri", href = "http://www.twitter.com/yurikim")
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
    
    
    # Jump straight to results
    observeEvent(input$skip_button, {
      
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
    
    
    # render Results
    observeEvent(input$submit, {
      output$adminPanelContainer <- renderUI({
        div(
          id = "adminPanel",
          h4("Part 1: Are You Comfortable?"),
          plotOutput("visual_comfort_grocery", height = 310, width = 600)  %>% withSpinner(color="#e88f00"),
          plotOutput("visual_comfort_plane", height = 310, width = "100%"),
          plotOutput("visual_comfort_restaurant_in", height = 310, width = "100%"),
          plotOutput("visual_comfort_restaurant_out", height = 310, width = "100%"),
          plotOutput("visual_comfort_friends_in", height = 310, width = "100%"),
          plotOutput("visual_comfort_friends_out", height = 310, width = "100%"),
          plotOutput("visual_comfort_event_in", height = 310, width = "100%"),
          plotOutput("visual_comfort_event_out", height = 310, width = "100%"),
          plotOutput("visual_comfort_medical", height = 310, width = "100%"),
          plotOutput("visual_comfort_personal", height = 310, width = "100%"),
          
          div(class = "spacer"),
          h4("Part 2: What Have You Done?"),
          div("When was the last time you did any of the following?"),
          plotOutput("visual_behavior_grocery", height = 310, width = "100%"),
          plotOutput("visual_behavior_restaurant_in", height = 310, width = "100%"),
          plotOutput("visual_behavior_restaurant_out", height = 310, width = "100%"),
          plotOutput("visual_behavior_friends_in", height = 310, width = "100%"),
          plotOutput("visual_behavior_friends_out", height = 310, width = "100%"),
          plotOutput("visual_behavior_event_in", height = 310, width = "100%"),
          plotOutput("visual_behavior_event_out", height = 310, width = "100%"),
          plotOutput("visual_behavior_medical", height = 310, width = "100%"),
          plotOutput("visual_behavior_personal", height = 310, width = "100%"),
          
          div(class = "spacer"),
          h4("Part 3: What's On Your Face?"),
          div("Please describe your face masking wearing habits."),
          plotOutput("visual_mask_indoor", height = 310, width = "100%"),
          plotOutput("visual_mask_outdoor", height = 310, width = "100%"),
          
          div(class = "spacer"),
          h4("Part 4: How Ya Feelin'?"),
          div("Please describe your face masking wearing habits."),
          plotOutput("visual_feelings_catch", height = 310, width = "100%"),
          plotOutput("visual_feelings_spread", height = 310, width = "100%"),
          plotOutput("visual_feelings_precaution", height = 310, width = "100%"),
          plotOutput("visual_feelings_severity", height = 310, width = "100%")
          
          
        )
      })}
    )
    
    # Only results - no highlights
    observeEvent(input$skip_button, {
      output$adminPanelContainer <- renderUI({
        div(
          id = "adminPanel",
          h4("Part 1: Are You Comfortable?"),
          plotOutput("nohighlight_visual_comfort_grocery", height = 310, width = "100%")  %>% withSpinner(color="#e88f00"),
          plotOutput("nohighlight_visual_comfort_plane", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_comfort_restaurant_in", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_comfort_restaurant_out", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_comfort_friends_in", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_comfort_friends_out", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_comfort_event_in", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_comfort_event_out", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_comfort_medical", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_comfort_personal", height = 310, width = "100%"),
          
          div(class = "spacer"),
          h4("Part 2: What Have You Done?"),
          div("When was the last time you did any of the following?"),
          plotOutput("nohighlight_visual_behavior_grocery", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_behavior_restaurant_in", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_behavior_restaurant_out", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_behavior_friends_in", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_behavior_friends_out", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_behavior_event_in", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_behavior_event_out", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_behavior_medical", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_behavior_personal", height = 310, width = "100%"),
          
          div(class = "spacer"),
          h4("Part 3: What's On Your Face?"),
          div("Please describe your face masking wearing habits."),
          plotOutput("nohighlight_visual_mask_indoor", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_mask_outdoor", height = 310, width = "100%"),
          
          div(class = "spacer"),
          h4("Part 4: How Ya Feelin'?"),
          div("Please describe your face masking wearing habits."),
          plotOutput("nohighlight_visual_feelings_catch", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_feelings_spread", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_feelings_precaution", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_feelings_severity", height = 310, width = "100%")
          
          
        )
      })}
    )
    
    # All the visuals
    # COMFORT
    output$visual_comfort_grocery <- renderPlot(func_visual_1to4(df_name = loadData(),
                                                                 variable_name = "comfort_grocery",
                                                                 question_text = "Comfort: Grocery shopping in-store",
                                                                 var1_rename =  "1: Very Uncomfortable",
                                                                 var2_rename =  "4: Very Comfortable"), height = 275, width = 500)
    output$visual_comfort_plane <- renderPlot(func_visual_1to4(df_name = loadData(),
                                                               variable_name = "comfort_plane",
                                                               var1_rename =  "1: Very Uncomfortable",
                                                               var2_rename =  "4: Very Comfortable",
                                                               question_text = "Comfort: Take a 3+ hour plane ride"), height = 275, width = 500)
    output$visual_comfort_restaurant_in <- renderPlot(func_visual_1to4(df_name = loadData(),
                                                                       variable_name = "comfort_restaurant_in",
                                                                       var1_rename =  "1: Very Uncomfortable",
                                                                       var2_rename =  "4: Very Comfortable",
                                                                       question_text = "Comfort: Eating at an restaurant INDOORS"), height = 275, width = 500)
    output$visual_comfort_restaurant_out <- renderPlot(func_visual_1to4(df_name = loadData(),
                                                                        variable_name = "comfort_restaurant_out",
                                                                        var1_rename =  "1: Very Uncomfortable",
                                                                        var2_rename =  "4: Very Comfortable",
                                                                        question_text = "Comfort: Eating at an restaurant OUTDOORS"), height = 275, width = 500)
    output$visual_comfort_friends_in <- renderPlot(func_visual_1to4(df_name = loadData(),
                                                                    variable_name = "comfort_friends_in",
                                                                    var1_rename =  "1: Very Uncomfortable",
                                                                    var2_rename =  "4: Very Comfortable",
                                                                    question_text = "Comfort: Seeing friends/family INDOORS"), height = 275, width = 500)
    output$visual_comfort_friends_out <- renderPlot(func_visual_1to4(df_name = loadData(),
                                                                     variable_name = "comfort_friends_out",
                                                                     var1_rename =  "1: Very Uncomfortable",
                                                                     var2_rename =  "4: Very Comfortable",
                                                                     question_text = "Comfort: Seeing friends/family OUTDOORS"), height = 275, width = 500)
    output$visual_comfort_event_in <- renderPlot(func_visual_1to4(df_name = loadData(),
                                                                  variable_name = "comfort_event_in",
                                                                  var1_rename =  "1: Very Uncomfortable",
                                                                  var2_rename =  "4: Very Comfortable",
                                                                  question_text = "Comfort: Attending large events (30+ people) that take place partially\nor fully INDOORS (weddings, parties, etc.)"), height = 275, width = 500)
    output$visual_comfort_event_out <- renderPlot(func_visual_1to4(df_name = loadData(),
                                                                   variable_name = "comfort_event_out",
                                                                   var1_rename =  "1: Very Uncomfortable",
                                                                   var2_rename =  "4: Very Comfortable",
                                                                   question_text = "Comfort: Attending large events (30+ people) that take place fully\nOUTDOORS (weddings, parties, etc.)"), height = 275, width = 500)
    output$visual_comfort_medical <- renderPlot(func_visual_1to4(df_name = loadData(),
                                                                 variable_name = "comfort_medical",
                                                                 var1_rename =  "1: Very Uncomfortable",
                                                                 var2_rename =  "4: Very Comfortable",
                                                                 question_text = "Comfort: Receiving elective medical treatments (dental cleanings, check-ups, etc.)"), height = 275, width = 500)
    output$visual_comfort_personal <- renderPlot(func_visual_1to4(df_name = loadData(),
                                                                  variable_name = "comfort_personal",
                                                                  var1_rename =  "1: Very Uncomfortable",
                                                                  var2_rename =  "4: Very Comfortable",
                                                                  question_text = "Comfort: Receiving personal services (haircuts, manicures, etc.)"), height = 275, width = 500)
    
    # BEHAVIOR
    output$visual_behavior_grocery <- renderPlot(func_visual_behavior(df_name = loadData(),
                                                                      variable_name = "behavior_grocery",
                                                                      question_text = "I have done grocery shopping in-store..."), height = 275, width = 500)
    output$visual_behavior_plane <- renderPlot(func_visual_behavior(df_name = loadData(),
                                                                    variable_name = "behavior_plane",
                                                                    question_text = "I have taken a 3+ hour plane ride..."), height = 275, width = 500)
    output$visual_behavior_restaurant_in <- renderPlot(func_visual_behavior(df_name = loadData(),
                                                                            variable_name = "behavior_restaurant_in",
                                                                            question_text = "I have eaten at a restaurant INDOORS..."), height = 275, width = 500)
    output$visual_behavior_restaurant_out <- renderPlot(func_visual_behavior(df_name = loadData(),
                                                                             variable_name = "behavior_restaurant_out",
                                                                             question_text = "I have eaten at a restaurant OUTDOORS..."), height = 275, width = 500)
    output$visual_behavior_friends_in <- renderPlot(func_visual_behavior(df_name = loadData(),
                                                                         variable_name = "behavior_friends_in",
                                                                         question_text = "I have seen family/friends INDOORS..."), height = 275, width = 500)
    output$visual_behavior_friends_out <- renderPlot(func_visual_behavior(df_name = loadData(),
                                                                          variable_name = "behavior_friends_out",
                                                                          question_text = "I have seen family/friends OUTDOORS..."), height = 275, width = 500)
    output$visual_behavior_event_in <- renderPlot(func_visual_behavior(df_name = loadData(),
                                                                       variable_name = "behavior_event_in",
                                                                       question_text = "I have attended a large event that is partially or fully INDOORS..."), height = 275, width = 500)
    output$visual_behavior_event_out <- renderPlot(func_visual_behavior(df_name = loadData(),
                                                                        variable_name = "behavior_event_out",
                                                                        question_text = "I have attended a large event that is partially or fully OUTDOORS..."), height = 275, width = 500)
    output$visual_behavior_medical <- renderPlot(func_visual_behavior(df_name = loadData(),
                                                                      variable_name = "behavior_medical",
                                                                      question_text = "I have recieved elective medical treatments..."), height = 275, width = 500)
    output$visual_behavior_personal <- renderPlot(func_visual_behavior(df_name = loadData(),
                                                                       variable_name = "behavior_personal",
                                                                       question_text = "I have recieved personal services..."), height = 275, width = 500)
    
    # MASK
    output$visual_mask_indoor <- renderPlot(func_visual_1to4(df_name = loadData(),
                                                             variable_name = "mask_indoor",
                                                             var1_rename =  "1: I Never Wear a Face Mask",
                                                             var2_rename =  "4: I Wear a Face Mask\n100% of the Time",
                                                             question_text = "I wear a face mask any time I am INDOORS"), height = 275, width = 500)
    output$visual_mask_outdoor <- renderPlot(func_visual_1to4(df_name = loadData(),
                                                              variable_name = "mask_outdoor",
                                                              var1_rename =  "1: I Never Wear a Face Mask",
                                                              var2_rename =  "4: I Wear a Face Mask\n100% of the Time",
                                                              question_text = "I wear a face mask any time I am OUTDOORS"), height = 275, width = 500)
    
    
    # FEELING
    output$visual_feelings_catch <- renderPlot(func_visual_1to4(df_name = loadData(),
                                                                variable_name = "feelings_catch",
                                                                var1_rename =  "1: Very Much Disagree",
                                                                var2_rename =  "4: Very Much Agree",
                                                                question_text = "I am worried about catching COVID-19"), height = 275, width = 500)
    output$visual_feelings_spread <- renderPlot(func_visual_1to4(df_name = loadData(),
                                                                 variable_name = "feelings_spread",
                                                                 var1_rename =  "1: Very Much Disagree",
                                                                 var2_rename =  "4: Very Much Agree",
                                                                 question_text = "I am worried about being a spreader of COVID-19"), height = 275, width = 500)
    output$visual_feelings_precaution <- renderPlot(func_visual_1to4(df_name = loadData(),
                                                                     variable_name = "feelings_precaution",
                                                                     var1_rename =  "1: Very Much Disagree",
                                                                     var2_rename =  "4: Very Much Agree",
                                                                     question_text = "I am taking the appropriate amount of precautions around COVID-19"), height = 275, width = 500)
    output$visual_feelings_severity <- renderPlot(func_visual_1to4(df_name = loadData(),
                                                                   variable_name = "feelings_severity",
                                                                   var1_rename =  "1: Very Much Disagree",
                                                                   var2_rename =  "4: Very Much Agree",
                                                                   question_text = "I think COVID-19 needs to be taken seriously"), height = 275, width = 500)
    
    
    
    
    # No highlight visuals reactive -------------------------------------------
    
    output$nohighlight_visual_comfort_grocery <- renderPlot(func_nohighlight_visual_1to4(df_name = loadData(),
                                                                                         variable_name = "comfort_grocery",
                                                                                         question_text = "Comfort: Grocery shopping in-store",
                                                                                         var1_rename =  "1: Very Uncomfortable",
                                                                                         var2_rename =  "4: Very Comfortable"), height = 275, width = 500)
    output$nohighlight_visual_comfort_plane <- renderPlot(func_nohighlight_visual_1to4(df_name = loadData(),
                                                                                       variable_name = "comfort_plane",
                                                                                       var1_rename =  "1: Very Uncomfortable",
                                                                                       var2_rename =  "4: Very Comfortable",
                                                                                       question_text = "Comfort: Take a 3+ hour plane ride"), height = 275, width = 500)
    output$nohighlight_visual_comfort_restaurant_in <- renderPlot(func_nohighlight_visual_1to4(df_name = loadData(),
                                                                                               variable_name = "comfort_restaurant_in",
                                                                                               var1_rename =  "1: Very Uncomfortable",
                                                                                               var2_rename =  "4: Very Comfortable",
                                                                                               question_text = "Comfort: Eating at an restaurant INDOORS"), height = 275, width = 500)
    output$nohighlight_visual_comfort_restaurant_out <- renderPlot(func_nohighlight_visual_1to4(df_name = loadData(),
                                                                                                variable_name = "comfort_restaurant_out",
                                                                                                var1_rename =  "1: Very Uncomfortable",
                                                                                                var2_rename =  "4: Very Comfortable",
                                                                                                question_text = "Comfort: Eating at an restaurant OUTDOORS"), height = 275, width = 500)
    output$nohighlight_visual_comfort_friends_in <- renderPlot(func_nohighlight_visual_1to4(df_name = loadData(),
                                                                                            variable_name = "comfort_friends_in",
                                                                                            var1_rename =  "1: Very Uncomfortable",
                                                                                            var2_rename =  "4: Very Comfortable",
                                                                                            question_text = "Comfort: Seeing friends/family INDOORS"), height = 275, width = 500)
    output$nohighlight_visual_comfort_friends_out <- renderPlot(func_nohighlight_visual_1to4(df_name = loadData(),
                                                                                             variable_name = "comfort_friends_out",
                                                                                             var1_rename =  "1: Very Uncomfortable",
                                                                                             var2_rename =  "4: Very Comfortable",
                                                                                             question_text = "Comfort: Seeing friends/family OUTDOORS"), height = 275, width = 500)
    output$nohighlight_visual_comfort_event_in <- renderPlot(func_nohighlight_visual_1to4(df_name = loadData(),
                                                                                          variable_name = "comfort_event_in",
                                                                                          var1_rename =  "1: Very Uncomfortable",
                                                                                          var2_rename =  "4: Very Comfortable",
                                                                                          question_text = "Comfort: Attending large events (30+ people) that take place partially\nor fully INDOORS (weddings, parties, etc.)"), height = 275, width = 500)
    output$nohighlight_visual_comfort_event_out <- renderPlot(func_nohighlight_visual_1to4(df_name = loadData(),
                                                                                           variable_name = "comfort_event_out",
                                                                                           var1_rename =  "1: Very Uncomfortable",
                                                                                           var2_rename =  "4: Very Comfortable",
                                                                                           question_text = "Comfort: Attending large events (30+ people) that take place fully\nOUTDOORS (weddings, parties, etc.)"), height = 275, width = 500)
    output$nohighlight_visual_comfort_medical <- renderPlot(func_nohighlight_visual_1to4(df_name = loadData(),
                                                                                         variable_name = "comfort_medical",
                                                                                         var1_rename =  "1: Very Uncomfortable",
                                                                                         var2_rename =  "4: Very Comfortable",
                                                                                         question_text = "Comfort: Receiving elective medical treatments (dental cleanings, check-ups, etc.)"), height = 275, width = 500)
    output$nohighlight_visual_comfort_personal <- renderPlot(func_nohighlight_visual_1to4(df_name = loadData(),
                                                                                          variable_name = "comfort_personal",
                                                                                          var1_rename =  "1: Very Uncomfortable",
                                                                                          var2_rename =  "4: Very Comfortable",
                                                                                          question_text = "Comfort: Receiving personal services (haircuts, manicures, etc.)"), height = 275, width = 500)
    
    # BEHAVIOR
    output$nohighlight_visual_behavior_grocery <- renderPlot(func_nohighlight_visual_behavior(df_name = loadData(),
                                                                                              variable_name = "behavior_grocery",
                                                                                              question_text = "I have done grocery shopping in-store..."), height = 275, width = 500)
    output$nohighlight_visual_behavior_plane <- renderPlot(func_nohighlight_visual_behavior(df_name = loadData(),
                                                                                            variable_name = "behavior_plane",
                                                                                            question_text = "I have taken a 3+ hour plane ride..."), height = 275, width = 500)
    output$nohighlight_visual_behavior_restaurant_in <- renderPlot(func_nohighlight_visual_behavior(df_name = loadData(),
                                                                                                    variable_name = "behavior_restaurant_in",
                                                                                                    question_text = "I have eaten at a restaurant INDOORS..."), height = 275, width = 500)
    output$nohighlight_visual_behavior_restaurant_out <- renderPlot(func_nohighlight_visual_behavior(df_name = loadData(),
                                                                                                     variable_name = "behavior_restaurant_out",
                                                                                                     question_text = "I have eaten at a restaurant OUTDOORS..."), height = 275, width = 500)
    output$nohighlight_visual_behavior_friends_in <- renderPlot(func_nohighlight_visual_behavior(df_name = loadData(),
                                                                                                 variable_name = "behavior_friends_in",
                                                                                                 question_text = "I have seen family/friends INDOORS..."), height = 275, width = 500)
    output$nohighlight_visual_behavior_friends_out <- renderPlot(func_nohighlight_visual_behavior(df_name = loadData(),
                                                                                                  variable_name = "behavior_friends_out",
                                                                                                  question_text = "I have seen family/friends OUTDOORS..."), height = 275, width = 500)
    output$nohighlight_visual_behavior_event_in <- renderPlot(func_nohighlight_visual_behavior(df_name = loadData(),
                                                                                               variable_name = "behavior_event_in",
                                                                                               question_text = "I have attended a large event that is partially or fully INDOORS..."), height = 275, width = 500)
    output$nohighlight_visual_behavior_event_out <- renderPlot(func_nohighlight_visual_behavior(df_name = loadData(),
                                                                                                variable_name = "behavior_event_out",
                                                                                                question_text = "I have attended a large event that is partially or fully OUTDOORS..."), height = 275, width = 500)
    output$nohighlight_visual_behavior_medical <- renderPlot(func_nohighlight_visual_behavior(df_name = loadData(),
                                                                                              variable_name = "behavior_medical",
                                                                                              question_text = "I have recieved elective medical treatments..."), height = 275, width = 500)
    output$nohighlight_visual_behavior_personal <- renderPlot(func_nohighlight_visual_behavior(df_name = loadData(),
                                                                                               variable_name = "behavior_personal",
                                                                                               question_text = "I have recieved personal services..."), height = 275, width = 500)
    
    # MASK
    output$nohighlight_visual_mask_indoor <- renderPlot(func_nohighlight_visual_1to4(df_name = loadData(),
                                                                                     variable_name = "mask_indoor",
                                                                                     var1_rename =  "1: I Never Wear a Face Mask",
                                                                                     var2_rename =  "4: I Wear a Face Mask\n100% of the Time",
                                                                                     question_text = "I wear a face mask any time I am INDOORS"), height = 275, width = 500)
    output$nohighlight_visual_mask_outdoor <- renderPlot(func_nohighlight_visual_1to4(df_name = loadData(),
                                                                                      variable_name = "mask_outdoor",
                                                                                      var1_rename =  "1: I Never Wear a Face Mask",
                                                                                      var2_rename =  "4: I Wear a Face Mask\n100% of the Time",
                                                                                      question_text = "I wear a face mask any time I am OUTDOORS"), height = 275, width = 500)
    
    
    # FEELING
    output$nohighlight_visual_feelings_catch <- renderPlot(func_nohighlight_visual_1to4(df_name = loadData(),
                                                                                        variable_name = "feelings_catch",
                                                                                        var1_rename =  "1: Very Much Disagree",
                                                                                        var2_rename =  "4: Very Much Agree",
                                                                                        question_text = "I am worried about catching COVID-19"), height = 275, width = 500)
    output$nohighlight_visual_feelings_spread <- renderPlot(func_nohighlight_visual_1to4(df_name = loadData(),
                                                                                         variable_name = "feelings_spread",
                                                                                         var1_rename =  "1: Very Much Disagree",
                                                                                         var2_rename =  "4: Very Much Agree",
                                                                                         question_text = "I am worried about being a spreader of COVID-19"), height = 275, width = 500)
    output$nohighlight_visual_feelings_precaution <- renderPlot(func_nohighlight_visual_1to4(df_name = loadData(),
                                                                                             variable_name = "feelings_precaution",
                                                                                             var1_rename =  "1: Very Much Disagree",
                                                                                             var2_rename =  "4: Very Much Agree",
                                                                                             question_text = "I am taking the appropriate amount of precautions around COVID-19"), height = 275, width = 500)
    output$nohighlight_visual_feelings_severity <- renderPlot(func_nohighlight_visual_1to4(df_name = loadData(),
                                                                                           variable_name = "feelings_severity",
                                                                                           var1_rename =  "1: Very Much Disagree",
                                                                                           var2_rename =  "4: Very Much Agree",
                                                                                           question_text = "I think COVID-19 needs to be taken seriously"), height = 275, width = 500)    
    
  }
)