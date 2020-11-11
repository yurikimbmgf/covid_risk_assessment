# Test App 2
# Building this off of https://github.com/daattali/shiny-server/blob/master/mimic-google-form/app.R
# http://shinyapps.dreamrs.fr/shinyWidgets/
# https://stackoverflow.com/questions/49488228/how-to-show-spinning-wheel-or-busy-icon-while-waiting-in-shiny



# Packages ----------------------------------------------------------------
library(tidyverse)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(janitor)
library(shinycssloaders)
library(googlesheets4)
library(googledrive)
library(scales)
library(ggthemes)

# Getting Google Sheets to work -------------------------------------------
options(
  gargle_oob_default = TRUE, # from https://github.com/jennybc/googlesheets/issues/343#issuecomment-370202906
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = "yurickim@gmail.com",
  gargle_quiet = FALSE
)

options(gargle_quiet = FALSE)

# gs4_auth(
#   email = gargle::gargle_oauth_email(),
#   path = NULL,
#   scopes = "https://www.googleapis.com/auth/spreadsheets",
#   cache = gargle::gargle_oauth_cache(),
#   use_oob = gargle::gargle_oob_default(),
#   token = NULL
# )
# 4/5wGnxCij9h-Y2Unx6MYUGmif15fYOihEO_kOQh0mS6GWhnlYnREAAFs
googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1JcgeXPfYdqlbCQ_jcV6D6Sj_LCRwMSIB610Vqzmi45I/edit#gid=1394247382")
  


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
               "about_health",
               "about_covid_had",
               "about_covid_know"
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
                     "about_health",
                     "about_covid_had",
                     "about_covid_know"
)

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
  googlesheets4::sheet_append("https://docs.google.com/spreadsheets/d/1JcgeXPfYdqlbCQ_jcV6D6Sj_LCRwMSIB610Vqzmi45I/edit#gid=1394247382", data)
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


loadData <- function() {
  # files <- list.files(file.path(responsesDir), full.names = TRUE)
  # data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  # data <- do.call(rbind, data)
  # data
  data <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1JcgeXPfYdqlbCQ_jcV6D6Sj_LCRwMSIB610Vqzmi45I/edit#gid=1394247382")
  data
}


# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header {background: #ebf1f5; padding: 10px; margin-left:0px; border-top: 6px solid #435e70;  border-bottom: 1px solid #435e70; }
  "

# Functions to transform the responses to visuals
func_visual_1to4 <- function(df_name, variable_name, question_text, var1_rename, var2_rename) {
  df <- tabyl(df_name, variable_name) %>% 
    rename(choice = variable_name) %>% 
    full_join(data.frame(choice = c(1:4)), by = "choice") %>%  # fix for if there is no data
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
    scale_x_discrete(drop=F) +
    geom_text(vjust = -1) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(label = percent) +
    theme_fivethirtyeight() +
    coord_cartesian(ylim = c(0, 1.05)) +
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
    full_join(data.frame(choice = c("in the last month", "in the last quarter", "since April 1st", "since before April 1st")), by = "choice") %>%  # fix for if there is no data
    mutate(bar_color = case_when(choice == df_name %>% filter(timestamp == max(timestamp)) %>% select(variable_name) %>% as.character() ~ "#e88f00",
                                 TRUE ~ "#124d47")) %>% 
    mutate(your_selection = case_when(choice == df_name %>% filter(timestamp == max(timestamp)) %>% select(variable_name) %>% as.character() ~ paste0(percent(percent, accuracy = 1), " (Your pick)"),
                                      TRUE ~ percent(percent, accuracy = 1))) %>% 
    mutate(choice = fct_relevel(choice, "in the last month", "in the last quarter", "since April 1st", "since before April 1st"))
  
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
    scale_x_discrete(drop=F) +
    theme_fivethirtyeight() +
    coord_cartesian(ylim = c(0, 1.05)) +
    labs(title = question_text,
         caption = paste0("(Total Responses: ", sum(df$n, na.rm = T), ")" )
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
    full_join(data.frame(choice = c(1:4)), by = "choice") %>%  # fix for if there is no data
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
    coord_cartesian(ylim = c(0, 1.05)) +
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
    full_join(data.frame(choice = c("in the last month", "in the last quarter", "since April 1st", "since before April 1st")), by = "choice") %>%  # fix for if there is no data
    mutate(bar_color = "#124d47") %>% 
    mutate(your_selection = percent(percent, accuracy = 1)) %>% 
    mutate(choice = fct_relevel(choice, "in the last month", "in the last quarter", "since April 1st", "since before April 1st"))
  
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
    coord_cartesian(ylim = c(0, 1.05)) +
    labs(title = question_text,
         caption = paste0("(Total Responses: ", sum(df$n), ")" )
    ) +
    theme(plot.title = element_text(size=12),
          plot.background = element_rect(fill = "#fdfdfd"),
          panel.grid.major.x = element_blank(),
          panel.background = element_rect(fill = "#fdfdfd"),
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
  ui = fixedPage(
    theme = "style.css",
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    title = "COVID-19 Comfort Study",
    # google font
    tags$link(
      rel = "stylesheet", 
      href="https://fonts.googleapis.com/css2?family=Raleway:wght@200&display=swap"
    ),
    
    
    # Layout

    
    fixedRow(
      column(4,
             div(id = "header",
                 h1("COVID-19 COMFORT STUDY", style = "font-family: 'Raleway', sans-serif; font-size:30px; color:#c98406; margin-top:-8px;"),
                 h4(style = "font-size:12px;", "Please take this short survey how the COVID-19 pandemic has impacted your comfort levels about common activities, your behaviors, and your feelings."),
                 h4(style = "font-size:12px;", "After you submit your answers, you'll how your responses compare to others who have taken the survey!"),
                    h4(style = "font-size:12px;", "Please note that all answers are required!"),
                 br(),
                 h5(style = "font-size:10px; font-style:italic;",
                   "(Have you already taken the survey and just want to see the results?", 
                   actionLink("skip_button", "Click Here"),
                   ")"
                 )
                 
             )
             ),
      column(8,
             div(
               id = "form",
               
               # Section 1: comfort
               div(class = "sectionheader",
                 "Part 1: Are You Comfortable?"
               ),
               div(strong("Please rate your comfort level for the following as a result of the COVID-19 pandemic:")),
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
               
               div(class = "questiontext", "Seeing friends/family (not in your household) INDOORS"),
               div(class = "rowquestion",
                   div(class = "columnlabel", "Very Uncomfortable"),
                   div(class = "column",
                       radioGroupButtons("comfort_friends_in", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                   div(class = "columnlabel", "Very Comfortable")
               ),
               
               div(class = "questiontext", "Seeing friends/family (not in your household) OUTDOORS"),
               div(class = "rowquestion",
                   div(class = "columnlabel", "Very Uncomfortable"),
                   div(class = "column",
                       radioGroupButtons("comfort_friends_out", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                   div(class = "columnlabel", "Very Comfortable")
               ),
               
               div(class = "questiontext", "Attending large events (30+ people) that take place partially or fully INDOORS (weddings, parties, etc.)"),
               div(class = "rowquestion",
                   div(class = "columnlabel", "Very Uncomfortable"),
                   div(class = "column",
                       radioGroupButtons("comfort_event_in", NULL, c(1:4), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                   div(class = "columnlabel", "Very Comfortable")
               ),
               
               div(class = "questiontext", "Attending large events (30+ people) that take place fully OUTDOORS (weddings, parties, etc.)"),
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
               div(class = "sectionheader",
                   "Part 2: What Have You Done?"
               ),
               div(strong("When was the LAST TIME you did any of the following?")),
               
               div(class = "questiontext", "I have done grocery shopping in-store..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_grocery", NULL, c("in the last month", "in the last quarter", "since April 1st", "since before April 1st"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
               div(class = "questiontext", "I have taken a 3+ hour plane ride..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_plane", NULL, c("in the last month", "in the last quarter", "since April 1st", "since before April 1st"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
               div(class = "questiontext", "I have eaten at a restaurant INDOORS..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_restaurant_in", NULL, c("in the last month", "in the last quarter", "since April 1st", "since before April 1st"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
               div(class = "questiontext", "I have eaten at a restaurant OUTDOORS..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_restaurant_out", NULL, c("in the last month", "in the last quarter", "since April 1st", "since before April 1st"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
               div(class = "questiontext", "I have seen family/friends INDOORS..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_friends_in", NULL, c("in the last month", "in the last quarter", "since April 1st", "since before April 1st"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
               div(class = "questiontext", "I have seen family/friends OUTDOORS..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_friends_out", NULL, c("in the last month", "in the last quarter", "since April 1st", "since before April 1st"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
               div(class = "questiontext", "I have attended a large event that is partially or fully INDOORS..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_event_in", NULL, c("in the last month", "in the last quarter", "since April 1st", "since before April 1st"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
               div(class = "questiontext", "I have attended a large event that is partially or fully OUTDOORS..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_event_out", NULL, c("in the last month", "in the last quarter", "since April 1st", "since before April 1st"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
               div(class = "questiontext", "I have received elective medical treatments (dental cleanings, check-ups, etc.)..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_medical", NULL, c("in the last month", "in the last quarter", "since April 1st", "since before April 1st"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
               div(class = "questiontext", "I have received personal services (haircuts, manicures, etc.)..."),
               div(class = "rowquestion",
                   div(class = "column",
                       radioGroupButtons("behavior_personal", NULL, c("in the last month", "in the last quarter", "since April 1st", "since before April 1st"), checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
               
               
               # Section 2: behavior
               div(class = "spacer"),
               div(class = "sectionheader",
                   "Part 3: What's On Your Face?"
               ),
               div("Please describe your face masking wearing habits?"),
               
               div(class = "questiontext", "I wear a face mask any time I am INDOORS (excluding when you're at home)"),
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
               div(class = "sectionheader",
                   "Part 4: How Ya Feelin'?"
               ),
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
              div(class = "sectionheader",
                  "Part 5: Who Are You?"
              ),
               div(strong('A couple of demographics questions. NOTE: This data is not shared publicly.')),
               selectInput("about_age", "How old are you?",
                           c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+", "Prefer Not To Say")),
               selectInput("about_gender", "To which gender identity do you most identify?",
                           c("Female", "Male", "Transgender", "Gender Variant / Non-Conforming", "Prefer Not To Say")),
               radioButtons("about_job", "I am a frontline worker.", c("Yes", "No", "Prefer Not To Say"), inline = TRUE),
              radioButtons("about_health", "I have a health condition that makes me a high-risk of a serious illness from COVID-19.", c("Yes", "No", "Prefer Not To Say"), inline = TRUE),
              radioButtons("about_covid_had", "I have previously been diagnosed with COVID-19.", c("Yes", "No", "Prefer Not To Say"), inline = TRUE),
              radioButtons("about_covid_know", "I know someone who has previously been diagnosed with COVID-19.", c("Yes", "No", "Prefer Not To Say"), inline = TRUE),
              
               # submit

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
             ),
             
             fixedRow(
               column(12,
                      uiOutput("adminPanelContainer")
               )
             ),
             fixedRow(
                 column(12,
                      div(style = "font-size:10px; font-style:italic; padding-bottom:5px; text-align:right;",
                          span("Much of this code was taken from "),
                          a("Dean Attali's Mimic Google Form", href = "https://deanattali.com/2015/06/14/mimicking-google-form-shiny/"),
                          span("project."),
                          span("Created by"),
                          a("Yuri", href = "http://www.twitter.com/yurikim"),
                          "."
                      )
               )
             )
             
             
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
      data <- t(data) %>% 
        as_tibble() %>% 
        mutate_at(vars(comfort_grocery:comfort_personal), as.numeric) %>% 
        mutate_at(vars(mask_indoor:feelings_severity), as.numeric) %>% 
        mutate(timestamp = as.numeric(timestamp))
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
        # merged_data <<- loadData() %>% 
        #   bind_rows()
        
        saveData(formData())
        
        merged_data <<- loadData()
        
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

    
    # render Results
    observeEvent(input$submit, {
      output$adminPanelContainer <- renderUI({
        div(
          id = "form",
          div(class = "sectionheader",
              "Part 1: Are You Comfortable?"
          ),
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
          div(class = "sectionheader",
              "Part 2: What Have You Done?"
          ),
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
          div(class = "sectionheader",
              "Part 3: What's On Your Face?"
          ),
          div("Please describe your face masking wearing habits."),
          plotOutput("visual_mask_indoor", height = 310, width = "100%"),
          plotOutput("visual_mask_outdoor", height = 310, width = "100%"),
          
          div(class = "spacer"),
          div(class = "sectionheader",
              "Part 4: How Ya Feelin'?"
          ),
          div("Please describe your face masking wearing habits."),
          plotOutput("visual_feelings_catch", height = 310, width = "100%"),
          plotOutput("visual_feelings_spread", height = 310, width = "100%"),
          plotOutput("visual_feelings_precaution", height = 310, width = "100%"),
          plotOutput("visual_feelings_severity", height = 310, width = "100%")
        )
      })}
    )
    
    
    observeEvent(input$skip_button, {
      # User-experience stuff
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      # Hide the form and show a thank you message.
      tryCatch({
        # saveData(formData())
        pulled_data <<- loadData()
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
    
    # Only results - no highlights
    observeEvent(input$skip_button, {
      output$adminPanelContainer <- renderUI({
        div(
          id = "form",
          div(class = "sectionheader",
              "Part 1: Are You Comfortable?"
          ),
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
          div(class = "sectionheader",
              "Part 2: What Have You Done?"
          ),
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
          div(class = "sectionheader",
              "Part 3: What's On Your Face?"
          ),
          div("Please describe your face masking wearing habits."),
          plotOutput("nohighlight_visual_mask_indoor", height = 310, width = "100%"),
          plotOutput("nohighlight_visual_mask_outdoor", height = 310, width = "100%"),

          div(class = "spacer"),
          div(class = "sectionheader",
              "Part 4: How Ya Feelin'?"
          ),
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
    output$visual_comfort_grocery <- renderPlot(func_visual_1to4(df_name = merged_data,
                                                                 variable_name = "comfort_grocery",
                                                                 question_text = "Comfort: Grocery shopping in-store",
                                                                 var1_rename =  "1: Very Uncomfortable",
                                                                 var2_rename =  "4: Very Comfortable"), height = 275, width = 500)
    output$visual_comfort_plane <- renderPlot(func_visual_1to4(df_name = merged_data,
                                                               variable_name = "comfort_plane",
                                                               var1_rename =  "1: Very Uncomfortable",
                                                               var2_rename =  "4: Very Comfortable",
                                                               question_text = "Comfort: Take a 3+ hour plane ride"), height = 275, width = 500)
    output$visual_comfort_restaurant_in <- renderPlot(func_visual_1to4(df_name = merged_data,
                                                                       variable_name = "comfort_restaurant_in",
                                                                       var1_rename =  "1: Very Uncomfortable",
                                                                       var2_rename =  "4: Very Comfortable",
                                                                       question_text = "Comfort: Eating at an restaurant INDOORS"), height = 275, width = 500)
    output$visual_comfort_restaurant_out <- renderPlot(func_visual_1to4(df_name = merged_data,
                                                                        variable_name = "comfort_restaurant_out",
                                                                        var1_rename =  "1: Very Uncomfortable",
                                                                        var2_rename =  "4: Very Comfortable",
                                                                        question_text = "Comfort: Eating at an restaurant OUTDOORS"), height = 275, width = 500)
    output$visual_comfort_friends_in <- renderPlot(func_visual_1to4(df_name = merged_data,
                                                                    variable_name = "comfort_friends_in",
                                                                    var1_rename =  "1: Very Uncomfortable",
                                                                    var2_rename =  "4: Very Comfortable",
                                                                    question_text = "Comfort: Seeing friends/family (not in your household) INDOORS"), height = 275, width = 500)
    output$visual_comfort_friends_out <- renderPlot(func_visual_1to4(df_name = merged_data,
                                                                     variable_name = "comfort_friends_out",
                                                                     var1_rename =  "1: Very Uncomfortable",
                                                                     var2_rename =  "4: Very Comfortable",
                                                                     question_text = "Comfort: Seeing friends/family (not in your household) OUTDOORS"), height = 275, width = 500)
    output$visual_comfort_event_in <- renderPlot(func_visual_1to4(df_name = merged_data,
                                                                  variable_name = "comfort_event_in",
                                                                  var1_rename =  "1: Very Uncomfortable",
                                                                  var2_rename =  "4: Very Comfortable",
                                                                  question_text = "Comfort: Attending large events (30+ people) that take place partially\nor fully INDOORS (weddings, parties, etc.)"), height = 275, width = 500)
    output$visual_comfort_event_out <- renderPlot(func_visual_1to4(df_name = merged_data,
                                                                   variable_name = "comfort_event_out",
                                                                   var1_rename =  "1: Very Uncomfortable",
                                                                   var2_rename =  "4: Very Comfortable",
                                                                   question_text = "Comfort: Attending large events (30+ people) that take place fully\nOUTDOORS (weddings, parties, etc.)"), height = 275, width = 500)
    output$visual_comfort_medical <- renderPlot(func_visual_1to4(df_name = merged_data,
                                                                 variable_name = "comfort_medical",
                                                                 var1_rename =  "1: Very Uncomfortable",
                                                                 var2_rename =  "4: Very Comfortable",
                                                                 question_text = "Comfort: Receiving elective medical treatments (dental cleanings, check-ups, etc.)"), height = 275, width = 500)
    output$visual_comfort_personal <- renderPlot(func_visual_1to4(df_name = merged_data,
                                                                  variable_name = "comfort_personal",
                                                                  var1_rename =  "1: Very Uncomfortable",
                                                                  var2_rename =  "4: Very Comfortable",
                                                                  question_text = "Comfort: Receiving personal services (haircuts, manicures, etc.)"), height = 275, width = 500)
    
    # BEHAVIOR
    output$visual_behavior_grocery <- renderPlot(func_visual_behavior(df_name = merged_data,
                                                                      variable_name = "behavior_grocery",
                                                                      question_text = "I have done grocery shopping in-store..."), height = 275, width = 500)
    output$visual_behavior_plane <- renderPlot(func_visual_behavior(df_name = merged_data,
                                                                    variable_name = "behavior_plane",
                                                                    question_text = "I have taken a 3+ hour plane ride..."), height = 275, width = 500)
    output$visual_behavior_restaurant_in <- renderPlot(func_visual_behavior(df_name = merged_data,
                                                                            variable_name = "behavior_restaurant_in",
                                                                            question_text = "I have eaten at a restaurant INDOORS..."), height = 275, width = 500)
    output$visual_behavior_restaurant_out <- renderPlot(func_visual_behavior(df_name = merged_data,
                                                                             variable_name = "behavior_restaurant_out",
                                                                             question_text = "I have eaten at a restaurant OUTDOORS..."), height = 275, width = 500)
    output$visual_behavior_friends_in <- renderPlot(func_visual_behavior(df_name = merged_data,
                                                                         variable_name = "behavior_friends_in",
                                                                         question_text = "I have seen family/friends INDOORS..."), height = 275, width = 500)
    output$visual_behavior_friends_out <- renderPlot(func_visual_behavior(df_name = merged_data,
                                                                          variable_name = "behavior_friends_out",
                                                                          question_text = "I have seen family/friends OUTDOORS..."), height = 275, width = 500)
    output$visual_behavior_event_in <- renderPlot(func_visual_behavior(df_name = merged_data,
                                                                       variable_name = "behavior_event_in",
                                                                       question_text = "I have attended a large event that is partially or fully INDOORS..."), height = 275, width = 500)
    output$visual_behavior_event_out <- renderPlot(func_visual_behavior(df_name = merged_data,
                                                                        variable_name = "behavior_event_out",
                                                                        question_text = "I have attended a large event that is partially or fully OUTDOORS..."), height = 275, width = 500)
    output$visual_behavior_medical <- renderPlot(func_visual_behavior(df_name = merged_data,
                                                                      variable_name = "behavior_medical",
                                                                      question_text = "I have recieved elective medical treatments..."), height = 275, width = 500)
    output$visual_behavior_personal <- renderPlot(func_visual_behavior(df_name = merged_data,
                                                                       variable_name = "behavior_personal",
                                                                       question_text = "I have recieved personal services..."), height = 275, width = 500)
    
    # MASK
    output$visual_mask_indoor <- renderPlot(func_visual_1to4(df_name = merged_data,
                                                             variable_name = "mask_indoor",
                                                             var1_rename =  "1: I Never Wear a Face Mask",
                                                             var2_rename =  "4: I Wear a Face Mask\n100% of the Time",
                                                             question_text = "I wear a face mask any time I am INDOORS"), height = 275, width = 500)
    output$visual_mask_outdoor <- renderPlot(func_visual_1to4(df_name = merged_data,
                                                              variable_name = "mask_outdoor",
                                                              var1_rename =  "1: I Never Wear a Face Mask",
                                                              var2_rename =  "4: I Wear a Face Mask\n100% of the Time",
                                                              question_text = "I wear a face mask any time I am OUTDOORS"), height = 275, width = 500)
    
    
    # FEELING
    output$visual_feelings_catch <- renderPlot(func_visual_1to4(df_name = merged_data,
                                                                variable_name = "feelings_catch",
                                                                var1_rename =  "1: Very Much Disagree",
                                                                var2_rename =  "4: Very Much Agree",
                                                                question_text = "I am worried about catching COVID-19"), height = 275, width = 500)
    output$visual_feelings_spread <- renderPlot(func_visual_1to4(df_name = merged_data,
                                                                 variable_name = "feelings_spread",
                                                                 var1_rename =  "1: Very Much Disagree",
                                                                 var2_rename =  "4: Very Much Agree",
                                                                 question_text = "I am worried about being a spreader of COVID-19"), height = 275, width = 500)
    output$visual_feelings_precaution <- renderPlot(func_visual_1to4(df_name = merged_data,
                                                                     variable_name = "feelings_precaution",
                                                                     var1_rename =  "1: Very Much Disagree",
                                                                     var2_rename =  "4: Very Much Agree",
                                                                     question_text = "I am taking the appropriate amount of precautions around COVID-19"), height = 275, width = 500)
    output$visual_feelings_severity <- renderPlot(func_visual_1to4(df_name = merged_data,
                                                                   variable_name = "feelings_severity",
                                                                   var1_rename =  "1: Very Much Disagree",
                                                                   var2_rename =  "4: Very Much Agree",
                                                                   question_text = "I think COVID-19 needs to be taken seriously"), height = 275, width = 500)
    
    
    
    
    # No highlight visuals reactive -------------------------------------------

    output$nohighlight_visual_comfort_grocery <- renderPlot(func_nohighlight_visual_1to4(df_name = pulled_data,
                                                                                         variable_name = "comfort_grocery",
                                                                                         question_text = "Comfort: Grocery shopping in-store",
                                                                                         var1_rename =  "1: Very Uncomfortable",
                                                                                         var2_rename =  "4: Very Comfortable"), height = 275, width = 500)
    output$nohighlight_visual_comfort_plane <- renderPlot(func_nohighlight_visual_1to4(df_name = pulled_data,
                                                                                       variable_name = "comfort_plane",
                                                                                       var1_rename =  "1: Very Uncomfortable",
                                                                                       var2_rename =  "4: Very Comfortable",
                                                                                       question_text = "Comfort: Take a 3+ hour plane ride"), height = 275, width = 500)
    output$nohighlight_visual_comfort_restaurant_in <- renderPlot(func_nohighlight_visual_1to4(df_name = pulled_data,
                                                                                               variable_name = "comfort_restaurant_in",
                                                                                               var1_rename =  "1: Very Uncomfortable",
                                                                                               var2_rename =  "4: Very Comfortable",
                                                                                               question_text = "Comfort: Eating at an restaurant INDOORS"), height = 275, width = 500)
    output$nohighlight_visual_comfort_restaurant_out <- renderPlot(func_nohighlight_visual_1to4(df_name = pulled_data,
                                                                                                variable_name = "comfort_restaurant_out",
                                                                                                var1_rename =  "1: Very Uncomfortable",
                                                                                                var2_rename =  "4: Very Comfortable",
                                                                                                question_text = "Comfort: Eating at an restaurant OUTDOORS"), height = 275, width = 500)
    output$nohighlight_visual_comfort_friends_in <- renderPlot(func_nohighlight_visual_1to4(df_name = pulled_data,
                                                                                            variable_name = "comfort_friends_in",
                                                                                            var1_rename =  "1: Very Uncomfortable",
                                                                                            var2_rename =  "4: Very Comfortable",
                                                                                            question_text = "Comfort: Seeing friends/family (not in your household) INDOORS"), height = 275, width = 500)
    output$nohighlight_visual_comfort_friends_out <- renderPlot(func_nohighlight_visual_1to4(df_name = pulled_data,
                                                                                             variable_name = "comfort_friends_out",
                                                                                             var1_rename =  "1: Very Uncomfortable",
                                                                                             var2_rename =  "4: Very Comfortable",
                                                                                             question_text = "Comfort: Seeing friends/family (not in your household) OUTDOORS"), height = 275, width = 500)
    output$nohighlight_visual_comfort_event_in <- renderPlot(func_nohighlight_visual_1to4(df_name = pulled_data,
                                                                                          variable_name = "comfort_event_in",
                                                                                          var1_rename =  "1: Very Uncomfortable",
                                                                                          var2_rename =  "4: Very Comfortable",
                                                                                          question_text = "Comfort: Attending large events (30+ people) that take place partially\nor fully INDOORS (weddings, parties, etc.)"), height = 275, width = 500)
    output$nohighlight_visual_comfort_event_out <- renderPlot(func_nohighlight_visual_1to4(df_name = pulled_data,
                                                                                           variable_name = "comfort_event_out",
                                                                                           var1_rename =  "1: Very Uncomfortable",
                                                                                           var2_rename =  "4: Very Comfortable",
                                                                                           question_text = "Comfort: Attending large events (30+ people) that take place fully\nOUTDOORS (weddings, parties, etc.)"), height = 275, width = 500)
    output$nohighlight_visual_comfort_medical <- renderPlot(func_nohighlight_visual_1to4(df_name = pulled_data,
                                                                                         variable_name = "comfort_medical",
                                                                                         var1_rename =  "1: Very Uncomfortable",
                                                                                         var2_rename =  "4: Very Comfortable",
                                                                                         question_text = "Comfort: Receiving elective medical treatments (dental cleanings, check-ups, etc.)"), height = 275, width = 500)
    output$nohighlight_visual_comfort_personal <- renderPlot(func_nohighlight_visual_1to4(df_name = pulled_data,
                                                                                          variable_name = "comfort_personal",
                                                                                          var1_rename =  "1: Very Uncomfortable",
                                                                                          var2_rename =  "4: Very Comfortable",
                                                                                          question_text = "Comfort: Receiving personal services (haircuts, manicures, etc.)"), height = 275, width = 500)

    # BEHAVIOR
    output$nohighlight_visual_behavior_grocery <- renderPlot(func_nohighlight_visual_behavior(df_name = pulled_data,
                                                                                              variable_name = "behavior_grocery",
                                                                                              question_text = "I have done grocery shopping in-store..."), height = 275, width = 500)
    output$nohighlight_visual_behavior_plane <- renderPlot(func_nohighlight_visual_behavior(df_name = pulled_data,
                                                                                            variable_name = "behavior_plane",
                                                                                            question_text = "I have taken a 3+ hour plane ride..."), height = 275, width = 500)
    output$nohighlight_visual_behavior_restaurant_in <- renderPlot(func_nohighlight_visual_behavior(df_name = pulled_data,
                                                                                                    variable_name = "behavior_restaurant_in",
                                                                                                    question_text = "I have eaten at a restaurant INDOORS..."), height = 275, width = 500)
    output$nohighlight_visual_behavior_restaurant_out <- renderPlot(func_nohighlight_visual_behavior(df_name = pulled_data,
                                                                                                     variable_name = "behavior_restaurant_out",
                                                                                                     question_text = "I have eaten at a restaurant OUTDOORS..."), height = 275, width = 500)
    output$nohighlight_visual_behavior_friends_in <- renderPlot(func_nohighlight_visual_behavior(df_name = pulled_data,
                                                                                                 variable_name = "behavior_friends_in",
                                                                                                 question_text = "I have seen family/friends INDOORS..."), height = 275, width = 500)
    output$nohighlight_visual_behavior_friends_out <- renderPlot(func_nohighlight_visual_behavior(df_name = pulled_data,
                                                                                                  variable_name = "behavior_friends_out",
                                                                                                  question_text = "I have seen family/friends OUTDOORS..."), height = 275, width = 500)
    output$nohighlight_visual_behavior_event_in <- renderPlot(func_nohighlight_visual_behavior(df_name = pulled_data,
                                                                                               variable_name = "behavior_event_in",
                                                                                               question_text = "I have attended a large event that is partially or fully INDOORS..."), height = 275, width = 500)
    output$nohighlight_visual_behavior_event_out <- renderPlot(func_nohighlight_visual_behavior(df_name = pulled_data,
                                                                                                variable_name = "behavior_event_out",
                                                                                                question_text = "I have attended a large event that is partially or fully OUTDOORS..."), height = 275, width = 500)
    output$nohighlight_visual_behavior_medical <- renderPlot(func_nohighlight_visual_behavior(df_name = pulled_data,
                                                                                              variable_name = "behavior_medical",
                                                                                              question_text = "I have recieved elective medical treatments..."), height = 275, width = 500)
    output$nohighlight_visual_behavior_personal <- renderPlot(func_nohighlight_visual_behavior(df_name = pulled_data,
                                                                                               variable_name = "behavior_personal",
                                                                                               question_text = "I have recieved personal services..."), height = 275, width = 500)

    # MASK
    output$nohighlight_visual_mask_indoor <- renderPlot(func_nohighlight_visual_1to4(df_name = pulled_data,
                                                                                     variable_name = "mask_indoor",
                                                                                     var1_rename =  "1: I Never Wear a Face Mask",
                                                                                     var2_rename =  "4: I Wear a Face Mask\n100% of the Time",
                                                                                     question_text = "I wear a face mask any time I am INDOORS"), height = 275, width = 500)
    output$nohighlight_visual_mask_outdoor <- renderPlot(func_nohighlight_visual_1to4(df_name = pulled_data,
                                                                                      variable_name = "mask_outdoor",
                                                                                      var1_rename =  "1: I Never Wear a Face Mask",
                                                                                      var2_rename =  "4: I Wear a Face Mask\n100% of the Time",
                                                                                      question_text = "I wear a face mask any time I am OUTDOORS"), height = 275, width = 500)


    # FEELING
    output$nohighlight_visual_feelings_catch <- renderPlot(func_nohighlight_visual_1to4(df_name = pulled_data,
                                                                                        variable_name = "feelings_catch",
                                                                                        var1_rename =  "1: Very Much Disagree",
                                                                                        var2_rename =  "4: Very Much Agree",
                                                                                        question_text = "I am worried about catching COVID-19"), height = 275, width = 500)
    output$nohighlight_visual_feelings_spread <- renderPlot(func_nohighlight_visual_1to4(df_name = pulled_data,
                                                                                         variable_name = "feelings_spread",
                                                                                         var1_rename =  "1: Very Much Disagree",
                                                                                         var2_rename =  "4: Very Much Agree",
                                                                                         question_text = "I am worried about being a spreader of COVID-19"), height = 275, width = 500)
    output$nohighlight_visual_feelings_precaution <- renderPlot(func_nohighlight_visual_1to4(df_name = pulled_data,
                                                                                             variable_name = "feelings_precaution",
                                                                                             var1_rename =  "1: Very Much Disagree",
                                                                                             var2_rename =  "4: Very Much Agree",
                                                                                             question_text = "I am taking the appropriate amount of precautions around COVID-19"), height = 275, width = 500)
    output$nohighlight_visual_feelings_severity <- renderPlot(func_nohighlight_visual_1to4(df_name = pulled_data,
                                                                                           variable_name = "feelings_severity",
                                                                                           var1_rename =  "1: Very Much Disagree",
                                                                                           var2_rename =  "4: Very Much Agree",
                                                                                           question_text = "I think COVID-19 needs to be taken seriously"), height = 275, width = 500)    
    
  }
)