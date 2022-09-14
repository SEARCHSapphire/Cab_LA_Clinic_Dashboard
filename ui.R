library(shiny)
library(timevis)
library(fullcalendarWidget)
library(shinyWidgets)
library(shinydashboard)
# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("CABLA - Clinic Dashboard"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      dateInput('date', 'Date', value = Sys.Date()),
      HTML('<b>Jump to </b>'),
      actionButton("today", "Today"),
      actionButton("tomorrow", "Tomorrow"),
      HTML("<h3>Visit Calendar</h3>"),
      uiOutput('ui_calendar'),
      HTML("<br>"),
      # materialSwitch('showattended', 'Show attended visits', status = 'primary'),
      hr(),
      # textInput('ind_subjid', 'Find SAPPHIRE ID'),
      # actionButton("search", "Search"),
      actionButton("refresh_db",label = "Refresh", icon('sync')),
      HTML('<br><br>'),
      footer = fluidPage(htmlOutput('software_version'))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Participant Schedule", uiOutput('scheduleA_table_ui')),
                  tabPanel("Summary Reports", uiOutput('rpt_summary_ui')),
                  tabPanel("Contact Directory", uiOutput('participant_dir_ui')),
                  tabPanel("Visit Summary", uiOutput('visit_table_ui')),
                  tabPanel("Missed Visits", timevisOutput("ind_timeline")),
                  # tabPanel("Visit Schedule - CAB", timevisOutput("timelineGroups"))
      )
      
    )
  )
)