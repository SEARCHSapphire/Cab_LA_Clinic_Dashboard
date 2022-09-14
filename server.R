library(timevis)
library(dplyr)
library(shiny)

sw_version_date = '2022.08.31'

helperfilepath <- file.path("scripts","helper_functions.R")
source(helperfilepath)
df_master <- read.csv("data/master_list.csv")
cabla_data = load_data_from_db()

# Define server logic for random distribution app ----
server <- function(input, output, session) {
  
  # -------------------------------------------
  # SOFTWARE VERSION DATE
  # -------------------------------------------
  sw_version_date <- "2022-09-14"
  output$software_version = renderUI({
    HTML(sprintf('<br> &nbsp &nbsp &nbsp Software Version: <b>%s</b>', sw_version_date))
  })
  
  # ---------------------------------------------------------------------------
  # SOME REACTIVE VALUES
  # ---------------------------------------------------------------------------
  # -------------------------------------------
  # Refresh - Reload the database
  # -------------------------------------------
  cabla <- reactiveValues(data = cabla_data)
  observeEvent(input$refresh_db,{
    cabla$data = load_data_from_db()
  })
  
  
  subjid_select = reactiveVal(NULL)
  encounter_select = reactiveVal(NULL)
  use_searchbox = reactiveValues(use=FALSE)
  
  df_scheduled <- reactive({
    create_schedule(df_enr(), df_enr_cab(), df_non_cab_fu(), df_cab_fu(), df_withdrawal(), df_death(), df_sero(), df_cab_restart())
  })
  
  df_enr <- reactive({
    ret_val <- cabla$data$enrollment
  })
  
  df_base_demo <- reactive({
    ret_val <- cabla$data$baseline
  })
  
  df_enr_cab <- reactive({
    ret_val <- cabla$data$baseline_cab
  })
  
  df_contactinfo <- reactive({
    ret_val <- cabla$data$contact
  })
  
  df_cab_fu <- reactive({
    ret_val <- cabla$data$sched_c_fu
  })
  
  df_cab_fu_selected <- reactive({
    ret_val <- df_cab_fu() %>%
      mutate(vdate = ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), 
                            as.character(as.Date(as.character(vdate),'%m/%d/%y')))) %>%
      filter(subjid == subjid_select() & as.Date(vdate, "%Y-%m-%d") == selected_date())
  })
  
  df_non_cab_fu <- reactive({
    ret_val <- cabla$data$sched_b_fu
  })
  
  df_non_cab_fu_selected <- reactive({
    ret_val <- df_non_cab_fu() %>%
      mutate(vdate = ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), 
                            as.character(as.Date(as.character(vdate),'%m/%d/%y')))) %>%
      filter(subjid == subjid_select() & as.Date(vdate, "%Y-%m-%d") == selected_date())
  })
  
  df_death <- reactive({
    ret_val <- cabla$data$death
  })
  
  df_withdrawal <- reactive({
    ret_val <- cabla$data$withdrawal
  })
  
  df_sero <- reactive({
    ret_val <- cabla$data$seroconversion
  })
  
  df_cab_restart <- reactive({
    ret_val <- cabla$data$cab_reinduction
  })
  
  
  # -------------------------------------------
  # QUICK JUMP DATA SELECTOR: TODAY AND TOMORROW
  # -------------------------------------------
  
  # if today is clicked
  observeEvent(input$today, {
    updateDateInput(session, "date",
                    value = as.Date(Sys.Date())
    )
    selected_cal_date(as.Date(Sys.Date()))
  }) 
  
  # if tomorrow is clicked 
  observeEvent(input$tomorrow, {
    updateDateInput(session, "date",
                    value = as.Date(Sys.Date() + 1)
    )
    selected_cal_date(as.Date(Sys.Date() + 1))
  }) 
  
  
 
  # ---------------------------------------------------------------------------
  # Add Click event on the Calender
  # ---------------------------------------------------------------------------
  
  observeEvent(input$calDate, {
    selected_cal_date(input$calDate)
  })
  
  
  observeEvent(input$lastClick, {
    # if you click on "View Details" options
    if (grepl('view', input$lastClickId)){
      subjid_select(gsub("view_", "", input$lastClickId, fixed = TRUE))
      showModal(patient_details_modal())
    } else if (grepl('encounter', input$lastClickId)){
      encounter_select(gsub("encounter_", "", input$lastClickId, fixed = TRUE))
      showModal(patient_encounter_modal())
    } else if (grepl('contact', input$lastClickId)){
      subjid_select(gsub("contactinfo_", "", input$lastClickId, fixed = TRUE))
      showModal(participant_contactinfo_modal())
    }
  })
  
  
  # ---------------------------------------------------------------------------
  # If Search Buttons are clicked
  # ---------------------------------------------------------------------------
  observeEvent(input$search, {
    use_searchbox$use = TRUE
    subjid_select(toupper(gsub(" ", "", input$ind_subjid)))
    showModal(patient_details_modal())
  })
  
  observeEvent(input$search2, {
    use_searchbox$use = TRUE
    subjid_select(toupper(gsub(" ", "", input$ind_subjid2)))
  })
  
  #-----------------------------------------------------------------------------
  # UI Schedule A
  #-----------------------------------------------------------------------------
  # ---------------------------------------------------------------------------
  # SCHEDULE TABLE UI
  # ---------------------------------------------------------------------------
  # Table 
  output$scheduleA_table_ui = renderUI({
    fluidPage(
      htmlOutput('date_statement'),
      dataTableOutput('schedule_table'),
      
      # Not sure how this work, but allows click interaction with the table
      tags$script("$(document).on('click', '#schedule_table button', function () {
                  Shiny.onInputChange('lastClickId',this.id);
                  Shiny.onInputChange('lastClick', Math.random())
  });"))
  })
  
  
  
  # Output date of visit on top 
  selected_cal_date <- reactiveVal(Sys.Date())
  
  output$date_statement = renderUI({
    HTML(sprintf('<h3> Scheduled Visits on %s</h3>', format(as.Date(selected_cal_date()), format = '%Y-%B-%d')))
  })
  
  
  # -------------------------------------------
  # CALENDAR
  # -------------------------------------------
  
  output$ui_calendar = renderUI({
    fullcalendarOutput('calendar', 
                       height = "450")
  })
  
  
  
  output$calendar = renderFullcalendar({
    df <- df_scheduled()
    createCalendar(df)
  })
  
  output$schedule_table = renderDataTable({
    df <- df_scheduled()
    createInteractiveSchedule(df, selected_cal_date())
  })
  
  
  # ---------------------------------------------------------------------------
  # REPORTS SUMMARY UI
  # ---------------------------------------------------------------------------
  # Table 
  output$rpt_summary_ui = renderUI({
    fluidPage(
      fluidRow(
        box(status ='primary',
            solidHeader = TRUE,
            title='General Enrollment Summary',
            tableOutput('enrollment_table')),
        box(status ='primary',
          solidHeader = TRUE,
          title='CAB-LA Enrollment Summary',
          tableOutput('enrollment_cab_table'))
      ),
      fluidRow(
        box(status ='primary',
            solidHeader = TRUE,
            title='NON-CAB-LA Follow-up Visits Summary Report',
            selectInput('visit_week_noncab', label = 'Follow-up Visit Week:',
                        choices = c('Week 12' = '12', 'Week 24' = '24',  'Week 36' = '36',  'Week 48' = '48')),
            dataTableOutput('fu_noncab_table')),
        box(status ='primary',
            solidHeader = TRUE,
            title='CAB-LA Follow-up Visits Summary',
            selectInput('visit_week_cab', label = 'CAB-LA Follow-up Visit Week:',
                        choices = c('CAB Week 4' = '4', 'CAB Week 8' = '8','CAB Week 16' = '16', 'CAB Week 24' = '24',  
                                    'CAB Week 32' = '32', 'CAB Week 40' = '40',  'CAB Week 48' = '48')),
            dataTableOutput('fu_cab_table'))
      )
      )
  })
  
  # output$title1_statement = renderUI({
  #   HTML(sprintf('<h3> General Enrollment Summary</h3>'))
  # })
  # 
  # output$title2_statement = renderUI({
  #   HTML(sprintf('<br><h3> </h3>'))
  # })
  
  # Enrollment Reports
  output$enrollment_table <- renderTable({
    createEnrollmentSummary(df_enr())
  })
  
  output$enrollment_cab_table <- renderTable({
    createEnrollmentSummaryCab(df_enr_cab())
  })
  
  
  # Follow-up Reports
  output$fu_noncab_table <- renderDataTable({
    createFollowupSummary(df_non_cab_fu(), input$visit_week_noncab)
    
  })
  
  output$fu_cab_table <- renderDataTable({
    createFollowupSummaryCab(df_cab_fu(), input$visit_week_cab)
    
  })
  
  # -------------------------------------------
  # PHONE NUMBER LOOKUP
  # -------------------------------------------
  
  
  output$participant_dir_ui = renderUI({
    fluidPage(
      htmlOutput('title_statement'),
      dataTableOutput('participant_dir_table'),
      
      # Not sure how this work, but allows click interaction with the table
      tags$script("$(document).on('click', '#participant_dir_table button', function () {
                  Shiny.onInputChange('lastClickId',this.id);
                  Shiny.onInputChange('lastClick', Math.random())
  });")
    )
    
  })
  
  output$title_statement = renderUI({
    HTML(sprintf('<h3> Participant Contact Information</h3>'))
  })
  
  output$participant_dir_table <- renderDataTable({
    createPhoneLookup(df_enr(), df_contactinfo())
  })
  
  
  # -------------------------------------------
  # VISIT UI
  # -------------------------------------------
  output$visit_table_ui = renderUI({
    fluidPage(
      fluidRow(htmlOutput('title5_statement'),
               uiOutput('getdetails')),
      fluidRow(uiOutput('participant_visit_table'),
               # Not sure how this work, but allows click interaction with the table
               tags$script("$(document).on('click', '#participant_visit_table button', function () {
                  Shiny.onInputChange('lastClickId',this.id);
                  Shiny.onInputChange('lastClick', Math.random())
  });"))
      
    )
  })
  
  output$getdetails = renderUI({
    fluidPage(textInput('ind_subjid2', 'Find SAPPHIRE ID'),
              actionButton("search2", "Search"))
  })
  
  output$title5_statement = renderUI({
    HTML(sprintf('<h3> Participant Visit Details</h3>'))
  })
  
  output$participant_visit_table <- renderUI({
    fluidPage(
      fluidRow(HTML(sprintf('<hr><h3> Participant Demographics</h3>'))),
      fluidRow(htmlOutput('demodata')),
      fluidRow(HTML(sprintf('<hr><h3> Visit History</h3><hr>'))),
      fluidRow(dataTableOutput('Visits'))
    )
    # fluidRow(tabsetPanel(id = 'tabs', type = 'tabs',
    #                      tabPanel('Demographics',htmlOutput('demodata')),
    #                      tabPanel('Visit Summary', dataTableOutput('Visits'))))
  })
  
  
  #-----------------------------------------------------------------------------
  # Demographic Information
  #-----------------------------------------------------------------------------
  
  output$demodata = renderUI({
    demographics = getPatientHeader(df_enr(), df_base_demo(), df_contactinfo(), subjid_select())
    #HTML(demographics)
    fluidPage(fluidRow(
      column(6, HTML(demographics[[1]])),
      column(6, HTML(demographics[[2]]))
    ))
  })
  
  #-----------------------------------------------------------------------------
  # Visit Summary
  #-----------------------------------------------------------------------------
  df_visit_hist_list <- reactive({
    df_list <- createVisitHistory(subjid_select(), df_enr(), df_base_demo(), df_enr_cab(),  df_non_cab_fu(),df_cab_fu())
  })
  output$Visits <- renderDataTable({
    df_visit_hist_list()$data_table
  })
  
  
  # ---------------------------------------------------------------------------
  # PARTICIPANT CONTACT INFORMATION MODAL STRUCTURE
  # ---------------------------------------------------------------------------
  
  participant_contactinfo_modal <- reactive({
    modalDialog(
      fluidPage(
      htmlOutput('contact_details')),
      footer = tagList(actionButton("refresh_contact",label = "Refresh", icon = icon('refresh')),
                         actionButton("dismiss_contact",label = "Close"),modalButton("Cancel_contact")) , size = 'l')
  })
  
  # ---------------------------------------------------------------------------
  # MODAL LOGIC - Contact Info modal
  # ---------------------------------------------------------------------------
  
  # remove modal if dismiss is clicked 
  observeEvent(input$dismiss_contact,{
    use_searchbox$use = FALSE
    removeModal()
  })
  
  observeEvent(input$Cancel_contact,{
    use_searchbox$use = FALSE
    removeModal()
  })
  
  output$contact_details <- renderUI({
    contact_data <- getContactInformation(df_contactinfo(), subjid_select())
    HTML(contact_data)
  })
  
  
  # ---------------------------------------------------------------------------
  # PARTICIPANT ENCOUNTER/VISIT DETAILS MODAL STRUCTURE
  # ---------------------------------------------------------------------------
  
  patient_encounter_modal = reactive({
    if (length(df_enr()[which(df_enr()$subjid == subjid_select()),]$subjid) == 0){
      modalDialog(fluidPage(
        HTML(sprintf("<h4>SAPPHIRE ID %s Not Found</h4>", subjid_select()))
      ), footer = tagList(actionButton("refresh_modal",label = "Refresh"),
                          actionButton("dismiss_modal",label = "Close")))
    }
    else{
      modalDialog(
        fluidPage(
          uiOutput('patient_header'),
          #htmlOutput('ind_demographics'),
          htmlOutput('encounter_details')
        ), footer = tagList(actionButton("refresh_modal",label = "Refresh", icon = icon('refresh')),
                            actionButton("dismiss_modal",label = "Close"),modalButton("Cancel")) , size = 'l')
    }
  })
  
  # ---------------------------------------------------------------------------
  # MODAL LOGIC - Encounter modal
  # ---------------------------------------------------------------------------
  
  # remove modal if dismiss is clicked 
  observeEvent(input$dismiss_modal,{
    #browser()
    use_searchbox$use = FALSE
    removeModal()
  })
  
  
  # ---------------------------------------------------------------------------
  # PATIENT HEADER AND DEMOGRAPHICS 
  # ---------------------------------------------------------------------------
  
  output$patient_header = renderUI({
    h3(strong(sprintf("Patient: %s", subjid_select())),align="center")
  })
  
  output$ind_demographics = renderUI({
    demographics = getPatientHeader(df_enr(), df_base_demo(), df_contactinfo(), subjid_select())
    #HTML(demographics)
    fluidPage(fluidRow(
      column(6, HTML(demographics[[1]])),
      column(6, HTML(demographics[[2]]))
    ))
  })
  
  
  
  # select date on visit History
  selected_date = reactive({
    as.Date(as.character(df_visit_hist_list()$visit_hist[df_visit_hist_list()$visit_hist$id ==encounter_select(),]$`Visit date`))
  })
  
  
  # ---------------------------------------------------------------------------
  # GENERATES VISIT DETAILS BASED ON DATE SELECTED 
  # ---------------------------------------------------------------------------
  
  output$encounter_details = renderUI({
    row_id = as.integer(encounter_select())
    df_selected_item <- df_visit_hist_list()$visit_hist[df_visit_hist_list()$visit_hist$id == row_id,]
    if (nrow(df_selected_item) == 0) {
      fluidRow(column(12, HTML('')))
    } else if (grepl('Screening',df_selected_item$Encounter)) {
      screening = getScreeningDetails(df_enr(), subjid_select())
      HTML(screening)
    } else if (grepl('Demographics',df_selected_item$Encounter)) {
      base_demo = getDemograhicInfo(df_base_demo(), subjid_select())
      fluidPage(
        fluidRow(HTML(sprintf('<h4> Baseline Demographics CRF (%s)</h4>', df_selected_item$`Visit date`))),
        fluidRow(
        column(6, HTML(base_demo[[1]])),
        column(6, HTML(base_demo[[2]]))
      ))
    } else if (grepl('Baseline CABLA',df_selected_item$Encounter)) {
      base_cab = getCABLA_Baseline(df_enr_cab(), subjid_select())
      fluidPage(
        fluidRow(HTML(sprintf('<h4> CAB-LA Baseline CRF (%s)</h4>', df_selected_item$`Visit date`))),
        fluidRow(
        column(6, HTML(base_cab[[1]])),
        column(6, HTML(base_cab[[2]]))
      ))
    } else if (grepl('CAB visit',df_selected_item$Encounter)) {
      cab_fu = getCABLA_Followup(df_cab_fu_selected())
      fluidPage(
        fluidRow(HTML(sprintf('<h4> CAB-LA Follow-up CRF (%s - %s)</h4>', df_selected_item$Encounter, df_selected_item$`Visit date`))),
        fluidRow(
          column(6, HTML(cab_fu[[1]])),
          column(6, HTML(cab_fu[[2]]))
        ))
    }  else if (grepl('DCP',df_selected_item$Encounter)) {
      non_cab_fu = getFollowup(df_non_cab_fu_selected())
      fluidPage(
        fluidRow(HTML(sprintf('<h4>Follow-up Visit CRF (%s - %s)</h4>', df_selected_item$Encounter, df_selected_item$`Visit date`))),
        fluidRow(
          column(6, HTML(non_cab_fu[[1]])),
          column(6, HTML(non_cab_fu[[2]]))
        ))
    }
  })
  
  # Test timeline on calander
  # create timeline, see helper function below
  data <- data.frame(
    id      = 1:4,
    content = c("Item one"  , "Item two"  ,"Ranged item", "Item four"),
    start   = c("2016-01-10", "2016-01-11", "2016-01-20", "2016-02-14 15:00:00"),
    end     = c(NA          ,           NA, "2016-02-04", NA)
  )
  output$ind_timeline = renderTimevis({
    timevis(data)
  })
  
  # Groups Data to support CAB - LA
  # Data for groups example (this data also gets exported in the package)
  timevisData <- data.frame(
    id = 1:11,
    content = c("Open", "Open",
                "Open", "Open", "Half price entry",
                "Staff meeting", "Open", "Adults only", "Open", "Hot tub closes",
                "Siesta"),
    start = c("2016-05-01 07:30:00", "2016-05-01 14:00:00",
              "2016-05-01 06:00:00", "2016-05-01 14:00:00", "2016-05-01 08:00:00",
              "2016-05-01 08:00:00", "2016-05-01 08:30:00", "2016-05-01 14:00:00",
              "2016-05-01 16:00:00", "2016-05-01 19:30:00",
              "2016-05-01 12:00:00"),
    end   = c("2016-05-01 12:00:00", "2016-05-01 20:00:00",
              "2016-05-01 12:00:00", "2016-05-01 22:00:00", "2016-05-01 10:00:00",
              "2016-05-01 08:30:00", "2016-05-01 12:00:00", "2016-05-01 16:00:00",
              "2016-05-01 20:00:00", NA,
              "2016-05-01 14:00:00"),
    group = c(rep("schA", 2), rep("schB", 3), rep("schC", 5), NA),
    type = c(rep("range", 9), "point", "background")
  )
  timevisData <- read.csv("data/data.csv") 
  timevisData <- timevisData %>%
    mutate(start = as.Date(start, "%d/%m/%Y"))
  timevisDataGroups <- data.frame(
    id = c("schA", "schB", "schC"),
    content = c("Schedule A", "SChedule B", "Schedule C")
  )
  
  output$timelineGroups <- renderTimevis({
    timevis(data = timevisData, groups = timevisDataGroups, options = list(editable = TRUE))
  })
  
}