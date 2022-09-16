# ------------------------------------------------------------------------------
# Required Libraries
# ------------------------------------------------------------------------------
library(RODBC)
library(dplyr)
library(DT)

# ------------------------------------------------------------------------------
# Load Data
# ------------------------------------------------------------------------------
load_data_from_db <- function() {
  file_path = "C:/CABLA/MSAccessDatabase"
  db_file_path <- paste0(file_path,"/cabla.mdb")
  
  conaccessdb = odbcConnectAccess2007(db_file_path)
  df_ae <- sqlFetch(conaccessdb, "adverseevent", stringsAsFactors=FALSE)
  df_baseline <- sqlFetch(conaccessdb, "baseline", stringsAsFactors=FALSE)
  df_contact <- sqlFetch(conaccessdb, "contact_info", stringsAsFactors=FALSE)
  df_enr <- sqlFetch(conaccessdb, "enrollment", stringsAsFactors=FALSE)
  df_death <- sqlFetch(conaccessdb, "death", stringsAsFactors=FALSE)
  df_lab_local <- sqlFetch(conaccessdb, "labresults", stringsAsFactors=FALSE)
  df_ptsurvey <- sqlFetch(conaccessdb, "participant_mm_surv", stringsAsFactors=FALSE)
  df_prsurvey <- sqlFetch(conaccessdb, "prvdr_mm_survey", stringsAsFactors=FALSE)
  df_hivrisk <- sqlFetch(conaccessdb, "risk", stringsAsFactors=FALSE)
  df_schedb_fu <- sqlFetch(conaccessdb, "schedb_followup", stringsAsFactors=FALSE)
  df_baseline_cab <- sqlFetch(conaccessdb, "schedc_cab_baseline", stringsAsFactors=FALSE)
  df_schedc_fu <- sqlFetch(conaccessdb, "schedc_cab_followup", stringsAsFactors=FALSE)
  df_cab_preg_outcome <- sqlFetch(conaccessdb, "schedc_cab_preg_outcomes", stringsAsFactors=FALSE)
  df_cab_preg_report <- sqlFetch(conaccessdb, "schedc_cab_preg_report", stringsAsFactors=FALSE)
  df_cab_reinduction <- sqlFetch(conaccessdb, "schedc_cab_reinduction", stringsAsFactors=FALSE)
  df_cab_concomitant <- sqlFetch(conaccessdb, "schedc_con_meds", stringsAsFactors=FALSE)
  df_cab_infant_outcome <- sqlFetch(conaccessdb, "schedc_inf_outcomes", stringsAsFactors=FALSE)
  df_cab_liver_event <- sqlFetch(conaccessdb, "schedc_liver_event", stringsAsFactors=FALSE)
  df_seroconversion <- sqlFetch(conaccessdb, "seroconversion", stringsAsFactors=FALSE)
  df_tracking <- sqlFetch(conaccessdb, "tracking", stringsAsFactors=FALSE)
  df_withrawal <- sqlFetch(conaccessdb, "withdrawal", stringsAsFactors=FALSE)
  df_endpoint <- sqlFetch(conaccessdb, "wk24", stringsAsFactors=FALSE)
  
  odbcCloseAll()
  
  to_return = list('adverseenvent' = df_ae,
                   'baseline' = df_baseline,
                   'baseline_cab' = df_baseline_cab,
                   'enrollment' = df_enr,
                   'contact' = df_contact,
                   'participant_survey' = df_ptsurvey,
                   'provider_survey' = df_prsurvey,
                   'clinic_lab' = df_lab_local,
                   'hivrisk' = df_hivrisk,
                   'sched_b_fu'  = df_schedb_fu,
                   'sched_c_fu'  = df_schedc_fu,
                   'pregnancy_outcome' = df_cab_preg_outcome,
                   'pregnancy_report' = df_cab_preg_report,
                   'cab_reinduction' = df_cab_reinduction,
                   'concomitant' = df_cab_concomitant,
                   'infant_outcome' = df_cab_infant_outcome,
                   'liver_event' = df_cab_liver_event,
                   'seroconversion' = df_seroconversion,
                   'tracking'  = df_tracking,
                   'withdrawal' = df_withrawal,
                   'death' = df_death)
  
}

# -----------------------------------------------------------------------------
# CREATE CALENDAR
# -----------------------------------------------------------------------------

createCalendar = function(df_schedule) {
  
  if (nrow(df_schedule) > 0) {
    
    # df_temp = df_schedule[!duplicated(df_schedule[,c('subjid','visit_week')]),]
    df_temp = df_schedule
    df_temp$window_span = as.Date(as.character(df_temp$window_span))
    df <- df_temp %>%
      group_by(window_span) %>%
      summarise(count = n()) %>%
      filter(!grepl("S(at|un)", weekdays(as.Date(window_span), abbr=TRUE)))
    
    df$ctstr = with(df, ifelse(count > 1, sprintf('%s Visits', count), '1 Visit'))
    data_calendar = data.frame(title = df$ctstr,
                               start = df$window_span)
    
    return(fullcalendar(data_calendar, 
                        callbacks = list(
                          dayClick = DT::JS(
                            "function(date, jsEvent, view) {
                            Shiny.setInputValue('calDate', date);
                            $('#calendar').fullCalendar('select', date);
                            }"
                          )
                        )))
  }
  return(fullcalendar())
}

createInteractiveSchedule = function(df_master, inputDate, showattended) {
  
  if(nrow(df_master) > 0) {
    if (showattended == TRUE) {
      df <- df_master %>%
        # mutate(status = "Not Seen") %>%
        filter(as.Date(as.character(window_span)) == inputDate & status != "Already Seen")
    } else {
      df <- df_master %>%
        filter(as.Date(as.character(window_span)) == inputDate)
      
    }
    
  }
  
  # Add column to check if participant's window closes in the next 5 days and highlight in red
  df <- df %>%
    mutate(closes_5d = ifelse((as.Date(window_close) >= Sys.Date() & as.Date(window_close) - Sys.Date() <=5 & status != "Already Seen"), 1,
                              ifelse(as.Date(window_close) < Sys.Date() & status != "Already Seen", 2, 0)))
 
  final_dt = datatable(df,
                       escape=F, selection = 'none',
                       options = 
                         list(columnDefs = list(list(visible=FALSE, targets=7)), # hide the conditional color formatting column
                              language = list(
                                zeroRecords = "No scheduled visits on this day.",
                                search = 'Find in table:')), rownames = FALSE)  %>% formatStyle(
                                  'closes_5d',
                                  target = 'row',
                                  backgroundColor = styleEqual(c(2, 1, 0), c('yellow', 'red', 'white'))
                                )
  
  return(final_dt)
}


#-------------------------------------------------------------------------------
# Schedule A: All - Participants
#-------------------------------------------------------------------------------

generateScheduleA <- function(df_master) {
  df_master <- df_master %>%
    filter(screen_met == 1 & screen_sign == 1)
  
  df = df_master %>%
    mutate(enr_date1 = ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), as.character(as.Date(as.character(vdate),'%m/%d/%y'))),
           enr_date2 = ifelse(grepl("-", starttime), as.character(as.Date(as.character(substr(starttime,1,10)),'%Y-%m-%d')), as.character(as.Date(as.character(substr(starttime,1,8)),'%m/%d/%y'))),
           enr_date = ifelse(enr_date1 != enr_date2, as.character(enr_date2), as.character(enr_date1)),
           visit_week = 'Schedule A/B Week 24',
           window_start = as.Date(enr_date) + (20*7),
           target_date = as.Date(enr_date) + (24*7),
           window_close = as.Date(enr_date) + (28*7)) %>%
    select(subjid, enr_date, window_start,target_date, window_close, visit_week)
  
  df2 = df_master %>%
    mutate(enr_date1 = ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), as.character(as.Date(as.character(vdate),'%m/%d/%y'))),
           enr_date2 = ifelse(grepl("-", starttime), as.character(as.Date(as.character(substr(starttime,1,10)),'%Y-%m-%d')), as.character(as.Date(as.character(substr(starttime,1,8)),'%m/%d/%y'))),
           enr_date = ifelse(enr_date1 != enr_date2, as.character(enr_date2), as.character(enr_date1)),
           visit_week = 'Schedule A/B Week 48',
           window_start = as.Date(enr_date) + (44*7),
           target_date = as.Date(enr_date) + (48*7),
           window_close = as.Date(enr_date) + (52*7)) %>%
    select(subjid, enr_date, window_start, target_date, window_close, visit_week)
  
  df <- rbind(df, df2)
  
  return(df)
}

#-------------------------------------------------------------------------------
# Schedule B: Non - Cab-la users (Intervention)
#-------------------------------------------------------------------------------

generateScheduleB <- function(df_master) {
  df_master <- df_master %>%
    filter(study_arm == 1 & screen_met == 1 & screen_sign == 1) # Limit to intervention arm only
  
  df = df_master %>%
    mutate(enr_date1 = ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), as.character(as.Date(as.character(vdate),'%m/%d/%y'))),
           enr_date2 = ifelse(grepl("-", starttime), as.character(as.Date(as.character(substr(starttime,1,10)),'%Y-%m-%d')), as.character(as.Date(as.character(substr(starttime,1,8)),'%m/%d/%y'))),
           enr_date = ifelse(enr_date1 != enr_date2, as.character(enr_date2), as.character(enr_date1)),
           visit_week = 'Schedule B Week 12',
           window_start = as.Date(enr_date) + (8*7),
           target_date = as.Date(enr_date) + (12*7),
           window_close = as.Date(enr_date) + (16*7)) %>%
    select(subjid, enr_date, window_start, window_close, target_date,
           visit_week)
  
  
  df1 = df_master %>%
    mutate(enr_date1 = ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), as.character(as.Date(as.character(vdate),'%m/%d/%y'))),
           enr_date2 = ifelse(grepl("-", starttime), as.character(as.Date(as.character(substr(starttime,1,10)),'%Y-%m-%d')), as.character(as.Date(as.character(substr(starttime,1,8)),'%m/%d/%y'))),
           enr_date = ifelse(enr_date1 != enr_date2, as.character(enr_date2), as.character(enr_date1)),
           visit_week = 'Schedule B Week 36',
           window_start = as.Date(enr_date) + (32*7),
           target_date = as.Date(enr_date) + (36*7),
           window_close = as.Date(enr_date) + (40*7)) %>%
    select(subjid, enr_date, window_start, window_close, target_date,
           visit_week)
  
  df2 = df_master %>%
    mutate(enr_date1 = ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), as.character(as.Date(as.character(vdate),'%m/%d/%y'))),
           enr_date2 = ifelse(grepl("-", starttime), as.character(as.Date(as.character(substr(starttime,1,10)),'%Y-%m-%d')), as.character(as.Date(as.character(substr(starttime,1,8)),'%m/%d/%y'))),
           enr_date = ifelse(enr_date1 != enr_date2, as.character(enr_date2), as.character(enr_date1)),
           visit_week = 'Schedule B Week 48',
           window_start = as.Date(enr_date) + (44*7),
           target_date = as.Date(enr_date) + (48*7),
           window_close = as.Date(enr_date) + (52*7)) %>%
    select(subjid, enr_date, window_start, window_close, target_date,
           visit_week)
  
  df <- rbind(df, df1, df2)
  
  return(df)
}

#-------------------------------------------------------------------------------
# Schedule C: Cab-la users (Intervention)
#-------------------------------------------------------------------------------

generateScheduleC <- function(df_master) {
  print(paste("n records", nrow(df_master)))
  df_master <- df_master %>%
    filter(screen_cab_met == 1)
  
  df = df_master %>%
    mutate(enr_date1 = ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), as.character(as.Date(as.character(vdate),'%m/%d/%y'))),
           enr_date2 = ifelse(grepl("-", starttime), as.character(as.Date(as.character(substr(starttime,1,10)),'%Y-%m-%d')), as.character(as.Date(as.character(substr(starttime,1,8)),'%m/%d/%y'))),
           enr_date = ifelse(enr_date1 != enr_date2, as.character(enr_date2), as.character(enr_date1)),
           visit_week = 'Schedule C Week 4',
           window_start = as.Date(enr_date) + (3*7),
           target_date = as.Date(enr_date) + (4*7),
           window_close = as.Date(enr_date) + (5*7)) %>%
    select(subjid, enr_date, window_start, window_close, target_date,
           visit_week)
  
  
  df1 = df_master %>%
    mutate(enr_date1 = ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), as.character(as.Date(as.character(vdate),'%m/%d/%y'))),
           enr_date2 = ifelse(grepl("-", starttime), as.character(as.Date(as.character(substr(starttime,1,10)),'%Y-%m-%d')), as.character(as.Date(as.character(substr(starttime,1,8)),'%m/%d/%y'))),
           enr_date = ifelse(enr_date1 != enr_date2, as.character(enr_date2), as.character(enr_date1)),
           visit_week = 'Schedule C Week 8',
           window_start = as.Date(enr_date) + (7*7),
           target_date = as.Date(enr_date) + (8*7),
           window_close = as.Date(enr_date) + (9*7)) %>%
    select(subjid, enr_date, window_start, window_close, target_date,
           visit_week)
  
  
  df2 = df_master %>%
    mutate(enr_date1 = ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), as.character(as.Date(as.character(vdate),'%m/%d/%y'))),
           enr_date2 = ifelse(grepl("-", starttime), as.character(as.Date(as.character(substr(starttime,1,10)),'%Y-%m-%d')), as.character(as.Date(as.character(substr(starttime,1,8)),'%m/%d/%y'))),
           enr_date = ifelse(enr_date1 != enr_date2, as.character(enr_date2), as.character(enr_date1)),
           visit_week = 'Schedule C Week 16',
           window_start = as.Date(enr_date) + (15*7),
           target_date = as.Date(enr_date) + (16*7),
           window_close = as.Date(enr_date) + (17*7)) %>%
    select(subjid, enr_date, window_start, window_close, target_date,
           visit_week)
  
  df3 = df_master %>%
    mutate(enr_date1 = ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), as.character(as.Date(as.character(vdate),'%m/%d/%y'))),
           enr_date2 = ifelse(grepl("-", starttime), as.character(as.Date(as.character(substr(starttime,1,10)),'%Y-%m-%d')), as.character(as.Date(as.character(substr(starttime,1,8)),'%m/%d/%y'))),
           enr_date = ifelse(enr_date1 != enr_date2, as.character(enr_date2), as.character(enr_date1)),
           visit_week = 'Schedule C Week 24',
           window_start = as.Date(enr_date) + (23*7),
           target_date = as.Date(enr_date) + (24*7),
           window_close = as.Date(enr_date) + (25*7)) %>%
    select(subjid, enr_date, window_start, window_close, target_date,
           visit_week)
  
  df4 = df_master %>%
    mutate(enr_date1 = ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), as.character(as.Date(as.character(vdate),'%m/%d/%y'))),
           enr_date2 = ifelse(grepl("-", starttime), as.character(as.Date(as.character(substr(starttime,1,10)),'%Y-%m-%d')), as.character(as.Date(as.character(substr(starttime,1,8)),'%m/%d/%y'))),
           enr_date = ifelse(enr_date1 != enr_date2, as.character(enr_date2), as.character(enr_date1)),
           visit_week = 'Schedule C Week 32',
           window_start = as.Date(enr_date) + (31*7),
           target_date = as.Date(enr_date) + (32*7),
           window_close = as.Date(enr_date) + (33*7)) %>%
    select(subjid, enr_date, window_start, window_close, target_date,
           visit_week)
  
  df5 = df_master %>%
    mutate(enr_date1 = ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), as.character(as.Date(as.character(vdate),'%m/%d/%y'))),
           enr_date2 = ifelse(grepl("-", starttime), as.character(as.Date(as.character(substr(starttime,1,10)),'%Y-%m-%d')), as.character(as.Date(as.character(substr(starttime,1,8)),'%m/%d/%y'))),
           enr_date = ifelse(enr_date1 != enr_date2, as.character(enr_date2), as.character(enr_date1)),
           visit_week = 'Schedule C Week 40',
           window_start = as.Date(enr_date) + (39*7),
           target_date = as.Date(enr_date) + (40*7),
           window_close = as.Date(enr_date) + (41*7)) %>%
    select(subjid, enr_date, window_start, window_close, target_date,
           visit_week)
  
  df6 = df_master %>%
    mutate(enr_date1 = ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), as.character(as.Date(as.character(vdate),'%m/%d/%y'))),
           enr_date2 = ifelse(grepl("-", starttime), as.character(as.Date(as.character(substr(starttime,1,10)),'%Y-%m-%d')), as.character(as.Date(as.character(substr(starttime,1,8)),'%m/%d/%y'))),
           enr_date = ifelse(enr_date1 != enr_date2, as.character(enr_date2), as.character(enr_date1)),
           visit_week = 'Schedule C Week 48',
           window_start = as.Date(enr_date) + (47*7),
           target_date = as.Date(enr_date) + (48*7),
           window_close = as.Date(enr_date) + (49*7)) %>%
    select(subjid, enr_date, window_start, window_close, target_date,
           visit_week)
  
  df <- rbind(df, df1, df2, df3, df4, df5, df6)
  
  return(df)
}

#-------------------------------------------------------------------------------
# Create Schedule Combines: Schedule A; Schedule B and Schedule C
#-------------------------------------------------------------------------------
create_schedule <- function(df_baseline_all, df_baseline_cab, df_non_cab_fu, df_cab_fu, df_wd, df_death, df_sero, df_cab_restart, df_24, df_48) {
  df1 <- generateScheduleA(df_baseline_all)
  df2 <- generateScheduleB(df_baseline_all)
  df3 <- generateScheduleC(df_baseline_cab)
  
  # Get participants who stopped and never restarted
  df_stopped_cab <- df_cab_fu %>%
    mutate(cab_stop_date = ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), as.character(as.Date(as.character(vdate),'%m/%d/%y')))) %>%
    arrange(desc(as.Date(cab_stop_date))) %>%
    distinct(subjid, .keep_all = T) %>%
    filter(stop_cab == 1) %>%
    select(subjid, cab_stop_date)
  
  df_restarted_cab <- df_cab_restart %>%
    mutate(cab_restart_date = ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), as.character(as.Date(as.character(vdate),'%m/%d/%y')))) %>%
    arrange(desc(as.Date(cab_restart_date))) %>%
    distinct(subjid, .keep_all = T) %>%
    select(subjid, cab_restart_date)
  
  df_stopped_cab <- df_stopped_cab %>%
    merge(df_restarted_cab, by="subjid", all.x = T) %>%
    filter(is.na(cab_restart_date) | (as.Date(cab_stop_date) > as.Date(cab_restart_date)))
  
  # Drop Participants who stopped CAB from Schedule C
  if(nrow(df_stopped_cab) > 0) {
    df3 <- df3 %>%
      filter(!(subjid %in% df_stopped_cab$subjid))
  }
  
  # Drop all active participants on CABLA (Schedule C) from the NON CABLA schedule (Schedule B)
  
  if(nrow(df3) > 0) {
    df2 <- df2 %>%
      filter(!(subjid %in% df3$subjid))
  }
  
  # Drop/Remove participants that have been reported as dead or withdrawn consent from study participation
  df4 <- data.frame()
  if(!missing(df_wd)) {
    if(nrow(df_wd) > 0) {
      df4 <- df_wd[, c("subjid", "vdate", "pinitials")]
    }
  }
  
  if(!missing(df_death)) {
    if(nrow(df_death) > 0 ) {
      df5 <- df_death[, c("subjid", "vdate", "pinitials")]
      if(nrow(df4) > 0) {
        df4 <- rbind(df4, df5)
      } else {
        df4 <- df5
      }
      
    }
  }
  
  
  
  # we need some way of ensuring the participant is listed to be on all the dates within his/her window
  # so that they are listed in the calender across all the dates of interest
  
  df <- rbind(df1, df2, df3)

  
  df2 <- df %>%
    mutate(window_span = window_start)
  
  # exclude dead and withdrawn participants if any
  if (nrow(df4)>0) {
    df2 <- df2 %>%
      filter(subjid %in% df4$subjid)
  }
  
  
  n = nrow(df)
  for(i in 1:nrow(df)) {
    ndays = as.Date(df[i,"window_close"]) - as.Date(df[i,"window_start"])
    
    for(x in 1:ndays - 1) {
      
      date2 = (as.Date(df[i,"window_start"] + x))
      
      if(as.Date(date2) <= as.Date(df[i,"window_close"])) {
        n <- n + 1
        df2 <- rbind(df2, df2[i,])
        df2[n,"window_span"] = as.Date(date2)
      
      }
    }
    
  }
  
  
  df2 <- df2[!duplicated(df2[, c('subjid', 'window_span')]),]
  
  
  
  # Update schedule
  if (missing(df_24)) {
    df_24 <- data.frame()
  }
  
  if (missing(df_48)) {
    df_48 <- data.frame()
  }
  
  df_return <- update_visit_status(df2, df_cab_fu, df_non_cab_fu, df_24, df_48)
  
  print(paste("Updated List", nrow(df_return)))
  return(df_return)
  
}


#-------------------------------------------------------------------------------
# Update Participant Visit status on the schedule
#-------------------------------------------------------------------------------
update_visit_status <- function(df_schedule, df_cab_fu, df_non_cab_fu, df_24, df_48) {
  
  df12_nc <- df_non_cab_fu %>%
    filter(study_visit == 12)
  
  df24_nc <- df_non_cab_fu %>%
    filter(study_visit == 24)
  
  df36_nc <- df_non_cab_fu %>%
    filter(study_visit == 36)
  
  df48_nc <- df_non_cab_fu %>%
    filter(study_visit == 48)
  
  df_schedule = df_schedule %>%
    mutate(status = "Not Seen") 
  
  df_schedule$status = with(df_schedule, ifelse(visit_week == 'Schedule B Week 12' & subjid %in% df12_nc$subjid, "Already Seen", status))
  df_schedule$status = with(df_schedule, ifelse(visit_week == 'Schedule B Week 24' & subjid %in% df24_nc$subjid, "Already Seen", status))
  df_schedule$status = with(df_schedule, ifelse(visit_week == 'Schedule B Week 36' & subjid %in% df36_nc$subjid, "Already Seen", status))
  df_schedule$status = with(df_schedule, ifelse(visit_week == 'Schedule B Week 48' & subjid %in% df48_nc$subjid, "Already Seen", status))
  
  
  df4_ca <- df_cab_fu %>%
    filter(cab_vweek == 4)
  
  df8_ca <- df_cab_fu %>%
    filter(cab_vweek == 8)
  
  df16_ca <- df_cab_fu %>%
    filter(cab_vweek == 16)
  
  df24_ca <- df_cab_fu %>%
    filter(cab_vweek == 24)
  
  df32_ca <- df_cab_fu %>%
    filter(cab_vweek == 32)
  
  df40_ca <- df_cab_fu %>%
    filter(cab_vweek == 40)
  
  df48_ca <- df_cab_fu %>%
    filter(cab_vweek == 48)
  
  df_schedule$status = with(df_schedule, ifelse(visit_week == 'Schedule C Week 4' & subjid %in% df4_ca$subjid, "Already Seen", status))
  df_schedule$status = with(df_schedule, ifelse(visit_week == 'Schedule C Week 8' & subjid %in% df8_ca$subjid, "Already Seen", status))
  df_schedule$status = with(df_schedule, ifelse(visit_week == 'Schedule C Week 16' & subjid %in% df16_ca$subjid, "Already Seen", status))
  df_schedule$status = with(df_schedule, ifelse(visit_week == 'Schedule C Week 24' & subjid %in% df24_ca$subjid, "Already Seen", status))
  df_schedule$status = with(df_schedule, ifelse(visit_week == 'Schedule C Week 32' & subjid %in% df32_ca$subjid, "Already Seen", status))
  df_schedule$status = with(df_schedule, ifelse(visit_week == 'Schedule C Week 40' & subjid %in% df40_ca$subjid, "Already Seen", status))
  df_schedule$status = with(df_schedule, ifelse(visit_week == 'Schedule C Week 48' & subjid %in% df48_ca$subjid, "Already Seen", status))
  
  df_schedule$status = with(df_schedule, ifelse(visit_week == 'Schedule A/B Week 24' & subjid %in% df_24$subjid, "Already Seen", status))
  df_schedule$status = with(df_schedule, ifelse(visit_week == 'Schedule A/B Week 48' & subjid %in% df_48$subjid, "Already Seen", status))
  
  df_schedule$status = with(df_schedule, ifelse(status == "Not Seen" & as.Date(window_close) < Sys.Date(), "Not Seen, Missed Visit", status))
  
  return(df_schedule)
  
}

#-------------------------------------------------------------------------------
# Get Participant Contact information
#-------------------------------------------------------------------------------
getContactInformation <- function(df, selectedSubjid) {
  return_string = sprintf('No Contact Information')
  df <- df %>%
    filter(subjid == selectedSubjid)
  
  if(nrow(df)>0) {
    pname = with(df, pname)
    village = with(df, village)
    how_find_house = with(df, how_find_house)
    landmarks = with(df, landmarks)
    primary_phone = with(df, primary_phone)
    secondary_phone = with(df, ifelse(secondary_phone %in% c('-6','-9'), "Not Provided", secondary_phone))
    place_work = with(df, place_work)
    preferred_call_time = with(df, case_when(preferred_call_time == 1 ~"Morning",
                                             preferred_call_time == 2 ~ 'Afternoon',
                                             preferred_call_time == 3 ~ 'Evening',
                                             preferred_call_time == 4 ~ paste("Other -",preferred_call_time_other),
                                             TRUE ~ "N/A"))
     
    contact1 = with(df, contact1)
    contact1_phone = with(df, ifelse(contact1_phone %in% c('-6','-9'), "Not Provided", contact1_phone))
    contact2 = with(df, ifelse(contact2 %in% c('-6','-9'), "Not Provided", contact2))
    contact2_phone = with(df, ifelse(contact2_phone %in% c('-6','-9'), "N/a", contact2_phone))
    excuse_others_home = with(df, case_when(excuse_others_home == 1 ~ "Yes",
                                            excuse_others_home == 0 ~ "No",
                                            TRUE ~ "N/A"))
    comments = with(df, ifelse(comments %in% c('-6','-9'), "N/a", comments))
    
    
    return_string = sprintf('<h3> Participant Contact Information Crf</h3><hr>
                            <b>Participant Full Names</b>:%s<br>
                            <b>Village of Residence</b>:%s<br>
                            <b>Provide a detailed description of how to find household</b>:%s<br>
                            <b>Describe nearby landmarks to participant’s home</b>:%s<br>
                            <b>Primary phone number</b>:%s<br>
                            <b>Secondary phone Number</b>:%s<br>
                            <b>Place of Work</b>:%s<br>
                            <b>Preferred times to call</b>:%s<br>
                            <b>Additional Contact Person #1 (Name and Relationship)</b>:%s<br>
                            <b>Additional Contact Person #1 phone</b>:%s<br>
                            <b>Additional Contact Person #2 (Name and Relationship)</b>:%s<br>
                            <b>Additional Contact Person #2 phone</b>:%s<br>
                            <b>In the event of home visit, should study staff excuse themselves if others are home?</b>:%s<br>
                            <b>Additional comments</b>:%s<br>',
                            pname,village,how_find_house,landmarks,primary_phone,
                            secondary_phone,place_work,preferred_call_time, contact1,
                            contact1_phone,contact2,contact2_phone,excuse_others_home,comments
    )
  }
  
  
  return(return_string)
}

#-------------------------------------------------------------------------------
# Create Participant Contact information Look up table
#-------------------------------------------------------------------------------

createPhoneLookup = function(df_enr, df_contact){
  
  df_return <- df_enr %>%
    filter(screen_met == 1 & screen_sign == 1) %>%
    select(vdate, subjid, study_arm) %>%
    mutate(arm = ifelse(study_arm == 1, "Int", "Con")) %>%
    merge(df_contact, by="subjid", all.x = T) %>%
    mutate(`Participant Name` = pname) %>%
    distinct(subjid, .keep_all = T) %>%
    select(subjid, `Participant Name`, arm, village, primary_phone, secondary_phone)
  
  # add the View Patient Details button if there are rows to show
  df_return[["Details"]] = 
    paste0('
               <div class="btn-group" role="group" aria-label="Basic example">
               <button type="button" class="btn btn-secondary delete" id=contactinfo_',df_return$subjid,'>View Contact</button>
               </div>
               
               ')
  
  
  final_dt = datatable(df_return,
                       escape=F, selection = 'none',
                       options = 
                         list(#columnDefs = list(list(visible=FALSE, targets=c(4))), 
                           language = list(
                             zeroRecords = "No Contact Data Available",
                             search = 'Find in table:')), rownames = FALSE)
  
  return(final_dt)
}


#-------------------------------------------------------------------------------
# Enrollment Summary - General
#-------------------------------------------------------------------------------
createEnrollmentSummary <- function(df) {
  df_return <- df %>%
    mutate(arm = ifelse(study_arm == 1, "Int", "Con")) %>%
    group_by(arm) %>%
    summarise(`# Screened` = n(),
              `# Enrolled` = sum(screen_met == 1 & screen_sign == 1, na.rm = T),
              `% Enrolled` = sprintf("%.1f%%", `# Enrolled`/`# Screened` * 100)
              )
  return(df_return)
}


#-------------------------------------------------------------------------------
# Follow-up Summary - General
#-------------------------------------------------------------------------------
createFollowupSummary <- function(df, selectedvweek) {
  df_return <- df %>%
    filter(study_visit == selectedvweek) %>%
    mutate(arm = ifelse(study_arm == 1, "Int", "Con")) %>%
    group_by(arm) %>%
    summarise(`# Seen` = n(),
              `# PrEP` = sprintf("%d (%.1f%%)",sum(dcp_choice == 1, na.rm = T), sum(dcp_choice == 1, na.rm = T)/n()*100),
              `# PEP` = sprintf("%d (%.1f%%)",sum(dcp_choice == 2, na.rm = T), sum(dcp_choice == 2, na.rm = T)/n()*100),
              `# Rapid-Test` = sprintf("%d (%.1f%%)",sum(testchoice == 2, na.rm = T), sum(testchoice == 2, na.rm = T)/n()*100),
              `# Self-Test` = sprintf("%d (%.1f%%)",sum(testchoice == 1, na.rm = T), sum(testchoice == 1, na.rm = T)/n()*100),
              `# Offered CAB-LA` = sprintf("%d (%.1f%%)",sum(cab_offered == 1, na.rm = T), sum(cab_offered == 1, na.rm = T)/n()*100),
              `# HIV-Positive` = sprintf("%d (%.1f%%)",sum(hivtest_result == 1, na.rm = T), sum(hivtest_result == 1, na.rm = T)/n()*100),
    )
  if(nrow(df_return) > 0) {
    print(str(as.data.frame(df_return)))
    df_summary <- t(df_return)
   row.names(df_summary) <- c("Arm", '# Seen','# PrEP','# PEP','# Rapid-Test',
                              '# Self-Test','# Offered CAB-LA','# HIV-Positive')
    print(str(df_summary))
  } else {
    df_summary <- data.frame()
  }
  
  return(datatable(df_summary, options = list(dom = 't'))) #Display the table only
}


#-------------------------------------------------------------------------------
# Enrollment Summary - CABLA
#-------------------------------------------------------------------------------
createEnrollmentSummaryCab <- function(df, df_wd, df_death, df_sero) {
  df_return <- df %>%
    mutate(arm = ifelse(study_arm == 1, "Int", "Con")) %>%
    group_by(arm) %>%
    summarise(`# Screened` = n(),
              `# Enrolled` = sum(int_enrolled == 1, na.rm = T),
              `% Enrolled` = sprintf("%.1f%%", `# Enrolled`/`# Screened` * 100)
    )
  return(df_return)
}


#-------------------------------------------------------------------------------
# Follow-up Summary - Cab-LA
#-------------------------------------------------------------------------------
createFollowupSummaryCab <- function(df, selectedvweek) {
  df_return <- df %>%
    filter(cab_vweek == selectedvweek) %>%
    mutate(arm = ifelse(study_arm == 1, "Int", "Con")) %>%
    group_by(arm) %>%
    summarise(`# Seen` = n(),
              `# stopped Cab` = sprintf("%d (%.1f%%)",sum(stop_cab == 1, na.rm = T), sum(stop_cab == 1, na.rm = T)/n()*100),
              `# HIV RNA Collected` = sprintf("%d (%.1f%%)",sum(specimen_collected == 1, na.rm = T), sum(specimen_collected == 1, na.rm = T)/n()*100),
              `# Plasma Collected` = sprintf("%d (%.1f%%)",sum(plasma_collected == 1, na.rm = T), sum(plasma_collected == 1, na.rm = T)/n()*100),
              `# Hair Collected` = sprintf("%d (%.1f%%)",sum(hair_collected == 1, na.rm = T), sum(hair_collected == 1, na.rm = T)/n()*100),
              `# Offered CAB-LA` = sprintf("%d (%.1f%%)",sum(offered_cab == 1, na.rm = T), sum(offered_cab == 1, na.rm = T)/n()*100),
              `# Pregnant` = sprintf("%d (%.1f%%)",sum(pregnancy_test_result == 1, na.rm = T), sum(pregnancy_test_result == 1, na.rm = T)/n()*100),
              `# HIV-Positive` = sprintf("%d (%.1f%%)",sum(hivtest_result == 1, na.rm = T), sum(hivtest_result == 1, na.rm = T)/n()*100),
              `# Completed CAB Survey` = sprintf("%d (%.1f%%)",sum(cab_survey_completed == 1, na.rm = T), sum(cab_survey_completed == 1, na.rm = T)/n()*100)
    )
  if(nrow(df_return) > 0) {
    print(str(as.data.frame(df_return)))
    df_summary <- t(df_return)
    row.names(df_summary) <- c("Arm", '# Seen','# stopped Cab','# HIV RNA Collected','# Plasma Collected',
                               '# Hair Collected','# Offered CAB-LA', '# Pregnant', '# HIV-Positive',
                               '# Completed CAB Survey')
    print(str(df_summary))
  } else {
    df_summary <- data.frame()
  }
  
  return(datatable(df_summary, options = list(dom = 't'))) #Display the table only
}


#-------------------------------------------------------------------------------
# Participants Visit/Encounter Summary
#-------------------------------------------------------------------------------
createVisitHistory <- function(selectedSubjid, df_scr, df_base, df_base_cab, df_fu, df_fu_cab) {
  df = data.frame()
  
  if(!missing(df_scr) & !is.null(selectedSubjid)) {
    df1 = df_scr %>%
      filter(subjid == selectedSubjid) %>%
      mutate(Encounter = 'Screening and Enrollment',
             `Visit date` = as.character(as.Date(as.character(vdate),'%Y-%m-%d'))) %>%
      select(subjid, `Visit date`, Encounter)
    df = df1
  } 
  
  if(!missing(df_base) & !is.null(selectedSubjid)) {
    df2 = df_base %>%
      filter(subjid  == selectedSubjid) %>%
      mutate(Encounter = 'Baseline Demographics',
             `Visit date` = as.character(as.Date(as.character(vdate),'%Y-%m-%d'))) %>%
      select(subjid, `Visit date`, Encounter)
  } else {
    df2 = data.frame()
  }
  
  if(!missing(df_base_cab) & !is.null(selectedSubjid)) {
    df3 = df_base_cab %>%
      filter(subjid  == selectedSubjid) %>%
      mutate(Encounter = 'Baseline CABLA',
             `Visit date` = as.character(as.Date(as.character(vdate),'%Y-%m-%d'))) %>%
      select(subjid, `Visit date`, Encounter)
  } else {
    df3 = data.frame()
  }
  
  if(!missing(df_fu) & !is.null(selectedSubjid)) {
    df4 = df_fu %>%
      filter(subjid  == selectedSubjid) %>%
      mutate(Encounter = case_when(study_visit == 12 ~ 'DCP visit week 12',
                                   study_visit == 24 ~ 'DCP visit week 24',
                                   study_visit == 36 ~ 'DCP visit week 36',
                                   study_visit == 48 ~ 'DCP visit week 48',
                                   study_visit == 99 ~ 'Unscheduled - Schedule B'),
             `Visit date` = as.character(as.Date(as.character(vdate),'%Y-%m-%d'))) %>%
      select(subjid, `Visit date`, Encounter)
  } else {
    df4 = data.frame()
  }
  
  if(!missing(df_fu_cab) & !is.null(selectedSubjid)) {
    df5 = df_fu_cab %>%
      filter(subjid == selectedSubjid) %>%
      mutate(Encounter = case_when(cab_vweek == 4 ~ 'CAB visit week 4',
                                   cab_vweek == 8 ~ 'CAB visit week 8',
                                   cab_vweek == 16 ~ 'CAB visit week 16',
                                   cab_vweek == 24 ~ 'CAB visit week 24',
                                   cab_vweek == 32 ~ 'CAB visit week 32',
                                   cab_vweek == 40 ~ 'CAB visit week 40',
                                   cab_vweek == 48 ~ 'CAB visit week 48',
                                   cab_vweek == 99 ~ 'Not Starting - Schedule C'),
             `Visit date` = as.character(as.Date(as.character(vdate),'%Y-%m-%d'))) %>%
      select(subjid, `Visit date`, Encounter)
  } else {
    df5 = data.frame()
  }
  
  if(nrow(df) > 0 & nrow(df2) > 0) {
    df = rbind(df, df2)
  }
  
  if(nrow(df) > 0 & nrow(df3) > 0) {
    df = rbind(df, df3)
  }
  
  if(nrow(df) > 0 & nrow(df4) > 0) {
    df = rbind(df, df4)
  }
  
  if(nrow(df) > 0 & nrow(df5) > 0) {
    df = rbind(df, df5)
  }
  
  
  if (nrow(df) > 0){
    
    df$id = seq(1: nrow(df))
    
    # add the View Patient Details button if there are rows to show
    df[["Details"]] = 
      paste0('
               <div class="btn-group" role="group" aria-label="Basic example">
               <button type="button" class="btn btn-secondary delete" id=encounter_',df$id,'>View Details</button>
               </div>
               
               ')
    df <- df %>%
      select(id, subjid, `Visit date`, Encounter, Details)
  }
  
  final_dt = datatable(df,
                       escape=F, selection = 'none',
                       options = 
                         list(#columnDefs = list(list(visible=FALSE, targets=c(4))), 
                           language = list(
                             zeroRecords = "No visit History for this participant.",
                             search = 'Find in table:')), rownames = FALSE)
  
  
  return_list <- list("data_table" = final_dt,
       "visit_hist" = df)
  
  return(return_list)
  
}


#-------------------------------------------------------------------------------
# get Participant's Baseline Demographic Data
#-------------------------------------------------------------------------------

getDemograhicInfo <- function(df_demo, selectedsubjid) {
  df <- data.frame()
  
  if(!is.null(selectedsubjid)) {
    df <- df_demo %>%
      filter(subjid == selectedsubjid)
  }
   
  
  if(nrow(df) > 0) {
    initials <- with(df, pinitials)
    study_arm <- with(df, ifelse(study_arm == 1, "Intervention", "Control"))
    hivtest_date <- with(df, hivtest_date)
    hivtest  <- with(df, ifelse(hivtest == 1, "Yes", "No"))
    sex  <- with(df, ifelse(sex == 1, "Male", "Female"))
    
    marital <- with(df, case_when(marital == 1 ~ "Unmarried",
                                  marital == 2 ~ "Married - monogamous",
                                  marital == 3 ~ "Divorced/separated",
                                  marital == 4 ~ "Widowed",
                                  marital == 5 ~ 'Cohabiting',
                                  marital == 6 ~ "Married polygamous"))
    
    hivtest_result <- with(df, case_when(hivtest_result == 0 ~ "HIV Negative",
                                         hivtest_result == 1 ~ "HIV Positive",
                                         hivtest_result == 2 ~ "Indeterminate"))
    
    occupation <- with(df, case_when(occupation == 1 ~ 'Farmer',
                                     occupation == 2 ~ 'Fishing/Fishmonger',
                                     occupation == 4 ~ 'Bar owner/Bar worker',
                                     occupation == 6 ~ 'Hotel/Restaurant worker',
                                     occupation == 7 ~ 'Tourism',
                                     occupation == 8 ~ 'Teacher',
                                     occupation == 9 ~ 'Student',
                                     occupation == 10 ~ 'Government worker',
                                     occupation == 11 ~ 'Military',
                                     occupation == 12 ~ 'Housewife',
                                     occupation == 3 ~ 'Shopkeeper/Market vendor',
                                     occupation == 5 ~ 'Transport drivers',
                                     occupation == 13 ~ 'Household worker',
                                     occupation == 14 ~ 'Healthcare worker',
                                     occupation == 15 ~ 'Construction worker',
                                     occupation == 16 ~ 'Factory worker',
                                     occupation == 17 ~ 'Mining',
                                     occupation == 19 ~ 'No job',
                                     occupation == 20 ~ 'Clerical',
                                     occupation == 21 ~ 'Manual Labor',
                                     occupation == 22 ~ 'Clergy',
                                     occupation == 23 ~ paste("Other (",occupation_other,")")))
    
    education <- with(df, case_when(education == 1 ~ 'None',
                                    education == 2 ~ 'Primary 1-4',
                                    education == 3 ~ 'Primary 5-8',
                                    education == 4 ~ 'Completed primary 8',
                                    education == 5 ~ 'Secondary form 1-2',
                                    education == 6 ~ 'Secondary form 3-4',
                                    education == 7 ~ 'Completed secondary form 4',
                                    education == 8 ~ "Beyond/higher secodary form 4"))
    
    mobility_2w <- with(df, case_when(mobility_2w == 1 ~ "Yes",
                                      mobility_2w == 0 ~ "No"))
    
    circumcision <- with(df, case_when(circumcision == 0 ~ "No",
                                       circumcision == 1 ~ 'Yes, traditional/ritual circumcision',
                                       circumcision == 2 ~ "Yes, medical circumcision",
                                       TRUE ~ "N/A"))
    
    pregnant <- with(df, case_when(pregnant == 1 ~ "Yes",
                                   pregnant == 0 ~ "No",
                                   TRUE ~ "N/A"))
    
    alcohol <- df$alcohol
    
    riskuse_complete <- with(df, case_when(riskuse_complete == 1 ~ "Yes",
                                           riskuse_complete == 0 ~ "No"))
    
    partner_concern <- with(df, case_when(partner_concern == 1 ~ "Yes",
                                          partner_concern == 0 ~ "No"))
    
    height <- df$height
    weight <- df$weight
    
    
    return_string = sprintf("<h3> Baseline Demographic Data </h3> <hr>
                            <b>Study Arm</b>:%s<br>
                            <b>Participant Initials</b>:%s<br>
                            <b>Participant Sex</b>:%s<br>
                            <b>HIV Rapid test done</b>:%s<br>
                            <b>HIV Rapid test result</b>:%s<br>
                            <b>HIV Rapid test date</b>:%s<br>
                            <b>Participant Marital Status</b>:%s<br>
                            <b>Participant Occupations</b>:%s<br>
                            <b>Participant Highest Level of Educations</b>:%s<br>
                            <b>Has participant consecutively been away for 2 weeks at least twice in the last 12 months?s</b>:%s<br>
                            <b>Is the participant circumcised? </b>:%s<br>
                            <b>Does the participant currently report being pregnant? </b>:%s<br>
                            <b>Average number of units of alcohol consumed per week units per week</b>:%s<br>
                            <b>HIV Risk and PrEP or PEP Use Assessment completed? </b>:%s<br>
                            <b>Does the participant have concerns about their partner knowing they are taking PrEP/PEP/CAB-LA? </b>:%s<br>
                            <b>Participant Height (in centimeters) </b>:%s cm<br>
                            <b>Participant Weight (in kilograms) </b>:%s Kg<br>", 
                            study_arm, initials, sex, hivtest,hivtest_result,hivtest_date, marital,
                            occupation, education, mobility_2w, circumcision, pregnant, alcohol,
                            riskuse_complete, partner_concern, height, weight)
    
    if (df$study_arm == 1) {
      sero_symptoms <- with(df, case_when(sero_symptoms == 1 ~ "Yes",
                                          sero_symptoms == 0 ~ "No",
                                          TRUE ~ "N/A"))
      
      barriers_assessed <- with(df, case_when(barriers_assessed == 1 ~ "Yes",
                                              barriers_assessed == 0 ~ "No",
                                              TRUE ~ "N/A"))
      
      cabla_offer <- with(df, case_when(cabla_offer == 1 ~ "Yes",
                                        cabla_offer == 0 ~ "No",
                                        TRUE ~ "N/A"))
      
      dcp_choice <- with(df, case_when(dcp_choice == 1 ~ "Daily oral PrEP",
                                       dcp_choice == 2 ~ "PEP",
                                       dcp_choice == 3 ~ "CAB-LA",
                                       dcp_choice == 4 ~ "None",
                                       TRUE ~ "N/A"))
      
      sti_screen <- with(df, case_when(sti_screen == 1 ~ "Yes",
                                       sti_screen == 0 ~ "No",
                                       TRUE ~ "N/A"))
      
      pregnancy_test <- with(df, case_when(pregnancy_test == 1 ~ "Yes",
                                           pregnancy_test == 0 ~ "No",
                                           TRUE ~ "N/A"))
      
      pregnancy_test_result <- with(df, case_when(pregnancy_test_result == 1 ~ "Positive",
                                                  pregnancy_test_result == 0 ~ "Negative",
                                                  TRUE ~ "N/A"))
      
      vislocation <- with(df, case_when(vislocation == 1 ~ "ANC",
                                        vislocation == 2 ~ "OPD",
                                        vislocation == 3 ~ "Home",
                                        vislocation == 4 ~ "Bar",
                                        vislocation == 5 ~ "Hotel/Restaurant",
                                        vislocation == 6 ~ "Guest House",
                                        vislocation == 7 ~ "Trading Center",
                                        vislocation == 8 ~ "Phone",
                                        TRUE ~ "N/A"))
      
      hark_humiliate <- with(df, case_when(hark_humiliate == 1 ~ "Yes",
                                           hark_humiliate == 0 ~ "No",
                                           TRUE ~ "N/A"))
      
      hark_afraid <- with(df, case_when(hark_afraid == 1 ~ "Yes",
                                        hark_afraid == 0 ~ "No",
                                        TRUE ~ "N/A"))
      
      hark_forced <- with(df, case_when(hark_forced == 1 ~ "Yes",
                                        hark_forced == 0 ~ "No",
                                        TRUE ~ "N/A"))
      
      hark_hurt <- with(df, case_when(hark_hurt == 1 ~ "Yes",
                                      hark_hurt == 0 ~ "No",
                                      TRUE ~ "N/A"))
      
      
      return_string2 <- sprintf("<h3>Intervention Only</h3><hr>
                                <b>Participant has symptoms of HIV Seroconversion </b>:%s<br>
                                <b>Was the structural assessment of barriers completed? </b>:%s<br>
                                <b>Was the participant offered CAB-LA at this visit? </b>:%s<br>
                                <b>Participant choice for biomedical prevention option </b>:%s<br>
                                <b>Intervention and NOT chosing CAB-LA: Does the participant have symptoms of an STI? </b>:%s<br>
                                <b>Intervention and NOT chosing CAB-LA: Was an optional pregnancy test conducted? </b>:%s<br>
                                <b>If conducted, result </b>:%s<br>
                                <b>Intervention and NOT chosing CAB-LA: Participant choice for study visit location </b>:%s
                                <h4>HARK screen for gender-based violence</h4><hr>
                                <b>(H) Within the last year, have you been humiliated or emotionally abused in other ways by your partner or your ex-partner? </b>:%s<br>
                                <b>(A) Within the last year, have you been afraid of your partner or ex-partner? </b>:%s<br>
                                <b>(R) Within the last year, have you been forced to have any kind of sexual activity by your partner or ex-partner? </b>:%s<br>
                                <b>(K) Within the last year, have you been kicked, hit, slapped or otherwise physically hurt by your partner or ex-partner? </b>:%s<br>",
                                sero_symptoms, barriers_assessed, cabla_offer, dcp_choice, sti_screen, pregnancy_test,
                                pregnancy_test_result, vislocation, hark_humiliate, hark_afraid, hark_forced, hark_hurt)
      
      
    } else {
      return_string2 <- sprintf("")
    }
    
  } else {
    return_string2 <- sprintf("")
    if(!is.null(selectedsubjid)) {
      return_string = sprintf("<h3> Participant Not found </h3>")
    } else {
      return_string = sprintf("<h3> Participant Not found </h3>")
    }
    
  }
  
  return(c(return_string, return_string2))
}



#-------------------------------------------------------------------------------
# get Participant's Baseline Demographic Data - for the Header Page
#-------------------------------------------------------------------------------
getPatientHeader <- function(df_enr, df_baseline, df_contact, inputId) {
  
  if(!is.null(inputId)) {
    df <- df_contact %>%
      filter(subjid == inputId)
    
    if(nrow(df)> 0) {
      pname <- df$pname
      phone <- with(df, ifelse(primary_phone == "-6" & secondary_phone == "-6", "No Phone Contact",
                               ifelse(primary_phone != "-6" & secondary_phone != "-6",paste0(primary_phone,"/",secondary_phone),
                                      ifelse(primary_phone != "-6", primary_phone,
                                             ifelse(secondary_phone != "-6",secondary_phone, "No Phone Contact")))))
    } else {
      pname <- "Missing Contact Info"
      phone <- pname
    }
    
    df2 <- df_enr %>%
      filter(subjid == inputId)
    
    if(nrow(df2)>0) {
      date_enrolled = with(df2, ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), as.character(as.Date(as.character(vdate),'%m/%d/%y'))))
      
      age = df2$age
      arm = with(df2, ifelse(study_arm == 1, "Intervention", "Control"))
      pinitials = df2$pinitials
      
    } else {
      date_enrolled <- "Not Enrolled"
      age <- date_enrolled
      arm <- date_enrolled
      pinitials <- date_enrolled
    }
    
    df3 <- df_baseline %>%
      filter(subjid == inputId)
    
    
    if(nrow(df3)>0) {
      sex = with(df3, ifelse(sex == 1, "Male", "Female"))
      age = sprintf("%s (<b>Gender</b>:%s)", age, sex)
      marital <- with(df3, case_when(marital == 1 ~ "Unmarried",
                                     marital == 2 ~ "Married - monogamous",
                                     marital == 3 ~ "Divorced/separated",
                                     marital == 4 ~ "Widowed",
                                     marital == 5 ~ 'Cohabiting',
                                     marital == 6 ~ "Married polygamous"))
      
      height_in_m <- with(df3, (height/100) * (height/100))
      bmi_text <- with(df3, sprintf("Weight: %s Kg; Height: %s cm (BMI:%.1f)", weight, height, weight/height_in_m))
    } else {
      marital <- "Baseline Infor"
      bmi_text <- marital
    }
    
    
    
    
    col1 <- sprintf("<b>Name</b>:%s <br>
                  <b>Initials</b>:%s <br>
                  <b>Age</b>:%s <br>
                  <b>Study Arm</b>:%s <br>", pname, pinitials, age, arm)
    
    col2 <- sprintf("<b>Phone Number</b>:%s <br>
                  <b>Date Enrolled</b>:%s <br>
                  <b>Marital Status</b>:%s <br>
                  <b>BMI at Enrollment</b>:%s <br>", phone, date_enrolled, marital, bmi_text)
    
    return(c(col1, col2))
    
  } else {
    return(c("No Records to be viewed", "No Records to be viewed"))
  }
  
}


#-------------------------------------------------------------------------------
# get Participant's Screening and Enrollment Data
#-------------------------------------------------------------------------------
getScreeningDetails <- function(df_enr, selected_subjid) {
  df_enr <- df_enr %>%
    filter(subjid == selected_subjid)
  date_enrolled = with(df_enr, ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), as.character(as.Date(as.character(vdate),'%m/%d/%y'))))
  screen_age = with(df_enr,case_when(screen_age == 1 ~ "Yes",
                                     screen_age == 0 ~ "No"))
  screen_enroll = with(df_enr,case_when(screen_enroll == 1 ~ "Yes",
                                        screen_enroll == 0 ~ "No"))
  screen_hiv = with(df_enr,case_when(screen_hiv == 1 ~ "Yes",
                                     screen_hiv == 0 ~ "No"))
  screen_reside = with(df_enr,case_when(screen_reside == 1 ~ "Yes",
                                        screen_reside == 0 ~ "No"))
  screen_met  = with(df_enr,case_when(screen_met == 1 ~ "Yes",
                                      screen_met == 0 ~ "No"))
  screen_sign  = with(df_enr,case_when(screen_sign == 1 ~ "Yes",
                                       screen_sign == 0 ~ "No"))
  no_sign_reason = df_enr$no_sign_reason
  ret_val <- 'N/A'
  if (no_sign_reason != '-9'){
    not_at_risk <- ifelse(grepl('1', no_sign_reason), 'Participant states they are not at risk for HIV','')
    moved_out <- ifelse(grepl('2', no_sign_reason), 'Participant moved out of area','')
    not_willing <- ifelse(grepl('3', no_sign_reason), 'Participant unwilling to make study visits','')
    partner_refused <- ifelse(grepl('4', no_sign_reason), 'Participant said partner did not give permission','')
    other <- ifelse(grepl('5', no_sign_reason), str_to_sentence(no_sign_other),'')
    ret_val <- paste(not_at_risk, moved_out, not_willing, partner_refused, other, sep = ',')
  }
  
  no_sign_reason <- ret_val
  age = with(df_enr, age)
  pinitials = with(df_enr, pinitials)
  
  # Contact Information
  
  contact_verify  = with(df_enr,case_when(contact_verify == 1 ~ "Yes",
                                          contact_verify == 0 ~ "No"))
  contact_update  = with(df_enr,case_when(contact_update == 1 ~ "Yes",
                                          contact_update == 0 ~ "No"))
  
  return_string <- sprintf("<h4>Eligibility Criteria</h4><hr>
                            <b>Visit date </b>:%s <br>
                            <b>Participant Initials</b> :%s <br>
                            <b>What is your age in completed years?</b> :%s <br>
                            <b>Age ≥ 15 years </b>:%s <br>
                            <b>Enrollment in SEARCH SAPPHIRE Dynamic Prevention Study </b>:%s <br>
                            <b>Is the participant HIV Negative? </b>:%s <br>
                            <b>Residing in Study Region </b>:%s <br>
                            <b>All criteria for study inclusion met? </b>:%s <br>
                            <b>Did the participant sign the DCP CAB-LA Prevention Informed Consent Form? </b>:%s <br>
                            <b>If the participant was eligible, but did not sign the consent, provide reason (may select more than one): </b>:%s
                            <hr> <h4><h4>Participant Contact Information</h4>
                            <b>Participant contact information verified?</b>:%s <br>
                            <b>If contact information changed, participant contact information CRF updated</b>:%s <br>",
                           date_enrolled, pinitials,age, screen_age, screen_enroll, screen_hiv,
                           screen_reside, screen_met, screen_sign, no_sign_reason, contact_verify,contact_update)
  
  return(return_string)
}


#-------------------------------------------------------------------------------
# get Participant's Baseline Cab-LA Data
#-------------------------------------------------------------------------------

getCABLA_Baseline <- function(df_demo, selectedsubjid) {
  df <- df_demo %>%
    filter(subjid == selectedsubjid)
  
  return_string = "" 
  return_string2 = ""
  
  if(nrow(df) > 0) {
    date_enrolled = with(df, ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), as.character(as.Date(as.character(vdate),'%m/%d/%y'))))
    
    int_enrolled = with(df, case_when(int_enrolled == 1 ~ "Yes",
                                      int_enrolled == 0 ~ "No",
                                      TRUE ~ "N/A"))
    
    sex = with(df, case_when(sex == 1 ~ "Male",
                             sex == 2 ~ "Female",
                             TRUE ~ "N/A"))
    
    
    pregnancy_test = with(df, case_when(pregnancy_test == 1 ~ "Yes",
                                        pregnancy_test == 0 ~ "No (Includes decline, refuse",
                                        TRUE ~ "N/A"))
    
    pregnancy_test_result = with(df, case_when(pregnancy_test_result == 1 ~ "Positive",
                                               pregnancy_test_result == 0 ~ "Negative",
                                               TRUE ~ "N/A"))
    
    min_weight = with(df, case_when(min_weight == 1 ~ "Yes",
                                    min_weight == 0 ~ "No",
                                    TRUE ~ "N/A"))
    
    screen_cab_hepc = with(df, case_when(screen_cab_hepc == 1 ~ "Yes",
                                         screen_cab_hepc == 0 ~ "No",
                                         TRUE ~ "N/A"))
    
    screen_cab_hepb = with(df, case_when(screen_cab_hepb == 1 ~ "Yes",
                                         screen_cab_hepb == 0 ~ "No",
                                         TRUE ~ "N/A"))
    
    hbsag_safety = with(df, case_when(hbsag_safety == 1 ~ "Yes",
                                      hbsag_safety == 0 ~ "No",
                                      TRUE ~ "N/A"))
    
    hbsag_result = with(df, case_when(hbsag_result == 1 ~ "Positive",
                                      hbsag_result == 0 ~ "Negative",
                                      TRUE ~ "N/A"))
    
    alt_safety  = with(df, case_when(alt_safety == 1 ~ "Yes",
                                     alt_safety == 0 ~ "No",
                                     TRUE ~ "N/A"))
    
    screen_cab_alt = with(df, case_when(screen_cab_alt == 1 ~ "Yes",
                                        screen_cab_alt == 0 ~ "No",
                                        TRUE ~ "N/A"))
    
    screen_cab_hyper = with(df, case_when(screen_cab_hyper == 1 ~ "Yes",
                                          screen_cab_hyper == 0 ~ "No",
                                          TRUE ~ "N/A"))
    
    screen_cab_liver = with(df, case_when(screen_cab_liver == 1 ~ "Yes",
                                          screen_cab_liver == 0 ~ "No",
                                          TRUE ~ "N/A"))
    
    screen_cab_bleed = with(df, case_when(screen_cab_bleed == 1 ~ "Yes",
                                          screen_cab_bleed == 0 ~ "No",
                                          TRUE ~ "N/A"))
    screen_cab_drug1 = with(df, ifelse(grepl('1',screen_cab_drug), "Yes", "No"))
    screen_cab_drug2 = with(df, ifelse(grepl('2',screen_cab_drug), "Yes", "No"))
    screen_cab_drug3 = with(df, ifelse(grepl('3',screen_cab_drug), "Yes", "No"))
    screen_cab_drug4 = with(df, ifelse(grepl('4',screen_cab_drug), "Yes", "No"))
    screen_cab_drug5 = with(df, ifelse(grepl('5',screen_cab_drug), "Yes", "No"))
    screen_cab_drug6 = with(df, ifelse(grepl('6',screen_cab_drug), "Yes", "No"))
    screen_cab_drug7 = with(df, ifelse(grepl('99',screen_cab_drug), "Yes", "No"))
    
    hivtest = with(df, case_when(hivtest == 1 ~ "Yes",
                                 hivtest == 0 ~ "No",
                                 TRUE ~ "N/A"))
    
    hivtest_result = with(df, case_when(hivtest_result == 1 ~ "Positive",
                                        hivtest_result == 0 ~ "Negative",
                                        hivtest_result == 2 ~ "Indeterminate",
                                        TRUE ~ "N/A"))
    
    specimen_collected = with(df, case_when(specimen_collected == 1 ~ "Yes",
                                            specimen_collected == 0 ~ "No",
                                            TRUE ~ "N/A"))
    
    plasma_collected = with(df, case_when(plasma_collected == 1 ~ "Yes",
                                          plasma_collected == 0 ~ "No",
                                          TRUE ~ "N/A"))
    
    sero_symptoms = with(df, case_when(sero_symptoms == 1 ~ "Yes",
                                       sero_symptoms == 0 ~ "No",
                                       TRUE ~ "N/A"))
    
    sti_screen = with(df, case_when(sti_screen == 1 ~ "Yes",
                                    sti_screen == 0 ~ "No",
                                    TRUE ~ "N/A"))
    
    suicidal = with(df, case_when(suicidal == 1 ~ "Yes",
                                  suicidal == 0 ~ "No",
                                  TRUE ~ "N/A"))
    
    suicidal_action = with(df, ifelse(suicidal_action %in% c("-6", "-9"), "N/a", suicidal_action))
    
    screen_cab_met = with(df, case_when(screen_cab_met == 1 ~ "Yes",
                                        screen_cab_met == 0 ~ "No",
                                        TRUE ~ "N/A"))
    
    not_cab_revert = with(df, case_when(not_cab_revert == 1 ~ "Yes",
                                        not_cab_revert == 0 ~ "No",
                                        TRUE ~ "N/A"))
    
    
    return_string <- sprintf("<h3>Complete before CAB-LA injection</h3><hr>
                              <b> Cab-LA Baseline Visit Date </b>:%s <br>
                              <b>Participant is enrolled in the intervention arm of the SAPPHIRE DCP trial?</b>:%s<br>
                              <b>Participant Sex</b>:%s<br>
                              <b>If female, pregnancy testing done</b>:%s<br>
                              <b>If yes, results</b>:%s<br>
                              <b>Participant weighs at least 35kg</b>:%s<br>
                              <b>Participant has known chronic Hepatitis C Diagnosis</b>:%s<br>
                              <b>Participant has known chronic Hepatitis B Diagnosis</b>:%s<br>
                              <b>HBsAg safety lab done</b>:%s<br>
                              <b>HBsAg result</b>:%s<br>
                              <b>ALT safety lab done</b>:%s<br>
                              <b>Participant has known ALT >=5x ULN</b>:%s<br>
                              <b>Participant has previous hypersensitivity reaction to Cabotegravir</b>:%s<br>
                              <b>Participant has clinical history of liver cirrhosis or current clinical evidence of cirrhosis or severe liver disease</b>:%s<br>
                              <b>Participants has a current or anticipated need for chronic systemic anticoagulation or a history of known or suspected bleeding disorder</b>:%s<br>
                              <b>Participant is receiving the following co-administered drugs for which significant decreases in cabotegravir plasma concentrations may occur due to uridine diphosphate glucuronosyltransferase
                              <ul>
                              <li><b>1:Carbamazepine</b>:%s</li>
                              <li><b>2:Oxcarbazepin </b>:%s</li>
                              <li><b>3:Phenobarbital</b>:%s </li>
                              <li><b>4:Phenytoin</b>:%s </li>
                              <li><b>5:Rifampin</b>:%s </li>
                              <li><b>6:Rifapentine</b>:%s </li>
                              <li><b>99:Participant not receiving any of these drugs</b>:%s</li>
                              </ul>
                              <b>HIV Rapid test done</b>:%s<br>
                              <b>HIV Rapid Test Result</b>:%s<br>
                              <b>Specimen for HIV RNA collected</b>:%s<br>
                              <b>Stored plasma for future HIV testing collected</b>:%s<br>
                              <b>Participant has symptoms of HIV Seroconversion</b>:%s<br>
                              <b>Does the participant have symptoms of an STI?</b>:%s<br>
                              <b>Did the client have evidence of suicidal ideation or severe depression?</b>:%s<br>
                              <b>If yes, action</b>:%s<br>
                              <b>All criteria for CAB-LA Administration met?</b>:%s<br>
                              <b>This participant is not eligible to start CAB. Was the participant reverted to non-CAB-LA intervention schedule?</b>:%s<br>",
                             date_enrolled, int_enrolled,sex,pregnancy_test,pregnancy_test_result,min_weight,
                             screen_cab_hepc,screen_cab_hepb,hbsag_safety,hbsag_result,alt_safety,
                             screen_cab_alt,screen_cab_hyper,screen_cab_liver,screen_cab_bleed,
                             screen_cab_drug1,screen_cab_drug2,screen_cab_drug3,screen_cab_drug4,
                             screen_cab_drug5,screen_cab_drug6,screen_cab_drug7,hivtest,hivtest_result,
                             specimen_collected,plasma_collected,sero_symptoms,sti_screen,suicidal,
                             suicidal_action,screen_cab_met,not_cab_revert)
    
    
    hark_humiliate <- with(df, case_when(hark_humiliate == 1 ~ "Yes",
                                         hark_humiliate == 0 ~ "No",
                                         TRUE ~ "N/A"))
    
    hark_afraid <- with(df, case_when(hark_afraid == 1 ~ "Yes",
                                      hark_afraid == 0 ~ "No",
                                      TRUE ~ "N/A"))
    
    hark_forced <- with(df, case_when(hark_forced == 1 ~ "Yes",
                                      hark_forced == 0 ~ "No",
                                      TRUE ~ "N/A"))
    
    hark_hurt <- with(df, case_when(hark_hurt == 1 ~ "Yes",
                                    hark_hurt == 0 ~ "No",
                                    TRUE ~ "N/A"))
    
    
    injection_received <- with(df, case_when(injection_received == 1 ~ "Yes",
                                             injection_received == 0 ~ "No",
                                             TRUE ~ "N/A"))
    survey_completed <- with(df, case_when(survey_completed == 1 ~ "Yes",
                                           survey_completed == 0 ~ "No",
                                           TRUE ~ "N/A"))
    miss_dose <- with(df, case_when(miss_dose == 1 ~ "Yes",
                                    miss_dose == 0 ~ "No",
                                    TRUE ~ "N/A"))
    
    miss_dose_date = with(df, ifelse(grepl("-", miss_dose_date),  as.character(as.Date(as.character(miss_dose_date),'%Y-%m-%d')), as.character(as.Date(as.character(miss_dose_date),'%m/%d/%y'))))
    
    miss_dose_arrange <- with(df, case_when(miss_dose_arrange == 1 ~ "Yes",
                                            miss_dose_arrange == 0 ~ "No",
                                            TRUE ~ "N/A"))
    
    
    
    return_string2 <- sprintf("<h4>HARK screen for gender-based violence</h4><hr>
                                <b>(H) Within the last year, have you been humiliated or emotionally abused in other ways by your partner or your ex-partner? </b>:%s<br>
                                <b>(A) Within the last year, have you been afraid of your partner or ex-partner? </b>:%s<br>
                                <b>(R) Within the last year, have you been forced to have any kind of sexual activity by your partner or ex-partner? </b>:%s<br>
                                <b>(K) Within the last year, have you been kicked, hit, slapped or otherwise physically hurt by your partner or ex-partner? </b>:%s<br>
                                <h4>Complete after CAB-LA injection</h4><hr>
                                <b>Participant received CAB-LA injection</b>:%s<br>
                                <b>CAB-LA participant Mixed Method survey completed</b>:%s<br>
                                <b>Participant plans to miss next dose</b>:%s<br>
                                <b>If yes, date of planned missed visit</b>:%s<br>
                                <b>If yes, arrangements were made for provision for oral medication during treatment interruption</b>:%s<br>",
                              hark_humiliate, hark_afraid, hark_forced, hark_hurt,
                              injection_received,survey_completed,miss_dose,miss_dose_date,miss_dose_arrange)
  }
  
  return(c(return_string, return_string2))
}


#-------------------------------------------------------------------------------
# get Participant's Cab-LA Follow-up Visit Data
#-------------------------------------------------------------------------------

getCABLA_Followup <- function(df) {
  
  return_string = "" 
  return_string2 = ""
  
  if(nrow(df) > 0) {
    stop_cab = with(df, case_when(stop_cab == 1 ~ "Yes",
                                  stop_cab == 0 ~ "No",
                                  TRUE ~ "N/A"))
    
    stop_cab_reason = with(df, case_when(stop_cab_reason == 1 ~ "Prefers oral pills",
                                         stop_cab_reason == 2 ~ "Does not feel at risk",
                                         stop_cab_reason == 3 ~ "Provider decision (toxicity management)",
                                         stop_cab_reason == 4 ~ paste("Other -",stop_cab_other),
                                         TRUE ~ "N/A"))
    
    
    hivtest = with(df, case_when(hivtest == 1 ~ "Yes",
                                 hivtest == 0 ~ "No",
                                 TRUE ~ "N/A"))
    
    hivtest_result = with(df, case_when(hivtest_result == 1 ~ "Positive",
                                        hivtest_result == 0 ~ "Negative",
                                        hivtest_result == 2 ~ "Indeterminate",
                                        TRUE ~ "N/A"))
    
    specimen_collected = with(df, case_when(specimen_collected == 1 ~ "Yes",
                                            specimen_collected == 0 ~ "No",
                                            TRUE ~ "N/A"))
    
    plasma_collected = with(df, case_when(plasma_collected == 1 ~ "Yes",
                                          plasma_collected == 0 ~ "No",
                                          TRUE ~ "N/A"))
    
    sero_symptoms = with(df, case_when(sero_symptoms == 1 ~ "Yes",
                                       sero_symptoms == 0 ~ "No",
                                       TRUE ~ "N/A"))
    
    hair_collected = with(df, case_when(hair_collected == 1 ~ "Yes",
                                        hair_collected == 0 ~ "No",
                                        TRUE ~ "N/A"))
    
    sti_screen = with(df, case_when(sti_screen == 1 ~ "Yes",
                                    sti_screen == 0 ~ "No",
                                    TRUE ~ "N/A"))
    
    suicidal = with(df, case_when(suicidal == 1 ~ "Yes",
                                  suicidal == 0 ~ "No",
                                  TRUE ~ "N/A"))
    
    suicidal_action = with(df, ifelse(suicidal_action %in% c("-6", "-9"), "N/a", suicidal_action))
    
    hbsag_safety = with(df, case_when(hbsag_safety == 1 ~ "Yes",
                                      hbsag_safety == 0 ~ "No",
                                      TRUE ~ "N/A"))
    
    hbsag_result = with(df, case_when(hbsag_result == 1 ~ "Positive",
                                      hbsag_result == 0 ~ "Negative",
                                      TRUE ~ "N/A"))
    
    alt_safety  = with(df, case_when(alt_safety == 1 ~ "Yes",
                                     alt_safety == 0 ~ "No",
                                     TRUE ~ "N/A"))
    
    screen_cab_alt = with(df, case_when(screen_cab_alt == 1 ~ "Yes",
                                        screen_cab_alt == 0 ~ "No",
                                        TRUE ~ "N/A"))
    
    sex = with(df, case_when(sex == 1 ~ "Male",
                             sex == 2 ~ "Female",
                             TRUE ~ "N/A"))
    
    
    pregnancy_test = with(df, case_when(pregnancy_test == 1 ~ "Yes",
                                        pregnancy_test == 0 ~ "No (Includes decline, refuse",
                                        TRUE ~ "N/A"))
    
    pregnancy_test_result = with(df, case_when(pregnancy_test_result == 1 ~ "Positive",
                                               pregnancy_test_result == 0 ~ "Negative",
                                               TRUE ~ "N/A"))
    
    pregnancy_counsel  = with(df, case_when(pregnancy_counsel == 1 ~ "Yes",
                                            pregnancy_counsel == 0 ~ "No",
                                            TRUE ~ "N/A"))
    
    pregnancy_consent  = with(df, case_when(pregnancy_consent == 1 ~ "Yes",
                                            pregnancy_consent == 0 ~ "No",
                                            TRUE ~ "N/A"))
    
    pregnancy_no_consent_reason <- with(df, ifelse(pregnancy_no_consent_reason %in% c("-6","-9"),"N/a",pregnancy_no_consent_reason))
    
    
    screen_cab_met = with(df, case_when(screen_cab_met == 1 ~ "Yes",
                                        screen_cab_met == 0 ~ "No",
                                        TRUE ~ "N/A"))
    
    no_cab_revert = with(df, case_when(no_cab_revert == 1 ~ "Yes",
                                       no_cab_revert == 0 ~ "No",
                                        TRUE ~ "N/A")) 
    
    offered_cab = with(df, case_when(offered_cab == 1 ~ "Yes",
                                     offered_cab == 0 ~ "No",
                                        TRUE ~ "N/A"))
    
    hark_humiliate <- with(df, case_when(hark_humiliate == 1 ~ "Yes",
                                         hark_humiliate == 0 ~ "No",
                                         TRUE ~ "N/A"))
    
    hark_afraid <- with(df, case_when(hark_afraid == 1 ~ "Yes",
                                      hark_afraid == 0 ~ "No",
                                      TRUE ~ "N/A"))
    
    hark_forced <- with(df, case_when(hark_forced == 1 ~ "Yes",
                                      hark_forced == 0 ~ "No",
                                      TRUE ~ "N/A"))
    
    hark_hurt <- with(df, case_when(hark_hurt == 1 ~ "Yes",
                                    hark_hurt == 0 ~ "No",
                                    TRUE ~ "N/A"))
    
    injection_received <- with(df, case_when(injection_received == 1 ~ "Yes",
                                             injection_received == 0 ~ "No",
                                             TRUE ~ "N/A"))
    
    no_injection_reason <- with(df, ifelse(no_injection_reason %in% c("-6","-9"),"N/a",no_injection_reason))
    
    recommend_prep <- with(df, case_when(recommend_prep == 1 ~ "Yes",
                                         recommend_prep == 0 ~ "No",
                                         TRUE ~ "N/A"))
    
    survey_completed <- with(df, case_when(cab_survey_completed == 1 ~ "Yes",
                                           cab_survey_completed == 0 ~ "No",
                                           TRUE ~ "N/A"))
    miss_dose <- with(df, case_when(miss_dose == 1 ~ "Yes",
                                    miss_dose == 0 ~ "No",
                                    TRUE ~ "N/A"))
    
    miss_dose_date = with(df, ifelse(grepl("-", miss_dose_date),  as.character(as.Date(as.character(miss_dose_date),'%Y-%m-%d')), as.character(as.Date(as.character(miss_dose_date),'%m/%d/%y'))))
    
    miss_dose_arrange <- with(df, case_when(miss_dose_arrange == 1 ~ "Yes",
                                            miss_dose_arrange == 0 ~ "No",
                                            TRUE ~ "N/A"))
    
   female_text <- sprintf('')
   if(sex == "Female") {
     female_text <- sprintf('<h4>For female participants only</h4><hr>
                            <b>If female, pregnancy testing done</b>:%s <br>
                            <b>If participant declined pregnancy consent, declination reason</b>:%s <br>
                            <b>If yes, results</b>:%s <br>
                            <b>If pregnancy test positive, participant was counseled about CAB-LA use during pregnancy</b>:%s <br>
                            <b>If pregnancy test positive, participant was consented using the Dynamic Choice Prevention Extension for Pregnant Participants ICF to continue in study</b>:%s <br>',
                            pregnancy_test,pregnancy_no_consent_reason,pregnancy_test_result,
                            pregnancy_counsel,pregnancy_consent)
   }
    return_string = sprintf("<h4>Complete before CAB-LA Injection</h4><hr>
                            <b>Is the participant discontinuing CAB-LA at this visit?</b>:%s<br>
                            <b>If yes, specify</b>:%s<br>
                            <b>Specimen for HIV RNA collected (CAB-LA Weeks 24 and 48 only)</b>:%s<br>
                            <b>Stored plasma for future HIV testing collected</b>:%s<br>
                            <b>HIV Rapid test done</b>:%s<br>
                            <b>HIV Rapid Test Result</b>:%s<br>
                            <b>Participant has symptoms of seroconversion</b>:%s<br>
                            <b>For CAB-LA Week 8, 24, 32, and 48, hair was collected</b>:%s<br>
                            <b>Participant's previous ALT was >=5x ULN</b>:%s<br>
                            <b>At Week 24 or discretion of provider, ALT safety lab done</b>:%s<br>
                            <b>At discretion of provider, HBsAg safety lab done</b>:%s<br>
                            <b>HBsAg Result</b>:%s<br>
                            <b>Does the participant have symptoms of an STI?</b>:%s<br>
                            <b>Did the client have evidence of suicidal ideation or severe depression?</b>:%s<br>
                            <b>If yes, action</b>:%s<br>",
                            stop_cab,stop_cab_reason,specimen_collected,plasma_collected,
                            hivtest,hivtest_result,sero_symptoms,hair_collected,screen_cab_alt,
                            alt_safety,hbsag_safety,hbsag_result,sti_screen,suicidal,suicidal_action,
                            female_text,screen_cab_met,offered_cab,no_cab_revert) 
    
    return_string2 <- sprintf("<h4>HARK screen for gender-based violence</h4><hr>
                                <b>(H) Within the last year, have you been humiliated or emotionally abused in other ways by your partner or your ex-partner? </b>:%s<br>
                                <b>(A) Within the last year, have you been afraid of your partner or ex-partner? </b>:%s<br>
                                <b>(R) Within the last year, have you been forced to have any kind of sexual activity by your partner or ex-partner? </b>:%s<br>
                                <b>(K) Within the last year, have you been kicked, hit, slapped or otherwise physically hurt by your partner or ex-partner? </b>:%s<br>
                                <h4>Complete after CAB-LA injection</h4><hr>
                                <b>Participant received CAB-LA injection</b>:%s<br>
                                <b>If participant did not receive CAB-LA injection, reason</b>:%s<br>
                                <b>If no, participant was counseled and recommended oral PrEP (TDF/3TC) for an additional 6-months to cover tail?</b>:%s<br>
                                <b>CAB-LA participant Mixed Method survey completed</b>:%s<br>
                                <b>Participant plans to miss next dose</b>:%s<br>
                                <b>If yes, date of planned missed visit</b>:%s<br>
                                <b>If yes, arrangements were made for provision for oral medication during treatment interruption</b>:%s<br>",
                              hark_humiliate, hark_afraid, hark_forced, hark_hurt,
                              injection_received,no_injection_reason,recommend_prep,
                              survey_completed,miss_dose,miss_dose_date,miss_dose_arrange)
  }
  
  return(c(return_string, return_string2))
}


#-------------------------------------------------------------------------------
# get Participant's Non Cab-LA Follow-up Visit Data
#-------------------------------------------------------------------------------

getFollowup <- function(df) {
  return_string = "" 
  return_string2 = ""
  
  if(nrow(df) > 0) {
    
    
    cab_stop = with(df, case_when(cab_stop == 1 ~ "Yes",
                                  cab_stop == 0 ~ "No",
                                  TRUE ~ "N/A"))
    
    cab_stop_reason = with(df, case_when(cab_stop_reason == 1 ~ "Prefers oral pills",
                                         cab_stop_reason == 2 ~ "Does not feel at risk",
                                         cab_stop_reason == 3 ~ "Provider decision (toxicity management)",
                                         cab_stop_reason == 4 ~ paste("Other -",cab_stop_other),
                                         TRUE ~ "N/A"))
    
    testchoice = with(df, case_when(testchoice == 1 ~ "Self",
                                    testchoice == 2 ~ "Rapid",
                                    TRUE ~ "N/A"))
    hivtest_result = with(df, case_when(hivtest_result == 1 ~ "Positive",
                                        hivtest_result == 0 ~ "Negative",
                                        hivtest_result == 2 ~ "Indeterminate",
                                        TRUE ~ "N/A"))
    
    cab_offered = with(df, case_when(cab_offered == 1 ~ "Yes",
                                     cab_offered == 0 ~ "No",
                                                 TRUE ~ "N/A"))
    
    sero_symptoms = with(df, case_when(sero_symptoms == 1 ~ "Yes",
                                       sero_symptoms == 0 ~ "No",
                                       TRUE ~ "N/A"))
    
    
    assess_barriers <- with(df, case_when(assess_barriers == 1 ~ "Yes",
                                          assess_barriers == 0 ~ "No",
                                          TRUE ~ "N/A"))
    
    
    dcp_choice <- with(df, case_when(dcp_choice == 1 ~ "Daily oral PrEP",
                                     dcp_choice == 2 ~ "PEP",
                                     dcp_choice == 3 ~ "None",
                                     TRUE ~ "N/A"))
    
    sti_screen <- with(df, case_when(sti_screen == 1 ~ "Yes",
                                     sti_screen == 0 ~ "No",
                                     TRUE ~ "N/A"))
    
    pregnancy_test <- with(df, case_when(pregnancy_test == 1 ~ "Yes",
                                         pregnancy_test == 0 ~ "No",
                                         TRUE ~ "N/A"))
    
    pregnancy_test_result <- with(df, case_when(pregnancy_test_result == 1 ~ "Positive",
                                                pregnancy_test_result == 0 ~ "Negative",
                                                TRUE ~ "N/A"))
    
    delivery <- with(df, case_when(delivery == 1 ~ "Yes",
                                       delivery == 0 ~ "No",
                                       TRUE ~ "N/A"))
    
    dod <- with(df, ifelse(delivery == 1, dod, "N/a"))
    
    
    vislocation <- with(df, case_when(vislocation == 1 ~ "ANC",
                                      vislocation == 2 ~ "OPD",
                                      vislocation == 3 ~ "Home",
                                      vislocation == 4 ~ "Bar",
                                      vislocation == 5 ~ "Hotel/Restaurant",
                                      vislocation == 6 ~ "Guest House",
                                      vislocation == 7 ~ "Trading Center",
                                      vislocation == 8 ~ "Phone",
                                      TRUE ~ "N/A"))
    
    hark_humiliate <- with(df, case_when(hark_humiliate == 1 ~ "Yes",
                                         hark_humiliate == 0 ~ "No",
                                         TRUE ~ "N/A"))
    
    hark_afraid <- with(df, case_when(hark_afraid == 1 ~ "Yes",
                                      hark_afraid == 0 ~ "No",
                                      TRUE ~ "N/A"))
    
    hark_forced <- with(df, case_when(hark_forced == 1 ~ "Yes",
                                      hark_forced == 0 ~ "No",
                                      TRUE ~ "N/A"))
    
    hark_hurt <- with(df, case_when(hark_hurt == 1 ~ "Yes",
                                    hark_hurt == 0 ~ "No",
                                    TRUE ~ "N/A"))
    
    sex = with(df, ifelse(sex == 1, "Male", "Female"))
    
    female_text <- sprintf('')
    if(sex == "Female") {
      female_text <- sprintf('<h4>For female participants only</h4><hr>
                              <b>Pregnancy testing done (optional)</b>:%s <br>
                              <b>If yes, results</b>:%s <br>
                              <b>Participant gave birth since last visit</b>:%s <br>
                              <b>If participant gave birth, date of delivery</b>:%s <br>',
                             pregnancy_test,pregnancy_test_result,delivery,dod)
    }
    
    return_string = sprintf('<h4>SCHEDULE B: DCP Visits 12, 24, 36, 48</h4><hr>
                            <b>Was the structural assessment of barriers completed?</b>:%s <br>
                            <b>Participant choice for biomedical prevention option</b>:%s <br>
                            <b>Is the participant discontinuing from CAB-LA at this visit?</b>:%s <br>
                            <b>If yes, (specify)</b>:%s <br>
                            <b>Participant choice for HIV testing option (Week 48 must be rapid test)</b>:%s <br>
                            <b>HIV Test Result</b>:%s <br>
                            <b>Was the participant offered CAB-LA at this visit?</b>:%s <br>
                            <b>Participant choice for study visit location</b>:%s <br>
                            <b>Participant has symptoms of STI?</b>:%s <br>
                            <b>Participant has symptoms of seroconversion</b>:%s <br>',
                            assess_barriers,dcp_choice,cab_stop,cab_stop_reason,
                            testchoice,hivtest_result,cab_offered,vislocation,sti_screen,
                            sero_symptoms)
    
    return_string2 <- sprintf("<h4>HARK screen for gender-based violence</h4><hr>
                                <b>(H) Within the last year, have you been humiliated or emotionally abused in other ways by your partner or your ex-partner? </b>:%s<br>
                                <b>(A) Within the last year, have you been afraid of your partner or ex-partner? </b>:%s<br>
                                <b>(R) Within the last year, have you been forced to have any kind of sexual activity by your partner or ex-partner? </b>:%s<br>
                                <b>(K) Within the last year, have you been kicked, hit, slapped or otherwise physically hurt by your partner or ex-partner? </b>:%s<br>
                                %s",
                              hark_humiliate, hark_afraid, hark_forced, hark_hurt,female_text)
  }
  
  
  return(c(return_string, return_string2))
}


#-------------------------------------------------------------------------------
# get Participants have missed their visits and tracking status
#-------------------------------------------------------------------------------
getMissedVisits <- function(df_schedule, df_tracking, not_tracked) {
  
  df_missed <- df_schedule %>%
    filter(window_close < Sys.Date() & status != "Already Seen")
  
  # drop any duplicates
  if(nrow(df_missed)>0) {
    df_missed <- df_missed[!duplicated(df_missed[, c('subjid', 'window_start')]),]
  }
  
  # Update tracking status : Check if participant was tracked within the window period
  df_tracking <- df_tracking %>%
    mutate(date_tracked  = ifelse(grepl("-", vdate),  as.character(as.Date(as.character(vdate),'%Y-%m-%d')), as.character(as.Date(as.character(vdate),'%m/%d/%y'))),
           tracking_outcome = case_when(outcome_tracking == 1 ~ "Participant Found",
                                        outcome_tracking == 0 ~ "Participant Not Found")) %>%
    arrange(desc(as.Date(date_tracked))) %>%
    distinct(subjid, .keep_all = T)
  
  if(nrow(df_missed) > 0 & nrow(df_tracking) > 0) {
    df <- df_missed %>%
      rename(missed_visit_date = window_close) %>%
      select(subjid, target_date, missed_visit_date, visit_week, status) %>%
      merge(df_tracking[, c("subjid", "date_tracked", "tracking_outcome")], by="subjid", all.x = T) %>%
      mutate(tracking_status = ifelse((is.na(date_tracked) | as.Date(target_date) > as.Date(date_tracked)), "Not Tracked", tracking_outcome)) %>%
      arrange(missed_visit_date)
      
  } else {
    
    df <- df_missed %>%
      rename(missed_visit_date = window_close) %>%
      select(subjid, target_date, missed_visit_date, visit_week, status)
  }
   
  if (not_tracked == TRUE) {
    if(nrow(df) > 0) {
      df <- df %>%
        filter(tracking_status == "Not Tracked")
    }
  }
  
  
  final_dt2 = datatable(df,
                       escape=F, selection = 'none',
                       options = 
                         list(# columnDefs = list(list(visible=FALSE, targets=c(7))),
                           pageLength = 5,
                           language = list(
                             zeroRecords = "No Missed Visits",
                             search = 'Search:')), rownames = FALSE)
  print(paste("Check output",typeof(final_dt2)))
  return(final_dt2)
  
}


#-------------------------------------------------------------------------------
# get List of Participants who are due for a their visits based on custom number
# of days provided by the user the minimum is 5 days and maximum is 30 days
#-------------------------------------------------------------------------------

getParticipantsDueForVisit <- function(df_schedule, ndays) {
  # Due for visit in the next ndays or window closes in the ndays are they are yet to be seen
  
  df <- df_schedule %>%
    filter(window_close <= Sys.Date() + as.numeric(ndays) & status != "Already Seen") %>%
    distinct(subjid, .keep_all = T) 
  
  # Add column to check if participant's window closes in the next 5 days and highlight in red
  df <- df %>%
    mutate(closes_5d = ifelse((as.Date(window_close) >= Sys.Date() & as.Date(window_close) - Sys.Date() <=5 & status != "Already Seen"), 1,
                              ifelse(as.Date(window_close) < Sys.Date() & status != "Already Seen", 2, 0)))
  
  final_dt = datatable(df,
                       escape=F, selection = 'none',
                       options = 
                         list(columnDefs = list(list(visible=FALSE, targets=c(7))), # hide the conditional color formatting column
                              pageLength = 5,
                              language = list(
                                zeroRecords = sprintf("No Participants are due for the next %s days", ndays),
                                search = 'Find in table:')), rownames = FALSE)  %>% formatStyle(
                                  'closes_5d',
                                  target = 'row',
                                  backgroundColor = styleEqual(c(2, 1, 0), c('yellow', 'red', 'white'))
                                )
  
  return(final_dt)
  
  
}