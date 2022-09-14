r = getOption('repos')
r["CRAN"] = 'http://cran.us.r-project.org'
options(repos = r)
list.of.packages <- c("shiny", "rprojroot", "DT", "timevis", "lubridate", "plyr", "dplyr", "ggplot2", 
                      "RODBC", "readxl", "shinyShortcut", "shinyWidgets", "devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# TODO: Add calendar and RDOBMClient
list.of.packages <- c("fullcalendar", "RDCOMClient")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require("devtools")

if (!"fullcalendar" %in% installed.packages()[,"Package"]){
  devtools::install_github("rasmusab/fullcalendar")
}

if (!"RDCOMClient" %in% installed.packages()[,"Package"]){
  devtools::install_github("omegahat/RDCOMClient")
}

require(shiny)
require(rprojroot)
require(shinyShortcut)
require(RDCOMClient)


folder_address = dirname(thisfile())


runApp(folder_address, launch.browser= function(shinyurl) {
  
  ieapp <- COMCreate("InternetExplorer.Application")
  ieapp[["MenuBar"]] = FALSE
  ieapp[["StatusBar"]] = FALSE
  ieapp[["ToolBar"]] = FALSE
  ieapp[["Visible"]] = TRUE
  ieapp$Navigate(shinyurl)
  
})
