#install.packages('taskscheduleR')
#install.packages('miniUI')
#install.packages('shiny')
#install.packages('shinyFiles')

library(taskscheduleR)
taskscheduler_stop("r_web_scraping_aptmon")
taskscheduler_delete("r_web_scraping_aptmon")

taskscheduler_create(
  taskname = "r_web_scraping_aptmon",
  rscript = "D:\\Documents\\GitHub\\ssm-nat-dp\\scraping\\SSM-aptmon-scraping.R",
  schedule = "MINUTE",
  starttime = format(Sys.time() + 62, "%H:%M"),
  startdate = format(Sys.Date(), "%Y-%m-%d"),
  Rexe = file.path(Sys.getenv("R_HOME"),"bin","Rscript.exe")
)

