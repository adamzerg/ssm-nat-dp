#install.packages('taskscheduleR')
#install.packages('miniUI')
#install.packages('shiny')
#install.packages('shinyFiles')

library(taskscheduleR)
taskscheduler_stop("r_xlsx_scraping_RNA010")
taskscheduler_delete("r_xlsx_scraping_RNA010")

taskscheduler_create(
  taskname = "r_xlsx_scraping_RNA010",
  rscript = "D:\\Documents\\GitHub\\ssm-nat-dp\\scraping\\SSM-RNA010-download.R",
  schedule = "MINUTE",
  starttime = format(Sys.time() + 62, "%H:%M"),
  startdate = format(Sys.Date(), "%Y-%m-%d"),
  Rexe = file.path(Sys.getenv("R_HOME"),"bin","Rscript.exe")
)


