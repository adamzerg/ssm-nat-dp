#install.packages('taskscheduleR')
#install.packages('miniUI')
#install.packages('shiny')
#install.packages('shinyFiles')

library(taskscheduleR)
taskscheduler_stop("r_infochart_png")
taskscheduler_delete("r_infochart_png")

taskscheduler_create(
  taskname = "r_infochart_png",
  rscript = "D:\\Documents\\GitHub\\ssm-nat-dp\\02-ssm-nat-dp.R",
  schedule = "MINUTE",
  starttime = format(Sys.time() + 62, "%H:%M"),
  startdate = format(Sys.Date(), "%Y-%m-%d"),
  Rexe = file.path(Sys.getenv("R_HOME"),"bin","Rscript.exe")
)

setwd("D:/Documents/GitHub/ssm-nat-dp")