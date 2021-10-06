
library(taskscheduleR)

taskscheduler_create(
  taskname="r_update_functions",
  rscript="C:\\Users\\gudjo\\Documents\\PhD\\Covid19\\covid19_app\\app\\data\\smoothed_curves.R",
  schedule = "DAILY",
  starttime = "11:00",
  startdate = format(Sys.Date(), "%m/%d/%Y"),
  Rexe = file.path(Sys.getenv("R_HOME"), "bin", "Rscript.exe")
)

taskscheduler_stop("r_update_functions")
taskscheduler_delete("r_update_functions")
