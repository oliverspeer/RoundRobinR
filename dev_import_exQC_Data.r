# prepare libraries
library(tidyverse)

# set working directory ----------------------------------------------------
setwd("C:/R_local/RoundRobinR")

# read data from several csv file
# containing pattern for file names
fun.read.qmzhcsv.data <- function(file_pattern, df_name) {
  data_path <- "C:\\R_local\\labStat\\"
  files <- list.files(data_path)
  file_names <- files[grep(file_pattern, files)]                                                                                                                                                                                                     
  
#read data from several csv file
  df <- data.frame()
  for (file_name in file_names) {
    full_path <- file.path(data_path, file_name)
    df <- bind_rows(df, read_csv(full_path))
  }
  assign(df_name, df, envir = .GlobalEnv)
}

fun.read.qmzhcsv.data("4669", "qmzh.data")
