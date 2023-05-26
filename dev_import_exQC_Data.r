# prepare libraries
library(tidyverse)

# set working directory ----------------------------------------------------
setwd("C:/R_local/RoundRobinR")

# read data from several csv file
# containing pattern for file names
read_multiple_extqm_csv <- function(directory_path, file_pattern, df_name) {
  file_paths <- list.files(path = directory_path, pattern = file_pattern, full.names = TRUE)
  combined_df <- data.frame()
  
  for (file_path in file_paths) {
    df <- read.csv(file_path, header = T, sep = ";", dec = ".", fileEncoding = "Windows-1253")
    df <- df[,1:14]
    combined_df <- rbind(combined_df, df)
  }
  
  assign(df_name, combined_df, envir = .GlobalEnv)
}


# fun.read.qmzhcsv.data <- function(file_pattern, df_name) {
#   data_path <- "C:\\R_local\\RoundRobinR\\"
#   files <- list.files(data_path)
#   file_names <- files[grep(file_pattern, files)]                                                                                                                                                                                                     
#   
# #read data from several csv file
#   df <- data.frame()
#   for (file_name in file_names) {
#     full_path <- file.path(data_path, file_name)
#     df <- bind_rows(df, read_csv(full_path))
#   }
#   assign(df_name, df, envir = .GlobalEnv)
# }
# 
# fun.read.qmzhcsv.data("4669", "qmzh.data")

read_multiple_extqm_csv("c:/R_local/RoundRobinR", "4669_", "qmzh.data")


# read_multiple_extqm_csv <- function(directory_path, file_pattern, df_name) {
#   file_paths <- list.files(path = directory_path, pattern = file_pattern, full.names = TRUE)
#   combined_df <- NULL
#   
#   for (file_path in file_paths) {
#     df <- read.csv(file_path)
#     if (is.null(combined_df)) {
#       combined_df <- df
#     } else {
#       combined_df <- rbind(combined_df, df[, names(combined_df)])
#     }
#   }
#   
#   assign(df_name, combined_df, envir = .GlobalEnv)
# }
# 
