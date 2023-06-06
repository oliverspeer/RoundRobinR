library(stringr)
library(pdftools)
library(readr)

process.pdf <- function(filename){
pdf_text <- pdf_text(filename) |> str_split("\n")
date_value <- str_extract(pdf_text[[1]][14], "[0-9]{2}-[0-9]{2}")  # Extract the date value

pdf_text_squished <- lapply(pdf_text[3:length(pdf_text)], str_squish)
PDFunl <- unlist(pdf_text_squished)
PDFC <- PDFunl[-c((length(PDFunl)-40):length(PDFunl))]


# extract the range
Bereich = str_extract(PDFC, "\\[\\d+\\.?\\d*-\\d+\\.?\\d*\\] \\(\\d+%\\)|(?<=Bereich :\\s)--") |> na.omit()

# create empty vectors for ZielU and ZielO
ZielU = numeric(length(Bereich))
ZielO = numeric(length(Bereich))

# loop through the Bereich and assign ZielU and ZielO
for (i in seq_along(Bereich)) {
  if (Bereich[i] == "(?<=Bereich :\\s) --") {
    ZielU[i] = NA
    ZielO[i] = NA
  } else {
    ZielU[i] = as.numeric(str_extract(Bereich[i], "(?<=\\[)\\d+\\.?\\d*(?=-)"))
    ZielO[i] = as.numeric(str_extract(Bereich[i], "(?<=-)(\\d+\\.?\\d*)(?=])"))
  }
}

toleranz = numeric(length(Bereich))
for (i in seq_along(Bereich)) {
  if (Bereich[i] == "(?<=Bereich :\\s) --") {
    toleranz[i] = NA
  } else {
    toleranz[i] = as.numeric(str_extract(Bereich[i], "(?<=\\()\\d+(?=%\\))"))
  }
}


# # Extract the values or the indicators
# zielwert <- str_extract(PDFC, "(?<=Zielwert : )\\d+\\.?\\d*|(?<=Zielwert : )(\\+ Pos\\.|Positiv|Negativ|--)") |> na.omit()
# 
# # Initialize an empty numeric vector for zielWert
# zielWert <- numeric(length(zielwert))
# 
# # Loop through the zielwert and assign zielWert
# for (i in seq_along(zielwert)) {
#   if (zielwert[i] %in% c("+ Pos.", "Positiv", "Negativ", "--")) {
#     zielWert[i] <- 0
#   } else {
#     zielWert[i] <- as.numeric(zielwert[i])
#   }
# }


cscq.df.ff <- data.frame(
  Code = as.numeric(str_extract(PDFC, "(?<=\\[)(\\d+)(?=\\] ———————————————)") |> na.omit()),
  substanz = as.character(str_extract(PDFC, "(.*)(?= \\[\\d+\\] —————)") |> na.omit()),
  geraet = as.character(str_extract(PDFC, "(?<=Gerät :\\s)(.*)") |> na.omit()),
  resultat = as.character(str_extract(PDFC, "(?<=Resultat :\\s)(.*)") |>  na.omit()),
  ZielU = ZielU,
  ZielO = ZielO,
  ziel = as.character(str_extract(PDFC, 
                                  "(?<=Zielwert : )\\d+\\.?\\d*|(?<=Zielwert : )(\\+ Pos\\.|Positiv|Negativ|--)") |> 
                        na.omit()),
  toleranz = toleranz
  )
cscq.df.ff$Datum  <-  as_date(rep(date_value, nrow(cscq.df.ff)), format = "%y-%m")
cscq.df.ff <- cscq.df.ff |> mutate(jahr = year(Datum), quar = quarter(Datum))



# Add a new column 'Units' by extracting it from 'Resultat'
cscq.df.ff <- cscq.df.ff %>%
  mutate(einheit = str_extract(resultat, "\\D+$"),  # Extracts non-digit characters at the end of the string
         resultat = parse_number(resultat))  # Converts 'Resultat' to numeric

return(cscq.df.ff)
}

# Define the directory
# dir_path <- "/path/to/your/directory"
# 
# # Define the pattern
# pattern <- "^\\d{2}-\\d{2}-[A-Za-z]+\\.pdf$"
# 
# # List files that match the pattern
# pdf_files <- list.files(path = dir_path, pattern = pattern)



# Specify the PDF files to process
pdf_files <- c("23-03-CH.pdf",  "22-12-CH.pdf", 
               "22-11-CH.pdf",  "22-09-CH.pdf", "22-06-CH.pdf", "22-05-CH.pdf")

# Use lapply to apply the function to each file and bind the results together into a single data frame
df_list <- lapply(pdf_files, process.pdf)

# bind all dataframes together
cscq.data <- bind_rows(df_list)


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
  saveRDS(df_name, "RVDaten.rds")
}

# read data ------------------------------------------------------------
read_multiple_extqm_csv("c:/R_local/RoundRobinR", "4669_", "qmzh.data")

RV.data.KC <- merge(qmzh.data, cscq.data, by = intersect(names(qmzh.data), names(cscq.data)), all = TRUE)