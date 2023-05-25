library(stringr)

pdf_text <- pdf_text("mq_20223_04669.pdf") |> str_split("\n")
pdf_text <- pdf_text[-2]
pdf_text <- pdf_text[[1]][-1:-19]
# PDF1 <- PDF1[-31:-37]
pdf_text <- pdf_text[-c(2,4,8:11,13,17:18,22:25,27,31:37)] |> str_squish()
PDF3 <- pdf_text[-c(2:4,9:13,18,23:27,32:37)] |> str_squish()

df <- data.frame(
  Name = str_extract(pdf_text, "^[^0-9]+"),
  Value = as.numeric(str_extract(pdf_text, "[0-9.]+")),
  Unit = str_extract(pdf_text, "(?<= )µmol/l|µg/l|ng/l(?=[ ,])"),
  `proz.Abweichung` = str_extract(pdf_text, "(?<=µmol/l|µg/l|ng/l|%)\\s*[±-]?\\s*[0-9.]+%"),
  Deviation = str_extract(pdf_text, "(?<=%)[^,]+")
   )


df1 <- data.frame(
  Name = str_extract(PDF3, "^[^0-9]+"),
  Value = as.numeric(str_extract(PDF3, "[0-9.]+")),
  Unit = str_extract(PDF3, "(?<= )µmol/l|µg/l|ng/l(?=[ ,])"),
  Abweichung = str_extract(PDF3, "-?[0-9.]+%"),
  Deviation = str_extract(PDF3, "(?<=%)[^,]+")
)
# Load the necessary packages
# library(stringr)
# 
# # Define the vector
# PDF1 <- c("Ammoniak, Beckman",
#           "Ihr Resultat 98.0 µmol/l 5.4% Abweichung",
#           "Zielwert 93.0 µmol/l ± 25.0% MQ Toleranz",
#           "Cyclosporin, AU5800 (Architect)",
#           "Ihr Resultat 262.0 µg/l -6.4% Abweichung",
#           "Zielwert 280.0 µg/l ± 25.0% MQ Toleranz",
#           "Tacrolimus, Alle Methoden",
#           "Ihr Resultat 17.9 µg/l 11.0% Abweichung",
#           "Zielwert 16.1 µg/l ± 25.0% MQ Toleranz",
#           "Inhibin B, Beckman (Alle Methoden)",
#           "Ihr Resultat 16.4 ng/l 22.4% Abweichung",
#           "Zielwert 13.4 ng/l ± 25.0% MQ Toleranz",
#           "")
# 
# # Define the regular expression patterns
# pattern_name <- "^[^0-9]+"
# pattern_value <- "[0-9.]+"
# pattern_unit <- "[^0-9.]+"
# pattern_deviation <- "-?[0-9.]+%"
# 
# # Extract the relevant information and store in a data.frame
# df <- data.frame(
#   Name = str_extract(PDF1, pattern_name),
#   Value = as.numeric(str_extract(PDF1, pattern_value)),
#   Unit = str_extract(PDF1, "(?<=µmol/l|µg/l|ng/l)[^,]+"),
#   Deviation = str_extract(PDF1, "(?<=%)[^,]+") #,
#   #Unit = str_extract(PDF1, pattern_unit),
#   #Deviation = as.numeric(str_extract(PDF1, pattern_deviation))
# )
# 
# library(stringr)
# 
# library(stringr)
# 
# df1 <- data.frame(
#   Name = str_extract(PDF1, "^[^,]+"),
#   Value = as.numeric(sub("^[^0-9]+", "", PDF1)),
#   Unit = str_extract(PDF1, "(?<=µmol/l|µg/l|ng/l)[^,]+"),
#   Deviation = str_extract(PDF1, "(?<=%)[^,]+"),
#   MQ_Tolerance = str_extract(PDF1, "(?<=MQ Toleranz)[^,]+")
# )
# 
# 
# # Remove rows where Name or Value is NA
# df1 <- df1[complete.cases(df[c("Name", "Value")]),]
# 
# # Group rows by Name and Unit and take the average Value, Deviation, and MQ_Tolerance
# library(dplyr)
# df2 <- df1 %>%
#   group_by(Name, Unit) %>%
#   summarize(Value = Value,
#             Deviation = as.numeric(str_extract(Deviation, "\\d+\\.\\d+")),
#             MQ_Tolerance = as.numeric(str_extract(MQ_Tolerance, "\\d+\\.\\d+")) %>%
#   # ungroup()
# 
# # Pivot the table to put Unit, Deviation, and MQ_Tolerance in separate columns
# library(tidyr)
# df <- df %>%
#   pivot_wider(names_from = "Unit", values_from = "Value") %>%
#   pivot_wider(names_from = "Unit", values_from = "Deviation", names_prefix = "Deviation_", values_fn = list(Deviation_ = mean)) %>%
#   pivot_wider(names_from = "Unit", values_from = "MQ_Tolerance", names_prefix = "MQ_Tolerance_", values_fn = list(MQ_Tolerance_ = mean))
# 
# # Rename columns
# colnames(df) <- sub("^\\d+\\s", "", colnames(df))
# 
# 
# df <- data.frame(
#   Name = str_extract(PDF1, pattern_name),
#   Value = as.numeric(str_extract(PDF1, pattern_value)),
#   Unit = as.numeric(str_extract(PDF1, "(?<=µmol/l|µg/l|ng/l|%)\\s*([0-9.]+)")),
#   Deviation = str_extract(PDF1, "(?<=%)[^,]+")
# )
# 

df <- data.frame(
  Name = c(str_extract(pdf_text, "^[^0-9]+[A-Z]*[0-9]*\\s[^0-9]*")),
  Value = as.numeric(str_extract(pdf_text, "(?<=\\s|[^[:alnum:]])[0-9.]+")),
  Unit = str_extract(pdf_text, "(?<= )µmol/l|µg/l|ng/l(?=[ ,])"),
  `proz.Abweichung` = str_extract(pdf_text, "(?<=µmol/l|µg/l|ng/l|%)\\s*[±-]?\\s*[0-9.]+%"),
  Deviation = str_extract(pdf_text, "(?<=%)[^,]+")
)
colnames(df)[1] <- str_extract(pdf_text[1], "^[^,]+,?")
df <- df[-1, ]


library(tidyr)

tidy_df <- df %>%
  pivot_longer(cols = c(Value, Unit, `proz.Abweichung`, Deviation), 
               names_to = "Variable", 
               values_to = "Value") %>%
  filter(!is.na(Value))

library(tidyr)

df_tidy <- df %>% 
  gather(key = "Variable", value = "Measurement", -Name) %>% 
  separate(Variable, into = c("Metric", "Qualifier"), sep = "\\.", extra = "merge") %>% 
  spread(key = Qualifier, value = Measurement)

df_tidy


