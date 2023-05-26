library(stringr)
library(pdftools)

pdf_text <- pdf_text("23-03-CH.pdf") |> str_split("\n")
pdf_text3 <- pdf_text[[3]][-1:-5] |> str_squish()
PDF3 <- pdf_text3[-c(33:35, 39:52)]
PDF.date <- str_extract(pdf_text3[[49]], "[0-9]+")

# cscq.data <- matrix(nrow = length(PDF4), ncol = 7)

# max_elements <- max(sapply(strsplit(PDF4, "\\s+"), length))
# cscq.data <- matrix(nrow = length(PDF4), ncol = max_elements)
# for (i in 1:length(PDF4)) {
#   line <- gsub("\\[|\\]", "", PDF4[i])  # Remove square brackets
#   elements <- strsplit(line, "\\s+")[[1]]  # Split line into elements
#   cscq.data[i, 1:length(elements)] <- elements  # Store elements in the matrix
# }
# 
# cscq.df <- as.data.frame(cscq.data, stringsAsFactors = FALSE)
# colnames(cscq.df) <- c(1:max_elements)


cscq.df <- data.frame(
  Code = as.numeric(str_extract(PDF3, "[0-9]+")),
  substanz = as.character(str_extract(PDF3, "(?<=\\d\\s)[^0-9]+(?=\\s(?:mmol/L|µmol/L|µg/L|ng/L|g/L|mosmol/kg|U/L))")),
  einheit =as.character(str_extract(PDF3, "(?<= )mmol/L|µmol/L|µg/L|ng/L|g/L|mosmol/kg|U/L(?=[ ,])")),
  ziel = as.numeric(str_extract(PDF3, "(?<=\\s|[^[:alnum:]])[0-9.]+")),
  resultat = as.numeric(str_extract(PDF3, "(?<=[[:digit:]]\\s)[0-9.]+")),
  
)
