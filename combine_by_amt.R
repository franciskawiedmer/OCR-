
library(dplyr)
library(readr)
library(purrr)
library(writexl)


# combining the data by amt
in_dir  <- "matrikkel_1950_ocr_format"
out_dir <- "matrikkel_1950_xlsx"
dir.create(out_dir, showWarnings = FALSE)

files <- list.files(in_dir, "\\.csv$", recursive = TRUE, full.names = TRUE)
amt <- basename(dirname(files))

iwalk(split(files, amt), function(paths, name) {
  data <- map_dfr(paths, read_csv, show_col_types = FALSE,
                  col_types = cols(.default = "c"))
  write_xlsx(data, file.path(out_dir, paste0(name, "_1950_ExcelCorrection.xlsx")))
})

# check whether it worked 
library(readxl)
akershus <- read_excel("matrikkel_1950_xlsx/Akershus_1950_ExcelCorrection.xlsx")
head(akershus)
unique(akershus$herred) # looks good 

# check another one 
ostfold <- read_excel("matrikkel_1950_xlsx/Østfold_1950_ExcelCorrection.xlsx")
head(ostfold)
unique(ostfold$herred) # looks good
