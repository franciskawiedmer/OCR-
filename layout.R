
# load packages
library(dplyr)
library(stringr)
library(readr)
library(purrr)

# define folders 
in_dir  <- "matrikkel_1950"
out_dir <- "matrikkel_1950_ocr_format"
dir.create(out_dir, showWarnings = FALSE) 

transform_file <- function(f) {
  d <- read_csv(f, show_col_types = FALSE, col_types = cols(.default = "c"))
  m <- str_match(d$skyld, "(\\d+)\\D+mark\\D+(\\d+)")
  rel <- sub(paste0("^", in_dir, "/"), "", f)
  out_path <- file.path(out_dir, rel)
  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE) # rename headers and stuff
  d |> transmute(
    herred        = herad,
    sogn          = NA_character_,
    gaards_no     = gaard_nr,
    brugs_no      = bruk_nr,
    gaardens_navn = gaard_name,
    brugets_navn  = bruk_name,
    eier_bruker   = owner,
    mark          = m[, 2],
    ore           = m[, 3],
    anmerkn       = comment,
    postanstalt   = NA_character_,
    check         = NA_character_
  ) |> write_excel_csv(out_path)
}

# show how many files
files <- list.files(in_dir, "\\.csv$", recursive = TRUE, full.names = TRUE)
message(length(files), " files found") # how many files are there in total?

# run function transform_file on all files 
walk(files, transform_file)


# read in one for example 
akershus <- read.csv("matrikkel_1950_ocr_format\\Akershus\\Akershus_Blaker.csv")

#read xlsx file for comparison
library(readxl)
smaalenenes <- read_excel("Smaalenenes_1903_FinalFile.xlsx")
head(smaalenenes)
head(akershus)

# both have the same columns, except that akershus has less (no raw columns)
# there is no sogn in akershus 
