# loading all necessary libraries 
library(readxl)
library(tidyverse)

# reading in 
process <- function(IN, OUT) {
  df <- if (grepl("\\.csv$", IN, ignore.case = TRUE))
    read_csv(IN, col_types = cols(.default = "c"),
             locale = locale(encoding = guess_encoding(IN)$encoding[1])) else
               read_excel(IN, col_types = "text")
  df <- df %>% mutate(across(everything(), ~enc2utf8(replace_na(., "")))) # force UTF-8 so æøå survive
  
  g  <- function(p, r, x) gsub(p, r, x, perl = TRUE)
  is_x <- function(x) grepl("^\\s*x\\s*$", x, ignore.case = TRUE)
  
  clean <- function(x) {
    x <- g("(?i)\\\\?operatorname\\s*\\{[^}]*\\}", " ", x) # removes OCR mistakes
    x <- g("(?i)\\\\?\\bemptyset\\b", "\u00D8", x)
    x <- g("(?i)\\\\?\\b(operatorname|mathrm|text|prime|[Il1]?dots)\\b", " ", x)
    x <- g("[\\\\{}$^_#]", " ", x)
    x <- g("\\b(STATE|CONTROL)\\b", " ", x)
    x <- g("[^A-Za-z\u00C0-\u024F0-9 .,;:!?()/&'\"-]", " ", x)
    x <- g("\\.{2,}|\u2026", " ", x) # trailing dots 
    x <- g("[ .]+$", "", x)
    trimws(g("\\s{2,}", " ", x)) 
  }
  
  for (j in seq_len(ncol(df)))                  df[[j]] <- clean(df[[j]]) # applying clean function 
  if ("anmerkn" %in% names(df))                 df$anmerkn <- trimws(g("(?i)beregnet\\s*\\(ikke fra ocr\\)", "", df$anmerkn)) # drop calc flag
  for (j in intersect(7, seq_len(ncol(df))))    df[[j]] <- clean(g("[0-9]+", " ", df[[j]])) # removes digits from G 
  
  do_pat <- "^\\s*do\\s*\\.?\\s*$"
  for (i in 2:nrow(df)) {
    if (grepl(do_pat, df$eier_bruker[i], ignore.case = TRUE))
      df$eier_bruker[i] <- df$eier_bruker[i - 1]
  } # taking the thing from above loop in order to fill up do.
  
  # removes sogn headers (don't need them)
  xs    <- trimws(df$eier_bruker)
  words <- lengths(strsplit(xs, "\\s+"))
  junk  <- grepl("sogn\\.?\\)?$", xs, ignore.case = TRUE) & words <= 2 &
    !grepl("sognepr", xs, ignore.case = TRUE)
  df <- df[!junk, ]
  
  # calculation of mark
  num <- function(x) {
    x <- gsub("[^0-9]", "", trimws(as.character(x)))
    x[!nzchar(x)] <- "0"
    as.numeric(x)
  }
  df$markfinal <- ifelse(is_x(df$mark) | is_x(df$ore), NA,
                         num(df$mark) + num(df$ore) / 100)
  
  # merging across rows 
  merge_wraps <- function(df) {
    blank <- function(nm) if (is.null(df[[nm]])) rep(TRUE, nrow(df))
    else is.na(df[[nm]]) | trimws(df[[nm]]) == ""
    cont <- blank("brugs_no_raw") & blank("gaards_no_raw") &
      blank("mark") & blank("ore")
    cols <- c("gaardens_navn", "brugets_navn", "eier_bruker")
    for (i in which(cont)) {
      j <- i - 1
      while (j >= 1 && cont[j]) j <- j - 1
      if (j < 1) next
      for (k in cols) {
        a <- trimws(df[[k]][j]); b <- trimws(df[[k]][i])
        if (b == "") next
        df[[k]][j] <- if (grepl("[-\u00AC]$", a)) paste0(sub("[-\u00AC]$", "", a), b)
        else trimws(paste(a, b))
      }
    }
    df[!cont, ]
  }
  
  df <- merge_wraps(df)                      # <- run first, raw columns still present
  
  # remove columns I do not need
  df <- df %>% select(-any_of(c("gaards_no_raw", "brugs_no_raw", "check"))) # dropping columns I do not need 
  
  # saving the file 
  write_excel_csv(df, OUT, na = "") # UTF-8 with BOM: Excel shows æøå correctly
  cat(basename(IN), "->", nrow(df), "rows,",
      sum(is.na(df$markfinal)), "unread skyld\n")
  invisible(df)
}

files <- list.files("Excel", "\\.(xlsx?|csv)$", full.names = TRUE)  # move Kristians_1904.csv into Excel/
files <- files[!grepl("^~\\$", basename(files))]          # skip Excel lock files

dir.create("CSV-Files", showWarnings = FALSE)
for (f in files) {
  out <- file.path("CSV-Files", paste0(tools::file_path_sans_ext(basename(f)), ".csv"))
  try(process(f, out))
}

check <- read.csv("CSV-Files\\Akershus_1903.csv")
