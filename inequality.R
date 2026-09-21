library(tidyverse)                                                        # loads dplyr, readr, purrr, stringr

key  <- function(x) str_squish(str_replace_all(str_to_lower(x),           # lowercase the owner name,
                                               "[^a-z\u00e0-\u024f ]", " ")) # replace everything except letters (incl. æøå) with a space, then collapse spaces
gini <- function(x) {                                                     # Gini coefficient of a vector of land values
  x <- sort(x)                                                            # sort values from smallest to largest
  n <- length(x)                                                          # number of owners
  sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))                        # standard formula for the Gini on sorted data
}
top  <- function(x, p) {                                                  # share of total value held by the top p (e.g. 0.01 = top 1%)
  x <- sort(x, TRUE)                                                      # sort values from largest to smallest
  w <- pmin(pmax(p * length(x) - seq_along(x) + 1, 0), 1)                 # weight 1 for owners fully inside the top group, a fraction for the owner at the cutoff, 0 for the rest
  sum(w * x) / sum(x)                                                     # weighted top value divided by total value
}

d <- list.files("CSV-Files", "\\.csv$", full.names = TRUE) %>%            # all cleaned CSV files
  set_names(~ tools::file_path_sans_ext(basename(.))) %>%                 # name each file by its file name (e.g. Akershus_1903)
  map(~ read_csv(.x, col_types = cols(.default = "c", markfinal = "d")) %>% # read all columns as text, markfinal as a number
        rename(parish = 2) %>%                                            # column B holds the parish; call it "parish"
        fill(1, parish)) %>%                                              # fill empty cells in columns A and B with the value above, within each file
  bind_rows(.id = "file") %>%                                             # stack all files, keep the file name in column "file"
  filter(!if_any(where(is.character),                                     # drop rows where any text column contains
                 ~ str_detect(.x, regex("overf[\\u00f8o]r(es|t)", ignore_case = TRUE)) %in% TRUE)) %>% # "Overføres"/"Overført" (also OCR'd without ø)
  mutate(owner = key(eier_bruker)) %>%                                    # normalized owner name used for matching
  filter(owner != "")                                                     # drop rows without an owner name

units <- c("file", "parish")                                              # one unit = one parish within one file

ineq <- function(d, units) {                                              # computes inequality measures per unit
  d %>%
    group_by(across(all_of(c(units, "owner")))) %>%                       # one group per owner within each unit
    summarise(value     = sum(markfinal, na.rm = TRUE),                   # sum all land value of the same owner (several properties / repeated names)
              n_parcels = n(),                                            # number of properties this owner holds
              unread    = sum(is.na(markfinal)),                          # number of unreadable values ("x") for this owner
              .groups = "drop") %>%
    group_by(across(all_of(units))) %>%                                   # one group per unit
    summarise(n_owners        = sum(value > 0),                           # number of owners with positive land value
              n_multi_owners  = sum(n_parcels > 1 & value > 0),           # number of owners holding more than one property
              n_unread        = sum(unread),                              # unreadable values in the unit
              total_value     = sum(value),                               # total land value in the unit (mark)
              gini            = gini(value[value > 0]),                   # Gini coefficient
              top1_share      = top(value[value > 0], .01),               # share held by the top 1%
              top10_share     = top(value[value > 0], .10),               # share held by the top 10%
              .groups = "drop")
}

res <- ineq(d, units)                                                     # one row per parish
write_excel_csv(res, "land_inequality_parish.csv")                        # save the results

check <- read.csv("land_inequality_parish.csv")
akershus <- read.csv("CSV-Files\\Akershus_1903.csv")
