library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(purrr)

base    <- "https://www.dokpro.uio.no/cgi-bin/stad/matr50.cgi?task=h"
domain  <- "https://www.dokpro.uio.no"
out_dir <- "matrikkel_1950"

# Optional filters — set to NULL to scrape everything
target_fylke <- "Østfold"   # change, for example to Ostfold
target_herad <- "Tune"  # change accordingly (for example Askim)

dir.create(out_dir, showWarnings = FALSE)

fetch <- function(u) { Sys.sleep(0.3); read_html(u) }
absu  <- function(h) if (startsWith(h, "http")) h else paste0(domain, h)

get_links <- function(url, pat) {
  a <- fetch(url) %>% html_elements("a")
  h <- html_attr(a, "href")
  k <- !is.na(h) & str_detect(h, pat)
  tibble(name = str_trim(html_text(a[k])), url = map_chr(h[k], absu)) %>% distinct()
}

parse_gaard <- function(url) {
  page  <- fetch(url)
  fylke <- page %>% html_element("h2") %>% html_text() %>% str_trim()
  herad <- page %>% html_element("h3") %>% html_text() %>% str_trim() %>% str_remove("\\s*herad\\s*$")
  rows  <- page %>% html_elements("table tr")
  if (length(rows) < 2) return(tibble())
  hd <- rows[[1]] %>% html_elements("td") %>% html_text() %>% str_trim()
  bruks <- map(rows[-1], function(r) {
    c <- r %>% html_elements("td") %>% html_text() %>% str_squish()
    if (length(c) < 5) return(NULL)
    tibble(bruk_nr = c[1], bruk_name = c[2], skyld = c[3], owner = c[4], comment = c[5])
  }) %>% compact() %>% bind_rows()
  if (!nrow(bruks)) return(tibble())
  bruks %>% mutate(fylke = fylke, herad = herad,
                   gaard_nr = hd[1], gaard_name = hd[2], src = url)
}

safe <- function(x) str_replace_all(x, "[^A-Za-z0-9]+", "_")

cs <- get_links(base, "task=h&fnr=\\d+$")
for (i in seq_len(nrow(cs))) {
  if (!is.null(target_fylke) && cs$name[i] != target_fylke) next
  message("[", i, "/", nrow(cs), "] ", cs$name[i])
  hs <- get_links(cs$url[i], "task=h&fnr=\\d+&hid=\\d+")
  for (j in seq_len(nrow(hs))) {
    if (!is.null(target_herad) && hs$name[j] != target_herad) next
    fname <- file.path(out_dir, paste0(safe(cs$name[i]), "_", safe(hs$name[j]), ".csv"))
    if (file.exists(fname)) { message("  skip ", hs$name[j]); next }
    message("  ", hs$name[j])
    gs <- get_links(hs$url[j], "task=s&fnr=\\d+&hid=\\d+&gnr=\\d+")
    all <- tibble()
    for (k in seq_len(nrow(gs))) {
      t <- tryCatch(parse_gaard(gs$url[k]), error = function(e) tibble())
      if (nrow(t)) all <- bind_rows(all, t)
    }
    if (nrow(all)) write_excel_csv(all, fname) # make sure encoding is correct 
  }
}