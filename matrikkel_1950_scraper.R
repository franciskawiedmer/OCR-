# load libraries 
library(rvest) # html parsing 
library(dplyr)
library(stringr)
library(readr) # csv
library(purrr)

base   <- "https://www.dokpro.uio.no/cgi-bin/stad/matr50.cgi?task=h"
domain <- "https://www.dokpro.uio.no"
out    <- "matrikkel_1950.csv"

fetch <- function(u) { Sys.sleep(0.3); read_html(u) } # wait three seconds 
absu  <- function(h) if (startsWith(h, "http")) h else paste0(domain, h) # turns it into full url

# return full url 
get_links <- function(url, pat) {
  a <- fetch(url) %>% html_elements("a")
  h <- html_attr(a, "href")
  k <- !is.na(h) & str_detect(h, pat)
  tibble(name = str_trim(html_text(a[k])), url = map_chr(h[k], absu)) %>% distinct()
}

# table parser 
parse_gaard <- function(url) {
  page  <- fetch(url)
  fylke <- page %>% html_element("h2") %>% html_text() %>% str_trim()
  herad <- page %>% html_element("h3") %>% html_text() %>% str_trim() %>% str_remove("\\s*herad\\s*$")
  rows  <- page %>% html_elements("table tr")
  if (length(rows) < 2) return(tibble())
  hd <- rows[[1]] %>% html_elements("td") %>% html_text() %>% str_trim()
  gaard_nr   <- hd[1]
  gaard_name <- hd[2]
  bruks <- map(rows[-1], function(r) {
    c <- r %>% html_elements("td") %>% html_text() %>% str_squish()
    if (length(c) < 5) return(NULL)
    tibble(bruk_nr = c[1], bruk_name = c[2], skyld = c[3], owner = c[4], comment = c[5])
  }) %>% compact() %>% bind_rows()
  if (!nrow(bruks)) return(tibble())
  bruks %>% mutate(fylke = fylke, herad = herad,
                   gaard_nr = gaard_nr, gaard_name = gaard_name, src = url)
}

done <- if (file.exists(out)) unique(read_csv(out, show_col_types = FALSE)$src) else character()
all  <- if (file.exists(out)) read_csv(out, show_col_types = FALSE) else tibble()

# loops over counties and herads 
cs <- get_links(base, "task=h&fnr=\\d+$")
for (i in seq_len(nrow(cs))) {
  message("[", i, "/", nrow(cs), "] ", cs$name[i])
  hs <- get_links(cs$url[i], "task=h&fnr=\\d+&hid=\\d+")
  for (j in seq_len(nrow(hs))) {
    message("  ", hs$name[j])
    gs <- get_links(hs$url[j], "task=s&fnr=\\d+&hid=\\d+&gnr=\\d+")
    for (k in seq_len(nrow(gs))) {
      if (gs$url[k] %in% done) next
      t <- tryCatch(parse_gaard(gs$url[k]), error = function(e) tibble())
      if (nrow(t)) all <- bind_rows(all, t)
      done <- c(done, gs$url[k])
    }
    write_csv(all, out)
  }
}
