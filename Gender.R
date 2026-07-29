
#################### Cleaning data ################################################################
df <- read_excel("Landinequality\\Smaalenenes_1903_Excel_Correction.xlsx") # read in scraped names
remove <- (c("gaards_no_raw", "brugs_no_raw", "check"))
library(tidyverse)
df <- df %>% select(-any_of(remove))
colnames(df)

# fill up "do" names (Important!!!)
for (i in 2:nrow(df)) {
  if (grepl("^\\s*do\\.?\\s*$", df$eier_bruker[i], ignore.case = TRUE)) {
    df$eier_bruker[i] <- df$eier_bruker[i - 1]
  }
}

# remove overfort and sogn 
x <- trimws(df$eier_bruker)
junk <- grepl("sogn", x, ignore.case = TRUE) |
  grepl("overf(ø|o)r", x, ignore.case = TRUE)

unique(x[junk]) # check whether it is correct 
df <- df[!junk, ]

# build mark ore column 
num <- function(x) {
  x <- gsub("[^0-9]", "", trimws(as.character(x))) # keeps only numbers 
  x[is.na(x) | !nzchar(x)] <- "0"
  as.numeric(x)
}
df$markfinal <- num(df$mark) + num(df$ore) / 100

###### splitting up data 
x    <- trimws(replace(as.character(df$eier_bruker), is.na(df$eier_bruker), ""))  # converts names into vector
x

inst <- grepl("kommun|sogneprest|kanalv|forening|& co|meieri|sparebank|aktie|kirke",
              x, ignore.case = TRUE) # institution or person? 
inst# makes dummy variable 

parts <- strsplit(gsub("\\s+og\\s+", ", ", x, ignore.case = TRUE), "\\s*,\\s*") # word og is turned into a comma 
parts[inst] <- as.list(x[inst]) # puts the institution part back 


parts <- lapply(parts, function(v) {
  v <- trimws(v); v <- v[nzchar(v)]; if (!length(v)) return("") # cleans empty spaces 
  v <- sub("-?sønner$", "søn", v); v <- sub("-?døtre$", "datter", v) # should read as one name 
  en <- ifelse(grepl("\\s", v), sub(".*\\s", "", v), "")
  for (i in rev(seq_along(v)))
    if (!nzchar(en[i]) && i < length(v) && nzchar(en[i + 1])) v[i] <- paste(v[i], en[i + 1])
  v
})

n   <- lengths(parts)
idx <- rep(seq_len(nrow(df)), n)

df$NOTES <- NULL
df <- df[idx, ] # all other rows are carried along 
df$eier_single <- unlist(parts) # each row names only one owner
df$n_owners    <- n[idx]
df$institution <- as.integer(inst[idx])
df$markfinal   <- df$markfinal / n[idx] # divides the markfinal across owners , single-owners remain unchanged 
df$NOTES     <- trimws(paste0(df$NOTES,
                                ifelse(n[idx] > 1, paste0(" [skyld split among ", n[idx], " owners]"), ""))) # adding notes where there is a split 
rownames(df) <- NULL

nrow(df)
table(n)
summary(df$markfinal)


################################ Figuring out Gender ####################################
df$eier_single[is.na(df$eier_single)] <- ""
df$woman <- as.integer(grepl("datter|dotter|dtr", df$eier_single, ignore.case = TRUE)) # check datter 
df$woman[grepl("enke(?!mann)", df$eier_single, ignore.case = TRUE, perl = TRUE)] <- 1L # check enke
table(df$woman)
# use the gender data package 
# use genderdata package in R 
# choose method: napp (North Atlantic Population Project)
# more details here: https://github.com/lmullen/genderdata/blob/master/R/gender-data.R
# With Data from Germany, Iceland, Norway, and Sweden from 1801 to 1910 which is method <- "napp"
library(gender)
library(genderdata)
 
first <- sub("^[^[:alpha:]]*([[:alpha:]]+).*", "\\1", trimws(df$eier_single)) # focusing on first name 

g   <- gender(unique(tolower(first[nzchar(first)])), years = 1903, method = "napp")
fem <- tolower(first) %in% g$name[g$gender == "female"]

df$woman[df$woman == 0 & fem] <- 1L
table(df$woman)

# how many abbreviations like Chr.? 
sum(nchar(first) <= 2) # 1421 

# use the name list 
first <- sub("^[^[:alpha:]]*([[:alpha:]]+).*", "\\1", trimws(df$eier_single))

fem_list <- tolower(trimws(readLines("Landinequality\\Norway_female_names.txt", encoding = "UTF-8")))
fem_list <- fem_list[nzchar(fem_list)]

before <- df$woman
df$woman[df$woman == 0 & tolower(first) %in% fem_list] <- 1L

table(df$woman)
unique(first[before == 0 & df$woman == 1]) # which names did it spot? 

# how to mark organisationssss? 
#allnames <- trimws(paste(df$EIER1_FORNAVN, df$EIER1_ENAVN, df$EIER2_FORNAVN,
# df$EIER2_ENAVN, df$EIER3_FORNAVN, df$EIER3_ENAVN))

#inst <- grepl("kommun|sogneprest|kanalv|forening|& co|meieri|sparebank|aktie|kirke",
# allnames, ignore.case = TRUE)

#df$woman[inst] <- 0L
#table(df$woman)



#################################### Main Calculation #################################################
tot   <- sum(df$markfinal, na.rm = TRUE) # sums up the total DEC_SKILL
tot

women <- sum(df$markfinal[df$woman == 1], na.rm = TRUE) # looks at the share of women 
women

share <- (women / tot) * 100
share


