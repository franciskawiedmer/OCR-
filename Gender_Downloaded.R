###################### Step 1 #######################################################
# installing the package : https://cran.r-project.org/web/packages/gender/readme/README.html
# install.packages("gender")
# remotes::install_github("lmullen/genderdata")


##################### Step 2 #############################################################
# Reading the data set 
library(readxl) # for excel files 
df <- read.csv("Webscraping 1950\\GoodCadasters\\02 Akershus 1838.csv")# read in data file 
colnames(df)
summary(df)
# for 1838: using DEC_SKYLL

# removing columns that are not needed 
remove1 <- c("check", "FYLKE","KNR", "GMNR", "DALER", "ORT", "SKILL", "skyld_dec",  "GSKYLD", "FYLKENAVN", "EID", "BILDENAVN")
df <- df %>% select(-any_of(remove1))
summary(df)
df$DEC_SKILL


###################### Step 3 ############################################################
# what if there are multiple owners with different genders? => Split them and create rows ()
# finds EIER columns (Vorname) 
eier <- grep("^EIER", names(df), value = TRUE)
eier # check 
# NA becomes nothing 
# no stray spaces 
df[eier] <- lapply(df[eier], function(x) trimws(replace(x, is.na(x), "")))
eier
# which owner slots are actually occupied? 
has <- df[paste0("EIER", 1:3, "_FORNAVN")] != "" | df[paste0("EIER", 1:3, "_ENAVN")] != ""
has # table looks ok 
has[rowSums(has) == 0, 1] <- TRUE

n    <- unname(rowSums(has))
idx  <- rep(seq_len(nrow(df)), n) # repeats rows as many times as there are owners 

slot <- unlist(lapply(asplit(has, 1), which))


df$DEC_SKILL <- as.numeric(df$DEC_SKILL) # making sure it is numeric 
df <- df[idx, ] # makes copy 

df$DEC_SKILL <- df$DEC_SKILL / n[idx]
for (i in 1:3) df[slot != i, paste0("EIER", i, c("_FORNAVN", "_ENAVN"))] <- ""
df$ANMERKN <- trimws(paste0(df$NOTE, ifelse(n[idx] > 1, paste0(" [skyld split among ", n[idx], " owners]"), "")))
# check number of rows 
nrow(df)   # should be 6518, not 6265


#################### Step 4 ##############################################################
# now see which name ends with datter 
eier <- grep("^EIER", names(df), value = TRUE)
df[eier][is.na(df[eier])] <- ""

df$woman <- as.integer(
  grepl("datter|dotter|dtr", df$EIER1_FORNAVN, ignore.case = TRUE) |
    grepl("datter|dotter|dtr", df$EIER1_ENAVN,   ignore.case = TRUE) |
    grepl("datter|dotter|dtr", df$EIER2_FORNAVN, ignore.case = TRUE) |
    grepl("datter|dotter|dtr", df$EIER2_ENAVN,   ignore.case = TRUE) |
    grepl("datter|dotter|dtr", df$EIER3_FORNAVN, ignore.case = TRUE) |
    grepl("datter|dotter|dtr", df$EIER3_ENAVN,   ignore.case = TRUE)
)

table(df$woman)

####################### Step 5 ##########################################################
# see which words end with enke 
df$woman[
  grepl("enke", df$EIER1_FORNAVN, ignore.case = TRUE) |
    grepl("enke", df$EIER1_ENAVN,   ignore.case = TRUE) |
    grepl("enke", df$EIER2_FORNAVN, ignore.case = TRUE) |
    grepl("enke", df$EIER2_ENAVN,   ignore.case = TRUE) |
    grepl("enke", df$EIER3_FORNAVN, ignore.case = TRUE) |
    grepl("enke", df$EIER3_ENAVN,   ignore.case = TRUE) |
    grepl("enke", df$ANMERKN,       ignore.case = TRUE)
] <- 1

table(df$woman)

###################### Step 6 #######################################################
# use the gender data package 
library(gender)
library(genderdata)

first <- sub("^[^[:alpha:]]*([[:alpha:]]+).*", "\\1",
             trimws(paste(df$EIER1_FORNAVN, df$EIER2_FORNAVN, df$EIER3_FORNAVN)))

g   <- gender(unique(tolower(first[nzchar(first)])), years = 1903, method = "napp")
fem <- tolower(first) %in% g$name[g$gender == "female"]

df$woman[df$woman == 0 & fem] <- 1L
table(df$woman)


#################### Step 7 ######################################################
# use the name list 
first <- sub("^[^[:alpha:]]*([[:alpha:]]+).*", "\\1",
             trimws(paste(df$EIER1_FORNAVN, df$EIER2_FORNAVN, df$EIER3_FORNAVN)))

fem_list <- tolower(trimws(readLines("Landinequality\\Norway_female_names.txt", encoding = "UTF-8")))
fem_list <- fem_list[nzchar(fem_list)]

before <- df$woman
df$woman[df$woman == 0 & tolower(first) %in% fem_list] <- 1L

table(df$woman)

################# Step 8 ############################################################
# how to mark organisationssss? 
#allnames <- trimws(paste(df$EIER1_FORNAVN, df$EIER1_ENAVN, df$EIER2_FORNAVN,
                        # df$EIER2_ENAVN, df$EIER3_FORNAVN, df$EIER3_ENAVN))

#inst <- grepl("kommun|sogneprest|kanalv|forening|& co|meieri|sparebank|aktie|kirke",
             # allnames, ignore.case = TRUE)

#df$woman[inst] <- 0L
#table(df$woman)



###################### Main Calculation #################################################
tot   <- sum(df$DEC_SKILL, na.rm = TRUE) # sums up the total DEC_SKILL
tot

women <- sum(df$DEC_SKILL[df$woman == 1], na.rm = TRUE) # looks at the share of women 
women

share <- (women / tot) * 100
share




