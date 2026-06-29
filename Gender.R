
###################### Step 1 #######################################################
# installing the package : https://cran.r-project.org/web/packages/gender/readme/README.html
# install.packages("gender")
# remotes::install_github("lmullen/genderdata")

###################### Step 2 #########################################################
# Reading the data set 
library(readxl)
df <- read_excel("Smaalenenes_1903_Excel_Correction.xlsx")# read in data file 

# create necessary headers 
df$year <- 1903
df$amt <- "Smaalenenes"
colnames(df)

###################### Step 3 ############################################################
# indicate dummy whether it ends with datter 
df$woman <- as.integer(grepl("datter|dtr\\.", df$eier_bruker, ignore.case = TRUE))

# check: 
table(df$woman)

###################### Step 4 ###########################################################
# indicate dummy whether there is enke in the name column or in anmerkn
df$woman[grepl("enke", df$eier_bruker, ignore.case = TRUE) |
           grepl("enke", df$anmerkn,     ignore.case = TRUE)] <- 1
# check 
table(df$woman)

##################### Step 5 ###############################################################
# use genderdata package in R 
# choose method: napp (North Atlantic Population Project)
# more details here: https://github.com/lmullen/genderdata/blob/master/R/gender-data.R
# With Data from Germany, Iceland, Norway, and Sweden from 1801 to 1910 which is method <- "napp"
library(gender)
library(genderdata)
first <- sub("^[^[:alpha:]]*([[:alpha:]]+).*", "\\1", trimws(df$eier_bruker))   # first given name
g <- gender(unique(first), years = 1903, method = "napp")            # napp = covers Norway
fem <- first %in% g$name[g$gender %in% "female"]
df$woman[df$woman == 0 & fem] <- 1L # adding it back to the df 
table(df$woman)

##################### Step 6 #######################################################################
# all names that are in this document: https://github.com/stcoats/Nordic-Name-Data/blob/master/README.md are female 
# check for them directly 

fem_list <- tolower(trimws(readLines("Norway_female_names.txt", encoding = "UTF-8")))
fem_list <- fem_list[nzchar(fem_list)]                       # drop blank lines

before <- df$woman
df$woman[df$woman == 0 & tolower(first) %in% fem_list] <- 1L
table(df$woman)
unique(first[before == 0 & df$woman == 1]) 

table(df$woman)

################## Step 7 ############################################################
# with patronymics : https://www.norwaydna.no/gedcoms-and-genealogy/norwegian-names-en/




