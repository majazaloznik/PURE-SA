###############################################################################
## Data Clean
###############################################################################
## 0. Prelim
## 1. Import and  merge files
##    1.1 Summary of files
##    1.2 Check number of rows - there are clearly issues - before merging
## 2.
###############################################################################


###############################################################################
## 0. Prelim
###############################################################################
library(memisc)
library(dplyr)
library(tidyr)
options(stringsAsFactors = FALSE)


###############################################################################
## 1. Import and  merge files
###############################################################################

filenames <- list.files("data/raw-data", pattern="*.sav", full.names=TRUE)
raw.import <- lapply(filenames, function(x) data.frame(as.data.set(spss.system.file(x))))
## there are some 'empty varz' here,  need to be removed
raw.import <- lapply(raw.import, function(el) el[sapply(el, function(x) !all(is.na(x)))])

## 1.1 Summary of files
###############################################################################
# var.list.with.repetitions <- unlist(sapply(no.missing.import, colnames))
# FunNumberUniqueVarz <- function(x){
#   sum(table(var.list.with.repetitions)[names(table(var.list.with.repetitions)) %in% names(x)]==1)
# }

file.list <- data.frame(file.name = substr(filenames, 15, 200),
                        no.varz.original = sapply(raw.import, length),
                        no.cases = sapply(raw.import, nrow))

write.csv(file.list, "data/outputs/file.list.csv") # see below
rm(file.list, filenames)
## 1.2 Check number of rows - there are clearly issues - before merging
###############################################################################

## tables 3 and 4 (2005 and 2010 demographic:)
nrow( raw.import[[3]])
nrow( raw.import[[4]])
nrow(full_join(raw.import[[3]], raw.import[[4]]))
# see, there are 2040 merged rows, because barcode, location and gender are used 
# to merge. even though barcode should be unique, and there shoud not be more 
# people in the 2010 wave.

# Then there is 1011, who has no data 4453 and 4454 only location 
# actually there are 11 of them:
raw.import[[3]][is.na(raw.import[[3]][,3]),]$barcode
# so we remove these eleven
raw.import[[3]] <- raw.import[[3]][!is.na(raw.import[[3]][,3]),]

# indicators for individual waves
raw.import[[3]]$wave.1 <- 1
raw.import[[4]]$wave.2 <- 1
# in 2010 furthermore there is at least one barcode input error, #13020, also needs to be removed.
# so to het rid of the 20 additional cases in 2010 we use a left join on barcode only
left_join(raw.import[[3]], raw.import[[4]], by = "barcode", 
          suffix = c("_2005", "_2010"))  -> fd

# the 13020 case occurs in several other files, let's just add that to the list
# file.list <- cbind(file.list, no.13020 = sapply(raw.import, 
#                                                function(x) any(x$barcode == 13020)))



## table 5 (2015 demographic)
# first label location and gender
raw.import[[5]]$gender <- factor(raw.import[[5]]$gender,
                                        labels = c("Male", "Female"))
raw.import[[5]]$location <- factor(raw.import[[5]]$location,
                                        labels = c( "Urban"))
# there are two duplicates in this file, remove
raw.import[[5]] <- raw.import[[5]][-which(duplicated(raw.import[[5]]$barcode)),]

raw.import[[5]] %>% 
  mutate(has.age_2015  = ifelse(is.na(age_2015), 0, 1)) %>% 
  filter(has.age_2015 == 0) %>% 
  filter(is.na(location)) %>% 
  pull(barcode) 
# there are four cases with no 2015 updates on demographic/life, thse are removed
raw.import[[5]] <- raw.import[[5]][!is.na(raw.import[[5]]$age_2015) | !is.na(raw.import[[5]]$location),]

# add a indicator for the case being in 2015 wave
raw.import[[5]]$wave.3 <- 1

# merge 
full_join(fd, raw.import[[5]], by = "barcode") %>% 
  rename(location_2015 = location, gender_2015 = gender)-> fd

# How many are rural 2015 missing demogaphic questions? 539 have age_2015, but no location 
# and another 4 that don't have age, but have the diet questions  926 are unique 2015 rows
# How many are rural 2015 missing demogaphic questions? 539 have no location
table(as.character(raw.import[[5]]$location), useNA = "always" )

# NUMBER IN EACH WAVE
fd$wave.1[is.na(fd$wave.1)] <- 0
fd$wave.2[is.na(fd$wave.2)] <- 0
fd$wave.3[is.na(fd$wave.3)] <- 0
margin.table(xtabs(~ wave.2+ wave.3, data = fd))
margin.table(xtabs(~ wave.2+ wave.3, data = fd), 1)
margin.table(xtabs(~ wave.2+ wave.3, data = fd), 2)


## table 6 (2005 household income) - 2020 cases, that's 10 too many. 
# full_join(fd, raw.import[[6]], by = "barcode", suffix = c("", "new")) -> fd
# all.equal(fd$month_hh_income_2005.x, fd$month_hh_income_2005.y)
## correctly merged already, double checked. 


# # ## table 7 (2005 life events) - 2010 cases good
# full_join(fd, raw.import[[7]], by = "barcode", suffix = c("", "_new")) -> fd
# for (i in 2:length(raw.import[[7]])){
# x <- all.equal(eval(parse(text=paste0("fd$",colnames(raw.import[[7]])[i]))),
# eval(parse(text=paste0("fd$",colnames(raw.import[[7]]), "_new")[i])))
# print(x)}
# ## correctly merged already, double checked. 


# # ## table 8 (2010 life events) - 1266 cases - one too many
# full_join(fd, raw.import[[8]], by = "barcode", suffix = c("", "_new")) -> fd
# fd <- fd[fd$barcode != 13020, ]
#  for (i in 2:length(raw.import[[8]])){
#  x <- all.equal(eval(parse(text=paste0("fd$",colnames(raw.import[[8]])[i]))),
#  eval(parse(text=paste0("fd$",colnames(raw.import[[8]]), "_new")[i])))
#  print(x)}
# # ## correctly merged already, double checked. 



# # ## table 9 (2015 life events) - 915 cases - short of 926!, but also two duplicates. 

## check duplicates are the same and delete them
# raw.import[[9]][raw.import[[9]]$barcode %in% raw.import[[9]]$barcode[which(duplicated(raw.import[[9]]))],]
raw.import[[9]] <- raw.import[[9]][-which(duplicated(raw.import[[9]])),]
full_join(fd, raw.import[[9]], by = "barcode", suffix = c("", "_new")) -> fd



