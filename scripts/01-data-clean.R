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
                        no.cases = sapply(raw.import, nrow),
                        no.varz.used = NA,
                        no.cases.used = NA)



## 1.2 Check number of rows - there are clearly issues - before merging
###############################################################################

## tables 3 and 4 (2005 and 2010 demographic:)
###############################################################################
nrow( raw.import[[3]])
nrow( raw.import[[4]])
nrow(full_join(raw.import[[3]], raw.import[[4]]))
# see, there are 2040 merged rows, because barcode, location and gender are used 
# to merge. even though barcode should be unique, and there shoud not be more 
# people in the 2010 wave.

# Then there is 1011, who has no data 4453 and 4454 only location 
# actually there are 11 of them, one is duplicatred (2184), the others empty
raw.import[[3]][is.na(raw.import[[3]][,3]),]$barcode
# so we remove these eleven
raw.import[[3]] <- raw.import[[3]][!is.na(raw.import[[3]][,3]),]

# there are 16 cases with no demographic data
# two are completely empty, one is 13020 and the other 13 are left in.
raw.import[[4]]$barcode[which(rowSums(is.na(raw.import[[4]][,2:24]))>=15)]
# in wave 2 there are also two with all missing:
raw.import[[4]] <- raw.import[[4]][-which(rowSums(is.na(raw.import[[4]][,2:24]))==22),]

# indicators for individual waves
raw.import[[3]]$ind.wave.1 <- 1
raw.import[[4]]$ind.wave.2 <- 1
# in 2010 furthermore there is at least one barcode input error, #13020, also needs to be removed.
# so  use a left join on barcode only
left_join(raw.import[[3]], raw.import[[4]], by = "barcode", 
          suffix = c("_2005", "_2010"))  -> fd
fd$ind.wave.2[is.na(fd$ind.wave.2)] <- 0
# nothing from first two tables
file.list[1:2,4:5] <- 0

# used cases and vars from tables 3 and 4 (barcode counted only once in 3.)
file.list[3,4:5] <- c(ncol(raw.import[[3]]), sum(fd$ind.wave.1))
file.list[4,4:5] <- c(ncol(raw.import[[4]])-1, sum(fd$ind.wave.2))

## table 5 (2015 demographic)
###############################################################################
# first label location and gender
raw.import[[5]]$gender <- factor(raw.import[[5]]$gender,
                                        labels = c("Male", "Female"))
raw.import[[5]]$location <- factor(raw.import[[5]]$location,
                                        labels = c( "Urban"))
# there are two duplicates in this file, remove
raw.import[[5]] <- raw.import[[5]][-which(duplicated(raw.import[[5]]$barcode)),]

# add a indicator for the case being in 2015 wave
raw.import[[5]]$ind.wave.3 <- 1

# merge 
full_join(fd, raw.import[[5]], by = "barcode") %>% 
  rename(location_2015 = location, gender_2015 = gender)-> fd
fd$ind.wave.3[is.na(fd$ind.wave.3)] <- 0
# used cases and vars from table 5
file.list[5,4:5] <- c(ncol(raw.import[[5]])-1, sum(fd$ind.wave.3))


# How many are rural 2015 missing demogaphic questions? 543 have no location 
# four of these have no age_2015 either. 539? uncoded?
table(as.character(raw.import[[5]]$location), useNA = "always" )
table(raw.import[[5]]$location[!is.na(raw.import[[5]]$age_2015)], useNA = "always" )

# NUMBER IN EACH WAVE
margin.table(xtabs(~ ind.wave.2+ ind.wave.3, data = fd))
margin.table(xtabs(~ ind.wave.2+ ind.wave.3, data = fd), 1)
margin.table(xtabs(~ ind.wave.2+ ind.wave.3, data = fd), 2)


## table 6 (2005 household income) - 2020 cases, that's 10 too many. 
###############################################################################

# full_join(fd, raw.import[[6]], by = "barcode", suffix = c("", "new")) -> fd
# all.equal(fd$month_hh_income_2005.x, fd$month_hh_income_2005.y)
## correctly merged already, double checked. 

# used cases and vars from table 6
file.list[6,4:5] <- 0

# # ## table 7 (2005 life events) - 2010 cases good
###############################################################################

# full_join(fd, raw.import[[7]], by = "barcode", suffix = c("", "_new")) -> fd
# for (i in 2:length(raw.import[[7]])){
# x <- all.equal(eval(parse(text=paste0("fd$",colnames(raw.import[[7]])[i]))),
# eval(parse(text=paste0("fd$",colnames(raw.import[[7]]), "_new")[i])))
# print(x)}
# ## correctly merged already, double checked. 

# used cases and vars from table 7
file.list[7,4:5] <- 0


# # ## table 8 (2010 life events) - 1266 cases - one too many
###############################################################################
# full_join(fd, raw.import[[8]], by = "barcode", suffix = c("", "_new")) -> fd
# fd <- fd[fd$barcode != 13020, ]
#  for (i in 2:length(raw.import[[8]])){
#  x <- all.equal(eval(parse(text=paste0("fd$",colnames(raw.import[[8]])[i]))),
#  eval(parse(text=paste0("fd$",colnames(raw.import[[8]]), "_new")[i])))
#  print(x)}
# # ## correctly merged already, double checked. 

# used cases and vars from table 8
file.list[8,4:5] <- 0

# # ## table 9 (2015 life events) - 915 cases - short of 930? 
###############################################################################
## check duplicates are the same and delete them - two 
# raw.import[[9]][raw.import[[9]]$barcode %in% raw.import[[9]]$barcode[which(duplicated(raw.import[[9]]))],]
raw.import[[9]] <- raw.import[[9]][-which(duplicated(raw.import[[9]])),]
## seven cases have all values missing, remove them here
raw.import[[9]] <-   raw.import[[9]][-which(rowSums(is.na(raw.import[[9]][,2:7]))==6),]
# add indicator for being in this table
raw.import[[9]]$ind.wave.3.tab.9 <- 1
# join with fd.
full_join(fd, raw.import[[9]], by = "barcode", suffix = c("", "_new")) -> fd

# used cases and vars from table 9
fd$ind.wave.3.tab.9[is.na(fd$ind.wave.3.tab.9)] <- 0
file.list[9,4:5] <- c(ncol(raw.import[[9]])-1, sum(fd$ind.wave.3.tab.9))

# there are be 24 cases from 5 that are not in 9
# actually there are 25
fd$barcode[which(fd$ind.wave.3 == 1 & fd$ind.wave.3.tab.9 ==0)]
# and one from 9 that is not in 5
fd$barcode[which(fd$ind.wave.3 == 0 & fd$ind.wave.3.tab.9 ==1)]


# # ## table 10 (2005 medical history) - 2021 cases, that'll be too many ;)
###############################################################################


# raw.import[[10]][which(rowSums(is.na(raw.import[[10]][,2:14]))==13),]
# add indicator variable
raw.import[[10]]$ind.wave.1.med.h <- 1

# remove the eleven with missing values
raw.import[[10]] <- raw.import[[10]][rowSums(is.na(raw.import[[10]][,2:14])) !=13,]

# join with fd. 
full_join(fd, raw.import[[10]], by = "barcode", suffix = c("", "_new")) -> fd
fd$wave.1.med.h[is.na(fd$ind.wave.1.med.h)] <- 0

# used cases and vars from table 10
file.list[10,4:5] <- c(ncol(raw.import[[10]])-1, sum(fd$ind.wave.1.med.h))



# # ## table 11 (2010 medical history) - 2035 cases, that'll be WAY too many ;)
###############################################################################
nrow(raw.import[[11]])
#remove ones with no data 
raw.import[[11]] <- raw.import[[11]][-which(rowSums(is.na(raw.import[[11]][,2:15]))==14),]

# also remove # 13020 
raw.import[[11]] <- raw.import[[11]][raw.import[[11]]$barcode != 13020,]

# add indicator variable
raw.import[[11]]$ind.wave.2.med.h <- 1

# join with fd. 
full_join(fd, raw.import[[11]], by = "barcode", suffix = c("", "_new")) -> fd

fd$ind.wave.2.med.h[is.na(fd$ind.wave.2.med.h)] <- 0

# used cases and vars from table 10
file.list[11,4:5] <- c(ncol(raw.import[[11]])-1, sum(fd$ind.wave.2.med.h))


# # ## table 12 (2015 medical history) - 901 cases
###############################################################################
## Two duplicates 
raw.import[[12]] <- raw.import[[12]][!duplicated(raw.import[[12]]$barcode),]

## 2241 is a total dulicate, 2285 has different hypertension, so set to NA
raw.import[[12]][raw.import[[12]]$barcode == 2258, 'hypertension_2015'] <- NA

# add indicator variable
raw.import[[12]]$ind.wave.3.med.h <- 1

# join with fd. 
full_join(fd, raw.import[[12]], by = "barcode", suffix = c("", "_new")) -> fd
fd$ind.wave.3.med.h[is.na(fd$ind.wave.3.med.h)] <- 0

# used cases and vars from table 12
file.list[12,4:5] <- c(ncol(raw.import[[12]])-1, sum(fd$ind.wave.3.med.h))

# also  there are 31 cases with wave 3 demographic data, but no medical history data
fd$barcode[fd$ind.wave.3 == 1 & fd$ind.wave.3.med.h == 0]



# # ## table 13 (HIV status) - 923 
###############################################################################
# no duplicates 
# none with 3 missing values, good to merge
# add indicator variable
raw.import[[13]]$ind.hiv.testing <- 1

# join with fd. 
full_join(fd, raw.import[[13]], by = "barcode", suffix = c("", "_new")) -> fd
fd$ind.hiv.testing[is.na(fd$ind.hiv.testing)] <- 0

# used cases and vars from table 12
file.list[13,4:5] <- c(ncol(raw.import[[13]])-1, 
                       sum(fd$ind.hiv.testing))



# # ## table 14 (Physical measurements) - 2010
###############################################################################
# no duplicates 
# two with completely missing values 
raw.import[[14]] <- raw.import[[14]][which(rowSums(is.na(raw.import[[14]][,2:10])) != 9),]
# add indicator variable
raw.import[[14]]$ind.bmi <- 1
# join with fd. 
full_join(fd, raw.import[[14]], by = "barcode", suffix = c("", "_new")) -> fd
fd$ind.bmi[is.na(fd$ind.bmi)] <- 0

# used cases and vars from table 12
file.list[14,4:5] <- c(ncol(raw.import[[14]])-1, sum(fd$ind.bmi))




## 1.3    CLEAN weird characters
###############################################################################
fd$month_hh_income_2005 <- gsub('â€™', "'", fd$month_hh_income_2005) 


## 2. EXPORT DATA AND SUMMARY TABLE
###############################################################################
write.csv(fd, "data/outputs/clean.fd.csv",  row.names = FALSE)


## SUMMARY TABLE
###############################################################################
# summary of files - list and merged case and variable numbers
file.list <- rbind(file.list, list("Total", sum(file.list$no.varz.original), 
                                   max(file.list$no.cases), 
                                   sum(file.list$no.varz.used, na.rm = TRUE), 
                                   max(file.list$no.cases.used)))
write.csv(file.list, "data/outputs/file.list.csv")


