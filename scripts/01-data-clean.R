###############################################################################
## Data Clean
###############################################################################
## 0. Prelim
## 1.
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
just.filenames <- substr(filenames, 15, 200)
raw.import <- lapply(filenames, function(x) data.frame(as.data.set(spss.system.file(x))))
## there are some 'empty varz' here,  need to be removed
no.missing.import <- lapply(raw.import, function(el) el[sapply(el, function(x) !all(is.na(x)))])

## 1.1 Summary of files
###############################################################################
# var.list.with.repetitions <- unlist(sapply(no.missing.import, colnames))
# FunNumberUniqueVarz <- function(x){
#   sum(table(var.list.with.repetitions)[names(table(var.list.with.repetitions)) %in% names(x)]==1)
# }

file.list <- data.frame(file.name = just.filenames,
                        no.varz.original = sapply(raw.import, length),
                        no.varz.NA.removed = sapply(no.missing.import, length),
                        no.cases = sapply(raw.import, nrow))
write.csv(file.list, "data/outputs/file.list.csv")

## 1.2 Check number of rows - there are clearly issues - before merging
###############################################################################

## tables 3 and 4 (2005 and 2010 demographic:)
nrow( no.missing.import[[3]])
nrow( no.missing.import[[4]])
nrow(full_join(no.missing.import[[3]], no.missing.import[[4]]))
# see, there are 2040 merged rows, because barcode, location and gender are used 
# to merge. even though barcode should be unique, and there shoud not be more 
# people in the 2010 wave.

## there are 19 too many
full_join(no.missing.import[[3]], no.missing.import[[4]]) %>% 
dplyr::select( gender, location , barcode) ->  extra.2010
extra.2010[duplicated(extra.2010$barcode),] -> extra.2010
# here are the ones that only have missing gender and location data. 
extra.2010$barcode[is.na(extra.2010$gender)]
# the other three have changed in 2010, but change back in 2015, so presumably 
# they are typos. 
# there is at least one barcode input error, #13020, removed. 
file.list <- cbind(file.list, no.13020 = sapply(no.missing.import, function(x) any(x$barcode == 13020)))

# but this needs to be removed, so use left join here instead of full.       
# similarly, we ignore the 3 gender changes and 16 missing genders
left_join(no.missing.import[[3]], no.missing.import[[4]], by = "barcode", 
                  suffix = c("_2005", "_2010"))  -> full.data

## table 5 (2015 demographic)
# first label location and gender
no.missing.import[[5]]$gender <- factor(no.missing.import[[5]]$gender,
                                        labels = c("Male", "Female"))
no.missing.import[[5]]$location <- factor(no.missing.import[[5]]$location,
                                        labels = c( "Urban"))
nrow( no.missing.import[[4]])
nrow( no.missing.import[[5]])
full_join(no.missing.import[[4]], no.missing.import[[5]], by = "barcode") -> test

full_join(no.missing.import[[4]], no.missing.import[[5]]) %>% 
  dplyr::select( gender, location , barcode) ->  extra.2015
extra.2015[duplicated(extra.2015$barcode),] -> extra.2015
# four cases change gender between 2010 and 2015 - two we already had before, two are new
extra.2015[extra.2015$barcode %in% extra.2015$barcode[!is.na(extra.2015$gender)],]
# These others are cases that don't have any 2015 data NAs all the way down. 492 of them 
nrow(extra.2015[is.na(extra.2015$gender),])
# Are they the rural ones?
no.missing.import[[4]][no.missing.import[[4]]$barcode %in% extra.2015$barcode[is.na(extra.2015$gender)],]

no.missing.import[[3]][no.missing.import[[3]]$barcode == 2258,]
no.missing.import[[4]][no.missing.import[[4]]$barcode == 2258,]
no.missing.import[[5]][no.missing.import[[5]]$barcode == 2258,]
