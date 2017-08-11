###############################################################################
## OVERVIEW 
###############################################################################
library(memisc)
library(dplyr)
library(tidyr)
options(stringsAsFactors = FALSE)


## Import combined dataset from SPSS datafile
combined.dataset <- data.frame(as.data.set(spss.system.file(
  "../data/PURE/data-raw/Combined data 2005_2010_2015.sav")))

# there is an apostrophe in the 2005 income question
combined.dataset$month_hh_income_2005 <- as.character(combined.dataset$month_hh_income_2005)
combined.dataset$month_hh_income_2005 <- gsub('â€™', "'", combined.dataset$month_hh_income_2005) 

## codebook summary
combined.summary <- codebook(combined.dataset)

## tidy table of variable/year with percent missing in each cell
data.frame(original.varname = colnames(combined.dataset)) %>% 
  separate(original.varname, sep =-5, c("variable", "year")) %>% 
  separate(variable, sep = -2, c("variable", "sep")) %>% 
  select(-sep) %>%
  mutate(variable = ifelse(variable == "ba", "barcode", 
                           ifelse(variable == "loc", "location", 
                                  ifelse(variable == "g", "gender", variable))),
         year = ifelse(variable %in% c("barcode", "location", "gender"), 
                       2005, year)) %>% 
  mutate(missing = 100*sapply(combined.summary, function(x) if (x@spec == "double") 
    x@stats$descr[7] else  x@stats$NAs)/2025 ) %>% 
  mutate(missing = ifelse(variable == "barcode",0, missing)) %>% 
  rbind(c("barcode", 2010, 0), c("barcode", 2015, 0)) %>% 
  spread(year, missing, fill = NA)  -> combined.variables

write.csv(combined.variables, "data/outputs/combined.variables.missing.csv")

save(combined.summary, file="data/outputs/combined.variables.codebook.RData")
