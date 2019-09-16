library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(tibble)
library(countrycode)

## SQLs used are in the file wiod_data_prep.sql

## this raw data is obtained from PATSTAT database
patstat.raw <- read.csv("patstat_pct_ctry_ind_year.csv", skip=2, sep = "|",
                        strip.white = TRUE, stringsAsFactors = FALSE)

## changing the names of the columns
col.names <- c("ctry_code", "nace2_code", "nace2_descr",
"nace2_weight", "appln_filing_year", "sum_of_country_share")

names(patstat.raw) <- col.names

## obtaining nace dataframe
nace.df <- patstat.raw %>% select(nace2_code, nace2_descr) %>% unique

patstat.raw %<>% select(-nace2_descr)

patstat.raw$nace2_code <- as.integer(patstat.raw$nace2_code)

## PATSTAT data is not always precise
patstat.raw$ctry_code[patstat.raw$ctry_code == ""] <- NA

## summing up the data obtained by group_by values
patstat.raw %<>% group_by(ctry_code, nace2_code, appln_filing_year) %>%
    summarise(patent_sum = sum(sum_of_country_share))

## using only complete cases no NA is accepted
p.raw <- patstat.raw[complete.cases(patstat.raw), ]

## obtaining only manufactuing nace2_code
p.raw.manuf <- p.raw %>% filter(nace2_code >= 10 & nace2_code <= 33)

## adding a new column tech.level and mapping accorging to nace2 code.
p.raw.manuf %<>% mutate(tech.level = ifelse(nace2_code >= 10 & nace2_code <= 18, "Low Tech",
                                     ifelse(nace2_code == 19, "Medium-Low Tech",
                                     ifelse(nace2_code == 20, "Medium-High Tech",
                                     ifelse(nace2_code == 21, "High Tech",
                                     ifelse(nace2_code >= 22 & nace2_code <= 25, "Medium-Low Tech",
                                     ifelse(nace2_code == 26, "High Tech",
                                     ifelse(nace2_code >= 27 & nace2_code <= 30, "Medium-High Tech",
                                     ifelse(nace2_code >= 31 & nace2_code <= 33, "Low Tech", NA)))))))))


## adding country3 column; 3 char country codes. WIOD uses 3 char.
## the aim is to join this dataframe with the one obtaine from WIOD.
p.raw.manuf$country3  <- countrycode(p.raw.manuf$ctry_code, 'iso2c', 'iso3c')

## creating the country.ind column by joining 3 char counry code and technology level
p.raw.manuf %<>% unite(country.ind, country3, tech.level, sep=".") %>% as.data.frame

names(p.raw.manuf)

p.raw.manuf %<>% select(country.ind, appln_filing_year, patent_sum)

## summing up all pct count wrt tech level
patstat.manuf.df <- p.raw.manuf %>% group_by(country.ind, appln_filing_year) %>%
    summarise(pat_tech_sum = sum(patent_sum))

patstat.manuf.df <- as.data.frame(patstat.manuf.df)

write.csv(patstat.manuf.df, "country_ind_yearly_pat_tech_sum.csv", row.names=FALSE)




