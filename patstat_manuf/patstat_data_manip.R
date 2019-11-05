library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(tibble)
library(countrycode)

## SQLs used are in the file wiod_data_prep.sql

rm(list=ls())

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

## adding a new column tech.level and mapping accorging to nace2 code.
p.raw %<>% mutate(tech.level = ifelse(nace2_code >= 10 & nace2_code <= 18, "Low Tech",
                                     ifelse(nace2_code == 19, "Medium-Low Tech",
                                     ifelse(nace2_code == 20, "Medium-High Tech",
                                     ifelse(nace2_code == 21, "High Tech",
                                     ifelse(nace2_code >= 22 & nace2_code <= 25, "Medium-Low Tech",
                                     ifelse(nace2_code == 26, "High Tech",
                                     ifelse(nace2_code >= 27 & nace2_code <= 30, "Medium-High Tech",
                                     ifelse(nace2_code >= 31 & nace2_code <= 33, "Low Tech", NA)))))))))


## adding country3 column; 3 char country codes. WIOD uses 3 char.
## the aim is to join this dataframe with the one obtaine from WIOD.
p.raw$country3  <- countrycode(p.raw$ctry_code, 'iso2c', 'iso3c')
p.raw$ctry_code <- NULL
p.raw <- ungroup(p.raw)

p.raw <- p.raw %>% select(country3, nace2_code, tech.level, appln_filing_year, patent_sum)

p.raw %>% select(nace2_code) %>% arrange(nace2_code) %>% unique %>% as.data.frame

head(p.raw)

wiod.org <- get(load("/tmp/trial/wiod_original_data/WIOT2003_October16_ROW.RData"))
wiod.nace <- wiod.org %>% filter(RNr < 57) %>% select(IndustryCode) %>% unique

wiod.nace


industry.RNr %<>% mutate(IndustryCode = ifelse(!is.na(tech.type),
                                               tech.type, IndustryCode)) 


p.raw %<>% mutate(isic0 = ifelse((nace2_code >=10 & nace2_code <= 12), "C10-C12", 
                         ifelse((nace2_code >= 13 & nace2_code <= 15), "C13-C15",
                         ifelse((nace2_code == 16), "C16",
                         ifelse((nace2_code == 17), "C17",
                         ifelse((nace2_code == 18), "C18",
                         ifelse((nace2_code == 19), "C19",
                         ifelse((nace2_code == 20), "C20",
                         ifelse((nace2_code == 21), "C21",
                         ifelse((nace2_code == 22), "C22",
                         ifelse((nace2_code == 23), "C23",
                         ifelse((nace2_code == 24), "C24",
                         ifelse((nace2_code == 25), "C25",
                         ifelse((nace2_code == 26), "C26",
                         ifelse((nace2_code == 27), "C27",
                         ifelse((nace2_code == 28), "C28",
                         ifelse((nace2_code == 29), "C29",
                         ifelse((nace2_code == 30), "C30",
                         ifelse((nace2_code >= 31 & nace2_code <= 32), "C31_C32",
                         ifelse((nace2_code == 33), "C33",
                         ifelse((nace2_code == 35), "D35",
                         ifelse((nace2_code == 36), "E36",
                         ifelse((nace2_code >= 37 & nace2_code <= 39), "C37-C39",
                         ifelse((nace2_code >= 41 & nace2_code <= 43), "F",
                         ifelse((nace2_code == 45), "G45",
                         ifelse((nace2_code == 62), "J62_J63", NA))))))))))))))))))))))))))

## creating the country.ind column by joining 3 char counry code and technology level
##p.raw %<>% unite(country.ind, country3, tech.level, sep=".") %>% as.data.frame

p.raw %<>% mutate(isic1 = str_sub(isic0, start = 1, end = 1))

p.raw %<>% mutate(isic2 = ifelse(is.na(tech.level), isic0, tech.level)) %>% as.data.frame


patstat.isic.0  <- p.raw %>% unite("country.ind", "country3", "isic0", sep=".") %>%
    select(country.ind, appln_filing_year, patent_sum) %>%
    group_by(country.ind, appln_filing_year) %>% summarise(patent_num = sum(patent_sum))

write.csv(patstat.isic.0, "country_ind_yearly_pat_sum_isic_0.csv", row.names=FALSE)


patstat.isic.1  <- p.raw %>% unite("country.ind", "country3", "isic1", sep=".") %>%
    select(country.ind, appln_filing_year, patent_sum) %>%
    group_by(country.ind, appln_filing_year) %>% summarise(patent_num = sum(patent_sum))

write.csv(patstat.isic.1, "country_ind_yearly_pat_sum_isic_1.csv", row.names=FALSE)


patstat.isic.2 <- patstat.isic.1  <- p.raw %>% unite("country.ind", "country3", "isic2", sep=".") %>%
    select(country.ind, appln_filing_year, patent_sum) %>%
    group_by(country.ind, appln_filing_year) %>% summarise(patent_num = sum(patent_sum))

write.csv(patstat.isic.2, "country_ind_yearly_pat_sum_isic_2.csv", row.names=FALSE)




