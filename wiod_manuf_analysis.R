## This file is made for the reproducibility of the results obtained in
## Kim, K., Özaygen, A. (2019) Analysis of the innovative capacity and
## the network position of national manufacturing industries in world
## production.


## installing the WIODnet package from github
library(devtools)
install_github("altay-oz/WIODnet")

library(WIODnet) ## developed by Ozaygen for this paper

library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(stargazer)
library(plm)
library(lmtest)
library(texreg)
library(Hmisc)

## getting wiod zip file and unzipping and obtaining yearly IO tables
getWIOD()

## making network calculations
netWIOD()

## getting the panel data with country-ind as unit analysis.
panelWIOD()

## getting the panel data with country as unit analysis.
getCountryWIOD()

## panel data are ready. 
wiod.manuf.df <- read.csv("wiod_manuf_net_panel_2000_2014.csv")
wiod.ctry.df <- read.csv("wiod_ctry_net_panel_2000_2014.csv")

## merging patent data
## reading the patent count data obtained from patstat
patstat.df <- read.csv("./patstat_manuf/country_ind_yearly_pat_tech_sum.csv",
                             stringsAsFactors = FALSE)

## then join it
wiod.pat.manuf <- left_join(wiod.manuf.df, patstat.df, 
                  by = c("country.ind" = "country.ind", "year" = "appln_filing_year"))

## changing all NAs in patent info (pat_tech_sum) into zeros.
wiod.pat.manuf$pat_tech_sum[is.na(wiod.pat.manuf$pat_tech_sum)]  <- 0

###### country data
## country.ind to country and sum
patstat.ctry.df  <- patstat.df %>% separate(country.ind, c("country", "ind"), 3) %>%
    select(-ind) %>% group_by(country, appln_filing_year) %>%
    summarise(pat_num = sum(pat_tech_sum))

wiod.pat.ctry <- left_join(wiod.ctry.df, patstat.ctry.df, 
                  by = c("country" = "country", "year" = "appln_filing_year"))

## changing all NAs in patent info (pat_num) into zeros.
wiod.pat.ctry$pat_num[is.na(wiod.pat.ctry$pat_num)]  <- 0

################ ################ ################ ################ ################ 
## starting the analysis.

## creating the output directories for figures and tables to be used in
## the article
figures.dir <- "./figures"
tables.dir <- "./tables"

dir.create(figures.dir)
dir.create(tables.dir)

## creating country and industry columns
wiod.pat.manuf %<>% mutate(country = str_sub(country.ind, 1, 3)) %>%
    mutate(industry = str_sub(country.ind, 5))

## filtering manufacturing industries
wiod.manuf.data  <- wiod.pat.manuf %>% filter(industry %in% c("Low Tech",
                                                         "Medium-Low Tech",
                                                         "Medium-High Tech",
                                                         "High Tech"))

## creating a dummy variable
wiod.manuf.data$industry <- factor(wiod.manuf.data$industry)

names(wiod.manuf.data)

## renaming all variabes in capital letters.
names(wiod.manuf.data) <- c("country.ind", "STRENGTH.ALL",
                            "STRENGTH.OUT", "STRENGTH.IN",
                            "BETWEENNESS", "PAGE.RANK", "EIGEN.CENT",
                            "DOM.OUT", "INT.OUT", "DOM.IN", "INT.IN",
                            "DOM.FINAL", "INT.FINAL", "VA", "year",
                            "INNOV.CAP", "country", "industry")

## adding STRENGTH.EFFiciency variable
wiod.manuf.data %<>% mutate(STRENGTH.EFF = STRENGTH.OUT / STRENGTH.IN) 

## remove fields that will not be used in the descriptive stat and correlation matrix
omitted.fields <- c("year", "country.ind", "country", "industry") 

## change the TWO lines bellow for any removal of variables from the lists
##omitted.net.var <- NULL
omitted.net.var <- c("PAGE.RANK", "STRENGTH.ALL", "STRENGTH.OUT",
                     "STRENGTH.IN")

omitted.fields <- list(omitted.fields,  omitted.net.var)
omitted.fields <- unlist(omitted.fields)

## descriptive stat table
stargazer(wiod.manuf.data, type = "latex", out = "./tables/desc_stat.tex",
          title = "Descriptive statistics.", label = "table:desc_stat",
          font.size = "footnotesize", digits = 1, out.header = FALSE,
          omit = omitted.fields, 
          omit.summary.stat = c("p25", "p75"))


## remove fields for correlation table
wiod.corr.data <- wiod.manuf.data %>% select(-omitted.fields)

## correlation table, wiod.manuf.data is a dataframe 
corr.matrix <- round(cor(wiod.corr.data, method = "pearson"), 3)

corr.matrix[upper.tri(corr.matrix)] <- ""
corr.matrix <- as.data.frame(corr.matrix)
corr.matrix

stargazer(corr.matrix, summary=FALSE, type = "latex", out = "./tables/corr_matrix.tex",
          title = "Pearson correlation matrix.", label = "table:corr_matrix",
          font.size = "footnotesize", out.header = FALSE)

############ ############ ############ ############ ############ ############ 
## panel model analysis

reg.model <- function(lag.year) {
    ## insert the lag.year obtain 4 models with that lag in INNOV.CAP

    ## tentative models. 
    
    ## model without any interaction
    no.int <- as.formula(paste0("log(VA) ~ log(STRENGTH.EFF) + log(EIGEN.CENT) +
                                 log(BETWEENNESS) + log(lag(INNOV.CAP + 1,", lag.year, ")) +
                                 log(DOM.OUT) + log(INT.OUT)"))

    ## model with EIGEN.CENT and INNOV.CAP interaction
    eigen.innov <- as.formula(paste0("log(VA) ~ log(STRENGTH.EFF) + log(BETWEENNESS) +
                                      log(EIGEN.CENT) * log(lag(INNOV.CAP + 1,", lag.year, ")) +
                                      log(DOM.OUT) + log(INT.OUT)"))

    ## model with STRENGTH.EFFiciency and INNOV.CAP interaction
    eff.innov  <-  as.formula(paste0("log(VA) ~ log(EIGEN.CENT) + log(BETWEENNESS) +
                                      log(STRENGTH.EFF) * log(lag(INNOV.CAP + 1,", lag.year, ")) +
                                      log(DOM.OUT) + log(INT.OUT)"))

    ## model with STRENGTH.EFFiciency and INNOV.CAP interaction
    btw.innov  <-  as.formula(paste0("log(VA) ~ log(STRENGTH.EFF) + log(EIGEN.CENT) +
                                      log(BETWEENNESS) * log(lag(INNOV.CAP + 1,", lag.year, ")) +
                                      log(DOM.OUT) + log(INT.OUT)"))

    return(list(no.int, eigen.innov, eff.innov, btw.innov))
}


reg.output <- function(model) {

    p.mod.out <- plm(formula = model, data = wiod.manuf.data,
                           index=c("country.ind", "year"), model="within")

    ct.simple <- coeftest(p.mod.out, vcovHC) # Heteroskedasticity consistent coef.
    se.simple <- ct.simple[,2]
    pval.simple <- ct.simple[,4]

    return(list(p.mod.out, se.simple, pval.simple))
}

reg.table  <- function(lag.year) {

    models.lagged <- reg.model(lag.year)

    ## obtaining the regression table for the 4 models
    lagged.table <- sapply(models.lagged, reg.output)

    models.out <- lagged.table[1,]
    ses.out <- lagged.table[2,]
    pvals.out <- lagged.table[3,]

    label.lag <- paste0("table:reg_lag_", lag.year)

    caption.end <- ifelse(lag.year==1, "year lag", "years lag")
    caption.lag <- paste("\"Regression models for", lag.year, caption.end, ".\"", sep = " ")

    file.lag <- paste0(tables.dir, "/reg_table_lag_", lag.year, ".tex")
    
    latex.reg <- texreg(models.out, override.se = ses.out, override.pvalues = pvals.out,
                        fontsize = "scriptsize", sideways = FALSE, table = TRUE,
                        label = label.lag,
                        caption = caption.lag, caption.above = TRUE)
    sink(file = file.lag, type="output")
    cat(latex.reg)
    sink()

    screenreg(models.out, override.se = ses.out, override.pvalues = pvals.out)
}

xo## creating the list of lag years
list.lags <- c(1, 2, 3)

## runing all models for all lag years.
lapply(list.lags, reg.table)
