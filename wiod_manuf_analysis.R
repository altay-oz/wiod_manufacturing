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
library('texreg')
library(Hmisc)

## getting wiod zip file and unzipping and obtaining yearly IO tables
# getWIOD(2)

## making network calculations
# netWIOD(2)

## getting the panel data with country-ind as unit analysis.
# panelWIOD(0)

## getting the panel data with country as unit analysis.
# getCountryWIOD()

## panel data are ready. 
# wiod.manuf.df <- read.csv("wiod_manuf_net_panel_2000_2014_1.csv") # isic2
# summary(wiod.manuf.df$strength.out-wiod.manuf.df$strength.in)
# # wiod.ctry.df <- read.csv("wiod_ctry_net_panel_2000_2014_1.csv") #isic0
# wiod.ctry.df <- read.csv("wiod_isic_1_net_panel_2000_2014_1.csv") #isic1

# country: aus
# isic_0: aus.C10-C12
# isic_1: aus.c
# isic_2: aus.high-tech

wiod.pat.manuf <- read.csv("wiod_pat_isic_2.csv")  
wiod.pat.ctry.0 <- read.csv("wiod_pat_isic_1.csv")  
wiod.pat.ctry <- read.csv("wiod_pat_country.csv") 

# An important accounting identity in the WIOT is that gross output of each industry (given in the last element of each column) 
# is equal to the sum of all uses of the output from that industry (given in the last element of each row).

wiod.pat.manuf %<>% mutate(GO=strength.out+dom.Z.weight+int.Z.weight)
wiod.pat.manuf <- left_join(wiod.pat.manuf %>% mutate(country=substr(country.ind,1,3)),wiod.pat.ctry %>% mutate(VA.ALL=VA) %>% select(country,year, VA.ALL),by=c("country","year"))
wiod.pat.manuf %<>% mutate (VA.prop = VA/VA.ALL)

wiod.pat.ctry <- left_join(wiod.pat.ctry,wiod.pat.ctry.C %>% select(country,year, VA.C))
wiod.pat.ctry %<>% mutate (man.prop = VA.C/VA)
wiod.pat.ctry %<>% mutate (GO=strength.out+dom.Z.weight+int.Z.weight)

# measure the proportion of MANUF

## reading the patent count data obtained from patstat
# patstat.df <- read.csv("./patstat_manuf/country_ind_yearly_pat_tech_sum.csv",
#                              stringsAsFactors = FALSE)

## isic2
## then join it
# wiod.pat.manuf <- left_join(wiod.manuf.df, patstat.df, 
#                   by = c("country.ind" = "country.ind", "year" = "appln_filing_year"))

## isic1
## country.ind to country and sum
# patstat.ctry.df  <- patstat.df %>% separate(country.ind, c("country", "ind"), "\\.") %>%
#     dplyr::select(-ind) %>% group_by(country, appln_filing_year) %>%
#     dplyr::summarise(pat_num = sum(pat_tech_sum))
# wiod.ctry.df <- wiod.ctry.df %>% filter (substr(country.ind,5,5)=="C") %>%
#     separate(country.ind, c("country", "industry"), "\\.") %>% dplyr::select(-industry) 
# wiod.pat.ctry <- left_join(wiod.ctry.df, patstat.ctry.df %>% mutate(ind="C"),
#                   by = c("country" = "country","year" = "appln_filing_year")) %>% select (-ind)
                  # by = c("country" = "country", "industry"="ind","year" = "appln_filing_year"))


## add spatial data
library('readxl')
dist_cepii<-read_excel("E:/Google Drive (awekimm@gmail.com)/[RESEARCH]/[0PARIS]/06_industry network/kim_altay_projects/spatial/dist_cepii.xls") 

## data preparation for spatial data set
dist_cepii$iso_o[dist_cepii$iso_o =="ROM"] <- "ROU"
dist_cepii$iso_d[dist_cepii$iso_d =="ROM"] <- "ROU"
dist_cepii.1 <- dist_cepii %>% filter(iso_o %in% unique(wiod.pat.ctry$country) & iso_d %in% unique(wiod.pat.ctry$country))

# check
dist_cepii.1$iso_o %>% unique %>% length # 43, except ROW
dist_cepii.1$iso_d %>% unique %>% length # 43, except ROW
wiod.pat.ctry$country %>% unique %>% length # 44

# transform to matrix
dist_cepii.mat <- dist_cepii.1 %>% dplyr::select(iso_o,iso_d,dist) %>% spread(key=iso_o,value=dist) %>% data.frame
row.names(dist_cepii.mat) <- dist_cepii.mat$iso_d
dist_cepii.mat <- dist_cepii.mat %>% dplyr::select(-starts_with("iso_d"))
dist_cepii.matw <- as.matrix(dist_cepii.mat) 

# check missing file
# install.packages("naniar")  
library(ggplot2)
library(naniar) # visualise missing data

dim(wiod.pat.ctry)
dim(na.omit(wiod.pat.ctry))
gg_miss_var(wiod.pat.ctry)  

################ ################ ################ ################ ################ 
## starting the analysis.

## isic2
## creating the output directories for figures and tables to be used in
## the article
figures.dir <- "./figures"
tables.dir <- "./tables"

dir.create(figures.dir)
dir.create(tables.dir)

## creating country and industry columns
wiod.pat.manuf %<>% mutate(country = str_sub(country.ind, 1, 3)) %>%
    mutate(industry = str_sub(country.ind, 5))
wiod.pat.manuf$Manuf[wiod.pat.manuf$industry %in% c("Low Tech","Medium-Low Tech","Medium-High Tech","High Tech")] <- 1
wiod.pat.manuf$Manuf[is.na(wiod.pat.manuf$Manuf)==TRUE] <- 0
wiod.pat.manuf %<>% filter (country!="ROW")

head(wiod.pat.manuf)
length(unique(wiod.pat.manuf$country))

## creating va growth 
wiod.pat.manuf %<>% arrange(country.ind,year) %>% 
    group_by(country.ind) %>% 
    mutate(lva.gr = log(VA) - dplyr::lag(log(VA)),
           lgo.gr = (log(GO) - dplyr::lag(log(GO)))
           # lgo.gr = (log(GO) - dplyr::lag(log(GO)))/dplyr::lag(log(VA))*100
           ) %>% data.frame
head(wiod.pat.manuf)

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
                            "INNOV.CAP", "GO", "country","VA.ALL","VA.PROP","industry","MANUF","VA.GR","GO.GR")

## adding STRENGTH.EFFiciency variable
names(wiod.manuf.data)
wiod.manuf.data %<>% mutate(STRENGTH.EFF = STRENGTH.OUT / (STRENGTH.IN+0.0001)) 
wiod.manuf.data %<>% mutate(OI = (STRENGTH.OUT-STRENGTH.IN) /  (STRENGTH.OUT+STRENGTH.IN)) 

## remove fields that will not be used in the descriptive stat and correlation matrix
omitted.fields <- c("year", "country.ind", "country", "industry", "MANUF") 

## change the TWO lines bellow for any removal of variables from the lists
##omitted.net.var <- NULL
omitted.net.var <- c("PAGE.RANK", "STRENGTH.ALL", "STRENGTH.OUT","STRENGTH.IN",
                     "DOM.IN","INT.IN","DOM.OUT","INT.OUT")

omitted.fields <- list(omitted.fields,  omitted.net.var)
omitted.fields <- unlist(omitted.fields)

############ ############ ############ ############ ############ ############ 
## panel model analysis
# I changed log-scale to normal form
detach("package:plyr")

wiod.manuf.data %<>% mutate(DOM.FINAL.L = log(abs(min(DOM.FINAL))+DOM.FINAL+0.001), INT.FINAL.L = log(abs(min(INT.FINAL))+INT.FINAL+0.001))

wiod.manuf.data %<>% left_join(emptopop.l[,c("country.code","year","emptopop")],by=c("country"="country.code","year"="year")) %>%
    left_join(gdp.l[,c("country.code","year","gdp")],by=c("country"="country.code","year"="year")) %>% 
    left_join(gpc.l[,c("country.code","year","gpc")],by=c("country"="country.code","year"="year")) %>% 
    left_join(pop.l[,c("country.code","year","pop")],by=c("country"="country.code","year"="year")) %>% 
    left_join(empinind.l[,c("country.code","year","empinind")],by=c("country"="country.code","year"="year")) %>%
    dplyr::rename(EMPTOPOP = emptopop, GDP = gdp, PPP = gpc, POP = pop, EMPININD = empinind)

## remove fields for correlation table
wiod.corr.data <- wiod.manuf.data %>% # filter(country!="TWN") %>% 
    dplyr::select(EIGEN.CENT,BETWEENNESS,INNOV.CAP,STRENGTH.EFF,DOM.FINAL,INT.FINAL)

## descriptive stat table
stargazer(wiod.corr.data, type = "latex", out = "./tables/desc_stat.tex",
          title = "Descriptive statistics.", label = "table:desc_stat",
          font.size = "footnotesize", digits = 1, out.header = FALSE,
          omit = omitted.fields, 
          omit.summary.stat = c("p25", "p75"))

## correlation table, wiod.manuf.data is a dataframe 
corr.matrix <- round(cor(wiod.corr.data, method = "pearson"), 3)

corr.matrix[upper.tri(corr.matrix)] <- ""
corr.matrix <- as.data.frame(corr.matrix)
corr.matrix

stargazer(corr.matrix, summary=FALSE, type = "latex", out = "./tables/corr_matrix.tex",
          title = "Pearson correlation matrix.", label = "table:corr_matrix",
          font.size = "footnotesize", out.header = FALSE)

library(car)
vif(lm(log(GO) ~ log(EIGEN.CENT) +
           log(BETWEENNESS) + log(INNOV.CAP+1) +
           STRENGTH.EFF + log(DOM.FINAL) + log(INT.FINAL),data=wiod.manuf.data))


lag.year <- 1
base.m <- as.formula(paste0("log(GO) ~ lag(log(EIGEN.CENT+1),0) + lag(log(BETWEENNESS+1),0) + 
                                 lag(STRENGTH.EFF,0) + lag(DOM.FINAL.L,0) + lag(INT.FINAL.L,0) "))
no.int <- as.formula(paste0("log(GO) ~ lag(log(EIGEN.CENT+0.0001),0) + lag(log(BETWEENNESS+0.0001),0) + 
                                 log(lag(INNOV.CAP + 1,", lag.year, ")) +
                                 lag(STRENGTH.EFF,0) + lag(DOM.FINAL.L,0) + lag(INT.FINAL.L,0) "))
eig.inn <- as.formula(paste0("log(GO) ~ lag(log(EIGEN.CENT+0.0001),0) + lag(log(BETWEENNESS+0.0001),0) +
                                 log(lag(INNOV.CAP + 1,", lag.year, ")) + I(lag(log(EIGEN.CENT+0.0001),0)*log(lag(INNOV.CAP + 1,", lag.year, "))) +
                                 lag(STRENGTH.EFF,0) + lag(DOM.FINAL.L,0) + lag(INT.FINAL.L,0) "))
btw.inn <- as.formula(paste0("log(GO) ~ lag(log(EIGEN.CENT+0.0001),0) + lag(log(BETWEENNESS+0.0001),0) +
                                 log(lag(INNOV.CAP + 1,", lag.year, ")) + I(lag(log(BETWEENNESS+0.0001),0)*log(lag(INNOV.CAP + 1,", lag.year, "))) +
                                 lag(STRENGTH.EFF,0) + lag(DOM.FINAL.L,0) + lag(INT.FINAL.L,0) "))

summary(wiod.manuf.data$INNOV.CAP)
names(wiod.manuf.data)

# panel
plm.model.wi.1 <- plm(base.m, data = wiod.manuf.data %>% filter (industry=="Low Tech" ), index=c("country.ind","year"), model = "within", effect ="twoways")
plm.model.wi.2 <- plm(no.int, data = wiod.manuf.data %>% filter (industry=="Low Tech" ), index=c("country.ind","year"), model = "within", effect ="twoways")
plm.model.wi.3 <- plm(eig.inn, data = wiod.manuf.data %>% filter (industry=="Low Tech" ), index=c("country.ind","year"), model = "within", effect ="twoways")
plm.model.wi.4 <- plm(btw.inn, data = wiod.manuf.data %>% filter (industry=="Low Tech" ), index=c("country.ind","year"), model = "within", effect ="twoways")
plm.model.wi.5 <- plm(base.m, data = wiod.manuf.data %>% filter (industry=="Medium-Low Tech" ), index=c("country.ind","year"), model = "within", effect ="twoways")
plm.model.wi.6 <- plm(no.int, data = wiod.manuf.data %>% filter (industry=="Medium-Low Tech" ), index=c("country.ind","year"), model = "within", effect ="twoways")
plm.model.wi.7 <- plm(eig.inn, data = wiod.manuf.data %>% filter (industry=="Medium-Low Tech" ), index=c("country.ind","year"), model = "within", effect ="twoways")
plm.model.wi.8 <- plm(btw.inn, data = wiod.manuf.data %>% filter (industry=="Medium-Low Tech" ), index=c("country.ind","year"), model = "within", effect ="twoways")
plm.model.wi.9 <- plm(base.m, data = wiod.manuf.data %>% filter (industry=="Medium-High Tech" ), index=c("country.ind","year"), model = "within", effect ="twoways")
plm.model.wi.10 <- plm(no.int, data = wiod.manuf.data %>% filter (industry=="Medium-High Tech" ), index=c("country.ind","year"), model = "within", effect ="twoways")
plm.model.wi.11 <- plm(eig.inn, data = wiod.manuf.data %>% filter (industry=="Medium-High Tech" ), index=c("country.ind","year"), model = "within", effect ="twoways")
plm.model.wi.12 <- plm(btw.inn, data = wiod.manuf.data %>% filter (industry=="Medium-High Tech" ), index=c("country.ind","year"), model = "within", effect ="twoways")
plm.model.wi.13 <- plm(base.m, data = wiod.manuf.data %>% filter (industry=="High Tech" ), index=c("country.ind","year"), model = "within", effect ="twoways")
plm.model.wi.14 <- plm(no.int, data = wiod.manuf.data %>% filter (industry=="High Tech" ), index=c("country.ind","year"), model = "within", effect ="twoways")
plm.model.wi.15 <- plm(eig.inn, data = wiod.manuf.data %>% filter (industry=="High Tech" ), index=c("country.ind","year"), model = "within", effect ="twoways")
plm.model.wi.16 <- plm(btw.inn, data = wiod.manuf.data %>% filter (industry=="High Tech" ), index=c("country.ind","year"), model = "within", effect ="twoways")
plm.model.ra <- plm(no.int,
                    data = wiod.manuf.data, index=c("country.ind","year"), model = "random")
phtest(plm.model.wi.2,plm.model.ra,method="aux", vcov=vcovHC)

library(plm)
library(clubSandwich)
# coef_test(plm.model.wi.2, vcov = "CR1", cluster = "individual", test = "naive-t")
coeftest(plm.model.wi.2, vcovHC)
coeftest(plm.model.wi.16, vcovHC)
coeftest(plm.model.wi.14, vcov = vcovHC(plm.model.wi.16, type="HC1"))
summary(plm.model.wi.14)

stargazer(
    # plm.model.wi.2,plm.model.wi.3,plm.model.wi.4,plm.model.wi.6,plm.model.wi.7,plm.model.wi.8,
    plm.model.wi.10,plm.model.wi.11,plm.model.wi.12,plm.model.wi.14,plm.model.wi.15,plm.model.wi.16,
    title="Results", align=TRUE, type="text",
    summary=TRUE, header=FALSE,
    label="tab:isic2_regresults",
    model.names=TRUE,
    font.size="small",flip=TRUE,
    dep.var.labels = c('GO'),
    covariate.labels = c("EIGEN", "BTW","INNOV.CAP", "EIGEN*INNOV.CAP", "BTW*INNOV.CAP","STRENGTH.EFF","DOM.FINAL","INT.FINAL","VA.PROP"),
    add.lines = list(c("Country", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                     c("Year", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"))
    )

library('wfe')
stargazer(
          coeftest(plm.model.wi.2, vcovHC),coeftest(plm.model.wi.3, vcovHC),coeftest(plm.model.wi.4, vcovHC),coeftest(plm.model.wi.6, vcovHC),coeftest(plm.model.wi.7, vcovHC),coeftest(plm.model.wi.8, vcovHC),
          # coeftest(plm.model.wi.10, vcovHC),coeftest(plm.model.wi.11, vcovHC),coeftest(plm.model.wi.12, vcovHC),coeftest(plm.model.wi.14, vcovHC),coeftest(plm.model.wi.15, vcovHC),coeftest(plm.model.wi.16, vcovHC),
          # plm.model.wi.2,plm.model.wi.3,plm.model.wi.4,plm.model.wi.6,plm.model.wi.7,plm.model.wi.8,
          # plm.model.wi.10,plm.model.wi.11,plm.model.wi.12,plm.model.wi.14,plm.model.wi.15,plm.model.wi.16,
          title="Results", align=TRUE, type="text",
          summary=TRUE, header=FALSE,
          label="tab:isic2_regresults",
          model.names=TRUE,
          font.size="small",flip=TRUE,
          dep.var.labels = c('VA'),
          covariate.labels = c("EIGEN", "BTW","INNOV.CAP", "EIGEN*INNOV.CAP", "BTW*INNOV.CAP","STRENGTH.EFF","DOM.FINAL","INT.FINAL"),
          add.lines = list(c("Country", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Year", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"))
)

plm.model.wi.2.all <- plm(no.int, data = wiod.manuf.data, index=c("country.ind","year"), model = "within", effect ="twoways")
plm.model.wi.3.all <- plm(eig.inn, data = wiod.manuf.data, index=c("country.ind","year"), model = "within", effect ="twoways")
plm.model.wi.4.all <- plm(btw.inn, data = wiod.manuf.data, index=c("country.ind","year"), model = "within", effect ="twoways")
stargazer(plm.model.wi.2.all, plm.model.wi.3.all, plm.model.wi.4.all, type="text")
stargazer(
    plm.model.wi.2.all, plm.model.wi.3.all, plm.model.wi.4.all,
    title="Results", align=TRUE, type="text",
    summary=TRUE, header=FALSE,
    label="tab:isic2_regresults",
    model.names=TRUE,
    font.size="small",flip=TRUE,
    dep.var.labels = c('GO'),
    covariate.labels = c("EIGEN", "BTW","INNOV.CAP", "EIGEN*INNOV.CAP", "BTW*INNOV.CAP","STRENGTH.EFF","DOM.FINAL","INT.FINAL","PPP","EMPININD","VA.PROP"),
    add.lines = list(c("Country", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                     c("Year", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"))
)

summary(plm.model.wi.4.all)


library(dynpanel)
reg<- dpd(base.m,wiod.manuf.data.p %>% select(VA,EIGEN.CENT, BETWEENNESS, STRENGTH.EFF,DOM.FINAL,INT.FINAL),1,3)
summary(reg)
wiod.manuf.data.p <- pdata.frame(wiod.manuf.data,index=c("country.ind","year"))
z1 <- pgmm(log(VA + 1e-04) ~ lag(log(VA + 1e-04),1:2) + lag(log(EIGEN.CENT + 1e-04),0) + 
               lag(log(BETWEENNESS +1e-04),0) + lag(log(INNOV.CAP + 1), 1) +
               log(STRENGTH.EFF + 1e-04) + log(DOM.FINAL+ 1e-04) + log(INT.FINAL+ 1e-04) | lag(log(VA + 1e-04),2:99),
           data = wiod.manuf.data.p, effect = "twoways", model = "twosteps")
summary(z1, robust = FALSE)
help(pgmm)


#########################################################
## isic1
## remove ROW. ROW is not considered in spatial regression
# wiod.pat.ctry <- wiod.pat.ctry %>% filter (country!="ROW")
# 
# names(wiod.pat.ctry)
# 
# ## renaming all variabes in capital letters.
# names(wiod.pat.ctry) <- c("country", "STRENGTH.ALL",
#                             "STRENGTH.OUT", "STRENGTH.IN",
#                             "BETWEENNESS", "PAGE.RANK", "EIGEN.CENT",#"AUTH","HUB",
#                             "DOM.OUT", "INT.OUT", "DOM.IN", "INT.IN",
#                             "DOM.FINAL", "INT.FINAL", "VA", "year",
#                             "INNOV.CAP", "VA.C","MANUF", "GO")
# 
# 
# ## adding STRENGTH.EFFiciency variable
# wiod.pat.ctry %<>% mutate(STRENGTH.EFF = STRENGTH.OUT / (STRENGTH.IN+0.0001)) 
# wiod.pat.ctry %<>% mutate(OI = (STRENGTH.OUT-STRENGTH.IN) /  (STRENGTH.OUT+STRENGTH.IN)) 
# wiod.pat.ctry %<>% mutate(ALL.FINAL = DOM.FINAL+INT.FINAL) 
# 
# ## remove fields that will not be used in the descriptive stat and correlation matrix
# omitted.fields <- c("year", "country", "MANUF") 
# 
# ## change the TWO lines bellow for any removal of variables from the lists
# ##omitted.net.var <- NULL
# omitted.net.var <- c("PAGE.RANK", "STRENGTH.ALL", "STRENGTH.OUT","STRENGTH.IN",
#                      "AUTH","HUB","DOM.IN","INT.IN","DOM.OUT","INT.OUT")
# 
# omitted.fields <- list(omitted.fields,  omitted.net.var)
# omitted.fields <- unlist(omitted.fields)
# 
# ## descriptive stat table
# stargazer(wiod.manuf.data, type = "latex", out = "./tables/desc_stat.tex",
#           title = "Descriptive statistics.", label = "table:desc_stat",
#           font.size = "footnotesize", digits = 1, out.header = FALSE,
#           omit = omitted.fields, 
#           omit.summary.stat = c("p25", "p75"))
# 
# 
# ## remove fields for correlation table
# wiod.corr.data.all <- wiod.pat.ctry %>% select(-omitted.fields) %>% 
#     select(GO, EIGEN.CENT,BETWEENNESS,INNOV.CAP,STRENGTH.EFF,ALL.FINAL,PPP,EMPININD,POP)
# 
# ## correlation table, wiod.manuf.data is a dataframe 
# corr.matrix <- round(cor(wiod.corr.data.all, method = "pearson"), 3)
# 
# corr.matrix[upper.tri(corr.matrix)] <- ""
# corr.matrix <- as.data.frame(corr.matrix)
# corr.matrix
# 
# stargazer(corr.matrix, summary=FALSE, type = "latex", out = "./tables/corr_matrix.tex",
#           title = "Pearson correlation matrix.", label = "table:corr_matrix",
#           font.size = "footnotesize", out.header = FALSE)
# 
# vif(lm(VA ~ EIGEN.CENT +
#            BETWEENNESS + INNOV.CAP +
#            OI + STRENGTH.EFF + DOM.FINAL + INT.FINAL + MANUF,data=wiod.pat.ctry))


############ ############ ############ ############ ############ ############ 
## panel model analysis

# wiod.pat.ctry$country.ind<-paste(wiod.pat.ctry$country,wiod.pat.ctry$industry,sep=".")
# lag.year <- 1
# base.m <- as.formula(paste0("log(VA+0.0001) ~ log(EIGEN.CENT+0.0001) + log(BETWEENNESS+0.0001) + 
#                                  OI + log(STRENGTH.EFF+0.0001) + log(DOM.FINAL+0.0001) + log(INT.FINAL+0.0001) + MANUF"))
# no.int <- as.formula(paste0("log(VA+0.0001) ~ log(EIGEN.CENT+0.0001) + log(BETWEENNESS+0.0001) + 
#                                  log(lag(INNOV.CAP + 1,", lag.year, ")) +
#                                  OI + log(STRENGTH.EFF+0.0001) + log(DOM.FINAL+0.0001) + log(INT.FINAL+0.0001) + MANUF"))
# eig.inn <- as.formula(paste0("log(VA+0.0001) ~ log(EIGEN.CENT+0.0001) + log(BETWEENNESS+0.0001) +
#                                  log(lag(INNOV.CAP + 1,", lag.year, ")) + I(log(EIGEN.CENT+0.0001)*log(lag(INNOV.CAP + 1,", lag.year, "))) +
#                                  OI + log(STRENGTH.EFF+0.0001) + log(DOM.FINAL+0.0001) + log(INT.FINAL+0.0001) + MANUF"))
# btw.inn <- as.formula(paste0("log(VA+0.0001) ~ log(EIGEN.CENT+0.0001) + log(BETWEENNESS+0.0001) +
#                                  log(lag(INNOV.CAP + 1,", lag.year, ")) + I(log(BETWEENNESS+0.0001)*log(lag(INNOV.CAP + 1,", lag.year, "))) +
#                                  OI + log(STRENGTH.EFF+0.0001) + log(DOM.FINAL+0.0001) + log(INT.FINAL+0.0001) + MANUF"))
# 
# reg.model <- no.int
# 
# # panel
# plm.model.wi.11 <- plm(base.m, 
#                       data = wiod.pat.ctry, index=c("country.ind","year"), model = "within", effect ="twoways")
# plm.model.wi.22 <- plm(no.int, 
#                       data = wiod.pat.ctry, index=c("country.ind","year"), model = "within", effect ="twoways")
# plm.model.wi.33 <- plm(eig.inn, 
#                       data = wiod.pat.ctry, index=c("country.ind","year"), model = "within", effect ="twoways")
# plm.model.wi.44 <- plm(btw.inn, 
#                       data = wiod.pat.ctry, index=c("country.ind","year"), model = "within", effect ="twoways")
# # plm.model.ra <- plm(reg.model, 
# #                     data = wiod.manuf.data, index=c("country.ind","year"), model = "random", effect ="individual")
# # phtest(plm.model.wi,plm.model.ra,method="aux", vcov=vcovHC)
# summary(plm.model.wi.22)
# 
# stargazer(plm.model.wi.11, plm.model.wi.22,plm.model.wi.33,plm.model.wi.44,
#           title="Results", align=TRUE, type="text")






################ ################ ################ ################ ################ 
# country level data analysis
################ ################ ################ ################ ################ 
## starting the analysis.
################ ################ ################ ################ ################ 
## spatial regression data set
## the article
figures.dir <- "./country/figures"
tables.dir <- "./country/tables"

dir.create(figures.dir)
dir.create(tables.dir)

## remove ROW. ROW is not considered in spatial regression
wiod.pat.ctry <- wiod.pat.ctry %>% filter (country!="ROW" & country!="TWN")
wiod.pat.ctry <- wiod.pat.ctry %>% mutate(OI=((strength.out-strength.in)/(strength.out+strength.in)))
names(wiod.pat.ctry)

## ADD macro variables #####################
# gini-simpson
# gini<-data.frame(read.csv('C:/Users/user/Google 드라이브/[RESEARCH]/[0PARIS]/06_industry network/kim_altay_projects/condensed_manufacturing_(low_high_etc)/gini_labour/gini_data_csv.csv',header=TRUE,sep=','))
# names(gini)<-c("country.name","country.code","year","gini")

# WSDB 
WSDB<-data.frame(read.csv('C:/Users/user/Google 드라이브/[RESEARCH]/[0PARIS]/06_industry network/kim_altay_projects/condensed_manufacturing_(low_high_etc)/gini_labour/WSDB_07062019142153011.csv',header=FALSE,sep=','))
names(WSDB) <- c("country.code","country","var","variable","sex","sex2","time","year","unitcode","unit","powercodecode","powercode","reference","reference2","value","blank1","blank2")

# emptopop
emptopop<-data.frame(read.csv('C:/Users/user/Google 드라이브/[RESEARCH]/[0PARIS]/06_industry network/kim_altay_projects/condensed_manufacturing_(low_high_etc)/gini_labour/emptopop.csv',header=TRUE,sep=','))
# emptopop <- emptopop %>% rename_at(vars(starts_with('X')),funs(str_replace(.,'X',''))) 
emptopop.l <- emptopop %>% dplyr::select(Country.Code, starts_with('X')) %>% dplyr::rename(country.code=Country.Code) %>% gather(year,emptopop,X1960:X2018)
emptopop.l$year <- gsub('X','',emptopop.l$year)
emptopop.l$year <- as.integer(emptopop.l$year)

# gdp
gdp <- data.frame(read.csv('C:/Users/user/Google 드라이브/[RESEARCH]/[0PARIS]/06_industry network/kim_altay_projects/condensed_manufacturing_(low_high_etc)/gini_labour/gdp.csv',header=TRUE,sep=','))
gdp.l <- gdp %>% dplyr::select(Country.Code, starts_with('X')) %>% dplyr::rename(country.code=Country.Code) %>% gather(year,gdp,X1960:X2018)
gdp.l$year <- gsub('X','',emptopop.l$year)
gdp.l$year <- as.integer(gdp.l$year)

# gdp per capita
gpc<-data.frame(read.csv('C:/Users/user/Google 드라이브/[RESEARCH]/[0PARIS]/06_industry network/kim_altay_projects/condensed_manufacturing_(low_high_etc)/gini_labour/gdppercap.csv',header=TRUE,sep=','))
gpc.l <- gpc %>% dplyr::select(Country.Code, starts_with('X')) %>% dplyr::rename(country.code=Country.Code) %>% gather(year,gpc,X1960:X2018)
gpc.l$year <- gsub('X','',emptopop.l$year)
gpc.l$year <- as.integer(gpc.l$year)

# pop
pop<-data.frame(read.csv('C:/Users/user/Google 드라이브/[RESEARCH]/[0PARIS]/06_industry network/kim_altay_projects/condensed_manufacturing_(low_high_etc)/gini_labour/pop.csv',header=TRUE,sep=','))
pop.l <- pop %>% dplyr::select(Country.Code, starts_with('X')) %>% dplyr::rename(country.code=Country.Code) %>% gather(year,pop,X1960:X2018)
pop.l$year <- gsub('X','',pop.l$year)
pop.l$year <- as.integer(pop.l$year)

# emp in industry
empinind<-data.frame(read.csv('C:/Users/user/Google 드라이브/[RESEARCH]/[0PARIS]/06_industry network/kim_altay_projects/condensed_manufacturing_(low_high_etc)/gini_labour/empinind.csv',header=TRUE,sep=','))
empinind.l <- empinind %>% dplyr::select(Country.Code, starts_with('X')) %>% dplyr::rename(country.code=Country.Code) %>% gather(year,empinind,X1960:X2018)
empinind.l$year <- gsub('X','',empinind.l$year)
empinind.l$year <- as.integer(empinind.l$year)

# indvapw
# indvapw<-data.frame(read.csv('C:/Users/user/Google 드라이브/[RESEARCH]/[0PARIS]/06_industry network/kim_altay_projects/condensed_manufacturing_(low_high_etc)/gini_labour/industry_va_pw.csv',header=TRUE,sep=','))
# indvapw.l <- indvapw %>% dplyr::select(Country.Code, starts_with('X')) %>% dplyr::rename(country.code=Country.Code) %>% gather(year,indvapw,X1960:X2018)
# indvapw.l$year <- gsub('X','',indvapw.l$year)
# indvapw.l$year <- as.integer(indvapw.l$year)

# rndexp
# rndexp <- data.frame(read.csv('C:/Users/user/Google 드라이브/[RESEARCH]/[0PARIS]/06_industry network/kim_altay_projects/condensed_manufacturing_(low_high_etc)/gini_labour/rndexp.csv',header=TRUE,sep=','))
# rndexp.l <- rndexp %>% dplyr::select(Country.Code, starts_with('X')) %>% dplyr::rename(country.code=Country.Code) %>% gather(year,rndexp,X1960:X2018)
# rndexp.l$year <- gsub('X','',rndexp.l$year)
# rndexp.l$year <- as.integer(rndexp.l$year)

# rndres
# rndres <- data.frame(read.csv('C:/Users/user/Google 드라이브/[RESEARCH]/[0PARIS]/06_industry network/kim_altay_projects/condensed_manufacturing_(low_high_etc)/gini_labour/rndresearcher.csv',header=TRUE,sep=','))
# rndres.l <- rndres %>% dplyr::select(Country.Code, starts_with('X')) %>% dplyr::rename(country.code=Country.Code) %>% gather(year,rndres,X1960:X2018)
# rndres.l$year <- gsub('X','',rndres.l$year)
# rndres.l$year <- as.integer(rndres.l$year)

# left_join gini
# wiod.pat.ctry <- left_join(wiod.pat.ctry,gini[,c("country.code","year","gini")],by=c("country"="country.code","year"="year")) %>% as.data.frame
# left_join emptopop
wiod.pat.ctry <- left_join(wiod.pat.ctry,emptopop.l[,c("country.code","year","emptopop")],by=c("country"="country.code","year"="year")) %>% as.data.frame
# left_join gdp
wiod.pat.ctry <- left_join(wiod.pat.ctry,gdp.l[,c("country.code","year","gdp")],by=c("country"="country.code","year"="year")) %>% as.data.frame
# left_join gpc
wiod.pat.ctry <- left_join(wiod.pat.ctry,gpc.l[,c("country.code","year","gpc")],by=c("country"="country.code","year"="year")) %>% as.data.frame
# left_join pop
wiod.pat.ctry <- left_join(wiod.pat.ctry,pop.l[,c("country.code","year","pop")],by=c("country"="country.code","year"="year")) %>% as.data.frame
# left_join empinind
wiod.pat.ctry <- left_join(wiod.pat.ctry,empinind.l[,c("country.code","year","empinind")],by=c("country"="country.code","year"="year")) %>% as.data.frame
# left_join indvapw
# wiod.pat.ctry <- left_join(wiod.pat.ctry,indvapw.l[,c("country.code","year","indvapw")],by=c("country"="country.code","year"="year")) %>% as.data.frame
# left_join rndexp
# wiod.pat.ctry <- left_join(wiod.pat.ctry,rndexp.l[,c("country.code","year","rndexp")],by=c("country"="country.code","year"="year")) %>% as.data.frame
# left_join rndres
# wiod.pat.ctry <- left_join(wiod.pat.ctry,rndres.l[,c("country.code","year","rndres")],by=c("country"="country.code","year"="year")) %>% as.data.frame
#######################

## renaming all variabes in capital letters.
names(wiod.pat.ctry) <- c("country", "STRENGTH.ALL",
                            "STRENGTH.OUT", "STRENGTH.IN",
                            "BETWEENNESS", "PAGE.RANK", "EIGEN.CENT",
                            "DOM.OUT", "INT.OUT", "DOM.IN", "INT.IN",
                            "DOM.FINAL", "INT.FINAL", "VA", "year",
                            "INNOV.CAP","VA.C","MAN.PROP","GO","OI","EMPTOPOP","GDP","PPP","POP","EMPININD"#,"INDVAPW","RNDEXP","RNDRES"
                          )

## adding STRENGTH.EFFiciency variable
wiod.pat.ctry %<>% mutate(STRENGTH.EFF = STRENGTH.OUT / STRENGTH.IN) 
wiod.pat.ctry %<>% mutate(ALL.FINAL = DOM.FINAL / INT.FINAL) 

## remove fields that will not be used in the descriptive stat and correlation matrix
omitted.fields <- c("year","country") 

## change the TWO lines bellow for any removal of variables from the lists
## omitted.net.var <- NULL
omitted.net.var <- c("PAGE.RANK", "STRENGTH.ALL", "STRENGTH.OUT","STRENGTH.IN",
                     "DOM.OUT","INT.OUT","DOM.IN","INT.IN")#,"INDVAPW"),"RNDEXP","RNDRES")

omitted.fields <- list(omitted.fields,  omitted.net.var)
omitted.fields <- unlist(omitted.fields)

## remove fields for correlation table
wiod.ctr.corr.data <- wiod.pat.ctry %>% #filter(is.na(EMPTOPOP)==FALSE) %>% filter(year!=2014) %>%
    dplyr::select(EIGEN.CENT,BETWEENNESS,INNOV.CAP,DOM.FINAL,INT.FINAL,STRENGTH.EFF,PPP,POP)
# wiod.ctr.corr.data$RNDEXP[is.na(wiod.ctr.corr.data$RNDEXP)==TRUE]<-0.0001

## descriptive stat table
stargazer(wiod.ctr.corr.data, type = "latex", out = "./tables/desc_stat.tex",
          title = "Descriptive statistics.", label = "table:desc_stat",
          font.size = "footnotesize", digits = 1, out.header = FALSE,
          omit = omitted.fields, 
          omit.summary.stat = c("p25", "p75"))


## correlation table, wiod.manuf.data is a dataframe 
ctr.corr.matrix <- round(cor(wiod.ctr.corr.data, method = "pearson"), 3)

ctr.corr.matrix[upper.tri(ctr.corr.matrix)] <- ""
ctr.corr.matrix <- as.data.frame(ctr.corr.matrix)
ctr.corr.matrix

stargazer(ctr.corr.matrix, summary=FALSE, type = "latex", out = "./country/tables/ctr_corr_matrix.tex",
          title = "Pearson correlation matrix.", label = "table:corr_matrix",
          font.size = "footnotesize", out.header = FALSE)

vif(lm(log(GO) ~ EIGEN.CENT +
           BETWEENNESS + INNOV.CAP + INT.FINAL +  
           STRENGTH.EFF + PPP + EMPININD,data=wiod.pat.ctry))

############ ############ ############ ############ ############ ############ 
## Country level

############ ############ ############ ############ ############ ############ 
## spatial regression
help(spml)

# create manuf dummy
wiod.pat.ctry$man.dum[wiod.pat.ctry$MAN.PROP>=mean(wiod.pat.ctry$MAN.PROP)]<-1
wiod.pat.ctry$man.dum[is.na(wiod.pat.ctry$man.dum)==TRUE]<-0

# LM test set 
wiod.pat.ctry.1 <- wiod.pat.ctry %>% dplyr::select(country, year, GO, VA, EIGEN.CENT, BETWEENNESS, INNOV.CAP, ALL.FINAL, DOM.FINAL, INT.FINAL, STRENGTH.EFF, PPP, EMPININD, EMPTOPOP, POP, MAN.PROP)
# wiod.pat.ctry.1 <- wiod.manuf.data %>% mutate(ALL.FINAL=DOM.FINAL+INT.FINAL) %>%
#     group_by(country, year) %>%
#     dplyr::summarise(GO=sum(GO), EIGEN.CENT=sum(EIGEN.CENT), BETWEENNESS=sum(BETWEENNESS),
#                      INNOV.CAP=sum(INNOV.CAP), ALL.FINAL=sum(ALL.FINAL), STRENGTH.EFF=sum(STRENGTH.EFF))
# wiod.pat.ctry.1 %<>% left_join(wiod.pat.ctry %>% dplyr::select(country, year, EMPTOPOP, GDP, PPP, POP, EMPININD) %>% unique, by=c("country","year"))

# create panel data frame
wiod.pat.ctry.p.1 <- pdata.frame(wiod.pat.ctry.1  %>% filter(country!="TWN") %>%
                                     group_by(country) %>% mutate(INNOV.CAP.LAG=dplyr::lag(INNOV.CAP,1)), c("country","year"))  
wiod.pat.ctry.p.1 %<>% filter(year!=2000)

length(unique(wiod.pat.ctry.p.1$country))
row.names(dist_cepii.matw) %>% length

# wiod.pat.ctry.p <- wiod.pat.ctry.p %>% filter (year!=2014)
# wiod.pat.ctry.p.1 <- wiod.pat.ctry.p.1 %>% filter (year!=2014)

# spatial regression
library('splm')
library("spatialreg")

# create distance degay spatial weight matrix
# create listw object
dist_cepii.matw.0 <- dist_cepii.matw
dist_cepii.matw <- 1/dist_cepii.matw
diag(dist_cepii.matw) <- 0
# dist_cepii.matw <- dist_cepii.matw[-42,] # remove TWN
# dist_cepii.matw <- dist_cepii.matw[,-42] # remove TWN
dist_cepii.matw.lw <- mat2listw(dist_cepii.matw, style="W")

# create spatial lag variable (first-order contiguity)
# wiod.pat.ctry.p$SLEIGEN.CENT <- slag(wiod.pat.ctry.p$EIGEN.CENT, dist_cepii.matw)
# wiod.pat.ctry.p$SLBETWEENNESS <- slag(wiod.pat.ctry.p$BETWEENNESS, dist_cepii.matw)
# wiod.pat.ctry.p$SLSTRENGTH.EFF <- slag(wiod.pat.ctry.p$STRENGTH.EFF, dist_cepii.matw)
# wiod.pat.ctry.p.df <- as.data.frame(wiod.pat.ctry.p)


# model configuration
names(wiod.pat.ctry.p.1)
lag.year <- 0 # set lag

no.int <- as.formula(paste0("log(GO) ~ log(EIGEN.CENT) + log(BETWEENNESS+1) + 
                                 log(lag(INNOV.CAP.LAG + 1,", lag.year, ")) +
                                 STRENGTH.EFF + log(DOM.FINAL) + log(INT.FINAL) + log(PPP) + EMPININD +  log(POP)"))
eig.inn <- as.formula(paste0("log(GO) ~ log(EIGEN.CENT) + log(BETWEENNESS+1) +
                                 log(lag(INNOV.CAP.LAG + 1,", lag.year, ")) + I(log(EIGEN.CENT)*log(lag(INNOV.CAP.LAG + 1,", lag.year, "))) +
                                 STRENGTH.EFF + log(DOM.FINAL) + log(INT.FINAL) + log(PPP) + EMPININD + log(POP)"))
btw.inn <- as.formula(paste0("log(GO) ~ log(EIGEN.CENT) + log(BETWEENNESS+1) +
                                 log(lag(INNOV.CAP.LAG + 1,", lag.year, ")) + I(log(BETWEENNESS+1)*log(lag(INNOV.CAP.LAG + 1,", lag.year, "))) +
                                 STRENGTH.EFF + log(DOM.FINAL)+ log(INT.FINAL) + log(PPP) + EMPININD + log(POP)"))

# stage 0. model setting
reg.model <- eig.inn

# panel
plm.model.wi <- plm(reg.model, 
                 data = wiod.pat.ctry.p.1, model = "within", effect ="twoways")
plm.model.ra <- plm(reg.model, 
                 data = wiod.pat.ctry.p.1, model = "random", effect ="twoways")
phtest(plm.model.wi,plm.model.ra,method="aux", vcov=vcovHC)
summary(plm.model.wi)

reg.1 <- plm(no.int, 
             data = wiod.pat.ctry.p.1, model = "within", effect ="twoways")
reg.2 <- plm(eig.inn, 
             data = wiod.pat.ctry.p.1, model = "within", effect ="twoways")
reg.3 <- plm(btw.inn, 
             data = wiod.pat.ctry.p.1, model = "within", effect ="twoways")
summary(reg.1)
stargazer(reg.1,reg.2,reg.3,
          title="Results", align=TRUE, type="text",
summary=TRUE, header=FALSE, 
label="tab:regresults",
model.names=TRUE, 
# no.sapce=TRUE,
font.size="small",flip=TRUE,
dep.var.labels = c('GO'),
covariate.labels = c("EIGEN", "STR.EFF", "BTW","INNOV.CAP", "EIGEN*INNOV.CAP", "STR.EFF*INNOV.CAP", "BTW*INNOV.CAP", "Dom.CON","Int.CON","No.IN","No.OUT"),
add.lines = list(c("Country", "Yes", "Yes", "Yes"),c("Year", "Yes", "Yes", "Yes"))
)
help(stargazer)

# step 1. check spatial autocorrelation
# LM1: No random effect
# LM2: there is spatial autocorrelation
# CLMlambda: there is spatial autocorrelation

testh <- bsktest(x = reg.model,
                 data = wiod.pat.ctry.p.1,
                 listw = dist_cepii.matw.lw, test = "LMH")
test1 <- bsktest(x = reg.model,
                 data = wiod.pat.ctry.p.1,
                 listw = dist_cepii.matw.lw, test = "LM1")
test2 <- bsktest(x = reg.model,
                 data = wiod.pat.ctry.p.1,
                 listw = dist_cepii.matw.lw, test = "LM2")
testh
test1
test2

# step 2. spatial Hausman test
# Hausman test robust to spatial autocorrelation (splm)
test3 <- sphtest(x = reg.model, 
                 data = wiod.pat.ctry.p.1,
                 listw = dist_cepii.matw.lw, spatial.model="error", method = "ML") 
test3 # fixed effect
test3 <- sphtest(x = reg.model,
                 data = wiod.pat.ctry.p.1,
                 listw = dist_cepii.matw.lw, spatial.model = "lag", method="ML")
test3 # fixed effect

mod1 <- spgm(formula = reg.model,
             data = wiod.pat.ctry.p.1,
             listw = dist_cepii.matw.lw, lag = TRUE,
             moments = "fullweights", model = "random", spatial.error = TRUE)
mod2 <- spgm(formula = reg.model,
             data = wiod.pat.ctry.p.1,
             listw = dist_cepii.matw.lw, lag = TRUE,
             model = "within", spatial.error = TRUE)
test3 <- sphtest(x = mod1, x2 = mod2)
test3 # fixed effect

# https://maczokni.github.io/crimemapping_textbook_bookdown/spatial-regression-models.html
# https://methods.sagepub.com/dataset/howtoguide/spatial-lag-bristol-sale-2004
reg.model <- no.int

# LM test for spatial lag
slmtest(reg.model, data=wiod.pat.ctry.p.1 %>% data.frame %>% filter(year!=2000), listw = dist_cepii.matw.lw, test="lml", model="within",
        index=c("country","year"))
# LM test for spatial lag (robust)
slmtest(reg.model, data=wiod.pat.ctry.p.1 %>% data.frame %>% filter(year!=2000), listw = dist_cepii.matw.lw, test="rlml", model="within",
        index=c("country","year"))
# LM test for spatial error
slmtest(reg.model, data=wiod.pat.ctry.p.1 %>% data.frame %>% filter(year!=2000), listw = dist_cepii.matw.lw, test="lme", model="within",
        index=c("country","year"))
# LM test for spatial error (robust)
slmtest(reg.model, data=wiod.pat.ctry.p.1 %>% data.frame %>% filter(year!=2000), listw = dist_cepii.matw.lw, test="rlme", model="within",
        index=c("country","year"))

data(Produc, package="plm")
data(usaww)
fm <- log(gsp)~log(pcap)+log(pc)+log(emp)+unemp
## robust LM test for spatial error sub spatial lag
## model on original data, pooling hypothesis
slmtest(fm, data=Produc, listw = usaww, test="rlme")

head(wiod.manuf.data)
head(wiod.pat.ctry)
head(wiod.pat.ctry.p)
wiod.pat.ctry.p$ALL.FINAL

# stage 0. model setting
reg.model <- no.int

sem.mod.out <- spml(formula = reg.model, 
                        data = wiod.pat.ctry.p.1, # wiod.pat.ctry.p.1
                        listw = dist_cepii.matw.lw, model = "within", effect ="twoways", 
                        lag = TRUE, spatial.error = "none")#"kkp"
# sem.mod.out1 <- spml(formula = reg.model, 
#                         data = wiod.pat.ctry.p,
#                         listw = dist_cepii.matw.lw, model = "within", effect ="twoways", 
#                         lag = TRUE, spatial.error = "kkp")#"kkp"
sgm.mod.out <- spgm(formula = reg.model, 
                        data = wiod.pat.ctry.p.1,
                        listw = dist_cepii.matw.lw, 
                        lag = TRUE, moments = "fullweights", model = "within", spatial.error = FALSE)
summary(sem.mod.out)
# summary(sem.mod.out1)
summary(sgm.mod.out)


# hist(wiod.pat.ctry.p.1$MAN.PROP)
# 
lag.year <- 0 # set lag

no.int <- as.formula(paste0("log(GO) ~ log(EIGEN.CENT) + log(BETWEENNESS+1) + 
                                 log(lag(INNOV.CAP.LAG + 1,", lag.year, ")) +
                                 STRENGTH.EFF + ALL.FINAL + log(PPP) + EMPININD + log(POP)"))
eig.inn <- as.formula(paste0("log(GO) ~ log(EIGEN.CENT) + log(BETWEENNESS+1) +
                                 log(lag(INNOV.CAP.LAG + 1,", lag.year, ")) + I(log(EIGEN.CENT)*log(lag(INNOV.CAP.LAG + 1,", lag.year, "))) +
                                 STRENGTH.EFF + log(ALL.FINAL.L) + log(PPP) + EMPININD + log(POP)"))
btw.inn <- as.formula(paste0("log(GO) ~ log(EIGEN.CENT) + log(BETWEENNESS+1) +
                                 log(lag(INNOV.CAP.LAG + 1,", lag.year, ")) + I(log(BETWEENNESS+1)*log(lag(INNOV.CAP.LAG + 1,", lag.year, "))) +
                                  STRENGTH.EFF + log(ALL.FINAL.L) + log(PPP) + EMPININD + log(POP)"))
# STRENGTH.EFF + ALL.FINAL  + EMPTOPOP + VA.PROP

wiod.pat.ctry.p <- pdata.frame(wiod.manuf.data %>% mutate(ALL.FINAL = DOM.FINAL+INT.FINAL, country=substr(country.ind,1,3)) %>% 
                                   mutate(ALL.FINAL.L = log(abs(min(ALL.FINAL))+ALL.FINAL+1)) %>% 
                                   filter(industry=="High Tech" & country!="TWN") %>%
                                   group_by(country) %>% mutate(INNOV.CAP.LAG=dplyr::lag(INNOV.CAP,1)) %>% ungroup %>% filter(year!=2000) %>%
                                   dplyr::select(country, year, GO, EIGEN.CENT, BETWEENNESS, INNOV.CAP.LAG, STRENGTH.EFF, ALL.FINAL, ALL.FINAL.L, PPP, EMPININD, POP, EMPTOPOP, VA.PROP), 
                               c("country","year"))
table(wiod.pat.ctry.p$country)

summary(wiod.pat.ctry.p)

reg.model <- eig.inn
sem.mod.out.ctr <- spml(formula = reg.model, 
                    data = wiod.pat.ctry.p, # wiod.pat.ctry.p.1
                    listw = dist_cepii.matw.lw, model = "within", effect ="twoways", 
                    lag = TRUE, spatial.error = "none")#"kkp"
sgm.mod.out.ctr <- spgm(formula = reg.model, 
                    data = wiod.pat.ctry.p,
                    listw = dist_cepii.matw.lw, 
                    lag = TRUE, moments = "fullweights", model = "within", spatial.error = FALSE)
summary(sem.mod.out.ctr)
summary(sgm.mod.out.ctr)


# slm.mod.out <- spml(formula = reg.model, 
#                     data = wiod.pat.ctry.p, 
#                     listw = dist_cepii.matw, model = "within", effect ="individual",
#                     lag = TRUE, spatial.error = "none")
# sarma.mod.out <- spml(formula = reg.model, 
#                       data = wiod.pat.ctry.p, 
#                       listw = dist_cepii.matw, model = "within", effect ="individual",
#                       lag = TRUE, spatial.error = "kkp")
# sglm.mod.out <- spgm(formula= reg.model, 
#                         data = wiod.pat.ctry.p,
#                         listw = dist_cepii.matw.lw, 
#                         lag = TRUE, moments = "fullweights", model = "within", spatial.error = FALSE)
# sargm.mod.out <- spgm(formula= reg.model, 
#                         data = wiod.pat.ctry.p,
#                         listw = dist_cepii.matw.lw, 
#                         lag = TRUE, moments = "fullweights", model = "within", spatial.error = TRUE)
summary(slm.mod.out)
summary(sarma.mod.out)
summary(sglm.mod.out)
summary(sargm.mod.out)

help(bsktest)

reg.model<-log(VA) ~ OI + log(EIGEN.CENT) + 
    log(BETWEENNESS+0.0001) * log(INNOV.CAP + 1) +
    log(INT.FINAL) + log(GDC)+log(POP)+log(EMPININD) 

help(slmtest)

summary(log(wiod.pat.ctry.p$EMPININD))

table(wiod.pat.ctry.p$country) %>% length
row.names(dist_cepii.matw) %>% length
dist_cepii.matw.lw


data(Produc, package="plm")
data(usaww)
fm <- log(gsp)~log(pcap)+log(pc)+log(emp)+unemp
## robust LM test for spatial error sub spatial lag
## model on original data, pooling hypothesis
slmtest(fm, data=Produc, listw = usaww, test="lml")
## model on within-transformed (time-demeaned) data,
## eliminates individual effects
slmtest(fm, data=Produc, listw = usaww, test="rlme",
        model="within")




#############################
# create graph

# groupting by time
wiod.manuf.data$year.type[wiod.manuf.data$year>=2000 & wiod.manuf.data$year<=2008]<-c("2000-2008")
wiod.manuf.data$year.type[wiod.manuf.data$year>=2009 & wiod.manuf.data$year<=2014]<-c("2009-2014")

# create data set for overall plots
# for statistics
head(wiod.manuf.data)
library(plyr)
summary(wiod.manuf.data$STRENGTH.EFF)

# dom.weight - dom.final = dom.intermediate
# int.weight - int.final = int.intermediate
# difference in access to the final market 
wiod.manuf.data %<>% mutate(DOM.ITM = DOM.OUT-DOM.FINAL) %>% mutate(INT.ITM = INT.OUT-INT.FINAL) 
wiod.manuf.data %<>% mutate(DOM.OI = (DOM.ITM-DOM.IN)/(DOM.ITM+DOM.IN)) %>% mutate(INT.OI = (INT.ITM-DOM.IN)/(INT.ITM+INT.IN)) 
names(wiod.manuf.data)

wiod.manuf.data.sum <- ddply(wiod.manuf.data, .(country, year.type, industry), dplyr::summarize,strength.all.1=mean(STRENGTH.ALL),
                                           btw.1=mean(BETWEENNESS),page.rank.1=mean(PAGE.RANK),eigen.1=mean(EIGEN.CENT),
                                           str.eff.1=mean(STRENGTH.EFF),innov.1=mean(INNOV.CAP),oi.1=mean(OI),
                                           VA.1=mean(VA[is.na(VA)==FALSE]),dom.itm.1=mean(DOM.ITM),int.itm=mean(INT.ITM),
                                           dom.oi.1=mean(DOM.OI), int.oi.1=mean(INT.OI))
wiod.manuf.data.sum.yr <- ddply(wiod.manuf.data %>% filter (MANUF=="Manuf"), .(country, year, industry), dplyr::summarize,strength.all.1=mean(STRENGTH.ALL),
                             btw.1=mean(BETWEENNESS),page.rank.1=mean(PAGE.RANK),eigen.1=mean(EIGEN.CENT),
                             str.eff.1=mean(STRENGTH.EFF),innov.1=mean(INNOV.CAP),
                             VA.1=mean(VA[is.na(VA)==FALSE]))
wiod.manuf.data.sum.bar <- ddply(wiod.manuf.data %>% filter (MANUF=="Manuf"), .(country, industry), dplyr::summarize,strength.all.1=mean(STRENGTH.ALL),
                                btw.1=mean(BETWEENNESS),page.rank.1=mean(PAGE.RANK),eigen.1=mean(EIGEN.CENT),
                                str.eff.1=mean(STRENGTH.EFF),innov.1=mean(INNOV.CAP),
                                VA.1=mean(VA[is.na(VA)==FALSE]))


# plot annuaal industry-network of each countries
dev.off()

wiod.manuf.data.sum.yr.g<-list()
for (i in 1:length(unique(wiod.manuf.data.sum.yr$country))){
    temp<-wiod.manuf.data.sum.yr[wiod.manuf.data.sum.yr$country==unique(wiod.manuf.data.sum.yr$country)[[i]] &
                                                    is.na(wiod.manuf.data.sum.yr$industry)==FALSE,]
    wiod.manuf.data.sum.yr.g[[i]] <- ggplot(temp, aes(str.eff.1/max(str.eff.1),btw.1/max(btw.1))) +    # geom_point()+
        geom_line(aes(color=industry),arrow=arrow(),size=1)+ggtitle(paste("Industry network of ",unique(wiod.manuf.data.sum.yr$country)[[i]]))+
        # geom_smooth(method="auto",se=TRUE,fullrange=FALSE,level=0.95)
        geom_hline(yintercept=0.5, linetype="dashed",color = "gray", size=1)+
        geom_vline(xintercept=0.5, linetype="dashed",color = "gray", size=1)+
        theme_bw()+theme(aspect.ratio=1)
}
# high: DEU, mid-high: FRA, low-high: ITA, low: BGR
wiod.manuf.data.sum.yr.g[[11]]
wiod.manuf.data.sum.yr.g[[16]]
wiod.manuf.data.sum.yr.g[[24]]
wiod.manuf.data.sum.yr.g[[8]] # CHINA
wiod.manuf.data.sum.yr.g[[43]] # USA

# plot bar graph
ctr.mantype.bar.1 <- ggplot(wiod.manuf.data.sum.bar[is.na(wiod.manuf.data.sum.bar$industry)==FALSE,], aes(x=country,y=eigen.1,fill=industry))+
    geom_bar(stat="identity", color="black",position=position_dodge())+
    scale_fill_brewer(palette="Greys")+labs(fill="Manufacturing Type") +
    theme_bw()+coord_flip()+theme(legend.position="bottom")+
    ylab("Average of Eigen Centrality") + 
    xlab("Country")
ctr.mantype.bar.2 <- ggplot(wiod.manuf.data.sum.bar[is.na(wiod.manuf.data.sum.bar$industry)==FALSE,], aes(x=country,y=str.eff.1,fill=industry))+
    geom_bar(stat="identity", color="black",position=position_dodge())+
    scale_fill_brewer(palette="Greys")+labs(fill="Manufacturing Type") +
    theme_bw()+coord_flip()+theme(legend.position="bottom")+
    ylab("Average of Strength Efficiency") + 
    xlab("Country")
ctr.mantype.bar.3 <- ggplot(wiod.manuf.data.sum.bar[is.na(wiod.manuf.data.sum.bar$industry)==FALSE,], aes(x=country,y=btw.1,fill=industry))+
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette="Greys")+labs(fill="Manufacturing Type") +
    theme_bw()+coord_flip()+theme(legend.position="bottom") +
    ylab("Average of Betweenness Centrality") + 
    xlab("Country")


### Industry network of all countries (all time period)
wiod.manuf.data.sum.bar$clabel[wiod.manuf.data.sum.bar$country=='USA'] <- 'USA'
wiod.manuf.data.sum.bar$clabel[wiod.manuf.data.sum.bar$country=='CHN'] <- 'CHN'
wiod.manuf.data.sum.bar$clabel[wiod.manuf.data.sum.bar$country=='POL'] <- 'POL'
wiod.manuf.data.sum.bar.g <-
    ggplot(wiod.manuf.data.sum.bar[is.na(wiod.manuf.data.sum.bar$industry)==FALSE,], aes(str.eff.1/max(str.eff.1),btw.1/max(btw.1),shape=industry,label=clabel)) + #,size = IH_intang/max(IH_intang))) + geom_point()+
    geom_point(aes(,size=VA.1))+ geom_text() +
    # geom_line(aes(color=man.type),arrow=arrow(),size=1)+
    ggtitle("Industry network of all countries")+
    # geom_smooth(method="auto",se=TRUE,fullrange=FALSE,level=0.95)
    geom_hline(yintercept=0.5, linetype="dashed",color = "gray", size=1)+
    geom_vline(xintercept=0.5, linetype="dashed",color = "gray", size=1)+
    theme_bw()+theme(aspect.ratio=1)

### Industry network of all countries (comparison between two periods)
# lable for overall plot

# O-I graph
library(grid)
wiod.manuf.data.sum$country.ind<-paste(wiod.manuf.data.sum$country,wiod.manuf.data.sum$industry,sep=".")
wiod.manuf.data.sum$clabel<-""
names(wiod.manuf.data.sum)
wiod.manuf.data.sum.g.yr <-list()
wiod.manuf.data.sum.g.yr.1 <-list()
dim.table <- list()
for (i in 1:length(unique(wiod.manuf.data.sum$year.type))){
    temp <- wiod.manuf.data.sum[wiod.manuf.data.sum$year.type==unique(wiod.manuf.data.sum$year.type)[[i]],]
    temp <- temp %>% mutate (inn.1 = innov.1/max(innov.1[is.na(innov.1)==FALSE]))
    
    # create dummy for each domain
    temp$oi.type[temp$dom.oi.1>=0 & temp$int.oi.1>0] <- c("DOM.OUT_INT.OUT")
    temp$oi.type[temp$dom.oi.1<=0 & temp$int.oi.1>0] <- c("DOM.IN_INT.OUT")
    temp$oi.type[temp$dom.oi.1<=0 & temp$int.oi.1<0] <- c("DOM.IN_INT.IN")
    temp$oi.type[temp$dom.oi.1>=0 & temp$int.oi.1<0] <- c("DOM.OUT_INT.IN")
    
    # collect dominant tech-intensity
    dim.1<-data.frame(table(temp[temp$oi.type=="DOM.OUT_INT.OUT",]$industry)) %>% filter(Freq==max(Freq)) 
    dim.2<-data.frame(table(temp[temp$oi.type=="DOM.IN_INT.OUT",]$industry)) %>% filter(Freq==max(Freq)) 
    dim.3<-data.frame(table(temp[temp$oi.type=="DOM.IN_INT.IN",]$industry)) %>% filter(Freq==max(Freq)) 
    dim.4<-data.frame(table(temp[temp$oi.type=="DOM.OUT_INT.IN",]$industry)) %>% filter(Freq==max(Freq)) 
    
    wiod.manuf.data.sum.g.yr[[i]]  <-
        ggplot(temp, 
               aes(dom.oi.1,int.oi.1,shape=industry,label=clabel)) + #,size = IH_intang/max(IH_intang))) + geom_point()+
        # aes(x=out.in/max(out.in),y=btw/max(btw),color=eig/max(eig))) + #,size = IH_intang/max(IH_intang))) + geom_point()+
        geom_point(aes(size=inn.1/max(inn.1)),color="gray")+geom_text()+
        # geom_point()+
        # geom_line(aes(color=man.type),arrow=arrow(),size=1)+
        # ggtitle(paste("Industry network of all countries in",unique(wiod_yearly_net_scores_Z_VA.2.0.ctr$year.type)[[i]])) +
        # geom_smooth(method="auto",se=TRUE,fullrange=FALSE,level=0.95)
        geom_hline(yintercept=0, linetype="dashed",color = "black", size=1)+
        geom_vline(xintercept=0, linetype="dashed",color = "black", size=1)+
        theme_bw()+theme(aspect.ratio=1)+ #scale_colour_grey(start = 0, end = .5) +
        xlab("Domestic O-I Index") + ylab("International O-I Index") +
        scale_shape_discrete(name  ="Manufacturing Type",
                             # breaks=c("H","MH","ML", "L"),
                             labels=c("H (High Tech)","L (Low Tech)","MH (Medium-high Tech)","ML (Medium-low Tech)" ))+
        # scale_color_discrete(name  ="Country Type",
        #                        breaks=c("Developed","Developing"),
        #                        labels=c("Developed","Developing")) +
        scale_size_continuous(name  ="Normalized Innovative Capacity")+
        scale_x_continuous(limits=c(-1,1)) + scale_y_continuous(limits=c(-1,1)) +
        annotation_custom(grobTree(textGrob(dim.1$Var1, x=0.6,  y=0.95, hjust=0,
                                                    gp=gpar(col="black", fontsize=13, fontface="italic")))) +
        annotation_custom(grobTree(textGrob(dim.2$Var1, x=0.15,  y=0.95, hjust=0,
                                                    gp=gpar(col="black", fontsize=13, fontface="italic")))) +
        annotation_custom(grobTree(textGrob(dim.3$Var1, x=0.15,  y=0.05, hjust=0,
                                                    gp=gpar(col="black", fontsize=13, fontface="italic")))) +
        annotation_custom(grobTree(textGrob(dim.4$Var1, x=0.6,  y=0.05, hjust=0,
                                                    gp=gpar(col="black", fontsize=13, fontface="italic")))) 
        
    # scale_color_gradient(low="gray", high="black")
    
    # create table
    dim.table[[i]] <- rbind(data.frame(table(temp[temp$oi.type=="DOM.OUT_INT.OUT",]$industry)) %>% mutate(dim="dim.1") %>% mutate(year=unique(wiod.manuf.data.sum$year.type)[[i]]),
                          data.frame(table(temp[temp$oi.type=="DOM.OUT_INT.IN",]$industry)) %>% mutate(dim="dim.4") %>% mutate(year=unique(wiod.manuf.data.sum$year.type)[[i]]),
                          data.frame(table(temp[temp$oi.type=="DOM.IN_INT.OUT",]$industry)) %>% mutate(dim="dim.2") %>% mutate(year=unique(wiod.manuf.data.sum$year.type)[[i]]),
                          data.frame(table(temp[temp$oi.type=="DOM.IN_INT.IN",]$industry)) %>% mutate(dim="dim.3") %>% mutate(year=unique(wiod.manuf.data.sum$year.type)[[i]]))

}
dim.table.1<-dim.table[[1]] %>% select(dim, Var1, Freq) %>% arrange(dim)
dim.table.2<-dim.table[[2]] %>% select(dim, Var1, Freq) %>% arrange(dim)
names(dim.table.1) <- c("Quadrant","Manufacturing","2000-2008")
names(dim.table.2) <- c("Quadrant","Manufacturing","2009-2014")
dim.table.all <- cbind(dim.table.1, dim.table.2 %>% select(-Domain,-Manufacturing))
dim.table.all$Domain[dim.table.all$Domain=="dim.1"]<-"DOM.out & INT.OUT (I)"
dim.table.all$Domain[dim.table.all$Domain=="dim.2"]<-"DOM.IN & INT.OUT (II)"
dim.table.all$Domain[dim.table.all$Domain=="dim.3"]<-"DOM.IN & INT.IN (III)"
dim.table.all$Domain[dim.table.all$Domain=="dim.4"]<-"DOM.OUT & INT.IN (IV)"

library('xtable')
xtable(dim.table.all, caption=c("Summary of domestic and international O-I index"))




# eigen and BTW graph
wiod.manuf.data.sum.doibtw.yr <-list()
wiod.manuf.data.sum.ioibtw.yr <-list()
wiod.manuf.data.sum.eigbtw.yr <-list()
i<-1
for (i in 1:length(unique(wiod.manuf.data.sum$year.type))){
    temp <- wiod.manuf.data.sum[wiod.manuf.data.sum$year.type==unique(wiod.manuf.data.sum$year.type)[[i]],]
    temp <- temp %>% mutate (inn.1 = innov.1/max(innov.1[is.na(innov.1)==FALSE]))
    
    # top 5 group
    temp.eig <- temp %>% arrange(desc(eigen.1)) %>% top_n(5, eigen.1) %>% select(country.ind)
    temp.btw <- temp %>% arrange(desc(btw.1)) %>% top_n(5, btw.1) %>% select(country.ind) 
    
    # add label
    for (j in 1:5){
        temp.eig$clabel[[j]][substr(temp.eig$country.ind[[j]],5,50) == "High Tech"] <- paste(substr(temp.eig$country.ind[[j]],1,3),"H",sep=".")
        temp.eig$clabel[[j]][substr(temp.eig$country.ind[[j]],5,50) == "Medium-High Tech"] <- paste(substr(temp.eig$country.ind[[j]],1,3),"MH",sep=".")
        temp.eig$clabel[[j]][substr(temp.eig$country.ind[[j]],5,50) == "Medium-Low Tech"] <- paste(substr(temp.eig$country.ind[[j]],1,3),"ML",sep=".")
        temp.eig$clabel[[j]][substr(temp.eig$country.ind[[j]],5,50) == "Low Tech"] <- paste(substr(temp.eig$country.ind[[j]],1,3),"L",sep=".")
        
        temp.btw$clabel[[j]][substr(temp.btw$country.ind[[j]],5,50) == "High Tech"] <- paste(substr(temp.btw$country.ind[[j]],1,3),"H",sep=".")
        temp.btw$clabel[[j]][substr(temp.btw$country.ind[[j]],5,50) == "Medium-High Tech"] <- paste(substr(temp.btw$country.ind[[j]],1,3),"MH",sep=".")
        temp.btw$clabel[[j]][substr(temp.btw$country.ind[[j]],5,50) == "Medium-Low Tech"] <- paste(substr(temp.btw$country.ind[[j]],1,3),"ML",sep=".")
        temp.btw$clabel[[j]][substr(temp.btw$country.ind[[j]],5,50) == "Low Tech"] <- paste(substr(temp.btw$country.ind[[j]],1,3),"L",sep=".")
            
        temp$clabel[temp$country.ind == temp.eig$country.ind[[j]]] <- temp.eig$clabel[[j]]
        temp$clabel[temp$country.ind == temp.btw$country.ind[[j]]] <- temp.btw$clabel[[j]]
    }

    wiod.manuf.data.sum.doibtw.yr[[i]]  <-
        ggplot(temp, 
               aes(dom.oi.1,btw.1/max(btw.1),shape=industry,label=clabel)) + 
        geom_point(aes(size=inn.1/max(inn.1)),color="gray")+geom_text()+
        geom_hline(yintercept=0, linetype="dashed",color = "black", size=1)+
        geom_vline(xintercept=0, linetype="dashed",color = "black", size=1)+
        theme_bw()+theme(aspect.ratio=1)+ #scale_colour_grey(start = 0, end = .5) +
        xlab("Domestic O-I Index") + ylab("Average of Normalized Betweeness Centrality") +
        scale_shape_discrete(name  ="Manufacturing Type",
                             labels=c("H (High Tech)","L (Low Tech)","MH (Medium-high Tech)","ML (Medium-low Tech)"))+
        scale_size_continuous(name  ="Normalized Innovative Capacity") +
        scale_x_continuous(limits=c(-1,1)) + scale_y_continuous(limits=c(0,1)) 
    wiod.manuf.data.sum.ioibtw.yr[[i]]  <-
        ggplot(temp, 
               aes(int.oi.1,btw.1/max(btw.1),shape=industry,label=clabel)) + 
        geom_point(aes(size=inn.1/max(inn.1)),color="gray")+geom_text()+
        geom_hline(yintercept=0, linetype="dashed",color = "black", size=1)+
        geom_vline(xintercept=0, linetype="dashed",color = "black", size=1)+
        theme_bw()+theme(aspect.ratio=1)+ #scale_colour_grey(start = 0, end = .5) +
        xlab("International O-I Index") + ylab("Average of Normalized Betweeness Centrality") +
        scale_shape_discrete(name  ="Manufacturing Type",
                             labels=c("H (High Tech)","L (Low Tech)","MH (Medium-high Tech)","ML (Medium-low Tech)"))+
        scale_size_continuous(name  ="Normalized Innovative Capacity") +
        scale_x_continuous(limits=c(-1,1)) + scale_y_continuous(limits=c(0,1)) 
    wiod.manuf.data.sum.eigbtw.yr[[i]]  <-
        ggplot(temp, 
               aes(eigen.1/max(eigen.1),btw.1/max(btw.1),shape=industry,label=clabel)) + 
        geom_point(aes(size=inn.1/max(inn.1)),color="gray")+geom_text()+
        geom_hline(yintercept=0.5, linetype="dashed",color = "black", size=1)+
        geom_vline(xintercept=0.5, linetype="dashed",color = "black", size=1)+
        theme_bw()+theme(aspect.ratio=1)+ #scale_colour_grey(start = 0, end = .5) +
        xlab("Eigenvector Centrality") + ylab("Betweeness Centrality") +
        scale_shape_discrete(name  ="Manufacturing Type",
                             labels=c("H (High Tech)","L (Low Tech)","MH (Medium-high Tech)","ML (Medium-low Tech)"))+
        scale_size_continuous(name  ="Normalized Innovative Capacity") +
        scale_x_continuous(limits=c(0,1)) + scale_y_continuous(limits=c(0,1)) 
}

# create table for top rank in EIG-BTW
wiod.manuf.data.sum.rank.b1 <- wiod.manuf.data.sum %>% filter(year.type=="2000-2008") %>% arrange(desc(btw.1)) %>% dplyr::select(country.ind)
wiod.manuf.data.sum.rank.e1 <- wiod.manuf.data.sum %>% filter(year.type=="2000-2008") %>% arrange(desc(eigen.1)) %>% dplyr::select(country.ind)
wiod.manuf.data.sum.rank.i1 <- wiod.manuf.data.sum %>% filter(year.type=="2000-2008") %>% arrange(desc(innov.1)) %>% dplyr::select(country.ind)
wiod.manuf.data.sum.rank.b2 <- wiod.manuf.data.sum %>% filter(year.type=="2009-2014") %>% arrange(desc(btw.1)) %>% dplyr::select(country.ind)
wiod.manuf.data.sum.rank.e2 <- wiod.manuf.data.sum %>% filter(year.type=="2009-2014") %>% arrange(desc(eigen.1)) %>% dplyr::select(country.ind)
wiod.manuf.data.sum.rank.i2 <- wiod.manuf.data.sum %>% filter(year.type=="2009-2014") %>% arrange(desc(innov.1)) %>% dplyr::select(country.ind)

wiod.manuf.data.sum.rank <- cbind(wiod.manuf.data.sum.rank.e1[1:20,],wiod.manuf.data.sum.rank.e2[1:20,],
                                  wiod.manuf.data.sum.rank.b1[1:20,],wiod.manuf.data.sum.rank.b2[1:20,],
                                  wiod.manuf.data.sum.rank.i1[1:20,],wiod.manuf.data.sum.rank.i2[1:20,]) %>% as.data.frame
names(wiod.manuf.data.sum.rank)<-c("EIGEN (2000-2008)","EIGEN (2009-2014)",
                                                      "BTW (2000-2008)","BTW (2009-2014)",
                                                      "INNOV.CAP (2000-2008)","INNOV.CAP (2009-2014)")
library('xtable')
xtable(wiod.manuf.data.sum.rank, caption=c("Rank of EIGEN, BTW and INNOV.CAP (Top 20)"))

