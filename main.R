## This file is made for the reproducibility of the results obtained in
## Kim, K., Özaygen, A. (2019) Analysis of the innovative capacity and
## the network position of national manufacturing industries in world
## production.

library(dplyr)
library(magrittr)
library(tidyr)
library(reshape2)
library(stringr)
library(tibble)
library(igraph)
library(stargazer)
library(Hmisc)

## global values
dir.to.write <<- "./wiod_long_data"
dir.create(dir.to.write)

## you have access to the R code and the sqls related to the ipc patstat
## manipulations from the directory patstat_manuf.

## these scripts creates several directories and at the end the final
## data which is used in the analysis is wiod_manuf_net_VA_patent.csv

## WIOD data obtained from the web and changed into a long file
source("./wiod_manuf_data_prep.R")

## the long file is used on year basis for the calculation of network
## scores for each node. at the end, patstat data is joined to the final data.
source("./wiod_manuf_network_scores.R")

## this is last portion which produces all figures, tables that are used
## in the article. It produces latex tables and also txt tables printed
## on the screen. Figures are in pdf and png. Figures in pdf are used in
## the article. All results are stored in output directory.
source("./wiod_manuf_analysis.R")

