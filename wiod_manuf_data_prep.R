## this script prepare the downloaded WIOD data in zip format to be used
## in network score calculations. In this script we are aggregating the
## WIOD table data according to the technology intensity of the
## manufacturing industries. We obtain 15 long tables (2000:2014).  two
## directories are created ./wiod_orginal_data where the WIOD data is
## stored and unziped and ./wiod_long_data where the 15 long tables
## created with this script.

## wiod files are from 2000 to 2014
## 44 countries, RoW included
## 56 industries, the final consumption data and VA value

ind.RNr <- function(yearly.raw.data) {

    ## get the industry and RNr code 
    industry.RNr <- yearly.raw.data %>% select(RNr, IndustryCode) %>% unique

    ## adding 57 to 61 the code Z. this is done to group them later
    ## 57 to 61 are final consumption
    ## we are using/keeping it for network score calculations.
    RNr <- c(57:61)
    IndustryCode <- "Z"
    df <- data.frame(RNr, IndustryCode, stringsAsFactors = FALSE)

    industry.RNr <- rbind(industry.RNr, df)

    ## changing the name of the manufacturing industry ISIC code into
    ## their technology intensity. Then we will aggregate according to
    ## their technology intensity.
    IndustryCode <- c("C10-C12","C13-C15","C16","C17","C18","C19","C20","C21","C22","C23","C24",
                "C25","C26","C27","C28","C29","C30","C31_C32","C33")
    tech.type <- c("Low Tech","Low Tech","Low Tech",
                "Low Tech","Low Tech",
                "Medium-Low Tech","Medium-High Tech",
                "High Tech","Medium-Low Tech",
                "Medium-Low Tech","Medium-Low Tech",
                "Medium-Low Tech","High Tech",
                "Medium-High Tech","Medium-High Tech",
                "Medium-High Tech","Medium-High Tech",
                "Low Tech","Low Tech")
    man.table <- data.frame(IndustryCode, tech.type, stringsAsFactors = FALSE)

    industry.RNr  <- left_join(industry.RNr, man.table, by = "IndustryCode")

    industry.RNr %<>% mutate(IndustryCode = ifelse(!is.na(tech.type),
                                                   tech.type, IndustryCode)) %>% select(-tech.type)

    names(industry.RNr) <- c("RNr", "NewIndustryCode")

    return(industry.RNr)
}

data.prep.raw <- function(yearly.raw) {

    industry.RNr <- ind.RNr(yearly.raw)
    
    ## changing the IndustryCode column wihtin the main df
    yearly.raw %<>% left_join(industry.RNr, yearly.raw, by = c("RNr"))
    
    ## cleaning the data frame
    yearly.raw %<>% mutate(IndustryCode = NewIndustryCode)

    return(yearly.raw)
}

divide.long.IO <- function(yearly.raw) {

    ## creating yearly complementary data to be used to get VA etc.
    yearly.complementary <- yearly.raw %>% filter(RNr > 64)
    ## obtaining
    ##   IndustryCode                                   IndustryDescription Country
    ## 1       II_fob                        Total intermediate consumption     TOT
    ## 2         TXSP                      taxes less subsidies on products     TOT
    ## 3      EXP_adj                       Cif/ fob adjustments on exports     TOT
    ## 4         PURR                  Direct purchases abroad by residents     TOT
    ## 5        PURNR Purchases on the domestic territory by non-residents      TOT
    ## 6           VA                           Value added at basic prices     TOT

    ## cleaning the data frame, removing unwanted columns in the long file
    yearly.long.IO <- yearly.raw %>% filter(RNr < 64)

    return(list(yearly.long.IO, yearly.complementary))
}

get.net.long <- function(yearly.IO) {

    yearly.IO %<>% select(-c("Year", "IndustryDescription", "NewIndustryCode", "RNr", "TOT"))

    ## joining Country and IndustryCode columns to create source (country.ind)
    yearly.IO %<>% unite("source", "Country", "IndustryCode", sep = ".") 

    ## creating the long table
    yearly.IO <- gather(yearly.IO, target.country.ind,
                        raw.weight, AUS1:ROW61, factor_key = FALSE)

    ## giving the industry code to the target.country.ind column
    yearly.IO %<>% separate(target.country.ind, c("target.country", "target.ind"), 3)

    yearly.IO$target.ind <- as.numeric(yearly.IO$target.ind)
    
    yearly.IO <- left_join(yearly.IO, industry.RNr, by=c("target.ind" = "RNr")) 

    yearly.IO %<>% select(-target.ind)

    ## removing the final consumption
    yearly.IO %<>% filter("NewIndustryCode" != "Z")
    
    yearly.IO %<>% unite("target", "target.country", "NewIndustryCode", sep = ".")

    ## ## aggregating according the Low Tech etc in target and source
    ## ## dividing the target column into country|ind|z.cat and making their sum
    yearly.IO %<>% group_by(source, target) %>% summarise(weight = sum(raw.weight)) %>% as.data.frame

    return(yearly.IO)
}

get.complementary  <- function(yearly.complementary, value) {

    yearly.select <- yearly.complementary %>% filter(IndustryCode == value) %>% select(AUS1:ROW61)
    yearly.select.long <- gather(yearly.select, countryind, factor_key = FALSE)

    ## repetition function which takes columnname, value to be summed and name
    yearly.select.long %<>% separate(countryind, c("country", "ind"), 3)

    yearly.select.long$ind <- as.numeric(yearly.select.long$ind)

    yearly.select.long <- left_join(yearly.select.long, industry.RNr, by=c("ind" = "RNr")) 

    head(yearly.select.long)

    yearly.select.long %<>% select(-ind)

    yearly.select.long %<>% unite("country.ind", "country", "NewIndustryCode", sep = ".")

    ## change the VA to the variable "value" as column name.
    yearly.select.long %<>% group_by(country.ind) %>% summarise(sum = sum(value))

    names(yearly.select.long) <- c("country.ind", value)

    return(yearly.select.long)    
}

dom.int.trade <- function(net.long.df) {
    ## inserting a long table of network realtion (source/target/weight)
    ## and obtaining the weight for each node's (country.ind)
    ## international and domestic trade.

    head(net.long.df)
    
    ## obtaining all nodes for making a left join at the end
    source <- net.long.df %>% select(source) %>% unique
    target <- net.long.df %>% select(target) %>% unique

    names(source) <- "country.ind"
    names(target) <- "country.ind"

    all.nodes <- rbind(source, target) %>% unique
    
    ## adding two columns source.country and target.country
    net.long.df %<>% mutate(source.country=substr(source, 1,3)) %>% 
        mutate(target.country=substr(target, 1,3))

    ## adding two columns source.industry and target.industry
    net.long.df %<>% mutate(source.industry=substr(source, 5, 25)) %>% 
        mutate(target.industry=substr(target, 5, 25))

    #####################
    ## domestic and international in and out weight for each
    ## country.industry (source)
    
    ## summing the the weights according to target country domestic
    dom.out <- net.long.df %>% filter(source.country == target.country) %>%
        group_by(source) %>% summarise(dom.out.weight = sum(weight)) 
    names(dom.out)[1] <- "country.ind"
    
    ## summing the the weights according to target country international
    int.out <- net.long.df %>% filter(source.country != target.country) %>%
        group_by(source) %>% summarise(int.out.weight = sum(weight)) 
    names(int.out)[1] <- "country.ind"
    
    ## summing the the weights according to target country domestic
    dom.in <- net.long.df %>% filter(source.country == target.country) %>%
        group_by(target) %>% summarise(dom.in.weight = sum(weight)) 
    names(dom.in)[1] <- "country.ind"
    
    ## summing the the weights according to target country international
    int.in <- net.long.df %>% filter(source.country != target.country) %>%
        group_by(target) %>% summarise(int.in.weight = sum(weight)) 
    names(int.in)[1] <- "country.ind"
    
    #####################
    ## domestic and international out weight for final consumption (Z)
    ## for each country.industry (source)
    
    ## domestic final consumption
    dom.Z <- net.long.df %>% filter(source.country == target.country, target.industry == "Z") %>%
        group_by(source) %>% summarise(dom.Z.weight = sum(weight))
    names(dom.Z)[1] <- "country.ind"

    ## international final consumption
    int.Z <- net.long.df %>% filter(source.country != target.country, target.industry == "Z") %>%
        group_by(source) %>% summarise(int.Z.weight = sum(weight))
    names(int.Z)[1] <- "country.ind"

    ## merge all
    dom.int.weights <- all.nodes %>% left_join(dom.out) %>% left_join(int.out) %>%
        left_join(dom.in) %>% left_join(int.in) %>% left_join(dom.Z) %>% left_join(int.Z)

    return(dom.int.weights)
}


write.files <- function(df, file.name) {
    dir.file <- paste(dir.to.write, file.name, sep="/")
    write.csv(df, dir.file, row.names = FALSE)
}

transform.all.files <- function(file.name) {
    ## insert a RDATA WIOT file and obtain a long data
    ## this function uses the above functions

    ##file.name <- wiod.files[2]

    year <- substr(file.name, 25, 28)
    
    yearly.raw.data <- get(load(file.name))
    
    ## creating the data.frame necessary to change all column names
    ## according to the industry.NACEcode such as AUS1 to AUS.A01
    industry.RNr <<- ind.RNr(yearly.raw.data)

    ## using the raw file to change it according to industry.RNr
    yearly.raw <- data.prep.raw(yearly.raw.data)

    ## obtaining the wide df for network transformation and other data
    two.df <- divide.long.IO(yearly.raw)

    ## the wide df to be used to obtain network long df
    yearly.IO <- two.df[[1]]
    ## getting the long df
    net.long.df <- get.net.long(yearly.IO)
    
    ## other info grabbed from the raw data
    yearly.complementary <- two.df[[2]]
    
    ## IndustryCode                                   IndustryDescription Country
    ##     II_fob                        Total intermediate consumption     TOT
    ##       TXSP                      taxes less subsidies on products     TOT
    ##    EXP_adj                       Cif/ fob adjustments on exports     TOT
    ##       PURR                  Direct purchases abroad by residents     TOT
    ##      PURNR Purchases on the domestic territory by non-residents      TOT
    ##         VA                           Value added at basic prices     TOT

    ## getting only the VA
    VA.df <- get.complementary(yearly.complementary, "VA")

    ## and the other info | uncomment them if needed and don't forget to
    ## bind them
    ## II_fob.df <- get.complementary(yearly.complementary, "II_fob")
    ## TXSP.df <- get.complementary(yearly.complementary, "TXSP")
    ## EXP_adj.df <- get.complementary(yearly.complementary, "EXP_adj")
    ## PURR.df <- get.complementary(yearly.complementary, "PURR")
    ## PURNR.df <- get.complementary(yearly.complementary, "PURNR")

    ## obtaining domestic and international trade in and out weight
    ## values for each country.industry
    dom.int.weights.df <- dom.int.trade(net.long.df)
    
    ## create file names
    file.name.net <- paste0(paste("wiod_long", year, sep = "_"), ".csv")
    file.name.VA <- paste0(paste("VA_long", year, sep = "_"), ".csv")
    file.name.dom.int <- paste0(paste("dom_int_trade_long", year, sep = "_"), ".csv")

    ## writing all dataframes
    write.files(net.long.df, file.name.net)
    write.files(VA.df, file.name.VA)
    write.files(dom.int.weights.df, file.name.dom.int)
    
    ## just printing where we are.
    print(paste("Year finished:", year), row.names = FALSE)
}

## creating directories to store downloaded and treated files
orginal.data.dir <- "./wiod_orginal_data"
dir.create(orginal.data.dir)

dir.to.write <<- "./wiod_long_data"
dir.create(dir.to.write)

## obtaining wiod files from the source
## this wiod file is the 2016 version, a zip file containing RData files.
data.url <- "http://www.wiod.org/protected3/data16/wiot_ROW/wiot_r_Nov16.zip"
dest.file <- paste(orginal.data.dir, "wiod_2016_release.zip", sep="/")
download.file(data.url, dest.file, mode="wb")

## extracting the zip file
unzip(dest.file, exdir=orginal.data.dir)

## creating the list of files
wiod.files <- list.files(orginal.data.dir, pattern="*.RData", full.names = TRUE)

## call all functions above with this line, creating a final long file
## wiod_long_YEAR.csv to perform network analysis.
lapply(wiod.files, transform.all.files)

