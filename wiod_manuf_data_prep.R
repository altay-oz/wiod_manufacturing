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

data.prep <- function(yearly.raw.data, dir.to.write) {
    ## this function prapares the yearly raw data to be used in the next
    ## function to get a long data. It also creates a yearly VA data.     
    
    ## obtaining the year information to be used in files naming
    year <- yearly.raw.data$YEAR
    
    ## get the industry and RNr code to use in renaming columns. colname = XXX.indshort.number
    industry.RNr <- yearly.raw.data %>% select(RNr, IndustryCode) %>% unique

    ## adding 57 to 61 the code Z. this is done to group them later
    ## 57 to 61 are final consumption
    ## we are using/keeping it for network score calculations.
    RNr <- c(57:61)
    IndustryCode <- c("Z.1", "Z.2", "Z.3", "Z.4", "Z.5")
    df <- data.frame(RNr, IndustryCode)

    industry.RNr <- rbind(industry.RNr, df)

    ## changing the name of the manufacturing industry ISIC code into
    ## their technology intensity. Then we will aggregate according to
    ## their technology intensity.
    IndustryCode <- c("C10-C12","C13-C15","C16","C17","C18","C19","C20","C21","C22","C23","C24",
                "C25","C26","C27","C28","C29","C30","C31_C32","C33")
    tech.type <- c("Low Tech.1","Low Tech.2","Low Tech.3",
                "Low Tech.4","Low Tech.5",
                "Medium-Low Tech.1","Medium-High Tech.1",
                "High Tech.1","Medium-Low Tech.2",
                "Medium-Low Tech.3","Medium-Low Tech.4",
                "Medium-Low Tech.5","High Tech.2",
                "Medium-High Tech.2","Medium-High Tech.3",
                "Medium-High Tech.4","Medium-High Tech.5",
                "Low Tech.6","Low Tech.7")
    man.table <- data.frame(IndustryCode, tech.type, stringsAsFactors = FALSE)

    industry.RNr  <- left_join(industry.RNr, man.table, by = "IndustryCode")

    industry.RNr %<>% mutate(IndustryCode = ifelse(!is.na(tech.type),
                                                   tech.type, IndustryCode)) %>% select(-tech.type)

    names(industry.RNr) <- c("RNr", "NewIndustryCode")

    ## changing the IndustryCode column wihtin the main df
    yearly.raw.data %<>% left_join(industry.RNr, yearly.raw.data, by = c("RNr"))

    yearly.raw.data %<>% mutate(IndustryCode = NewIndustryCode) %>% select(-NewIndustryCode)

    ## change the col.names for the intermediate production AUS24 -> AUS.C.24
    ## change the col.names for the final production AUS57 -> AUS.Z.1 : AUS61 -> AUS.Z.5
    colname.df <- as.data.frame(names(yearly.raw.data))
    names(colname.df) <- "colnames"

    colname.df <- transform(colname.df, country=substr(colnames, 1, 3),
                            ind.code=substr(colnames, 4, 5))

    ## emptying cells which are not relevant to any contry such as
    ## IndustryCode, IndustryDesctiption etc.
    colname.df[2690, 2:3 ] <- NA
    colname.df[1:5,2:3 ] <- NA

    colname.df$ind.code <- as.numeric(as.character(colname.df$ind.code))

    names(industry.RNr) <- c("RNr", "IndustryCode")
    
    new.col.name <- left_join(colname.df, industry.RNr, by = c("ind.code" = "RNr")) %>%
        unite(new.col.name, country, IndustryCode, sep=".") %>% select(new.col.name) %>% as.list

    names(yearly.raw.data)[6:2689] <- new.col.name$new.col.name[6:2689]

    ## obtaining VA of all industries.
    ## selecting year and all industries, Z columns are all 0, getting rid of them
    ## This file will contain columns such as "Medium-High Tech.3","Medium-High Tech.4",
    ## they should be aggregated
    yearly.VA.row <- yearly.raw.data %>% filter(IndustryCode == "VA") %>% select(5:2469)
    yearly.VA.df <- gather(yearly.VA.row, country.ind, VA, AUS.A01:ROW.U, factor_key=TRUE)
    
    ## remove TOT row.
    ## remove the last section, all rows without any sectoral information
    yearly.raw.data %<>% filter(Country!="TOT") %>% filter(RNr <= 56)
    
    yearly.raw.data <- unite(yearly.raw.data, "country.ind", c("Country", "IndustryCode"),
                               sep = ".", remove = TRUE)

    yearly.raw.data %<>% select(-c("IndustryDescription", "RNr", "Year"))
    
    return(list(yearly.raw.data, yearly.VA.df))
}

create.long.df <- function(yearly.data) {
    ## function which aggregates all types of final consumption into Z.
    ## and cretes long table (Source - Target - Weight) to be used in network analysis.

    yearly.melted.data <- yearly.data %>% melt(id.vars = "country.ind")

    ## variable is the new column made of column name of the Target, and
    ## obtaining Z and melted data
    yearly.melted.data.z <- yearly.melted.data %>%
        separate(variable, c("target.country", "target.ind", "target.z.cat"), sep = "\\.",
                 fill = "right") %>%
        filter(target.ind == "Z") %>% 
        separate(country.ind, c("source.country", "source.ind", "source.tech.cat"), sep = "\\.",
                 fill = "right") %>% select(-c("source.tech.cat","target.z.cat")) 

    yearly.z <- yearly.melted.data.z %>%
        group_by(source.country, source.ind, target.country, target.ind) %>%
        summarise(Weight = sum(value)) %>% as.data.frame

    yearly.z %<>% unite(Source, c("source.country", "source.ind"), sep = ".") %>%
        unite(Target, c("target.country", "target.ind"), sep = ".")

    ## aggregating according the Low Tech etc in target and source
    ## dividing the Target column into country|ind|z.cat and making their sum
    yearly.melted.wo.z <- yearly.melted.data %>%
        separate(variable, c("country", "ind", "z.cat"), sep = "\\.", fill = "right") %>%
        filter(! ind == "Z") %>% select(-c("z.cat")) 

    yearly.melted.wo.z.1 <- yearly.melted.wo.z %>% group_by(country.ind, country, ind) %>%
        summarise(tot.1.value = sum(value)) %>%
        unite(Target, c("country", "ind"), sep = ".") %>% as.data.frame
    
    yearly.melted.wo.z.2 <- yearly.melted.wo.z.1 %>%
        separate(country.ind, c("country", "ind", "z.cat"), sep = "\\.", fill = "right") %>%
        select(-c("z.cat")) %>% 
        group_by(country, ind, Target) %>%
        summarise(Weight = sum(tot.1.value)) %>%
        unite(Source, c("country", "ind"), sep = ".") %>% as.data.frame

    yearly.long.network.table <- rbind(yearly.melted.wo.z.2, yearly.z)

    return(yearly.long.network.table)
}

dom.int.trade <- function(net.long.df) {
    ## inserting a long table of network realtion (source/target/weight)
    ## and obtaining the weight for each node's (country.ind)
    ## international and domestic trade.

    ## obtaining all nodes for making a left join at the end
    source <- net.long.df %>% select(Source) %>% unique
    target <- net.long.df %>% select(Target) %>% unique

    names(source) <- "country.ind"
    names(target) <- "country.ind"

    all.nodes <- rbind(source, target) %>% unique
    
    ## adding two columns source.country and target.country
    net.long.df %<>% mutate(source.country=substr(Source, 1,3)) %>% 
        mutate(target.country=substr(Target, 1,3))

    ## summing the the weights according to target country domestic
    dom.out <- net.long.df %>% filter(source.country == target.country) %>%
        group_by(Source) %>% summarise(dom.out.weight = sum(Weight)) 
    names(dom.out)[1] <- "country.ind"
    
    ## summing the the weights according to target country international
    int.out <- net.long.df %>% filter(source.country != target.country) %>%
        group_by(Source) %>% summarise(int.out.weight = sum(Weight)) 
    names(int.out)[1] <- "country.ind"
    
    ## summing the the weights according to target country domestic
    dom.in <- net.long.df %>% filter(source.country == target.country) %>%
        group_by(Target) %>% summarise(dom.in.weight = sum(Weight)) 
    names(dom.in)[1] <- "country.ind"
    
    ## summing the the weights according to target country international
    int.in <- net.long.df %>% filter(source.country != target.country) %>%
        group_by(Target) %>% summarise(int.in.weight = sum(Weight)) 
    names(int.in)[1] <- "country.ind"
    
    dom.int.weights <- all.nodes %>% left_join(dom.out) %>% left_join(int.out) %>%
        left_join(dom.in) %>% left_join(int.in)

    return(dom.int.weights)
}

write.files <- function(df, file.name) {
    dir.file <- paste(dir.to.write, file.name, sep="/")
    write.csv(df, dir.file, row.names = FALSE)
}

transform.all.files <- function(file.name) {
    ## insert a RDATA WIOT file and obtain a long data
    ## this function uses the above functions

    yearly.raw.data <- get(load(file.name))
    
    year <- substr(file.name, 25, 28)

    ## obtaining a list of two data.frame from the data.prep function
    prepared.data.list <- data.prep(yearly.raw.data)

    ## the first data.frame will be used obtaining long.df for network analysis
    yearly.wiod.df <- prepared.data.list[[1]]
    ## the second data.frame is yearly VA values for each country.industry 
    yearly.VA.df <- prepared.data.list[[2]]

    ## calling the function create.long.df to prepare the long tables
    ## for network analysis
    net.long.df <- create.long.df(yearly.wiod.df)

    ## domestic and internation trade for each country.industry
    dom.int.weights <- dom.int.trade(net.long.df)
    
    ## create file names
    file.name.net <- paste0(paste("wiod_long", year, sep = "_"), ".csv")
    file.name.VA <- paste0(paste("VA_long", year, sep = "_"), ".csv")
    file.name.dom.int <- paste0(paste("dom_int_trade_long", year, sep = "_"), ".csv")

    ## writing all dataframes
    write.files(net.long.df, file.name.net)
    write.files(yearly.VA.df, file.name.VA)
    write.files(dom.int.weights, file.name.dom.int)
    
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


