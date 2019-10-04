## in this file wiot long format network data is used to create network
## scores for each country.industry node. Then the network score data
## for each country.industry node is merged with VA and domestic and
## international final consumption data.

yearly.net.calc <- function(wiod.yearly.long) {
    ## making yearly network calculations
    yearly.wiod <- wiod.yearly.long %>% filter(weight > 0.05)

    wiod.nodes.t <- yearly.wiod %>% select(target) %>% unique %>% transmute(country.ind=target)
    wiod.nodes.f <- yearly.wiod %>% select(source) %>% unique %>% transmute(country.ind=source)
    wiod.nodes <- rbind(wiod.nodes.t, wiod.nodes.f) %>% unique

    g <- graph.data.frame(yearly.wiod, directed = TRUE, vertices=wiod.nodes)

    ## node strength
    strength.all <- strength(g, vids = V(g), mode = c("all"), loops = TRUE)
    strength.all <- rownames_to_column(as.data.frame(strength.all), var = "country.ind")
    strength.all[is.na(strength.all)] <- 0
    
    strength.out <- strength(g, vids = V(g), mode = c("out"), loops = TRUE)
    strength.out <- rownames_to_column(as.data.frame(strength.out), var = "country.ind")
    strength.out[is.na(strength.out)] <- 0
    
    strength.in <- strength(g, vids = V(g), mode = c("in"), loops = TRUE)
    strength.in <- rownames_to_column(as.data.frame(strength.in), var = "country.ind")
    strength.in[is.na(strength.in)] <- 0
    
    ## betweenness
    btw <- betweenness(g, v = V(g), directed = TRUE)
    btw <- rownames_to_column(as.data.frame(btw), var = "country.ind")
    btw[is.na(btw)] <- 0
    
    ## page.rank
    page.rank <- page_rank(g, damping = 0.999)$vector
    page.rank <- rownames_to_column(as.data.frame(page.rank), var = "country.ind")
    page.rank[is.na(page.rank)] <- 0
    
    ## eigen centrality
    eigen.cent <- eigen_centrality(g, directed = TRUE)$vector
    eigen.cent <- rownames_to_column(as.data.frame(eigen.cent), var = "country.ind")
    eigen.cent[is.na(eigen.cent)] <- 0
    
    net.scores <- Reduce(function(x, y) merge(x = x, y = y, by = "country.ind",
                                              all.x = TRUE),
                         list(wiod.nodes, strength.all,
                              strength.out, strength.in, btw,
                              page.rank, eigen.cent))
    
    return(net.scores)
}

create.yearly.net.files <- function(file.name, dir.to.write) {
    ### this function call the yearly.network function to make yearly
    ### network calculations and print the yearly result to dir.to.write
    ### directory.

    yearly.long.data <- read.csv(file.name)
    year <- substr(file.name, 28, 31)

    ## calculating the network score for the long file 
    net.scores <- yearly.net.calc(yearly.long.data)
        
    ## create file names
    file.name <- paste0(paste("wiod_network_scores", year, sep = "_"), ".csv")

    dir.file.name <- paste(dir.to.write, file.name, sep="/")
    ## write files with year
    write.csv(net.scores, dir.file.name, row.names = FALSE)

    ## just printing where we are.
    print(paste("The network file for the year", year, "is ready."), row.names = FALSE)
}

long.data.dir <<- "./wiod_long_data"
network.data.dir <<- "./wiod_network_data"
dir.create(network.data.dir)

## list of all long files obtained from wiod_data_prep.R
wiod.long.files <- list.files(path=long.data.dir, pattern=glob2rx('wiod_long*.csv'),
                              full.names = TRUE)

## call all functions and obtain yearly network score files
lapply(wiod.long.files, create.yearly.net.files, dir.to.write=network.data.dir)



bind_files <- function(year) {
    ## bind the network scores, VA values and domestic and internation
    ## trade based on year and country.industry level.
    
    net.score.file <- paste0(network.data.dir, "/wiod_network_scores_", year, ".csv")
    dom.int.trade.file <- paste0(long.data.dir, "/dom_int_trade_long_", year, ".csv")
    VA.file <- paste0(long.data.dir, "/VA_long_", year, ".csv")
        
    net.score.df <- read.csv(net.score.file)
    dom.int.trade.df <- read.csv(dom.int.trade.file)
    VA.df <- read.csv(VA.file)
    
    ## from the longest one to the smallest df.  dom.int.trade.df
    ## comprises final consumption per country, (for example AUS.Z)
    yearly.net.score.dom.int.VA.df <- net.score.df %>% left_join(dom.int.trade.df) %>%
        left_join(VA.df)

    ## yearly.net.score.dom.int.VA.df$year <- year
    
    yearly.net.score.dom.int.VA.df  <- add_column(yearly.net.score.dom.int.VA.df,
                                                  year = year, .after = "country.ind")
    
    file.name <- paste0(yearly.net.dom.int.VA.dir, "net_score_dom_int_VA_", year, ".csv")

    write.csv(yearly.net.score.dom.int.VA.df, file.name, row.names = FALSE)
}

## creating the directory where all yearly binded network score and VA
## value files are stored
yearly.net.dom.int.VA.dir <<- "./yearly_net_dom_int_VA/"
dir.create(yearly.net.dom.int.VA.dir)

## creating all yearly binded network score and VA value files and
## stored at yearly.net.VA.dir
lapply(seq(2000,2014), bind_files)

## obtaining the last file to use in econometric study.
## rbinding all yearly network score VA value files
final.wiod.df <- do.call(rbind,
                         lapply(list.files(path = yearly.net.dom.int.VA.dir, full.names = TRUE),
                                read.csv))

## reading the patent count data obtained from patstat
patstat.count.df <- read.csv("./patstat_manuf/country_ind_yearly_pat_tech_sum.csv",
                             stringsAsFactors = FALSE)

## joining wiod data and the patstat data.
final.df <- left_join(final.wiod.df, patstat.count.df,
                  by = c("country.ind" = "country.ind", "year" = "appln_filing_year"))

## changing all NAs in patent info (pat_tech_sum) into zeros.
final.df$pat_tech_sum[is.na(final.df$pat_tech_sum)]  <- 0

write.csv(final.df, "wiod_manuf_net_VA_patent.csv", row.names = FALSE)

## END of data preparation. Continue with data analysis.
