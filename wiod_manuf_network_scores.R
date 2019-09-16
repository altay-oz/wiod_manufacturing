## in this file wiot long format network data is used to create network
## scores for each country.industry node. Then the network score data
## for each country.industry node is merged with VA and domestic and
## international final consumption data.

yearly.net.calc <- function(wiod.yearly.long) {
    ## making yearly network calculations
    
    yearly.wiod <- wiod.yearly.long %>% filter(Weight > 0)

    wiod.nodes.t <- yearly.wiod %>% select(Target) %>% unique %>% transmute(country.ind=Target)
    wiod.nodes.f <- yearly.wiod %>% select(Source) %>% unique %>% transmute(country.ind=Source)
    wiod.nodes <- rbind(wiod.nodes.t, wiod.nodes.f) %>% unique

    g <- graph.data.frame(yearly.wiod, directed = TRUE, vertices=wiod.nodes)

    ## node strength
    strength.all <- strength(g, vids = V(g), mode = c("all"), loops = TRUE)
    strength.all <- rownames_to_column(as.data.frame(strength.all), var = "country.ind")
    
    strength.out <- strength(g, vids = V(g), mode = c("out"), loops = TRUE)
    strength.out <- rownames_to_column(as.data.frame(strength.out), var = "country.ind")
    
    strength.in <- strength(g, vids = V(g), mode = c("in"), loops = TRUE)
    strength.in <- rownames_to_column(as.data.frame(strength.in), var = "country.ind")

    ## betweenness
    btw <- betweenness(g, v = V(g), directed = TRUE)
    btw <- rownames_to_column(as.data.frame(btw), var = "country.ind")

    ## page.rank
    page.rank <- page_rank(g, damping = 0.999)$vector
    page.rank <- rownames_to_column(as.data.frame(page.rank), var = "country.ind")

    ## eigen centrality
    eigen.cent <- eigen_centrality(g, directed = TRUE)$vector
    eigen.cent <- rownames_to_column(as.data.frame(eigen.cent), var = "country.ind")

    ## random walk
    random.walk <- random_walk(g, start = 1, steps = 1000000)
    random.walk <- as.data.frame(table(attr(random.walk, "names")))
    names(random.walk)  <- c("country.ind", "random.walk")

    net.scores <- Reduce(function(x, y) merge(x = x, y = y, by = "country.ind",
                                             all.x = TRUE),
                        list(wiod.nodes, strength.all,
                             strength.out, strength.in, btw,
                             page.rank, eigen.cent, random.walk))
    
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
sapply(wiod.long.files, create.yearly.net.files, dir.to.write=network.data.dir)

bind_files <- function(year) {
    ## bind the network scores, VA values and domestic and internation
    ## trade based on year and country.industry level.
    
    VA.file <- paste0(long.data.dir, "/VA_long_", year, ".csv")
    net.score.file <- paste0(network.data.dir, "/wiod_network_scores_", year, ".csv")
    dom.int.trade.file <- paste0(long.data.dir, "/dom_int_trade_long_", year, ".csv")
    
    VA.df <- read.csv(VA.file)
    net.score.df <- read.csv(net.score.file)
    dom.int.trade.df <- read.csv(dom.int.trade.file)
    
    yearly.net.score.VA.dom.int.df <- VA.df %>% left_join(net.score.df) %>%
        left_join(dom.int.trade.df)
    
    yearly.net.score.VA.dom.int.dir.file.name <-
        paste0(yearly.net.VA.dom.int.dir, "net_score_VA_dom_int_", year, ".csv")

    write.csv(yearly.net.score.VA.dom.int.df,
              yearly.net.score.VA.dom.int.dir.file.name, row.names = FALSE)
}

## creating the directory where all yearly binded network score and VA
## value files are stored
yearly.net.VA.dom.int.dir <<- "./yearly_net_VA_dom_int/"
dir.create(yearly.net.VA.dom.int.dir)

## creating all yearly binded network score and VA value files and
## stored at yearly.net.VA.dir
lapply(seq(2000,2014), bind_files)

## obtaining the last file to use in econometric study.
## rbinding all yearly network score VA value files
final.wiod.df <- do.call(rbind,
                         lapply(list.files(path = yearly.net.VA.dom.int.dir, full.names = TRUE),
                                read.csv))

## reading the patent count data obtained from patstat
patstat.count.df <- read.csv("./patstat_manuf/country_ind_yearly_pat_tech_sum.csv")

## joining wiod data and the patstat data.
final.df <- left_join(final.wiod.df, patstat.count.df,
                  by = c("country.ind" = "country.ind", "Year" = "appln_filing_year"))

write.csv(final.df, "wiod_manuf_net_VA_patent.csv", row.names = FALSE)

nrow(final.df)

## END of data preparation. Continue with data analysis.
