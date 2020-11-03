
###### Download Wisconsin Absentee Ballot Data ######
## https://elections.wi.gov/publications/statistics/absentee

library(rvest)
library(openxlsx)
library(tidyverse)
library(geojsonio)
library(sf)
library(httr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# get the links to all the pages with absentee ballot data
page <- read_html("https://elections.wi.gov/publications/statistics/absentee")
links <- page %>%
  html_nodes(xpath = "//td/a") %>% 
  html_attr("href")

links_all <- page %>% 
  html_nodes("a") %>% 
  html_attr("href")
links <- substr(links_all, 7, 10)
#only keep those that are related the 2020 General Election and are static tables
links_most <- links[!is.na(as.numeric(links)) & (as.numeric(links)>=7071) & (as.numeric(links)<7196)]

the_df = data.frame(matrix(ncol = 6, nrow = 0))


# 7071 is the starting page with Absentee data for 2020 General Election
for (i in links_most) {
  print(i)
  if (http_status(GET(paste0("https://elections.wi.gov/index.php/node/",i)))$category != "Server error") {
  page <- read_html(paste0("https://elections.wi.gov/index.php/node/",i))
  # Scape the table with data
  try({ table <- page %>%
      html_nodes ("table") %>% 
      .[[1]] %>%
      html_table(fill = TRUE)}, silent=F)

   if (dim(table)[1]>70) {

      # adjust column names
      table <- table[-1,]
      if (length(colnames(table)) >4) {
        colnames(table) <- c("County", "Absentee Applications", "Ballots Reported Sent", "Ballots Reported Returned", "In-Person Absentee Ballots")
      }
      if (length(colnames(table)) >2 & length(colnames(table)) <= 4) {
        table <- cbind(table, data.frame(matrix(ncol = 1, nrow = dim(table)[1])))
        colnames(table) <- c("County", "Absentee Applications", "Ballots Reported Sent", "Ballots Reported Returned", "In-Person Absentee Ballots")
      }
      if (length(colnames(table)) ==2) {
        table <- cbind(table, data.frame(matrix(ncol = 3, nrow = dim(table)[1])))
        colnames(table) <- c("County", "Absentee Applications", "Ballots Reported Sent", "Ballots Reported Returned", "In-Person Absentee Ballots")
      }

      # Extract date of report
      page %>%
        html_text() -> text
      
      date_regex <- "[A-Z]{1}[a-z]{2},\\s[0-9]{2}/[0-9]{2}/[0-9]{2}"
      date <- regmatches(text, gregexpr(date_regex, text)) 
      # remove day of week
      date <- substr(date, 6, 13)
      
      table <- cbind(date = rep(date, dim(table)[1]), table)
      
      # remove "totals" row
      table <- table[table$County!="TOTAL",]
      the_df <- rbind(the_df,table)
  }
  }
}

# reset index
row.names(the_df) <- NULL

# webscrape csv data
# starting October 22, 2020, the WI Election Commission started reporting results
# in csv files rather than static tables on their website.
# This section scraps and adds this recent data.


links_pg_csv <- links[!is.na(as.numeric(links)) & (as.numeric(links)>=7196)]

for (i in links_pg_csv) {

  page <- read_html(paste0("https://elections.wi.gov/index.php/node/",i))
  
  # find link to csv on webpage
  links_all <- page %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  csv_link <- links_all[grepl(x=links_all, pattern = "ounty")]
  
  if (length(csv_link)!=0) {
    
      if (str_sub(csv_link,-4,-1)=="xlsx") {
        p <- rio::import(csv_link)
      }
      
      else {
        # pull in data from the csv County file
        p <- read_csv(csv_link)
      }
      
      # Extract date of report
      page %>%
        html_text() -> text
      
      date_regex <- "[A-Z]{1}[a-z]{2},\\s[0-9]{2}/[0-9]{2}/[0-9]{2}"
      date <- regmatches(text, gregexpr(date_regex, text)) 
      # remove day of week
      date <- substr(date, 6, 13)
      print(date)
      # add date column to dataframe
      p<- cbind(data.frame("date" = rep(date, times=dim(p)[1])), p)
      
      
      # rename Jurisdiction to County
      colnames(p)[colnames(p)=="Jurisdiction"] <- "County"
      
      
      p <- p %>%
        select(!c(starts_with("Elect"), "HINDI"))
      
      
      # remove "totals" row
      p <- p[p$County!="TOTAL",]
      
      colnames(p) <- colnames(the_df)
      # add to complete dataframe
      the_df <- rbind(the_df,p)
      
  }
}



###### what proportion of those registered to vote requested absentee? ######

## voter registration data page: https://elections.wi.gov/publications/statistics/registration
## voter registration data as of Sept. 1: 2020 https://elections.wi.gov/node/7070
## voter registration data as of Oct. 1: 2020 https://elections.wi.gov/node/7147
reg_voter <- read.xlsx("https://elections.wi.gov/sites/elections.wi.gov/files/2020-10/RegisteredVotersByCounty_10-01-2020.xlsx", colNames=F)

voter_reg_county <- data.frame(County = the_df[1:72,]$County,
                                reg_voter = reg_voter[-73,])

# join voter registration data
the_df <- merge(x = the_df, y = voter_reg_county, by = "County", all.x = TRUE)

the_df <- the_df %>%
  arrange(date)



### what proportion of the voting age population in each county is registered to vote?
### what proportion of the citizen voting age population in each county is registered to vote?
# TOTAL POP, VAP, CVAP data: https://www.census.gov/programs-surveys/decennial-census/about/voting-rights/cvap.2018.html 
cvap <- read_csv("./raw-data/CVAP_2014-2018_ACS_csv_files/County.csv")

cvap <- cvap %>%
  filter(lntitle=="Total", str_detect(geoname, 'Wisconsin')) %>%
  select(geoname, tot_est, adu_est, cvap_est)
  
names(cvap) <- c("County", "total_est", "vap_est", "cvap_est")

cvap <- cvap %>%
  mutate(County = substr(County,1,nchar(County)-11)) %>%
  mutate_if(is.character, str_to_upper) 


# join with voter registration data
the_df <- merge(x = the_df, y = cvap, by = "County", all.x = TRUE)


# change column types to numeric
cols.num <- c("Absentee Applications","Ballots Reported Sent", "Ballots Reported Returned","In-Person Absentee Ballots",
              "reg_voter", "total_est", "vap_est", "cvap_est")

the_df[cols.num] <- map(the_df[cols.num], as.numeric)

# remove in-person counts double counts from Absentee Application and Ballots Reported Returned
the_df <- the_df %>%
  mutate(`Absentee Applications` = ifelse(is.na(`In-Person Absentee Ballots`), `Absentee Applications`,
                                          `Absentee Applications` - `In-Person Absentee Ballots`),
         `Ballots Reported Sent` = ifelse(is.na(`In-Person Absentee Ballots`), `Ballots Reported Sent`,
                                          `Ballots Reported Sent` - `In-Person Absentee Ballots`),
        `Ballots Reported Returned` = ifelse(is.na(`In-Person Absentee Ballots`), `Ballots Reported Returned`,
                                              `Ballots Reported Returned` - `In-Person Absentee Ballots`))



# changes in the past day
the_df <- the_df %>%
  arrange(County, date) %>% 
  group_by(County) %>%
  mutate(oneday_absentee_app = c(0,diff(`Absentee Applications`)),
         oneday_ballot_sent = c(0,diff(`Ballots Reported Sent`)),
         oneday_ballot_returned = c(0,diff(`Ballots Reported Returned`)),
         oneday_absentee_person = c(0,diff(`In-Person Absentee Ballots`)),
         oneday_absentee_app_prop = (`Absentee Applications` - lag(`Absentee Applications`))/lag(`Absentee Applications`),
         oneday_ballot_sent_prop = (`Ballots Reported Sent` - lag(`Ballots Reported Sent`))/lag(`Ballots Reported Sent`),
         oneday_ballot_returned_prop = (`Ballots Reported Returned` - lag(`Ballots Reported Returned`))/lag(`Ballots Reported Returned`),
         oneday_absentee_person_prop = (`In-Person Absentee Ballots` - lag(`In-Person Absentee Ballots`))/lag(`In-Person Absentee Ballots`),
  ) %>%
  ungroup()


  
  # add week marks
the_df <- the_df %>%
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
  mutate(week = cut.Date(date, breaks = "1 week", labels = FALSE)) 

# changes in the past week
the_df <- the_df %>%
  arrange(County, date) %>% 
  group_by(County, week) %>%
  mutate(oneweek_absentee_app = sum(oneday_absentee_app),
         ooneweek_ballot_sent = sum(oneday_ballot_sent),
         oneweek_ballot_returned = sum(oneday_ballot_returned),
         oneweek_absentee_person = sum(oneday_absentee_person)
  ) %>%
  ungroup()


# % change in absentee ballot requests from the week before
the_df %>%
  select(County, week, oneweek_absentee_app) %>%
  distinct() %>%
  group_by(County) %>% mutate(lag.value = lag(oneweek_absentee_app, n=1)) -> lagged
 

the_df %>%
  left_join(lagged, by=c('County','week')) %>%
  mutate(oneweek_absentee_app_prop = (oneweek_absentee_app.x - lag.value)/lag.value) -> the_df



# example
# the_df %>%
#   select(`Absentee Applications`, oneday_absentee_app, oneweek_absentee_app, week)



the_df <- the_df %>%
  # change from the day before
  group_by(County, date) %>%
          # what proportion of those registered to vote requested absentee?
  mutate(prop_reg_absentee_applied = round(`Absentee Applications` / reg_voter,2),
         # what proportion of those registered to vote requested and voted in-person absentee?
         prop_reg_absentee_person = round(`In-Person Absentee Ballots` / reg_voter,2),
        # what proportion of the voting age population in each county is registered to vote?
        prop_vap_reg = round(reg_voter / vap_est,2),
        # what proportion of the citizen voting age population in each county is registered to vote?
        prop_cvap_reg = round(reg_voter / cvap_est,2),
        # proportion of absentee ballots sent
        prop_ballot_sent = round(`Ballots Reported Sent` / `Absentee Applications`,2),
        # prop applied that have been returned
        prop_ballot_returned = round(`Ballots Reported Returned` / `Absentee Applications`,2),
        ) %>%
  ungroup() %>%
  # change from the weeks before
  group_by(County, week) %>%
          # what proportion of those registered to vote requested absentee?
  mutate(prop_reg_absentee_applied_wk = round(`Absentee Applications` / reg_voter,2),
         prop_reg_absentee_person_wk = round(`In-Person Absentee Ballots` / reg_voter,2),
         # proportion of absentee ballots sent
         prop_ballot_sent_wk = round(`Ballots Reported Sent` / `Absentee Applications`,2),
         # prop applied that have been returned
         prop_ballot_returned_wk = round(`Ballots Reported Returned` / `Absentee Applications`,2),
  ) 

# add statewide total number of requests
the_df %>%
  group_by(County) %>%
  slice(which.max(as.Date(date, '%m/%d/%Y'))) %>% ungroup() %>%
  mutate(totala_abrequests = sum(`Absentee Applications`, `In-Person Absentee Ballots`)) %>% select(totala_abrequests) %>%
  pull() -> total_absentee_requests

the_df$total_absentee_requests <- rep(unique(total_absentee_requests), times=dim(the_df)[1])

write.csv(the_df,"./output/data.csv", row.names = FALSE)


#### add data to county map shapefile
wi_shp <- st_read("raw-data/wi_counties/wi_counties.shp")
wi_shp$NAME <- paste(str_to_upper(wi_shp$NAME), "COUNTY")

# join shapefile and data
wi_map <- merge(wi_shp, the_df, by.x = "NAME", by.y = "County")
wi_map_json <- geojson_json(wi_map)
geojson_write(wi_map_json, file = "output/wi_counties.geojson")





