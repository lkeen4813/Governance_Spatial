########################################################################

#   Author(s): Laura Keen
#   Date created: 07 May 2019
#   Date last modified: 07 May 2019
#   Files used:
#     - data_raw/Afrobarometer
#   Files created:
#     - data_intermediate/Afrobarometer

#  Country: Kenya

########################################################################

library(dplyr)
library(foreign)
library(tidyverse)
library(lubridate)
library(broom)

# File path
afrob <- "./data/data_raw/Afrobarometer"

# Do you want to save the results?
save <- TRUE

if (save) {out <- "./data/data_intermediate/Afrobarometer"}

########################################################
# Load Data and Set Parameters
########################################################

afrob.files <- list.files(afrob, pattern = "[2-7]",full.names = TRUE) # round 1 survey is excluded
roundkeys <- paste0("r", c(2:7))

ctry <- "Kenya" # which country are you analyzing?

########################################################
# Iterate over the survey rounds
########################################################

results <- as.data.frame(matrix(nrow = 0, ncol = 0)) # dataframe to save statistics summaries
regions_un <- as.data.frame(matrix(nrow = 0, ncol = 0)) # dataframe to save region names per survey round

for (i in roundkeys){
  
  round <- list.files(afrob, pattern = i,full.names = TRUE) %>% read.spss(to.data.frame = TRUE) 
  colnames(round) <- colnames(round) %>% tolower() # change colnames to lowercase
  
  country.exist <- unique(round[["country"]]) %>% {grepl(ctry, .)} %>% sum() #is your country included in this survey round?
  
  if(country.exist == 1) {
    
    r <- gsub("\\D", "", i) # extract country round
    
    n_duplicates <- round[["respno"]] %>% duplicated() %>% sum() # check duplicates
    
    cat("\n", ctry, "in Round", i, 
        "\n", "Number of Duplicates:", n_duplicates, "\n") # print duplicate check results
    
    
    round <- round %>% 
      filter(country == ctry) %>%  # only keep your country
      mutate(dateintr = as.Date(dateintr/86400, origin = "1582-10-14")) # convert SPSS date to calendar date
    
    # interview time variabes
    inter_start <- round$dateintr %>% unique() %>% min() # when did the interview start?
    inter_end <- round$dateintr %>% unique() %>% max() # when did it end?
    inter_length <- difftime(inter_end, inter_start) # number of days it took
    
    # region/geolocation variables
    num_regions <- round[["region"]] %>% unique() %>% length() # how many regions are surveyed?
    
    regions <- round[["region"]] %>% unique() %>% 
      as.data.frame() %>% `colnames<-`("region") %>%
      mutate(region = tolower(region),
             region =gsub("[^a-z]|duplicated|duplicate", "", region)) # extract regions names and remove unrelated strings
    
    regions[[paste0("round", r)]] <- 1
    
    
    if(length(regions_un) == 0) {regions_un <- bind_cols(regions)}
    else {regions_un <- regions_un %>% full_join(regions)} 
    
    location_level_1 <- grepl("location", colnames(round)) %>% sum() # is there a location_level_1 variable?
    
    if(location_level_1 == 1){
      num_locations <- round %>% pull(colnames(round)[grep("location", colnames(round))]) %>% unique() %>% length()
    } else {num_locations = NA}
    
    # save your results
    country_results <- data.frame(ctry, r, inter_start, inter_end, inter_length, 
                                  num_regions, location_level_1, num_locations)
    results <- bind_rows(results, country_results)
    
    # NA Check
    threshold <- 0.1 * (dim(round)[2]) # using 10% threshold for NA check
    
    na_check <- round %>% 
      apply(2, function(x) sum(grepl("Missing", x))) %>% as.data.frame() %>%
      rownames_to_column() %>% `colnames<-`(c("var","num_na")) %>% 
      filter(num_na >= threshold) %>% pull(var) # variables have missing values greater than 10% of the observations
    
    if(length(na_check) > 0) {cat("\n", "Variables with NAs greater than 0.1 threshold:", na_check)}
    
  } else {cat("\n", ctry, "NOT in round", r, "\n")}
}


if (save) {
  write.csv(results, paste0(out, "/countrySummary_", ctry))
  write.csv(regions_un, paste0(out, "/countryRegionCheck_", ctry))
}

######################################################
# Extract data for Kenya from survey round 6
######################################################

afrob_r6 <- read.spss("./data/data_raw/Afrobarometer/merged_r6_data.sav", to.data.frame = TRUE) 

var.names<-tolower(colnames(afrob_r6)) # make column names lowercase
colnames(afrob_r6)<-var.names

kenya <- afrob_r6 %>% 
  filter(country == ctry) %>%  # only keep Kenya
  mutate(dateintr = as.Date(dateintr/86400, origin = "1582-10-14")) # change SPSS data format

n_duplicates <- kenya[["respno"]] %>% duplicated() %>% sum() # check duplicates

# drop unneccesary variables

kenya_gov <- kenya %>%
  select("respno", "country", "urbrur", "location.level.1", "adult_ct", "q1", "q2", "q45c", "q45d",
         "q45e", "q50", "q51c", "q52a","q52b", "q52c", "q52e", "q53a", "q53b", "q53c", "q53d", "q66a", "q66b", "q66c", "q66d", 
         "q66e", "q66f","q66g","q66h","q66i","q66j","q66k","q66l","q66m","q67a", "q67b", "q87", "q91d", "q95",
         "q96a", "q97", "q98a", "q101")        

kenya_gov$location.level.1 <- as.character(kenya_gov$location.level.1)

kenya_gov$location.level.1 <- trimws(kenya_gov$location.level.1, "r") # twim righthand white space

########################################################
# Merge with shapefile
########################################################

library(sf)

# first, make sure that your shapefile package has .shp, .shx, .dbf, .prj within the same folder

shapefile_ken <- st_read("data/data_raw/shapefiles/gadm36_KEN_1.shp")

# create key column in shapefile for merge

names(shapefile_ken) <- sub("^NAME_1$", "location.level.1", names(shapefile_ken))
shapefile_ken$location.level.1 <- as.character(shapefile_ken$location.level.1) # change factor to character for merge.
colnames(shapefile_ken) <- colnames(shapefile_ken) %>% 
  tolower()
shapefile_ken <- shapefile_ken %>%
  select(c("location.level.1", "geometry", "cc_1"))

# merge

shapefile.merge_ken <- full_join(kenya_gov, shapefile_ken, by = "location.level.1")

plot(shapefile_ken["geometry"])



# Save dataframe with shapefile         
write.csv(shapefile.merge_ken, paste0(out, "/shapefile.merge_kenya.csv"))

st_write(shapefile.merge_ken, paste0(out, "/shapefile.merge_kenya.shp"))

# Save unmerged shapefile in Shapefile folder
st_write(shapefile_ken, "./data/data_intermediate/shapefiles/kenya.shp")

tidy(lm(factor(q66a) ~ factor(q95) , data=kenya_gov))



