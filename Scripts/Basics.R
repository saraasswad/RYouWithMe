#Load packages ----------

library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(magrittr)

# Read in data --------

beaches <- readr::read_csv(here("data", "sydneybeaches.csv")) ##for multiple folders just add the folders in the respective order before the file name: here("folder1", "folder2", "filename.txt")
## readr:: specifies from which package the function is pulled if this specific function also exist in another package. 

#Exploring the data ---------

View(beaches)

dim(beaches) ## the table is of 3690 rows and 8 columns
str(beaches)

glimpse(beaches) ##does the same but puts it in a nicer table

head(beaches) ##to look at the top half a dozen rows

tail(beaches) ##to look at the bottom half a dozen rows 

skim(beaches) ## it separates character data from numeric or integer and shows the distribution of this data with a simple histogram at the end

#tidying columns-------
summary(beaches)
glimpse(beaches)

select_all(beaches, toupper) ##to uppercase 
##all the names of the columns (variable names) in Beaches and turn it into uppercase

select_all(beaches, tolower)

cleanbeaches <- clean_names(beaches) #it puts underscores 
##between words in the names of the variables

names(beaches) ##to list names of the variables
## we notice that the names of the variables does not change because what we have done so far is to just visualize the output
##the functions we used does not actually imply changes in the raw data
##to change the raw data we need to create a new object (will appear in the environment)
##we assign to the new object the function cleannames

names(cleanbeaches)

#select a subset of columns ## used when interested in a subset of variables
select(cleanbeaches, site, council, enterococci_cfu_100ml, everything()) ##lists the first three in their order then everythingelse

cleanbeaches <- rename(cleanbeaches, beachbugs = enterococci_cfu_100ml)
##to rename particular column in my dataframe 
## newname = old name
##assign an object to the rename function so it implies change in the raw data

#pipe %<%----

cleanbeaches <- beaches %>% ##don't forget to load the library magrittr before using the piping function 
  clean_names() %>%
  rename(beachbugs = enterococci_cfu_100ml) %>% # do not forget that you used clean names first so the name we want to rename should be in the newformat
  select(site, council, beachbugs)
  write_csv(cleanbeaches, "cleanbeaches.csv") ## create a new csv files with the selected variables or columns in it and name it "name.csv"
  ##the pipe function means take this data (beaches) and perform this function then this then this etc...
  ##newfolder containing cleanbeaches data will be created.

#which beach has the most extreme levels of bugs----
worstbugs <- cleanbeaches %>% arrange(desc(beachbugs))##take the data from cleanbeaches and arrange it according to beachbugs in a descending order
  ##type (desc(beachbugs)) or just (-beachbugs)
  
worstcoogee <- cleanbeaches %>%
  filter(site == "Coogee Beach") %>% ##filter to site coogee only
  arrange (-beachbugs)

#comparing the highest bug levels across different beaches
coogee_bondi <- cleanbeaches %>%
  filter(site %in% c("Coogee Beach", "Bondi Beach")) %>% # %in% means at those sights
  arrange(-beachbugs)
  
cleanbeaches %>%
  filter(site %in% c("Coogee Beach", "Bondi Beach")) %>% ##we can remove this filter to summarize the whole data grouped by site.
  group_by(site) %>% # to summarize data in each site separately
  summarise(maxbug = max(beachbugs, na.rm = TRUE), # to remove NA data
            meanbug = mean(beachbugs, na.rm = TRUE),
            medianbug = median(beachbugs, na.rm = TRUE),
            sdbug = sd(beachbugs, na.rm = TRUE)) # standard deviartion
#summarize all the data----
cleanbeaches %>%
  group_by(site) %>% # to summarize data in each site separately
  summarise(maxbug = max(beachbugs, na.rm = TRUE), # to remove NA data
            meanbug = mean(beachbugs, na.rm = TRUE),
            medianbug = median(beachbugs, na.rm = TRUE),
            sdbug = sd(beachbugs, na.rm = TRUE))

#no grouping by site----
cleanbeaches %>%
  filter(site %in% c("Coogee Beach", "Bondi Beach")) %>%
  summarise(maxbug = max(beachbugs, na.rm = TRUE), # to remove NA data
            meanbug = mean(beachbugs, na.rm = TRUE),
            medianbug = median(beachbugs, na.rm = TRUE),
            sdbug = sd(beachbugs, na.rm = TRUE))
#NA not removed----
cleanbeaches %>%
  filter(site %in% c("Coogee Beach", "Bondi Beach")) %>% ##we can remove this filter to summarize the whole data grouped by site.
  group_by(site) %>% # to summarize data in each site separately
  summarise(maxbug = max(beachbugs), # to remove NA data
            meanbug = mean(beachbugs),
            medianbug = median(beachbugs),
            sdbug = sd(beachbugs)) # standard deviartion
#comparing councils----
cleanbeaches %>% group_by(council)
cleanbeaches %>% distinct(council) # to know the different councils that exist
councilbysite <- cleanbeaches %>% 
  group_by(council, site) %>%
  summarise(meanbug = mean(beachbugs, na.rm = TRUE),
            medianbugs = median(beachbugs, na.rm = TRUE))
#Computing new variables (new columns)----
glimpse(beaches)
cleanbeacheswithoutselect <- beaches %>% 
  clean_names() %>%
  rename(beachbugs = enterococci_cfu_100ml) ##because the file got messed up

testdate <- cleanbeacheswithoutselect %>% ## remove is used to not remove the original columns (variables) before changing them
  separate(date, c("day","month","year"), remove = FALSE) %>%
  unite(council_site, council:site, remove = FALSE)
View(testdate)

#use mutate to transform the beachbugs data----
summary(cleanbeacheswithoutselect)
##we see that the values are hard to understand so we use mutate to create a new variable (column) that is the log values that are easier to cmprehend
cleanbeacheswithoutselect %>% mutate(beachbugslog = log(beachbugs))

#use mutate to compute new numeric values----
cleanbeacheswithoutselect %>% mutate(beachbugsdiff = beachbugs - lag(beachbugs))
##the 1st value is NA becuase the lag means one value back, so 19 - nothing is NA
##log is log base 10 for (x)

#use mutate to compute new logical values----
cleanbeacheswithoutselect %>% mutate (buggier = beachbugs > mean(beachbugs, na.rm = TRUE))

meanbugs = mean(cleanbeacheswithoutselect$beachbugs, na.rm = TRUE)
##dataframe name $name of the variable, allows me to pull this column from this specific data frame
##meanbug = just assigns a value to meanbugs from the dataframe but does not create a new dataframe
##we did this step to make sure the mutate logical step makes sense, and it does since 97>33.924 = true

cleanbeaches_new <- cleanbeacheswithoutselect %>%
  separate(date, c("day", "month", "year"), remove = FALSE) %>%
  mutate(logbeachbugs = log(beachbugs)) %>%
  mutate(beachbugsdiff = beachbugs - lag(beachbugs)) %>%
  mutate(beachbugs_all = beachbugs > mean(beachbugs, na.rm=TRUE)) %>%
  group_by(site) %>% 
  mutate(beachbugs_site = beachbugs > mean(beachbugs, na.rm=TRUE))
##the last two lines were used to tell us whether the reading is greater than the average for that site because we used the function grouped by site which basically mean that the following operation will be conducted on data from each site separately
##the fifth line tells us whether the reading is greater than the average across all the sites
