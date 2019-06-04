download.file(url = "https://ndownloader.figshare.com/files/2292169",
              destfile = "data/portal_data.csv")
#initial read of surveys data
#read csv file (local - use a relative path)
surveys <- read.csv("data/portal_data.csv")
#get first six lines
head(surveys)
str(surveys)
dim(surveys)
nrow(surveys)
ncol(surveys)
summary(surveys)
sex <- surveys$sex
nlevels(sex)
year <- surveys$year
year_factor <- as.factor(year)
levels(year_factor)
head(surveys)

library(lubridate)
surveys$date <- ymd(paste(surveys$year,surveys$month,surveys$day, sep="-"))
summary(surveys$date)

library(tidyverse)
is.na(surveys$date) #get true/false of whether date is NA - returns vector
head(filter(surveys, is.na(date))) #check why there are NA in date - note 31Sep
#need to filter weird dates - see below

#CHALLENGE: filter out data (and make a new dataset) where the date is wrong
surveys_gooddates <- filter(surveys,!is.na(date))

#CHALLENGE: filter data for rows where year == 1995
filter(surveys, year == 1995)

#hard ways to read - functions inside functions
head(select(surveys, species_id, weight))
#easier way - use pipes to send data to next command
surveys %>% head() #same as head(surveys)
surveys %>% filter(is.na(date)) %>% head()
surveys %>% filter(is.na(date)) %>% select(year,month,day) %>% head()

surveys_weightkg <- surveys %>% 
  filter(is.na(date)) %>% 
  mutate(weight_kg = weight/1000)

#Challenge: get a clean dataset (for plotting)
#filter out cases where weight, hindfoot_length, or sex are NA
surveys_complete <- surveys %>%
  filter(!is.na(weight),
         !is.na(hindfoot_length),
         !is.na(sex),
         sex != "")

#drop all NAs - drop_na from tidyr  
surveys_complete2 <- surveys %>% drop_na()

#get just species that are common
#1. find common species - more than 50 samples
species_common <- surveys_complete2 %>% count(species_id) %>%
  filter(n>50)
#2. filter whole data frame based on whether species
# is in list of common species
species_complete_common <- surveys_complete2 %>%
  filter(species_id %in% species_common$species_id)

write_csv(species_complete_common,
          path = "processed_data/surveys_complete_common.csv")

#load data w read_csv
surveys_complete_common <- read_csv("processed_data/surveys_complete_common.csv")

#basic gglot with color in aes in geom_point layer only
ggplot(data = surveys_complete_common,
       aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = .1, aes(color = species_id))
#can also put color in aes in main plot

#color in geom_point but not aes
ggplot(data = surveys_complete_common,
       aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = .1, color = "blue")

plot_weight_hind <- ggplot(data = surveys_complete_common,
       aes(x = weight, y = hindfoot_length))

plot_weight_hind + 
  geom_point(aes(color=species_id))

#Challenge: plot weight as a function of species_id
ggplot(surveys_complete_common,
       aes(x = species_id, y = weight)) +
  geom_jitter(alpha = .1, 
              aes(color = plot_type)) + 
  geom_violin()

#Challenge:
#how many of each species are caught each year

#plot the number of each spp as a function of time
