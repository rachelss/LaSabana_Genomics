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
yearly_counts <- surveys_complete_common %>%
  count(year,species_id)
#plot the number of each spp as a function of time
ggplot(yearly_counts,
       aes(x = year, y = n, color = species_id))+
  geom_line()# + facet_wrap(~species_id)

#plot each species with sex separately
yearly_counts_sex <- surveys_complete_common %>%
  count(year,species_id,sex)
plot_year_count_sex <- ggplot(yearly_counts_sex,
       aes(x = year, y = n, color = sex))+
  geom_line() + facet_wrap(~species_id) +
  theme_bw() +
  theme(panel.grid = element_blank())
ggsave(plot = plot_year_count_sex, width = 9, height = 6,
       filename = "figures/plot_year_count_sex.pdf")

yearly_counts_sex$species_id <- as.factor(yearly_counts_sex$species_id)
levels(yearly_counts_sex$species_id) <- c("DM2", "DO2", "DS2", "NL2", "OL2", "OT", "PB", "PE", "PF", "PM", "PP", "RF", "RM", "SH")
plot_year_count_sex <- ggplot(yearly_counts_sex,
                              aes(x = year, y = n, color = sex))+
  geom_line() + facet_wrap(~species_id) +
  theme_bw() +
  theme(panel.grid = element_blank())

#Day 2 of R
#Challenge: how many individuals were caught in each plot_id
surveys %>% count(plot_id)

#get mean weight for each species
#this works for some species but produces NaN for others
#surveys %>% group_by(species_id) %>%
#  summarise(mean_weight = mean(weight, na.rm = TRUE))

#how do we investigate NaN? pick one species w NaN for weight
#get weights - observe NA - should filter in advance
#filter(surveys,species_id=="CB") %>% select(weight) %>%
#  unique()

surveys %>% filter(!is.na(weight)) %>%
  group_by(species_id) %>%
  summarise(mean_weight = mean(weight))

surveys %>% filter(!is.na(weight)) %>%
  group_by(species_id, sex) %>%
  summarise(mean_weight = mean(weight),
            min_weight = min(weight),
            max_weight = max(weight)) %>%
  arrange(desc(mean_weight))

#Challenge
#find mean, min, max hindfoot_length for each species_id
surveys %>% filter(!is.na(hindfoot_length)) %>%
  group_by(species_id) %>%
  summarise(mean_hindfoot_length = mean(hindfoot_length),
            min_hindfoot_length = min(hindfoot_length),
            max_hindfoot_length = max(hindfoot_length)) %>%
  arrange(desc(mean_hindfoot_length))

#Challenge
#find the genus and species_id of the heaviest (max weight)
#individual each year

#first get rid of NAs (weight)
surveys %>% filter(!is.na(weight)) %>%
group_by(year) %>% #group by year
filter(weight == max(weight)) %>% #get the row with the max weight in each group
select(year, genus, species_id, weight) #get year, genus, species_id, weight

#WIDE TO LONG TO WIDE DATA
#Challenge
#mean weight of each species_id in each plot_id
surveys_meanweight_species_plot <- surveys %>% 
  filter(!is.na(weight)) %>%
  group_by(plot_id,species_id) %>%
  summarize(mean_weight = mean(weight))
#now spread so people can read it
surveys_meanweight_species_plot_wide <- surveys_meanweight_species_plot %>%
  spread(key = plot_id, value = mean_weight)
surveys_meanweight_species_plot_wide2 <- surveys_meanweight_species_plot %>%
  spread(key = species_id, value = mean_weight)

#gather wide data to long format
surveys_meanweight_species_plot_long <- surveys_meanweight_species_plot_wide %>%
  gather(key = plot_number, value = m_weight, -species_id)

surveys_meanweight_species_plot_long2 <- surveys_meanweight_species_plot_wide %>%
  gather(key = plot_number, value = m_weight, "1":"24")

#Challenge
#spread surveys where there is a row for each plot_id and columns for years
#value is the number of species_id per plot
#summarize first
#use the function n_distinct (use ? for help as needed)
surveys %>% group_by(plot_id, year) %>%
  summarise(n_species = n_distinct(species_id)) %>%
  spread(key = year, value = n_species)

surveys_numsp_year_plot <- surveys %>% count(plot_id, year, species_id) %>%
  count(plot_id, year) %>% spread(year,n)

#Challenge: gather the wide data above
#each row is a unique year and plot_id
surveys_numsp_year_plot_long <- surveys_numsp_year_plot %>%
  gather(year, number_spp_caught, -plot_id)

surveys_long <- surveys %>%
  gather(measurement, value, hindfoot_length:weight)


#Databases (or multiple spreadsheets)
#install.packages(c("dbplyr","RSQLite"))
download.file(url = "https://ndownloader.figshare.com/files/2292171",
              destfile = "data/portal_mammals.sqlite", mode = "wb")
library(dbplyr)
mammals <- DBI::dbConnect(RSQLite::SQLite(),
                          "data/portal_mammals.sqlite")
src_dbi(mammals)
surveys_db <- tbl(mammals,"surveys")
surveys_db %>% select(species_id,year, plot_id)
nrow(surveys_db)

#get samples weight <5, select a few cols
surveys_db %>% filter(weight < 5) %>% select(species_id, year) %>%
  collect()

colnames(surveys_db)
plots_db <- tbl(mammals,"plots")
species_db <- tbl(mammals,"species")
colnames(plots_db)
colnames(species_db)

#get the number of rodents in each plot
rodents_per_plot <- left_join(surveys_db, species_db) %>% filter(taxa == "Rodent") %>%
  group_by(plot_id,year) %>% tally %>% collect()

#