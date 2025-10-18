library(tidyverse)
library(abind)
library(lubridate)
# if statement to get directory in right place (will do later)

setwd("raw_data")

# must combine the following data sets by state and by year.
legal <- read.csv("AGA-LegalizationData.csv")
pop <- read.csv("all_states_population.csv")
unemp <- read.csv("all_states_unemployed.csv")
edu <- read.csv("all_states_education.csv")
rgdp <- read.csv("all_states_rgdp.csv")
stateid <- read.csv("states.csv")

#new data as of Aug 27, 2025
poverty <- read.csv("UKCPR_National_Welfare_Data.csv")
lsr_tax_rate <- read.csv("LSR-Gambling-Revenues-Data.csv")

# Step 1: Combining legal data with state id
# renaming the stateid dataframe
names(stateid) <- c("state", "abbrev")
df <- left_join(stateid, legal, by = 'state') %>%
  filter(!(abbrev == "DC"))

# Step 2: Combining df with lsr_tax_rate with the same method, while getting yearly tax rates

lsr_tax_rate$Date = as.Date(lsr_tax_rate$Date, format = '%m/%d/%Y') # adjusted the date
lsr_tax_rate$Effective.Tax.Rate = as.numeric(lsr_tax_rate$Effective.Tax.Rate)

lsr_tax_rate <- lsr_tax_rate %>%
  mutate(year = year(Date)) %>% 
  group_by(year, State) %>%
  summarize(mean_eff_tax = mean(Effective.Tax.Rate, na.rm = TRUE))

# this merge should not happen yet 
# df_test <- merge(df, lsr_tax_rate, by.x = c('state', 'legal.year'), by.y = c('State', 'year'))

# Step 2.5: Reducing poverty data set to just what we need

poverty <- poverty %>% 
  select(Federal.Minimum.Wage, Governor.is.Democrat..1.Yes., 
         Personal.income, Poverty.Rate, state, state_name, State.Minimum.Wage,
         Unemployment, year, state_fips, SSI.recipients, Employment) %>%
  mutate(date = year(as.Date(as.character(year), format = '%Y')),
         state = as.character(state),
         SSI.recipients = SSI.recipients/1000,
         Employment = Employment/1000,
         Unemployment = Unemployment/1000,
         Personal.income = Personal.income/1000) %>%
  rename(state_num = state, abbrev = state_name)

# Step 2: Converting all datetime objects to just year and renaming columns of values.
pop <- pop %>% 
  mutate(date = year(date)) %>%
  rename("population" = "value") 

unemp <- unemp %>%
  mutate(date = year(as.Date(date))) %>%
  rename("unemployment" = "value")

edu <- edu %>%
  mutate(date = year(as.Date(date))) %>%
  rename("bachelors" = "value")

rgdp <- rgdp %>%
  mutate(date = year(as.Date(date))) %>%
  rename("rgdp_2017" = "value") #chained to 2017

eff_tax_rate <- lsr_tax_rate %>%
  rename(state = State, date = year)

# Step 3: Assemble all FRED data and then assemble with the new data at the end
fred_list <- list(pop, unemp, edu, rgdp)
df_fred <- Reduce(function(x,y) merge(x,y, all = TRUE), fred_list)
# intermediary step: rename state to abbrev and filter to just 2000
df_fred <- df_fred %>%
  filter(date > 1999) %>%
  rename("abbrev" = "state") %>%
  na.omit() # omits the NAs, which only occur legitimately here.


df_clean <- left_join(df_fred, df, by = "abbrev")

# Step 4: Two more additional joins with the UKCPR and LSR data to complete it

df_clean <- left_join(df_clean, eff_tax_rate, by = c('state', 'date'))
df_clean <- left_join(df_clean, poverty, by = c('abbrev', 'date'))

write.csv(df_clean, "../clean_data/df_clean.csv", row.names = F)
# small test, comment to exclude
# df_clean %>% filter(abbrev == "IA") #Iowa, it works.
