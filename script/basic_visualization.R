library(data.table)
library(ggplot2)
library(tidyverse)
library(ggthemes)

# import search and mobility data
setwd("/Users/greatyifan/Desktop/@Columbia/2020fall/CU 2020 Data Hackathon/Mobility Dataset")
mobility_search_df <- fread('bq-results-20200919-221703-44o64hvfk23a.csv')
mobility_search_df$date <- as.Date(mobility_search_df$date)
## remove empty records with state == ''
mobility_search_df <- mobility_search_df[!state == '']
head(mobility_search_df)

# import policy date data
policy_df <- fread('state_policy.csv')
policy_df <- policy_df[-1, ]
colnames(policy_df) <- c('state', 'emergency', 'stayAtHome')
policy_df$stayAtHome <- as.Date(policy_df$stayAtHome, format = '%m/%d/%y')
policy_df$emergency <- as.Date(policy_df$emergency, format = '%m/%d/%y')

# join 
mobility_search_df <- left_join(mobility_search_df, policy_df, on = 'state')

# aggregate sympton by mean
table(mobility_search_df$state == mobility_search_df$state_name)
table(mobility_search_df$date == mobility_search_df$datesympton)

mean_sympton <- rowMeans(mobility_search_df[, 16:25], na.rm = T)
symptom_df <- cbind(mobility_search_df[, c(1:2, 26:27)], mean_sympton)
mobility_search_df$mean_sympton <- mean_sympton

# sympton search by state by stay at home order
ggplot(data = symptom_df) +
  geom_line(aes(x = date, y = mean_sympton)) +
  geom_vline(aes(xintercept = stayAtHome)) + 
  facet_wrap(facets = state ~ .)

# sympton search by state by emergency declaration
ggplot(data = symptom_df) +
  geom_line(aes(x = date, y = mean_sympton)) +
  geom_vline(aes(xintercept = emergency)) + 
  facet_wrap(facets = state ~ .)

# mobility - retail
ggplot(data = mobility_search_df) +
  geom_line(aes(x = date, y = retail_and_recreation_percent_change_from_baseline)) +
  geom_vline(aes(xintercept = stayAtHome)) + 
  facet_wrap(facets = state ~ .) +
  ggtitle(label = 'Retail Mobility and Stay at Home Order Date')

# mobility - grocery
ggplot(data = mobility_search_df) +
  geom_line(aes(x = date, y = grocery_and_pharmacy_percent_change_from_baseline)) +
  geom_vline(aes(xintercept = emergency)) + 
  facet_wrap(facets = state ~ .) +
  ggtitle(label = 'Grocery Mobility and Emergency Declaration Date -- (panic shopping)')

# mobility - parks
ggplot(data = mobility_search_df) +
  geom_line(aes(x = date, y = parks_percent_change_from_baseline)) +
  geom_vline(aes(xintercept = stayAtHome)) + 
  facet_wrap(facets = state ~ .) +
  ggtitle(label = 'Grocery Mobility and Stay at Home Declaration Date')

# mobility - transit
ggplot(data = mobility_search_df) +
  geom_line(aes(x = date, y = transit_stations_percent_change_from_baseline)) +
  geom_vline(aes(xintercept = stayAtHome)) + 
  facet_wrap(facets = state ~ .) +
  ggtitle(label = 'Transit Mobility and Stay at Home Declaration Date')

# mobility - workplace
ggplot(data = mobility_search_df) +
  geom_line(aes(x = date, y = workplaces_percent_change_from_baseline)) +
  geom_vline(aes(xintercept = stayAtHome)) + 
  facet_wrap(facets = state ~ .) +
  ggtitle(label = 'Workplace Mobility and Stay at Home Declaration Date')

# mobility - residential
ggplot(data = mobility_search_df) +
  geom_line(aes(x = date, y = residential_percent_change_from_baseline)) +
  geom_vline(aes(xintercept = stayAtHome)) + 
  facet_wrap(facets = state ~ .) +
  ggtitle(label = 'Residential Mobility and Stay at Home Declaration Date')

# confirmed/death data with emergency
ggplot(data = mobility_search_df, aes(x = date)) +
  geom_line(aes(y = confirmed_cases), color = 'blue') +
  geom_line(aes(y = deaths), color = 'red') +
  geom_vline(aes(xintercept = emergency)) + 
  facet_wrap(facets = state ~ .) +
  ggtitle(label = 'Confirmed Cases and Deaths with Emergency Declaration Date')


# confirmed/death data with stay at home
ggplot(data = mobility_search_df, aes(x = date)) +
  geom_line(aes(y = confirmed_cases), color = 'blue') +
  geom_line(aes(y = deaths), color = 'red') +
  geom_vline(aes(xintercept = stayAtHome)) + 
  facet_wrap(facets = state ~ .) +
  ggtitle(label = 'Confirmed Cases and Deaths with Stay at Home Declaration Date')



# NEW YORK 
mobility_search_df <- as.data.table(mobility_search_df)
mobility_search_df[state == 'New York']

ggplot(mobility_search_df[state == 'New York'], 
       aes(x = date)) +
  geom_line(aes(y = confirmed_cases)) +
  geom_line(aes(y = deaths)) +
  geom_line(aes(y = mean_sympton * 2000), color = 'red')+
  geom_line(aes(y = parks_percent_change_from_baseline * 1000), color = 'blue') +
  geom_line(aes(y = grocery_and_pharmacy_percent_change_from_baseline * 1000), color = 'green') + 
  geom_vline(aes(xintercept = emergency), color = 'darkred') +
  geom_vline(aes(xintercept = stayAtHome), color = 'purple')


# grocery shopping 
ggplot(data = mobility_search_df[date < '2020-04-20' & state != 'District of Columbia'], 
       aes(x = date)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(y = grocery_and_pharmacy_percent_change_from_baseline)
            ) +
  # geom_line(aes(y = confirmed_cases)) +
  geom_vline(aes(xintercept = emergency), color = 'orange', linetype="dashed") + 
  geom_vline(aes(xintercept = stayAtHome), color = '#4eb3d3', linetype="dashed") + 
  facet_wrap(facets = state ~ .) + 
  # ggtitle(label = 'Grocery Mobility and Emergency Declaration Date -- (panic shopping)') +
  theme_economist_white(gray_bg = F) +
  theme(axis.title = element_blank(), 
        axis.line.x = element_blank(), 
        axis.line.y = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        panel.grid.major.y = element_line(size = .2), 
        strip.background = element_rect(color = 'yellow'), 
        strip.text = element_text(size = 8))

# linerange 
ggplot(data = mobility_search_df[date < '2020-04-20' & state != 'District of Columbia'], 
       aes(x = date, y = grocery_and_pharmacy_percent_change_from_baseline, 
           ymin = 0, ymax = grocery_and_pharmacy_percent_change_from_baseline)) +
  geom_hline(yintercept = 0) +
  geom_linerange(aes(colour = ifelse(grocery_and_pharmacy_percent_change_from_baseline <0, "red", "green")),stat = "identity",
                 position = "identity",size=0.4) +
  # geom_line(aes(y = confirmed_cases)) +
  geom_vline(aes(xintercept = emergency), color = 'orange', 
             show.legend = T,
             linetype="solid", size = .5) + 
  geom_vline(aes(xintercept = stayAtHome), color = '#4eb3d3', 
             show.legend = T,
             linetype="solid", size = .5) + 
  facet_wrap(facets = state ~ .) + 
  # ggtitle(label = 'Grocery Mobility and Emergency Declaration Date -- (panic shopping)') +
  theme_economist_white(gray_bg = F) +
  theme(axis.title = element_blank(), 
        axis.line.x = element_blank(), 
        axis.line.y = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        panel.grid.major.y = element_line(size = .2), 
        strip.background = element_rect(color = 'yellow'), 
        strip.text = element_text(size = 8)) 

# parks 
ggplot(data = mobility_search_df, aes(x = date)) +
  geom_line(aes(y = parks_percent_change_from_baseline)) +
  # geom_line(aes(y = confirmed_cases)) +
  geom_vline(aes(xintercept = emergency), color = 'red') + 
  geom_vline(aes(xintercept = stayAtHome), color = 'darkred') + 
  facet_wrap(facets = state ~ .) + 
  ggtitle(label = 'Parks Mobility and Emergency Declaration Date')
