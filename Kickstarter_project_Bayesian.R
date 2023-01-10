############## Packages
library(tidyverse)
library(lubridate)
library(tidytext)
library(tm)
library(wordcloud2)
library(ROSE)
library(randomForest)
library(caret)
library(pROC)
library(gbm)
library(xgboost)
library(ggthemes)
library(paletteer)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3tuning)
library(assertive.base)
library(patchwork)
library(glmnet)
library(doParallel)


theme_fistro <- function() {
  theme_minimal() %+replace%
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      
      plot.title = element_text(
        family = "NimbusSan",
        size = 17,
        hjust = 0.9,
        vjust = -3),
      
      axis.text = element_text(
        family = "NimbusSan"),
      
      plot.background = element_rect(fill = "#FFFFF7")
    )
}


filepath <- "D:/Users/Gregor/Desktop/Kaggle/Kickstarter projects/ks-projects-201801.csv" 
kickstarter_data <- read_csv(filepath, col_names = T)
glimpse(kickstarter_data)

kickstarter_data <- kickstarter_data %>%
  mutate(category = factor(category),
         main_category = factor(main_category),
         currency = factor(currency),
         state = factor(state),
         country = factor(country))

summary(kickstarter_data)

kickstarter_data[year(kickstarter_data$launched) == 1970, ]

set.seed(2000)
kickstarter_data[kickstarter_data$country == 'N,0"', ][sample(1:3797, 20), ]


sapply(kickstarter_data, function(x) sum(is.na(x)))

(kickstarter_data[which(is.na(kickstarter_data$name)), ]) 

kickstarter_clean <- kickstarter_data %>%
  filter(!(year(launched) %in% c(1970, 2018))) %>% #We remove the projects from 1970 and 2018
  filter(!(country == 'N,0"')) %>% #We remove the projects with missing information about the countries
  filter(!is.na(name))

sapply(kickstarter_clean, function(x) sum(is.na(x)))

kickstarter_clean <- kickstarter_clean %>%
  filter(state %in% c("failed", "canceled", "successful", "suspended")) %>% #Omitting live and undefined projects
  mutate(state_bi = factor(ifelse(state == "successful", 1, 0))) %>% #Creating a factor -> 1 for a successful project, 0 otherwise
  mutate(launch_year = year(launched)) %>% #We aren't interested in datetime in our case, just the year of the project
  mutate(launched = date(launched)) #We are only interested in the date, as datetime is too detailed for our uses

kickstarter_clean <- kickstarter_clean %>%
  select(- c(pledged, state, `usd pledged`, goal, currency)) %>% #We remove redundant variables
  group_by(main_category) %>%
  mutate(bi_cat = mean(as.numeric(state_bi) - 1)) %>% #We calculate project success rates by main category
  ungroup()


count(kickstarter_clean, Funding = factor(state_bi, labels = c("Unsuccessful", "Successful")), name = "Total projects") # We can see that a bit more than a third of all projects were successful.

nrow(kickstarter_data[kickstarter_data$usd_goal_real > 1000000, ]) #Number of projects with a funding goal of 1M+ USD
nrow(kickstarter_data[kickstarter_data$usd_goal_real > 1000000, ]) / nrow(kickstarter_data)# Relative proportion of 1M+ projects
nrow(kickstarter_data[kickstarter_data$usd_goal_real > 1000000 & kickstarter_data$state == "successful", ]) / nrow(kickstarter_data[kickstarter_data$usd_goal_real > 1000000, ]) #Proportion of 1M + projects that were successful

kickstarter_data[kickstarter_data$usd_goal_real > 1000000 & kickstarter_data$state == "successful", ]$name

p7 <- kickstarter_clean %>%
        group_by(launch_year) %>%
        summarise(st_proj = n()) %>%
        ggplot(aes(x = launch_year, y = st_proj)) +
          geom_bar(stat = "identity", fill = "#14505C") +
          scale_x_continuous(breaks = seq(2009, 2017, 1)) +
          coord_cartesian(ylim = c(0, 90000)) +
          ylab("Number of Projects") +
          xlab("Launch Year") +
          ggtitle("Number of Projects Launched Between 2009 and 2017") +
          theme_fistro() +
          theme(axis.text.x = element_text(angle = 45, size = 12, vjust = 1, hjust = 0.8),
            axis.title.y = element_text(vjust = 3),
            panel.grid.major.y = element_line())
                                                                                                                            
                                                                                                                            
                                                                                                                            
#Proportion of successful projects by year
                                                                                                                            
p8 <- kickstarter_clean %>%
        group_by(launch_year) %>%
        summarise(succ_rate = (mean(as.numeric(state_bi) - 1)) * 100)  %>%
        ungroup() %>%
        ggplot(aes(x = launch_year, y = succ_rate)) +
          geom_bar(stat = "identity", fill = "#14505C") +
          scale_x_continuous(breaks = seq(2009, 2017, 1)) +
          scale_y_continuous(breaks = c(0, seq(10, 50, 10))) +
          coord_cartesian(ylim = c(0, 55)) +
          ylab("Project Success rate in %") +
          xlab("Launch Year") +
          ggtitle("Overall Project Success Rate between 2009 and 2017") +
          theme_fistro() +
          theme(axis.text.x = element_text(angle = 45, size = 12, vjust = 1, hjust = 0.8),
            axis.title.y = element_text(vjust = 3),
            panel.grid.major.y = element_line())
                                                                                                                            
#Total ammount of $ invested in all projects on kickstarter per year
                                                                                                                            
p9 <- kickstarter_clean %>%
        group_by(launch_year) %>%
        summarise(inv = sum(usd_pledged_real) / 1000000) %>% #Transform to millions of $ scale
        ungroup() %>%
        ggplot(aes(x = launch_year, y = inv)) +
          geom_bar(stat = "identity", fill = "#14505C") +
          scale_x_continuous(breaks = seq(2009, 2017, 1)) +
          coord_cartesian(ylim = c(0, 750)) +
          xlab("Lauch year") +
          ylab("Money invested (in million $)") +
          ggtitle("Total Amount of Money Invested in Projects \n on Kickstarter between 2009 and 2017") +
          theme_fistro() +
          theme(axis.text.x = element_text(angle = 45, size = 12, vjust = 1, hjust = 0.8),
            axis.title.y = element_text(vjust = 3),
            panel.grid.major.y = element_line())
                                                                                                                            
p7 / p8 / p9


p1 <-kickstarter_clean %>%
      group_by(main_category) %>%
      summarise(success_per = mean(as.numeric(state_bi) - 1)) %>% #We summarise mean success rate by category to be visualised
      arrange(desc(success_per)) %>%
      ungroup() %>%
      ggplot(aes(x = reorder(main_category, - success_per), y = success_per)) +
        geom_col(fill = "#14505C") +
        ggtitle("Project Success Rate by Main Categories") +
        xlab(element_blank()) +
        ylab("Project Success Rate") +
        scale_y_continuous(labels = scales::percent, breaks = c(0.2, 0.4, 0.6)) +
        theme_fistro() +
        theme(axis.text.x = element_text(angle = 60, size = 10, vjust = 1, hjust = 1),
          axis.title.y = element_text(vjust = 3),
          panel.grid.major.y = element_line(),
          plot.title = element_text(hjust = 1, vjust = 1.5))
  


#Which Categories have the Highest number of Backers

p2 <-kickstarter_clean %>%
      group_by(main_category) %>%
      summarise(n_backers = sum(backers)/1000000) %>% #We divide the number of Backers by a million for clarity
      ungroup() %>%
        ggplot(aes(x = reorder(main_category, - n_backers), y = n_backers)) + 
        geom_col(fill = "#14505C") +
        scale_y_continuous(expand = c(0, 0), limits = c(0, NA), breaks = c(0.2, 1, 3, 5, 7, 11)) +
        ggtitle("Number of Backers per category") +
        theme_fistro()+
        ylab("Total number of Backers in Millions") +
        xlab(element_blank()) +
        theme(axis.text.x = element_text(angle = 60, size = 10, vjust = 1, hjust = 1),
          axis.title.y = element_text(vjust = 3),
          panel.grid.major.y = element_line(),
          plot.title = element_text(hjust = 1, vjust = 1.5))



#Which categories attracted the most funding in total

p3 <- kickstarter_clean %>%
        group_by(main_category) %>%
        summarise(total_pledged = sum(usd_pledged_real)/1000000) %>% #We divide by a million to get better readability
        ungroup() %>%
          ggplot(aes(x = reorder(main_category, - total_pledged), y = total_pledged)) +
          geom_bar(stat = "identity", fill = "#14505C") +
          scale_y_continuous(breaks = c(0, 50, 100, 200, 400, 600)) +
          xlab(element_blank()) +
          ylab("Ammount pledged in Million $") +
          ggtitle("Total Amount of $ pledged by Category") +
          theme_fistro() +
          theme(axis.text.x = element_text(angle = 60, size = 10, vjust = 1, hjust = 0.8),
            axis.title.y = element_text(vjust = 3),
            panel.grid.major.y = element_line())


#Which Categories had the most of projects

p4 <- kickstarter_clean %>%
        group_by(main_category) %>%
        summarise(n_proj = n()/1000) %>% #We transform to number of projects in thousands
          ggplot(aes(x = reorder(main_category, -n_proj), y = n_proj)) +
          geom_bar(stat = "identity", fill = "#14505C") +
          scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
          xlab(element_blank()) +
          ylab("Number of Projects (in thousands)") +
          ggtitle("Total Number of Projects Launched by Category") +
          theme_fistro() +
            theme(axis.text.x = element_text(angle = 60, size = 10, vjust = 1, hjust = 0.8),
            axis.title.y = element_text(vjust = 3),
            panel.grid.major.y = element_line())

p1 + p4 + p3 + p2


p5 <- kickstarter_clean %>%
        ggplot(aes(x = main_category, y = usd_goal_real, fill = bi_cat * 100)) + #We transform the fill to %
          geom_boxplot(outlier.shape = NA) +
          coord_cartesian(ylim = c(0, 120000)) +
          scale_fill_paletteer_c("grDevices::BluGrn", direction = -1) +
          xlab(element_blank()) +
          ylab("Project funding goal ($)") +
          ggtitle("Funding Goal Distribution by Categories") +
          labs(fill = "Project Success \n      Rate in %") +
          theme_fistro() +
          theme(axis.text.x = element_text(angle = 60, size = 10, vjust = 1, hjust = 0.8),
            axis.title.y = element_text(vjust = 3),
            panel.grid.major.y = element_line())


#Comparison of the ammount of $ invested per project between categories

p6 <- kickstarter_clean %>%
        ggplot(aes(x = main_category, y = usd_pledged_real, fill = bi_cat * 100)) +
        geom_boxplot(outlier.shape = NA) +
        coord_cartesian(ylim = c(0, 35000)) +
        scale_fill_paletteer_c("grDevices::BluGrn", direction = -1) +
        xlab(element_blank()) +
        ylab("Money Invested in $") +
        ggtitle("Amount of $ Invested in a Project by Categories") +
        labs(fill = "Project Success \n      Rate in %") +
        theme_fistro() +
          theme(axis.text.x = element_text(angle = 60, size = 10, vjust = 1, hjust = 0.8),
            axis.title.y = element_text(vjust = 3),
            panel.grid.major.y = element_line())


kickstarter_clean %>%
  group_by(country) %>%
  summarise(n_projects = n()) %>%
  ggplot(aes(x = reorder(country, -n_projects), y = n_projects)) +
  geom_bar(stat = "identity", fill = "#14505C") +
        xlab(element_blank()) +
        ylab("Number of Projects") +
        ggtitle("Total Number of Projects Launched by Country") +
        theme_fistro() +
        theme(axis.text.x = element_text(angle = 60, size = 10, vjust = 2, hjust = 2.2),
          axis.title.y = element_text(vjust = 3),
          panel.grid.major.y = element_line())


kickstarter_clean %>%
  filter(country != "US") %>%
  group_by(country) %>%
  summarise(n_projects = n()) %>%
  ggplot(aes(x = reorder(country, - n_projects), y = n_projects)) +
  geom_bar(stat = "identity", fill = "#14505C") +
        xlab(element_blank()) +
        ylab("Number of Projects") +
        ggtitle("Total Number of Projects Launched \n by Country without the US") +
        theme_fistro() +
        theme(axis.text.x = element_text(angle = 60, size = 10, vjust = 2, hjust = 2.5),
          axis.title.y = element_text(vjust = 3),
          panel.grid.major.y = element_line())


kickstarter_eu <- kickstarter_clean %>%
  filter(country %in% c("DE", "FR", "NL", "IT", "ES", "SE", "DK", "IE", "CH", "NO", "BE", "AT", "LU"))

p10 <- kickstarter_eu %>%
  group_by(country) %>%
  summarise(n_projects = n()) %>%
  ggplot(aes(x = reorder(country, -n_projects), y = n_projects)) +
  geom_bar(stat = "identity", fill = "#14505C") +
        xlab(element_blank()) +
        ylab("Number of Projects") +
        ggtitle("Number of Projects Launched \n by Country in the EU") +
        theme_fistro() +
        theme(axis.text.x = element_text(angle = 60, size = 10, vjust = 2, hjust = 2.5),
          axis.title.y = element_text(vjust = 3),
          panel.grid.major.y = element_line())

p11 <- kickstarter_eu %>%
  group_by(country) %>%
  summarise(project_succes = mean(as.numeric(state_bi)-1)) %>%
  ggplot(aes(x = reorder(country, -project_succes), y = project_succes)) +
  geom_bar(stat = "identity", fill = "#14505C") +
        xlab(element_blank()) +
        ylab("Project Success in %") +
        ggtitle("Average Project Success \n by Country in the EU") +
        theme_fistro() +
        theme(axis.text.x = element_text(angle = 60, size = 10, vjust = 2, hjust = 2.5),
          axis.title.y = element_text(vjust = 3),
          panel.grid.major.y = element_line())

p10 / p11
                                                 
                                                                                                                          

kickstarter_eu %>%
  group_by(main_category) %>%
  summarise(project_succes = mean(as.numeric(state_bi)-1)) %>%
    ggplot(aes(x = reorder(main_category, -project_succes), y = project_succes)) +
    geom_bar(stat = "identity", fill = "#14505C") +
    xlab(element_blank()) +
    ylab("Project Success in %") +
    ggtitle("Average Project Success \n by Category in the EU Countries") +
    theme_fistro() +
      theme(axis.text.x = element_text(angle = 60, size = 10, vjust = 0.9, hjust = 0.9),
      axis.title.y = element_text(vjust = 3),
      panel.grid.major.y = element_line())
                                                                                                                            
kickstarter_eu %>%
  group_by(country, main_category) %>%
  summarise(success = mean(as.numeric(state_bi) - 1), n_proj = n()) %>%
  filter(n_proj > 10) %>%
  arrange(desc(success)) %>%
  head(20)
                                                                                                                            

## SENTIMENT ANALYSIS - AFINN AND BING
### _BING Lexicon_
                                                                                                                            

                                                                                                                            
kickstarter_bing <- kickstarter_clean %>%
  unnest_tokens(word, name) %>%
  inner_join(get_sentiments("bing"), by = "word") #Preparing data for analysis
                                                                                                                            
kickstarter_bing <- kickstarter_bing %>% 
  count(ID, sentiment) %>% #We split by ID and sentiment (on longer titles that have more relevant words)
  spread(sentiment, n) %>% #Spread the sentiment values to two columns (neg, pos)
  replace_na(list(negative = 0, positive = 0)) %>% #Replacing the resulting NAs
  mutate(ovrl_sen = positive - negative) %>% #Calculating the overall sentiment for every ID
  select(ID, ovrl_sen)
                                                                                                                            
                                                                                                                            
clean_bing <- left_join(kickstarter_clean, kickstarter_bing, by = "ID") #Joining the sentiment values to the original data
                                                                                                                                                                                                                                                      

head(clean_bing$ovrl_sen, 10) #It seems that a lot of entries didn't get a sentiment value assigned
sum(is.na(clean_bing$ovrl_sen)) 
                                                                                                                            
                                                                                                                            

clean_bing <- clean_bing %>% 
  replace_na(list(ovrl_sen = 0)) #We replace the NA values with 0
                                                                                                                            
sum(is.na(clean_bing$ovrl_sen)) #We see there are no more NA values

table(clean_bing$ovrl_sen)

clean_bing <- clean_bing %>%
  filter(ovrl_sen > -5 & ovrl_sen < 5) #We filter for projects with sentiment values between -5 and 5, because there is only 1 project with a value of -5 and 8 projects with a value of 5


clean_bing %>%
  group_by(ovrl_sen) %>%
  summarise(per = mean(as.numeric(state_bi) - 1), avg_fund = sum(usd_pledged_real / n()), nproj = n()) %>% #we calculate the average success rate of projects and the average amount raised per project for every category
  ungroup() %>%
    ggplot(aes(x = ovrl_sen, y = per, fill = avg_fund)) + #We plot the proportion of successful projects and average amount of $ funded per project by categories
    geom_col(position = "dodge2", alpha = 0.8) +
    geom_text(aes(label = paste0("* ", nproj), vjust = - 0.8)) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    scale_x_continuous(breaks = seq(-5, 5, by = 1)) +
    labs(title = "Proportion of Successful projects \n and Average Ammount of Money spent \n by BING Sentiment \n",
         x = "Overall BING Sentiment",
         y = "Proportion of Successful Projects in %",
         fill = "Average $ \n per project") +
    scale_fill_paletteer_c("grDevices::BluGrn", direction = -1) +
    geom_text(x = 2, y = 0.41, label = "* Number of projects", color = "#14505C") +
    theme_fistro() +
    theme(plot.title = element_text(vjust = 1),
      panel.grid.major.y = element_line(),
      axis.text.x = element_text(vjust = 5)) 
                                                                                                                           
summary(glm(state_bi ~ ovrl_sen, data = clean_bing, family = "binomial")) #We try a simple logistic model
exp(confint(glm(state_bi ~ ovrl_sen, data = clean_bing, family = "binomial"))) #The simple model predicts an 8-10% reduction in project success with rising values of overall sentiment


### _AFINN Lexicon_


#Sentiment analysis - does the sentiment value of the name of the project affect it's success - AFINN LEXICON
                                                                                                                            
kickstarter_afinn <- kickstarter_clean %>%
  unnest_tokens(word, name) %>%
  inner_join(get_sentiments("afinn"), by = "word") #We prepare the data for analysis with an AFINN lexicon
                                                                                                                            
                                                                                                                            
#We calculate the sentiment value for every project
kickstarter_afinn <- kickstarter_afinn %>% 
  group_by(ID) %>%
  mutate(ovrl_sen = sum(value)) %>% #We calculate the overall sentiment value of a projects with longer names by adding the sentiment scores of the included words together
  select(ID, ovrl_sen)



count(kickstarter_afinn, ID) %>% arrange(desc(n)) %>% head(20) #We take a quick look at the titles containing the most sentiment defined words.


filter(kickstarter_afinn, ID == 1219933968) #We take a look at one of the projects
filter(kickstarter_clean, ID == 1219933968)$name 

clean_afinn <- left_join(kickstarter_clean, unique(kickstarter_afinn), by = "ID") #We join the AFINN sentiment values with the original dataset, using only unique values to avoid duplication

table(clean_afinn$ovrl_sen) #A brief overview of the range of our calculated sentiment values
sum(is.na(clean_afinn$ovrl_sen))

clean_afinn <- clean_afinn %>% 
  replace_na(list(ovrl_sen = 0)) #We replace the NA values with 0
                                                                                                                            
sum(is.na(clean_afinn$ovrl_sen)) #We see there are no more NA values

                                                                                                                            
clean_afinn %>%
  group_by(ovrl_sen) %>%
  summarise(per = mean(as.numeric(state_bi) - 1), cash = sum(usd_pledged_real / n()), nproj = n()) %>%
  filter(between(ovrl_sen, -8, 10)) %>% #We filter out instances of sentiment values that have fewer than 20 samples
  ungroup() %>%
  ggplot(aes(x = ovrl_sen, y = per, fill = cash)) +
  geom_col(position = "dodge2", width = 1, alpha = 0.8) +
  geom_text(aes(label = paste0("*", nproj), vjust = - 1), size = 2.2) +
  scale_y_continuous(labels = scales::percent, breaks = c(0, 0.1, 0.2, 0.3, 0.35)) +
  scale_x_continuous(breaks = seq(-8, 10, by = 1)) +
  scale_fill_paletteer_c("grDevices::BluGrn", direction = -1) +
  geom_text(x = 8, y = 0.37, label = "* Number of Projects", size = 3, color = "#14505C") +
  theme_fistro() +
  labs(title = "Rate of Funding by AFINN Sentiment Value",
       x = "AFINN Sentiment Value",
       y = "Proportion of Successfuly Funded Projects (%)",
       fill = "Average Ammount \n of $ per Project",) +
  theme(plot.title = element_text(vjust = 1.4),
    panel.grid.major.y = element_line(),
    axis.text.x = element_text(vjust = 8))
#Any effects are much less pronounced
                                                                                                                            
summary(glm(state_bi ~ ovrl_sen, data = clean_afinn, family = "binomial")) #Very simple model
exp(confint(glm(state_bi ~ ovrl_sen, data = clean_afinn, family = "binomial"))) #This time the effect seems to be significantly lower (perhaps because of more bins in case of AFINN) and represents an decrease of about ~ 1 do 2%

clean_bing %>%
  filter(ovrl_sen <= -4) %>% #We filter for the entries with the lowest sentiment scores from BING Lexicon analysis
  select(name, ovrl_sen) %>% 
  arrange(ovrl_sen) %>%
  head(20)
  
clean_bing %>%
  filter(ovrl_sen >= 4) %>% #We filter for the entries with the highest sentiment scores from BING Lexicon analysis
  select(name, ovrl_sen) %>%
  arrange(desc(ovrl_sen)) %>%
  head(20)
  

clean_afinn %>%
  filter(ovrl_sen < -8) %>% #We filter for the entries with the lowest sentiment scores from AFINN Lexicon analysis
  select(name, ovrl_sen) %>%
  arrange(ovrl_sen) %>%
  head(20)
                                                                                                                            
clean_afinn %>%
  filter(ovrl_sen > 11) %>% #We filter for the entries with the highest sentiment scores from AFINN Lexicon analysis
  select(name, ovrl_sen) %>%
  arrange(desc(ovrl_sen)) %>%
  head(20)


### Preparing the data for modeling

clean_afinn <- clean_afinn %>%
  mutate(duration = as.numeric(deadline - launched)) #We calculate the duration of the project

set.seed(2002)

split_rows <- sample(nrow(clean_afinn), round(nrow(clean_afinn) * 0.6)) #We make an index for a 60/40 data split
train_data <- clean_afinn[split_rows, ] #We create a training dataset
test_data <- clean_afinn[-split_rows, ] #We create a testing dataset


table(train_data$state_bi) #Once again, we see that the data on the training set is quite unbalanced
sum(train_data$state_bi == 0) / nrow(train_data) #Class 0 represents 64% of cases


over_spl <- ovun.sample(state_bi ~ ., data = train_data, method = "over") #We oversample the dataset
table(over_spl$data$state_bi) #We see that the classes are now balanced 
over_data <- over_spl$data #We assign our data to a new dataset to use later

set.seed(2003)

clean_over <- over_data %>%
  select(-c("ID", "name", "deadline", "backers", "usd_pledged_real", "bi_cat", "launched")) 
                                                                                                                            
clean_over <- clean_over[sample(nrow(clean_over)), ] #We shuffle the data, because they were split by state_bi variable from ovun.sample

clean_over2 <- clean_over %>%
  select(-category)
                                                                                                                            
clean_over <- clean_over %>%
  select(-main_category)
                                                                                                                            
clean_train <- train_data %>%
  select(-c("ID", "name", "deadline", "backers", "usd_pledged_real", "bi_cat", "launched", "main_category"))
                                                                                                                            
clean_train2 <- train_data %>%
  select(-c("ID", "name", "deadline", "backers", "usd_pledged_real", "bi_cat", "launched", "category"))
                                                                                                                            
clean_test <- test_data %>%
  select(-c("ID", "name", "deadline", "backers", "usd_pledged_real", "bi_cat", "launched", "main_category"))
                                                                                                                            
clean_test2 <- test_data %>%
  select(-c("ID", "name", "deadline", "backers", "usd_pledged_real", "bi_cat", "launched", "category"))
                                                                                                                            


## MODELLING

### Using the unbalanced dataset
                                                                                                                            
logi_mod_ub <- glm(state_bi ~ ., data = clean_train, family = binomial) 
summary(logi_mod_ub) #Compared to the balanced data, we can see an improvement in AIC

# rstanarm Bayesian
library(UPG)
library(fastDummies)

set.seed(2)
clean_test_bay <- clean_test[sample(nrow(clean_test), size = 50000), ]

datk <- clean_test_bay

#datk <- datk %>%
#  mutate(goal_bin = case_when(usd_goal_real < 5000 ~ 1,
#                              usd_goal_real < 20000 ~ 2,
#                              usd_goal_real < 100000 ~ 3,
#                              usd_goal_real < 1000000 ~ 4,
#                              usd_goal_real >= 1000000 ~ 5),
#         sentiment_bins = case_when(ovrl_sen < -5 ~ 1,
#                                    ovrl_sen < 0 ~ 2,
#                                    ovrl_sen == 0 ~ 3,
#                                    ovrl_sen < 5 ~ 4,
#                                    ovrl_sen >= 5 ~ 5),
#         duration_bin = case_when(duration < 30 ~ 1,
#                                  duration < 60 ~ 2,
#                                  duration >= 60 ~ 3))

datk <- datk %>%
  mutate(goal_bin = case_when(usd_goal_real < 5000 ~ '< 5000',
                              usd_goal_real < 20000 ~ 'between 5000 and 20000',
                              usd_goal_real < 100000 ~ 'between 20000 and 100000',
                              usd_goal_real < 1000000 ~ 'between 100000 and 1000000',
                              usd_goal_real >= 1000000 ~ 'over 1000000'),
         sentiment_bins = case_when(ovrl_sen < -5 ~ 'very negative',
                                    ovrl_sen < 0 ~ 'negative',
                                   ovrl_sen == 0 ~ 'neutral',
                                    ovrl_sen < 5 ~ 'positive',
                                    ovrl_sen >= 5 ~ 'very_positive'),
         duration_bin = case_when(duration < 30 ~ 'short',
                                  duration < 60 ~ 'standard',
                                  duration >= 60 ~'long'))

datk <- datk %>%
  mutate(launch_year = as.factor(launch_year),
         goal_bin = as.factor(goal_bin),
         sentiment_bins = as.factor(sentiment_bins),
         duration_bin = as.factor(duration_bin))

datk <- datk[, c(-3,-6,-7)]

datk <- datk %>%
  group_by(category, country, launch_year, goal_bin, sentiment_bins, duration_bin) %>%
  summarise(category = category, country = country, launch_year = launch_year,
            goal_bin = goal_bin, sentiment_bins = sentiment_bins, duration_bin = duration_bin,
            total = n(), successful = sum(as.numeric(state_bi) - 1)) %>%
  ungroup()

datk <- unique(datk)

intercept <- rep(1, nrow(datk))

datk <- cbind(intercept, datk)

y <- datk[, 9]
Ni <- datk[, 8]
X <- datk[, c(-8,-9)]

datko <- dummy_cols(X, select_columns = c("category", "country", "launch_year", "goal_bin", "sentiment_bins", "duration_bin"))
datko <- datko[, -c(2,3,4,5,6,7)]

results.binomial <- UPG(y = y, Ni = Ni, X = datko, model = "binomial")

summary(results.binomial)

newobs <- data.frame(category = "3D Printing", country = "US", launch_year = as.factor(2017), goal_bin = "< 5000", sentiment_bins = "negative", duration_bin = "long")

datcc_form <- dummyVars(~ category + country + launch_year + goal_bin + sentiment_bins + duration_bin,
                        data = datk,
                        levelsOnly = F)

datcc <- as_tibble(predict(datcc_form, newdata = datk))
datcc <- datcc[, -c(206,207)]

datcc.res <- UPG(y = y, Ni = Ni, X = datcc, model = "binomial")


newobs <- as_tibble(predict(datcc_form, newdata = newobs))
newobs <- cbind(intercept = intercept[1], newobs[1, ])

predict(results.binomial, newdata = newobs, q =c(0.05, 0.95))
