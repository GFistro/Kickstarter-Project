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

                                                                                                                            
                                                                                                                            
### Wordcloud
set.seed(2001)
                                                                                                                            
text <- kickstarter_data$name
kickstarter_corpus <- Corpus(VectorSource(text[sample(length(text), 1000)])) #We downsample the Corpus so that we get a reasonable output
                                                                                                                            
clean_corpus <- kickstarter_corpus %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>% #We do the standard cleaning procedure, removing numbers, punctuation and whitespace
  tm_map(stripWhitespace)
                                                                                                                            
clean_corpus <- tm_map(clean_corpus, content_transformer(tolower))
clean_corpus <- tm_map(clean_corpus, removeWords, c(stopwords("english"), "canceled", "film", "book", "project", "game", "new", "music", "art", "album", "novel", "video", "story", "documentary", "magazine", "movie", "dance")) #We remove the stopwords and words describing Kickstarter categories
                                                                                                                            
dtm <- TermDocumentMatrix(clean_corpus) #Transforming the data to a document matrix form
dtm_matrix <- as.matrix(dtm)
words <- sort(rowSums(dtm_matrix), decreasing = T)
wordcloud_data <- data.frame(word = names(words), freq = words)
                                                                                                                            
gbl_palette <- paletteer_c("grDevices::BluGrn", n = 50) #We use the custom palette we used before
gbl_palette2 <- c(gbl_palette, rep(gbl_palette[50], 350)) #We edit the palette so that we still get the color differentiation but recycle the lightest colour for the words with the lowest occurences
wordcloud2(data = wordcloud_data, color = gbl_palette2)
#We see mostly words such as first, world, life, help, mostly positive words

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

naive_preds <- rep(0, nrow(test_data)) #We construct the binary 'predictions' in our naive model
naive_preds_probs <- rep(sum(test_data$state_bi == 1) / nrow(test_data), nrow(test_data))
#We construct the prediction probabilities for our naive model. We get the probability of a observation being assigned a class 1 from the class imbalance in our data. We then recycle that value to all predictions.
                                                                                                                            
LogLossBinary = function(actual, predicted, eps = 1e-15) {
  predicted = pmin(pmax(predicted, eps), 1-eps)
  - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
} #We define a log-loss function
                                                                                                                            
mean((naive_preds_probs - (as.numeric(test_data$state_bi) - 1))^2) #The naive model gives a Brier Score of 0.230
auc(test_data$state_bi, naive_preds_probs) #It gives the AUC of 0.5
LogLossBinary((as.numeric(test_data$state_bi) - 1), naive_preds_probs)#The log-loss of the naive model is 0.653


#Using the balanced dataset
logi_mod <- glm(state_bi ~ ., data = clean_over, family = binomial) #We fit the logistic model on our balanced dataset
summary(logi_mod) #We see a residual distribution skewed to the right

logipreds_prob <- predict(logi_mod, newdata = clean_test, type = "response") #We predict the class probabilities on the test set
logipreds <- ifelse(logipreds_prob > 0.5, 1, 0) #To transform our probabilities to class labels, we use the default 0.5 threshold
                                                                                                                            
auc(clean_test$state_bi, logipreds_prob) #We get an AUC value of 0.717
mean((logipreds_prob - (as.numeric(clean_test$state_bi) - 1))^2) #We reach a Brier score of 0.216
LogLossBinary((as.numeric(clean_test$state_bi) - 1), logipreds_prob) #We reach a log-loss score of 0.619
                                                                                                                            
table(predictions = logipreds, truth = clean_test$state_bi)

### Using the unbalanced dataset
                                                                                                                            
logi_mod_ub <- glm(state_bi ~ ., data = clean_train, family = binomial) 
summary(logi_mod_ub) #Compared to the balanced data, we can see an improvement in AIC


logipreds_prob_ub <- predict(logi_mod_ub, newdata = clean_test, type = "response")
logipreds_ub <- ifelse(logipreds_prob_ub > 0.5, 1, 0)
                                                                                                                            
auc(clean_test$state_bi, logipreds_prob_ub) #We get an AUC value of 0.718
mean((logipreds_prob_ub - (as.numeric(clean_test$state_bi) - 1))^2) #We reach a Brier score of 0.200
LogLossBinary((as.numeric(clean_test$state_bi) - 1), logipreds_prob_ub) #Our calculated Log-loss is 0.584
                                                                                                                            
                                                                                                                            
table(predictions = logipreds_ub, truth = clean_test$state_bi)

train_mm_ub <- clean_train %>%
  select(-c(state_bi, category, country)) #We remove any non numeric variables and the target variable
                                                                                                                            
train_mm_lab_ub <- clean_train$state_bi #Saving the labels
categories_ub <- model.matrix(~category + country -1, clean_train) #One-hot encoding
                                                                                                                            
train_mm_num_ub <- cbind(train_mm_ub, categories_ub) #We combine the two datasets
train_mm_mat_ub <- data.matrix(train_mm_num_ub) #And transform it into a matrix
                                                                                                                            
test_mm_data <- clean_test %>%
  select(-c(state_bi, category, country)) #We do the same on the test data
                                                                                                                            
test_mm_data_lab <- clean_test$state_bi #Labels for the test set
categories_test <- model.matrix(~category + country -1, clean_test)
                                                                                                                            
test_mm_num <- cbind(test_mm_data, categories_test) 
test_mm_mat <- data.matrix(test_mm_num)


set.seed(2005)
reg_cv_ub <- cv.glmnet(train_mm_mat_ub, as.numeric(train_mm_lab_ub) - 1, alpha = 0) #Ridge

logipreds_prob_reg_ub <- predict(reg_cv_ub, newx = test_mm_mat, type = "response", s = reg_cv_ub$lambda.min)
logipreds_reg_ub <- ifelse(logipreds_prob_reg_ub > 0.5, 1, 0)
                                                                                                                            
auc(clean_test$state_bi, logipreds_prob_reg_ub) #We get an AUC value of 0.695
mean((logipreds_prob_reg_ub - (as.numeric(clean_test$state_bi) - 1))^2) #We reach a Brier score of 0.206
LogLossBinary((as.numeric(clean_test$state_bi) - 1), logipreds_prob_reg_ub) #Our calculated Log-loss is 0.605
                                                                                                                            
                                                                                                                            
table(predictions = logipreds_reg_ub, truth = clean_test$state_bi)



# Random Forest
set.seed(2005)
kick_rf_model <- randomForest(state_bi ~ ., data = clean_over2)
                                                                                                                            
print(kick_rf_model)
importance(kick_rf_model) 

kick_rf_preds <- predict(kick_rf_model, newdata = clean_test2, type = "response") #Make class predictions
kick_rf_preds_prob <- predict(kick_rf_model, newdata = clean_test2, type = "prob") #Make probability predictions
                                                                                                                            
auc(clean_test2$state_bi, kick_rf_preds_prob[, 2]) #We use the 2nd column of our probabilities prediction, because that is the column that gives the probability of the observation to be class 1
mean((kick_rf_preds_prob[, 2] - (as.numeric(clean_test2$state_bi) - 1))^2)
LogLossBinary((as.numeric(clean_test2$state_bi) - 1), kick_rf_preds_prob[, 2])


set.seed(2005)
kick_rf_model_ub <- randomForest(state_bi ~ ., data = clean_train2)

print(kick_rf_model_ub)
importance(kick_rf_model_ub) 

kick_rf_preds_ub <- predict(kick_rf_model_ub, newdata = clean_test2, type = "response") #Make class predictions
kick_rf_preds_prob_ub <- predict(kick_rf_model_ub, newdata = clean_test2, type = "prob") #Make probability predictions

auc(clean_test2$state_bi, kick_rf_preds_prob_ub[, 2]) #We use the 2nd column of our probabilities prediction, because that is the column that gives the probability of the observation to be class 1
mean((kick_rf_preds_prob_ub[, 2] - (as.numeric(clean_test2$state_bi) - 1))^2)
LogLossBinary((as.numeric(clean_test2$state_bi) - 1), kick_rf_preds_prob_ub[, 2])

### GBM - Gradient Boosting

##### GBM model
over_data_gbm <- clean_over
over_data_gbm$state_bi <- as.numeric(over_data_gbm$state_bi) - 1 #We transform the factor variable to numeric for the GBM algorithm

set.seed(2006)
kick_gbm <- gbm(state_bi ~ ., data = over_data_gbm, distribution = "bernoulli", n.trees = 1500, shrinkage = 0.01, interaction.depth = 2, cv.folds = 5) #We train the model
summary(kick_gbm) #We see that the three influential predictors are category, goal and the project duration

opt_tree <- gbm.perf(kick_gbm, method = "cv")

gbm_preds_prob <- predict(kick_gbm, newdata = clean_test, type = "response", n.trees = 1500) #We predict on the test set
gbm_preds <- ifelse(gbm_preds_prob > 0.5, 1, 0)

auc(clean_test$state_bi, gbm_preds_prob) #The AUC from GBM is 0.742
mean((gbm_preds_prob - (as.numeric(clean_test$state_bi) - 1))^2) #Brier Score of 0.207
LogLossBinary((as.numeric(clean_test$state_bi) - 1), gbm_preds_prob) #Log-loss of 0.598

#### Unbalanced dataset data preparation and fitting

train_data_gbm_ub <- clean_train
train_data_gbm_ub$state_bi <- as.numeric(train_data_gbm_ub$state_bi) - 1

set.seed(2007)
kick_gbm_ub <- gbm(state_bi ~ ., data = train_data_gbm_ub, distribution = "bernoulli", n.trees = 1500, shrinkage = 0.01, interaction.depth = 2, cv.folds = 5)
summary(kick_gbm_ub)

opt_tree2 <- gbm.perf(kick_gbm_ub, method = "cv")

gbm_preds_ub_prob <- predict(kick_gbm_ub, newdata = clean_test, type = "response", n.trees = 1500)
gbm_preds_ub <- ifelse(gbm_preds_ub_prob > 0.5, 1, 0)

auc(clean_test$state_bi, gbm_preds_ub_prob) #The AUC is 0.741
mean((gbm_preds_ub_prob - (as.numeric(clean_test$state_bi) - 1))^2) #Brier score - 0.193
LogLossBinary((as.numeric(clean_test$state_bi) - 1), gbm_preds_ub_prob) #Log-loss of 0.565

### _Hyperparameter tuning of GBM_

set.seed(2007)
sam_ub_gbm <- clean_train[sample(nrow(clean_train), 4000), ] #we construct a smaller sample

levels(sam_ub_gbm$state_bi) <- c("Failed", "Successful")

cacontrol <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3,
                          classProbs = T,
                          summaryFunction = twoClassSummary)

gbmGrid <- expand.grid(interaction.depth = c(2,4,6),
                       n.trees = 1500,
                       shrinkage = c(0.001, 0.005, 0.01, 0.015, 0.5, 0.1),
                       n.minobsinnode = 10)


clus <- makePSOCKcluster(8)
registerDoParallel(clus)

gbmtun <- train(state_bi ~ ., data = sam_ub_gbm,
                method = "gbm",
                trControl = cacontrol,
                tuneGrid = gbmGrid,
                metric = "ROC")

stopCluster(clus)

gbmtun
ggplot(gbmtun)



set.seed(2007)
kick_gbm_ub_tun <- gbm(state_bi ~ ., data = train_data_gbm_ub, distribution = "bernoulli", n.trees = 2000, shrinkage = 0.01, interaction.depth = 4, cv.folds = 5)
summary(kick_gbm_ub_tun)

gbm_preds_ub_tun_prob <- predict(kick_gbm_ub_tun, newdata = clean_test, type = "response", n.trees = 2000)
gbm_preds_ub_tun <- ifelse(gbm_preds_ub_tun_prob > 0.5, 1, 0)

auc(clean_test$state_bi, gbm_preds_ub_tun_prob) #The AUC is 0.748
mean((gbm_preds_ub_tun_prob - (as.numeric(clean_test$state_bi) - 1))^2) #Brier score - 0.190
LogLossBinary((as.numeric(clean_test$state_bi) - 1), gbm_preds_ub_tun_prob) #Log-loss of 0.560

### XGBoost
#Used the process described in https://www.kaggle.com/rtatman/machine-learning-with-xgboost-in-r

xgtrain_data <- clean_over %>%
  select(-c(state_bi, category, country)) #We remove any non numeric variables and target variable

xgtrain_data_lab <- clean_over$state_bi #We save the labels separately so that they don't interfere

categories <- model.matrix(~category + country -1, clean_over) #We one-hot encode categorical variables to transform them to numerical

xgtrain_data_num <- cbind(xgtrain_data, categories) #We combine the two datasets
xgtrain_data_mat <- data.matrix(xgtrain_data_num) #And transform it into a matrix
                                                                                                                            
xgtrain <- xgb.DMatrix(data = xgtrain_data_mat, label = as.numeric(as.character(xgtrain_data_lab))) #We transform the data into an xgb.DMatrix format for better processing
xgtest <- xgb.DMatrix(data = test_mm_mat, label = as.numeric(as.character(test_mm_data_lab))) #Same for the test data

                                                                                                                            
set.seed(2008)
xg_bal_model   <- xgboost(xgtrain,         #We train the XgBoost algorithm
                          max.depth = 6,
                          nround = 3000,
                          early_stopping_rounds = 5, #We add early stopping rounds if the algorithm stabilizes
                          objective = "binary:logistic",  
                          gamma = 0,
                          verbose = 0)


im_xgbal <- xgb.importance(names(xgtrain_data_mat), model = xg_bal_model)
im_xgbal[1:10, ] #We see usd_goal_real as the most important feature, followed by duration, launch_year and ovrl_sen
xgb.plot.importance(im_xgbal[1:20, ])

xg_bal_preds_prob <- predict(xg_bal_model, xgtest) #We make predictions on the test set

auc(clean_test$state_bi, xg_bal_preds_prob) #The AUC of the model reaches 0.723
mean((xg_bal_preds_prob - (as.numeric(clean_test$state_bi) - 1))^2) #We reach a Brier score of 0.211
LogLossBinary((as.numeric(clean_test$state_bi) - 1), xg_bal_preds_prob) #The Log-loss is 0.620

## Using the unbalanced dataset
xgtrain_ub <- xgb.DMatrix(data = train_mm_mat_ub, label = as.numeric(as.character(train_mm_lab_ub))) #Transforming the data into an xgb.DMatrix format

negative_cases_ub <- sum(train_mm_lab_ub == 0) #We isolate the negative and positive cases for later use
positive_cases_ub <- sum(train_mm_lab_ub == 1)

set.seed(2011)
xg_ub_model <- xgboost(xgtrain_ub,              #We train a xgboost model on the unbalanced dataset
                       max.depth = 6,
                       nround = 3000,
                       early_stopping_rounds = 5,
                       objective = "binary:logistic",
                       scale_pos_weight = negative_cases_ub/positive_cases_ub,
                       verbose = 0)

print(xg_ub_model) #After 3000 iterations we get a logloss value of 0.450
                                                                                                                            
im_xgub <- xgb.importance(names(train_mm_mat_ub), model = xg_ub_model)
im_xgub[1:10, ] #We see very similar trends as on the balanced dataset
xgb.plot.importance(im_xgub[1:20, ])


xg_ub_preds <- predict(xg_ub_model, xgtest) #We make predictions on the test set

auc(clean_test$state_bi, xg_ub_preds) #The model gives us an AUC value of 0.730
mean((xg_ub_preds - (as.numeric(clean_test$state_bi) - 1))^2) #Brier score of 0.211
LogLossBinary((as.numeric(clean_test$state_bi) - 1), xg_ub_preds) #The Log-loss is 0.615

## _Hyperparameter tuning for XGBoost_

getOption("lgr.log_levels")
lgr::get_logger("mlr3")$set_threshold("warn")

lrn1 <- lrn("classif.xgboost", predict_type = "prob") #We define the learner to be xgboost
lrn1 <- po("encode") %>>% lrn1 #As xgboost has trouble with factors, we convert them

traintask_ub <- as_task_classif(x = clean_train, target = "state_bi") #We create the task object

rsmpl <- rsmp("cv", folds = 3)

ss <- ps(classif.xgboost.max_depth = p_int(lower = 2, upper = 10),
classif.xgboost.gamma = p_int(lower = 0, upper = 10),
classif.xgboost.min_child_weight = p_int(lower = 1, upper = 10),
classif.xgboost.eta = p_dbl(lower = 0, upper = 1)
)

instance_prob_ub <- TuningInstanceSingleCrit$new(task = traintask_ub, learner = lrn1, resampling = rsmpl,
measure = msr("classif.auc"), search_space = ss,
terminator =  trm("evals", n_evals = 150))

tuner <- tnr("grid_search")
                                                                                                                            
set.seed(2010)
tuner$optimize(instance_prob_ub)

instance_prob_ub$result

set.seed(2012)
xg_ub_model_tun <- xgboost(xgtrain_ub,              #We train the tuned model on the unbalanced dataset
                           max.depth = 10,
                           nround = 3000,
                           early_stopping_rounds = 5,
                           objective = "binary:logistic",
                           scale_pos_weight = negative_cases_ub/positive_cases_ub,
                           gamma = 10,
                           min_child_weight = 8,
                           eta = 0.667,
                           verbose = 0)

print(xg_ub_model_tun) 

im_xgub_tun <- xgb.importance(names(train_mm_mat_ub), model = xg_ub_model_tun)
im_xgub_tun[1:10, ] #Again very similar
xgb.plot.importance(im_xgub_tun[1:20, ])


xg_ub_preds_tun <- predict(xg_ub_model_tun, xgtest) #We make predictions on the test set

auc(clean_test$state_bi, xg_ub_preds_tun) #The model gives us an AUC value of 0.748
mean((xg_ub_preds_tun - (as.numeric(clean_test$state_bi) - 1))^2) #Brier score of 0.204
LogLossBinary((as.numeric(clean_test$state_bi) - 1), xg_ub_preds_tun) #The log-loss is 0.591



### MODEL PERFORMANCE COMPARISON

final_table_balanced <- data.frame(c(auc(test_data$state_bi, naive_preds), #Naive AUC
                                     mean((naive_preds - (as.numeric(test_data$state_bi) - 1))^2), #Naive Brier 
                                     LogLossBinary((as.numeric(test_data$state_bi) - 1), naive_preds_probs)), #Naive Log-loss
                                   
                                   c(auc(test_data$state_bi, logipreds_prob), #GLM AUC
                                     mean((logipreds_prob - (as.numeric(clean_test$state_bi) - 1))^2), #GLM Brier
                                     LogLossBinary((as.numeric(clean_test$state_bi) - 1), logipreds_prob)), #GLM Log-loss
                                   c(auc(clean_test2$state_bi, kick_rf_preds_prob[, 2]), #RF AUC 
                                     mean((kick_rf_preds_prob[, 2] - (as.numeric(clean_test2$state_bi) - 1))^2),#RF Brier
                                     LogLossBinary((as.numeric(clean_test2$state_bi) - 1), kick_rf_preds_prob[, 2])), #RF Log-loss 
                                  
                                    c(auc(clean_test$state_bi, gbm_preds_prob), #GBM AUC
                                     mean((gbm_preds_prob - (as.numeric(clean_test$state_bi) - 1))^2), #GBM Brier
                                     LogLossBinary((as.numeric(clean_test$state_bi) - 1), gbm_preds_prob)), #GBM Log-loss
                                   
                                   c(auc(clean_test$state_bi, xg_bal_preds_prob), #XGBoost AUC
                                     mean((xg_bal_preds_prob - (as.numeric(clean_test$state_bi) - 1))^2), #Xgboost Brier 
                                     LogLossBinary((as.numeric(clean_test$state_bi) - 1), xg_bal_preds_prob))) #XGBoost Log-loss


colnames(final_table_balanced) <- c("Naive model", "Logistic Regression", "Random Forest", "GBM", "XGBoost")
rownames(final_table_balanced) <- c("AUC Values", "Brier scores", "Log-loss values")

final_table_balanced


final_table_unbalanced <- data.frame(c(auc(test_data$state_bi, naive_preds), #Naive AUC U
                                 mean((naive_preds - (as.numeric(test_data$state_bi) - 1))^2),#Naive Brier  U
                                 LogLossBinary((as.numeric(test_data$state_bi) - 1), naive_preds_probs)), #Naive Log-loss U
                               c(auc(test_data$state_bi, logipreds_prob_ub), #GLM AUC U
                                mean((logipreds_prob_ub - (as.numeric(clean_test$state_bi) - 1))^2), #GLM Brier U
                                LogLossBinary((as.numeric(clean_test$state_bi) - 1), logipreds_prob_ub)), #GLM Log-loss U
                               
                               c(auc(test_data$state_bi, logipreds_prob_reg_ub), #Ridge GLM AUC
                                mean((logipreds_prob_reg_ub - (as.numeric(clean_test$state_bi) - 1))^2), #Ridge GLM Brier
                                LogLossBinary((as.numeric(clean_test$state_bi) - 1), logipreds_prob_reg_ub)), #Ridge GLM Log-loss
                               c(auc(test_data$state_bi, as.numeric(kick_rf_preds_prob_ub[, 2]) - 1), #RF AUC U
                                mean((kick_rf_preds_prob_ub[, 2] - (as.numeric(test_data$state_bi) - 1))^2), #RF Brier U
                                LogLossBinary((as.numeric(clean_test$state_bi) - 1), kick_rf_preds_prob_ub[, 2])), #RF Log-loss U
                               c(auc(test_data$state_bi, gbm_preds_ub_prob), #GBM AUC U
                                mean((gbm_preds_ub_prob - (as.numeric(test_data$state_bi) - 1))^2), #GBM Brier U
                                LogLossBinary((as.numeric(clean_test$state_bi) - 1), gbm_preds_ub_prob)), #GBM Log-loss U
                              
                                c(auc(clean_test$state_bi, gbm_preds_ub_tun_prob), #GBM AUC U TUNED
                                mean((gbm_preds_ub_tun_prob - (as.numeric(clean_test$state_bi) - 1))^2), #GBM U TUNED Brier
                                LogLossBinary((as.numeric(clean_test$state_bi) - 1), gbm_preds_ub_tun_prob)), #GBM U TUNED Log-loss
                               
                               c(auc(clean_test$state_bi, xg_ub_preds), #XGBoost AUC U
                                mean((xg_ub_preds - (as.numeric(clean_test$state_bi) - 1))^2), #Xgboost Brier  U
                                LogLossBinary((as.numeric(clean_test$state_bi) - 1), xg_ub_preds)), #XGBoost Log-loss U
                               
                               c(auc(clean_test$state_bi, xg_ub_preds_tun), #Tuned xgboost AUC U
                                mean((xg_ub_preds_tun - (as.numeric(clean_test$state_bi) - 1))^2),#Tuned xgboost Brier U
                                LogLossBinary((as.numeric(clean_test$state_bi) - 1), xg_ub_preds_tun)) #Tuned XGBoost  Log-loss U
)

colnames(final_table_unbalanced) <- c("Naive model UB", "Logistic Regression UB", "Logistic Regression UB with Ridge Regularization", "Random Forest UB", "GBM UB", "GBM Tuned UB", "XGBoost UB", "XGBoost Tuned UB")
rownames(final_table_unbalanced) <- c("AUC Values", "Brier scores", "Log-loss values")

final_table_unbalanced

### PREDICTION FUNCTIONS


##Prediction function XgBoost


kickstarter_predict_xg <- function(category, country, goal_usd, launch_year, project_name, duration) {
  
  if(class(category) != "character") stop("category should be a character vector")
  if(!category %in% levels(clean_test$category)) stop("The entered category is not valid. Valid entries are: \n", print_and_capture(levels(clean_test$category)))
  if(class(country) != "character") stop("country should be a character vector")
  if(!country %in% levels(clean_test$country)) stop("The entered country is not valid. Valid entries are: \n", print_and_capture(levels(clean_test$country)))
  if(class(goal_usd) != "numeric") stop("goal_usd should be a numeric value")
  if(class(launch_year) != "numeric") stop("launch_year should be a numeric value")
  if(launch_year < 2009) stop("Kickstarter would definitely be revolutionary if it was lunched in ", launch_year,", however it was lunched in 2009 so please choose a year from 2009 onwards :)")
  if(class(project_name) != "character") stop("project_name should be a character vector")
  if(class(duration) != "numeric") stop("duration should be a numeric value")
  
  start_list <- clean_test[0, ] %>%
    select(-state_bi)
  
  sentscore <- data.frame(name = c("good and bad", project_name)) %>% 
    unnest_tokens(word, name) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    summarise(total = sum(value))
  
  start_list[1, ] <- list(category = category,
                          country = country,
                          usd_goal_real = goal_usd,
                          launch_year = launch_year,
                          ovrl_sen = sentscore[1,1],
                          duration = duration)
  
  cat_mat <-  model.matrix(~category + country -1, start_list)
  
  dat_mat <- start_list %>%
    select(-c(category, country))
  
  alldata <- cbind(dat_mat, cat_mat)
  alldataxgb <- xgb.DMatrix(as.matrix(alldata))
  
  predict(xg_ub_model_tun, alldataxgb)
  
}

kickstarter_predict_xg(category = "Drinks",
                       country = "US",
                       goal_usd =  40000,
                       launch_year = 2015,
                       project_name = "The Worst Disgusting Drink Ever",
                       duration = 30)


kickstarter_predict_gbm <- function(category, country, goal_usd, launch_year, project_name, duration, n.trees = 2000) {
  
  if(class(category) != "character") stop("category should be a character vector")
  if(!category %in% levels(clean_test$category)) stop("The entered category is not valid. Valid entries are: \n", print_and_capture(levels(clean_test$category)))
  if(class(country) != "character") stop("country should be a character vector")
  if(!country %in% levels(clean_test$country)) stop("The entered country is not valid. Valid entries are: \n", print_and_capture(levels(clean_test$country)))
  if(class(goal_usd) != "numeric") stop("goal_usd should be a numeric value")
  if(class(launch_year) != "numeric") stop("launch_year should be a numeric value")
  if(launch_year < 2009) stop("Kickstarter would definitely be revolutionary if it was lunched in ", launch_year,", however it was lunched in 2009 so please choose a year from 2009 onwards :)")
  if(class(project_name) != "character") stop("project_name should be a character vector")
  if(class(duration) != "numeric") stop("duration should be a numeric value")
  
  sentscore <- data.frame(name = c("good and bad", project_name)) %>% 
    unnest_tokens(word, name) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    summarise(total = sum(value))
  
  newdata <- train_data_gbm_ub[1, ]
  
  newdata[1, ] <- data.frame(category = category,
                             country = country,
                             usd_goal_real = goal_usd,
                             state_bi = 0,
                             launch_year = launch_year,
                             ovrl_sen = sentscore[1,1],
                             duration = duration)
  
  predict.gbm(kick_gbm_ub_tun, type = "response", n.trees, newdata = newdata)
  

kickstarter_predict_gbm(category = "Drinks",
                        country = "US",
                        goal_usd =  40000,
                        launch_year = 2015,
                        project_name = "The Worst Disgusting Drink Ever",
                        duration = 30)


### Prediction function logistic regression

kickstarter_predict_glm <- function(category, country, goal_usd, launch_year, project_name, duration) {
  
  if(class(category) != "character") stop("category should be a character vector")
   if(!category %in% levels(clean_test$category)) stop("The entered category is not valid. Valid entries are: \n", print_and_capture(levels(clean_test$category)))
  if(class(country) != "character") stop("country should be a character vector")
    if(!country %in% levels(clean_test$country)) stop("The entered country is not valid. Valid entries are: \n", print_and_capture(levels(clean_test$country)))
  if(class(goal_usd) != "numeric") stop("goal_usd should be a numeric value")
  if(class(launch_year) != "numeric") stop("launch_year should be a numeric value")
    if(launch_year < 2009) stop("Kickstarter would definitely be revolutionary if it was lunched in ", launch_year,", however it was lunched in 2009 so please choose a year from 2009 onwards :)")
  if(class(project_name) != "character") stop("project_name should be a character vector")
  if(class(duration) != "numeric") stop("duration should be a numeric value")
  
  sentscore <- data.frame(name = c("good and bad", project_name)) %>% 
    unnest_tokens(word, name) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    summarise(total = sum(value))
  
  predict.glm(logi_mod_ub, type = "response", newdata = data.frame(category = category,
                                                                   country = country,
                                                                   usd_goal_real = goal_usd,
                                                                   launch_year = launch_year,
                                                                   ovrl_sen = sentscore[1,1],
                                                                   duration = duration))
  
}

kickstarter_predict_glm(category = "Restaurants",
                        country  = "US",
                        goal_usd = 150000,
                        launch_year = 2025,
                        project_name = "The Delightful Cosy Fragrant Carpet",
                        duration = 30)
