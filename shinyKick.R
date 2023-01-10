library(shiny)
library(tidyverse)
library(lubridate)
library(tidytext)
library(assertive.base)
library(gbm)
library(xgboost)
library(UPG)
library(caret)

filepath <- "D:/Users/Gregor/Desktop/Kaggle/Kickstarter projects/ks-projects-201801.csv" 
kickstarter_data <- read_csv(filepath, col_names = T)

kickstarter_data <- kickstarter_data[sample(nrow(kickstarter_data), 50000), ]

kickstarter_data <- kickstarter_data %>%
  mutate(category = factor(category),
         main_category = factor(main_category),
         currency = factor(currency),
         state = factor(state),
         country = factor(country))

kickstarter_clean <- kickstarter_data %>%
  filter(!(year(launched) %in% c(1970, 2018))) %>% #We remove the projects from 1970 and 2018
  filter(!(country == 'N,0"')) %>% #We remove the projects with missing information about the countries
  filter(!is.na(name)) 

kickstarter_clean <- kickstarter_clean %>%
  filter(state %in% c("failed", "canceled", "successful", "suspended")) %>% #Omitting live and undefined projects
  mutate(state_bi = factor(ifelse(state == "successful", 1, 0))) %>% #Creating a factor -> 1 for a successful project, 0 otherwise
  mutate(launch_year = year(launched)) %>% #We aren't interested in datetime in our case, just the year of the project
  mutate(launched = date(launched)) 

kickstarter_clean <- kickstarter_clean %>%
  select(-c(pledged, state, `usd pledged`, goal, currency)) %>% #We remove redundant variables
  group_by(main_category) %>%
  mutate(bi_cat = mean(as.numeric(state_bi) - 1)) %>% #We calculate project success rates by main category
  ungroup()

kickstarter_afinn <- kickstarter_clean %>%
  unnest_tokens(word, name) %>%
  inner_join(get_sentiments("afinn"), by = "word")

kickstarter_afinn <- kickstarter_afinn %>% 
  group_by(ID) %>%
  mutate(ovrl_sen = sum(value)) %>% #We calculate the overall sentiment value of a projects with longer names by adding the sentiment scores of the included words together
  select(ID, ovrl_sen)

clean_afinn <- left_join(kickstarter_clean, unique(kickstarter_afinn), by = "ID") #We join the AFINN sentiment values with the original dataset, using only unique values to avoid duplication
table(clean_afinn$ovrl_sen) #A brief overview of the range of our calculated sentiment values
sum(is.na(clean_afinn$ovrl_sen))

clean_afinn <- clean_afinn %>% 
  replace_na(list(ovrl_sen = 0)) 

clean_afinn <- clean_afinn %>%
  mutate(duration = as.numeric(deadline - launched))

clean_train <- clean_afinn %>%
  select(-c("ID", "name", "deadline", "backers", "usd_pledged_real", "bi_cat", "launched", "main_category"))

clean_train$country <- droplevels(clean_train$country)

kicklm <- glm(state_bi ~ ., data = clean_train, family = binomial)
summary(kicklm)

clean_gbm <- clean_train
clean_gbm$state_bi <- as.numeric(clean_gbm$state_bi) - 1

kickgbm <- gbm(state_bi ~ ., data = clean_gbm, distribution = "bernoulli", n.trees = 2000, shrinkage = 0.01, interaction.depth = 4, cv.folds = 5)
summary(kickgbm)

xg_train <- clean_train %>%
  select(-c(state_bi, category, country)) #We remove any non numeric variables and the target variable

xg_train_lab <- clean_train$state_bi #Saving the labels
categories_xg <- model.matrix(~category + country -1, clean_train) #One-hot encoding

xg_train_num <- cbind(xg_train, categories_xg) #We combine the two datasets
clean_xgtrain <- data.matrix(xg_train_num) #And transform it into a matrix
clean_xgtrain <- xgb.DMatrix(data = clean_xgtrain, label = as.numeric(as.character(xg_train_lab)))

negative_cases <- sum(xg_train_lab == 0)
positive_cases <- sum(xg_train_lab == 1)

kickxg <- xgboost(clean_xgtrain,              #We train the tuned model on the unbalanced dataset
                           max.depth = 10,
                           nround = 3000,
                           early_stopping_rounds = 5,
                           objective = "binary:logistic",
                           scale_pos_weight = negative_cases/positive_cases,
                           gamma = 10,
                           min_child_weight = 8,
                           eta = 0.667,
                           verbose = 0)
kickxg

baydat <- clean_train %>%
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

baydat <- baydat %>%
  mutate(launch_year = as.factor(launch_year),
         goal_bin = as.factor(goal_bin),
         sentiment_bins = as.factor(sentiment_bins),
         duration_bin = as.factor(duration_bin))

baydat <- baydat[, c(-3,-6,-7)]

baydat <- baydat %>%
  group_by(category, country, launch_year, goal_bin, sentiment_bins, duration_bin) %>%
  summarise(category = category, country = country, launch_year = launch_year,
            goal_bin = goal_bin, sentiment_bins = sentiment_bins, duration_bin = duration_bin,
            total = n(), successful = sum(as.numeric(state_bi) - 1)) %>%
  ungroup()

baydat <- unique(baydat)

intercept <- rep(1, nrow(baydat))

baydat <- cbind(intercept, baydat)

y <- baydat[, 9]
Ni <- baydat[, 8]
X <- baydat[, c(-8,-9)]

bay_form <- dummyVars(~ category + country + launch_year + goal_bin + sentiment_bins + duration_bin,
                      data = baydat,
                      levelsOnly = F)

baydum <- as_tibble(predict(bay_form, newdata = baydat))

baydum <- cbind(intercept, baydum)


kickbay <- UPG(y = y, Ni = Ni, X = baydum, model = "binomial")


kickstarter_predict_glm <- function(category, country, goal_usd, launch_year, project_name, duration) {
  
  if(class(category) != "character") stop("category should be a character vector")
  if(!category %in% levels(clean_train$category)) stop("The entered category is not valid. Valid entries are: \n", print_and_capture(levels(clean_train$category)))
  if(class(country) != "character") stop("country should be a character vector")
  if(!country %in% levels(clean_train$country)) stop("The entered country is not valid. Valid entries are: \n", print_and_capture(levels(clean_train$country)))
  if(class(goal_usd) != "numeric") stop("goal_usd should be a numeric value")
  if(class(launch_year) != "numeric") stop("launch_year should be a numeric value")
  if(launch_year < 2009) stop("Kickstarter would definitely be revolutionary if it was lunched in ", launch_year,", however it was lunched in 2009 so please choose a year from 2009 onwards :)")
  if(class(project_name) != "character") stop("project_name should be a character vector")
  if(class(duration) != "numeric") stop("duration should be a numeric value")
  
  sentscore <- data.frame(name = c("good and bad", project_name)) %>% 
    unnest_tokens(word, name) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    summarise(total = sum(value))
  
  predict.glm(kicklm, type = "response", newdata = data.frame(category = category,
                                                                   country = country,
                                                                   usd_goal_real = goal_usd,
                                                                   launch_year = launch_year,
                                                                   ovrl_sen = sentscore[1,1],
                                                                   duration = duration))
  
}


kickstarter_predict_gbm <- function(category, country, goal_usd, launch_year, project_name, duration, n.trees = 2000) {
  
  if(class(category) != "character") stop("category should be a character vector")
  if(!category %in% levels(clean_train$category)) stop("The entered category is not valid. Valid entries are: \n", print_and_capture(levels(clean_train$category)))
  if(class(country) != "character") stop("country should be a character vector")
  if(!country %in% levels(clean_train$country)) stop("The entered country is not valid. Valid entries are: \n", print_and_capture(levels(clean_train$country)))
  if(class(goal_usd) != "numeric") stop("goal_usd should be a numeric value")
  if(class(launch_year) != "numeric") stop("launch_year should be a numeric value")
  if(launch_year < 2009) stop("Kickstarter would definitely be revolutionary if it was lunched in ", launch_year,", however it was lunched in 2009 so please choose a year from 2009 onwards :)")
  if(class(project_name) != "character") stop("project_name should be a character vector")
  if(class(duration) != "numeric") stop("duration should be a numeric value")
  
  sentscore <- data.frame(name = c("good and bad", project_name)) %>% 
    unnest_tokens(word, name) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    summarise(total = sum(value))
  
  newdata <- clean_gbm[1, ]
  
  newdata[1, ] <- data.frame(category = category,
                             country = country,
                             usd_goal_real = goal_usd,
                             state_bi = 0,
                             launch_year = launch_year,
                             ovrl_sen = sentscore[1,1],
                             duration = duration)
  
  predict.gbm(kickgbm, type = "response", n.trees, newdata = newdata)
  
}

kickstarter_predict_xg <- function(category, country, goal_usd, launch_year, project_name, duration) {
  
  if(class(category) != "character") stop("category should be a character vector")
  if(!category %in% levels(clean_train$category)) stop("The entered category is not valid. Valid entries are: \n", print_and_capture(levels(clean_train$category)))
  if(class(country) != "character") stop("country should be a character vector")
  if(!country %in% levels(clean_train$country)) stop("The entered country is not valid. Valid entries are: \n", print_and_capture(levels(clean_train$country)))
  if(class(goal_usd) != "numeric") stop("goal_usd should be a numeric value")
  if(class(launch_year) != "numeric") stop("launch_year should be a numeric value")
  if(launch_year < 2009) stop("Kickstarter would definitely be revolutionary if it was lunched in ", launch_year,", however it was lunched in 2009 so please choose a year from 2009 onwards :)")
  if(class(project_name) != "character") stop("project_name should be a character vector")
  if(class(duration) != "numeric") stop("duration should be a numeric value")
  
  start_list <- clean_train[0, ] %>%
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
  
  predict(kickxg, alldataxgb)
  
}


kickstarter_predict_bay <- function(category, country, goal_usd, launch_year, project_name, duration) {
  
  if(class(category) != "character") stop("category should be a character vector")
  if(!category %in% levels(clean_train$category)) stop("The entered category is not valid. Valid entries are: \n", print_and_capture(levels(clean_train$category)))
  if(class(country) != "character") stop("country should be a character vector")
  if(!country %in% levels(clean_train$country)) stop("The entered country is not valid. Valid entries are: \n", print_and_capture(levels(clean_train$country)))
  if(class(goal_usd) != "numeric") stop("goal_usd should be a numeric value")
  if(class(launch_year) != "numeric") stop("launch_year should be a numeric value")
  if(launch_year < 2009) stop("Kickstarter would definitely be revolutionary if it was lunched in ", launch_year,", however it was lunched in 2009 so please choose a year from 2009 onwards :)")
  if(class(project_name) != "character") stop("project_name should be a character vector")
  if(class(duration) != "numeric") stop("duration should be a numeric value")

  
  sentscore <- data.frame(name = c("good and bad", project_name)) %>% 
    unnest_tokens(word, name) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    summarise(total = sum(value))
  
  newobs <- data.frame(category = category,
                       country = country,
                       usd_goal_real = goal_usd,
                       launch_year = as.factor(launch_year),
                       ovrl_sen = sentscore[1,1],
                       duration = duration)

  newobs <- newobs %>%
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

  newobs <- as_tibble(predict(bay_form, newdata = newobs))
  newobs <- cbind(intercept = intercept[1], newobs[1, ])
  
  predict(kickbay, newdata = newobs, q =c(0.05, 0.95))
  
}

#-------------------------------------------

ui <- fluidPage(
  titlePanel("What are your % of Success?"),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "money", "How much $?", value = 5000),
      numericInput(inputId = "duration", "How long?", value = 30),
      selectInput(inputId = "country", "Which country do you want to launch from?",
                  choices = levels(unique(clean_train$country))),
      selectInput(inputId = "category", "Which category is the product from?",
                  choices = levels(unique(clean_train$category))),
      numericInput(inputId = "year", "Which year will you launch the product?",
                   min = 2009, value = 2020),
      textInput(inputId = "name", "What will be the name of the project?", value = "")),
  mainPanel(
    tabsetPanel(
      tabPanel("LM", verbatimTextOutput(outputId = "lm")),
      tabPanel("GBM", verbatimTextOutput(outputId = "gbm")),
      tabPanel("XGBoost", verbatimTextOutput(outputId = "xg"))
    )
  )
))

server <- function(input, output) {
  output$lm <- renderPrint({kickstarter_predict_glm(category = input$category, country = input$country, goal_usd = as.numeric(input$money),
                                                      launch_year = as.numeric(input$year), project_name = input$name, duration = as.numeric(input$duration))[[1]]*100})
  output$gbm <- renderPrint({kickstarter_predict_gbm(category = input$category, country = input$country, goal_usd = as.numeric(input$money),
                                                    launch_year = as.numeric(input$year), project_name = input$name, duration = as.numeric(input$duration))*100})
  output$xg <- renderPrint({kickstarter_predict_xg(category = input$category, country = input$country, goal_usd = as.numeric(input$money),
                                                    launch_year = as.numeric(input$year), project_name = input$name, duration = as.numeric(input$duration))*100})
}

shinyApp(ui = ui, server = server)


kickstarter_predict_glm(category = "Restaurants", country = "US", goal_usd = 5000,
                        launch_year = 2015, project_name = "Lovely", duration = 30)[[1]]
kickstarter_predict_gbm(category = "Restaurants", country = "US", goal_usd = 5000,
                        launch_year = 2015, project_name = "Lovely", duration = 30)*100
kickstarter_predict_xg(category = "Restaurants", country = "US", goal_usd = 5000,
                       launch_year = 2015, project_name = "Lovely", duration = 30)*100
