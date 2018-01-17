library(dplyr)
library(rpart)
library(rattle)

#setwd('~/University of Washington/Senior/Fall/Info 370/project-es3')

data <- read.csv('data/clean_num.csv')

# reformat data
data <- data %>%
  # reformat graduation data as date
  mutate(graduation_date = as.Date(graduation_date, "%m/%d/%Y")) %>%
  
  # split job type into 3 binary columns (1 = internship, 2 = part time, 3 = full time)
  mutate(fulltime = ifelse(Job_type == 3, 1, 0)) %>%
  mutate(parttime = ifelse(Job_type == 2, 1, 0)) %>%
  mutate(internship = ifelse(Job_type == 1, 1, 0)) %>%
  select(-Job_type) %>%
  
  # split class standing into 5 binary columns
  mutate(freshman = ifelse(class_standing_ == 1, 1, 0)) %>%
  mutate(sophomore = ifelse(class_standing_ == 2, 1, 0)) %>%
  mutate(junior = ifelse(class_standing_ == 3, 1, 0)) %>%
  mutate(senior = ifelse(class_standing_ == 4, 1, 0)) %>%
  mutate(fifth.year = ifelse(class_standing_ == 5, 1, 0)) %>%
  mutate(alumni = ifelse(class_standing_ == 6, 1, 0)) %>%
  select(-class_standing_) %>%
  
  #get subset where job found in <= 4 months
  filter(Months <= 4)

# split into training and test datasets
# (filter out people just beginning job search (no job, searching for < 3 mo.))
train <- data %>%
  filter((has_position == 0 & Months == 0) | (None_of_these == 1 & Months == 0) | has_position == 1 | None_of_these == 0) %>%
  select(-None_of_these, -has_position)
  
test <- data %>%
  filter((has_position == 0 & Months == 1) | (None_of_these == 1 & Months == 1) ) %>%
  select(-None_of_these, -has_position)

# create decision tree
tree <- rpart(Months ~ ., data = train, method = "anova", control=rpart.control(minbucket=2))
fancyRpartPlot(tree)

# make predictions
predict(tree, test)
