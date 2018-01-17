library(ggplot2)
library(base)
library(car)

#re-ordered columns to make the for loop easy to write
data <- read.csv('../data/clean_num_sorted.csv') 

#correlation between amount a resource was used and getting an offer from that resource
for (i in 31:38){
  print(ggplot(data, aes(x = data[[i + 10]], y = data[[i]])) +
          geom_point(alpha = 0.1) +
          geom_smooth(method=lm) +
          labs(y = colnames(data)[i], x = colnames(data)[i + 10]))
}

#exploring possible correlations in a really inefficient way 
data <- read.csv('../data/clean_num.csv')


no_out <- filter(data, online_job_postings < 500)
short_search <- filter(data, Months <= 3) 


ggplot(no_out, aes(x = networking, y = online_job_postings)) +
  geom_point(alpha = 0.1 ) + 
  geom_smooth(method=loess) 

ggplot(data, aes(x = cumulative_GPA, y = internships)) +
  geom_point(alpha = 0.1 ) + 
  geom_smooth(method=loess) 

ggplot(data, aes(x = cumulative_GPA, y = knowledge)) +
  geom_point(alpha = 0.1 ) + 
  geom_smooth(method=loess) 

ggplot(data, aes(x = knowledge, y = cumulative_GPA)) +
  geom_point(alpha = 0.1 ) + 
  geom_smooth(method=loess) 

ggplot(no_out, aes(x = gender, y = self.confidence)) +
  geom_point(alpha = 0.1 ) + 
  geom_smooth(method=loess) 

#cook's distance > 4/n can remove (?)
plot(cooks.distance(lm(online_job_postings ~ good_at_resume_cover_letters, data)))

ggplot(data, aes(x = good_at_resume_cover_letters, y = cover_letter_hours)) +
  geom_point(alpha = 0.1 ) + 
  geom_smooth(method=lm) 

ggplot(no_out, aes(x = online_job_postings, y = resume_hrs)) +
  geom_point(alpha = 0.1 ) + 
  geom_smooth(method=loess) 

ggplot(no_out, aes(x = online_job_postings, y = cover_letter_hours)) +
  geom_point(alpha = 0.1 ) + 
  geom_smooth(method=loess) 

ggplot(data, aes(y = online_job_postings, x = Months)) +
  geom_point(alpha = 0.1 ) + 
  geom_smooth(method=lm) 

postings.lm <- lm(online_job_postings ~ Months, data)
summary(postings.lm)
plot(resid(postings.lm))
?lm



