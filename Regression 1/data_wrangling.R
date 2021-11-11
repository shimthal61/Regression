crime_messy <- read.csv("https://raw.githubusercontent.com/ajstewartlang/09_glm_regression_pt1/master/data/crime_dataset.csv")
head(crime_messy)

#We do some data wrangling, changing our column names
crime_tidy <- crime_messy %>% 
  separate(col = "City..State", into = c("City", "State")) %>%
  rename(House_Price = "index_nsa") %>% 
  rename(Violent_Crimes = "Violent.Crimes")
<<<<<<< HEAD
=======

head(crime_tidy)


     
>>>>>>> fc5e3d241d58738b0ba82859e7c93033bc4eb69f
