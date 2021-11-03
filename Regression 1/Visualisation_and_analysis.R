crime_tidy %>% 
  ggplot(aes(x = Population, y = Violent_Crimes)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm") +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(title = "Examining the Effect of Population of Violent Crimes",
       x = "Population",
       y = "Violent Crimes")

#We can carry out Pearson's r
rcorr(crime_tidy$Population, crime_tidy$Violent_Crimes)
#Looking at the top, our r value is 0.81
#Looking at the bottom, our p value is 0 (i.e. p < 0.001.)

#Let's calculate our r^2 value
(0.81*0.81)*100

#Therefore, around 66% of the variance in our Violent_Crimes variable is explained by population size

#Filtering our data so that our data is not being skewed
crime_filtered <- crime_tidy %>% 
  filter(Population < 2000000)

#New plot with filtered data set
crime_filtered %>% 
  ggplot(aes(x = Population, y = Violent_Crimes)) +
  geom_point(alpha = .25) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(title = "Examining the Effect of Population on Violent Crimes",
       x = "Population",
       y = "Violent Crime")


#Pearson's r with filtered data set
rcorr(crime_filtered$Population, crime_filtered$Violent_Crimes)

#Calculate r^2 
print(0.69*0.69)*100

#Therefore, about 48% of the variance in Violent Crimes are explained by population size

crime_filtered <- filter(crime_filtered, Year == 2015)

crime_filtered %>% 
  ggplot(aes(x = Population, y = Violent_Crimes, label = City)) +
  geom_point() +
  geom_text(nudge_y = 500, check_overlap = TRUE) +
  geom_smooth(method = "lm", se = FALSE) +
  xlim(0, 1800000) +
  theme_minimal() +
  theme(text = element_text(size = 13)) + 
  labs(title = "Examining the Effect of Population on Violent Crimes",
       x = "Population",
       y = "Violent Crimes")

rcorr(crime_filtered$Population, crime_filtered$Violent_Crimes)  


#We need to create two linear models. 
#Model 1 will use the means of out outcome variable as the predictor
model1 <- lm(Violent_Crimes ~ 1, data = crime_filtered)

#Model 2 will use Population size to predict Violent Crimes
model2 <- lm(Violent_Crimes ~ Population, data = crime_filtered)

#Now we check our assumptions
check_model(model2)

#Finally, we can use the anova() function to see if the model which uses 
#population as a predictor is better than using the model which uses the mean

anova(model1, model2)

#We can see that the RSS (Residual Sum of Squares) is less in out second model.
#Therefore, the using Population as a predictor is better than using the mean

#Let's get the parameter estimates of model 2

summary(model2)
#We can see that every time population increases by 1, violent crime is estimated to increase by 0.006963
#Therefore, we can estimate the amount of violent crime in a place with population 1,000,000 

#(Estimate * Population) + Intercept 
print(0.006963*1000000)+944.3 

#For a city with population 1,000,000, we estimate there will be 7907.3 crimes