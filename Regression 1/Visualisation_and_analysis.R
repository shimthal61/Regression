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

#Lets create a linear model. First we filter for just the year 2015
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

print(0.65*0.65)*100
