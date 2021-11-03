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
print(0.81*0.81)*100
#Therefore, around 66% of the variance in our Violent_Crimes variable is explained by population size

crime_filtered <- crime_tidy %>% 
  filter(Population < 2000000)

crime_filtered %>% 
  ggplot(aes(x = Population, y = Violent_Crimes)) +
  geom_point(alpha = .25) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(title = "Examining the Effect of Population on Violent Crimes",
       x = "Population",
       y = "Violent Crime")
