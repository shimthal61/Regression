#Check whether the same relationship holds for population size and robberies in 2015.

robberies_filtered <- crime_tidy %>% 
  filter(Population < 2000000)


robberies_filtered %>% 
  ggplot(aes(x = Population, y = Robberies)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(title = "Examining the effect of population size on robberies",
       x = "Population",
       y = "Robberies")

rcorr(robberies_filtered$Population, robberies_filtered$Robberies)
# r = 0.81
print(0.67*0.67)*100
#45% of the variance can be explained by population

robberies_1 <- lm(Robberies ~ 1, data = robberies_filtered)
robberies_2 <- lm(Robberies ~ Population, data = robberies_filtered)

check_model(robberies_2)

anova(robberies_1, robberies_2)

summary(robberies_2) 0.005693

print(1000000*0.005693)+944.3

#We can expect there to be 6637.3 robberies

#Are house prices predicted by the number of violent crimes in 2015?
House_price_filtered <- crime_tidy %>% 
  filter(Violent_Crimes < 40000)

House_price_filtered %>% 
  ggplot(aes(x = House_Price, y = Violent_Crimes)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs (y = "Violent Crimes",
        x = "House Price")

rcorr(House_price_filtered$House_Price, House_price_filtered$Violent_Crimes)

print(0.05*0.05)*100
#0.25% of the variance in house prices can be explained by violent crimes

House_model1 <- lm(House_Price ~ 1, data = House_price_filtered)
House_model2 <- lm(House_Price ~ Violent_Crimes, data = House_price_filtered)

check_model(House_model2)

anova(House_model1, House_model2)

summary(House_model2)

#Are house prices predicted by population size in 2015?

population_filtered <- crime_tidy %>% 
  filter(Population < 2000000)

population_filtered %>% 
  ggplot(aes(x= House_Price, y = Population)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "House Price",
       y = "Population")

rcorr(population_filtered$Population, population_filtered$House_Price)

pop_house_1 <- lm(House_Price ~ 1, data = population_filtered)
pop_house_2 <- lm(House_Price ~ Population, data = population_filtered)

check_model(pop_house_2)

anova(pop_house_1, pop_house_2)

summary(pop_house_2)