crime_tidy %>% 
  ggplot(aes(x = Population, y = Violent_Crimes)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm") +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(title = "Examining the Effect of Population of Violent Crimes",
       x = "Population",
       y = "Violent Crimes")
