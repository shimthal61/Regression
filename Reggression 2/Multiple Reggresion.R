MRes_tut2 <- read.csv("https://raw.githubusercontent.com/ajstewartlang/10_glm_regression_pt2/master/data/MRes_tut2.csv")

#Looking at the relationships between performance(corr_spell) and our other variables

ggplot(MRes_tut2, aes(x = age, y = corr_spell)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

ggplot(MRes_tut2, aes(x = RA, y = corr_spell)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

ggplot(MRes_tut2, aes(x = std_RA, y = corr_spell)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

ggplot(MRes_tut2, aes(x = std_SPELL, y = corr_spell)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

# Now we have to model the data

# Model 0 is the mean our or outcome variable

model0 <- lm(corr_spell ~ 1, data = MRes_tut2)

#Model 1 contains all of our predictors

model1 <- lm(corr_spell ~ age + RA + std_RA + std_SPELL, data = MRes_tut2)

anova(model0, model1)

check_model(model1)

# We can see when checking the model that case 10 has a disproportionate effect on our model. Let's exclude it:
MRes_tut2_drop10 <- filter(MRes_tut2, case != 10)

model2 <- lm(corr_spell ~ age + RA + std_RA + std_SPELL, data = MRes_tut2_drop10)

check_model(model2)

# Now, let’s look at the multicollinearity values measured by VIF:

vif(model2)

# RA amd std_RA look problematic as their multicollinearity values hugel deviate from 2. 

rcorr(MRes_tut2_drop10$RA, MRes_tut2_drop10$std_RA)

#The correlation is pretty high, so it's best to make a new model

model3 <- lm(corr_spell ~ age + std_RA + std_SPELL, data = MRes_tut2_drop10) 

check_model(model3)

vif(model3)
