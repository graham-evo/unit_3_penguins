# Graham C. McLaughlin
# 2022-02-21
# t-tests

library(palmerpenguins)
library(rstatix)
library(tidyverse)

head(penguins)

ggplot(data = penguins) +
  geom_histogram(aes(x=body_mass_g, fill = species))

#One-sample t-test
gentoo <- penguins %>%
  filter(species=="Gentoo")
head(gentoo)

ggplot(data = gentoo) +
  geom_histogram(aes(x=body_mass_g))

#Assumptions of t-test
# -normally distributed
# -No outliers

#Let's check the normality assumption:
ggplot(data = gentoo) +
  stat_qq(aes(sample = body_mass_g)) #plots your sample against a theoretical normal distribution from smallest to largest. Data should fit a 1:1 line approximately.

gentoo %>%
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm = TRUE),
            sd_body_mass_g = sd(body_mass_g, na.rm = TRUE))

#Let's run a One-sample t-test:

# Base r t-test
t.test(gentoo$body_mass_g, mu = 5500)

# TidyVerse t-test
t_test_results <- gentoo %>% 
  t_test(body_mass_g ~ 1, mu = 5500)

#Two-sample t-test:

# Are the adele penguins body mass different from the gentoo penguin body mass?

data_for_ttest <- penguins %>%
  filter(species %in% c("Gentoo", "Adelie"), #Only keep Gentoo and Adelie
         !is.na(body_mass_g)) %>% #Get rid of NAs
  select(species, body_mass_g) %>% #Which columns should we keep
  droplevels() #Drops unrepresented factor levels, Chinstrap is removed

summary(data_for_ttest) #We can see that we removed the chinstrap species

data_for_ttest %>%
  group_by(species) %>%
  summarize(mean = mean(body_mass_g),
            sd = sd(body_mass_g))

ggplot(data = data_for_ttest) +
  stat_qq(aes(sample = body_mass_g)) +
  facet_wrap(~species, scales = "free") #changing fixed scales to free scales

# Check equality of variance assumption:
data_for_ttest %>% 
  levene_test(body_mass_g ~ species) #variances are not significantly different

t.test(data_for_ttest$body_mass_g ~ data_for_ttest$species, var.equal = TRUE) #Two sample t-test is significant. The two species are statistically different in terms of body mass. R automatically uses the Welch's t-test for unequal variances. Welch's is more conservative than the student's t-test so we can trust the p-value from this test. However, we can tell the t.test function that the variances are indeed equal and force the t.test function to use a normal two sample t-test.


# Running correlations:

ggplot(data = gentoo) +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm)) 

ggplot(data = gentoo) +
  stat_qq(aes(sample = bill_depth_mm)) #Checking normality assumption

cor(x = gentoo$bill_depth_mm, y = gentoo$bill_length_mm, use = "complete.obs") #cor() outputs the correlation coefficient (Pearson's R)
cor.test(x = gentoo$bill_depth_mm, y = gentoo$bill_length_mm, use = "complete.obs") #Gives us a more complete suite of test statistics.
gentoo %>%
  cor_test(bill_length_mm, bill_depth_mm) #Same test but through dplyr piping.
# For running correlations for non-normal distributed data you can use a different test through the cor.test or cor function (i.e. Kendall test)

#Let's run a correlation matrix:
cor(gentoo[ ,c(3:6)], use = "complete.obs") #kind of like a pairwise comparison but with correlation tests.

#Running fancier correlation matrix
library(GGally) #adds the ggpairs() functionality

penguins %>%
  select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  ggpairs(aes(color = species))


