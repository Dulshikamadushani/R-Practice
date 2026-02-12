# Explain the following concepts about ggplot 

# a. What three elements do you need to produce a ggplot?
# a dataset 
# aesthetic mappings (aes)
# geometric objects (geoms) 

# b. What is a geom? 
# A geom (geometric object) is the visual representation of data in a plot. Ex:
# geom_boxplot() → boxplot
# geom_bar() → bar chart
# geom_point() → scatter plot

# c. What is a facet? 
# A facet splits a plot into multiple panels based on a categorical variable, therefore can compare groups easily.

# d. Explain the concept of layering. 
# Layering means building a plot by adding components one at a time using + operation.

# e. Where do you add x and y variables and map different shapes, colors, and other attributes to the data? 
# Inside the aes() function.


library(tidyverse)

# 2.Make a boxplot using ggplot with DON as the y variable, treatment as the x variable, and color mapped to the wheat cultivar. Change the y label to “DON (ppm)” and make the x label blank.
library(tidyverse) 
toxin.data <- read.csv("MycotoxinData.csv", na.strings = "na")
ggplot(toxin.data, aes(x=Treatment, y=DON, color = Cultivar)) +
  geom_boxplot() + 
  labs(y="DON (ppm)", x= "") 

# 3. Now convert this data into a bar chart with standard-error error bars using the stat_summary() command. (hint: use position = dodge)
ggplot(toxin.data, aes(x = Treatment, y = DON, fill= Cultivar)) +
  stat_summary(fun = mean, geom = "bar", 
               position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.9)) +
  labs(y = "DON (ppm)", 
       x = "")

# 4. Add points to the foreground of the boxplot and bar chart you made in questions 2 & 3 that show the distribution of points. Set the shape = 21 and the outline color black (hint: use jitter_dodge). 
# For boxplot
ggplot(toxin.data, aes(x = Treatment, y = DON, fill = Cultivar)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_point(aes(fill = Cultivar),
             shape = 21,
             color = "black",
             size = 2,
             position = position_jitterdodge(jitter.width = 0.2,dodge.width = 0.8)) +
  labs(y = "DON (ppm)",
       x = "")
#For bargraph
ggplot(toxin.data, aes(x = Treatment, y = DON, fill = Cultivar)) +
  stat_summary(fun = mean,
               geom = "bar",
               position = position_dodge(width = 0.8)) +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = 0.2,
               position = position_dodge(width = 0.8)) +
  geom_point(aes(fill = Cultivar),
             shape = 21,
             color = "black",
             size = 2,
             position = position_jitterdodge(
               jitter.width = 0.2,
               dodge.width = 0.8
             )) +
  labs(y = "DON (ppm)",
       x = "")

# 5. Add a facet to the plots based on cultivar.
# For boxplot
ggplot(toxin.data, aes(x = Treatment, y = DON, fill = Cultivar)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_point(aes(fill = Cultivar),
             shape = 21,
             color = "black",
             size = 2,
             position = position_jitterdodge(jitter.width = 0.2,dodge.width = 0.8)) +
  labs(y = "DON (ppm)", x = "") + 
  facet_wrap(~Cultivar)
# For Bar graph
ggplot(toxin.data, aes(x = Treatment, y = DON, fill = Cultivar)) +
  stat_summary(fun = mean,
               geom = "bar",
               position = position_dodge(width = 0.8)) +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = 0.2,
               position = position_dodge(width = 0.8)) +
  geom_point(aes(fill = Cultivar),
             shape = 21,
             color = "black",
             size = 2,
             position = position_jitterdodge(
               jitter.width = 0.2, dodge.width = 0.8 )) +
  labs(y = "DON (ppm)", x = "")+ 
  facet_wrap(~Cultivar)

# 6. Add transparency to the points so you can still see the boxplot or bar in the background.
# For boxplot
ggplot(toxin.data, aes(x = Treatment, y = DON, fill = Cultivar)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_point(aes(fill = Cultivar),
             shape = 21,
             color = "black",
             size = 2,
             alpha= 0.5,  
             position = position_jitterdodge(jitter.width = 0.2,dodge.width = 0.8)) +
  labs(y = "DON (ppm)", x = "") + 
  facet_wrap(~Cultivar)
# For Bar graph
ggplot(toxin.data, aes(x = Treatment, y = DON, fill = Cultivar)) +
  stat_summary(fun = mean,
               geom = "bar",
               position = position_dodge(width = 0.8)) +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = 0.2,
               position = position_dodge(width = 0.8)) +
  geom_point(aes(fill = Cultivar),
             shape = 21,
             color = "black",
             size = 2,
             alpha= 0.5,
             position = position_jitterdodge(
               jitter.width = 0.2, dodge.width = 0.8 )) +
  labs(y = "DON (ppm)", x = "")+ 
  facet_wrap(~Cultivar)






