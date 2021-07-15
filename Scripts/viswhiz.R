#load packages----

library(tidyverse)
library(here)
library(ggbeeswarm) #useful when we have a scatter plot and so many dots, it prevents dots from overlapping
library(RColorBrewer)

#set our favourite plot theme----
theme_set(theme_classic())

#read in cleanbeaches_new data----

plotbeaches <- read_csv(here("data", "cleanbeaches_new.csv"))

#plot buglevels by year----
##parenthesis allows to save it while also plotting it, usefull when we assign to an object (like p1).
(
p1 = plotbeaches %>% ##Pick a categorical variable for your x axis
                ##Pick a continuous variable for your y axis
  ggplot(aes(x = year, y = beachbugs)) + ##aesthetics are x and y, the + sign used to add more features to our plot
  geom_point()
)
## geom_point some kind of dot plot 
##we cannot see all the dots (n obs) therefore cannot see the spread of the data (how many obs are under 500 for example??)

#summarizing how many obs per year
 plotbeaches %>%
   group_by(year) %>%
   summarise(obs = n()) ##summarize the number of observations every year ( hence group by)
 ## obs is the name of the column we want to see the output in

#re-plotting buglevels by year by jitter/quasirandom

plotbeaches %>%
  ggplot(aes(x = year, y = beachbugs)) +
  geom_point() 

#plot buglevels by site----

plotbeaches %>%
  na.omit() %>% ##not to recieve error massage about NAs. it omits the whole row.
  ggplot(aes(x = site, y = beachbugs, color = year)) + ##sites on Xs not clear so we flip coor
  geom_jitter() +
  coord_flip()
##we notice that the color of year is a scale because if we glimpse we see that  R thinks year is  dbl (series of number) but we want it as factor (ordered variable)

#coerce year to be factor rather than integer
plotbeaches$year <- as.factor(plotbeaches$year)
##it will hellp us represent each year with a different color

#glimpse to see if it worked

glimpse(plotbeaches)

#facet_wrap----

plotbeaches %>%
  na.omit() %>%
  ggplot(aes(x = year, y = beachbugs, color = site)) +
  geom_jitter() +
  facet_wrap(~ site) +
  
  ##to separate plots by site

  theme(legend.position = "bottom")
##or write none instead of bottom to remove the legends
#combine filter and ggplot

plotbeaches %>%
  na.omit() %>%
  filter(beachbugs < 1000) %>%
  ggplot(aes(x = year, y = beachbugs, color = site)) +
  geom_jitter() +
  facet_wrap(~ site)
(
p6 = plotbeaches %>%
  drop_na() %>% #same as na.omit()
  filter(beachbugs < 1000) %>%
  filter(site %in% c("Coogee Beach", "Bondi Beach")) %>%
  ggplot(aes(x = year, y = beachbugs, color = site)) +
  geom_jitter() +
  facet_wrap(~ site)
)
#download plots on my device----

ggsave(here("output", "coogebondi.png")) ##saves the last plot

#boxes and violins----

plotbeaches %>%
  na.omit() %>%
  ggplot(aes(x = site, y = log(beachbugs + 1))) + ##to avoid getting missing values when the buglevel is zero
  geom_boxplot() + 
  coord_flip()

plotbeaches %>%
  na.omit() %>%
  ggplot(aes(x = year, y = logbeachbugs)) +
  geom_violin() ##tails are outliers, most data centered around low values, the wider the violin the more the obs that has same values

#filtered for buggier than average for this particular site = true

plotbeaches %>%
  na.omit() %>%
  filter(beachbugs_site == "TRUE") %>% ##double= if the question is : is it true or false
  ggplot(aes(x = year, y = logbeachbugs, color = year, fill = year)) +
  geom_violin()+ #to show distribution of the data
  facet_wrap(~site)

#histogram----
##we use it when we want to see if our data is normally distributed

#eyeballing our data for a quicky
hist(plotbeaches$beachbugs)
##notice that we just gave it the x axis because the y axis is by default the frequency of observations(how many times we observed 500 beachbug, for example, throught our data frame)

#detailed histogram

plotbeaches %>%
  na.omit() %>%
  filter(site == "Clovelly Beach",
         year == "2018") %>%
  ggplot(aes(x = beachbugs + 1)) +
  geom_histogram(binwidth = 0.5) + #bin is a bucket with an interval of values, if we make its wedth bigger then we are making it bigger, therefor it can contain more values in it, so the frequency will vary as a result
  scale_x_continuous(trans = "log") #to show the real levels while just transforming the x axis to log values

#combination plot----

plotbeaches %>%
  na.omit() %>%
  filter(beachbugs_site == "TRUE") %>%
  ggplot(aes(x = site, y = logbeachbugs)) +
  geom_boxplot() +
  geom_point(aes(color = year)) + #in those two lines order is important
  coord_flip()

plotbeaches %>%
  na.omit() %>%
  filter(site == "Clovelly Beach") %>%
  ggplot(aes(x = year, y = logbeachbugs)) + 
  geom_violin() +
  geom_quasirandom(aes(color = beachbugs_site)) # a nicer way to plot indivisual points than geom_point 

#Use geom_bar for frequency/count data
##how many observaions in this particular year (in our example)

plotbeaches %>%
  na.omit() %>%
  ggplot(aes(x = year)) +
  geom_bar(aes(fill = council)) + 
  facet_wrap(~site)

#use geom_col to plot a summary statistic (the default is sum)
plotbeaches %>%
  na.omit() %>%
  ggplot(aes(x = year, y = beachbugs)) +
  geom_col()

#check what geom_col has plotted
plotbeaches %>%
  na.omit() %>%
  group_by(year) %>%
  summarise(totalbugs = sum(beachbugs))


plotbeaches %>%
  na.omit() %>%
  group_by(year, site) %>%
summarize(meanbugs = mean(log(beachbugs + 1)), #log because data skewed and this is more representative than just mean
          .groups = 'drop') %>% # drop groups cuz it could mess with code later
  ggplot(aes(x = year, y = meanbugs, fill = year)) +
  geom_col()+
  facet_wrap(~site)

#add error bars----

plotbeaches %>%
  na.omit() %>%
  group_by(site) %>%
  summarise(mean = mean(log(beachbugs +1)),
            sd = sd(log(beachbugs + 1)),
            n = n(), ##number of observations
            stderr = sd/sqrt(n)) %>%
  ggplot(aes(x = site,
             y = mean,
             ymin = mean-sd,
             ymax = mean+sd)) + #mean+sd is more representative than mean+stderr
  geom_col() +
  coord_flip() + 
  geom_errorbar(aes(color = site))

#scatter plots to visualise the correlation between two continuous variables----

raintemp <- read_csv(here("data", "rain_temp_beachbugs.csv"))

raintemp %>%
  na.omit() %>%
  filter(beachbugs > 500) %>% ##for the regression line to be more representative we ignored values that are close to zero
  ggplot(aes(x = rain_mm, y = beachbugs, color = temp_airport)) +
  geom_point() +
  geom_smooth(method = loess, 
              formula = y ~ x) # ()added to avoid warning messege 

#how do I change x in ggplot----

##how to get rid of grey and gridlines
raintemp %>%
  na.omit() %>%
  filter(beachbugs > 500) %>% ##for the regression line to be more representative we ignored values that are close to zero
  ggplot(aes(x = rain_mm, y = beachbugs, color = temp_airport)) +
  geom_point() +
  geom_smooth() +
  theme_bw() ##gets rid of grey background


raintemp %>%
  na.omit() %>%
  filter(beachbugs > 500) %>% ##for the regression line to be more representative we ignored values that are close to zero
  ggplot(aes(x = rain_mm, y = beachbugs, color = temp_airport)) +
  geom_point() +
  geom_smooth() +
  theme_classic() ##gets rid of grey and gridlines

#For more information about using ggplot themes, check out the documentation: 
#visit https://ggplot2.tidyverse.org/reference/ggtheme.html
#https://cran.r-project.org/web/packages/cowplot/vignettes/introduction.html


#set the color to reflect that blue is cool and red is hot

raintemp %>%
  na.omit() %>%
  filter(beachbugs > 500) %>% 
  ggplot(aes(x = rain_mm, y = beachbugs, color = temp_airport)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  scale_color_gradient(low = "blue", high = "red")

#use a palette for setting colors

display.brewer.all() ##to see names of palettes
raintemp %>%
  na.omit() %>%
  filter(beachbugs > 500) %>% 
  ggplot(aes(x = rain_mm, y = beachbugs, color = temp_airport)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  scale_color_distiller(palette = "RdYlBu")
##When using colour palettes, the scale_colour_distiller function works well for continuous data,
##where you want a gradient of colour from low to high values.
##When using colour palettes to change the colour of bars (i.e., you want discrete colours) 
##apparently you want to use scale_colour_brewer()
##more palettes: https://github.com/karthik/wesanderson

#add titles and change axis labels----

raintemp %>%
  na.omit() %>%
  filter(beachbugs > 500) %>% 
  ggplot(aes(x = rain_mm, y = beachbugs, color = temp_airport)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  scale_color_distiller(name = "Temp (C)", palette = "RdYlBu") +
  labs(title = "Mean entercocci bacteria levels at eastern suburbs \nbeaches as a function of rainfall and temperature",
       subtitle = "only day > 500",
       caption = "data from https://www.environment.nsw.gov.au/beachapp/report_enterococci.aspx",
       x = "Rainfall_mm",
       y = "Enterococci levels")
##more info about lab https://ggplot2.tidyverse.org/reference/labs.html
##\n to wrap the title
##scale_color_distiller(name = "Temp (C).. ) to change names in the legends. 


#STORK PREFERENCE----

raintemp %>%
  drop_na() %>%
  mutate(temp_bin = cut_number(temp_airport,4)) %>% #Create 4 bins of temperature to plot it separately with facewrap
  ggplot(aes(x = log(rain_mm + 1),
             y = log(beachbugs + 1),
             color = temp_airport)) + 
  geom_point()+
  geom_smooth(method = "lm", #linear model
              se = FALSE, # standard error
              formula = y~x) +
  theme_classic()+
  scale_color_viridis_c(option = "plasma")+ #colorblind friendly palette, c is continuous
  labs(x = "log (rainfall [mm]+1)",
       y = "log (Enterococci+1)",
       color = "Temperature at Airport (°C)")+
  facet_wrap(~temp_bin)+
  theme(legend.position = "top")
#contour plots, shows the density of values
raintemp %>%
  drop_na() %>%
  filter(log(rain_mm +1) != 0) %>%
  # Create 4 bins of temperature
  ggplot(aes(x=log(rain_mm+1),
             y=log(beachbugs+1),
             color=temp_airport)) +
  #geom_point() +
  geom_density_2d_filled(contour_var = "count",
                         alpha = .5) +
  geom_density_2d(contour_var = "count",
                  size = 0.5, colour = "black") +
  geom_smooth(method = "lm",
              se = FALSE,
              formula = y ~ x) +
  theme_classic() +
  scale_fill_brewer() +
  labs(x = "log(rainfall [mm] + 1)",
       y = "log(enterococci + 1)",
       fill = "Observations",
       caption = "Note: Zeros removed from rainfall data") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0)) +
  #facet_wrap(~temp_bin) +
  theme(legend.position = "bottom")

# Alternative to plot 6 ------

library(ggdist)
(
  p6alt = plotbeaches %>% # get data
    na.omit() %>% # remove missing data
    mutate(site = as.factor(site)) %>% # ensure site is a factor
    filter(site %in% c("Coogee Beach", "Bondi Beach")) %>%
    ggplot(aes(x = as.factor(year),
               y = (beachbugs+1),
               color = site)) + # assign color by site
    facet_wrap(~site) + # create facets that are separated by site 
    stat_halfeye(.width = 0.5) + #densities instead of violins.. original is stat_eye which looks like an eye hence the name
    #it also shows the mean and the distribution (here is not normally distributed), and error bars that shows were 50%of values are plotted
    scale_y_continuous(trans = "log") + # to show raw data while transforming the axis into log
    labs(x = "Year",
         y = "log(enterococci + 1)",
         fill = "Site") + #for coloring. fill depending on site
    scale_color_viridis_d(option = "E") + #d as in discrete because the values are not continuous
    theme_tidybayes() +
    theme(legend.position = "top")
) 

