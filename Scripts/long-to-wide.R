library(tidyverse)
library(here)
spices <- readr::read_csv(here("data", "bakers_wide.csv"))
glimpse(spices)
spices %>% pivot_longer(names_to = "spice", values_to = "correct", cinnamon_1:nutmeg_3)

                        