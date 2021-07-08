library(tidyverse)
library(here)
wide <- readr::read_csv(here("data", "beachbugs_wide.csv"))
glimpse(wide)
longbeaches <- wide %>% 
  pivot_longer(names_to = "beach", values_to = "bugglevels", `Bondi Beach`:`Tamarama Beach`)
long <- read_csv(here("data", "beachbugs_long.csv"))
widebeaches <- long %>% 
  pivot_wider(names_from = site, values_from = buglevels) ##no quot variable already exists
