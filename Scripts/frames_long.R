library(tidyverse)
library(here)
wide <- read_csv(here("data", "frames_wide.csv"))
frames_long <- wide %>%
  pivot_longer(names_to = "size_item", values_to ="response", large_item1:small_item7) %>%
  separate(size_item, c("size", "item"))
#another way----
frames_long <- wide %>%
  pivot_longer(names_to = c("size","item"),
               values_to ="response",
               large_item1:small_item7,
               names_sep = "_")
