library(tidyverse)
dir("_site", full.names = T) %>% keep(~str_detect(.x, "qmd")) %>% walk(quarto::quarto_render)