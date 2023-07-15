source("utils.R")
# ?get_targeting
# get_targeting("41459763029", timeframe = "LAST_90_DAYS")
# debugonce(get_targeting)

library(httr)
library(tidyverse)

tstamp <- Sys.time()

write_lines(lubridate::as_date(tstamp), "tstamp.txt")

dir.create(paste0("historic/", lubridate::as_date(tstamp)), recursive = T)


source("utils.R")

unlink("targeting/7", recursive = T, force = T)
unlink("targeting/30", recursive = T, force = T)

dir.create("targeting/7")
dir.create("targeting/30")

 wtm_data <- openxlsx::read.xlsx("data/Presidential candidates, last 30 days.xlsx", sheet = 2) %>% janitor::clean_names()



all_dat <- wtm_data %>%
    # bind_rows(more_data) %>% 
    distinct(page_id, .keep_all = T) %>%
    add_count(page_name, sort  =T) %>%
    mutate(remove_em = n >= 2 & str_ends(page_id, "0")) %>%
    filter(!remove_em) %>%
    # filter(n >= 2) %>%
    # filter(n >= 2 & str_ends(page_id, "0", negate = T)) %>%
    select(-n)  

# all_dat %>% count(party, sort = T)
# all_dat %>% nrow

write_lines(all_dat %>% count(page_id, sort = T) %>% nrow, "n_advertisers.txt")



scraper <- function(.x, time = "7") {

  # print(paste0(.x$page_name,": ", round(which(internal_page_ids$page_id == .x$page_id)/nrow(internal_page_ids)*100, 2)))

  yo <- get_targeting(.x$page_id, timeframe = glue::glue("LAST_{time}_DAYS")) %>%
    mutate(tstamp = tstamp)

  if(nrow(yo)!=0){
    path <- paste0(glue::glue("targeting/{time}/"),.x$page_id, ".rds")
    # if(file.exists(path)){
    #   ol <- read_rds(path)
    #
    #   saveRDS(yo %>% bind_rows(ol), file = path)
    # } else {

    saveRDS(yo, file = path)
    # }
  }

  # print(nrow(yo))
  # })

}

scraper <- possibly(scraper, otherwise = NULL, quiet = F)


# if(F){
#     # dir("targeting/7", full.names
# }
# da30 <- readRDS("data/election_dat30.rds")
# da7 <- readRDS("data/election_dat7.rds")

already_there <- dir("targeting/7", full.names = T) %>% 
  str_remove_all("targeting/7/|\\.rds")

### save seperately
yo <- all_dat %>% #count(cntry, sort  =T) %>%
    # filter(!(page_id %in% already_there)) %>%
  # filter(!(page_id %in% unique(da7$page_id))) %>%
  # filter(cntry == "GB") %>%
  # slice(1:10) %>%
  split(1:nrow(.)) %>%
  map_dfr_progress(scraper, 7)

already_there <- dir("targeting/30", full.names = T) %>% 
  str_remove_all("targeting/30/|\\.rds")

yo <- all_dat %>% #count(cntry, sort  =T) %>%
    # filter(!(page_id %in% already_there)) %>%
    # filter(!(page_id %in% unique(da30$page_id))) %>%
    # filter(cntry == "GB") %>%
    # slice(1:10) %>%
    split(1:nrow(.)) %>%
    map_dfr_progress(scraper, 30)

# saveRDS(yo, file = )
library(tidyverse)
da30  <- dir("targeting/30", full.names = T) %>%
  map_dfr_progress(readRDS)  %>%
    mutate(total_spend_formatted = parse_number(total_spend_formatted)) %>%
    rename(page_id = internal_id) %>%
    left_join(all_dat)

# da30 %>%
#     count(party, sort = T) %>% View


da7  <- dir("targeting/7", full.names = T) %>%
    map_dfr_progress(readRDS) %>%
    mutate(total_spend_formatted = parse_number(total_spend_formatted)) %>%
    rename(page_id = internal_id) %>%
    left_join(all_dat)

saveRDS(da30, "data/election_dat30.rds")
saveRDS(da7, "data/election_dat7.rds")

saveRDS(da30, paste0("historic/", lubridate::as_date(tstamp), "/election_dat30.rds"))
saveRDS(da7, paste0("historic/", lubridate::as_date(tstamp), "/election_dat7.rds"))


# da30 %>%
#   # filter(party == "OTHER") %>% 
#   # # distinct(page_id, page_name) %>% 
#   # dput()
#   count(party)
# da7 %>% count(party)
# da30 %>% count(page_id)
# da30 %>% count(page_id)
# da7 %>% count(ds)
# 
# da30 %>% 
#   # filter(party == "Diğ") 
#   filter(str_detect(page_name, "\\bDEVA\\b|\\bDeva\\b"))
# 
# # bbb %>% filter(str_detect(funding_, "Strijker"))
# 
# # da7 %>%
# #   distinct(internal_id, .keep_all = T) %>%
# #   mutate(total_spend = parse_number(total_spend_formatted)) %>%
# #   rename(page_id = internal_id) %>%
# #   left_join(internal_page_ids) %>%
# #   group_by(party) %>%
# #   summarize(total_spend = sum(total_spend))
# #
# #
# # amgna <- da7 %>%
# #   mutate(total_spend = parse_number(total_spend_formatted)) %>%
# #   rename(page_id = internal_id) %>%
# #   left_join(internal_page_ids)
# #
# #
# # amgna %>%
# #   filter(type == "gender") %>%
# #   filter(value == "Women") %>%
# #   # mutate(total_spend = total_spend*total_spend_pct) %>%
# #   ggplot(aes(party, total_spend_pct)) +
# #   geom_boxplot() #+
# #   # scale_y_log10()
# #
# #
# #
# # amgna %>%
# #   filter(type == "detailed")
# 
