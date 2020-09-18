library(tidyverse)
library(eph)
library(rio)
library(janitor)

lista_eph <- list()

#download data
lista_eph$a2010t4 <- get_microdata(year = 2010, trimester = 4)
lista_eph$a2016t4 <- get_microdata(year = 2016, trimester = 4)
lista_eph$a2019t4 <- get_microdata(year = 2019, trimester = 4)


#run for 2016,2019
data <- data.frame()

added_id <- lista_eph$a2019t4 %>% 
  select(PONDII, CH04, P21, P47T, AGLOMERADO, ESTADO) %>% 
  filter(CH04 == 2,
         ESTADO == 1) %>% 
  rownames_to_column('id')

ids <- added_id %>% pull(id)

expanded_ids <- rep(ids, added_id$PONDII) %>% data.frame(id = .)

expanded_df <-  left_join(expanded_ids, added_id)


total <- expanded_df %>% summarise(p21_mean = mean(P21,na.rm = T),
                          p21_median = quantile(P21,.5, na.rm = T),
                          p47t_mean = mean(P47T, na.rm = T),
                          p47t_median = quantile(P47T,.5, na.rm = T),
                          aglom = 'Total',
                          base = 'a2019t4')
data <- data %>% rbind(total)


caba <- expanded_df %>%  filter(AGLOMERADO == 32) %>% summarise(p21_mean = mean(P21,na.rm = T),
                                   p21_median = quantile(P21,.5, na.rm = T),
                                   p47t_mean = mean(P47T, na.rm = T),
                                   p47t_median = quantile(P47T,.5, na.rm = T),
                                   aglom = 'CABA',
                                   base = 'a2019t4')

data <- data %>% rbind(caba)




added_id <- lista_eph$a2016t4 %>% 
  select(PONDII, CH04, P21, P47T, AGLOMERADO, ESTADO) %>% 
  filter(CH04 == 2,
         ESTADO == 1) %>%
  rownames_to_column('id')

ids <- added_id %>% pull(id)

expanded_ids <- rep(ids, added_id$PONDII) %>% data.frame(id = .)

expanded_df <-  left_join(expanded_ids, added_id)


total <- expanded_df %>% summarise(p21_mean = mean(P21,na.rm = T),
                                   p21_median = quantile(P21,.5, na.rm = T),
                                   p47t_mean = mean(P47T, na.rm = T),
                                   p47t_median = quantile(P47T,.5, na.rm = T),
                                   aglom = 'Total',
                                   base = 'a2016t4')
data <- data %>% rbind(total)


caba <- expanded_df %>%  filter(AGLOMERADO == 32) %>% summarise(p21_mean = mean(P21,na.rm = T),
                                                                p21_median = quantile(P21,.5, na.rm = T),
                                                                p47t_mean = mean(P47T, na.rm = T),
                                                                p47t_median = quantile(P47T,.5, na.rm = T),
                                                                aglom = 'CABA',
                                                                base = 'a2016t4')

data <- data %>% rbind(caba)


added_id <- lista_eph$a2010t4 %>% 
  select(PONDERA, CH04, P21, P47T, AGLOMERADO, ESTADO) %>% 
  filter(CH04 == 2,
         ESTADO == 1) %>%
  rownames_to_column('id')

ids <- added_id %>% pull(id)

expanded_ids <- rep(ids, added_id$PONDERA) %>% data.frame(id = .)

expanded_df <-  left_join(expanded_ids, added_id)


total <- expanded_df %>% summarise(p21_mean = mean(P21,na.rm = T),
                                   p21_median = quantile(P21,.5, na.rm = T),
                                   p47t_mean = mean(P47T, na.rm = T),
                                   p47t_median = quantile(P47T,.5, na.rm = T),
                                   aglom = 'Total',
                                   base = 'a2010t4')
data <- data %>% rbind(total)


caba <- expanded_df %>%  filter(AGLOMERADO == 32) %>% summarise(p21_mean = mean(P21,na.rm = T),
                                                                p21_median = quantile(P21,.5, na.rm = T),
                                                                p47t_mean = mean(P47T, na.rm = T),
                                                                p47t_median = quantile(P47T,.5, na.rm = T),
                                                                aglom = 'CABA',
                                                                base = 'a2010t4')

data <- data %>% rbind(caba)


# get_wincome <- function(df){
#   added_id <- lista_eph$a2019t4 %>% 
#     select(PONDII, CH04, P21, P47T, AGLOMERADO) %>% 
#     filter(CH04 == 2) %>% 
#     rownames_to_column('id')
#   
#   ids <- added_id %>% pull(id)
#   
#   expanded_ids <- rep(ids, added_id$PONDII) %>% data.frame(id = .)
#   
#   expanded_df <-  left_join(expanded_ids, added_id)
#   
#   total <- expanded_df %>% summarise(p21_mean = mean(P21,na.rm = T),
#                             p21_median = quantile(P21,.5, na.rm = T),
#                             p47t_mean = mean(P47T, na.rm = T),
#                             p47t_median = quantile(P47T,.5, na.rm = T),
#                             aglomerados = 'todos',
#                             base = 'name')
#   
#   caba <- expanded_df %>% 
#     filter(AGLOMERADO == 32) %>% 
#     summarise(p21_mean = mean(P21,na.rm = T),
#                            p21_median = quantile(P21,.5, na.rm = T),
#                            p47t_mean = mean(P47T, na.rm = T),
#                            p47t_median = quantile(P47T,.5, na.rm = T),
#                            aglomerado = 'CABA',
#                            base = 'name')  
#   
#   
#   out <- rbind(total,caba)
#   return(out)
#   
#   
#   
#   
# }



