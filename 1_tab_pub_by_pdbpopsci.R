options(stringsAsFactors = F)
library(tidyverse)

# cumulative step compare past run with new run ---------------------------
# id_google_scholar,pubid
# to compare last cumulative publications from last time with new run this time


# 0a read in recently downloaded new pubs at date T
dat_t1 = read_csv(file = "data/dat_id_pubid_20210723.csv")

dat_t1$"journal-article-popsci-ynmx" <- ""
View(dat_t1)

list_df_pubs_year = dat_t1 %>% 
	select(Name,Last,First,`journal-article-popsci-ynmx`,title,journal,number,cites,year,everything()) %>% 
	arrange(desc(year),Last) %>%
	filter(year >= 2016) %>% 
	group_by(year) %>% 
	group_split()
	
list_df_pubs_year[[2]]

# save each year into seperate file
for(i in seq_along(list_df_pubs_year)){
	# i=2
	list_df_pubs_year[[i]]
	year = list_df_pubs_year[[i]][1,'year']
	write_csv(x = list_df_pubs_year[[i]],path = paste0(getwd(),'\\data\\output\\pubs-yearly\\pubs_',year,'.csv'))
}


# crowdsource tagging / sorting of pop sci ynmx

# is this pop pub? y/n/m