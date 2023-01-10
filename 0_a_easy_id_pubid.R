options(stringsAsFactors = F)
library(tidyverse)
library(scholar)

# if(!requireNamespace('remotes')) install.packages("remotes")
# remotes::install_github('jkeirstead/scholar')

# manually dl Affiliates-* spreadsheets from airtable

# faculty fellows
# dat_affil = read_csv("/google_scholar/data/Affiliates-Fellows-20220707.csv")

# dat_affil=read_csv("/google_scholar/data/Affiliates-Fellows-20210722.csv")
# dat_affil=read_csv("/google_scholar/data/Affiliates-Fellows-20210420.csv")
# dat_affil=read_csv("/google_scholar/data/Affiliates-Faculty.csv")

# # t32 students (current and alumni)
dat_affil1 = read_csv("/google_scholar/data/Affiliates-Students_20220721.csv")
# dat_affil1=read_csv("/google_scholar/data/Affiliates-Students_20220706.csv")
dat_affil2=read_csv("/google_scholar/data/Affiliates-Alumni_20220706.csv")
dat_affil = bind_rows(dat_affil1,dat_affil2) %>% filter(grepl(x=`Trainee Type`,pattern="NICHD"))
# 
# dat_affil1 %>% filter(grepl(x=`Trainee Type`,pattern="NICHD")) %>% 
# 	filter(is.na(id_google_scholar)) %>% select(1:4)


# any affil ---------------------------------------------------------------

# below is generalizeable for any affil, even if hardcoded name is id_faculty


# dat_id_faculty=read.csv("/google_scholar/dat_ids.csv")
# names(dat_affil1)
# dat_affil1 %>% group_by(id_google_scholar) %>% tally() %>% View()

# head(dat_id_faculty)
# dat_id_faculty <-read.csv("scholar.csv",colClasses = "character",header=T)
# colnames(dat_id_faculty)[1]<-c("id")

# library(dplyr)
# dat_id_faculty = dat_id_faculty %>%
# 	rename(id=id_google_scholar) %>%
# 	filter(id!="")

dat_id_faculty = dat_affil %>% 
	select(Name,Last,First,id_google_scholar,status) %>% 
	rename(id=id_google_scholar) %>%
	filter(id!="")

# View(dat_id_faculty)

# Csdat_id_faculty%>% View()
# p=get_pub_p(id)
# dim(p)
# head(p)


# step 1, get publications of each author ---------------------------------
# dat_id_faculty_all = unlist(dat_id_faculty$id)
# dat_id_faculty_all = dat_id_faculty %>% filter(status=='Fellow') %>% select(id) %>% unlist()

dat_affil %>% filter(!is.na(id_google_scholar)) %>% distinct(id_google_scholar) %>% dim()
# length(unique(dat_id_faculty_all))


# ?get_profile
# get_profile_p <- purrr::possibly(get_profile,otherwise=NA)

# ?get_publications
# get_pub_p <- purrr::possibly(get_publications,otherwise=NA)
# get_cite_p <-purrr::possibly(get_article_cite_history,otherwise=NA)


get_pub_p_sleep = function(gid){
	Sys.sleep(3.0)
	# id=dat_id_faculty_all[[1]]
	# gid = dat_id_faculty_all[[2]]
	
	message(gid)
	
	# dat_id_pubid_one = get_pub_p(id)
	dat_id_pubid_one = scholar::get_publications(id=gid)
	# dat_id_pubid_one$id=id

	dat_id_pubid_one = dat_id_pubid_one %>% 
		dplyr:::mutate(id_google_scholar=gid) %>%
		dplyr:::select(id_google_scholar,author,year,pubid,cites,title,cid,journal,number)
	
	# View(dat_id_pubid_one)
	return(dat_id_pubid_one)
}
	

get_pub_p_sleep_possib = purrr::possibly(get_pub_p_sleep,otherwise=NA)

# any affil
dat_id_faculty_all = unlist(dat_id_faculty$id)

# fellows only
dat_id_faculty_all = dat_id_faculty %>% filter(status=='Fellow') %>% select(id) %>% unlist()



dat_id_pubid = dat_id_faculty_all %>% 
	# dat_id_faculty_all[1:3] %>% 
	map(get_pub_p_sleep_possib)

glimpse(dat_id_pubid)

# dat_id_pubid = dat_id_faculty_all %>% 
# # dat_id_faculty$id[1:2] %>%
# 	lapply(.,FUN=get_pub_p_sleep_possib)

# str(dat_id_pubid)

dat_id_pubid = bind_rows(dat_id_pubid)
dat_id_pubid %>% View()

dat_id_pubid %>% 
	select(id_google_scholar,author,year,
				 pubid,title,journal,number,
				 cites,
				 cid)

dat_id_pubid_fin = left_join(dat_id_faculty,dat_id_pubid,by=c('id'='id_google_scholar'))
# View(dat_id_pubid_fin)

dat_id_pubid_fin1 = dat_id_pubid_fin %>% 
	
	mutate(date_run_scholar=Sys.Date(),
				 news_pick="",
				 pubmed_pick="",
				 pubmed_deposit="") %>% 
	
	select(1:3,5,
				 news_pick,pubmed_pick,pubmed_deposit,
				 year,title,journal,author,cites,number,
				 date_run_scholar,id,pubid,-cid) %>% 
	arrange(desc(status),Last,desc(year))

# dat_id_pubid_fin %>% View

dat_id_pubid_fin1 %>% filter(year>=2021)

# getwd()
# write_csv(dat_id_pubid_fin1,path = 'data/output/students-t32/pubs_t32_20220721.csv')

# write_csv(dat_id_pubid_fin,path = 'data\\output\\faculty-fellows\\pubs-serial/dat_id_pubid_20220707.csv')


# dat_id_pubid_fin = read_csv('data/output/students-t32/dat_id_t32_pubid_20220706.csv')

	
# filter out previous runs ------------------------------------------------

# google scholar package returns cumulative results from time of pull
# so will always return historical results up to your current date
# need to set diff t1 from t0 to get 'new' non redundant results


# t0 previous
dat_id_pubid_fin_0 = read_csv('data\\output\\faculty-fellows\\pubs-serial/dat_id_pubid_20220110.csv')
# dat_id_pubid_fin_0 = read_csv('data\\output\\faculty-fellows\\pubs-serial/dat_id_pubid_20210723.csv')

# t1 setdiff new not in t0
dat_id_pubid_fin_1 = anti_join(x=dat_id_pubid_fin,y=dat_id_pubid_fin_0,by=c('id','pubid'))

dat_id_pubid_fin_1 = dat_id_pubid_fin_1 %>% filter(year>=2021) %>% 
	arrange(Last,desc(year)) %>% 
	select(Name,status,year,title,journal,everything())

# View(dat_id_pubid_fin_1)
# write_csv(dat_id_pubid_fin_1,path = 'data\\output\\faculty-fellows\\pubs-serial/dat_id_pubid_20220110.csv')



# gsub(Sys.Date(),pattern="-","")

