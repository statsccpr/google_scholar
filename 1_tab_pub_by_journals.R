library(tidyverse)
options(stringsAsFactors = F)

# head(df_cite_pub_auth)


# air table affil ---------------------------------------------------------


df_affil = read_csv(file="/google_scholar/data/Affiliates-Faculty.csv")
# View(df_affil)
head(df_affil)

# when google scholar ids were seperate from affiliates-faculty.csv, now they are on same airtable
# df_ids = read_csv(file="/google_scholar/dat_ids.csv")
# str(df_ids)

# test = left_join(df_affil,df_ids,by=c("First","Last"))
# View(test)
# names(test)

test = df_affil %>% 
	# test %>% 
	select(Last,First,`Department Home at UCLA`,
				 id_google_scholar,
				 Rank,status)

# test %>% View()

jf_promod = c('Gipson',
							'Liu',
							'Akee', 
							'Goodwin-White',
							"Foster","Holloway","Lens","Sudhinaraset","Beltran-Sanchez")

names(test)
test = test %>% 
	mutate(a_f_jf=ifelse((grepl(status,pattern="Fellow")==TRUE),
	'f','a')) %>% 
	mutate(a_f_jf = ifelse((grepl(status,pattern="Fellow")==TRUE)&(grepl(Rank,pattern="Assis")==TRUE),
											 'jf',
											 a_f_jf)) %>% 
	# manualy treat recent assoc promos as jf for their pubs
	mutate(a_f_jf = ifelse(Last %in% jf_promod,
												 'jf',a_f_jf)) 

test %>% 
	View()

# 29 jf, 70 f, 140 a
test %>% filter(grepl(a_f_jf,pattern='jf')) %>% tally
test %>% filter(grepl(a_f_jf,pattern='f')) %>% tally
test %>% tally

# data google scholar ----------------------------------------------------------
# get 2021 version

dat_id_pubid_raw = read_csv(file="/google_scholar/data/dat_id_pubid.csv")
# View(dat_id_pubid_raw)
names(dat_id_pubid_raw)

dat_id_pubid_raw$journal <- iconv(dat_id_pubid_raw$journal, "ASCII", "UTF-8", sub="byte")
dat_id_pubid_raw$author <- iconv(dat_id_pubid_raw$author, "ASCII", "UTF-8", sub="byte")
# dat_id_pubid_raw$title <- iconv(dat_id_pubid_raw$title, "ASCII", "UTF-8", sub="byte")

test$First <- iconv(test$First, "ASCII", "UTF-8", sub="byte")
test$Last <- iconv(test$Last, "ASCII", "UTF-8", sub="byte")

dat_id_pubid_join = left_join(x=test,y=dat_id_pubid_raw,
															by=c('id_google_scholar'='id_google_scholar'))
names(dat_id_pubid_join)

dat_id_pubid_join %>% 
	# filter(Department.Home.at.UCLA=="Statistics") %>% 
	view()


# dat_id_pubid_join$journal <- iconv(dat_id_pubid_join$journal, "ASCII", "UTF-8", sub="byte")
# dat_id_pubid_join$First <- iconv(dat_id_pubid_join$First, "ASCII", "UTF-8", sub="byte")
# dat_id_pubid_join$Last <- iconv(dat_id_pubid_join$Last, "ASCII", "UTF-8", sub="byte")

# dat_id_pubid %>% filter(is.na(number)) %>% View()

# add last name check filter

# dat_id_pubid_join %>% 
# 	filter(year >= 2015) %>% 
# 	mutate(name_abbrev = paste0(substr(First,1,1)," ",Last)) %>%   
# 	# most abbrev have middle initial, which you dont have in airtable so this method useless
# 	filter(grepl(author,pattern=as.name(name_abbrev),ignore.case = T)) %>% 
# 	View


# matching names will exclude true publications
# so maybe better to just include all results (allow false positive)

# maybe include may wang in the top 5 journal spec, since have specific journals as filters
# but exclude may in the all pub counts / citation since no filter

dat_id_pubid = dat_id_pubid_join %>% 
	filter(year >= 2015) %>%
	# filter(journal != "") %>% 
	
	# filter(Last!="Wang",First!='May') %>% 
	
	filter(Last!="Wang" | (Last=="Wang" & grepl(author,pattern="MC Wang"))) %>%  # only keep MC Wang
	
	mutate(journal_strp = tolower(journal)) %>% 
	
	mutate(journal_strp = ifelse(grepl(journal_strp,
																		 pattern='royal statistical society'), # lump together series abc
															 'journal of the royal statistical society',
															 journal_strp)) %>% 
	
	filter(!grepl(journal_strp,pattern="https")) %>% 
	filter(!grepl(journal_strp,pattern="^u$")) %>% 
	filter(!grepl(journal_strp,pattern="^journal$")) %>%
	
	# wierd chs pubs
	# likely from wrong may wang google scholar
	# "M Wang"
	
	filter(!grepl(journal_strp,pattern="auris nasus larynx")) %>% 
	
	filter(!grepl(journal_strp,pattern="advanced materials research")) %>% 
	filter(!grepl(journal_strp,pattern="applied mechanics and materials")) %>% 
	filter(!grepl(journal_strp,pattern="key engineering materials")) %>% 
	filter(!grepl(journal_strp,pattern="zhong")) %>% 
	filter(!grepl(journal_strp,pattern="materials science and engineering")) %>% 
	filter(!grepl(journal_strp,pattern="chemical")) %>% 
	filter(!grepl(journal_strp,pattern="faseb")) %>% 
	filter(!grepl(journal_strp,pattern="hot working technology")) %>% 
	filter(!grepl(journal_strp,pattern="chinese fisheries economics")) %>% 
	filter(!grepl(journal_strp,pattern="computer knowledge and technology")) %>% 
	filter(!grepl(journal_strp,pattern="computer engineering and design")) %>% 
	filter(!grepl(journal_strp,pattern="physical review d")) %>% 
	
	filter(!grepl(journal_strp,pattern="us patent")) %>% 
	filter(!grepl(journal_strp,pattern="university")) %>% 
	filter(!grepl(journal_strp,pattern="dissertation")) %>% 
	# filter(!grepl(journal_strp,pattern="arxiv|biorxiv")) %>% 
	filter(!grepl(journal_strp,pattern="book")) %>% 
	filter(!grepl(journal_strp,pattern="unpublished")) %>% 
	filter(!grepl(journal_strp,pattern="working paper")) %>% 
	filter(!grepl(journal_strp,pattern="taylor|francis")) %>% 
	filter(!grepl(journal_strp,pattern="url")) %>% 
	filter(!grepl(journal_strp,pattern="university of california"))
	
# filter lgl
# pub_cat = dat_id_pubid %>% 
# 	# group_by(`Department Home at UCLA`,journal_strp) %>%
# 	group_by(journal_strp) %>%
# 	
# 	summarise(cnt = n()) %>%
# 	
# 	top_n(cnt,n=40) %>% 
# 	select(journal_strp,cnt)
# 
# pub_cat %>% 
# 	View()
# 
# getwd()
# write_csv(pub_cat,path='pub_cat_nodept_alltime.csv')

dat_id_pubid %>% View()

# dedup pubid names, if 2 affiliates collab on 1 pub, the pub shows up 2 times

dim(dat_id_pubid)
dat_id_pubid %>% distinct(title) %>% dim()  # lose out on  redundant, a lot of collab

dat_id_pubid %>% distinct(pubid) %>% dim() # better # right thing to do

# dat_id_pubid %>% distinct(pubid,.keep_all = TRUE) %>% View() 
# 



# TOP 5 journal cats ------------------------------------------------------------

df_journal_cat_use = read_csv(file="/google_scholar/data/input/tab_key_top_pubs.csv")
# df_journal_cat_use
View(df_journal_cat_use)

# names(dat_id_pubid_uniq)
# dat_id_pubid_uniq$journal_strp

library(fuzzyjoin)
# install.packages('fuzzyjoin')
# ?fuzzy_left_join

# dim(dat_id_pubid_uniq)

dat_pub_top5 = stringdist_left_join(x=filter(dat_id_pubid,
																						 #dat_id_pubid_uniq,
																						 !is.na(journal_strp)),
																		y=df_journal_cat_use,
											 # match_fun=grepl,
											 distance_col='dist',
								by=c("journal_strp"='journal'))
View(dat_pub_top5)


# is pubid a secondary key? need scholarid,pubid ?
# yes, pubid is reused, is only unique within a scholarid
# can not distinct only on pubid, -3k pubs


# dedup
# ignore dedup, allow count seperate co authors
# 
# dat_pub_top5_dedup = dat_pub_top5 %>%
# 	filter(!is.na(journal.y)) %>% 
# 	
# 	# dedup mult pub titles becuz of collabs
# 	
# 	# distinct(title_strp,.keep_all = TRUE) %>% 
# 	mutate(title_strp = tolower(gsub(title,pattern="([^A-Za-z ])+",replacement= ""))) %>% 
# 	distinct(title_strp,.keep_all = TRUE)

# no dedup

dat_pub_top5_nodedup = dat_pub_top5 %>%
	filter(!is.na(journal.y)) %>% 
	
	# dedup mult pub titles becuz of collabs
	
	# distinct(title_strp,.keep_all = TRUE) %>% 
	mutate(title_strp = tolower(gsub(title,pattern="([^A-Za-z ])+",replacement= ""))) 

# dim(dat_pub_top5_dedup)
# dat_pub_top5_use = dat_pub_top5_dedup

dim(dat_pub_top5_nodedup)
dat_pub_top5_use = dat_pub_top5_nodedup

dim(dat_pub_top5_use)

dat_pub_top5_use %>% View()
# dat_pub_top5 %>% filter(dist<=1) %>% group_by(category) %>% arrange(category,journal.y) %>% View()
# 
# # out %>% filter(dist<=1) %>% filter(year>=2010) %>% group_by(category) %>% tally() %>% View()
# dat_pub_top5 %>% filter(dist<=1) %>% filter(year>=2015) %>% group_by(category) %>% tally() %>% View()
# 
# dat_pub_top5 %>% filter(dist<=1) %>% filter(year>=2015) %>% group_by(category) %>% View()
# # maybe drop jss post 2015, since only 2 in table. but mention in writeup s&t section
# 
# dat_pub_top5 %>% filter(dist<=1) %>% filter(year>=2015) %>% tally()



message('affil')
dat_pub_top5_use %>% 
	filter(dist<=1) %>% filter(year>=2015) %>% 
	# filter(!grepl(journal_strp,pattern='statistics in medicine')) %>% 
	# filter(!grepl(journal_strp,pattern='statistical soft')) %>% 
	group_by(category) %>% tally() %>% View()

# including stat soft only increases all affl pub by 1, post 2015
# drop staft soft in top 5 stat tally

# all fellows
message('fellows')
dat_pub_top5_use %>% filter(grepl(a_f_jf,pattern='f'))%>% 
	filter(dist<=1) %>% filter(year>=2015) %>% 
	group_by(category) %>% tally() %>% View()


# jr fellows
message('jf')
dat_pub_top5_use %>% filter(grepl(a_f_jf,pattern='jf')) %>% 
	filter(dist<=1) %>% filter(year>=2015) %>% 
	group_by(category) %>% tally() %>% View()

# lump citations for category in parenthesis ()

# ALL pubs ----------------------------------------------------------------
# other journals, books, preprints, working paper, reviews


dim(dat_id_pubid)



dat_id_pubid = dat_id_pubid %>%
	# distinct(pubid,.keep_all = TRUE) %>%
	# is pubid a secondary key? need scholarid,pubid ?
	# yes, pubid is reused, is only unique within a scholarid
	# can not distinct only on pubid, -3k pubs
	
	mutate(title_strp = tolower(gsub(title,pattern="([^A-Za-z ])+",replacement= "")))

dim(dat_id_pubid)

# ALL Pubs
# dedup all pubs count
dim(dat_id_pubid)

# jf
dat_id_pubid_uniq %>% 
	filter(grepl(a_f_jf,pattern='jf')) %>% 
	# dedup amongst collabs for ALL pub count (when ignoring top 5 journal)
	# group_by(a_f_jf) %>% 
	distinct(title_strp,.keep_all = TRUE) %>% 
	filter(year>=2015) %>% tally()

# jf + f
dat_id_pubid_uniq %>% 
	filter(grepl(a_f_jf,pattern='f')) %>% 
	# dedup amongst collabs for ALL pub count (when ignoring top 5 journal)
	# group_by(a_f_jf) %>% 
	distinct(title_strp,.keep_all = TRUE) %>% 
	filter(year>=2015) %>% tally()

# all affils
dat_id_pubid_uniq %>% 
	# filter(grepl(a_f_jf,pattern='f')) %>% # jf + f
	# dedup amongst collabs for ALL pub count (when ignoring top 5 journal)
	# group_by(a_f_jf) %>% 
	distinct(title_strp,.keep_all = TRUE) %>% 
	filter(year>=2015) %>% tally()


# extra dedup small effect

xx = dat_id_pubid_uniq %>% 
	filter(grepl(a_f_jf,pattern='a')) %>% 
	distinct(title_strp,.keep_all = TRUE)

names(xx)
library(fuzzyjoin)

stringdist_full_join(x=xx,y=xx,distance_col='p',
										 by='title_strp') %>% 
	filter(p>0) %>% View()



# ALL CITES  ---------------------------------------------
# all pubs no dedup


# cite of v1 + cites of v2 = total cites


# all affils
dat_id_pubid %>% filter(year>=2015) %>% summarize(sum(cites))

# fellows (jf + f)
dat_id_pubid %>% filter(grepl(a_f_jf,pattern='f')) %>% # View 
	filter(year>=2015) %>% summarize(sum(cites))

# jr fellows
dat_id_pubid %>% filter(grepl(a_f_jf,pattern='jf')) %>% 
	filter(year>=2015) %>% summarize(sum(cites))




# ucla departments in journals --------------------------------------------


dat_pub_top5 %>% 
	filter(dist<=1) %>% filter(year>=2015) %>% 
	group_by(journal.y) %>% distinct(`Department Home at UCLA`) %>% View()


dat_pub_top5 %>% 
	filter(dist<=1) %>% filter(year>=2015) %>% 
	group_by(journal.y) %>% distinct(`Department Home at UCLA`) %>% 
	# pivot_wider(names_from ='journal.y') %>%
	View()