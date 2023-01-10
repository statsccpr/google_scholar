course = read_csv(file="~/google_scholar/data/course_2017.csv")

seed = read_csv(file="~/google_scholar/data/seed_2017.csv")


pubs_2018 = read_csv(file="~/google_scholar/data/dat_fellows_pubs_2018.csv")

# course release

pubs_2018_course = pubs_2018 %>% filter(Name %in% course[[1]]) %>% 
	left_join(x=.,y=select(df_affil,Name,`Department Home at UCLA`)) %>% 
	
	filter(!is.na(journal)) %>% 
	filter(!grepl(x=journal,pattern="PAA|World Bank|Working Paper|ssrn|SSRN|Discussion Paper|Unpublished|Conference|arXiv")) %>% 
	filter(!grepl(x=number,pattern="Annual Meeting|[Cc]onference")) %>% 
	select(Name,`Department Home at UCLA`,everything())

pubs_2018_course %>% 	View()

# seed grant
pubs_2018_seed = pubs_2018 %>% 
 	left_join(x=.,y=select(df_affil,Name,`Department Home at UCLA`)) %>% 
 	
 	filter(Name %in% seed[[1]]) %>% 
	filter(!is.na(journal)) %>%
	
	filter(!grepl(x=journal,pattern="[WW]orking [pP]aper|National Bureau of Economic Research|ICPSR|SSRN|R package version|Work. Pap|Unpublished|medRxiv|PAA|World Bank|APHA's|Social Science Research Network|Workshop in Methods|ssrn|Conference")) %>% 
	
	filter(!grepl(x=number,pattern="Annual Meeting|[Cc]onference")) %>%
 	
 	select(Name,`Department Home at UCLA`,everything()) 

pubs_2018_seed %>% 	View()

# no felipe

getwd()
 
write_csv(pubs_2018_seed,path=paste0(getwd(),'\\google_scholar\\data\\output\\pubs_2018_seed.csv'))
write_csv(pubs_2018_course,path=paste0(getwd(),'\\google_scholar\\data\\output\\pubs_2018_course.csv'))


summary_seed = pubs_2018_seed %>% 
	# group_by(`Department Home at UCLA`) %>% 
	# group_by(Name) %>% 
	group_by(Name,`Department Home at UCLA`) %>% 
	
	summarize(n())

write_csv(summary_seed,path=paste0(getwd(),'\\google_scholar\\data\\output\\summary_2018_seed_name.csv'))

summary_course = pubs_2018_course %>% 
	# group_by(`Department Home at UCLA`) %>% 
	group_by(Name,`Department Home at UCLA`) %>% 
	
	summarize(n())

write_csv(summary_course,path=paste0(getwd(),'\\google_scholar\\data\\output\\summary_2018_course_name.csv'))


