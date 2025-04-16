
library(tidyverse)
library(sp)
library(sf)
library(readxl)
library(tidycensus)
library(tigris)
library(pdftools)

#data preparation====
#EPA
acres <- read_excel("Data/ACRES_ME_Data_pulled_06_24_24.xlsx")
#distinct(acres, `Recipient Name`)%>%write_csv("Data/recips.csv")
recipients <- read_excel("Data/ME_Recipients.xlsx")
colnames(acres)<-tolower(str_replace_all(colnames(acres)," ","_"))
right_join(acres,recipients, by= c("recipient_name"="recipients"))%>%
  select(-recipient_name)->acres

#MDEP
#geodatabase
medep <- read_csv("Data/DEP_REMO_Sites04142025.csv")
colnames(medep)<- str_replace(tolower(colnames(medep))," ","_")
#make date into date
separate(medep,status_date, into = c("month","day","year"))%>%
  mutate(month = ifelse(as.numeric(month)<10,paste("0",month,sep = ""),month))%>%
  mutate(day = ifelse(as.numeric(day)<10,paste("0",day,sep=""),day))%>%
  unite(col="status_date","month","day","year",sep = "-")%>%
  mutate(status_date = as_date(status_date,format = "%m-%d-%Y"))->medep

#MDEP pdf


concatinate_date_status <-function(dta){
  commas<-str_count(dta$text[1],",")
  if(commas == 2){
    separate(dta,col=text, into=c("name","date_status","IC"),sep = ",")%>%
      mutate(date_status = ifelse(is.na(date_status),name,date_status))%>%
      mutate(name = ifelse(name==date_status,NA,name))->dta
    date_status = str_c(dta$date_status,collapse = " ")
    filter(dta,!is.na(name))->dta
    dta$date_status<-date_status 
    dta$address = NA
  }
  if(commas == 3){
    separate(dta,col=text, into=c("name","address","date_status","IC"),sep = ",")%>%
      mutate(date_status = ifelse(is.na(date_status),name,date_status))%>%
      mutate(name = ifelse(name==date_status,NA,name))->dta
    date_status = str_c(dta$date_status,collapse = " ")
    filter(dta,!is.na(name))->dta
    dta$date_status<-date_status
  }
  dta
}

mdep_list <- as.list(pdf_text("Data/rpt_rem_site_list.pdf"))
str_split(mdep_list[[1]],pattern = "\n",simplify = FALSE)%>%unlist()->page_1
page_1 <- tibble(text=page_1)
page_1[-c(1:3),]%>%
  filter(!str_detect(text,"Disclaimer: Despite"))%>%
  filter(!str_detect(text,"Printed:"))%>%
  filter(!str_detect(text,"programs within the"))%>%
  filter(str_detect(text,"[A-Z]"))%>%
  filter(!str_detect(text,"\\s{2,}Site"))%>%
  mutate(city = substr(text,1,1))%>%
  mutate(city = ifelse(str_detect(city,"[A-Z]"),text,NA))%>%
  mutate(city = zoo::na.locf(city))%>%
  filter(text!=city)%>%
  mutate(site = str_extract(text,"REM[0-9]+"))%>%
  mutate(text = str_remove(text,"REM[0-9]+"))%>%
  mutate(program = str_extract(text,paste(paste(" ",unique(medep$program)," ",sep=""),collapse="|")))%>%
  mutate(text = str_remove_all(text,paste(paste(" ",unique(medep$program)," ",sep=""),collapse="|")))%>%
  mutate(text=str_replace_all(str_trim(text),"- ",""))%>%
  mutate(text=str_remove_all(str_trim(text)," , "))%>%
  mutate(text=str_remove_all(str_trim(text),","))%>%
  mutate(text=str_replace_all(str_trim(text),"\\s{2,}",","))%>%
  mutate(site = zoo::na.locf(site))%>%
  group_by(city,site)%>%
  nest()%>%
  mutate(data = map(data, ~concatinate_date_status(.x)))%>%
  unnest()%>%
  mutate(date = str_extract(date_status,"[0-9]+/[0-9]+/[0-9]+ "))%>%
    mutate(status=str_remove(date_status,"[0-9]+/[0-9]+/[0-9]+ "))%>%
    select(-date_status)->page_1

map_df(mdep_list[2:229],
  ~tibble(text = unlist( str_split(.x,pattern = "\n",simplify = FALSE)))%>%
  filter(!str_detect(text,"Disclaimer: Despite"))%>%
  filter(!str_detect(text,"Printed:"))%>%
  filter(!str_detect(text,"programs within the"))%>%
  filter(str_detect(text,"[A-Z]"))%>%
  filter(!str_detect(text,"\\s{2,}Site"))%>%
  mutate(city = substr(text,1,1))%>%
  mutate(city = ifelse(str_detect(city,"[A-Z]"),text,NA))%>%
  mutate(city = zoo::na.locf(city))%>%
  filter(text!=city)%>%
  mutate(site = str_extract(text,"REM[0-9]+"))%>%
  mutate(text = str_remove(text,"REM[0-9]+"))%>%
  mutate(program = str_extract(text,paste(paste(" ",unique(medep$program)," ",sep=""),collapse="|")))%>%
  mutate(text = str_remove_all(text,paste(paste(" ",unique(medep$program)," ",sep=""),collapse="|")))%>%
  mutate(text=str_replace_all(str_trim(text),"- ",""))%>%
  mutate(text=str_remove_all(str_trim(text)," , "))%>%
  mutate(text=str_remove_all(str_trim(text),","))%>%
  mutate(text=str_replace_all(str_trim(text),"\\s{2,}",","))%>%
  mutate(site = zoo::na.locf(site))%>%
  group_by(city,site)%>%
  nest()%>%
  mutate(data = map(data, ~concatinate_date_status(.x)))%>%
  unnest()%>%
  mutate(date = str_extract(date_status,"[0-9]+/[0-9]+/[0-9]+ "))%>%
  mutate(status=str_remove(date_status,"[0-9]+/[0-9]+/[0-9]+ "))%>%
  select(-date_status))->sites

sites <- bind_rows(page_1,sites)

separate(sites,date, into = c("month","day","year"))%>%
  mutate(month = ifelse(as.numeric(month)<10,paste("0",month,sep = ""),month))%>%
  mutate(day = ifelse(as.numeric(day)<10,paste("0",day,sep=""),day))%>%
  unite(col="status_date","month","day","year",sep = "-")%>%
  mutate(status_date = as_date(status_date,format = "%m-%d-%Y"))->sites

#discrepancies between list & geodatabase

medep$dbase <- "geodbase"
sites$dbase <- "pdf_list"

group_by(medep,site_number)%>%
  count(name="n_gdb")%>%
  arrange(desc(n_gdb))->sites_gdb

group_by(sites,site)%>%
  count(name="n_pdf")%>%
  arrange(desc(n_pdf))->sites_pdf

full_join(sites_gdb,sites_pdf,by=c("site_number"="site"))->joined_sites

group_by(sites,site)%>%
  slice(1L)%>%
  full_join(joined_sites,by=c("site"="site_number"))%>%
  select(-dbase)%>%write_csv(file="Data/pdf_spreadsheet.csv")

filter(joined_sites,is.na(n_gdb))

unique(sites$status)

filter(sites, !str_detect(status,"TRANSFERRED TODIVISION OIL"))%>%
  filter(!str_detect(status,"COMPLAINT INVESTIGATED UNSUBSTANTIATED"))%>%
  filter(!str_detect(status,"COMPLAINT INVESTIGATED REVIEWING PRELIMINARY SITE INFORMATION"))

separate(medep,status_date, into = c("month","day","year"))%>%
  mutate(month = ifelse(as.numeric(month)<10,paste("0",month,sep = ""),month))%>%
  mutate(day = ifelse(as.numeric(day)<10,paste("0",day,sep=""),day))%>%
  unite(col="status_date","month","day","year",sep = "-")%>%
  mutate(status_date = as_date(status_date,format = "%m-%d-%Y"))->medep

min(medep$status_date)  
  
filter(medep, status_date=="1949-12-31")


filter(sites, program == " BROWNFIELDS ")

blocks(state="ME",year=2020)->me_bg

ggplot(me_bg)+
  geom_sf()

unique(medep$substatus) 

filter(medep,site_number=="REM00553")


#Geocode properties====
#
select(acres,property_id,property_name,latitude,longitude)%>%
  distinct()%>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))%>%
  filter(!is.na(latitude))->epa_properties
st_as_sf(epa_properties,coords = c("longitude","latitude"))->epa_properties
urban <- st_read("Data/tl_2020_us_uac20_corrected/tl_2020_us_uac20_corrected.shp")
separate(urban, col=NAME20, into= c("city","state") ,sep = ",")%>%
  filter(str_detect(state,"ME"))->me_urban
rm(urban)
gc()


st_crs(epa_properties)<-st_crs(me_urban)
st_intersection(epa_properties,me_urban)%>%
  mutate(urban = "urban")->urban_bfs

mutate(epa_properties,urban = ifelse(property_id%in%urban_bfs$property_id,"urban","rural"))->epa_properties
rm(urban_bfs,me_urban)
#Spatial Analysis====

counties(state="ME",year=2020)->me_counties
st_crs(epa_properties)<- st_crs(me_counties)

ggplot(me_counties)+
  geom_sf()+
  geom_sf(data=epa_properties,aes(color=urban),size=.5)

#Funding====
#add funding by mechanism
select(acres,property_id,grant_type,epa_funding)%>%
  replace_na(list(epa_funding=0))%>%
  filter(!is.na(property_id))%>%
  group_by(property_id,grant_type)%>%
  summarize(epa_funding = sum(epa_funding))%>%
  spread(grant_type,epa_funding,fill=NA)%>%
  full_join(epa_properties,.)->epa_properties
#add funding by geographic level 
select(acres,property_id,grant_type,epa_funding,Level)%>%
  replace_na(list(epa_funding=0))%>%
  filter(!is.na(property_id))%>%
  filter(grant_type!="TBA")%>%
  group_by(property_id,Level)%>%
  summarize(epa_funding = sum(epa_funding))%>%
  spread(Level,epa_funding,fill = NA)%>%
  full_join(epa_properties,.)->epa_properties
#add total epa funding (no TBA) 
select(acres,property_id,grant_type,epa_funding,Level)%>%
  replace_na(list(epa_funding=0))%>%
  filter(!is.na(property_id))%>%
  filter(grant_type!="TBA")%>%
  group_by(property_id)%>%
  summarize(epa_funding_no_tba = sum(epa_funding))%>%
  full_join(epa_properties,.)->epa_properties
#Number of grants
select(acres,property_id,cooperative_agreement_number,grant_type,Level)%>%
  distinct()%>%
  group_by(property_id,Level)%>%
  summarize(n_grants = n())%>%
  mutate(Level = paste(Level,"grants",sep="_"))%>%
  spread(Level,n_grants,fill=0)%>%
  mutate(n_grants=County_grants+Federal_grants+Local_grants+Regional_grants+State_grants+Tribal_grants)%>%
  full_join(epa_properties,.)->epa_properties
#Time from beginning to end
select(acres,property_id,activity_start,activity_complete,`ready_for_anticipated_use?`)%>%
  group_by(property_id)%>%
  mutate(activity_complete = as_date(activity_complete),
        activity_start = as_date(activity_complete))%>%
  mutate(activity_complete=if_else(is.na(activity_complete),activity_start,activity_complete))%>%
  summarise(first_grant_start = min(activity_start,na.rm=TRUE),last_grant_end=max(activity_complete,na.rm=TRUE))%>%
  mutate(redev_time = (as_date(last_grant_end)-as_date(first_grant_start))/365)%>%
  full_join(epa_properties,.)->epa_properties


#ready for anticipated reuse?
select(acres,property_id,`ready_for_anticipated_use?`)%>%
  distinct()%>%
  rename(redevelopment_complete = `ready_for_anticipated_use?`)%>%
  full_join(epa_properties,.)->epa_properties    

group_by(epa_properties,urban,redevelopment_complete)%>%
  filter(!is.na(urban),!is.infinite(redev_time))%>%
  mutate(redev_time = as.numeric(redev_time))%>%
  summarize(redev_time = mean(redev_time))

ggplot(epa_properties,aes(x=1+as.numeric(redev_time)))+
  geom_histogram(aes(fill=urban),alpha=.2,position="identity")+
  scale_x_log10()

group_by(epa_properties,urban)%>%
  mutate(Tribal = ifelse(is.na(Tribal),0,1))%>%
  summarize(State=sum(Tribal),n=n())%>%
  with(prop.test(State,n))

with(epa_properties,wilcox.test(log(1+redev_time)~urban))

#Contaminants====
select(acres,property_id,contaminants_found,other_contaminants_found)%>%distinct()->contaminants

group_by(contaminants,property_id)%>%
  nest()%>%
  mutate(contam = map(data, ~unnest(enframe(str_split(.x$contaminants_found,",")))))%>%
  select(property_id,contam)%>%unnest()->primary_contaminants

primary_contaminants%>%
  mutate(value=str_trim(value))%>%
  spread(value,name,fill=0)%>%
  ungroup()
  summarise(across(2:24,.fns=sum))%>%
  gather()%>%
  view()

group_by(contaminants,property_id)%>%
  nest()%>%
  mutate(contam = map(data, ~unnest(enframe(str_split(.x$other_contaminants_found,",|&|;")))))%>%
  select(property_id,contam)%>%unnest()->secondary_contaminants

filter(secondary_contaminants,!is.na(value))%>%
  mutate(value= str_trim(tolower(value)))->secondary_contaminants

sc_key <- read_excel("Data/secondary_contaminants.xlsx")
full_join(secondary_contaminants,sc_key,by=c("value"="acis...1"))->secondary_contaminants

select(secondary_contaminants,-value)%>%
  distinct()%>%
  spread(`acis...2`,name,fill=0)


sc_key$
secondary_contaminants%>%
  mutate(value=str_trim(tolower(value)))%>%
  spread(value,name,fill=0)%>%
  ungroup()%>%
  summarise(across(2:24,.fns=sum))%>%view()


filter(acres, property_id == "251204")%>%
  view()

group_by(acres,property_id,property_name)%>%
  slice(1)%>%write_csv(file="data/ME_contaminants.csv")

acres_naics <- read_csv("Data/ME_Properties_NAICS.csv")%>%
  select(1:25)

select(acres_naics,-property_name,-past_uses,-property_highlights,-city)%>%
  full_join(acres)->first_join

select(acres_naics,-property_name,-past_uses,-property_highlights,-city)%>%
  right_join(acres)%>%filter(!is.na(property_id)) 

filter(acres_naics,is.na(`NAICS 1`))->uncoded

filter(first_join,property_id %in% uncoded$property_id)%>%
  select(property_id,property_name,address,city,names_cleaned)%>%
  distinct()->uncoded

arrange(uncoded,names_cleaned)%>%write_csv("Data/unclassified_properties.csv")

acres_naics%>%
  select(-past_uses,-property_highlights,-city,-`Coded By`,-Source)%>%
  gather("Key","Code",3:20)%>%
  filter(!str_detect(Key,"Description"),!is.na(Code))%>%
  mutate(Code = as.character(Code))%>%
  count(Code)%>%view()
  
filter(acres,property_id=="10060")

filter(acres, property_id==113161)%>%view()

select(acres,property_id,names_cleaned)%>%
  distinct()%>%
  count(names_cleaned)%>%
  arrange(desc(n))%>%view()
  

filter(grant_type!="TBA")%>%
  group_by(grant_type,cooperative_agreement_number)%>%
  count()%>%
  group_by(grant_type)%>%
  summarize(mean = mean(n),sd=sd(n),min=min(n),max=max(n))



#
select(acres,property_id,property_name,property_highlights,latitude,longitude)%>%
  distinct()%>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))%>%
  filter(!is.na(latitude))->properties
write_csv(properties,file="Data/property_locations.csv")


st_as_sf(properties,coords=c("longitude","latitude"),remove = TRUE)->properties

get_acs(geography = "county", variables = "B19013_001", state="ME", year=2020,geometry=TRUE)->me_counties

st_crs(me_counties)->st_crs(properties)

ggplot(me_counties)+
  geom_sf()+
  geom_sf(data=properties)+
  theme_bw()

#BFs and VRAPs shared by MEDEP and Acres
medep <- read_csv("Data/DEP_REMO_Sites_03192025.csv")
colnames(medep)<- str_replace(tolower(colnames(medep))," ","_")

length(unique(medep$site_number))

filter(medep,program == "BROWNFIELDS")%>%view()

select(medep,site_number,program)%>%
  distinct()%>%
  mutate(presence = 1)%>%
  spread(program,presence,fill=0)

t(as.matrix(medep_matrix))%*%as.matrix(medep_matrix)

select(medep,-objectid)%>%
  group_by(sitenumber,program)%>%
  count()%>%
  group_by(sitenumber)%>%
  count()%>%
  filter(n>3)
  
  
filter(medep_matrix, BROWNFIELDS ==1, RCRA==1)


arrange(medep,sitenumber)%>%view()
group_by(medep,sitenumber)%>%count()%>%arrange(desc(n))

filter(medep,sitenumber=="REM00519")%>%view()



select(acres,property_id,address,city)%>%
  distinct()%>%
  mutate(city=tolower(city))%>%
  group_by(address,city)%>%
  count()%>%
  arrange(desc(n))%>%
  filter(n>1)->multi_properties


select(acres,property_name,property_id,address,city)%>%
  distinct()%>%
  mutate(city=tolower(city))%>%
  inner_join(multi_properties)%>%arrange(desc(n),city)%>%view()






