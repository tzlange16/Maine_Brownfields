
library(tidyverse)
library(sp)
library(sf)
library(readxl)
library(tidycensus)
library(tigris)

acres <- read_excel("Data/ACRES_ME_Data_pulled_06_24_24.xlsx")


distinct(acres, `Recipient Name`)%>%write_csv("Data/recips.csv")
unique(acres$`Recipient Name`)

recipients <- read_excel("Data/ME_Recipients.xlsx")
colnames(acres)<-tolower(str_replace_all(colnames(acres)," ","_"))

right_join(acres,recipients, by= c("recipient_name"="recipients"))%>%
  select(-recipient_name)->acres


#number of grant types, & average number of properties per grant

select(acres,property_id,contaminants_found,other_contaminants_found)%>%distinct()->contaminants

group_by(contaminants,property_id)%>%
  nest()%>%
  mutate(contam = map(data, ~unnest(enframe(str_split(.x$contaminants_found,",")))))%>%
  select(property_id,contam)%>%unnest()->primary_contaminants

primary_contaminants%>%
  mutate(value=str_trim(value))%>%
  spread(value,name,fill=0)%>%
  ungroup()%>%
  summarise(across(2:24,.fns=sum))

group_by(contaminants,property_id)%>%
  nest()%>%
  mutate(contam = map(data, ~unnest(enframe(str_split(.x$other_contaminants_found,", | &")))))%>%
  select(property_id,contam)%>%unnest()->secondary_contaminants

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

medep <- read_csv("Data/MaineDEP_Remediation_Sites (1).csv")
colnames(medep)<- tolower(colnames(medep))

length(unique(medep$sitenumber ))

filter(medep,sitenumber == "REM00511")%>%view()

select(medep,sitenumber,program)%>%
  distinct()%>%
  mutate(presence = 1)%>%
  spread(program,presence,fill=0)%>%
  filter( BROWNFIELDS ==1, RCRA==1)

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



