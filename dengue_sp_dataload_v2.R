### This script reads the Dengue surveillance data from zip files into a single data.table for later manipulation and analysis 

# Data cleaning 
rm(list=ls())
### comment
# Main packages
library(plyr)
library(data.table)
library(readxl)
library(geobr)
library(crul)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

#  Set home directory  and load data 
if(Sys.info()[['user']]=="phpupmee"){
  # Paul
  setwd("C:/CADDE_data/Dengue")
}

data_dir <- getwd()

###
### 1 Read case data 
###
###


# get all the zip files
#zipF <- list.files(path = data_dir, pattern = "*.zip", full.names = TRUE)

# unzip all your files
#out_dir = paste0(data_dir,"/csv")

#plyr::ldply(.data = zipF, .fun = unzip, exdir = out_dir)

# get the csv files
#csv_files <- list.files(path = out_dir, pattern = "*.csv" , full.names = TRUE)

# read the csv files
#cadde_dengue_sp.dt <- as.data.table(ldply(.data = csv_files, .fun = read.csv, sep=";"))

# Save as an RData file

#save(cadde_dengue_sp.dt, file = paste0(data_dir,"/R_Data/cadde_dengue_sp.dt.RData"))

# If running interactively 
load(paste0(data_dir,"/R_Data/cadde_dengue_sp.dt.RData"))


###
### Read intervention  data 
###
## Reads through all worksheets and loads to a single data frame
# read in vector control data sheet by sheet adn paste together into one big data frame
VC.dt <- as.data.table(rbind(readxl::read_excel(paste0(data_dir,"/DADOS_SUCEN_LEG_translated.xlsx"), sheet = "2011"),
                             readxl::read_excel(paste0(data_dir,"/DADOS_SUCEN_LEG_translated.xlsx"), sheet = "2012"),
                             readxl::read_excel(paste0(data_dir,"/DADOS_SUCEN_LEG_translated.xlsx"), sheet = "2013"),
                             readxl::read_excel(paste0(data_dir,"/DADOS_SUCEN_LEG_translated.xlsx"), sheet = "2014"),
                             readxl::read_excel(paste0(data_dir,"/DADOS_SUCEN_LEG_translated.xlsx"), sheet = "2015"),
                             readxl::read_excel(paste0(data_dir,"/DADOS_SUCEN_LEG_translated.xlsx"), sheet = "2016"),
                             readxl::read_excel(paste0(data_dir,"/DADOS_SUCEN_LEG_translated.xlsx"), sheet = "2017"),
                             readxl::read_excel(paste0(data_dir,"/DADOS_SUCEN_LEG_translated.xlsx"), sheet = "2018")))

save(VC.dt, file = paste0(data_dir,"/VC_dt.RData"))
# If running interactively 
#load(paste0(data_dir,"/VC_dt.RData"))
# add a date column set all dates to 1st of month 
relcols = as.matrix(VC.dt[, 1:2])
dates = apply(relcols, 1, function(x) as.character(as.Date(paste(x[1], x[2], "01", sep = "-"))))
VC.dt$Date = as.Date(dates)

### limit to a subset of activities
incl_list = c("Bloqueio - Nebulizacao",
              "Ponto Estrategico",
              "Bloqueio - Contr Criadouro",
              "Casa a Casa - Intensificacao",
              "Imovel Especial")
VC_sub.dt = VC.dt[VC.dt$atividade %in% incl_list, ]


## Quick table of  interventions per municipality 
iv.year <- as.data.frame(table(VC.dt$municipio,VC.dt$ano))
names(iv.year)[1] <- "municipality"
names(iv.year)[2] <- "year"

iv.year <- reshape(iv.year, idvar = "municipality", timevar = "year", direction = "wide")

# Quick data summary 

# Flag missing geocode data for interventions 
cadde_dengue_sp.dt$cd_flag=1

cadde_dengue_sp.dt$cd_flag[is.na(cadde_dengue_sp.dt$cd_geocodi)] <- 0

tab_1 <- table(cadde_dengue_sp.dt$nu_ano,cadde_dengue_sp.dt$cd_flag)

# Tabulation of the proportion of data that has been geocoded in each year 

prop.table(tab_1, 1)

# Plot of intervention timing per municipality to look at association between upsurges of incidence and application of control measures


## Plot of incidence 

## Load municipality level population data and merge to case data - for now do not disaggregate by age

## Aggregate data by date and municipality 

# aggregate by date and municipality
cadde_dengue_sp.dt$dt_notific = as.character(cadde_dengue_sp.dt$dt_notific)
cadde_dengue_sp.dt$dt_notific = substr(cadde_dengue_sp.dt$dt_notific,1,nchar(cadde_dengue_sp.dt$dt_notific)-9)
DEN.dt <- aggregate(nu_notific ~ id_municip + dt_notific, data = cadde_dengue_sp.dt, FUN = length)
DEN.dt$dt_notific = as.Date(DEN.dt$dt_notific)

save(DEN.dt, file = paste0(data_dir,"/DEN_dt.RData"))
# If running interactively 
#load(paste0(data_dir,"/DEN_dt.RData"))



## unique municipalities

## municipalities with Dengue incidence data
 length(unique(DEN.dt$id_municip))
# 1450 ?
## municipalities with intervention data (all in SP)
 length(unique(VC_sub.dt$municipio))
# 500
 


# Join to  population data 
# Read Population data - 2020
# Source site
# https://www.ibge.gov.br/en/statistics/social/population/18448-estimates-of-resident-population-for-municipalities-and-federation-units.html?=&t=o-que-e

IBGE <- read_xlsx("IBGE/estimativa_dou_2020_municipality.xlsx")
names(IBGE)[2] <- "state_code"
names(IBGE)[3] <- "municipality_code"
names(IBGE)[4] <- "municipality_name"
names(IBGE)[5] <- "population"
### remove parentheses and text within  from population string and convert to integer
IBGE$population <- gsub("\\s*\\([^\\)]+\\)","",as.character(IBGE$population))
IBGE$population <- as.integer(IBGE$population)

### Create 6 digit municipality code
IBGE$code_muni6 = paste0(IBGE$state_code,IBGE$municipality_code)
IBGE$code_muni6 = substr(as.character(IBGE$code_muni6),1,6)

## unique municipalities
length(unique(IBGE$code_muni6))
## 5571 - all Brazil

# Join to Dengue incidence data 
DEN_muni.dt  <- merge(DEN.dt,IBGE,by.x="id_municip",by.y="code_muni6",all.X=TRUE)
length(unique(DEN_muni.dt$id_municip))
# 1450 - all muncipalities have population data

# Calculate daily incidence
DEN_muni.dt$incidence = DEN_muni.dt$nu_notific/DEN_muni.dt$population*10000

# Sort by municipality and date
DEN_muni.dt <- DEN_muni.dt %>%
  arrange(id_municip,dt_notific)

# This make a list of all dates between the first and last date for each municipality
all_municip_dates <- tidyr::complete(DEN_muni.dt, id_municip, dt_notific)
# Keep first two columns
all_municip_dates <- all_municip_dates[c("id_municip", "dt_notific")]
length(unique(all_municip_dates$id_municip))
# 1450 


# Merge with Dengue data  and add zero counts for missing dates with no incident cases 
DEN_all.dt  <- merge(x=all_municip_dates,y=DEN_muni.dt,by=c("id_municip","dt_notific"),all.x=TRUE)
# replace NA for incidence with 0 
DEN_all.dt  <- DEN_all.dt %>% tidyr::replace_na(list(incidence = 0))
length(unique(DEN_all.dt$id_municip))
#1450

# Just get place date & incidence 
DEN_all.dt <- DEN_all.dt[c("id_municip", "dt_notific","incidence")]
# Now add names of places again to give a dataset with daily incidence for each municipality
DEN_all.dt  <- merge(DEN_all.dt,IBGE,by.x="id_municip",by.y="code_muni6")
length(unique(DEN_all.dt$id_municip))

## Getting municiplaity data in upper case without spanish characters
DEN_all.dt$name_upper = toupper(DEN_all.dt$municipality_name.y)
DEN_all.dt$name_upper_ASC <-  iconv(DEN_all.dt$name_upper,from="UTF-8",to="ASCII//TRANSLIT")

### Tidy up incidence data file

DEN_all.dt <- DEN_all.dt[,c('municipality_name.y','name_upper_ASC','id_municip','UF.y','dt_notific','incidence')]
names(DEN_all.dt)[1] <- "municipality_name"
names(DEN_all.dt)[2] <- "municipality_name_upper"
names(DEN_all.dt)[3]  <- "municipality_id"
names(DEN_all.dt)[4]  <- "state"
names(DEN_all.dt)[5]  <- "notification_date"
names(DEN_all.dt)[6]  <- "incidence"


## Save incidence data file
save(DEN_all.dt, file = paste0(data_dir,"/DEN_all_dt.RData"))
# If running interactively 
#load(paste0(data_dir,"/DEN_all_dt.RData"))
length(unique(DEN_all.dt$municipality_id))
#1450

## select incidence data for SP state
DEN_all_SP.dt <- dplyr::filter(DEN_all.dt, state == "SP")

length(unique(DEN_all_SP.dt$municipality_id))
##645

## Checking municipality names in intervention data 
inter_mun_list<- as.data.frame(unique(VC_sub.dt$municipio))
names(inter_mun_list)[1] <- "municipio"
# Data OK all upper case without spanish language characters - 500 places
incidence_mun_list <- as.data.frame(unique(DEN_all_SP.dt$municipality_name_upper))
names(incidence_mun_list)[1] <- "municipality_name_upper"
## 645 no duplicate names

check_data.dt <- merge(inter_mun_list,incidence_mun_list,by.x="municipio",by.y="municipality_name_upper",all.x=TRUE)
## all 500 municipalities with intervention data also have incidence data

## Now merge incidence data with intervention data 
DEN_VC.dt  <- merge(DEN_all_SP.dt,VC_sub.dt,by.x=c("municipality_name_upper","notification_date"),by.y=c("municipio","Date"),all.x = TRUE)

# NB We cannot assume that places with not interventions reported actually had no interventions 

# Extract year 
DEN_VC.dt$yr_notific <- lubridate::year(DEN_VC.dt$notification_date)

# Rolling mean of incidence 
# window length (days)
mean_window <- 7 
DEN_VC.dt <- DEN_VC.dt %>% 
    dplyr::mutate(mean_inc= zoo::rollmean(incidence, mean_window,align="left",fill=NA)) 

## Save DEN_VC data
save(DEN_VC.dt, file = paste0(data_dir,"/DEN_VC_dt.RData"))
# If running interactively 
#load(paste0(data_dir,"/DEN_VC_dt.RData"))


### Consider removing rows before 1st intervention anywhere to reduce file size
## Checking dates 

tmp.dt <- DEN_VC.dt %>% 
   filter(!is.na(DEN_VC.dt$ano)) 

min(tmp.dt$notification_date)
# 1st intervention Jan 1st 2011
max(tmp.dt$notification_date)
# last intervention June 1st 2018
min(DEN_VC.dt$notification_date)
# 1st incidence data Jan 1st 2007
max(DEN_VC.dt$notification_date)
# last incidence data Dec 31st 2019

## remove data before 2011 ?

##NB - Need to consider multiple interventions on the same date in same state - multiple rows for same date in analysis
                                                                                  
# Select Municipality 

#municip.dt <- unique(DEN.dt[c("id_municip","name_muni")])

   DEN_VC_2011.dt <-  DEN_VC.dt %>%
        filter(notification_date >= as.Date("2011-01-01") )



# Plot 
## Set parameters
#munic <- "SAO PAULO"

munic="SAO CAETANO DO SUL"

#munic="ADAMANTINA"

year1 <- "2011"
year2 <- "2019"


plot_title <- paste0("Dengue weekly mean incidence for  ",munic," - ",year1," to ",year2)

plot_data <- DEN_VC.dt %>%
  # select municipality
  filter(municipality_name_upper==munic) %>%
    # select year 
    filter(yr_notific>=year1 & yr_notific<=year2) 
  
plot_data <- as.data.frame(plot_data)
y_val = (max(plot_data$mean_inc,na.rm=TRUE) - min(plot_data$mean_inc,na.rm=TRUE))/2

# Plots to explore association of control activities and incidence for particular municipalities
   p1 <- ggplot() +
   geom_line(data=plot_data,aes(x=notification_date,y=mean_inc),color = "red", size = .75) +
   labs(title=plot_title,
    y="Dengue Incidence - smoothed") +
    # control activities
     if(length(which(plot_data$atividade == "Bloqueio - Nebulizacao"))>0) 
       {geom_point( data=subset(plot_data,(atividade=="Bloqueio - Nebulizacao")) 
      ,aes(x=notification_date,y=y_val,color=atividade), shape="circle", size = 2)}  
     
     if(length(which(plot_data$atividade == "Ponto Estrategico"))>0) 
     {  p1 <- p1 + geom_point( data=subset(plot_data,(atividade=="Ponto Estrategico")) 
                  ,aes(x=notification_date,y=y_val+0.1*y_val/3,color=atividade), shape="circle", size = 2)}  
   
     if(length(which(plot_data$atividade == "Bloqueio - Contr Criadouro"))>0) 
     {p1 <- p1 + geom_point( data=subset(plot_data,(atividade=="Bloqueio - Contr Criadouro")) 
                           ,aes(x=notification_date,y=y_val+0.2*y_val/3,color=atividade), shape="circle", size = 2)}   
   
     if(length(which(plot_data$atividade == "Casa a Casa - Intensificacao"))>0) 
     {  p1 <- p1 + geom_point( data=subset(plot_data,(atividade=="Casa a Casa - Intensificacao")) 
                           ,aes(x=notification_date,y=y_val+0.3*y_val/3,color=atividade), shape="circle", size = 2)}   
   
     if(length(which(plot_data$atividade == "Imovel Especial"))>0) 
     { p1 <- p1 + geom_point( data=subset(plot_data,(atividade=="Imovel Especial")) 
                           ,aes(x=notification_date,y=y_val+0.4*y_val/3,color=atividade), shape="circle", size = 2)} 
   
   
    p1

  
ggsave(p1, "plots/examp_inc_intervention.png", width=40, height=16, units="cm")




## To do 

## Generate mean incidence at a particular date for all years 
## Calculate ratio current avg_inc / avg_inc (all years)






