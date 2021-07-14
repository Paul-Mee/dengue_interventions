### This script reads the Dengue surveillance data from zip files into a single data.table for later manipulation and analysis 

# Data cleaning 
rm(list=ls())

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


# get all the zip files
zipF <- list.files(path = data_dir, pattern = "*.zip", full.names = TRUE)

# unzip all your files
out_dir = paste0(data_dir,"/csv")

plyr::ldply(.data = zipF, .fun = unzip, exdir = out_dir)

# get the csv files
csv_files <- list.files(path = out_dir, pattern = "*.csv" , full.names = TRUE)

# read the csv files
cadde_dengue_sp.dt <- as.data.table(ldply(.data = csv_files, .fun = read.csv, sep=";"))

# Save as an RData file

save(cadde_dengue_sp.dt, file = paste0(data_dir,"/R_Data/cadde_dengue_sp.dt.RData"))

# If running interactively 
#load(paste0(data_dir,"/R_Data/cadde_dengue_sp.dt.RData"))


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
#load(paste0(data_dir,"/R_Data/VC_dt.RData"))
# add a date column
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

# Flag missing geocode data 
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


# Match to municipality names 
muni <- geobr::read_municipality(code_muni= "SP", year=2019)
muni$name_upper = toupper(muni$name_muni)
muni$name_upper = stri_trans_general(str = muni$name_upper, id = "Latin-ASCII")
# 6 digit municipality code
muni$code_muni6 = substr(as.character(muni$code_muni),1,6)


muni$name_upper_ASC <-  iconv(muni$name_upper,from="UTF-8",to="ASCII//TRANSLIT")


# Join to Dengue data 
DEN.dt  <- merge(DEN.dt,muni,by.x="id_municip",by.y="code_muni6")

# Join to  population data 
# Read Population data - 2020
# Source site
# https://www.ibge.gov.br/en/statistics/social/population/18448-estimates-of-resident-population-for-municipalities-and-federation-units.html?=&t=o-que-e

IBGE <- read_xlsx("IBGE/estimativa_dou_2020_municipality.xlsx")
names(IBGE)[2] <- "state_code"
names(IBGE)[3] <- "municipality_code"
names(IBGE)[4] <- "municipality_name"
names(IBGE)[5] <- "population"
IBGE$code_muni6 = paste0(IBGE$state_code,IBGE$municipality_code)
IBGE$code_muni6 = substr(as.character(IBGE$code_muni6),1,6)
# Join to Dengue data 
DEN.dt  <- merge(DEN.dt,IBGE,by.x="id_municip",by.y="code_muni6",all.X=TRUE)
DEN.dt$population <- as.integer(DEN.dt$population)
DEN.dt$incidence = DEN.dt$nu_notific/DEN.dt$population*10000





# Sort by date
DEN.dt <- DEN.dt %>%
  arrange(id_municip,dt_notific)

# add zero counts for missing dates
all_municip_dates <- tidyr::complete(DEN.dt, id_municip, dt_notific)
# Keep first two columns
all_municip_dates <- all_municip_dates[c("id_municip", "dt_notific")]
# Merge with Dengue data 
DEN_all.dt  <- merge(x=all_municip_dates,y=DEN.dt,by=c("id_municip","dt_notific"),all.x=TRUE)
# replace NA for incidence with 0 
DEN_all.dt  <- DEN_all.dt %>% tidyr::replace_na(list(incidence = 0))
# Just get place date & incidence 
DEN_all.dt <- DEN_all.dt[c("id_municip", "dt_notific","incidence")]
# Now add names of places again
DEN_all.dt  <- merge(DEN_all.dt,muni,by.x="id_municip",by.y="code_muni6")

# Merge VC subset data 
DEN_VC.dt  <- merge(DEN_all.dt,VC_sub.dt,by.x=c("name_upper_ASC","dt_notific"),by.y=c("municipio","Date"),all.x = TRUE)
# Extract year 
DEN_VC.dt$yr_notific <- lubridate::year(DEN_VC.dt$dt_notific)


## Save DEN_VC data
save(DEN_VC.dt, file = paste0(data_dir,"/DEN_VC_dt.RData"))
# If running interactively 
#load(paste0(data_dir,"/DEN_VC_dt.RData"))
                                                                                    
# Select Municipality 

#municip.dt <- unique(DEN.dt[c("id_municip","name_muni")])





# Plot 
## Set parameters
munic="355030" #SAO PAULO
#munic="350280" #ARCATUBA
# munic="354880" # SAO CAETANO
year1="2015"
year2="2016"
mean_window=7
plot_title = paste0("Dengue incidence for municipality ID = ",munic," - ",year1," to ",year2, " - Mean window = ",mean_window, "days")

plot_data <- DEN_VC.dt %>%
  # select municipality
  filter(id_municip==munic) %>%
    # select year 
    filter(yr_notific>=year1 & yr_notific<=year2) %>%
      # add zero counts for missing dates
      tidyr::complete(dt_notific = seq.Date(min(dt_notific), max(dt_notific), by="day")) %>%
        # replace NA for incidence with 0 
          tidyr::replace_na(list(incidence = 0)) %>%
          # Rolling mean
          dplyr::group_by(id_municip) %>%
              dplyr::mutate(avg_inc= zoo::rollmean(incidence, mean_window,align="left",fill=NA)) 
plot_data <- as.data.frame(plot_data)
            
y_val = (max(plot_data$avg_inc,na.rm=TRUE) - min(plot_data$avg_inc,na.rm=TRUE))/2

# Plot
   ggplot() +
   geom_line(data=plot_data,aes(x=dt_notific,y=avg_inc),color = "red", size = .75) +
   labs(title=plot_title,
    y="Dengue Incidence - smoothed") +
    # control activities
    geom_point( data=subset(plot_data,!is.na(atividade)) 
                ,aes(x=dt_notific,y=y_val,color=atividade), shape="circle", size = 3)     
  
ggsave("plots/examp_inc_intervention.png", width=40, height=16, units="cm")


### Histogram of intervention vs incidence 


#unique(DEN_VC.dt[c("atividade")])

hist_data <- DEN_VC.dt %>%
  tidyr::complete(dt_notific = seq.Date(min(dt_notific), max(dt_notific), by="day")) %>%
  # replace NA for incidence with 0 
  tidyr::replace_na(list(incidence = 0)) %>%
  # Rolling mean
  dplyr::group_by(id_municip) %>%
  dplyr::mutate(avg_inc= zoo::rollmean(incidence, mean_window,align="left",fill=NA)) 
  # Filter out very high values
  #filter(avg_inc > 0.02 & avg_inc < 0.5 ) 
   
  hist_data  <- as.data.frame(hist_data )

# ggplot()+
#   geom_histogram(data=subset(hist_data,!is.na(atividade))
#             ,aes(x = avg_inc,color = atividade), fill = "white"
#              ,position = "identity", bins = 30) 

ggplot(data=subset(hist_data,!is.na(atividade)), aes(x = avg_inc)) +
  geom_histogram(fill = "white", colour = "black", bins = 50) +
  facet_grid(atividade ~ ., scales = "free")

ggsave("plots/hist_intervention_all.png", width=20, height=60, units="cm")


## Generate mean incidence at a particular date for all years 
## Calculate ratio current avg_inc / avg_inc (all years)


DEN_summ <- hist_data[c("name_upper_ASC","dt_notific","avg_inc")]



