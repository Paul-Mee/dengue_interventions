### This script reads the Dengue surveillance data from zip files into a single data.table for later manipulation and analysis 

# Data cleaning 
rm(list=ls())

# Main packages
require(plyr)
require(data.table)
require(readxl)
require(geobr)

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

## Quick table of  interventions per year per municipality 
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

# Join to Dengue data 
DEN.dt  <- merge(DEN.dt,muni,by.x="id_municip",by.y="code_muni6")

# Join to 2019 population data 


