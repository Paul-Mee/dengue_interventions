### This script reads the Dengue surveillance data from zip files into a single data.table for later manipulation and analysis 

# Data cleaning 
rm(list=ls())

# Main packages
library("plyr")
library("data.table")
library("readxl")

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

paste0(data_dir,"/DADOS_SUCEN_LEG_translated.xlsx")

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


