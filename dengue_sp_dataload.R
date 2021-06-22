### This script reads the Dengue surveillance data from zip files into a single data.table for later manipulation and analysis 

# Data cleaning 
rm(list=ls())

# Main packages
library("plyr")
library("data.table")

#  Set home directory  and load data 
if(Sys.info()[['user']]=="phpupmee"){
  # Paul
  setwd("C:/CADDE_data/Dengue")
}

data_dir <- getwd()

# get all the zip files
zipF <- list.files(path = data_dir, pattern = "*.zip", full.names = TRUE)

# unzip all your files
out_dir = paste0(data_dir,"/csv")

ldply(.data = zipF, .fun = unzip, exdir = out_dir)

# get the csv files
csv_files <- list.files(path = out_dir, pattern = "*.csv" , full.names = TRUE)

# read the csv files
cadde_dengue_sp.dt <- as.data.table(ldply(.data = csv_files, .fun = read.csv, sep=";"))

# Save as an RDS file

# Quick data summary 

# Flag missing geocode data 
cadde_dengue_sp.dt$cd_flag=1

cadde_dengue_sp.dt$cd_flag[is.na(cadde_dengue_sp.dt$cd_geocodi)] <- 0

tab_1 <- table(cadde_dengue_sp.dt$nu_ano,cadde_dengue_sp.dt$cd_flag)

# Tabulation of the proportion of data that has been geocoded in each year 

prop.table(tab_1, 1)