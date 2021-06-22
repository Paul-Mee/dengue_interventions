rm(list = ls())


require(readxl)
require(ggplot2)

setwd("/Users/eideobra/Dropbox/09_CADDE_arbo/Vector_control/")


# read in vector control data sheet by sheet adn paste together into one big data frame
VC <- rbind(read_excel("DADOS_SUCEN_LEG_translated.xlsx", sheet = "2011"),
            read_excel("DADOS_SUCEN_LEG_translated.xlsx", sheet = "2012"),
            read_excel("DADOS_SUCEN_LEG_translated.xlsx", sheet = "2013"),
            read_excel("DADOS_SUCEN_LEG_translated.xlsx", sheet = "2014"),
            read_excel("DADOS_SUCEN_LEG_translated.xlsx", sheet = "2015"),
            read_excel("DADOS_SUCEN_LEG_translated.xlsx", sheet = "2016"),
            read_excel("DADOS_SUCEN_LEG_translated.xlsx", sheet = "2017"),
            read_excel("DADOS_SUCEN_LEG_translated.xlsx", sheet = "2018"))

# read in CADDE CVE dengue data
DEN <- rbind(read.csv("/Users/eideobra/Desktop/CADDE_dengue_data/cve_2011_pesquisador-20191111.csv", sep = ";"),
             read.csv("/Users/eideobra/Desktop/CADDE_dengue_data/cve_2012_pesquisador-20191111.csv", sep = ";"),
             read.csv("/Users/eideobra/Desktop/CADDE_dengue_data/cve_2013_pesquisador-20191111.csv", sep = ";"),
             read.csv("/Users/eideobra/Desktop/CADDE_dengue_data/cve_2014_pesquisador-20191111.csv", sep = ";"),
             read.csv("/Users/eideobra/Desktop/CADDE_dengue_data/cve_2015_pesquisador-20191111.csv", sep = ";"),
             read.csv("/Users/eideobra/Desktop/CADDE_dengue_data/cve_2016_pesquisador-20191111.csv", sep = ";"),
             read.csv("/Users/eideobra/Desktop/CADDE_dengue_data/cve_2017_pesquisador-20191111.csv", sep = ";"),
             read.csv("/Users/eideobra/Desktop/CADDE_dengue_data/cve_2018_pesquisador-20191111.csv", sep = ";"))
#DEN <- read.csv("/Users/eideobra/Desktop/CADDE_dengue_data/cve_2016_processed.csv")
# aggregate by date and municiplaity
DEN$dt_notific = as.character(DEN$dt_notific)
DEN$dt_notific = substr(DEN$dt_notific,1,nchar(DEN$dt_notific)-9)
DEN <- aggregate(nu_notific ~ id_municip + dt_notific, data = DEN, FUN = length)
DEN$dt_notific = as.Date(DEN$dt_notific)

# filter to 2016 for example year?
DEN = DEN[format(as.Date(DEN$dt_notific, format="%d/%m/%Y"),"%Y") == "2016", ]
VC = VC[VC$ano == 2016, ]



# types of activity
type_tab = table(VC$atividade)
type_tab[order(type_tab, decreasing = T)]
100 * type_tab[order(type_tab, decreasing = T)] / sum(type_tab[order(type_tab, decreasing = T)])


# what products are used for different activity types?

# most popular "Bloqueio - Nebulizacao"
BN <- VC[VC$atividade == "Bloqueio - Nebulizacao", ]
table(BN$focal_prod) # used about 15% time
table(BN$peri_prod) # almost never used
table(BN$neb_prod) # Almost always used (half Malathion, half Komvektor)
table(BN$sol_prod) # mostly complete - water or Soy oil?

# 2nd most popular "Ponto Estrategico"
PE <- VC[VC$atividade == "Ponto Estrategico", ]
table(PE$focal_prod) # almost always used, mix of products
table(PE$peri_prod) # rarely (~10%) used
table(PE$neb_prod) # almost never used
table(PE$sol_prod) # almost never used

# 3rd most popular "Bloqueio - Contr Criadouro"
BCC <- VC[VC$atividade == "Bloqueio - Contr Criadouro", ]
table(BCC$focal_prod) # almost always used, mostly "Sumilarv"
table(BCC$peri_prod) # almost never used
table(BCC$neb_prod) # almost never used
table(BCC$sol_prod) # almost never used

# 4th most popular "Casa a Casa - Intensificacao"
CCI <- VC[VC$atividade == "Casa a Casa - Intensificacao", ]
table(CCI$focal_prod) # almost always used, mostly "Sumilarv"
table(CCI$peri_prod) # almost never used
table(CCI$neb_prod) # almost never used
table(CCI$sol_prod) # almost never used

# 5th most popular "Imovel Especial"
IE <- VC[VC$atividade == "Imovel Especial", ]
table(IE$focal_prod) # almost always used, mostly "Sumilarv"
table(IE$peri_prod) # almost never used
table(IE$neb_prod) # almost never used
table(IE$sol_prod) # almost never used


# add a date column
relcols = as.matrix(VC[, 1:2])
dates = apply(relcols, 1, function(x) as.character(as.Date(paste(x[1], x[2], "01", sep = "-"))))
VC$Date = dates

VC_time = table(VC$Date)
plot(VC_time, ylab = "Count")


incl_list = c("Bloqueio - Nebulizacao",
              "Ponto Estrategico",
              "Bloqueio - Contr Criadouro",
              "Casa a Casa - Intensificacao",
              "Imovel Especial")
VC_sub = VC[VC$atividade %in% incl_list, ]
VC_time_det = aggregate(ano ~ atividade + Date, data = VC_sub, FUN = length)
VC_time_det$Date = as.Date(VC_time_det$Date)


p1 <- ggplot(VC_time_det, aes(fill=atividade, y=ano, x=Date)) + 
  geom_bar(position="stack", stat="identity") +
  ylab("Count")
p1

ggsave("Vector_control_timseries.pdf", p1, height = 3, width = 6)

# by space
require(geobr)
require(stringi)
require(tmap)
require(sf)
states <- read_state(year=2019)
states = states[states$abbrev_state != "SP", ]
muni <- read_municipality(code_muni= "SP", year=2019)
muni$name_upper = toupper(muni$name_muni)
muni$name_upper = stri_trans_general(str = muni$name_upper, id = "Latin-ASCII")

# how many direct matches
table(unique(VC$municipio) %in% muni$name_upper) # nearly all
unmatched = unique(VC$municipio)[!(unique(VC$municipio) %in% muni$name_upper)] # !!!! to fix

VC_geog = VC[VC$municipio %in% muni$name_upper, ]
VC_geog$code_muni = muni$code_muni[match(VC_geog$municipio, muni$name_upper)]

# now summarise over time
total_interventions = aggregate(ano ~ code_muni, data = VC_geog, FUN = length)
muni$Intervention_events = total_interventions$ano[match(muni$code_muni, total_interventions$code_muni)]
muni$Intervention_events[is.na(muni$Intervention_events)] = 0
g1 <- tm_shape(muni) +
  tm_fill("Intervention_events", title = "Total interventions",
          n = 10) +
  tm_shape(states) +
  tm_fill(col = "darkgrey") +
  tm_layout(bg.color = "lightblue")
tmap_save(g1, file = "Vector_control_map.pdf", height = 4.5, width = 6)








u_space = table(VC$municipio)
plot(u_space[order(u_space)], ylab = "Count", xlab = "Municipality")
length(u_space)

# who responsible
table(VC$execucao)

VC_time_det



# missing data
# manual corrections for different missing data codes
VC[VC == "NULL"] = NA
VC[VC == 0] = NA

apply(VC, 2, function(x) 100 * sum(is.na(x)) / length(x))






# quick preliminary analysis
VC_analysis = VC_geog
VC_analysis$code_muni = floor(VC_analysis$code_muni / 10)
VC_analysis = VC_analysis[VC_analysis$code_muni %in% DEN$id_municip, ]
VC_analysis = VC_analysis[VC_analysis$atividade %in% c("Bloqueio - Contr Criadouro",
                                                       "Bloqueio - Nebulizacao",
                                                       "Casa a Casa - Intensificacao",
                                                       "Imovel Especial",
                                                       "Ponto Estrategico"), ]
# add dates (assume middle of the month)
months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
VC_analysis$Date = apply(VC_analysis[, c("mes", "ano")], 1, function(x) paste0("15", months[x[1]], x[2]))
VC_analysis$Date = as.Date(VC_analysis$Date, "%d%b%Y")

mun_list <- aggregate(mes ~ code_muni + atividade, data = VC_analysis, FUN = length)
mun_list = mun_list[, 1:2]
eff_tab <- data.frame(municiplaity = mun_list[, 1],
                          activity = mun_list[, 2],
                          cases_before = NA,
                          cases_after = NA)
counter = 1

# main loop
for(i in 1:nrow(mun_list)){
  # VC data
  VC_subset = VC_analysis[VC_analysis$code_muni == mun_list[i, 1], ]
  VC_subset = VC_subset[VC_subset$atividade == mun_list[i, 2], ]

  # corresponding dengue data
  DEN_subset = DEN[DEN$id_municip == mun_list[i, 1], ]
  
  # now loop through interventions one by one
  for(k in 1:nrow(VC_subset)){
    int_date = VC_subset$Date[k]
    # dengue cases before and after
    Daydiff = DEN_subset$dt_notific - int_date
    cases_before = sum(DEN_subset$nu_notific[(Daydiff < 0) & (Daydiff > -30)])
    cases_after = sum(DEN_subset$nu_notific[(Daydiff > 0) & (Daydiff < 30)])
    eff_tab[counter, 3] = cases_before
    eff_tab[counter, 4] = cases_after
    
    counter = counter + 1
  }
}

# remove NAs
eff_tab = eff_tab[!is.na(rowSums(eff_tab[, 3:4])), ]

# Odds ratios
OR_calc <- function(before, after){
  vals <- log10((after +0.1) / (before +0.1))
  vals = vals[is.finite(vals)]
  #hist(vals, n = 20)
  # return mean at standard deviation
  return(10^c(mean(vals) - sd(vals), mean(vals), mean(vals) + sd(vals)))
}

# crude odds
OR_df <- data.frame(Activity = c(na.omit(as.character(unique(eff_tab$activity))), "Overall"),
                    Odds_lower = NA,
                    Odds_mid = NA,
                    Odds_upper = NA)
# by intervention
OR_df[1, 2:4] = OR_calc(eff_tab$cases_before[eff_tab$activity == unique(eff_tab$activity)[1]], 
                        eff_tab$cases_after[eff_tab$activity == unique(eff_tab$activity)[1]])
OR_df[2, 2:4] = OR_calc(eff_tab$cases_before[eff_tab$activity == unique(eff_tab$activity)[2]], 
                        eff_tab$cases_after[eff_tab$activity == unique(eff_tab$activity)[2]])
OR_df[3, 2:4] = OR_calc(eff_tab$cases_before[eff_tab$activity == unique(eff_tab$activity)[3]], 
                        eff_tab$cases_after[eff_tab$activity == unique(eff_tab$activity)[3]])
OR_df[4, 2:4] = OR_calc(eff_tab$cases_before[eff_tab$activity == unique(eff_tab$activity)[4]], 
                        eff_tab$cases_after[eff_tab$activity == unique(eff_tab$activity)[4]])
OR_df[5, 2:4] = OR_calc(eff_tab$cases_before[eff_tab$activity == unique(eff_tab$activity)[5]], 
                        eff_tab$cases_after[eff_tab$activity == unique(eff_tab$activity)[5]])
OR_df[6, 2:4] = OR_calc(eff_tab$cases_before, eff_tab$cases_after)

credplot.gg <- function(d){
  # d is a data frame with 4 columns
  # d$x gives variable names
  # d$y gives center point
  # d$ylo gives lower limits
  # d$yhi gives upper limits
  require(ggplot2)
  p <- ggplot(d, aes(x=x, y=y, ymin=ylo, ymax=yhi))+
    geom_pointrange()+
    geom_hline(yintercept = 1, linetype=2)+
    coord_flip()+
    xlab('') +
    ylab("Odds of cases in 30 days after the intervention") +
    #scale_y_log10() +
    theme_bw()
  return(p)
}

colnames(OR_df) = c("x", "ylo", "y", "yhi")
OR_df = OR_df[OR_df$x != "Overall", ]
OR_df$x = c("Block-wide larval",
            "Block-wide adult",
            "Focal larval",
            "Peri-focal larval",
            "Focal and Peri-focal \n larval and adult")
pE <- credplot.gg(OR_df)
ggsave(filename = "Crude_effectiveness_plot.pdf", pE, width = 5, height = 4)






# basic sample size calculation
IBGE <- read.csv("Municipalities_sociodemographics_chars_UNICODE.csv")

muni$POP = IBGE$Popula__o..2000.[match(muni$code_muni, IBGE$Codigo)]

# dengue to weekly


DEN$POP = IBGE$Popula__o..2000.[match(DEN$id_municip, floor(IBGE$Codigo / 10))]
DEN$Week_of_year = strftime(DEN$dt_notific, format = "%V")
DEN$Year = strftime(DEN$dt_notific, format = "%Y")
DEN$Wk_ID = as.numeric(DEN$Year) + as.numeric(DEN$Week_of_year) / 54
DEN_sum <- aggregate(nu_notific ~ Wk_ID + id_municip, data = DEN, FUN = sum, drop = FALSE)

DEN_sum = DEN_sum[!is.na(DEN_sum$nu_notific), ]
DEN_sum$POP = IBGE$Popula__o..2000.[match(DEN_sum$id_municip, floor(IBGE$Codigo / 10))]
DEN_sum$Incid = DEN_sum$nu_notific / DEN_sum$POP
summary(DEN_sum$Incid)

# re-project onto the median municipality population size then calculate variance in case counts
Case_mean = mean(DEN_sum$Incid * median(DEN_sum$POP, na.rm = T), na.rm = T)
Case_SD = sd(DEN_sum$Incid * median(DEN_sum$POP, na.rm = T), na.rm = T)

# sample size =
a = 0.05
b = 0.8

effectiveness = 0.2
mew1 = Case_mean
mew2 = Case_mean * (1 - effectiveness)

ss1 = 2 * (((a + b)^2) * (Case_SD ^ 2)) / ((mew1 - mew2)^2)









