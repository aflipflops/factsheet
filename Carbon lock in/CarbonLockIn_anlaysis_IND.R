library(quitte)
library(tidyr)
library(ggplot2)
library(readr)
library(dplyr)
library(reshape)
library(data.table)


#read in data
# all <- invisible(fread("cdlinks_compare_20180508-032045.csv",header=TRUE)) #seems to be missing the V3 variants
# all <- invisible(fread("cdlinks_compare_20180810-010937.csv",header=TRUE)) #only MESSAGE
# all <- invisible(fread("cdlinks_compare_20180720-122848.csv",header=TRUE)) #only MESSAGE, AIM/CGE, IMAGE3.0, POLES, WITCH
all <- invisible(fread("cdlinks_compare_20180615-153808.csv",header=TRUE)) #all V3 and V4 scens

#filter for desired set of time series
models <- unique(all$MODEL)
regions <- "IND" # unique(all$REGION)
scens <- unique(all$SCENARIO)
variables <- as.character(read.csv2("plotstyles_CLI.csv")$name)


all  <- all[all$MODEL %in% models & all$SCENARIO %in% scens & all$REGION %in% regions & all$VARIABLE %in% variables,]


all <- invisible(melt(all,measure.vars=names(all)[grep("[0-9]+",names(all))],variable.name = "period",variable.factor=FALSE))

all$period <- as.numeric(all$period)
all <- all[!period %in% c(1950,1955,1960,1965,1970,1975,1980,1985,1990,1995,2000,2001,2002,2003,2004,2006,2007,2008,2009,2011,2012,2013,2014,2016,2017,2018,2019,2021,2022,2023,2024,2026,2027,2028,2029,2031,2032,2033,2034,2036,2037,2038,2039,2041,2042,2043,2044,2046,2047,2048,2049,2051,2052,2053,2054,2056,2057,2058,2059,2061,2062,2063,2064,2066,2067,2068,2069,2071,2072,2073,2074,2076,2077,2078,2079,2081,2082,2083,2084,2086,2087,2088,2089,2091,2092,2093,2094,2096,2097,2098,2099,2101,2102,2103,2104,2106,2107,2108,2109)]
# Rename columns
setnames(all, "MODEL", "model")
setnames(all, "SCENARIO", "scenario")
setnames(all, "REGION", "region")
setnames(all, "VARIABLE", "variable")
setnames(all, "UNIT", "unit")

all  <- na.omit(all) # remove rows containing NAs

#make variables a factor, so that the order in facets can be manipulated easily
all$variable <- factor(all$variable)

all$model1 <- all$model
all$scenario1 <- all$scenario
all <- unite(all,col="modscen",c("model1","scenario1"),sep = " - ")
#make variables a factor, so that the order in facets can be manipulated easily
all$model <- factor(all$model)
all$scenario <- factor(all$scenario)
all$modscen <- factor(all$modscen)

#get rid of scenarios not interesting for us
scens_other <- c(grep("rec",unique(all$scenario),value=T),
                  grep("USD",unique(all$scenario),value=T),
              grep("Food",unique(all$scenario),value=T))
all <- all[!scenario %in% scens_other]

#get rid of V3 variants for models that have V4
v3scens <- grep("_V3",unique(all$scenario),value=T)
v4scens <- grep("_V4",unique(all$scenario),value=T)
v4mods <- unique(all[scenario %in% v4scens]$model)
all <- rbind(all[!model %in% v4mods],all[model %in% v4mods & scenario %in% v4scens])

save(all,file="data_IND.RData")
load("data_IND.RData")



#plot emissions lines
plotvar <- "Emissions|CO2"
plot1 <- all[scenario %in% c("NPi2020_1000_V4","NPi2020_1000_V3") & variable==plotvar]
plot1a <- all[scenario %in% c("INDC2030i_1000_V4","INDC2030i_1000_V3") & variable==plotvar]
plot2 <- all[scenario %in% c("NPi2020_low_V3","INDC2030_low_V3") & variable==plotvar]    

ggplot()+
  geom_line(data=plot1,aes(x=period,y=value/1000,group=model,linetype=model),color="black")+
  geom_line(data=plot1a,aes(x=period,y=value/1000,group=model,linetype=model),color="grey")+
  geom_line(data=plot2,aes(x=period,y=value/1000,group=modscen,color=modscen))+
  theme_bw()+ylab("CO2 Emissions (Gt/yr)")+xlab("year")+xlim(2000,2050)
ggsave(file="CO2emi_IND.png",width=8,height=4)

#plot emissions lines
plotvar <- "Emissions|CO2|Energy|Supply|Electricity"
plot1 <- all[scenario %in% c("NPi2020_1000_V4","NPi2020_1000_V3") & variable==plotvar]
plot1a <- all[scenario %in% c("INDC2030i_1000_V4","INDC2030i_1000_V3") & variable==plotvar]
plot2 <- all[scenario %in% c("NPi2020_low_V3","INDC2030_low_V3") & variable==plotvar]    

ggplot()+
  geom_line(data=plot1,aes(x=period,y=value/1000,group=model,linetype=model),color="black")+
  geom_line(data=plot1a,aes(x=period,y=value/1000,group=model,linetype=model),color="grey")+
  geom_line(data=plot2,aes(x=period,y=value/1000,group=modscen,color=modscen))+
  theme_bw()+ylab("CO2 Emissions Electricity (Gt/yr)")+xlab("year")+xlim(2000,2050)+ylim(-0.5,2.5)
ggsave(file="CO2emi_elec_IND.png",width=8,height=4)

#plot final energy lines
plotvar <- "Final Energy|Electricity"
plot1 <- all[scenario %in% c("NPi2020_1000_V4","NPi2020_1000_V3") & variable==plotvar]
plot1a <- all[scenario %in% c("INDC2030i_1000_V4","INDC2030i_1000_V3") & variable==plotvar]
plot2 <- all[scenario %in% c("NPi2020_low_V3","INDC2030_low_V3") & variable==plotvar]    

ggplot()+
  geom_line(data=plot1,aes(x=period,y=value/1,group=model,linetype=model),color="black")+
  geom_line(data=plot1a,aes(x=period,y=value/1,group=model,linetype=model),color="grey")+
  geom_line(data=plot2,aes(x=period,y=value/1,group=modscen,color=modscen))+
  theme_bw()+ylab("Final Energy Electricity (EJ/yr)")+xlab("year")+xlim(2000,2050)+ylim(0,30)
ggsave(file="FEelec_IND.png",width=8,height=4)

#plot capital cost
plotvar <- "Capital Cost|Electricity|Solar|PV"
plot1 <- all[scenario %in% c("NPi2020_1000_V4","NPi2020_1000_V3") & variable==plotvar]
plot1a <- all[scenario %in% c("INDC2030i_1000_V4","INDC2030i_1000_V3") & variable==plotvar]
plot2 <- all[scenario %in% c("NPi2020_low_V3","INDC2030_low_V3") & variable==plotvar]    

localrate = 0.015 #https://www.xe.com/currencycharts/?from=INR&to=USD&view=10Y
ggplot()+
  geom_line(data=plot1,aes(x=period,y=value/1,group=model,linetype=model),color="black")+
  geom_line(data=plot1a,aes(x=period,y=value/1,group=model,linetype=model),color="grey")+
  geom_line(data=plot2,aes(x=period,y=value*localrate,group=modscen,color=modscen))+
  theme_bw()+ylab("Tech Cost PV ($/W)")+xlab("year")+xlim(2000,2050)
ggsave(file="CapCostPV_IND.png",width=8,height=4)

#plot Coal power
plotvar <- "Secondary Energy|Electricity|Coal|w/o CCS"
plot1 <- all[scenario %in% c("NPi2020_1000_V4","NPi2020_1000_V3") & variable==plotvar]
plot1a <- all[scenario %in% c("INDC2030i_1000_V4","INDC2030i_1000_V3") & variable==plotvar]
plot2 <- all[scenario %in% c("NPi2020_low_V3","INDC2030_low_V3") & variable==plotvar]    

ggplot()+
  geom_line(data=plot1,aes(x=period,y=value/1,group=model,linetype=model),color="black")+
  geom_line(data=plot1a,aes(x=period,y=value/1,group=model,linetype=model),color="grey")+
  geom_line(data=plot2,aes(x=period,y=value/1,group=modscen,color=modscen))+
  theme_bw()+ylab("Unabated Coal Power (EJ/yr)")+xlab("year")+xlim(2000,2050)
ggsave(file="UnabatedCoal_IND.png",width=8,height=4)

#plot Coal power capacity
plotvar <- "Capacity|Electricity|Coal|w/o CCS"
plot1 <- all[scenario %in% c("NPi2020_1000_V4","NPi2020_1000_V3") & variable==plotvar]
plot1a <- all[scenario %in% c("INDC2030i_1000_V4","INDC2030i_1000_V3") & variable==plotvar]
plot2 <- all[scenario %in% c("NPi2020_low_V3","INDC2030_low_V3") & variable==plotvar]    

ggplot()+
  geom_line(data=plot1,aes(x=period,y=value/1,group=model,linetype=model),color="black")+
  geom_line(data=plot1a,aes(x=period,y=value/1,group=model,linetype=model),color="grey")+
  geom_line(data=plot2,aes(x=period,y=value/1,group=modscen,color=modscen))+
  theme_bw()+ylab("Unabated Coal Power (GW)")+xlab("year")+xlim(2000,2050)
ggsave(file="UnabatedCoalGW_IND.png",width=8,height=4)


#Power generation in 2030
plotvar <- c("Secondary Energy|Electricity|Biomass|w/ CCS",
             "Secondary Energy|Electricity|Biomass|w/o CCS",
             "Secondary Energy|Electricity|Coal|w/ CCS",
             "Secondary Energy|Electricity|Coal|w/o CCS",
             "Secondary Energy|Electricity|Gas|w/ CCS",
             "Secondary Energy|Electricity|Gas|w/o CCS",
             "Secondary Energy|Electricity|Geothermal",
             "Secondary Energy|Electricity|Hydro",
             "Secondary Energy|Electricity|Nuclear",
             "Secondary Energy|Electricity|Ocean",
             "Secondary Energy|Electricity|Oil",
             "Secondary Energy|Electricity|Other",
             "Secondary Energy|Electricity|Solar",
             "Secondary Energy|Electricity|Wind")

plot1 <- all[scenario %in% c("NPi2020_1000_V4","NPi2020_1000_V3", "INDC2030i_1000_V4","INDC2030i_1000_V3",
                             "NPi2020_low_V3","INDC2030_low_V3") & variable %in% plotvar & period==2030]

plot2 <- all[scenario %in% c("NPi2020_1000_V4","NPi2020_1000_V3", "INDC2030i_1000_V4","INDC2030i_1000_V3",
                             "NPi2020_low_V3","INDC2030_low_V3") & period==2030 & 
                              variable %in% "Secondary Energy|Electricity" ]

source("plotstyle.R")

ggplot()+
  geom_bar(data=plot1,aes(x=modscen,y=value/1,group=interaction(modscen,variable),fill=variable),stat="identity")+
scale_fill_manual(values=plotstyle(plotvar))  + xlab("Model - Scenario")+
  geom_text(data=plot1,aes(x=modscen,y=25,label=modscen,angle=90,hjust="right"),color="red",size=2.5)+
  geom_point(data=plot2,aes(x=modscen,y=value))+ylim(0,25)+
  theme_bw()+ylab("Secondary Energy Electricity (EJ/yr)")
ggsave(file="SEelec_IND.png",width=8,height=5)

#Power capacity in 2030
plotvar <- c("Capacity|Electricity|Biomass|w/ CCS",
             "Capacity|Electricity|Biomass|w/o CCS",
             "Capacity|Electricity|Coal|w/ CCS",
             "Capacity|Electricity|Coal|w/o CCS",
             "Capacity|Electricity|Gas|w/ CCS",
             "Capacity|Electricity|Gas|w/o CCS",
             "Capacity|Electricity|Geothermal",
             "Capacity|Electricity|Hydro",
             "Capacity|Electricity|Nuclear",
             "Capacity|Electricity|Ocean",
             "Capacity|Electricity|Oil",
             "Capacity|Electricity|Other",
             "Capacity|Electricity|Solar",
             "Capacity|Electricity|Wind")

plot1 <- all[scenario %in% c("NPi2020_1000_V4","NPi2020_1000_V3", "INDC2030i_1000_V4","INDC2030i_1000_V3",
                             "NPi2020_low_V3","INDC2030_low_V3") & variable %in% plotvar & period==2030]

plot1a <- all[scenario %in% c("NPi2020_1000_V4","NPi2020_1000_V3", "INDC2030i_1000_V4","INDC2030i_1000_V3",
                             "NPi2020_low_V3","INDC2030_low_V3") & period==2030 & 
               variable %in% "Capacity|Electricity|Solar|PV" &
                model %in% c("AIM V2.1","AIM/CGE","DNE21+ V.14", "REMIND-MAgPIE 1.7-3.0")]
plot1a$variable <- "Capacity|Electricity|Solar"

plot1b <- all[scenario %in% c("NPi2020_1000_V4","NPi2020_1000_V3", "INDC2030i_1000_V4","INDC2030i_1000_V3",
                              "NPi2020_low_V3","INDC2030_low_V3") & period==2030 & 
                variable %in% "Capacity|Electricity|Wind|Onshore" &
                model %in% c("DNE21+ V.14")]
plot1b$variable <- "Capacity|Electricity|Wind"

plot1 <- rbind(plot1,plot1a,plot1b)

plot2 <- all[scenario %in% c("NPi2020_1000_V4","NPi2020_1000_V3", "INDC2030i_1000_V4","INDC2030i_1000_V3",
                             "NPi2020_low_V3","INDC2030_low_V3") & period==2030 & 
               variable %in% "Capacity|Electricity" ]
ggplot()+
  geom_bar(data=plot1,aes(x=modscen,y=value/1,group=interaction(modscen,variable),fill=variable),stat="identity")+
  scale_fill_manual(values=plotstyle(plotvar))  + xlab("Model - Scenario")+
  geom_text(data=plot1,aes(x=modscen,y=1300,label=modscen,angle=90,hjust="right"),color="red",size=2.5)+
  geom_point(data=plot2,aes(x=modscen,y=value))+ylim(0,1300)+
  theme_bw()+ylab("Power Capacity (GW)")
ggsave(file="Capacity_IND.png",width=8,height=5)

