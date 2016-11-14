

#### processing function
process_data <- function(all,scens){
    # move years from rows to a new column
    all <- invisible(melt(all,measure.vars=names(all)[grep("[0-9]+",names(all))],variable.name = "period",variable.factor=FALSE))
    all$period <- as.numeric(all$period)
    all <- all[!period %in% c(1950,1955,1960,1965,1970,1975,1980,1985,1990,1995,2000,2001,2002,2003,2004,2006,2007,2008,2009,2011,2012,2013,2014,2016,2017,2018,2019,2021,2022,2023,2024,2026,2027,2028,2029,2031,2032,2033,2034,2036,2037,2038,2039,2041,2042,2043,2044,2046,2047,2048,2049,2051,2052,2053,2054,2056,2057,2058,2059,2061,2062,2063,2064,2066,2067,2068,2069,2071,2072,2073,2074,2076,2077,2078,2079,2081,2082,2083,2084,2086,2087,2088,2089,2091,2092,2093,2094,2096,2097,2098,2099,2101,2102,2103,2104,2106,2107,2108,2109)]
    # Rename columns
    setnames(all, "MODEL", "model")
    setnames(all, "SCENARIO", "scenario")
    setnames(all, "REGION", "region")
    setnames(all, "VARIABLE", "variable")
    setnames(all, "UNIT", "unit")

    all   <- merge(scens, all, by=c("scenario"), all=TRUE)

    all  <- na.omit(all) # remove rows containing NAs

    #add column for sorting into national and global models
    all$Scope <- factor("global",levels = c("global","national"))

    #make variables a factor, so that the order in facets can be manipulated easily
    all$variable <- factor(all$variable)
    #baseline scenarios will thus be exclude from loop over "Baselines of baselines"
    all[all$Baseline=="-",]$Baseline <- NA

    return(all)
}

#### function for adding variables
add_variables <- function(all,scens){
    ####Additional variables
    #source functions for creation of additional variables
    source("functions/calcVariable.R")
    source("functions/calcRel2Base.R")
    all <- calcVariable(all,'`Emissions|CO2|FFI` ~ `Emissions|CO2|Energy and Industrial Processes` ' , newUnit='Mt CO2/yr')
    all <- calcVariable(all,'`Emissions Intensity of GDP|MER` ~ `Emissions|CO2|FFI`/`GDP|MER` ' , newUnit='kg CO2/$US 2005')
    all <- calcVariable(all,'`Emissions Intensity of GDP|PPP` ~ `Emissions|CO2|FFI`/`GDP|PPP` ' , newUnit='kg CO2/$US 2005')
    all <- calcVariable(all,'`Emissions per capita` ~ `Emissions|CO2|FFI`/`Population` ' , newUnit='t CO2/cap')
    all <- calcVariable(all,'`LU Emissions per capita` ~ `Emissions|CO2|AFOLU`/`Population` ' , newUnit='t CO2/cap')
    all <- calcVariable(all,'`Final Energy per capita` ~ `Final Energy`/`Population` * 1000 ' , newUnit='GJ/cap')
    all <- calcVariable(all,'`BECCS per capita` ~ `Carbon Sequestration|CCS|Biomass` / `Population` ' , newUnit='t CO2/cap')
    all <- calcVariable(all,'`Non-CO2 GHG per capita` ~ (`Emissions|Kyoto Gases` - `Emissions|CO2` )  / `Population` ' , newUnit='t CO2/cap')
    all <- calcVariable(all,'`Emissions|CO2|FFI|gross` ~ `Emissions|CO2|Energy and Industrial Processes` +  `Carbon Sequestration|CCS|Biomass`' , newUnit='Mt CO2/yr')
    all <- calcVariable(all,'`Fossil emissions per cap` ~ `Emissions|CO2|FFI|gross` / `Population` ' , newUnit='t CO2/cap')
    all <- calcVariable(all,'`Wind and Solar Share` ~ ( `Secondary Energy|Electricity|Solar` + `Secondary Energy|Electricity|Wind` ) / `Secondary Energy|Electricity` * 100 ' , newUnit='%')
    all <- calcVariable(all,'`Nuclear Share` ~ ( `Secondary Energy|Electricity|Nuclear`  ) / `Secondary Energy|Electricity` * 100 ' , newUnit='%')
    all <- calcVariable(all,'`Share of Elec in FE` ~  `Final Energy|Electricity`   / `Final Energy` * 100 ' , newUnit='%')
    all <- calcVariable(all,'`Share of Elec in Transport` ~  `Final Energy|Transportation|Electricity`   / `Final Energy|Transportation` * 100 ' , newUnit='%')
    # all <- calcVariable(all,'`Carbon Intensity of Electricity` ~ `Emissions|CO2|Energy|Supply|Electricity`/ `Secondary Energy|Electricity` ' , newUnit='kg CO2/GJ')
    all <- calcVariable(all,'`Carbon Intensity of FE` ~ `Emissions|CO2|FFI`/`Final Energy` ' , newUnit='kg CO2/GJ')
    all <- calcVariable(all,'`Energy Intensity of GDP|MER` ~ `Final Energy`/`GDP|MER` ' , newUnit='GJ/$2005')
    all <- calcVariable(all,'`Energy Intensity of GDP|PPP` ~ `Final Energy`/`GDP|PPP` ' , newUnit='GJ/$2005')
    all <- calcVariable(all,'`GDP per capita|MER` ~ `GDP|MER`/`Population` ' , newUnit='1000 $US 2005/cap')
    all <- calcVariable(all,'`GDP per capita|PPP` ~ `GDP|PPP`/`Population` ' , newUnit='1000 $US 2005/cap')
    all <- calcRel2Base(all,var="Emissions|CO2|FFI",baseEq1=F,"relative Abatement|CO2",scens)
    all <- calcRel2Base(all,var="Carbon Intensity of FE",baseEq1=T,"Carbon intensity rel. to Base",scens)
    all <- calcVariable(all,'`Emissions|CO2|rel2Base` ~ 100.0 - `relative Abatement|CO2` ' , newUnit='%')
    all <- calcVariable(all,'`Reduction rel to 2010` ~ 100.0 - `relative Abatement|CO2` ' , newUnit='%')
    all <- calcRel2Base(all,var="Energy Intensity of GDP|MER",baseEq1=T,"Energy intensity rel. to Base",scens)
    all <- calcVariable(all,'`CI over EI indicator` ~ `Carbon intensity rel. to Base`/`Energy intensity rel. to Base` ' , newUnit='')

    all <- calcVariable(all,'`Policy Cost` ~ `Policy Cost|Area under MAC Curve` ' , newUnit='billion US$2010/yr')
    all <- calcVariable(all,'`Policy Cost` ~ `Policy Cost|Consumption Loss` ' , newUnit='billion US$2010/yr')
#    all <- calcVariable(all,'`Policy Cost` ~ `Policy Cost|Other` ' , newUnit='billion US$2010/yr')
    all <- calcVariable(all,'`Mitigation Costs` ~ `Policy Cost` / `GDP|MER` *100 ' , newUnit='% of GDP')
    source("functions/calcBudget.R")
    all <- calcBudget(all,'Emissions|CO2','Carbon budget',scens)

#    all <- overwrite(remind::calcCumulatedDiscount(all, discount = 0.05, nameVar = "GDP|MER"), all)



    # add variables indexed relative to baseyear
    source("functions/calcRel2BaseYear.R")

    vars <- c("Emissions|CO2", "Emissions|CO2|FFI")
    all <- rbind(all, calcRel2BaseYear(df=all,vars=vars))

    all <- calcVariable(all,'`Reduction rel to 2010` ~ 100.0 - `Emissions|CO2|FFI|rel2010` * 100 ' , newUnit='%')
    return(all)
}
