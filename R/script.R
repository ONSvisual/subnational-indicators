# Metrics #

library(tidyverse) ; library(sf) ; library(readxl)

# Local Authority names (2021)
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-april-2021-names-and-codes-in-the-united-kingdom/explore
ltla <- read_csv("geospatial/Local_Authority_Districts_(April_2021)_Names_and_Codes_in_the_United_Kingdom.csv") %>% 
  select(AREACD = LAD21CD, AREANM = LAD21NM)

# International Territorial Levels to Local Authority lookup
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/documents/local-authority-district-december-2020-to-lau1-to-itl3-to-itl2-to-itl1-january-2021-lookup-in-united-kingdom-v2/about
itl_ltla <- read_xlsx("geospatial/LAD20_LAU121_ITL321_ITL221_ITL121_UK_LU_v2.xlsx") %>% 
  select(AREACD = LAD20CD, `ITL code` = ITL321CD)

# Raw data
path <- "20211006 LU metrics data .xlsx"
df <- path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)

names(df)

## ------ Boosting Living Standards ------ ##

# GVA per capita
# Source: ONS
# URL: https://www.ons.gov.uk/economy/grossvalueaddedgva/datasets/nominalregionalgrossvalueaddedbalancedperheadandincomecomponents
GVApercapita <- left_join(ltla, (df$`62GVApercapita` %>% 
                                   left_join(itl_ltla, by = "ITL code") %>% 
                                   filter(AREACD %in% ltla$AREACD, Year == max(Year)) %>% 
                                   select(AREACD, Period = Year, Value = `GVA per head of population at current basic prices`) %>%
                                   mutate(Period = as.character(Period),
                                          Indicator = "GVA per capita", 
                                          Shortened = "Gross Value Added per head",
                                          Category = "Boosting Living Standards",
                                          Measure = "Per capita",
                                          Unit = "Per capita") %>% 
                                   select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE))

# Median weekly pay
# Source: ONS
# URL: https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/placeofresidencebylocalauthorityashetable8
PAYweekMEDIAN <- left_join(ltla, (df$`9PAYweekMEDIAN` %>% 
                                    select(2,3,4) %>% 
                                    set_names(.[1, ]) %>%
                                    slice(-1) %>% 
                                    filter(`LA Code` %in% ltla$AREACD, Year == max(Year)) %>% 
                                    mutate(Value = as.numeric(na_if(Value, "x"))) %>% 
                                    select(AREACD = `LA Code`, Period = Year, Value) %>%
                                    mutate(Period = as.character(Period),
                                           Indicator = "Gross median weekly pay",
                                           Shortened = "Median weekly pay",
                                           Category = "Boosting Living Standards", 
                                           Measure = "Pounds",
                                           Unit = "£") %>% 
                                    select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE))

# Unemployment rates of 16-64-year olds
# Source: ONS
# URL: https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/locallabourmarketindicatorsforcountieslocalandunitaryauthoritiesli01
RateUnemployment <- left_join(ltla, (df$`8RateUnemployment` %>% 
                                       filter(Geography %in% ltla$AREACD, year == max(year)) %>%
                                       mutate(Value = na_if(UnemploymentRate, "!"),
                                              Value = as.numeric(na_if(Value, "x"))) %>% 
                                       select(AREACD = Geography, Period = year, Value) %>% 
                                       mutate(Period = as.character(Period),
                                              Indicator = "Unemployment rate",
                                              Shortened = "Unemployment rate",
                                              Category = "Boosting Living Standards", 
                                              Measure = "Rate",
                                              Unit = "%",
                                              Value = as.numeric(Value)) %>% 
                                       select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE))

# Gross Disposable Household Income
# Source:
# URL: https://www.ons.gov.uk/economy/regionalaccounts/grossdisposablehouseholdincome/datasets/regionalgrossdisposablehouseholdincomelocalauthoritiesbyitl1region
GDIpercapita <- left_join(ltla, (df$`48GDIpercapita` %>% 
                                   filter(`LAD code` %in% ltla$AREACD, Year == max(Year)) %>% 
                                   select(AREACD = `LAD code`, Period = Year, Value = `GDIpercapita`) %>%
                                   mutate(Period = as.character(Period), 
                                          Indicator = "Gross Disposable Income", 
                                          Shortened = "Disposable income",
                                          Category = "Boosting Living Standards", 
                                          Measure = "Pounds",
                                          Unit = "£") %>% 
                                   select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE))

# Number of 'high growth' businesses
# Source: ONS
# URL: https://www.ons.gov.uk/businessindustryandtrade/business/activitysizeandlocation/datasets/businessdemographyreferencetable
BIZhighGROWTH <- left_join(ltla, (df$NEW_2BIZhighGROWTH %>% 
                                    select(1,3,4) %>% 
                                    set_names(.[1, ]) %>%
                                    slice(-1) %>% 
                                    filter(`LA code` %in% ltla$AREACD, Year == max(Year)) %>% 
                                    select(AREACD = `LA code`, Period = Year, Value) %>%
                                    mutate(Period = as.character(Period),
                                           Indicator = "Number of 'high growth' businesses", 
                                           Shortened = "High business growth",
                                           Category = "Boosting Living Standards", 
                                           Measure = "Businesses",
                                           Unit = "",
                                           Value = as.numeric(Value)) %>% 
                                    select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE))

# Percentage of premises with gigabit availability
# Source: OFCOM
# URL: https://www.ofcom.org.uk/research-and-data/multi-sector-research/infrastructure-research/connected-nations-update-summer-2021
broadbandAVAIL <- left_join(ltla, (df$`23broadbandAVAIL` %>% 
                                     filter(Geography %in% ltla$AREACD, year == max(year),
                                            `Broadband measure` == "Gigabit availability") %>% 
                                     select(AREACD = Geography, Period = year, Value = pctOFpremises) %>%
                                     mutate(Period = as.character(Period), 
                                            Indicator = "Premises with gigabit availability", 
                                            Shortened = "Access to gigabit broadband",
                                            Category = "Boosting Living Standards", 
                                            Measure = "Percentage",
                                            Unit = "%") %>% 
                                     select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down")  %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE))
  
# Travel time to employment centres (500 to 4999 jobs) by public transport, bike and car
# Source: DfT
# URL: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/853147/jts0401.ods
TIMEtravelWORKPT <- left_join(ltla, (df$`21TIMEtravelWORK` %>% 
                                       select(AREACD = 1, Value = 3) %>% 
                                       slice(-1) %>% 
                                       filter(AREACD %in% ltla$AREACD) %>%
                                       mutate(Period = 2017,
                                              Value = as.numeric(Value)) %>% 
                                       select(AREACD, Period, Value) %>%
                                       mutate(Period = as.character(Period),
                                              Indicator = "Journey time to employment by public transport",
                                              Shortened = "Public transport to work",
                                              Category = "Boosting Living Standards", 
                                              Measure = "Minutes",
                                              Unit = "mins") %>% 
                                       select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  # change polarity
  mutate(Value = Value*(-1)) %>% 
  arrange(Value) %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE),
         Value = abs(Value))

TIMEtravelWORKBike <- left_join(ltla, (df$`21TIMEtravelWORK` %>% 
                                         select(AREACD = 1, Value = 4) %>% 
                                         slice(-1) %>% 
                                         filter(AREACD %in% ltla$AREACD) %>%
                                         mutate(Period = 2017,
                                                Value = as.numeric(Value)) %>% 
                                         select(AREACD, Period, Value) %>%
                                         mutate(Period = as.character(Period),
                                                Indicator = "Journey time to employment by bike", 
                                                Shortened = "Cycle to work",
                                                Category = "Boosting Living Standards", 
                                                Measure = "Minutes",
                                                Unit = "mins") %>% 
                                         select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  # change polarity
  mutate(Value = Value*(-1)) %>% 
  arrange(Value) %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE),
         Value = abs(Value))

TIMEtravelWORKCar <- left_join(ltla, (df$`21TIMEtravelWORK` %>% 
                                        select(AREACD = 1, Value = 5) %>% 
                                        slice(-1) %>% 
                                        filter(AREACD %in% ltla$AREACD) %>%
                                        mutate(Period = 2017,
                                               Value = as.numeric(Value)) %>% 
                                        select(AREACD, Period, Value) %>%
                                        mutate(Period = as.character(Period),
                                               Indicator = "Journey time to employment by car", 
                                               Shortened = "Drive to work",
                                               Category = "Boosting Living Standards", 
                                               Measure = "Minutes",
                                               Unit = "mins") %>% 
                                        select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  # change polarity
  mutate(Value = Value*(-1)) %>% 
  arrange(Value) %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE),
         Value = abs(Value))

## ------ Spreading opportunity and improving public services ------ ##

# Healthy life expectancy at birth
# Source: ONS
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/healthstatelifeexpectancyallagesuk
LIFEXPFemale <- left_join(ltla, (df$`13LIFEXP` %>% 
  filter(Geography %in% ltla$AREACD,
         Measure == "Healthy life expectancy at birth", 
         Period == "2017-19",
         Sex == "Female") %>% 
  select(AREACD = Geography, Period, Value = Expectancy) %>% 
  mutate(Indicator = "Female healthy life expectancy", 
         Shortened = "Female healthy life expectancy",
         Category = "Spreading opportunity and improving public services", 
         Measure = "Years",
         Unit = "yrs") %>% 
  select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  arrange(Value) %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE),
         Value = abs(Value))

LIFEXPMale <- left_join(ltla, (df$`13LIFEXP` %>% 
                                   filter(Geography %in% ltla$AREACD,
                                          Measure == "Healthy life expectancy at birth", 
                                          Period == "2017-19",
                                          Sex == "Male") %>% 
                                   select(AREACD = Geography, Period, Value = Expectancy) %>% 
                                   mutate(Indicator = "Male healthy life expectancy", 
                                          Shortened = "Male healthy life expectancy",
                                          Category = "Spreading opportunity and improving public services", 
                                          Measure = "Years",
                                          Unit = "yrs") %>% 
                                   select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  arrange(Value) %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE),
         Value = abs(Value))

# Percentage of adults who currently smoke, are ex-smokers and never smoked
# Source: ONS
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/smokinghabitsintheukanditsconstituentcountries
Smoke <- left_join(ltla, (df$`43Smoke` %>% 
                            filter(`Local Authority Code` %in% ltla$AREACD, Year == max(Year), Smoke == "Current smokers") %>% 
                            select(AREACD = `Local Authority Code`, Period = Year, Value = Percentage) %>%
                            mutate(Period = as.character(Period),
                                   Indicator = "Adults who currently smoke", 
                                   Shortened = "Smokers",
                                   Category = "Spreading opportunity and improving public services",
                                   Measure = "Percentage",
                                   Unit = "%",
                                   Value = as.numeric(Value)) %>%
                            select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  # change polarity
  mutate(Value = Value*(-1)) %>% 
  arrange(Value) %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE),
         Value = abs(Value))

# Percentage of individuals who are active or fairly active on a weekly basis
# Source: Sport England / Welsh Government
# URL: https://www.sportengland.org/know-your-audience/data/active-lives 
# URL: https://gov.wales/national-survey-wales-results-viewer
PhysicalActivity <- left_join(ltla, (df$`44PhysicalActivity` %>% 
                                       filter(Code %in% ltla$AREACD, Year == "2019-20", Activity %in% c("Active", "Fairly Active")) %>% 
                                       select(AREACD = Code, Period = Year, Value = `Rate (%)`) %>%
                                       group_by(AREACD, Period) %>% 
                                       summarise(Value = sum(as.numeric(Value))) %>%
                                       ungroup() %>% 
                                       mutate(Period = as.character(Period),
                                              Indicator = "Individuals who are active or fairly active on a weekly basis", 
                                              Shortened = "Physically active",
                                              Category = "Spreading opportunity and improving public services",
                                              Measure = "Percentage",
                                              Unit = "%",
                                              Value = as.numeric(Value)) %>%
                                       select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE))


# Percentage of young people achieving Key Stage 4 outcomes in English and Maths by age 19
# Source: DfE
# URL: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/915273/2019_KS4_Revised_Local_authority_data.xlsx
GCSEmatheng <- left_join(ltla, (df$`11GCSEmatheng` %>% 
                                  filter(Geography %in% ltla$AREACD, year == max(year)) %>% 
                                  mutate(Value = as.numeric(na_if(pctGCSElvl2, ":"))) %>% 
                                  select(AREACD = Geography, Period = year, Value) %>%
                                  mutate(Period = as.character(Period),
                                         Indicator = "Achieving GCSE English and Maths", 
                                         Shortened = "Achieving GCSE English and Maths",
                                         Category = "Spreading opportunity and improving public services",
                                         Measure = "Percentage",
                                         Unit = "%",
                                         Value = as.numeric(Value)) %>%
                                  select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE))

# % of 5 year olds achieving 'expected level' on literacy, communication and maths early learning goals
# Source: DfE
# URL: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/839563/EYFSP_2019_Tables.xlsx
EarlyYearsFoundationComms <- left_join(ltla, (df$`26EarlyYearsFoundation` %>% 
                                                select(AREACD = `New LA code`, Period = Year, Value = 4) %>% 
                                                filter(AREACD %in% ltla$AREACD, Period == max(Period)) %>%
                                                mutate(Period = as.character(Period), 
                                                       Value = as.numeric(Value)) %>% 
                                                mutate(Indicator = "5 year olds achieving 'expected level' on communication", 
                                                       Shortened = "5 year olds with good communication",
                                                       Category = "Spreading opportunity and improving public services", 
                                                       Measure = "Percentage",
                                                       Unit = "%") %>% 
                                                select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE))

EarlyYearsFoundationLiteracy <- left_join(ltla, (df$`26EarlyYearsFoundation` %>% 
                                                   select(AREACD = `New LA code`, Period = Year, Value = 5) %>% 
                                                   filter(AREACD %in% ltla$AREACD, Period == max(Period)) %>%
                                                   mutate(Period = as.character(Period), 
                                                          Value = as.numeric(Value)) %>% 
                                                   mutate(Indicator = "5 year olds achieving 'expected level' on literacy", 
                                                          Shortened = "5 year olds with good literacy",
                                                          Category = "Spreading opportunity and improving public services", 
                                                          Measure = "Percentage",
                                                          Unit = "%") %>% 
                                                   select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE))

EarlyYearsFoundationMaths <- left_join(ltla, (df$`26EarlyYearsFoundation` %>%
                                                select(AREACD = `New LA code`, Period = Year, Value = 6) %>% 
                                                filter(AREACD %in% ltla$AREACD, Period == max(Period)) %>%
                                                mutate(Period = as.character(Period), 
                                                       Value = as.numeric(Value)) %>% 
                                                mutate(Indicator = "5 year olds achieving 'expected level' on maths", 
                                                       Shortened = "5 year olds with good maths",
                                                       Category = "Spreading opportunity and improving public services", 
                                                       Measure = "Percentage",
                                                       Unit = "%") %>% 
                                                select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE))

# Percentage of pupils achieving grades 4 or above in English and Maths GCSE
# Source: DfE
# URL: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/915263/2019_KS4_Revised_Local_authority_characteristics_data.xlsx
GCSEgrade4to9 <- left_join(ltla, (df$`63GCSEgrades4-9` %>%
                                    select(AREACD = `LA Code`, Period = Year, Value = `Total%`) %>% 
                                    filter(AREACD %in% ltla$AREACD, Period == "2019-20") %>%
                                    mutate(Period = as.character(Period), 
                                           Value = as.numeric(Value)) %>% 
                                    mutate(Indicator = "Pupils achieving grades 4 or above in English and Maths GCSE", 
                                           Shortened = "Grade 4 or above in GCSE English and Maths",
                                           Category = "Spreading opportunity and improving public services", 
                                           Measure = "Percentage",
                                           Unit = "%") %>% 
                                    select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE))

# Percentage of schools rated good or outstanding
# Source: Ofsted
# URL: https://www.gov.uk/government/statistics/state-funded-schools-inspections-and-outcomes-as-at-31-march-2021
Ofsted <- left_join(ltla, (df$`25 Ofsted` %>%
                             select(AREACD = Geography, Period = date, Value = percentage_schools_FE_SkillsProviders_rated_good_or_outstanding) %>% 
                             filter(AREACD %in% ltla$AREACD, Period == max(Period)) %>%
                             mutate(Period = as.character(Period),
                                    Indicator = "Percentage of schools rated good or outstanding", 
                                    Shortened = "Schools rated good or outstanding",
                                    Category = "Spreading opportunity and improving public services", 
                                    Measure = "Percentage",
                                    Unit = "%",
                                    Value = Value*100) %>% 
                             select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE)) 

# Proportion of people employed in skilled employment
# Source:ONS
# URL: https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=17
SkilledEmploy <- left_join(ltla, (df$`57SkilledEmploy` %>%
                                    select(AREACD = mnemonic, Period = Year, Value) %>% 
                                    filter(AREACD %in% ltla$AREACD, Period == "Apr 2020-Mar 2021") %>%
                                    mutate(Value = as.numeric(na_if(Value, "-")),
                                           Period = as.character(Period),
                                           Indicator = "Proportion of people employed in skilled employment", 
                                           Shortened = "People employed in skilled employment",
                                           Category = "Spreading opportunity and improving public services", 
                                           Measure = "Percentage",
                                           Unit = "%",
                                           Value = as.numeric(Value)) %>% 
                                    select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE)) 

# Apprenticeship starts 
# Source:DfE
# URL: https://www.gov.uk/government/statistical-data-sets/fe-data-library-apprenticeships
ApprenticeshipStarts <- left_join(ltla, (df$`28 ApprenticeshipStarts` %>% 
                                           filter(`Local Authority` %in% ltla$AREACD, Year == max(Year)) %>% 
                                           select(AREACD = `Local Authority`, Period = Year, Value) %>%
                                           mutate(Period = as.character(Period),
                                                  Indicator = "Apprenticeships starts", 
                                                  Shortened = "Apprenticeship starts",
                                                  Category = "Spreading opportunity and improving public services",
                                                  Measure = "Starts",
                                                  Unit = "",
                                                  Value = as.numeric(Value)) %>%
                                           select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  arrange(desc(Value)) %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE))

# Apprenticeship completions 
# Source:DfE
# URL: https://www.gov.uk/government/statistical-data-sets/fe-data-library-apprenticeships
ApprenticeshipCompletions <- left_join(ltla, (df$`28a ApprenticeshipCompletions` %>% 
                                           filter(`Local Authority` %in% ltla$AREACD, Year == max(Year)) %>% 
                                           select(AREACD = `Local Authority`, Period = Year, Value) %>%
                                           mutate(Period = as.character(Period),
                                                  Indicator = "Apprenticeships completions", 
                                                  Shortened = "Apprenticeship completions",
                                                  Category = "Spreading opportunity and improving public services",
                                                  Measure = "Completions",
                                                  Unit = "",
                                                  Value = as.numeric(Value)) %>%
                                           select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE))

# Proportion of working age adults Not in Education, Employment or Training
# Source: DfE
# URL: https://www.gov.uk/government/publications/neet-and-participation-local-authority-figures
NEET <- left_join(ltla, (df$`46NEET` %>%
                            filter(`ONS Geography code (9 digit)` %in% ltla$AREACD, Year == max(Year)) %>% 
                            select(AREACD = `ONS Geography code (9 digit)`, Period = Year, Value = `Proportion NEET or not known`) %>%
                            mutate(Period = as.character(Period),
                                   Indicator = "Working age adults Not in Education, Employment or Training", 
                                   Shortened = "Not in Education, Employment, or Training",
                                   Category = "Spreading opportunity and improving public services",
                                   Measure = "Percentage",
                                   Unit = "%",
                                   Value = as.numeric(Value)*100) %>%
                            select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  # change polarity
  mutate(Value = Value*(-1)) %>% 
  arrange(Value) %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE),
         Value = abs(Value))

## ------ Restoring pride in place ------ ##

# Access to local amenities (IMD subdomain)
# Source: DLUHC
# URL: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833995/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx
accesstoamenities <- left_join(ltla, (df$`10IMD` %>%
                          filter(`Local Authority District code (2019)` %in% ltla$AREACD) %>% 
                          select(AREACD = `Local Authority District code (2019)`, Value = `IMD - Average score`) %>%
                          mutate(Period = "2019", 
                                 Indicator = "Access to local amenities", 
                                 Shortened = "Access to local amenities",
                                 Category = "Restoring pride in place", 
                                 Measure = "Average score",
                                 Unit = "") %>% 
                          select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  # change polarity
  mutate(Value = Value*(-1)) %>% 
  arrange(Value) %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE),
         Value = abs(Value))

# Average life satisfaction of residents
# Source: ONS
# URL: https://www.ons.gov.uk/datasets/wellbeing-local-authority/editions/time-series/versions/1
LifeSatisfaction <- left_join(ltla, (df$`35LifeSatisfaction` %>%
                                       filter(`Geography code` %in% ltla$AREACD, Year == max(Year)) %>% 
                                       select(AREACD = `Geography code`, Period = Year, Value = `Life Satisfaction (mean)`) %>%
                                       mutate(Period = "2019/20", 
                                              Indicator = "Average life satisfaction of residents", 
                                              Shortened = "Life satisfaction",
                                              Category = "Restoring pride in place", 
                                              Measure = "Value",
                                              Unit = "") %>% 
                                       select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE))

# Net additions to the housing stock
# Source: DLUHC
# URL: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/938199/Live_Table_122.ods
HouseAdditions <- left_join(ltla, (df$`47HouseAdditions` %>%
                                     filter(`Current\r\nONS code` %in% ltla$AREACD, Year == max(Year)) %>% 
                                     select(AREACD = `Current\r\nONS code`, Period = Year, Value = `Net Additions`) %>%
                                     mutate(Period = as.character(Period),
                                            Indicator = "Net additions to the housing stock", 
                                            Shortened = "New houses",
                                            Category = "Restoring pride in place", 
                                            Measure = "Houses",
                                            Unit = "") %>% 
                                     select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE))

# Access to Green Space - Average distance to nearest park or public garden or playing field (m)
GreenSpace <- left_join(ltla, (df$`49 GreenSpace` %>%
                                 filter(`LAD code` %in% ltla$AREACD) %>%
                                 select(AREACD = `LAD code`, Value = `Average distance to nearest park or public garden or playing field (m)`) %>%
                                 mutate(Period = "2020",
                                        Indicator = "Access to Green Space", 
                                        Shortened = "Distance to green space",
                                        Category = "Restoring pride in place", 
                                        Measure = "Metres",
                                        Unit = "m") %>% 
                                 select(AREACD, Indicator, Shortened, Category, Period, Measure, Unit, Value)), by = "AREACD") %>% 
  fill(c(Indicator, Shortened, Category, Period, Measure, Unit), .direction = "down") %>% 
  # change polarity
  mutate(Value = Value*(-1)) %>% 
  arrange(Value) %>% 
  mutate(mean = mean(Value, na.rm = TRUE),
         median = median(Value, na.rm = TRUE),
         # z-scores
         z_score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE),
         ## median absolute deviation
         mad = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE),
         Value = abs(Value))

# Join data
data <- bind_rows(GVApercapita, PAYweekMEDIAN, RateUnemployment, GDIpercapita, BIZhighGROWTH, broadbandAVAIL, 
                  TIMEtravelWORKPT, TIMEtravelWORKBike, TIMEtravelWORKCar, LIFEXPFemale, LIFEXPMale, Smoke, 
                  PhysicalActivity, GCSEmatheng, EarlyYearsFoundationComms, EarlyYearsFoundationLiteracy, 
                  EarlyYearsFoundationMaths, GCSEgrade4to9, Ofsted, SkilledEmploy, ApprenticeshipStarts, 
                  ApprenticeshipCompletions, NEET, accesstoamenities, LifeSatisfaction, HouseAdditions, 
                  GreenSpace) %>% 
  select(AREACD, AREANM, everything())

glimpse(data)

# Write results
write_csv(rename(data, 
                 unique = AREACD,
                 group = AREANM,
                 id = Shortened,
                 unit = Unit,
                 real = Value,
                 value = mad), "revised_data.csv")
