library(tidyverse) ; library(sf) ; library(readxl)

# Counties and unitary authorities (2021)
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/ons::counties-and-unitary-authorities-may-2021-uk-buc
ctyua <- read_csv("geospatial/Counties_and_Unitary_Authorities_(May_2021)_UK_BUC.csv") %>% 
  select(AREACD = CTYUA21CD, AREANM = CTYUA21NM)

# Metrics
metadata <- read_xlsx("MetricsData.xlsx", sheet = "Metadata")

path <- "MetricsData.xlsx"

sheets <- path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  .[.!= "Metadata"] 

raw <- left_join(ctyua,
                 map_df(sheets, ~mutate(read_excel(path, sheet = .x, col_types = c("text", "numeric"), skip = 1), Worksheet = .x)),
                 by = "AREACD")

df <- map_df(pull(distinct(raw, Worksheet)), ~raw %>% 
         filter(Worksheet == .x) %>% 
         mutate(Indicator = pull(filter(metadata, Worksheet == .x), Indicator),
                Shortened = pull(filter(metadata, Worksheet == .x), Shortened),
                Category = pull(filter(metadata, Worksheet == .x), Category),
                Period = pull(filter(metadata, Worksheet == .x), Period),
                Measure = pull(filter(metadata, Worksheet == .x), Measure),
                Unit = pull(filter(metadata, Worksheet == .x), Unit),
                # reverse polarity
                Value = case_when(Worksheet %in% c("RateUnemployment", "TIMEtravelWORKPT", "TIMEtravelWORKBike",
                                                 "TIMEtravelWORKCar", "Smoke", "NEET", "accesstoamenities",
                                                 "GreenSpace") ~Value*(-1),
                                  TRUE ~ Value),
                # median absolute deviation
                MAD = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE),
                Value = case_when(Worksheet %in% c("RateUnemployment", "TIMEtravelWORKPT", "TIMEtravelWORKBike",
                                                   "TIMEtravelWORKCar", "Smoke", "Obesity", "NEET", "accesstoamenities",
                                                   "GreenSpace") ~abs(Value),
                                  TRUE ~ Value)) %>% 
           relocate(Value, .after = Unit)) %>% 
  mutate(Period = case_when(
    Worksheet == "HouseAdditions" &str_detect(AREACD, "^N") ~ "2021",
    Worksheet == "HouseAdditions" &str_detect(AREACD, "^W|^S") ~ "2020",
    Worksheet == "HouseAdditions" & str_detect(AREACD, "^E") ~ "2019-20",
    TRUE ~ Period)) %>% 
  select(-Worksheet)

# Write results
write_csv(select(df, -c(Shortened, MAD)), "ctyua.csv")
