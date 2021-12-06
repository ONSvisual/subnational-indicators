# District/Unitary and where missing, County/Unitary machine readable file #

library(tidyverse) ; library(sf) ; library(readxl)

# Load data ---------------------------------

# Local authority districts (2021)
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-may-2021-uk-buc
ltla <- read_csv("geospatial/Local_Authority_Districts_(May_2021)_UK_BUC.csv") %>% 
  select(AREACD = LAD21CD, AREANM = LAD21NM) %>% 
  mutate(Tier = "District/Unitary")

# Counties (2021)
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/counties-april-2021-names-and-codes-in-england
counties <- read_csv("geospatial/Counties_(April_2021)_Names_and_Codes_in_England.csv") %>% 
  select(AREACD = CTY21CD, AREANM = CTY21NM) %>% 
  mutate(Tier = "County/Unitary")

tiers <- bind_rows(ltla, counties)

# Metrics
metadata <- read_xlsx("MetricsData.xlsx", sheet = "Metadata")

path <- "MetricsData.xlsx"

sheets <- path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  .[.!= "Metadata"] 

raw <- map_df(sheets, ~mutate(read_excel(path, sheet = .x, 
                                         col_types = c("text", "numeric"), 
                                         skip = 1), Worksheet = .x))

# Clean data ---------------------------------

# enrich with tier information
raw_geo <- left_join(raw, tiers, by = "AREACD") %>% 
  filter(!is.na(Tier))

# filter metrics with "District/Unitary" and remove "County/Unitary"
a <- filter(raw_geo, Worksheet %in% pull(distinct(filter(raw_geo, AREANM == "Fareham")), Worksheet) &
           Tier != "County/Unitary")

# filter metrics with "County/Unitary"
b <- filter(raw_geo, !Worksheet %in% pull(distinct(filter(raw_geo, AREANM == "Fareham")), Worksheet))

raw_geo_final <- bind_rows(a, b)

# pull in metadata 
# cannot standardise across different tiers!!
df <- map_df(pull(distinct(raw_geo, Worksheet)), ~raw_geo %>% 
         filter(Worksheet == .x) %>% 
         mutate(Indicator = pull(filter(metadata, Worksheet == .x), Indicator),
                Shortened = pull(filter(metadata, Worksheet == .x), Shortened),
                Category = pull(filter(metadata, Worksheet == .x), Category),
                Period = pull(filter(metadata, Worksheet == .x), Period),
                Measure = pull(filter(metadata, Worksheet == .x), Measure),
                Unit = pull(filter(metadata, Worksheet == .x), Unit)) %>% 
           relocate(Value, .after = Unit)) %>% 
  mutate(Period = case_when(
    Worksheet == "HouseAdditions" &str_detect(AREACD, "^N") ~ "2021",
    Worksheet == "HouseAdditions" &str_detect(AREACD, "^W|^S") ~ "2020",
    Worksheet == "HouseAdditions" & str_detect(AREACD, "^E") ~ "2019-20",
    TRUE ~ Period)) %>% 
  select(-Worksheet)

# Write data ---------------------------------
write_csv(select(df, -Shortened), "machine_readable.csv")
