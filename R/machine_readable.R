# District/Unitary (and County/Unitary) machine readable file #

library(tidyverse) ; library(sf) ; library(readxl)

# Load data ---------------------------------

# Local authority districts (2021)
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-may-2021-uk-buc
ltla09 <- read_csv("geospatial/Local_Authority_Districts_(December_2009)_Boundaries.csv") %>%
  select(AREACD = lad09cd, AREANM = lad09nm) %>%
  mutate(Tier = "District/Unitary")

ltla11 <- read_csv("geospatial/Local_Authority_Districts_(December_2011)_Boundaries_EW_BGC.csv") %>%
  select(AREACD = lad11cd, AREANM = lad11nm) %>%
  mutate(Tier = "District/Unitary")

ltla17 <- read_csv("geospatial/Local_Authority_Districts__December_2017__Boundaries_GB_BUC.csv") %>%
  select(AREACD = LAD17CD, AREANM = LAD17NM) %>%
  mutate(Tier = "District/Unitary")

ltla18 <- read_csv("geospatial/Local_Authority_Districts_(December_2018)_Boundaries_UK_BUC.csv") %>%
  select(AREACD = lad18cd, AREANM = lad18nm) %>%
  mutate(Tier = "District/Unitary")

ltla19 <- read_csv("geospatial/Local_Authority_Districts_(December_2019)_Boundaries_UK_BUC.csv") %>%
  select(AREACD = lad19cd, AREANM = lad19nm) %>%
  mutate(Tier = "District/Unitary")

ltla20 <- read_csv("geospatial/Local_Authority_Districts_(December_2020)_UK_BUC.csv") %>%
  select(AREACD = LAD20CD, AREANM = LAD20NM) %>%
  mutate(Tier = "District/Unitary")

ltla21 <- read_csv("geospatial/Local_Authority_Districts_2021.csv") %>%
  select(AREACD = LAD21CD, AREANM = LAD21NM) %>%
  mutate(Tier = "District/Unitary")

ltla <- bind_rows(ltla09,ltla11,ltla17,ltla18,ltla19,ltla20,ltla21) %>% distinct(AREACD,.keep_all=TRUE)

# Counties (2021)
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/counties-april-2021-names-and-codes-in-england
counties21 <- read_csv("geospatial/Counties_2021.csv") %>%
  select(AREACD = CTY21CD, AREANM = CTY21NM) %>%
  mutate(Tier = "County/Unitary")

counties20 <- read_csv("geospatial/Counties_(December_2020)_EN_BUC.csv") %>%
  select(AREACD = CTY20CD, AREANM = CTY20NM) %>%
  mutate(Tier = "County/Unitary")

counties19 <- read.csv("geospatial/Counties_(December_2019)_Boundaries_EN_BUC.csv") %>%
  select(AREACD = cty19cd, AREANM = cty19nm) %>%
  mutate(Tier = "County/Unitary")

counties <- bind_rows(counties21,counties20,counties19) %>%
  distinct(AREACD,.keep_all=TRUE)

tiers <- bind_rows(ltla, counties)

# Metrics
path <- "MetricsData.xlsx"

metadata <- read_xlsx(path, sheet = "Metadata")

sheets <- path %>%
  excel_sheets() %>%
  set_names() %>%
  .[.!= "Metadata"]

raw <- map_df(sheets, ~mutate(read_excel(path, sheet = .x,
                                         col_types = c("text", "numeric","text"),
                                         skip = 1), Worksheet = .x))

# Clean data ---------------------------------

# enrich with tier information
raw_geo <- left_join(raw, tiers, by = "AREACD") %>%
  filter(!is.na(Tier))

# filter metrics with "District/Unitary" and remove "County/Unitary"
a <- filter(raw_geo, Worksheet %in% pull(distinct(filter(raw_geo, AREANM == "Fareham")), Worksheet) &
           Tier != "County/Unitary") %>%
  mutate(Tier == "District/Unitary")

# filter metrics with "County/Unitary"
b <- filter(raw_geo, !Worksheet %in% pull(distinct(filter(raw_geo, AREANM == "Fareham")), Worksheet)) %>%
  mutate(Tier == "County/Unitary")

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
    Worksheet == "HouseAdditions" & str_detect(AREACD, "^N") ~ "2021",
    Worksheet == "HouseAdditions" & str_detect(AREACD, "^W|^S") ~ "2020",
    Worksheet == "HouseAdditions" & str_detect(AREACD, "^E") ~ "2019-20",
    TRUE ~ Period)) %>%
  select(-Worksheet)

# Write data ---------------------------------
cat("There may be some discrepancies between this data download and the accompanying dataset caused by rounding issues but these should be insignificant and are unlikely to affect any further analysis\n\n",file="machine_readable.csv")
write_excel_csv(select(df, -c(Shortened)), "machine_readable.csv",append=TRUE,col_names=TRUE)
                