# Machine readable #

library(tidyverse)

# Local Authority districts (2021)
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-may-2021-uk-buc
lad <- pull(read_csv("geospatial/Local_Authority_Districts_2021.csv"), LAD21CD)

# Metadata and indicators
# Source: ONS
# URL: https://github.com/ONSdigital/LUDA
metadata <- read_csv("metadata.csv")

folder <- "../../LUDA/Output"
raw <- folder %>%
  dir(pattern = "*.csv", full.names = T) %>% 
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(.default = "c")), .id = "Worksheet") %>%
  mutate(Worksheet = str_extract(Worksheet, "(?<=Output/)(.+)(?=\\.)")) %>% 
  select(Worksheet, Polarity, AREACD, AREANM, Geography, Indicator, Category, Period, Measure, Unit, Value)

jitter <- raw %>% 
  filter(
    # filter by local authority
    AREACD %in% lad,
    # exclude indicators that aren't to be normalised
    !Worksheet %in% pull(filter(metadata, Jitter == "exclude"), Worksheet),
    # drop rows with missing values
    !is.na(Value)) %>% 
  # drop Welsh names
  mutate(AREANM = case_when(str_detect(AREANM, "/") ~ str_remove_all(AREANM, "\\/.*"), TRUE ~ AREANM)) %>% 
  # join metadata
  select(-Category) %>% 
  left_join(select(metadata, Worksheet, Category), by = "Worksheet") %>% 
  group_by(Worksheet) %>% 
  filter(Period == max(Period)) %>% # filter by latest period
  ungroup() %>% 
  group_by(Worksheet) %>% 
  mutate(
    # reverse polarity
    Value = as.double(Value),
    Value = case_when(Polarity == -1 ~Value*(-1), TRUE ~ Value),
    # median absolute deviation
    MAD = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE),
    Value = case_when(Polarity == -1 ~abs(Value), TRUE ~ Value)) %>% 
  ungroup() %>% 
  relocate(Category, .after = Indicator)

non_jitter <- raw %>% 
  filter(
    # filter by local authority
    AREACD %in% lad,
    # include indicators that aren't to be normalised
    Worksheet %in% pull(filter(metadata, Jitter == "exclude"), Worksheet),
    # drop rows with missing values
    !is.na(Value)) %>% 
  mutate(AREANM = case_when(str_detect(AREANM, "/") ~ str_remove_all(AREANM, "\\/.*"), TRUE ~ AREANM)) %>% 
  # join metadata
  select(-Category) %>% 
  left_join(select(metadata, Worksheet, Category), by = "Worksheet") %>% 
  group_by(Worksheet) %>% 
  filter(Period == max(Period)) %>% # filter by latest period
  ungroup() %>% 
  mutate(MAD = as.character(NA)) %>% 
  relocate(Category, .after = Indicator)
  
other_geographies <- raw %>% 
  filter(
    # exclude local authorities
    !AREACD %in% lad,
    # drop rows with missing values
    !is.na(Value)) %>% 
  # join metadata
  select(-Category) %>% 
  left_join(select(metadata, Worksheet, Category), by = "Worksheet") %>% 
  group_by(Worksheet) %>% 
  filter(Period == max(Period)) %>% # filter by latest period
  ungroup() %>% 
  mutate(MAD = as.character(NA)) %>% 
  relocate(Category, .after = Indicator) 
  
# Join dataframes
df <- bind_rows(
  mutate(jitter, Value = as.character(Value), MAD = as.character(MAD)), 
  non_jitter,
  other_geographies) %>% 
  # sort as per technical annex 
  arrange(factor(Worksheet, levels = pull(metadata, Worksheet))) %>% 
  select(-c(Worksheet, Polarity))

# write results
cat("There may be some discrepancies between this data download and the accompanying dataset caused by rounding issues but these should be insignificant and are unlikely to affect any further analysis\n\n", file = "machine_readable.csv")
write_excel_csv(df, "machine_readable.csv", append = TRUE, col_names = TRUE)