# Data for jitter plot #

library(tidyverse) ; library(readxl)

# Read data ------------------------------

# Metadata and indicators
# Source: ONS
# URL: https://github.com/ONSdigital/LUDA
metadata <- read_csv("metadata.csv")

folder <- "../LUDA-main/Output"
raw <- folder %>%
  dir(pattern = "*.csv", full.names = T) %>% 
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(.default = "c")), .id = "Worksheet") %>%
  mutate(Worksheet = str_extract(Worksheet, "(?<=Output/)(.+)(?=\\.)")) %>% 
  filter(
    # exclude indicators that aren't to be normalised
    !Worksheet %in% pull(filter(metadata, Jitter == "exclude"), Worksheet),
    # drop rows with missing values
    !is.na(Value))

# One-off change for August release # 
raw_df <- filter(raw,
    # remove FESkillsAchievements
    Indicator != "19+ Further Education and Skills Achievements (qualifications) excluding community learning, Multiply and bootcamps") %>% 
  mutate(
    # replace specific value with NA
    Value = case_when(Worksheet == "RateEmployment" & AREANM == "City of London" ~ "", TRUE ~ Value) 
    ) 

# Local Authority districts (2021)
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-may-2021-uk-buc
lad <- pull(read_csv("geospatial/Local_Authority_Districts_2021.csv"), LAD21CD)

# Clean and create MAD scores ------------------------------
df <- raw_df %>% 
  filter(AREACD %in% lad) %>% # filter by local authority
  # drop Welsh names
  mutate(AREANM = case_when(str_detect(AREANM, "/") ~ str_remove_all(AREANM, "\\/.*"), TRUE ~ AREANM)) %>% 
  group_by(Worksheet) %>% 
  filter(Period == max(Period)) %>% # filter by latest period
  ungroup() %>% 
  # join metadata
  select(-Category) %>% 
  left_join(select(metadata, Worksheet, Shortened, Category), by = "Worksheet") %>% 
  group_by(Worksheet) %>% 
  mutate(
    # reverse polarity
    Value = as.double(Value),
    Value = case_when(Polarity == -1 ~Value*(-1), TRUE ~ Value),
    # median absolute deviation
    MAD = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE),
    Value = case_when(Polarity == -1 ~abs(Value), TRUE ~ Value)) %>% 
  ungroup() %>% 
  # sort as per technical annex 
  arrange(factor(Worksheet, levels = pull(metadata, Worksheet))) %>%
  select(unique = AREACD, 
         group = AREANM, 
         Geography, Indicator, 
         id = Shortened, 
         Category, Period, Measure,
         unit = Unit, 
         real = Value,
         value = MAD)

# Write results ------------------------------
write_excel_csv(df, "../app/revised_data.csv")
