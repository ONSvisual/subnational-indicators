library(tidyverse) ; library(sf) ; library(readxl)

# Local Authority districts (2021)
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-may-2021-uk-buc
lad <- read_csv("geospatial/Local_Authority_Districts_2021.csv") %>% 
  select(AREACD = LAD21CD, AREANM = LAD21NM)

# Metrics
path <- "MetricsData.xlsx"

metadata <- read_xlsx(path, sheet = "Metadata")

sheets <- path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  .[.!= "Metadata"] 

negatives <- metadata%>%filter(Polarity=="Negative")%>%select(Worksheet)

raw <- left_join(lad,
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
                Value = case_when(Worksheet %in% negatives$Worksheet ~Value*(-1),
                                  TRUE ~ Value),
                # median absolute deviation
                MAD = (Value - median(Value, na.rm = TRUE)) / median(abs(Value - median(Value, na.rm = TRUE))*1.4826, na.rm = TRUE),
                Value = case_when(Worksheet %in% negatives$Worksheet ~abs(Value),
                                  TRUE ~ Value)) %>% 
           relocate(Value, .after = Unit)) %>% 
  mutate(Period = case_when(
    Worksheet == "HouseAdditions" & str_detect(AREACD, "^N") ~ "2021",
    Worksheet == "HouseAdditions" & str_detect(AREACD, "^W|^S") ~ "2020",
    Worksheet == "HouseAdditions" & str_detect(AREACD, "^E") ~ "2019-20",
    TRUE ~ Period)) %>% 
  select(-Worksheet) %>% 
  # exlude metrics that aren't normalised
  filter(!Indicator %in% pull(filter(metadata, Jitter == "exclude"), Indicator))


results<-rename(df, 
                unique = AREACD,
                group = AREANM,
                id = Shortened,
                unit = Unit,
                real = Value,
                value = MAD)
# Write results
write_excel_csv(results, "revised_data.csv")
