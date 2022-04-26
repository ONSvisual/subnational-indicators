# Machine readable file #

library(tidyverse) ; library(sf) ; library(readxl)

# Load data ---------------------------------

# Administrative geographies
# Source: ONS OPen Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/register-of-geographic-codes-march-2022-for-the-united-kingdom/about
geographies <- read_csv("geospatial/gss_codes.csv") %>% 
  select(-Geography)

# Local authority districts (2021)
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-may-2021-uk-buc
ltla <- read_csv("geospatial/Local_Authority_Districts_2021.csv") %>%
  select(AREACD = LAD21CD) %>% 
  pull(AREACD)

# Metrics
path <- "2022_04_22_MetricsData_input.xlsx"
metadata <- read_xlsx(path, sheet = "Metadata")

sheets <- path %>%
  excel_sheets() %>%
  set_names() %>%
  .[.!= "Metadata"]

raw <- map_df(sheets, ~mutate(read_excel(path, sheet = .x,
                                         col_types = c("text", "numeric","text"),
                                         skip = 1), Worksheet = .x))

# Clean data ---------------------------------

# enrich with AREANM values
raw_geo <- left_join(raw, geographies, by = "AREACD") %>%
  relocate(c(AREANM, Geography), .after = AREACD)

# pull in metadata
df <- map_df(pull(distinct(raw_geo, Worksheet)), ~raw_geo %>%
         filter(Worksheet == .x) %>%
         mutate(Indicator = pull(filter(metadata, Worksheet == .x), Indicator),
                Shortened = pull(filter(metadata, Worksheet == .x), Shortened),
                Category = pull(filter(metadata, Worksheet == .x), Category),
                Period = pull(filter(metadata, Worksheet == .x), Period),
                Measure = pull(filter(metadata, Worksheet == .x), Measure),
                Unit = pull(filter(metadata, Worksheet == .x), Unit)) %>%
           relocate(Value, .after = Unit)) %>% 
  select(-Worksheet)

# Write data ---------------------------------
cat("There may be some discrepancies between this data download and the accompanying dataset caused by rounding issues but these should be insignificant and are unlikely to affect any further analysis\n\n", file = "machine_readable.csv")
write_excel_csv(select(df, -c(Shortened)), "machine_readable.csv", append = TRUE, col_names = TRUE)
