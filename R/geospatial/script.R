# Administrative Geographies #

library(tidyverse) ; library(httr) ; library(readxl)

# Load data --------------------------------------------------------------------

# Register of Geographic Codes for the United Kingdom (March 2022) 
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/register-of-geographic-codes-march-2022-for-the-united-kingdom/about
url <- "https://www.arcgis.com/sharing/rest/content/items/1b5204ccb1b54314b9649075527a88c4/data"
download.file(url, dest = "Register_of_Geographic_Codes_March_2022_UK.zip")
unzip("Register_of_Geographic_Codes_March_2022_UK.zip", exdir = ".")
file.remove("Register_of_Geographic_Codes_March_2022_UK.zip")

# Scotland Register of GSS Codes (April 2022) 
# Source: Scottish Government
# URL: https://www.gov.scot/publications/small-area-statistics-reference-materials
GET("https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/01/small-area-statistics-reference-materials/documents/scotland-register-of-gss-codes---july-2019/scotland-register-of-gss-codes---july-2019/govscot%3Adocument/GI-SAT%2BNaming%2BConvention-%2BScottish%2BRegister%2Bof%2BGeographic%2BCodes%2B%2528GSS%2529%2BApril%2B2022.xlsx", 
    write_disk(Scottish_Register <- tempfile(fileext = ".xlsx")))

# Extract GSS codes for each geography type ------------------------------------

# Countries
ctry_northern_ireland <- read_excel("RGC_March_2022_UK.xlsx", sheet = "N92_CTRY") %>% 
  select(AREACD = GEOGCD, AREANM = GEOGNM) %>% 
  mutate(Geography = "Country")

ctry_england <- read_excel("RGC_March_2022_UK.xlsx", sheet = "E92_CTRY") %>% 
  select(AREACD = GEOGCD, AREANM = GEOGNM) %>% 
  mutate(Geography = "Country")

ctry_scotland <- read_excel(Scottish_Register, sheet = "S92_CTRY") %>% 
  select(AREACD = InstanceCode, AREANM = InstanceName) %>% 
  mutate(Geography = "Country")

ctry_wales <- read_excel("RGC_March_2022_UK.xlsx", sheet = "W92_CTRY") %>% 
  select(AREACD = GEOGCD, AREANM = GEOGNM) %>% 
  mutate(Geography = "Country")

ctry <- bind_rows(ctry_northern_ireland, ctry_england, ctry_scotland, ctry_wales)

# Regions
rgn <- read_excel("RGC_March_2022_UK.xlsx", sheet = "E12_RGN") %>% 
  select(AREACD = GEOGCD, AREANM = GEOGNM) %>% 
  mutate(Geography = "Region")

# Counties
cty <- read_excel("RGC_March_2022_UK.xlsx", sheet = "E10_CTY") %>% 
  select(AREACD = GEOGCD, AREANM = GEOGNM) %>% 
  mutate(Geography = "County")

# Metropolitan Counties
mcty <- read_excel("RGC_March_2022_UK.xlsx", sheet = "E11_MCTY") %>% 
  select(AREACD = GEOGCD, AREANM = GEOGNM) %>% 
  mutate(Geography = "Metropolitan County")

# Unitary Authorities
ua_england <- read_excel("RGC_March_2022_UK.xlsx", sheet = "E06_UA") %>% 
  select(AREACD = GEOGCD, AREANM = GEOGNM) %>% 
  mutate(Geography = "Unitary Authority")

ua_wales <- read_excel("RGC_March_2022_UK.xlsx", sheet = "W06_UA") %>% 
  select(AREACD = GEOGCD, AREANM = GEOGNM) %>% 
  mutate(Geography = "Unitary Authority")

ua <- bind_rows(ua_england, ua_wales)

# Metropolitan Districts
md <- read_excel("RGC_March_2022_UK.xlsx", sheet = "E08_MD") %>% 
  select(AREACD = GEOGCD, AREANM = GEOGNM) %>% 
  mutate(Geography = "Metropolitan District")

# Non-Metropolitan Districts
nmd <- read_excel("RGC_March_2022_UK.xlsx", sheet = "E07_NMD") %>% 
  select(AREACD = GEOGCD, AREANM = GEOGNM) %>% 
  mutate(Geography = "Non-Metropolitan District")

# London Boroughs
londb <- read_excel("RGC_March_2022_UK.xlsx", sheet = "E09_LONB") %>% 
  select(AREACD = GEOGCD, AREANM = GEOGNM) %>% 
  mutate(Geography = "London Borough")

# Council Areas
ca <- read_excel(Scottish_Register, sheet = "S12_CA") %>% 
  select(AREACD = InstanceCode, AREANM = InstanceName) %>% 
  mutate(Geography = "Council Area")

# Local Government Districts
lgd <- read_excel("RGC_March_2022_UK.xlsx", sheet = "N09_LGD") %>% 
  select(AREACD = GEOGCD, AREANM = GEOGNM) %>% 
  mutate(Geography = "Local Government District")

# Combine geography types and write results ------------------------------------
bind_rows(ctry, rgn, cty, mcty, ua, md, nmd, londb, ca, lgd) %>% 
  write_csv("gss_codes.csv")
