#### Subnational Statistics Revised_data.csv to machine readable R Script ####

# 24th August 2022
  # just for August 2022 iteration. Future iterations pull .csv files from D:/Coding_Repos/LUDA/Output directly

# remove all data and clear environment

rm(list = ls())


#### Import revised_data.csv ####

setwd("D:/Coding_Repos/subnational-indicators/app")

filename <- "revised_data.csv"

data <-read.csv(filename)


#### Rename and Select Columns for machine_readable csv File ####

csv_output <- rename(data, AREACD = ï..unique,
                     AREANM = group,
                     Unit = unit,
                     Value = real,
                     MAD = value) %>% 
  select(all_of(c("AREACD",
                  "AREANM",
                  "Geography",
                  "Indicator",
                  "Category",
                  "Period",
                  "Measure",
                  "Unit",
                  "Value",
                  "MAD")))


#### Set up working directory structure for output ####

setwd("D:/Coding_Repos/subnational-indicators/app")

date <- Sys.Date()


#### Write output ####

cat("There may be some discrepancies between this data download and the accompanying dataset caused by rounding issues but these should be insignificant and are unlikely to affect any further analysis\n\n", file = paste0(date, "_machine_readable.csv"))
write_excel_csv(csv_output, paste0(date, "_machine_readable.csv"), append = TRUE, col_names = TRUE)
