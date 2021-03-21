# Script for filter raw RKI data +++++++++++++++++++++++++++++++++++++++++++
# Author: Kai Budde
# Created: 2021/03/21
# Last changed: 2021/03/21
# Version: 0.1.0


# Start --------------------------------------------------------------------

# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# User-defined parameters --------------------------------------------------
input_dir <- "../RKIdata"
# csv file downloaded from
# https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0
input_file <- "RKI_COVID19.csv"

district <- "LK Vorpommern-Greifswald"

# Load packages ------------------------------------------------------------
list.of.packages <- c("dplyr")

new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){
  install.packages(new.packages)
}
require("dplyr")

# Read csv and extract information -----------------------------------------

directory_of_R_script <- rstudioapi::getSourceEditorContext()$path
directory_of_R_script <- gsub(pattern = "filterRKIdata.R$",
                              replacement = "",
                              x = directory_of_R_script)
old_wd <- getwd()

setwd(directory_of_R_script)
setwd(input_dir)

# Load csv file
df_data <- read.csv(file = input_file, stringsAsFactors = FALSE, fileEncoding="UTF-8-BOM")

# Filter csv ---------------------------------------------------------------
df_filter <- filter(df_data, Landkreis == district)
df_filter$Meldedatum <- as.Date(df_filter$Meldedatum)
df_filter <- df_filter %>%
  group_by(Meldedatum)  %>%
  summarise(NewCases = sum(AnzahlFall))

# Save data output as csv --------------------------------------------------
file_name <- paste(district, "_CasesPerDay", sep = "")

write.csv(x = df_filter, file = paste(file_name, "_en.csv", sep=""),
          row.names = FALSE)
write.csv2(x = df_filter, file = paste(file_name, "_de.csv", sep=""),
          row.names = FALSE)

setwd(old_wd)
