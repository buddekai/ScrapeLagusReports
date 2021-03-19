# Script for scraping pdf reports by LAGuS MV ++++++++++++++++++++++++++++++
# Author: Kai Budde
# Created: 2021/03/19
# Last changed: 2021/03/19
# Version: 0.1.1

# Start --------------------------------------------------------------------

# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# User-defined parameters --------------------------------------------------
input_dir <- "../reports"

# Define name of output directory
output_text <- "text"
output_image <- "images"
output_analysis <- "analysis_results"

# Load packages ------------------------------------------------------------
list.of.packages <- c("tesseract", "pdftools", "magick")

new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){
  install.packages(new.packages)
}
require("tesseract")
require("pdftools")
require("magick")

if(is.na(match("deu", tesseract_info()$available))){
  tesseract_download("deu")
}

deu <- tesseract("deu")

# Read pdfs and extract information ----------------------------------------

directory_of_R_script <- rstudioapi::getSourceEditorContext()$path
directory_of_R_script <- gsub(pattern = "ScrapeLagusReports.R$",
                              replacement = "",
                              x = directory_of_R_script)
old_wd <- getwd()

setwd(directory_of_R_script)
setwd(input_dir)

# Get pdf file names
pdf_files <- list.files(path = paste(input_dir, "/", sep = ""))
pdf_files <- pdf_files[grepl(pattern = "\\.pdf", x = pdf_files)]
number_of_pdfs <- length(pdf_files)

districts_old <-   c("LK Ludwigslust-Parchim", "LK Mecklenb. Seenplatte",
                     "LK Nordwestmecklenburg", "LK Rostock",
                     "LK Vorpommern-Greifswald", "LK Vorpommern-Rügen",
                     "Rostock (Stadt)", "Schwerin (Stadt)")
districts_new <-   c("LUP", "MSE", "NWM", "LRO", "VG", "VR", "HRO", "SN", "MV")

# Initialize data frame
df_results <- data.frame(
  "Datum" = rep(NA, number_of_pdfs),
  "LUP" = rep(NA, number_of_pdfs),
  "MSE" = rep(NA, number_of_pdfs),
  "NWM" = rep(NA, number_of_pdfs),
  "LRO" = rep(NA, number_of_pdfs),
  "VG" = rep(NA, number_of_pdfs),
  "VR" = rep(NA, number_of_pdfs),
  "HRO" = rep(NA, number_of_pdfs),
  "SN" = rep(NA, number_of_pdfs),
  "MV" = rep(NA, number_of_pdfs)
)

# Go through every file and try to parse it
for(i in 1:number_of_pdfs){
  
  # Convert pdf (first page) to png ----------------------------------------
  current_pdf <- pdf_files[i]
  current_pdf_without_ending <- gsub(pattern ="\\.pdf", replacement = "", x = current_pdf)
  
  datum <- gsub(pattern = "..+\\.(.+)\\..+", replacement = "\\1", x = current_pdf_without_ending)
  
  current_pdf_as_png <- pdftools::pdf_convert(pdf = current_pdf,
                                              format = "png",
                                              pages = c(1),
                                              dpi = 300)
  
  
  # Crop image and save information of cumulative cases
  full_png <- magick::image_read(path = current_pdf_as_png)
  full_png_height <- as.numeric(magick::image_info(full_png)["height"])
  full_png_width <- as.numeric(magick::image_info(full_png)["width"])
  
  
  if(as.Date(datum)<as.Date("2020-04-06")){
    # First format
    
    # Define start and end position of image for ocr
    width_cum <- as.integer(0.04*full_png_width)
    height_cum <- as.integer(0.13*full_png_height)
    start_pos_width_cum <- as.integer(0.59*full_png_width)
    start_pos_height_cum <- as.integer(0.25*full_png_height)
    
    width_district <- as.integer(0.17*full_png_width)
    height_district <- as.integer(0.17*full_png_height)
    start_pos_width_district <- as.integer(0.43*full_png_width)
    start_pos_height_district <- as.integer(0.22*full_png_height)
    
    width_mv <- as.integer(0.2*full_png_width)
    height_mv <- as.integer(0.083*full_png_height)
    start_pos_width_mv <- as.integer(0.1*full_png_width)
    start_pos_height_mv <- as.integer(0.1*full_png_height)
    
  }else if(as.Date(datum)<as.Date("2020-04-30")){
    # Second format
    
    # Define start and end position of image for ocr
    width_cum <- as.integer(0.04*full_png_width)
    height_cum <- as.integer(0.14*full_png_height)
    start_pos_width_cum <- as.integer(0.59*full_png_width)
    start_pos_height_cum <- as.integer(0.31*full_png_height)
    
    width_district <- as.integer(0.17*full_png_width)
    height_district <- as.integer(0.14*full_png_height)
    start_pos_width_district <- as.integer(0.43*full_png_width)
    start_pos_height_district <- as.integer(0.31*full_png_height)
    
    width_mv <- as.integer(0.2*full_png_width)
    height_mv <- as.integer(0.05*full_png_height)
    start_pos_width_mv <- as.integer(0.2*full_png_width)
    start_pos_height_mv <- as.integer(0.1*full_png_height)
    
  }else if(as.Date(datum)<as.Date("2020-12-15")){
    # Third format
    
    # Define start and end position of image for ocr
    width_cum <- as.integer(0.05*full_png_width)
    height_cum <- as.integer(0.2*full_png_height)
    start_pos_width_cum <- as.integer(0.68*full_png_width)
    start_pos_height_cum <- as.integer(0.36*full_png_height)
    
    width_district <- as.integer(full_png_width/15)
    height_district <- as.integer(full_png_height/5)
    start_pos_width_district <- as.integer(0.48*full_png_width)
    start_pos_height_district <- as.integer(0.35*full_png_height)
    
    width_mv <- as.integer(0.5*full_png_width)
    height_mv <- as.integer(0.0167*full_png_height)
    start_pos_width_mv <- as.integer(0.5*full_png_width)
    start_pos_height_mv <- as.integer(0.51*full_png_height)
    
  }else{
    # Current format
    
    # Define start and end position of image for ocr
    width_cum <- as.integer(0.05*full_png_width)
    height_cum <- as.integer(0.2*full_png_height)
    start_pos_width_cum <- as.integer(0.68*full_png_width)
    start_pos_height_cum <- as.integer(0.35*full_png_height)
    
    width_district <- as.integer(full_png_width/15)
    height_district <- as.integer(full_png_height/5)
    start_pos_width_district <- as.integer(0.48*full_png_width)
    start_pos_height_district <- as.integer(0.35*full_png_height)
    
    width_mv <- as.integer(full_png_width/2)
    height_mv <- as.integer(full_png_height/60)
    start_pos_width_mv <- as.integer(0.5*full_png_width)
    start_pos_height_mv <- as.integer(0.51*full_png_height)
    
  }
  
  # Crop image and save information of cumulative cases
  cumulative_cases_png <- image_crop(image = full_png,
                                     geometry = paste(
                                       width_cum,"x",
                                       height_cum, "+",
                                       start_pos_width_cum,
                                       "+",
                                       start_pos_height_cum,
                                       sep=""))
  #print(cumulative_cases_png)
  
  # Crop image and save information of city/district
  district_names_png <- image_crop(image = full_png,
                                   geometry = paste(
                                     width_district,"x",
                                     height_district, "+",
                                     start_pos_width_district,
                                     "+",
                                     start_pos_height_district,
                                     sep=""))
  #print(district_names_png)
  
  # Crop image and only save information of MV total
  mv_total_png <- image_crop(image = full_png,
                             geometry = paste(
                               width_mv,"x",
                               height_mv, "+",
                               start_pos_width_mv,
                               "+",
                               start_pos_height_mv,
                               sep=""))
  #print(mv_total_png)
  
  
  # Delete png file
  if(file.remove(current_pdf_as_png)){
    print(paste("File ", current_pdf_as_png, " successfully removed.", sep=""))
  }
  
  
  # Extract information ----------------------------------------------------
  
  df_results$Datum[i] <- datum
  
  numbers <- tesseract(options = list(tessedit_char_whitelist = "0123456789"))
  numbers_from_png <- tesseract::ocr(image = cumulative_cases_png, engine = numbers)
  numbers_from_png <- unlist(strsplit(x = numbers_from_png, split = "\n"))
  numbers_from_png <- suppressWarnings(as.integer(numbers_from_png))
  numbers_from_png <- numbers_from_png[!is.na(numbers_from_png)]
  
  if(length(numbers_from_png) == 9){
    numbers_from_png <- numbers_from_png[-length(numbers_from_png)]
  }
  if(length(numbers_from_png) != 8){
    print("Something went wrong with the conversion of the case numbers.")
  }
  
  
  for(j in 1:length(numbers_from_png)){
    df_results[i,(j+1)] <- numbers_from_png[j]
  }
  rm(j)
  
  df_results$MV[i] <- sum(df_results[i, 2:9])
  
  
  districts_from_png <- tesseract::ocr(image = district_names_png, engine = deu)
  districts_from_png <- unlist(strsplit(x = districts_from_png, split = "\n"))
  if(as.Date(datum)<as.Date("2020-05-01")){
    # Old format
    districts_from_png <- districts_from_png[districts_from_png %in% districts_old]
    
    for(j in 1:length(districts_from_png)){
      if(districts_from_png[j] != districts_old[j]){
        print("Something went wrong with the district names.")
        print(paste(districts_from_png[j], " != ", districts[j], sep=""))
      }
    }
    
    rm(j)
    
  }else{
    # New format
    
    # Remove all non upper-case letter
    districts_from_png <- gsub(pattern = "[^A-Z]", replacement = "", x = districts_from_png)
    districts_from_png <- districts_from_png[districts_from_png %in% districts_new]
    
    for(j in 1:length(districts_from_png)){
      if(districts_from_png[j] != districts_new[j]){
        print("Something went wrong with the district names.")
        print(paste(districts_from_png[j], " != ", districts[j], sep=""))
      }
    }  
    rm(j)
  }
  
  # Save texts in file
  dir.create(output_text, showWarnings = FALSE)
  
  utils::write.table(x = numbers_from_png,
                     file = paste(output_text, "/", current_pdf_without_ending,
                                  "_kumulative_Faelle.txt", sep = ""),
                     sep = "", row.names = FALSE,
                     col.names = FALSE, quote = FALSE)
  
  # Save png files for validation
  dir.create(output_image, showWarnings = FALSE)
  
  magick::image_write(image = mv_total_png,
                      path = paste(output_image, "/", current_pdf_without_ending,
                                   "_MV_gesamt.png", sep = ""))
  
}

# Save data frame in file
dir.create(output_analysis, showWarnings = FALSE)
write.csv(x = df_results, file = paste(
  output_analysis, "/cumulative_cases_en.csv", sep = ""), row.names = FALSE)
write.csv2(x = df_results, file = paste(
  output_analysis, "/cumulative_cases_de.csv", sep = ""), row.names = FALSE)

setwd(old_wd)
