
################################################################################
###########Generate the middle sections of the codebook #######################
############# The Migration Policy database (MGIPOL) ###########################
################################################################################

# This R script generates the middle sections of the codebook
# for the Migration Policy database (MIGPOL). It uses LaTex code
# and TinyTex as a compiler. The last part of the code calles
# an R script called "migpol-cb-assemble.R" which combines the
# sections of the codebook with "migpol-cb-intro.tex" and 
# "migpol-cb-outro.tex".The lines that follow save the final tex 
# file generate codebook as a pdf. Any changes to the tex environment need 
# to be made in the "intro.tex" file. Changes to the bibliography
# need to be made in the "outro.tex" file. Individual references
# can be changed in the "references.bib" file.

#Required files:
#   1) migpol-cb-intro.tex
#   2) MIGPOL.xlsx
#   3) migpol-cb-outro.tex
#   4) references.bib

#Author: Lutz Gschwind
#Last changed: 2023-08-25


##########################################################
#Load libraries

library("tidyverse") # load dplyr, ggplot2, stringr
library("sf") # working with geographic simple features in R
library("rnaturalearth") # world map data from Natural Earth
library("countrycode") # get ISO code from country names
library("readxl") # load excel files
library("tools") # compile tex files
library("sf") # working with geographic simple features in R
library("rnaturalearth") # world map data from Natural Earth
library("countrycode") # get ISO code from country names
library("stringr") # edit strings of dataframe cells

#Clean workspace
rm(list=ls())

#Loading projects as individual dataframes and change elements
MIPEX <- read_excel("MIND.xlsx", sheet = "MIPEX")
GLOBALCIT_ACQ <-read_excel("MIND.xlsx", sheet = "GLOBALCIT_ACQ")
GLOBALCIT_Country_Year<- read_excel("MIND.xlsx", sheet = "GLOBALCIT_Country_Year")
GLOBALCIT_LOSS <-read_excel("MIND.xlsx", sheet = "GLOBALCIT_LOSS")
IMPIC_RawData <- read_excel("MIND.xlsx", sheet = "IMPIC_RawData")
IMPIC_2024 <- read_excel("MIND.xlsx", sheet = "IMPIC_2024")
IMPIC_Politcal_Rights <- read_excel("MIND.xlsx", sheet = "IMPIC_Political_Rights")
IMPIC_Antidiscrimination <- read_excel("MIND.xlsx", sheet = "IMPIC_Antidiscrimination")
IMPIC_Antidiscrimination_RawData <- read_excel("MIND.xlsx", sheet = "IMPIC_Antidiscrimination_RawDat")
DEMIG_QUANTMIG <- read_excel("MIND.xlsx", sheet = "DEMIG_QUANTMIG")
IMISEM <- read_excel("MIND.xlsx", sheet = "IMISEM")
HIP <- read_excel("MIND.xlsx", sheet = "HIP")
IMMIGSR <- read_excel("MIND.xlsx", sheet = "IMMIGSR")


#Translations to make expressions work in LaTex
##Note: "\\\\" translates to "\" in the dataframe
substitute_characters <- function(data_frame) {
  substitutions <- function(x) {
    x <- gsub("&", "\\\\&", x)
    x <- gsub(">", "$>$", x)
    x <- gsub("<", "$<$", x)
    x <- gsub("≤", "$\\\\leq$", x)
    x <- gsub("≥", "$\\\\geq$", x)
    x <- gsub("\n", "\\\\\\\\", x)
    x <- gsub("%", "\\\\%", x)
    return(x)
  }
  
  data_frame <- data_frame %>%
    mutate_all(~ substitutions(.))
  
  return(data_frame)
}

# Apply the substitutions to all data frames
DEMIG_QUANTMIG <- substitute_characters(DEMIG_QUANTMIG)
GLOBALCIT_ACQ <- substitute_characters(GLOBALCIT_ACQ)
GLOBALCIT_Country_Year <- substitute_characters(GLOBALCIT_Country_Year)
GLOBALCIT_LOSS <- substitute_characters(GLOBALCIT_LOSS)
HIP <- substitute_characters(HIP)
IMMIGSR <- substitute_characters(IMMIGSR)
IMISEM <- substitute_characters(IMISEM)
IMPIC_2024 <- substitute_characters(IMPIC_2024)
IMPIC_Antidiscrimination <- substitute_characters(IMPIC_Antidiscrimination)
IMPIC_Antidiscrimination_RawData <- substitute_characters(IMPIC_Antidiscrimination_RawData)
IMPIC_Politcal_Rights <- substitute_characters(IMPIC_Politcal_Rights)
IMPIC_RawData <- substitute_characters(IMPIC_RawData)
MIPEX <- substitute_characters(MIPEX)

#Loading references
ref <- read_excel("references.xlsx")

################################################
## Generate functions to retain text elements ##
################################################
description <- function(data_frame, variable_name) {
  target <- filter(data_frame, Code == variable_name) 
  target <- merge(x = target, y = ref, by = "Dataset") 
  
  # Generate the map file name based on dataframe name and code value
  map_file_name <- paste0("MAPS/", target$Dataset, "_", target$Code, ".png")
  
  # Handle "Stem" and "Branch" types
  if (target$Type[1] %in% c("Stem", "Branch")) {
    return(paste0(
      "\\begin{tabular}{p{0.95\\textwidth}} \n",
      "\\textbf{Description}: \\\\ \n",
      target$Description," \\\\ \n",
      "\\end{tabular} \n",
      "\\bigskip \n"
    ))
  }
  
  # Handle other types
  paste0(
    "\\begin{tabular}{p{0.95\\textwidth}} \n",
    "\\textbf{Code}: \\\\ \n",
    tolower(target$Dataset[1]), "_", tolower(target$Code[1]), "\\\\ \n",
    "\\\\ \n",
    "\\textbf{Description}: \\\\ \n",
    target$Description," \\\\ \n",
    "\\\\ \n",
    "\\textbf{Values}: \\\\ \n",
    target$Values," \\\\ \n",
    "\\\\ \n",
    "\\end{tabular} \n",
    " \n",
    "\\begin{tabular}{p{0.4\\textwidth}p{0.4\\textwidth}} \n",
    "\\textbf{Missings}: & \\multirow{8}{*}[8mm]{\\includegraphics[width=0.5\\textwidth]{", map_file_name, "}}  \\\\ \n",
    target$Missings, "& \\\\ \n",
    "\\\\ \n",
    "\\textbf{Coverage}: & \\\\ \n",
    target$Coverage, " & \\\\ \n",
    "\\\\ \n",
    "\\textbf{Citation}: &  \\\\ \n",
    "\\cite{", target$Citation, "} &  \\\\ \n",
    "\\end{tabular} \n",
    "\\bigskip \n"
  )
}

hierarical_structure <- function(data_frame, variable_name) {
  target <- filter(data_frame, Code == variable_name)
  description_text <- ""
  
  # Check if target$Type is one of "Index," "Indicator," "Item," or "Subitem"
  if (target$Type[1] %in% c("Index", "Stem", "Indicator", "Branch", "Item",  "Subitem")) {
    description_text <- description(data_frame, variable_name)
  }
  
  if (target$Type[1] == "Index") {
    return(paste0(
      "\\subsection{", target$Title[1], "}\n",
      description_text
    ))
  } else if (target$Type[1] == "Stem") {
    return(paste0(
      "\\subsection{", target$Title[1], "}\n",
      description_text
    ))
  } else if (target$Type[1] == "Indicator") {
    return(paste0(
      "\\subsubsection{", target$Title[1], "}\n",
      description_text
    ))
  } else if (target$Type[1] == "Branch") {
    return(paste0(
      "\\subsubsection{", target$Title[1], "}\n",
      description_text
    ))
  } else if (target$Type[1] == "Item") {
    return(paste0(
      "\\subsubsubsection{", target$Title[1], "}\n",
      description_text
    ))
  } else if (target$Type[1] == "Leaf") {
    return(paste0(
      "\\subsubsubsection{", target$Title[1], "}\n",
      description_text
    ))
  } else if (target$Type[1] == "Subitem") {
    return(paste0(
      "\\subsubsubsubsection{", target$Title[1], "}\n",
      description_text
    ))
  } else {
    return(NULL)  # Handle unsupported 'Type' values here
  }
}


############################################################
######################  demig_policy  ######################
############################################################
# Get a vector of unique variable names from the 'Code' column
variable_names <- unique(DEMIG_QUANTMIG$Code)
# Initialize a list to store the results
results_list_demig_quantmig <- list()
# Loop through the variable names and apply the function
for (variable_name in variable_names) {
  result <- hierarical_structure(DEMIG_QUANTMIG, variable_name)
  results_list_demig_quantmig[[variable_name]] <- result
}

# Define the file path where you want to save the LaTeX file
latex_file_path <- "DEMIG_QUANTMIG-cb-section.tex"  # Replace with your desired file path
# Open the LaTeX file for writing
file_conn <- file(latex_file_path, "w")
# Loop through the results and write them to the LaTeX file
for (variable_name in variable_names) {
  cat("\n", file = file_conn)
  if (!is.null(results_list_demig_quantmig[[variable_name]])) {
    cat(results_list_demig_quantmig[[variable_name]], file = file_conn)
  } else {
    cat("No heading for the specified type.", file = file_conn)
  }
  cat("\n\n", file = file_conn)
}
# Close the LaTeX file
close(file_conn)

############################################################
######################globalcit_acq######################
############################################################
# Get a vector of unique variable names from the 'Code' column
variable_names <- unique(GLOBALCIT_ACQ$Code)
# Initialize a list to store the results
results_list_GLOBALCIT_ACQ <- list()
# Loop through the variable names and apply the function
for (variable_name in variable_names) {
  result <- hierarical_structure(GLOBALCIT_ACQ, variable_name)
  results_list_GLOBALCIT_ACQ[[variable_name]] <- result
}

# Define the file path where you want to save the LaTeX file
latex_file_path <- "GLOBALCIT_ACQ-cb-section.tex"  # Replace with your desired file path
# Open the LaTeX file for writing
file_conn <- file(latex_file_path, "w")
# Loop through the results and write them to the LaTeX file
for (variable_name in variable_names) {
  cat("Variable Name:", variable_name, "\n", file = file_conn)
  if (!is.null(results_list_GLOBALCIT_ACQ[[variable_name]])) {
    cat(results_list_GLOBALCIT_ACQ[[variable_name]], file = file_conn)
  } else {
    cat("No heading for the specified type.", file = file_conn)
  }
  cat("\n\n", file = file_conn)
}
# Close the LaTeX file
close(file_conn)

############################################################
##################globalcit_country_year####################
############################################################
# Get a vector of unique variable names from the 'Code' column
variable_names <- unique(GLOBALCIT_Country_Year$Code)
# Initialize a list to store the results
results_list_GLOBALCIT_Country_Year <- list()
# Loop through the variable names and apply the function
for (variable_name in variable_names) {
  result <- hierarical_structure(GLOBALCIT_Country_Year, variable_name)
  results_list_GLOBALCIT_Country_Year[[variable_name]] <- result
}

# Define the file path where you want to save the LaTeX file
latex_file_path <- "GLOBALCIT_Country_Year-cb-section.tex"  # Replace with your desired file path
# Open the LaTeX file for writing
file_conn <- file(latex_file_path, "w")
# Loop through the results and write them to the LaTeX file
for (variable_name in variable_names) {
  cat("\n", file = file_conn)
  if (!is.null(results_list_GLOBALCIT_Country_Year[[variable_name]])) {
    cat(results_list_GLOBALCIT_Country_Year[[variable_name]], file = file_conn)
  } else {
    cat("No heading for the specified type.", file = file_conn)
  }
  cat("\n\n", file = file_conn)
}
# Close the LaTeX file
close(file_conn)

############################################################
#######################globalcit_LOSS#######################
############################################################
# Get a vector of unique variable names from the 'Code' column
variable_names <- unique(GLOBALCIT_LOSS$Code)
# Initialize a list to store the results
results_list_globalcit_loss <- list()
# Loop through the variable names and apply the function
for (variable_name in variable_names) {
  result <- hierarical_structure(GLOBALCIT_LOSS, variable_name)
  results_list_globalcit_loss[[variable_name]] <- result
}

# Define the file path where you want to save the LaTeX file
latex_file_path <- "GLOBALCIT_LOSS-cb-section.tex"  # Replace with your desired file path
# Open the LaTeX file for writing
file_conn <- file(latex_file_path, "w")
# Loop through the results and write them to the LaTeX file
for (variable_name in variable_names) {
  cat("\n", file = file_conn)
  if (!is.null(results_list_globalcit_loss[[variable_name]])) {
    cat(results_list_globalcit_loss[[variable_name]], file = file_conn)
  } else {
    cat("No heading for the specified type.", file = file_conn)
  }
  cat("\n\n", file = file_conn)
}
# Close the LaTeX file
close(file_conn)

############################################################
######################      imisem    ######################
############################################################
# Get a vector of unique variable names from the 'Code' column
variable_names <- unique(IMISEM$Code)
# Initialize a list to store the results
results_list_imisem <- list()
# Loop through the variable names and apply the function
for (variable_name in variable_names) {
  result <- hierarical_structure(IMISEM, variable_name)
  results_list_imisem[[variable_name]] <- result
}

# Define the file path where you want to save the LaTeX file
latex_file_path <- "IMISEM-cb-section.tex"  # Replace with your desired file path
# Open the LaTeX file for writing
file_conn <- file(latex_file_path, "w")
# Loop through the results and write them to the LaTeX file
for (variable_name in variable_names) {
  cat("\n", file = file_conn)
  if (!is.null(results_list_imisem[[variable_name]])) {
    cat(results_list_imisem[[variable_name]], file = file_conn)
  } else {
    cat("No heading for the specified type.", file = file_conn)
  }
  cat("\n\n", file = file_conn)
}
# Close the LaTeX file
close(file_conn)

xyz <- colnames(IMISEM)
print(xyz)

############################################################
######################      ImmigSR    ######################
############################################################
# Get a vector of unique variable names from the 'Code' column
variable_names <- unique(IMMIGSR$Code)
# Initialize a list to store the results
results_list_immigsr <- list()
# Loop through the variable names and apply the function
for (variable_name in variable_names) {
  result <- hierarical_structure(IMMIGSR, variable_name)
  results_list_immigsr[[variable_name]] <- result
}

# Define the file path where you want to save the LaTeX file
latex_file_path <- "IMMIGSR-cb-section.tex"  # Replace with your desired file path
# Open the LaTeX file for writing
file_conn <- file(latex_file_path, "w")
# Loop through the results and write them to the LaTeX file
for (variable_name in variable_names) {
  cat("\n", file = file_conn)
  if (!is.null(results_list_immigsr[[variable_name]])) {
    cat(results_list_immigsr[[variable_name]], file = file_conn)
  } else {
    cat("No heading for the specified type.", file = file_conn)
  }
  cat("\n\n", file = file_conn)
}
# Close the LaTeX file
close(file_conn)

xyz <- colnames(IMMIGSR)
print(xyz)

############################################################
######################   impic_2024    #####################
############################################################
# Get a vector of unique variable names from the 'Code' column
variable_names <- unique(IMPIC_2024$Code)
# Initialize a list to store the results
results_list_impic_2024 <- list()
# Loop through the variable names and apply the function
for (variable_name in variable_names) {
  result <- hierarical_structure(IMPIC_2024, variable_name)
  results_list_impic_2024[[variable_name]] <- result
}

# Define the file path where you want to save the LaTeX file
latex_file_path <- "IMPIC_2024-cb-section.tex"  # Replace with your desired file path
# Open the LaTeX file for writing
file_conn <- file(latex_file_path, "w")
# Loop through the results and write them to the LaTeX file
for (variable_name in variable_names) {
  cat("\n", file = file_conn)
  if (!is.null(results_list_impic_2024[[variable_name]])) {
    cat(results_list_impic_2024[[variable_name]], file = file_conn)
  } else {
    cat("No heading for the specified type.", file = file_conn)
  }
  cat("\n\n", file = file_conn)
}
# Close the LaTeX file
close(file_conn)


############################################################
######################   impic_antidiscrimination    #####################
############################################################
# Get a vector of unique variable names from the 'Code' column
variable_names <- unique(IMPIC_Antidiscrimination$Code)
# Initialize a list to store the results
results_list_impic_Antidiscrimination <- list()
# Loop through the variable names and apply the function
for (variable_name in variable_names) {
  result <- hierarical_structure(IMPIC_Antidiscrimination, variable_name)
  results_list_impic_Antidiscrimination[[variable_name]] <- result
}

# Define the file path where you want to save the LaTeX file
latex_file_path <- "IMPIC_Antidiscrimination-cb-section.tex"  # Replace with your desired file path
# Open the LaTeX file for writing
file_conn <- file(latex_file_path, "w")
# Loop through the results and write them to the LaTeX file
for (variable_name in variable_names) {
  cat("\n", file = file_conn)
  if (!is.null(results_list_impic_Antidiscrimination[[variable_name]])) {
    cat(results_list_impic_Antidiscrimination[[variable_name]], file = file_conn)
  } else {
    cat("No heading for the specified type.", file = file_conn)
  }
  cat("\n\n", file = file_conn)
}
# Close the LaTeX file
close(file_conn)

############################################################
######################   impic_antidiscrimination    #####################
############################################################
# Get a vector of unique variable names from the 'Code' column
variable_names <- unique(IMPIC_Antidiscrimination_RawData$Code)
# Initialize a list to store the results
results_list_impic_Antidiscrimination_RawData <- list()
# Loop through the variable names and apply the function
for (variable_name in variable_names) {
  result <- hierarical_structure(IMPIC_Antidiscrimination_RawData, variable_name)
  results_list_impic_Antidiscrimination_RawData[[variable_name]] <- result
}

# Define the file path where you want to save the LaTeX file
latex_file_path <- "IMPIC_Antidiscrimination_RawData-cb-section.tex"  # Replace with your desired file path
# Open the LaTeX file for writing
file_conn <- file(latex_file_path, "w")
# Loop through the results and write them to the LaTeX file
for (variable_name in variable_names) {
  cat("\n", file = file_conn)
  if (!is.null(results_list_impic_Antidiscrimination_RawData[[variable_name]])) {
    cat(results_list_impic_Antidiscrimination_RawData[[variable_name]], file = file_conn)
  } else {
    cat("No heading for the specified type.", file = file_conn)
  }
  cat("\n\n", file = file_conn)
}
# Close the LaTeX file
close(file_conn)


############################################################
######################    impic_raw    #####################
############################################################
# Get a vector of unique variable names from the 'Code' column
variable_names <- unique(IMPIC_RawData$Code)
# Initialize a list to store the results
results_list_IMPIC_RawData <- list()
# Loop through the variable names and apply the function
for (variable_name in variable_names) {
  result <- hierarical_structure(IMPIC_RawData, variable_name)
  results_list_IMPIC_RawData[[variable_name]] <- result
}

# Define the file path where you want to save the LaTeX file
latex_file_path <- "IMPIC_RawData-cb-section.tex"  # Replace with your desired file path
# Open the LaTeX file for writing
file_conn <- file(latex_file_path, "w")
# Loop through the results and write them to the LaTeX file
for (variable_name in variable_names) {
  cat("\n", file = file_conn)
  if (!is.null(results_list_IMPIC_RawData[[variable_name]])) {
    cat(results_list_IMPIC_RawData[[variable_name]], file = file_conn)
  } else {
    cat("No heading for the specified type.", file = file_conn)
  }
  cat("\n\n", file = file_conn)
}
# Close the LaTeX file
close(file_conn)


############################################################
#################  impic_political_rights  #################
############################################################
# Get a vector of unique variable names from the 'Code' column
variable_names <- unique(IMPIC_Politcal_Rights$Code)
# Initialize a list to store the results
results_list_IMPIC_Politcal_Rights <- list()
# Loop through the variable names and apply the function
for (variable_name in variable_names) {
  result <- hierarical_structure(IMPIC_Politcal_Rights, variable_name)
  results_list_IMPIC_Politcal_Rights[[variable_name]] <- result
}

# Define the file path where you want to save the LaTeX file
latex_file_path <- "IMPIC_Politcal_Rights-cb-section.tex"  # Replace with your desired file path
# Open the LaTeX file for writing
file_conn <- file(latex_file_path, "w")
# Loop through the results and write them to the LaTeX file
for (variable_name in variable_names) {
  cat("\n", file = file_conn)
  if (!is.null(results_list_IMPIC_Politcal_Rights[[variable_name]])) {
    cat(results_list_IMPIC_Politcal_Rights[[variable_name]], file = file_conn)
  } else {
    cat("No heading for the specified type.", file = file_conn)
  }
  cat("\n\n", file = file_conn)
}
# Close the LaTeX file
close(file_conn)


############################################################
#######################   MIPEX   ##########################
############################################################
# Get a vector of unique variable names from the 'Code' column
variable_names <- unique(MIPEX$Code)
# Initialize a list to store the results
results_list_mipex <- list()
# Loop through the variable names and apply the function
for (variable_name in variable_names) {
  result <- hierarical_structure(MIPEX, variable_name)
  results_list_mipex[[variable_name]] <- result
}

# Define the file path where you want to save the LaTeX file
latex_file_path <- "MIPEX-cb-section.tex"  # Replace with your desired file path
# Open the LaTeX file for writing
file_conn <- file(latex_file_path, "w")
# Loop through the results and write them to the LaTeX file
for (variable_name in variable_names) {
  cat("\n", file = file_conn)
  if (!is.null(results_list_mipex[[variable_name]])) {
    cat(results_list_mipex[[variable_name]], file = file_conn)
  } else {
    cat("No heading for the specified type.", file = file_conn)
  }
  cat("\n\n", file = file_conn)
}
# Close the LaTeX file
close(file_conn)


############################################################
#######################   MIPEX   ##########################
############################################################
# Get a vector of unique variable names from the 'Code' column
variable_names <- unique(HIP$Code)
# Initialize a list to store the results
results_list_hip <- list()
# Loop through the variable names and apply the function
for (variable_name in variable_names) {
  result <- hierarical_structure(HIP, variable_name)
  results_list_hip[[variable_name]] <- result
}

# Define the file path where you want to save the LaTeX file
latex_file_path <- "HIP-cb-section.tex"  # Replace with your desired file path
# Open the LaTeX file for writing
file_conn <- file(latex_file_path, "w")
# Loop through the results and write them to the LaTeX file
for (variable_name in variable_names) {
  cat("\n", file = file_conn)
  if (!is.null(results_list_hip[[variable_name]])) {
    cat(results_list_hip[[variable_name]], file = file_conn)
  } else {
    cat("No heading for the specified type.", file = file_conn)
  }
  cat("\n\n", file = file_conn)
}
# Close the LaTeX file
close(file_conn)



# List of the individual .tex files in the desired order
tex_files <- c("migpol-cb-intro.tex",
               "DEMIG_QuantMig.tex", "DEMIG_QUANTMIG-cb-section.tex", 
               "GLOBALCIT_CITLAW_Country_Year_introduction.tex", "GLOBALCIT_Country_Year-cb-section.tex",
               "HIP_introduction.tex", "HIP-cb-section.tex",
               "IMISEM_introduction.tex", "imisem-cb-section.tex",
               "IMMIGSR-cb-section.tex", "IMMIGSR_introduction.tex",
               "IMPIC_2024_introduction.tex", "IMPIC_2024-cb-section.tex",
               "IMPIC_Antidiscrimination_introduction.tex", "IMPIC_Antidiscrimination-cb-section.tex",
               "IMPIC_Antidiscrimination_RawData_introduction.tex", "IMPIC_Antidiscrimination_RawData-cb-section.tex",
               "IMPIC_RawData_introduction.tex", "IMPIC_RawData-cb-section.tex",
               "IMPIC_Political_Rights_introduction.tex", "IMPIC_Politcal_Rights-cb-section.tex",
               "MIPEX_introduction.tex", "mipex-cb-section.tex", 
               "migpol-cb-outro.tex")

# Name of the output assembled .tex file
output_tex_file <- "migpol_Codebook.tex"

# Open the output .tex file for writing
cat("", file = output_tex_file)

# Iterate through the list of .tex files and append their contents to the output file
for (tex_file in tex_files) {
  tex_content <- readLines(tex_file)
  cat(tex_content, file = output_tex_file, sep = "\n", append = TRUE)
}




















