# LA_Analytical_Detective
# https://data.lacity.org/Public-Safety/Crime-Data-from-2020-to-Present/2nrs-mtv8/data_preview
LA_Analytical_Detective_URL <- "https://data.lacity.org/Public-Safety/Crime-Data-from-2020-to-Present/2nrs-mtv8/data_preview"
cat(LA_Analytical_Detective_URL) 

setwd("D:/Coding_Exercises/R/The_Analytics_Edge/Assignment_1/LA_Analytical_Detective/2024")
getwd()
wrkdir <- getwd()
wrkdir
list.files()

# P 1.1 Loading the Data
# reading the file
LA_2024_02_Feb <- read.csv("2024_02_Feb.csv", stringsAsFactors = FALSE)
LA_2024_02_Feb
str(LA_2024_02_Feb) 
head(LA_2024_02_Feb)
tail(LA_2024_02_Feb)
summary(LA_2024_02_Feb)

# e.1 How many rows of data (observations) are in this dataset?
num_obs <- nrow(LA_2024_02_Feb) #1.1 to see number of row
num_obs
cat("There are", num_obs, "rows of data (observations) in the dataset.")


# 1.2 how many variables are there in this dataset?
num_var <- ncol(LA_2024_02_Feb)
num_var
cat("There are", num_var, "variables in this dataset.")

#1.2a ow many observations are there in this dataset?
num_obs <-nrow(LA_2024_02_Feb)
num_obs
cat("There are", num_obs, "observations in this dataset.")

# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")


# list all the AREA.NAME names
unique(LA_2024_02_Feb$AREA.NAME)
length(unique(LA_2024_02_Feb$AREA.NAME))
table(LA_2024_02_Feb$AREA.NAME)
AREA.NAME_2024_02_Feb <- table(LA_2024_02_Feb$AREA.NAME)
AREA.NAME_2024_02_Feb

# Max AREA_NAME
max_AREA.NAME_2024_02_Feb <- names(which.max(AREA.NAME_2024_02_Feb))
max_AREA.NAME_2024_02_Feb
max_count_AREA.NAME_2024_02_Feb <- max(AREA.NAME_2024_02_Feb)
max_count_AREA.NAME_2024_02_Feb

# Min AREA_NAME
min_AREA.NAME_2024_02_Feb <- names(which.min(AREA.NAME_2024_02_Feb))
min_AREA.NAME_2024_02_Feb
min_count_AREA.NAME_2024_02_Feb <- min(AREA.NAME_2024_02_Feb)
min_count_AREA.NAME_2024_02_Feb

cat("AREA Most incidents:\n", max_AREA.NAME_2024_02_Feb, "with", max_count_AREA.NAME_2024_02_Feb, "incidents for 2024_02_Feb\n\n")
cat("AREA Fewest incidents:\n", min_AREA.NAME_2024_02_Feb, "with", min_count_AREA.NAME_2024_02_Feb, "incidents for 2024_02_Feb\n\n")

# Sort AREA.name in decreasing order
Sorted_AREA.NAME_2024_02_Feb <- sort(AREA.NAME_2024_02_Feb, decreasing = TRUE)
head(Sorted_AREA.NAME_2024_02_Feb)
tail(Sorted_AREA.NAME_2024_02_Feb)

# Get top 3 most frequent AREA_NAME
top_3_Sorted_AREA.NAME_2024_02_Feb <- head(Sorted_AREA.NAME_2024_02_Feb, 3)
top_3_Sorted_AREA.NAME_2024_02_Feb

# Extract SOrted AREA_NAME, names and counts
top_3_Sorted_AREA.NAME_names_2024_02_Feb <- names(top_3_Sorted_AREA.NAME_2024_02_Feb)
top_3_Sorted_AREA.NAME_names_2024_02_Feb
top_3_Sorted_AREA.NAME_counts_2024_02_Feb <- as.numeric(top_3_Sorted_AREA.NAME_2024_02_Feb)
top_3_Sorted_AREA.NAME_counts_2024_02_Feb

# Print summary sentence
cat(paste0(
  top_3_Sorted_AREA.NAME_names_2024_02_Feb[1], " had the most incidents for 2024_02_Feb with ", top_3_Sorted_AREA.NAME_counts_2024_02_Feb[1], " cases; ",
  top_3_Sorted_AREA.NAME_names_2024_02_Feb[2], " followed with ", top_3_Sorted_AREA.NAME_counts_2024_02_Feb[2], " cases; and ",
  top_3_Sorted_AREA.NAME_names_2024_02_Feb[3], " had ", top_3_Sorted_AREA.NAME_counts_2024_02_Feb[3], " cases."
))



###########################
# Filter dataset for "Central"
Central_2024_02_Feb_table <- subset(LA_2024_02_Feb, AREA.NAME == "Central", select = c("AREA.NAME","Crm.Cd.Desc", "LOCATION", "Cross.Street", "Premis.Desc", "TIME.OCC", "DATE.OCC", "Vict.Age", "Vict.Sex", "Vict.Descent"))
head(Central_2024_02_Feb_table)
tail(Central_2024_02_Feb_table)

# list all the Crm.Cd.Desc for AREA.NAME
unique(Central_2024_02_Feb_table$Crm.Cd.Desc)
length(unique(Central_2024_02_Feb_table$Crm.Cd.Desc))
Central_Crm.Cd.Desc_2024_02_Feb_table <- table(Central_2024_02_Feb_table$Crm.Cd.Desc)
head(Central_Crm.Cd.Desc_2024_02_Feb_table)

# Max Central_Crm.Cd.Desc
max_Central_Crm.Cd.Desc_2024_02_Feb_name <- names(which.max(Central_Crm.Cd.Desc_2024_02_Feb_table))
max_Central_Crm.Cd.Desc_2024_02_Feb_name
max_Central_Crm.Cd.Desc_2024_02_Feb_count <- max(Central_Crm.Cd.Desc_2024_02_Feb_table)
max_Central_Crm.Cd.Desc_2024_02_Feb_count

# Min Central_Crm.Cd.Desc
min_Central_Crm.Cd.Desc_2024_02_Feb_name <- names(which.min(Central_Crm.Cd.Desc_2024_02_Feb_table))
min_Central_Crm.Cd.Desc_2024_02_Feb_name
min_Central_Crm.Cd.Desc_2024_02_Feb_count <- min(Central_Crm.Cd.Desc_2024_02_Feb_table)
min_Central_Crm.Cd.Desc_2024_02_Feb_count

cat("Central Most incidents: ", max_Central_Crm.Cd.Desc_2024_02_Feb_name, "with", max_Central_Crm.Cd.Desc_2024_02_Feb_count, "incidents for Feb 2024 ")
cat("Central Fewest incidents: ", min_Central_Crm.Cd.Desc_2024_02_Feb_name, "with", min_Central_Crm.Cd.Desc_2024_02_Feb_count, "incidents Feb 2024")


### top 3 sorted for Crm.Cd.Desc in the Central Area
# Sort the Crm.Cd.Desc frequency table in descending order
Sorted_Central_Crm.Cd.Desc_2024_02_Feb <- sort(Central_Crm.Cd.Desc_2024_02_Feb_table, decreasing = TRUE)
Sorted_Central_Crm.Cd.Desc_2024_02_Feb
# Get top 3 areas
top_3_Central_Crm.Cd.Desc_2024_02_Feb <- head(Sorted_Central_Crm.Cd.Desc_2024_02_Feb, 3)
top_3_Central_Crm.Cd.Desc_2024_02_Feb
# Extract names and counts
top_3_Central_Crm.Cd.Desc_2024_02_Feb_names <- names(top_3_Central_Crm.Cd.Desc_2024_02_Feb)
top_3_Central_Crm.Cd.Desc_2024_02_Feb_names
top_3_Central_Crm.Cd.Desc_2024_02_Feb_counts <- as.numeric(top_3_Central_Crm.Cd.Desc_2024_02_Feb)
top_3_Central_Crm.Cd.Desc_2024_02_Feb_counts
# Print summary sentence
cat(paste0(
  top_3_Central_Crm.Cd.Desc_2024_02_Feb_names[1], " had the most incidents in the Central area for 2024_02_Feb  ", top_3_Central_Crm.Cd.Desc_2024_02_Feb_counts[1], " cases; ",
  top_3_Central_Crm.Cd.Desc_2024_02_Feb_names[2], " followed with ", top_3_Central_Crm.Cd.Desc_2024_02_Feb_counts[2], " cases; and ",
  top_3_Central_Crm.Cd.Desc_2024_02_Feb_names[3], " had ", top_3_Central_Crm.Cd.Desc_2024_02_Feb_counts[3], " cases."
))



# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")
# onto next variable


# Create a string for AREA_Names
AREA.NAME_2024_02_Feb_string <- '
# list all the AREA.NAME names
unique(LA_2024_02_Feb$AREA.NAME)
length(unique(LA_2024_02_Feb$AREA.NAME))
table(LA_2024_02_Feb$AREA.NAME)
AREA.NAME_2024_02_Feb <- table(LA_2024_02_Feb$AREA.NAME)
AREA.NAME_2024_02_Feb

# Max AREA_NAME
max_AREA.NAME_2024_02_Feb <- names(which.max(AREA.NAME_2024_02_Feb))
max_AREA.NAME_2024_02_Feb
max_count_AREA.NAME_2024_02_Feb <- max(AREA.NAME_2024_02_Feb)
max_count_AREA.NAME_2024_02_Feb

# Min AREA_NAME
min_AREA.NAME_2024_02_Feb <- names(which.min(AREA.NAME_2024_02_Feb))
min_AREA.NAME_2024_02_Feb
min_count_AREA.NAME_2024_02_Feb <- min(AREA.NAME_2024_02_Feb)
min_count_AREA.NAME_2024_02_Feb

cat("AREA Most incidents:\n", max_AREA.NAME_2024_02_Feb, "with", max_count_AREA.NAME_2024_02_Feb, "incidents for 2024_02_Feb\n\n")
cat("AREA Fewest incidents:\n", min_AREA.NAME_2024_02_Feb, "with", min_count_AREA.NAME_2024_02_Feb, "incidents for 2024_02_Feb\n\n")

# Sort AREA.name in decreasing order
Sorted_AREA.NAME_2024_02_Feb <- sort(AREA.NAME_2024_02_Feb, decreasing = TRUE)
head(Sorted_AREA.NAME_2024_02_Feb)
tail(Sorted_AREA.NAME_2024_02_Feb)

# Get top 3 most frequent AREA_NAME
top_3_Sorted_AREA.NAME_2024_02_Feb <- head(Sorted_AREA.NAME_2024_02_Feb, 3)
top_3_Sorted_AREA.NAME_2024_02_Feb

# Extract SOrted AREA_NAME, names and counts
top_3_Sorted_AREA.NAME_names_2024_02_Feb <- names(top_3_Sorted_AREA.NAME_2024_02_Feb)
top_3_Sorted_AREA.NAME_names_2024_02_Feb
top_3_Sorted_AREA.NAME_counts_2024_02_Feb <- as.numeric(top_3_Sorted_AREA.NAME_2024_02_Feb)
top_3_Sorted_AREA.NAME_counts_2024_02_Feb

# Print summary sentence
cat(paste0(
  top_3_Sorted_AREA.NAME_names_2024_02_Feb[1], " had the most incidents for 2024_02_Feb with ", top_3_Sorted_AREA.NAME_counts_2024_02_Feb[1], " cases; ",
  top_3_Sorted_AREA.NAME_names_2024_02_Feb[2], " followed with ", top_3_Sorted_AREA.NAME_counts_2024_02_Feb[2], " cases; and ",
  top_3_Sorted_AREA.NAME_names_2024_02_Feb[3], " had ", top_3_Sorted_AREA.NAME_counts_2024_02_Feb[3], " cases."
))

'
cat(AREA.NAME_2024_02_Feb_string)


# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")
# onto next variable


# 1. Switch AREA.NAME to Crm.Cd.Desc
Crm.Cd_Desc_2024_02_Feb_string <- gsub("AREA.NAME", "Crm.Cd.Desc", AREA.NAME_2024_02_Feb_string)
cat(Crm.Cd_Desc_2024_02_Feb_string)
# 2. Switch AREA (as a word) to Crm_Cd_Desc (variable prefix)
Crm.Cd_Desc_2024_02_Feb_string <- gsub("AREA", "Crm.Cd.Desc", Crm.Cd_Desc_2024_02_Feb_string)
cat(Crm.Cd_Desc_2024_02_Feb_string)
# Paste output
# list all the Crm.Cd.Desc names
unique(LA_2024_02_Feb$Crm.Cd.Desc)
length(unique(LA_2024_02_Feb$Crm.Cd.Desc))
table(LA_2024_02_Feb$Crm.Cd.Desc)
Crm.Cd.Desc_2024_02_Feb <- table(LA_2024_02_Feb$Crm.Cd.Desc)
Crm.Cd.Desc_2024_02_Feb

# Max Crm.Cd.Desc
max_Crm.Cd.Desc_2024_02_Feb <- names(which.max(Crm.Cd.Desc_2024_02_Feb))
max_Crm.Cd.Desc_2024_02_Feb
max_count_Crm.Cd.Desc_2024_02_Feb <- max(Crm.Cd.Desc_2024_02_Feb)
max_count_Crm.Cd.Desc_2024_02_Feb

# Min Crm.Cd.Desc
min_Crm.Cd.Desc_2024_02_Feb <- names(which.min(Crm.Cd.Desc_2024_02_Feb))
min_Crm.Cd.Desc_2024_02_Feb
min_count_Crm.Cd.Desc_2024_02_Feb <- min(Crm.Cd.Desc_2024_02_Feb)
min_count_Crm.Cd.Desc_2024_02_Feb

cat("Crm.Cd.Desc Most incidents:
", max_Crm.Cd.Desc_2024_02_Feb, "with", max_count_Crm.Cd.Desc_2024_02_Feb, "incidents for 2024_02_Feb

")
cat("Crm.Cd.Desc Fewest incidents:
", min_Crm.Cd.Desc_2024_02_Feb, "with", min_count_Crm.Cd.Desc_2024_02_Feb, "incidents for 2024_02_Feb

")

# Sort Crm.Cd.Desc.name in decreasing order
Sorted_Crm.Cd.Desc_2024_02_Feb <- sort(Crm.Cd.Desc_2024_02_Feb, decreasing = TRUE)
head(Sorted_Crm.Cd.Desc_2024_02_Feb)
tail(Sorted_Crm.Cd.Desc_2024_02_Feb)

# Get top 3 most frequent Crm.Cd.Desc
top_3_Sorted_Crm.Cd.Desc_2024_02_Feb <- head(Sorted_Crm.Cd.Desc_2024_02_Feb, 3)
top_3_Sorted_Crm.Cd.Desc_2024_02_Feb

# Extract SOrted Crm.Cd.Desc, names and counts
top_3_Sorted_Crm.Cd.Desc_names_2024_02_Feb <- names(top_3_Sorted_Crm.Cd.Desc_2024_02_Feb)
top_3_Sorted_Crm.Cd.Desc_names_2024_02_Feb
top_3_Sorted_Crm.Cd.Desc_counts_2024_02_Feb <- as.numeric(top_3_Sorted_Crm.Cd.Desc_2024_02_Feb)
top_3_Sorted_Crm.Cd.Desc_counts_2024_02_Feb

# Print summary sentence
cat(paste0(
  top_3_Sorted_Crm.Cd.Desc_names_2024_02_Feb[1], " had the most incidents for 2024_02_Feb with ", top_3_Sorted_Crm.Cd.Desc_counts_2024_02_Feb[1], " cases; ",
  top_3_Sorted_Crm.Cd.Desc_names_2024_02_Feb[2], " followed with ", top_3_Sorted_Crm.Cd.Desc_counts_2024_02_Feb[2], " cases; and ",
  top_3_Sorted_Crm.Cd.Desc_names_2024_02_Feb[3], " had ", top_3_Sorted_Crm.Cd.Desc_counts_2024_02_Feb[3], " cases."
))

# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")
summary(LA_2024_02_Feb)
# onto next variable



###########################
# Filter dataset for "VEHICLE - STOLEN"
VEHICLE_STOLEN_2024_02_Feb_table <- subset(LA_2024_02_Feb, Crm.Cd.Desc == "VEHICLE - STOLEN", select = c("Crm.Cd.Desc","AREA.NAME", "LOCATION", "Cross.Street","Premis.Desc", "TIME.OCC", "DATE.OCC", "Vict.Age", "Vict.Sex", "Vict.Descent"))
head(VEHICLE_STOLEN_2024_02_Feb_table)
tail(VEHICLE_STOLEN_2024_02_Feb_table)
View(VEHICLE_STOLEN_2024_02_Feb_table)

# list all the AREA.NAME for Vehicle - Stolen
unique(VEHICLE_STOLEN_2024_02_Feb_table$AREA.NAME)
length(unique(VEHICLE_STOLEN_2024_02_Feb_table$AREA.NAME))
table(VEHICLE_STOLEN_2024_02_Feb_table$AREA.NAME)
VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_table <- table(VEHICLE_STOLEN_2024_02_Feb_table$AREA.NAME)
head(VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_table)

# Max VEHICLE_STOLEN_AREA.NAME
max_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb <- names(which.max(VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_table))
max_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb
max_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_count <- max(VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_table)
max_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_count

# Min VEHICLE_STOLEN_AREA.NAME
min_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb <- names(which.min(VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_table))
min_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb
min_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_count <- min(VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_table)
min_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_count

cat("For 2024_02_Feb. Area with VEHICLE STOLEN Most incidents: ",max_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb, "with", max_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_count, "incidents ")
cat("For 2024_02_Feb, Area with VEHICLE STOLEN Fewest incidents: ", min_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb, "with", min_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_count, "incidents ")


### top 3 sorted for AREA.NAME
# Sort the AREA.NAME frequency table in descending order
Sorted_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb <- sort(VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_table, decreasing = TRUE)
Sorted_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb
# Get top 3 areas
top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb <- head(Sorted_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb, 3)
top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb
# Extract names and counts
top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_names <- names(top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb)
top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_names 
# Extract counts
top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_counts <- as.numeric(top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb)
top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_counts
# Print summary sentence
cat(paste0(
  top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_names [1], " had the most vehicle thefts for 2024_02_Feb with ", top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_counts[1], " cases; ",
  top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_names [2], " followed with ", top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_counts[2], " cases; and ",
  top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_names [3], " had ", top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_counts[3], " cases."
))


#######
# Create subset for Newton area only for Vehicle stolen
VEHICLE_STOLEN_Newton_2024_02_Feb_table <- subset(VEHICLE_STOLEN_2024_02_Feb_table, AREA.NAME == "Newton")
VEHICLE_STOLEN_Newton_2024_02_Feb_table
# View the first few rows
head(VEHICLE_STOLEN_Newton_2024_02_Feb_table)
tail(VEHICLE_STOLEN_Newton_2024_02_Feb_table) 
 
# Max/Min LOCATION for subset Newton area for vehicle stolen
VEHICLE_STOLEN_Newton_LOCATION_2024_02_Feb_table <- table(VEHICLE_STOLEN_Newton_2024_02_Feb_table$LOCATION)
head(VEHICLE_STOLEN_Newton_LOCATION_2024_02_Feb_table)

max_VEHICLE_STOLEN_Newton_LOCATION_2024_02_Feb_name <- names(which.max(VEHICLE_STOLEN_Newton_LOCATION_2024_02_Feb_table))
max_VEHICLE_STOLEN_Newton_LOCATION_2024_02_Feb_name
max_VEHICLE_STOLEN_Newton_LOCATION_2024_02_Feb_count <- max(VEHICLE_STOLEN_Newton_LOCATION_2024_02_Feb_table)
max_VEHICLE_STOLEN_Newton_LOCATION_2024_02_Feb_count

VEHICLE_STOLEN_Newton_Cross.Street_2024_02_Feb_table <- table(VEHICLE_STOLEN_Newton_2024_02_Feb_table$Cross.Street)
VEHICLE_STOLEN_Newton_Cross.Street_2024_02_Feb_table
head(VEHICLE_STOLEN_Newton_Cross.Street_2024_02_Feb_table )
# Max/Min Cross.Street for subset Newton area for vehicle stolen

# Filter out rows with blank Cross.Street values
VEHICLE_STOLEN_Newton_Cross.Street_2024_02_Feb_table <- VEHICLE_STOLEN_Newton_2024_02_Feb_table$Cross.Street[VEHICLE_STOLEN_Newton_2024_02_Feb_table$Cross.Street != ""]
VEHICLE_STOLEN_Newton_Cross.Street_2024_02_Feb_table

# Create table without blank entries
VEHICLE_STOLEN_Newton_Cross.Street_2024_02_Feb_table1 <- table(VEHICLE_STOLEN_Newton_Cross.Street_2024_02_Feb_table)
VEHICLE_STOLEN_Newton_Cross.Street_2024_02_Feb_table1

# Get max/min(1)
max_VEHICLE_STOLEN_Newton_Cross.Street_2024_02_Feb_name <- names(which.max(VEHICLE_STOLEN_Newton_Cross.Street_2024_02_Feb_table1))
max_VEHICLE_STOLEN_Newton_Cross.Street_2024_02_Feb_name
max_VEHICLE_STOLEN_Newton_Cross.Street_2024_02_Feb_count <- max(VEHICLE_STOLEN_Newton_Cross.Street_2024_02_Feb_table1)
max_VEHICLE_STOLEN_Newton_Cross.Street_2024_02_Feb_count
# Output result
cat("Most frequent Cross.Street (non-empty):", max_VEHICLE_STOLEN_Newton_Cross.Street_2024_02_Feb_name, "with", max_VEHICLE_STOLEN_Newton_Cross.Street_2024_02_Feb_count, "incidents
")

# Max/Min(1) TIME.OCC
VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb_table <- table(VEHICLE_STOLEN_Newton_2024_02_Feb_table$TIME.OCC)
head(VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb_table)
max_VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb_name <- names(which.max(VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb_table))
max_VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb_name 
max_VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb_count <- max(VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb_table)
max_VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb_count

# Create frequency table
VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb_table <- table(VEHICLE_STOLEN_Newton_2024_02_Feb_table$TIME.OCC)
VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb_table 
# Sort in descending order
sorted_VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb <- sort(VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb_table, decreasing = TRUE)
sorted_VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb 
# Extract top 3 TIME.OCC names and counts
top_3_sorted_VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb_names <- names(sorted_VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb[1:3])
top_3_sorted_VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb_names
top_3_sorted_VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb_counts <- as.numeric(sorted_VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb[1:3])
top_3_sorted_VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb_counts
# Print results
top_3_sorted_VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb <- data.frame(TIME.OCC = top_3_sorted_VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb_names, Count = top_3_sorted_VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb_counts)
print(top_3_sorted_VEHICLE_STOLEN_Newton_TIME.OCC_2024_02_Feb)

# Max/Min DATE.OCC
VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb_table <- table(VEHICLE_STOLEN_Newton_2024_02_Feb_table$DATE.OCC)
head(VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb_table)
max_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb_name <- names(which.max(VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb_table))
max_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb_name
max_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb_count <- max(VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb_table)
max_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb_count 

# Create frequency table
VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb_table <- table(VEHICLE_STOLEN_Newton_2024_02_Feb_table$DATE.OCC)
VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb_table
head(VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb_table)
# Sort in descending order
sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb <- sort(VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb_table, decreasing = TRUE)
sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb

# Extract top 3 DATE.OCC names and counts for Newton
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb_names <- names(sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb[1:3])
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb_names
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb_counts <- as.numeric(sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb[1:3])
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb_counts

# Print results
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb <- data.frame(DATE.OCC = top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb_names, Counts = top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb_counts)
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb

# Convert DATE.OCC to Date format (if not already)
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb$DATE.OCC <- as.Date(top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb$DATE.OCC, format = "%m/%d/%Y")
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb$DATE.OC
# Add a new column for the day of the week
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb$Day.of.Week <- weekdays(top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb$DATE.OCC)
top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb$Day.of.Week
# Print updated results
print(top_3_sorted_DATE_VEHICLE_STOLEN_Newton_DATE.OCC_2024_02_Feb)



### switch from AREA.NAME to TIME.OCC for Vehicle stolen
VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_String <- '
###########################
# Filter dataset for "VEHICLE - STOLEN"
VEHICLE_STOLEN_2024_02_Feb_table <- subset(LA_2024_02_Feb, Crm.Cd.Desc == "VEHICLE - STOLEN", select = c("Crm.Cd.Desc","AREA.NAME", "LOCATION", "Cross.Street","Premis.Desc", "TIME.OCC", "DATE.OCC", "Vict.Age", "Vict.Sex", "Vict.Descent"))
head(VEHICLE_STOLEN_2024_02_Feb_table)
tail(VEHICLE_STOLEN_2024_02_Feb_table)
View(VEHICLE_STOLEN_2024_02_Feb_table)

# list all the AREA.NAME for Vehicle - Stolen
unique(VEHICLE_STOLEN_2024_02_Feb_table$AREA.NAME)
length(unique(VEHICLE_STOLEN_2024_02_Feb_table$AREA.NAME))
table(VEHICLE_STOLEN_2024_02_Feb_table$AREA.NAME)
VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_table <- table(VEHICLE_STOLEN_2024_02_Feb_table$AREA.NAME)
head(VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_table)

# Max VEHICLE_STOLEN_AREA.NAME
max_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb <- names(which.max(VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_table))
max_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb
max_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_count <- max(VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_table)
max_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_count

# Min VEHICLE_STOLEN_AREA.NAME
min_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb <- names(which.min(VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_table))
min_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb
min_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_count <- min(VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_table)
min_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_count

cat("For 2024_02_Feb. AREA with VEHICLE STOLEN Most incidents: ",max_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb, "with", max_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_count, "incidents ")
cat("For 2024_02_Feb, AREA with VEHICLE STOLEN Fewest incidents: ", min_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb, "with", min_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_count, "incidents ")


### top 3 sorted for AREA.NAME
# Sort the AREA.NAME frequency table in descending order
Sorted_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb <- sort(VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_table, decreasing = TRUE)
Sorted_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb
# Get top 3 areas
top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb <- head(Sorted_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb, 3)
top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb
# Extract names and counts
top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb <- names(top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb)
top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb 
top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_counts <- as.numeric(top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb)
top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_counts
# Print summary sentence
cat(paste0(
  top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb[1], " had the most vehicle thefts for 2024_02_Feb with ", top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_counts[1], " cases; ",
  top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb[2], " followed with ", top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_counts[2], " cases; and ",
  top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb[3], " had ", top_3_VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_counts[3], " cases."
))

'
cat(VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_String)

# 1. Crete VEHICLE_STOLEN_TIME.OCC string
VEHICLE_STOLEN_TIME.OCC_2024_02_Feb <- gsub("AREA.NAME", "TIME.OCC", VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_String)
cat(VEHICLE_STOLEN_TIME.OCC_2024_02_Feb)
VEHICLE_STOLEN_TIME.OCC_2024_02_Feb <- gsub("AREA", "TIME", VEHICLE_STOLEN_TIME.OCC_2024_02_Feb)
cat(VEHICLE_STOLEN_TIME.OCC_2024_02_Feb)
#paste below and run
###########################
# Filter dataset for "VEHICLE - STOLEN"
VEHICLE_STOLEN_2024_02_Feb_table <- subset(LA_2024_02_Feb, Crm.Cd.Desc == "VEHICLE - STOLEN", select = c("Crm.Cd.Desc","TIME.OCC", "LOCATION", "Cross.Street","Premis.Desc", "TIME.OCC", "DATE.OCC", "Vict.Age", "Vict.Sex", "Vict.Descent"))
head(VEHICLE_STOLEN_2024_02_Feb_table)
tail(VEHICLE_STOLEN_2024_02_Feb_table)

# list all the TIME.OCC for Vehicle - Stolen
unique(VEHICLE_STOLEN_2024_02_Feb_table$TIME.OCC)
length(unique(VEHICLE_STOLEN_2024_02_Feb_table$TIME.OCC))
table(VEHICLE_STOLEN_2024_02_Feb_table$TIME.OCC)
VEHICLE_STOLEN_TIME.OCC_2024_02_Feb_table <- table(VEHICLE_STOLEN_2024_02_Feb_table$TIME.OCC)
head(VEHICLE_STOLEN_TIME.OCC_2024_02_Feb_table)

# Max VEHICLE_STOLEN_TIME.OCC
max_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb <- names(which.max(VEHICLE_STOLEN_TIME.OCC_2024_02_Feb_table))
max_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb
max_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb_count <- max(VEHICLE_STOLEN_TIME.OCC_2024_02_Feb_table)
max_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb_count

# Min VEHICLE_STOLEN_TIME.OCC
min_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb <- names(which.min(VEHICLE_STOLEN_TIME.OCC_2024_02_Feb_table))
min_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb
min_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb_count <- min(VEHICLE_STOLEN_TIME.OCC_2024_02_Feb_table)
min_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb_count

cat("For 2024_02_Feb. TIME with VEHICLE STOLEN Most incidents: ",max_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb, "with", max_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb_count, "incidents ")
cat("For 2024_02_Feb, TIME with VEHICLE STOLEN Fewest incidents: ", min_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb, "with", min_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb_count, "incidents ")


### top 3 sorted for TIME.OCC
# Sort the TIME.OCC frequency table in descending order
Sorted_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb <- sort(VEHICLE_STOLEN_TIME.OCC_2024_02_Feb_table, decreasing = TRUE)
Sorted_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb
# Get top 3 areas
top_3_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb <- head(Sorted_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb, 3)
top_3_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb
# Extract names and counts
top_3_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb <- names(top_3_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb)
top_3_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb 
top_3_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb_counts <- as.numeric(top_3_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb)
top_3_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb_counts
# Print summary sentence
cat(paste0(
  top_3_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb[1], " had the most vehicle thefts for 2024_02_Feb with ", top_3_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb_counts[1], " cases; ",
  top_3_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb[2], " followed with ", top_3_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb_counts[2], " cases; and ",
  top_3_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb[3], " had ", top_3_VEHICLE_STOLEN_TIME.OCC_2024_02_Feb_counts[3], " cases."
))




# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")
summary(LA_2024_02_Feb)
# onto next variable

# find the date with the most Vehicle stolen
# 1. Cretae VEHICLE_STOLEN_DATE.OCC string
VEHICLE_STOLEN_DATE.OCC_2024_02_Feb <- gsub("AREA.NAME", "DATE.OCC", VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_String)
cat(VEHICLE_STOLEN_DATE.OCC_2024_02_Feb)
VEHICLE_STOLEN_DATE.OCC_2024_02_Feb <- gsub("AREA", "DATE", VEHICLE_STOLEN_DATE.OCC_2024_02_Feb)
cat(VEHICLE_STOLEN_DATE.OCC_2024_02_Feb)
#paste below and run
###########################
# Filter dataset for "VEHICLE - STOLEN"
VEHICLE_STOLEN_2024_02_Feb_table <- subset(LA_2024_02_Feb, Crm.Cd.Desc == "VEHICLE - STOLEN", select = c("Crm.Cd.Desc","DATE.OCC", "LOCATION", "Cross.Street","Premis.Desc", "TIME.OCC", "DATE.OCC", "Vict.Age", "Vict.Sex", "Vict.Descent"))
head(VEHICLE_STOLEN_2024_02_Feb_table)
tail(VEHICLE_STOLEN_2024_02_Feb_table)

# list all the DATE.OCC for Vehicle - Stolen
unique(VEHICLE_STOLEN_2024_02_Feb_table$DATE.OCC)
length(unique(VEHICLE_STOLEN_2024_02_Feb_table$DATE.OCC))
table(VEHICLE_STOLEN_2024_02_Feb_table$DATE.OCC)
VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table <- table(VEHICLE_STOLEN_2024_02_Feb_table$DATE.OCC)
head(VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table)

# Max VEHICLE_STOLEN_DATE.OCC
max_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb <- names(which.max(VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table))
max_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb
max_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_count <- max(VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table)
max_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_count

# Min VEHICLE_STOLEN_DATE.OCC
min_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb <- names(which.min(VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table))
min_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb
min_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_count <- min(VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table)
min_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_count

cat("For 2024_02_Feb. Date",max_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb, "with VEHICLE STOLEN Most incidents: ", max_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_count, "incidents ")
cat("For 2024_02_Feb, Date",min_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb, " with VEHICLE STOLEN Fewest incidents: ", min_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb, "with", min_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_count, "incidents ")


### top 3 sorted for DATE.OCC
# Sort the DATE.OCC frequency table in descending order
Sorted_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb <- sort(VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table, decreasing = TRUE)
# Get top 3 DATE.OCC with counts
top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table <- head(Sorted_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb, 3)
top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table
# Extract dates
top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb <- names(top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table)
top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb
# Extract counts
top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_counts <- as.numeric(top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table)
top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_counts

# Convert to Date format and get weekdays
top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_dates <- as.Date(top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb, format = "%m/%d/%Y %I:%M:%S %p")
top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_weekdays <- weekdays(top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_dates)

# Create dataframe
top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_df <- data.frame(
  Date = top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb,
  Day = top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_weekdays,
  Count = top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_counts
)

# Print results
print(top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_df)
# Print summary sentence
cat(paste0(
  top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb[1], " (", top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_weekdays[1], ") had the most vehicle thefts for 2024_02_Feb with ", top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_counts[1], " cases; ",
  top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb[2], " (", top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_weekdays[2], ") followed with ", top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_counts[2], " cases; and ",
  top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb[3], " (", top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_weekdays[3], ") had ", top_3_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_counts[3], " cases."
))



###### day of week with most vehicle stolen in Feb 2024
# Create a frequency table
VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table <- table(VEHICLE_STOLEN_2024_02_Feb_table$DATE.OCC)
head(VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table)
# Convert date strings to Date objects
dates_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table <- as.Date(names(VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table), format = "%m/%d/%Y %I:%M:%S %p")
head(dates_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table)
# Extract day of the week
days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table  <- weekdays(dates_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table)
head(days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table)
# Sum theft counts by day of week
day_totals_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table <- tapply(as.numeric(VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table ), days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table , sum)
head(day_totals_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table)
# Find the day with the most vehicle thefts
max_day_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table <- names(which.max(day_totals_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table ))
max_day_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table
max_day_count_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table  <- max(day_totals_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table)
max_day_count_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table
# Print result
cat(paste0(max_day_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table, " had the most vehicle thefts with a total of ", max_day_count_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table, " cases.
"))

# Sort day totals in descending order
sorted_day_totals_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb <- sort(day_totals_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table, decreasing = TRUE)
sorted_day_totals_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb
# Get top 3 days with their counts
top_3_days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table <- head(sorted_day_totals_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb, 3)
top_3_days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table
# Extract names (days)
top_3_days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb <- names(top_3_days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table)
top_3_days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb
# Extract counts
top_3_days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_counts <- as.numeric(top_3_days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_table)
top_3_days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_counts
# Create data frame
top_3_days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_df <- data.frame(
  Day = top_3_days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb,
  Count = top_3_days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_counts
)
# Print the top 3 results
print(top_3_days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_df)

# Summary sentence for days with most Vehicle stolen
cat(paste0(
  top_3_days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb[1], " had the most vehicle thefts in 2024_02_Feb with ", top_3_days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_counts[1], " cases; ",
  top_3_days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb[2], " followed with ", top_3_days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_counts[2], " cases; and ",
  top_3_days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb[3], " had ", top_3_days_VEHICLE_STOLEN_DATE.OCC_2024_02_Feb_counts[3], " cases."
))


# find the times when vehicle thefts are commmitted on Sundays
# Convert DATE.OCC to Date type
VEHICLE_STOLEN_2024_02_Feb_table$DATE_PARSED <- as.Date(VEHICLE_STOLEN_2024_02_Feb_table$DATE.OCC, format = "%m/%d/%Y %I:%M:%S %p")
VEHICLE_STOLEN_2024_02_Feb_table$DATE_PARSED
# Extract day of the week
VEHICLE_STOLEN_2024_02_Feb_table$DAY_OF_WEEK <- weekdays(VEHICLE_STOLEN_2024_02_Feb_table$DATE_PARSED)
VEHICLE_STOLEN_2024_02_Feb_table$DAY_OF_WEEK
# Filter records for Sunday
VEHICLE_STOLEN_Sunday_2024_02_Feb_table <- VEHICLE_STOLEN_2024_02_Feb_table[VEHICLE_STOLEN_2024_02_Feb_table$DAY_OF_WEEK == "Sunday", ]
VEHICLE_STOLEN_Sunday_2024_02_Feb_table

# Check number of Sunday cases
cat("Number of Sunday cases for 2024_02_Feb:", nrow(VEHICLE_STOLEN_Sunday_2024_02_Feb_table), "
")

# Tabulate TIME.OCC for Sunday
VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_table <- table(VEHICLE_STOLEN_Sunday_2024_02_Feb_table$TIME.OCC)
VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_table
# Sort TIME.OCC frequencies in descending order
VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_sorted <- sort(VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_table, decreasing = TRUE)
VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_sorted
# Extract top 3 TIME.OCC values and their counts
top_5_VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_times <- names(VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_sorted)[1:5]
top_5_VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_times
top_5_VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_counts <- as.numeric(VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_sorted[1:5])
top_5_VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_counts
# Print summary
cat(paste0(
  "Top 5 times for vehicle theft on Sundays for 2024_02_Feb:
",
  top_5_VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_times[1], " with ", top_5_VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_counts[1], " cases
",
  top_5_VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_times[2], " with ", top_5_VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_counts[2], " cases
",
  top_5_VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_times[3], " with ", top_5_VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_counts[3], " cases
",
  top_5_VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_times[4], " with ", top_5_VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_counts[4], " cases
",
  top_5_VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_times[5], " with ", top_5_VEHICLE_STOLEN_Sunday_TIME.OCC_2024_02_Feb_counts[5], " cases
"
))


###########################
# Filter dataset for "BATTERY - SIMPLE ASSAULT"
BATTERY_SIMPLE_ASSAULT_2024_02_Feb_table <- subset(LA_2024_02_Feb, Crm.Cd.Desc == "BATTERY - SIMPLE ASSAULT", select = c("Crm.Cd.Desc","AREA.NAME", "LOCATION", "Cross.Street","Premis.Desc", "TIME.OCC", "DATE.OCC", "Vict.Age", "Vict.Sex", "Vict.Descent"))
head(BATTERY_SIMPLE_ASSAULT_2024_02_Feb_table)
tail(BATTERY_SIMPLE_ASSAULT_2024_02_Feb_table)
View(BATTERY_SIMPLE_ASSAULT_2024_02_Feb_table)
# switch from vehilce stolent to battery - simple assault
BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_string <- gsub("VEHICLE_STOLEN", "BATTERY_SIMPLE_ASSAULT", VEHICLE_STOLEN_AREA.NAME_2024_02_Feb_String)
cat(BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_string)
BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_string <- gsub("VEHICLE STOLEN", "BATTERY - SIMPLE ASSAULT", BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_string)
cat(BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_string)
# paste below
# list all the AREA.NAME for BATTERY SIMPLE ASSAULT
unique(BATTERY_SIMPLE_ASSAULT_2024_02_Feb_table$AREA.NAME)
length(unique(BATTERY_SIMPLE_ASSAULT_2024_02_Feb_table$AREA.NAME))
table(BATTERY_SIMPLE_ASSAULT_2024_02_Feb_table$AREA.NAME)
BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_table <- table(BATTERY_SIMPLE_ASSAULT_2024_02_Feb_table$AREA.NAME)
head(BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_table)

# Max BATTERY_SIMPLE_ASSAULT_AREA.NAME
max_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb <- names(which.max(BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_table))
max_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb
max_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_count <- max(BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_table)
max_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_count

# Min BATTERY_SIMPLE_ASSAULT_AREA.NAME
min_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb <- names(which.min(BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_table))
min_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb
min_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_count <- min(BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_table)
min_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_count

cat("For 2024_02_Feb. AREA with BATTERY - SIMPLE ASSAULT Most incidents: ",max_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb, "with", max_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_count, "incidents ")
cat("For 2024_02_Feb, AREA with BATTERY - SIMPLE ASSAULT Fewest incidents: ", min_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb, "with", min_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_count, "incidents ")


### top 3 sorted for AREA.NAME
# Sort the AREA.NAME frequency table in descending order
Sorted_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb <- sort(BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_table, decreasing = TRUE)
Sorted_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb
# Get top 3 areas
top_3_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb <- head(Sorted_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb, 3)
top_3_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb
# Extract names and counts
top_3_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_names <- names(top_3_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb)
top_3_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_names 
top_3_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_counts <- as.numeric(top_3_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb)
top_3_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_counts
# Print summary sentence
cat(paste0(
  top_3_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_names  [1], " had the most BATTERY - SIMPLE ASSAULT for 2024_02_Feb with ", top_3_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_counts[1], " cases; ",
  top_3_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_names [2], " followed with ", top_3_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_counts[2], " cases; and ",
  top_3_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_names [3], " had ", top_3_BATTERY_SIMPLE_ASSAULT_AREA.NAME_2024_02_Feb_counts[3], " cases."
))


##################

# classify each crime as violent vs non violent
# Define pattern of keywords that signify violent crimes
violent_keywords <- c("ASSAULT", "BATTERY", "HOMICIDE", "ROBBERY", "RAPE", "KIDNAPPING", "STALKING", 
                      "THREAT", "SEX", "SODOMY", "LEWD", "INTIMATE", "CHILD ABUSE", "WEAPON", 
                      "SHOTS FIRED", "CRIMINAL THREATS")
violent_keywords

# Combine keywords into a single regex pattern (case insensitive)
violent_pattern <- paste(violent_keywords, collapse = "|")
violent_pattern

# Get unique crime descriptions
crime_types <- unique(LA_2024_02_Feb$Crm.Cd.Desc)
crime_types

# Classify each crime as Violent or Non-Violent using grepl
crime_class <- ifelse(grepl(violent_pattern, crime_types, ignore.case = TRUE), "Violent", "Non-Violent")
crime_class

# Create a named vector or data frame for classification
crime_map <- setNames(crime_class, crime_types)
crime_map

# Add classification back to your data
LA_2024_02_Feb$Crime.Type <- crime_map[LA_2024_02_Feb$Crm.Cd.Desc]
LA_2024_02_Feb$Crime.Type
# Optional summary
table(LA_2024_02_Feb$Crime.Type)


# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")
# onto next variable


# 1. Switch AREA.NAME to Premis.Desc
Premis.Desc_2024_02_Feb_string <- gsub("AREA.NAME", "Premis.Desc", AREA.NAME_2024_02_Feb_string)
cat(Premis.Desc_2024_02_Feb_string)
# 2. Switch AREA (as a word) to Premis_Desc (variable prefix)
Premis.Desc_2024_02_Feb_string <- gsub("AREA", "Premis.Desc", Premis.Desc_2024_02_Feb_string)
cat(Premis.Desc_2024_02_Feb_string)
# Paste output
# list all the Premis.Desc names
unique(LA_2024_02_Feb$Premis.Desc)
length(unique(LA_2024_02_Feb$Premis.Desc))
table(LA_2024_02_Feb$Premis.Desc)
Premis.Desc_2024_02_Feb <- table(LA_2024_02_Feb$Premis.Desc)
Premis.Desc_2024_02_Feb

# Max Premis.Desc
max_Premis.Desc_2024_02_Feb <- names(which.max(Premis.Desc_2024_02_Feb))
max_Premis.Desc_2024_02_Feb
max_count_Premis.Desc_2024_02_Feb <- max(Premis.Desc_2024_02_Feb)
max_count_Premis.Desc_2024_02_Feb

# Min Premis.Desc
min_Premis.Desc_2024_02_Feb <- names(which.min(Premis.Desc_2024_02_Feb))
min_Premis.Desc_2024_02_Feb
min_count_Premis.Desc_2024_02_Feb <- min(Premis.Desc_2024_02_Feb)
min_count_Premis.Desc_2024_02_Feb

cat("Premis.Desc Most incidents:
", max_Premis.Desc_2024_02_Feb, "with", max_count_Premis.Desc_2024_02_Feb, "incidents for 2024_02_Feb

")
cat("Premis.Desc Fewest incidents:
", min_Premis.Desc_2024_02_Feb, "with", min_count_Premis.Desc_2024_02_Feb, "incidents for 2024_02_Feb

")

# Sort Premis.Desc.name in decreasing order
Sorted_Premis.Desc_2024_02_Feb <- sort(Premis.Desc_2024_02_Feb, decreasing = TRUE)
head(Sorted_Premis.Desc_2024_02_Feb)
tail(Sorted_Premis.Desc_2024_02_Feb)

# Get top 3 most frequent Premis.Desc
top_3_Sorted_Premis.Desc_2024_02_Feb <- head(Sorted_Premis.Desc_2024_02_Feb, 3)
top_3_Sorted_Premis.Desc_2024_02_Feb

# Extract SOrted Premis.Desc, names and counts
top_3_Sorted_Premis.Desc_names_2024_02_Feb <- names(top_3_Sorted_Premis.Desc_2024_02_Feb)
top_3_Sorted_Premis.Desc_names_2024_02_Feb
top_3_Sorted_Premis.Desc_counts_2024_02_Feb <- as.numeric(top_3_Sorted_Premis.Desc_2024_02_Feb)
top_3_Sorted_Premis.Desc_counts_2024_02_Feb

# Print summary sentence
cat(paste0(
  top_3_Sorted_Premis.Desc_names_2024_02_Feb[1], " had the most incidents for 2024_02_Feb with ", top_3_Sorted_Premis.Desc_counts_2024_02_Feb[1], " cases; ",
  top_3_Sorted_Premis.Desc_names_2024_02_Feb[2], " followed with ", top_3_Sorted_Premis.Desc_counts_2024_02_Feb[2], " cases; and ",
  top_3_Sorted_Premis.Desc_names_2024_02_Feb[3], " had ", top_3_Sorted_Premis.Desc_counts_2024_02_Feb[3], " cases."
))

## combine all MTA together
# Create frequency table of Premis.Desc
Premis.Desc_2024_02_Feb <- table(LA_2024_02_Feb$Premis.Desc)
head(Premis.Desc_2024_02_Feb)
# Trim whitespace from names, just in case
names(Premis.Desc_2024_02_Feb) <- trimws(names(Premis.Desc_2024_02_Feb))
# Extract rows that contain "MTA" (case-insensitive for safety)
MTA_Premis.Desc_2024_02_Feb <- Premis.Desc_2024_02_Feb[grep("MTA", names(Premis.Desc_2024_02_Feb), ignore.case = TRUE)]
# Print MTA-related Premis.Desc entries
print(MTA_Premis.Desc_2024_02_Feb)
# Sum all rows that contain "MTA"
total_MTA <- sum(MTA_Premis.Desc_2024_02_Feb)
cat("Total incidents involving MTA locations:", total_MTA, "\n")
# Max MTA location
max_MTA_Premis.Desc_2024_02_Feb <- names(which.max(MTA_Premis.Desc_2024_02_Feb))
max_count_MTA_Premis.Desc_2024_02_Feb <- max(MTA_Premis.Desc_2024_02_Feb)
cat("Most frequent MTA location for 2024_02_Feb:", max_MTA_Premis.Desc_2024_02_Feb, "with", max_count_MTA_Premis.Desc_2024_02_Feb, "incidents\n")
# Sort in descending order
Sorted_MTA_Premis.Desc_2024_02_Feb <- sort(MTA_Premis.Desc_2024_02_Feb, decreasing = TRUE)
Sorted_MTA_Premis.Desc_2024_02_Feb 
# Get top 3 MTA locations 
top_3_MTA_2024_02_Feb <- head(Sorted_MTA_Premis.Desc_2024_02_Feb , 3)
top_3_MTA_2024_02_Feb
# Print top 3
print(top_3_MTA_2024_02_Feb)

# Optional: summary sentence
top_3_names_MTA_2024_02_Feb <- names(top_3_MTA_2024_02_Feb)
top_3_names_MTA_2024_02_Feb
top_3_counts_MTA_2024_02_Feb <- as.numeric(top_3_MTA_2024_02_Feb)
top_3_counts_MTA_2024_02_Feb

cat(paste0(
  top_3_names_MTA_2024_02_Feb[1], " had the most MTA incidents on 2024_02_Feb with ", top_3_counts_MTA_2024_02_Feb[1], " cases; ",
  top_3_names_MTA_2024_02_Feb[2], " followed with ", top_3_counts_MTA_2024_02_Feb[2], " cases; and ",
  top_3_names_MTA_2024_02_Feb[3], " had ", top_3_counts_MTA_2024_02_Feb[3], " cases.\n"
))



# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")
# onto next variable


# 1. Switch AREA.NAME to LOCATION
LOCATION_2024_02_Feb_string <- gsub("AREA.NAME", "LOCATION", AREA.NAME_2024_02_Feb_string)
cat(LOCATION_2024_02_Feb_string)
# 2. Switch AREA (as a word) to Premis_Desc (variable prefix)
LOCATION_2024_02_Feb_string <- gsub("AREA", "LOCATION", LOCATION_2024_02_Feb_string)
cat(LOCATION_2024_02_Feb_string)
# Paste output
# list all the LOCATION names
unique(LA_2024_02_Feb$LOCATION)
length(unique(LA_2024_02_Feb$LOCATION))
table(LA_2024_02_Feb$LOCATION)
LOCATION_2024_02_Feb <- table(LA_2024_02_Feb$LOCATION)
LOCATION_2024_02_Feb

# Max LOCATION
max_LOCATION_2024_02_Feb <- names(which.max(LOCATION_2024_02_Feb))
max_LOCATION_2024_02_Feb
max_count_LOCATION_2024_02_Feb <- max(LOCATION_2024_02_Feb)
max_count_LOCATION_2024_02_Feb

# Min LOCATION
min_LOCATION_2024_02_Feb <- names(which.min(LOCATION_2024_02_Feb))
min_LOCATION_2024_02_Feb
min_count_LOCATION_2024_02_Feb <- min(LOCATION_2024_02_Feb)
min_count_LOCATION_2024_02_Feb

cat("LOCATION Most incidents:
", max_LOCATION_2024_02_Feb, "with", max_count_LOCATION_2024_02_Feb, "incidents for 2024_02_Feb

")
cat("LOCATION Fewest incidents:
", min_LOCATION_2024_02_Feb, "with", min_count_LOCATION_2024_02_Feb, "incidents for 2024_02_Feb

")

# Sort LOCATION.name in decreasing order
Sorted_LOCATION_2024_02_Feb <- sort(LOCATION_2024_02_Feb, decreasing = TRUE)
head(Sorted_LOCATION_2024_02_Feb)
tail(Sorted_LOCATION_2024_02_Feb)

# Get top 3 most frequent LOCATION
top_3_Sorted_LOCATION_2024_02_Feb <- head(Sorted_LOCATION_2024_02_Feb, 3)
top_3_Sorted_LOCATION_2024_02_Feb

# Extract SOrted LOCATION, names and counts
top_3_Sorted_LOCATION_names_2024_02_Feb <- names(top_3_Sorted_LOCATION_2024_02_Feb)
top_3_Sorted_LOCATION_names_2024_02_Feb
top_3_Sorted_LOCATION_counts_2024_02_Feb <- as.numeric(top_3_Sorted_LOCATION_2024_02_Feb)
top_3_Sorted_LOCATION_counts_2024_02_Feb

# Print summary sentence
cat(paste0(
  top_3_Sorted_LOCATION_names_2024_02_Feb[1], " had the most incidents for 2024_02_Feb with ", top_3_Sorted_LOCATION_counts_2024_02_Feb[1], " cases; ",
  top_3_Sorted_LOCATION_names_2024_02_Feb[2], " followed with ", top_3_Sorted_LOCATION_counts_2024_02_Feb[2], " cases; and ",
  top_3_Sorted_LOCATION_names_2024_02_Feb[3], " had ", top_3_Sorted_LOCATION_counts_2024_02_Feb[3], " cases."
))


# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")
# onto next variable


# 1. Switch AREA.NAME to Cross.Street
Cross.Street_2024_02_Feb_String <- gsub("AREA.NAME", "Cross.Street", AREA.NAME_2024_02_Feb_string)
cat(Cross.Street_2024_02_Feb_String)
# 2. Switch AREA (as a word) to Premis_Desc (variable prefix)
Cross_Street_string_2 <- gsub("AREA", "Cross.Street", Cross.Street_2024_02_Feb_String)
cat(Cross.Street_2024_02_Feb_String)
# paste below, add code to remove empty strings
# list all the Cross.Street names
unique(LA_2024_02_Feb$Cross.Street)
length(unique(LA_2024_02_Feb$Cross.Street))
table(LA_2024_02_Feb$Cross.Street)
Cross.Street_2024_02_Feb <- table(LA_2024_02_Feb$Cross.Street)
head(Cross.Street_2024_02_Feb)
# Remove empty or blank Cross.Street entries
Cross.Street_2024_02_Feb <- Cross.Street_2024_02_Feb[names(Cross.Street_2024_02_Feb) != ""]
head(Cross.Street_2024_02_Feb)
# Max Cross.Street
max_Cross.Street_2024_02_Feb <- names(which.max(Cross.Street_2024_02_Feb))
max_Cross.Street_2024_02_Feb
max_count_Cross.Street_2024_02_Feb <- max(Cross.Street_2024_02_Feb)
max_count_Cross.Street_2024_02_Feb

# Min Cross.Street
min_Cross.Street_2024_02_Feb <- names(which.min(Cross.Street_2024_02_Feb))
min_Cross.Street_2024_02_Feb
min_count_Cross.Street_2024_02_Feb <- min(Cross.Street_2024_02_Feb)
min_count_Cross.Street_2024_02_Feb

cat("AREA Most incidents:
", max_Cross.Street_2024_02_Feb, "with", max_count_Cross.Street_2024_02_Feb, "incidents for 2024_02_Feb

")
cat("AREA Fewest incidents:
", min_Cross.Street_2024_02_Feb, "with", min_count_Cross.Street_2024_02_Feb, "incidents for 2024_02_Feb

")

# Sort AREA.name in decreasing order
Sorted_Cross.Street_2024_02_Feb <- sort(Cross.Street_2024_02_Feb, decreasing = TRUE)
head(Sorted_Cross.Street_2024_02_Feb)
tail(Sorted_Cross.Street_2024_02_Feb)

# Get top 3 most frequent Cross.Street
top_3_Sorted_Cross.Street_2024_02_Feb <- head(Sorted_Cross.Street_2024_02_Feb, 3)
top_3_Sorted_Cross.Street_2024_02_Feb

# Extract SOrted Cross.Street, names and counts
top_3_Sorted_Cross.Street_names_2024_02_Feb <- names(top_3_Sorted_Cross.Street_2024_02_Feb)
top_3_Sorted_Cross.Street_names_2024_02_Feb
top_3_Sorted_Cross.Street_counts_2024_02_Feb <- as.numeric(top_3_Sorted_Cross.Street_2024_02_Feb)
top_3_Sorted_Cross.Street_counts_2024_02_Feb

# Print summary sentence
cat(paste0(
  top_3_Sorted_Cross.Street_names_2024_02_Feb[1], " had the most incidents for 2024_02_Feb with ", top_3_Sorted_Cross.Street_counts_2024_02_Feb[1], " cases; ",
  top_3_Sorted_Cross.Street_names_2024_02_Feb[2], " followed with ", top_3_Sorted_Cross.Street_counts_2024_02_Feb[2], " cases; and ",
  top_3_Sorted_Cross.Street_names_2024_02_Feb[3], " had ", top_3_Sorted_Cross.Street_counts_2024_02_Feb[3], " cases."
))


# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")
# onto next variable



# 1. Switch AREA.NAME to Vict.Descent
Vict.Descent_2024_02_Feb_string <- gsub("AREA.NAME", "Vict.Descent",  AREA.NAME_2024_02_Feb_string)
cat(Vict.Descent_2024_02_Feb_string)
# 2. Switch AREA (as a word) to Vict.Descent (variable prefix)
Vict.Descent_2024_02_Feb_stringg <- gsub("AREA", "Vict.Descent", Vict.Descent_2024_02_Feb_string)
cat(Vict.Descent_2024_02_Feb_string)
# paste below
# list all the Vict.Descent names
unique(LA_2024_02_Feb$Vict.Descent)
length(unique(LA_2024_02_Feb$Vict.Descent))
table(LA_2024_02_Feb$Vict.Descent)
Vict.Descent_2024_02_Feb <- table(LA_2024_02_Feb$Vict.Descent)
Vict.Descent_2024_02_Feb

# Max Vict.Descent
max_Vict.Descent_2024_02_Feb <- names(which.max(Vict.Descent_2024_02_Feb))
max_Vict.Descent_2024_02_Feb
max_count_Vict.Descent_2024_02_Feb <- max(Vict.Descent_2024_02_Feb)
max_count_Vict.Descent_2024_02_Feb

# Min Vict.Descent
min_Vict.Descent_2024_02_Feb <- names(which.min(Vict.Descent_2024_02_Feb))
min_Vict.Descent_2024_02_Feb
min_count_Vict.Descent_2024_02_Feb <- min(Vict.Descent_2024_02_Feb)
min_count_Vict.Descent_2024_02_Feb

cat("AREA Most incidents:
", max_Vict.Descent_2024_02_Feb, "with", max_count_Vict.Descent_2024_02_Feb, "incidents for 2024_02_Feb

")
cat("AREA Fewest incidents:
", min_Vict.Descent_2024_02_Feb, "with", min_count_Vict.Descent_2024_02_Feb, "incidents for 2024_02_Feb

")

# Sort AREA.name in decreasing order
Sorted_Vict.Descent_2024_02_Feb <- sort(Vict.Descent_2024_02_Feb, decreasing = TRUE)
head(Sorted_Vict.Descent_2024_02_Feb)
tail(Sorted_Vict.Descent_2024_02_Feb)

# Get top 3 most frequent Vict.Descent
top_3_Sorted_Vict.Descent_2024_02_Feb <- head(Sorted_Vict.Descent_2024_02_Feb, 3)
top_3_Sorted_Vict.Descent_2024_02_Feb

# Extract SOrted Vict.Descent, names and counts
top_3_Sorted_Vict.Descent_names_2024_02_Feb <- names(top_3_Sorted_Vict.Descent_2024_02_Feb)
top_3_Sorted_Vict.Descent_names_2024_02_Feb
top_3_Sorted_Vict.Descent_counts_2024_02_Feb <- as.numeric(top_3_Sorted_Vict.Descent_2024_02_Feb)
top_3_Sorted_Vict.Descent_counts_2024_02_Feb

# Print summary sentence
cat(paste0(
  top_3_Sorted_Vict.Descent_names_2024_02_Feb[1], " had the most incidents for 2024_02_Feb with ", top_3_Sorted_Vict.Descent_counts_2024_02_Feb[1], " cases; ",
  top_3_Sorted_Vict.Descent_names_2024_02_Feb[2], " followed with ", top_3_Sorted_Vict.Descent_counts_2024_02_Feb[2], " cases; and ",
  top_3_Sorted_Vict.Descent_names_2024_02_Feb[3], " had ", top_3_Sorted_Vict.Descent_counts_2024_02_Feb[3], " cases."
))

# Create legend lookup for Vict.Descent codes
vict_descent_legend <- c(
  A = "Asian",
  B = "Black",
  C = "Chinese",
  D = "Cambodian",
  F = "Filipino",
  G = "Guamanian",
  H = "Hispanic/Latin/Mexican",
  I = "American Indian/Alaskan Native",
  J = "Japanese",
  K = "Korean",
  L = "Laotian",
  O = "Other",
  P = "Pacific Islander",
  S = "Samoan",
  U = "Hawaiian",
  V = "Vietnamese",
  W = "White",
  X = "Unknown",
  Z = "Asian Indian"
)

# Get top 3 Vict.Descent names and counts
top_3_Vict.Descent_2024_02_Feb <- names(top_3_Sorted_Vict.Descent_2024_02_Feb)
top_3_Vict.Descent_2024_02_Feb
top_3_labels_Vict.Descent_2024_02_Feb <- vict_descent_legend[top_3_Vict.Descent_2024_02_Feb]
top_3_labels_Vict.Descent_2024_02_Feb
top_3_counts_Vict.Descent_2024_02_Feb <- as.numeric(top_3_Sorted_Vict.Descent_2024_02_Feb)
top_3_counts_Vict.Descent_2024_02_Feb

# Print summary with full legend names
cat(paste0(
  top_3_labels_Vict.Descent_2024_02_Feb[1], " had the most Vict.Descent incidents with ", top_3_counts_Vict.Descent_2024_02_Feb[1], " cases; ",
  top_3_labels_Vict.Descent_2024_02_Feb[2], " followed with ", top_3_counts_Vict.Descent_2024_02_Feb[2], " cases; and ",
  top_3_labels_Vict.Descent_2024_02_Feb[3], " had ", top_3_counts_Vict.Descent_2024_02_Feb[3], " cases."
))




# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")
# onto next variable




# 1. Switch AREA.NAME to TIME.OCC
TIME.OCC_2024_02_Feb_string <- gsub("AREA.NAME", "TIME.OCC", AREA.NAME_2024_02_Feb_string)
cat(TIME.OCC_2024_02_Feb_string)
# 2. Switch AREA (as a word) to TIME.OCC (variable prefix)
TIME.OCC_2024_02_Feb_string <- gsub("AREA", "TIME.OCC", TIME.OCC_2024_02_Feb_string)
cat(TIME.OCC_2024_02_Feb_string)
# paste below
# list all the TIME.OCC names
unique(LA_2024_02_Feb$TIME.OCC)
length(unique(LA_2024_02_Feb$TIME.OCC))
table(LA_2024_02_Feb$TIME.OCC)
TIME.OCC_2024_02_Feb <- table(LA_2024_02_Feb$TIME.OCC)
TIME.OCC_2024_02_Feb

# Max TIME.OCC
max_TIME.OCC_2024_02_Feb <- names(which.max(TIME.OCC_2024_02_Feb))
max_TIME.OCC_2024_02_Feb
max_count_TIME.OCC_2024_02_Feb <- max(TIME.OCC_2024_02_Feb)
max_count_TIME.OCC_2024_02_Feb

# Min TIME.OCC
min_TIME.OCC_2024_02_Feb <- names(which.min(TIME.OCC_2024_02_Feb))
min_TIME.OCC_2024_02_Feb
min_count_TIME.OCC_2024_02_Feb <- min(TIME.OCC_2024_02_Feb)
min_count_TIME.OCC_2024_02_Feb

cat("TIME.OCC Most incidents:
", max_TIME.OCC_2024_02_Feb, "with", max_count_TIME.OCC_2024_02_Feb, "incidents for 2024_02_Feb

")
cat("TIME.OCC Fewest incidents:
", min_TIME.OCC_2024_02_Feb, "with", min_count_TIME.OCC_2024_02_Feb, "incidents for 2024_02_Feb

")

# Sort TIME.OCC.name in decreasing order
Sorted_TIME.OCC_2024_02_Feb <- sort(TIME.OCC_2024_02_Feb, decreasing = TRUE)
head(Sorted_TIME.OCC_2024_02_Feb)
tail(Sorted_TIME.OCC_2024_02_Feb)

# Get top 3 most frequent TIME.OCC
top_3_Sorted_TIME.OCC_2024_02_Feb <- head(Sorted_TIME.OCC_2024_02_Feb, 3)
top_3_Sorted_TIME.OCC_2024_02_Feb

# Extract SOrted TIME.OCC, names and counts
top_3_Sorted_TIME.OCC_names_2024_02_Feb <- names(top_3_Sorted_TIME.OCC_2024_02_Feb)
top_3_Sorted_TIME.OCC_names_2024_02_Feb
top_3_Sorted_TIME.OCC_counts_2024_02_Feb <- as.numeric(top_3_Sorted_TIME.OCC_2024_02_Feb)
top_3_Sorted_TIME.OCC_counts_2024_02_Feb

# Print summary sentence
cat(paste0(
  top_3_Sorted_TIME.OCC_names_2024_02_Feb[1], " had the most incidents for 2024_02_Feb with ", top_3_Sorted_TIME.OCC_counts_2024_02_Feb[1], " cases; ",
  top_3_Sorted_TIME.OCC_names_2024_02_Feb[2], " followed with ", top_3_Sorted_TIME.OCC_counts_2024_02_Feb[2], " cases; and ",
  top_3_Sorted_TIME.OCC_names_2024_02_Feb[3], " had ", top_3_Sorted_TIME.OCC_counts_2024_02_Feb[3], " cases."
))



# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")
# onto next variable


# 1. Switch AREA.NAME to DATE.OCC
DATE.OCC_string_2024_02_Feb <- gsub("AREA.NAME", "DATE.OCC", AREA.NAME_2024_02_Feb_string)
cat(DATE.OCC_string_2024_02_Feb)
# 2. Switch AREA (as a word) to DATE.OCC (variable prefix)
DATE.OCC_string_2024_02_Feb <- gsub("AREA", "DATE.OCC", DATE.OCC_string_2024_02_Feb)
cat(DATE.OCC_string_2024_02_Feb)
# paste below
# list all the DATE.OCC names
unique(LA_2024_02_Feb$DATE.OCC)
length(unique(LA_2024_02_Feb$DATE.OCC))
table(LA_2024_02_Feb$DATE.OCC)
DATE.OCC_2024_02_Feb <- table(LA_2024_02_Feb$DATE.OCC)
DATE.OCC_2024_02_Feb

# Max DATE.OCC
max_DATE.OCC_2024_02_Feb <- names(which.max(DATE.OCC_2024_02_Feb))
max_DATE.OCC_2024_02_Feb
max_count_DATE.OCC_2024_02_Feb <- max(DATE.OCC_2024_02_Feb)
max_count_DATE.OCC_2024_02_Feb

# Min DATE.OCC
min_DATE.OCC_2024_02_Feb <- names(which.min(DATE.OCC_2024_02_Feb))
min_DATE.OCC_2024_02_Feb
min_count_DATE.OCC_2024_02_Feb <- min(DATE.OCC_2024_02_Feb)
min_count_DATE.OCC_2024_02_Feb

cat("DATE.OCC Most incidents:
", max_DATE.OCC_2024_02_Feb, "with", max_count_DATE.OCC_2024_02_Feb, "incidents for 2024_02_Feb

")
cat("DATE.OCC Fewest incidents:
", min_DATE.OCC_2024_02_Feb, "with", min_count_DATE.OCC_2024_02_Feb, "incidents for 2024_02_Feb

")

# Sort DATE.OCC.name in decreasing order
Sorted_DATE.OCC_2024_02_Feb <- sort(DATE.OCC_2024_02_Feb, decreasing = TRUE)
head(Sorted_DATE.OCC_2024_02_Feb)
tail(Sorted_DATE.OCC_2024_02_Feb)

# Get top 3 most frequent DATE.OCC
top_3_Sorted_DATE.OCC_2024_02_Feb <- head(Sorted_DATE.OCC_2024_02_Feb, 3)
top_3_Sorted_DATE.OCC_2024_02_Feb

# Extract SOrted DATE.OCC, names and counts
top_3_Sorted_DATE.OCC_names_2024_02_Feb <- names(top_3_Sorted_DATE.OCC_2024_02_Feb)
top_3_Sorted_DATE.OCC_names_2024_02_Feb
top_3_Sorted_DATE.OCC_counts_2024_02_Feb <- as.numeric(top_3_Sorted_DATE.OCC_2024_02_Feb)
top_3_Sorted_DATE.OCC_counts_2024_02_Feb

# Print summary sentence
cat(paste0(
  top_3_Sorted_DATE.OCC_names_2024_02_Feb[1], " had the most incidents for 2024_02_Feb with ", top_3_Sorted_DATE.OCC_counts_2024_02_Feb[1], " cases; ",
  top_3_Sorted_DATE.OCC_names_2024_02_Feb[2], " followed with ", top_3_Sorted_DATE.OCC_counts_2024_02_Feb[2], " cases; and ",
  top_3_Sorted_DATE.OCC_names_2024_02_Feb[3], " had ", top_3_Sorted_DATE.OCC_counts_2024_02_Feb[3], " cases."
))

# Convert DATE.OCC strings to Date objects (strip time part first)
top_3_Dates_2024_02_Feb <- as.Date(sub(" .*", "", top_3_Sorted_DATE.OCC_names_2024_02_Feb), format = "%m/%d/%Y")
cat(top_3_Dates_2024_02_Feb)
# Get day of the week
top_3_Days_2024_02_Feb <- weekdays(top_3_Dates_2024_02_Feb)
cat(top_3_Days_2024_02_Feb)
# Print summary with day of the week
cat(paste0(
  top_3_Days_2024_02_Feb[1], ", ", top_3_Sorted_DATE.OCC_names_2024_02_Feb[1], " had the most incidents for 2024_02_Feb with ", top_3_Sorted_DATE.OCC_counts_2024_02_Feb[1], " cases; ",
  top_3_Days_2024_02_Feb[2], ", ", top_3_Sorted_DATE.OCC_names_2024_02_Feb[2], " followed with ", top_3_Sorted_DATE.OCC_counts_2024_02_Feb[2], " cases; and ",
  top_3_Days_2024_02_Feb[3], ", ", top_3_Sorted_DATE.OCC_names_2024_02_Feb[3], " had ", top_3_Sorted_DATE.OCC_counts_2024_02_Feb[3], " cases."
))



# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")


# days of the week with most crimes

# Convert DATE.OCC to Date (if not already a Date object)
# Assuming LA_2024_02_Feb$DATE.OCC is in character format like "02/16/2024 12:00:00 AM"
LA_2024_02_Feb$DATE_ONLY <- as.Date(sub(" .*", "", LA_2024_02_Feb$DATE.OCC), format = "%m/%d/%Y")

# Get the day of the week
LA_2024_02_Feb$DAY_OF_WEEK <- weekdays(LA_2024_02_Feb$DATE_ONLY)

# Create frequency table of crimes per day
LA_2024_02_Feb_Day_Of_Week_table <- table(LA_2024_02_Feb$DAY_OF_WEEK)
LA_2024_02_Feb_Day_Of_Week_table
# Sort in descending order
Sorted_LA_2024_02_Feb_Day_Of_Week_table <- sort(LA_2024_02_Feb_Day_Of_Week_table, decreasing = TRUE)
Sorted_LA_2024_02_Feb_Day_Of_Week_table
# Display the sorted table
print(Sorted_LA_2024_02_Feb_Day_Of_Week_table)

# Get top 3 days and their counts
top_3_days_LA_2024_02_Feb_day <- head(Sorted_LA_2024_02_Feb_Day_Of_Week_table, 3)
top_3_days_LA_2024_02_Feb_day
top_3_day_names_LA_2024_02_Feb <- names(top_3_days_LA_2024_02_Feb_day)
top_3_day_names_LA_2024_02_Feb
top_3_day_counts_LA_2024_02_Feb <- as.numeric(top_3_days_LA_2024_02_Feb_day)
top_3_day_counts_LA_2024_02_Feb
# Print summary sentence
cat(paste0(
  top_3_day_names_LA_2024_02_Feb[1], " had the most crimes with in 2024_02_Feb ", top_3_day_counts_LA_2024_02_Feb[1], " incidents; ",
  top_3_day_names_LA_2024_02_Feb[2], " followed with ", top_3_day_counts_LA_2024_02_Feb[2], " incidents; and ",
  top_3_day_names_LA_2024_02_Feb[3], " had ", top_3_day_counts_LA_2024_02_Feb[3], " incidents."
))


# Filter for Thursday crimes
Thursday_2024_02_Feb_table <- subset(
  LA_2024_02_Feb,
  DAY_OF_WEEK == "Thursday",
  select = c("DAY_OF_WEEK", "Crm.Cd.Desc", "AREA.NAME", "LOCATION", "Cross.Street",
             "Premis.Desc", "TIME.OCC", "DATE.OCC", "Vict.Age", "Vict.Sex", "Vict.Descent")
)
Thursday_2024_02_Feb_table
# Preview result
head(Thursday_2024_02_Feb_table)
tail(Thursday_2024_02_Feb_table)


# 1. Switch to Thursday scan AREA.NAME
cat(AREA.NAME_2024_02_Feb_string)
# Filter for Thursday crimes by AREA.NAME. switch out LA_2024_02_Feb to Thursday_2024_02_Feb_table
Thursday_AREA.NAME_2024_02_Feb_string <- gsub("LA_2024_02_Feb", "Thursday_2024_02_Feb_table", AREA.NAME_2024_02_Feb_string)
cat(Thursday_AREA.NAME_2024_02_Feb_string)
Thursday_AREA.NAME_2024_02_Feb_string <- gsub("AREA.NAME_2024_02_Feb", "Thursday_AREA.NAME_2024_02_Feb", Thursday_AREA.NAME_2024_02_Feb_string)
cat(Thursday_AREA.NAME_2024_02_Feb_string)
# Paste output
# list all the AREA.NAME names
unique(Thursday_2024_02_Feb_table$AREA.NAME)
length(unique(Thursday_2024_02_Feb_table$AREA.NAME))
table(Thursday_2024_02_Feb_table$AREA.NAME)
Thursday_AREA.NAME_2024_02_Feb <- table(Thursday_2024_02_Feb_table$AREA.NAME)
Thursday_AREA.NAME_2024_02_Feb

# Max AREA_NAME
max_Thursday_AREA.NAME_2024_02_Feb <- names(which.max(Thursday_AREA.NAME_2024_02_Feb))
max_Thursday_AREA.NAME_2024_02_Feb
max_count_Thursday_AREA.NAME_2024_02_Feb <- max(Thursday_AREA.NAME_2024_02_Feb)
max_count_Thursday_AREA.NAME_2024_02_Feb

# Min AREA_NAME
min_Thursday_AREA.NAME_2024_02_Feb <- names(which.min(Thursday_AREA.NAME_2024_02_Feb))
min_Thursday_AREA.NAME_2024_02_Feb
min_count_Thursday_AREA.NAME_2024_02_Feb <- min(Thursday_AREA.NAME_2024_02_Feb)
min_count_Thursday_AREA.NAME_2024_02_Feb

cat("AREA Most incidents:
", max_Thursday_AREA.NAME_2024_02_Feb, "with", max_count_Thursday_AREA.NAME_2024_02_Feb, "incidents for 2024_02_Feb

")
cat("AREA Fewest incidents:
", min_Thursday_AREA.NAME_2024_02_Feb, "with", min_count_Thursday_AREA.NAME_2024_02_Feb, "incidents for 2024_02_Feb

")

# Sort AREA.name in decreasing order
Sorted_Thursday_AREA.NAME_2024_02_Feb <- sort(Thursday_AREA.NAME_2024_02_Feb, decreasing = TRUE)
head(Sorted_Thursday_AREA.NAME_2024_02_Feb)
tail(Sorted_Thursday_AREA.NAME_2024_02_Feb)

# Get top 3 most frequent AREA_NAME
top_3_Sorted_Thursday_AREA.NAME_2024_02_Feb <- head(Sorted_Thursday_AREA.NAME_2024_02_Feb, 3)
top_3_Sorted_Thursday_AREA.NAME_2024_02_Feb

# Extract SOrted AREA_NAME, names and counts
top_3_Sorted_Thursday_AREA.NAME_names_2024_02_Feb <- names(top_3_Sorted_Thursday_AREA.NAME_2024_02_Feb)
top_3_Sorted_Thursday_AREA.NAME_names_2024_02_Feb
top_3_Sorted_Thursday_AREA.NAME_counts_2024_02_Feb <- as.numeric(top_3_Sorted_Thursday_AREA.NAME_2024_02_Feb)
top_3_Sorted_Thursday_AREA.NAME_counts_2024_02_Feb

# Print summary sentence
cat(paste0(
  top_3_Sorted_Thursday_AREA.NAME_names_2024_02_Feb [1], " had the most incidents on Thursdays for 2024_02_Feb with ", top_3_Sorted_Thursday_AREA.NAME_counts_2024_02_Feb [1], " cases; ",
  top_3_Sorted_Thursday_AREA.NAME_names_2024_02_Feb [2], " followed with ", top_3_Sorted_Thursday_AREA.NAME_counts_2024_02_Feb [2], " cases; and ",
  top_3_Sorted_Thursday_AREA.NAME_names_2024_02_Feb [3], " had ", top_3_Sorted_Thursday_AREA.NAME_counts_2024_02_Feb[3], " cases."
))




# Filter for Thursday crimes to Central area
Thursday_Central_2024_02_Feb_table <- subset(
  Thursday_2024_02_Feb_table,
  AREA.NAME == "Central",
  select = c("DAY_OF_WEEK", "Crm.Cd.Desc", "AREA.NAME", "LOCATION", "Cross.Street",
             "Premis.Desc", "TIME.OCC", "DATE.OCC", "Vict.Age", "Vict.Sex", "Vict.Descent")
)
head(Thursday_2024_02_Feb_table)
# Preview result
head(Thursday_Central_2024_02_Feb_table)
tail(Thursday_Central_2024_02_Feb_table)



# 1. Switch to Thursday Central scan LOCATION
cat(Thursday_AREA.NAME_2024_02_Feb_string)
# Filter for Thursday crimes by AREA.NAME. switch out LA_2024_02_Feb to Thursday_2024_02_Feb_table
Thursday_Central_LOCATION_2024_02_Feb_string <- gsub("Thursday_2024_02_Feb_table", "Thursday_Central_2024_02_Feb_table", Thursday_AREA.NAME_2024_02_Feb_string)
cat(Thursday_Central_LOCATION_2024_02_Feb_string)
Thursday_Central_LOCATION_2024_02_Feb_string <- gsub("AREA.NAME", "LOCATION", Thursday_Central_LOCATION_2024_02_Feb_string)
cat(Thursday_Central_LOCATION_2024_02_Feb_string)
Thursday_Central_LOCATION_2024_02_Feb_string <- gsub("Thursday_LOCATION", "Thursday_Central_LOCATION", Thursday_Central_LOCATION_2024_02_Feb_string)
cat(Thursday_Central_LOCATION_2024_02_Feb_string)
Thursday_Central_LOCATION_2024_02_Feb_string <- gsub("top_3_Sorted_LOCATION", "top_3_Sorted_Thursday_Central_LOCATION", Thursday_Central_LOCATION_2024_02_Feb_string)
cat(Thursday_Central_LOCATION_2024_02_Feb_string)
# Paste output
# list all the LOCATION names
unique(Thursday_Central_2024_02_Feb_table$LOCATION)
length(unique(Thursday_Central_2024_02_Feb_table$LOCATION))
table(Thursday_Central_2024_02_Feb_table$LOCATION)
Thursday_Central_LOCATION_2024_02_Feb <- table(Thursday_Central_2024_02_Feb_table$LOCATION)
Thursday_Central_LOCATION_2024_02_Feb

# Max LOCATION
max_Thursday_Central_LOCATION_2024_02_Feb <- names(which.max(Thursday_Central_LOCATION_2024_02_Feb))
max_Thursday_Central_LOCATION_2024_02_Feb
max_count_Thursday_Central_LOCATION_2024_02_Feb <- max(Thursday_Central_LOCATION_2024_02_Feb)
max_count_Thursday_Central_LOCATION_2024_02_Feb

# Min LOCATION
min_Thursday_Central_LOCATION_2024_02_Feb <- names(which.min(Thursday_Central_LOCATION_2024_02_Feb))
min_Thursday_Central_LOCATION_2024_02_Feb
min_count_Thursday_Central_LOCATION_2024_02_Feb <- min(Thursday_Central_LOCATION_2024_02_Feb)
min_count_Thursday_Central_LOCATION_2024_02_Feb

cat("AREA Most incidents:
", max_Thursday_Central_LOCATION_2024_02_Feb, "with", max_count_Thursday_Central_LOCATION_2024_02_Feb, "incidents for 2024_02_Feb

")
cat("AREA Fewest incidents:
", min_Thursday_Central_LOCATION_2024_02_Feb, "with", min_count_Thursday_Central_LOCATION_2024_02_Feb, "incidents for 2024_02_Feb

")

# Sort AREA.name in decreasing order
Sorted_Thursday_Central_LOCATION_2024_02_Feb <- sort(Thursday_Central_LOCATION_2024_02_Feb, decreasing = TRUE)
head(Sorted_Thursday_Central_LOCATION_2024_02_Feb)
tail(Sorted_Thursday_Central_LOCATION_2024_02_Feb)

# Get top 3 most frequent LOCATION
top_3_Sorted_Thursday_Central_LOCATION_2024_02_Feb <- head(Sorted_Thursday_Central_LOCATION_2024_02_Feb, 3)
top_3_Sorted_Thursday_Central_LOCATION_2024_02_Feb

# Extract SOrted LOCATION, names and counts
top_3_Sorted_Thursday_Central_LOCATION_names_2024_02_Feb <- names(top_3_Sorted_Thursday_Central_LOCATION_2024_02_Feb)
top_3_Sorted_Thursday_Central_LOCATION_names_2024_02_Feb
top_3_Sorted_Thursday_Central_LOCATION_counts_2024_02_Feb <- as.numeric(top_3_Sorted_Thursday_Central_LOCATION_2024_02_Feb)
top_3_Sorted_Thursday_Central_LOCATION_counts_2024_02_Feb

# Print summary sentence
cat(paste0(
  top_3_Sorted_Thursday_Central_LOCATION_names_2024_02_Feb[1], " had the most incidents for 2024_02_Feb with ", top_3_Sorted_Thursday_Central_LOCATION_counts_2024_02_Feb[1], " cases; ",
  top_3_Sorted_Thursday_Central_LOCATION_names_2024_02_Feb[2], " followed with ", top_3_Sorted_Thursday_Central_LOCATION_counts_2024_02_Feb[2], " cases; and ",
  top_3_Sorted_Thursday_Central_LOCATION_names_2024_02_Feb[3], " had ", top_3_Sorted_Thursday_Central_LOCATION_counts_2024_02_Feb[3], " cases."
))


# Filter for Thursday in Central area for Crm.Cd.Desc
Thursday_Central_Crm.Cd.Desc_2024_02_Feb_string <- gsub("LOCATION", "Crm.Cd.Desc", Thursday_Central_LOCATION_2024_02_Feb_string)
cat(Thursday_Central_Crm.Cd.Desc_2024_02_Feb_string)
Thursday_Central_Crm.Cd.Desc_2024_02_Feb_string <- gsub("Thursday_Central_LOCATION", "Thursday_Central_Crm.Cd.Desc", Thursday_Central_LOCATION_2024_02_Feb_string)
cat(Thursday_Central_Crm.Cd.Desc_2024_02_Feb_string)
# paste below, manually type in Crm.Cd.Desc
# list all the LOCATION names
unique(Thursday_Central_2024_02_Feb_table$Crm.Cd.Desc)
length(unique(Thursday_Central_2024_02_Feb_table$Crm.Cd.Desc))
table(Thursday_Central_2024_02_Feb_table$Crm.Cd.Desc)
Thursday_Central_Crm.Cd.Desc_2024_02_Feb <- table(Thursday_Central_2024_02_Feb_table$Crm.Cd.Desc)
head(Thursday_Central_Crm.Cd.Desc_2024_02_Feb)

# Max Crm.Cd.Desc
max_Thursday_Central_Crm.Cd.Desc_2024_02_Feb <- names(which.max(Thursday_Central_Crm.Cd.Desc_2024_02_Feb))
max_Thursday_Central_Crm.Cd.Desc_2024_02_Feb
max_count_Thursday_Central_Crm.Cd.Desc_2024_02_Feb <- max(Thursday_Central_Crm.Cd.Desc_2024_02_Feb)
max_count_Thursday_Central_Crm.Cd.Desc_2024_02_Feb

# Min Crm.Cd.Desc for Thursday Central area
min_Thursday_Central_Crm.Cd.Desc_2024_02_Feb <- names(which.min(Thursday_Central_Crm.Cd.Desc_2024_02_Feb))
min_Thursday_Central_Crm.Cd.Desc_2024_02_Feb
min_count_Thursday_Central_Crm.Cd.Desc_2024_02_Feb <- min(Thursday_Central_Crm.Cd.Desc_2024_02_Feb)
min_count_Thursday_Central_Crm.Cd.Desc_2024_02_Feb

cat("AREA Most incidents:
", max_Thursday_Central_Crm.Cd.Desc_2024_02_Feb, "with", max_count_Thursday_Central_Crm.Cd.Desc_2024_02_Feb, "incidents for 2024_02_Feb

")
cat("AREA Fewest incidents:
", min_Thursday_Central_Crm.Cd.Desc_2024_02_Feb, "with", min_count_Thursday_Central_Crm.Cd.Desc_2024_02_Feb, "incidents for 2024_02_Feb

")

# Sort AREA.name in decreasing order
Sorted_Thursday_Central_Crm.Cd.Desc_2024_02_Feb <- sort(Thursday_Central_Crm.Cd.Desc_2024_02_Feb, decreasing = TRUE)
head(Sorted_Thursday_Central_Crm.Cd.Desc_2024_02_Feb)
tail(Sorted_Thursday_Central_Crm.Cd.Desc_2024_02_Feb)

# Get top 3 most frequent LOCATION
top_3_Sorted_Thursday_Central_Crm.Cd.Desc_2024_02_Feb <- head(Sorted_Thursday_Central_Crm.Cd.Desc_2024_02_Feb, 3)
top_3_Sorted_Thursday_Central_Crm.Cd.Desc_2024_02_Feb

# Extract SOrted LOCATION, names and counts
top_3_Sorted_Thursday_Central_Crm.Cd.Desc_names_2024_02_Feb <- names(top_3_Sorted_Thursday_Central_Crm.Cd.Desc_2024_02_Feb)
top_3_Sorted_Thursday_Central_Crm.Cd.Desc_names_2024_02_Feb
top_3_Sorted_Thursday_Central_Crm.Cd.Desc_counts_2024_02_Feb <- as.numeric(top_3_Sorted_Thursday_Central_Crm.Cd.Desc_2024_02_Feb)
top_3_Sorted_Thursday_Central_Crm.Cd.Desc_counts_2024_02_Feb

# Print summary sentence
cat(paste0(
  top_3_Sorted_Thursday_Central_Crm.Cd.Desc_names_2024_02_Feb[1], " had the most incidents on Thursday in the Central area for 2024_02_Feb with ", top_3_Sorted_Thursday_Central_Crm.Cd.Desc_counts_2024_02_Feb[1], " cases; ",
  top_3_Sorted_Thursday_Central_Crm.Cd.Desc_names_2024_02_Feb[2], " followed with ", top_3_Sorted_Thursday_Central_Crm.Cd.Desc_counts_2024_02_Feb[2], " cases; and ",
  top_3_Sorted_Thursday_Central_Crm.Cd.Desc_names_2024_02_Feb[3], " had ", top_3_Sorted_Thursday_Central_Crm.Cd.Desc_counts_2024_02_Feb[3], " cases."
))


# Filter for Thursday in Central area for TIME.OCC
Thursday_Central_TIME.OCC_2024_02_Feb_string <- gsub("Crm.Cd.Desc","TIME.OCC", Thursday_Central_Crm.Cd.Desc_2024_02_Feb_string)
cat(Thursday_Central_TIME.OCC_2024_02_Feb_string)
# paste below, manually type in TIME.OCC
# list all the LOCATION names
unique(Thursday_Central_2024_02_Feb_table$TIME.OCC)
length(unique(Thursday_Central_2024_02_Feb_table$TIME.OCC))
table(Thursday_Central_2024_02_Feb_table$TIME.OCC)
Thursday_Central_TIME.OCC_2024_02_Feb <- table(Thursday_Central_2024_02_Feb_table$TIME.OCC)
Thursday_Central_TIME.OCC_2024_02_Feb
# Max TIME.OCC
max_Thursday_Central_TIME.OCC_2024_02_Feb <- names(which.max(Thursday_Central_TIME.OCC_2024_02_Feb))
max_Thursday_Central_TIME.OCC_2024_02_Feb
max_count_Thursday_Central_TIME.OCC_2024_02_Feb <- max(Thursday_Central_TIME.OCC_2024_02_Feb)
max_count_Thursday_Central_TIME.OCC_2024_02_Feb
# Min TIME.OCC
min_Thursday_Central_TIME.OCC_2024_02_Feb <- names(which.min(Thursday_Central_TIME.OCC_2024_02_Feb))
min_Thursday_Central_TIME.OCC_2024_02_Feb
min_count_Thursday_Central_TIME.OCC_2024_02_Feb <- min(Thursday_Central_TIME.OCC_2024_02_Feb)
min_count_Thursday_Central_TIME.OCC_2024_02_Feb

cat("AREA Most incidents:
", max_Thursday_Central_TIME.OCC_2024_02_Feb, "with", max_count_Thursday_Central_TIME.OCC_2024_02_Feb, "incidents for 2024_02_Feb

")
cat("AREA Fewest incidents:
", min_Thursday_Central_TIME.OCC_2024_02_Feb, "with", min_count_Thursday_Central_TIME.OCC_2024_02_Feb, "incidents for 2024_02_Feb

")

# Sort AREA.name in decreasing order
Sorted_Thursday_Central_TIME.OCC_2024_02_Feb <- sort(Thursday_Central_TIME.OCC_2024_02_Feb, decreasing = TRUE)
head(Sorted_Thursday_Central_TIME.OCC_2024_02_Feb)
tail(Sorted_Thursday_Central_TIME.OCC_2024_02_Feb)

# Get top 3 most frequent LOCATION
top_3_Sorted_Thursday_Central_TIME.OCC_2024_02_Feb <- head(Sorted_Thursday_Central_TIME.OCC_2024_02_Feb, 3)
top_3_Sorted_Thursday_Central_TIME.OCC_2024_02_Feb

# Extract SOrted LOCATION, names and counts
top_3_Sorted_Thursday_Central_TIME.OCC_names_2024_02_Feb <- names(top_3_Sorted_Thursday_Central_TIME.OCC_2024_02_Feb)
top_3_Sorted_Thursday_Central_TIME.OCC_names_2024_02_Feb
top_3_Sorted_Thursday_Central_TIME.OCC_counts_2024_02_Feb <- as.numeric(top_3_Sorted_Thursday_Central_TIME.OCC_2024_02_Feb)
top_3_Sorted_Thursday_Central_TIME.OCC_counts_2024_02_Feb

# Print summary sentence
cat(paste0(
  top_3_Sorted_Thursday_Central_TIME.OCC_names_2024_02_Feb[1], " had the most incidents on Thursdays in the Central area for 2024_02_Feb with ", top_3_Sorted_Thursday_Central_TIME.OCC_counts_2024_02_Feb[1], " cases; ",
  top_3_Sorted_Thursday_Central_TIME.OCC_names_2024_02_Feb[2], " followed with ", top_3_Sorted_Thursday_Central_TIME.OCC_counts_2024_02_Feb[2], " cases; and ",
  top_3_Sorted_Thursday_Central_TIME.OCC_names_2024_02_Feb[3], " had ", top_3_Sorted_Thursday_Central_TIME.OCC_counts_2024_02_Feb[3], " cases."
))





# list all the variables (columns)
cat(names(LA_2024_02_Feb), sep = "\n")





### remove all memory from R console
rm(list = ls())
ls()
list.files()
dir()
