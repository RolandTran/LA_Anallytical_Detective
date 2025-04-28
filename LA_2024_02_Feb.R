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

cat(names(LA_2024_02_Feb), sep = "\n")
sum(LA_2024_02_Feb$AREA.NAME == "Hollywood")
max(LA_2024_02_Feb$Crm.Cd.Desc)
sum(LA_2024_02_Feb$Crm.Cd.Desc == "VIOLATION OF TEMPORARY RESTRAINING ORDER")
length(unique(LA_2024_02_Feb$Crm.Cd.Desc))
unique(LA_2024_02_Feb$Crm.Cd.Desc)
unique(LA_2024_02_Feb$Premis.Desc)
length(unique(LA_2024_02_Feb$Premis.Desc))
max((LA_2024_02_Feb$Premis.Desc))
sum(LA_2024_02_Feb$Premis.Desc == "YARD (RESIDENTIAL/BUSINESS)")
    