library(tidyverse)
library(beeswarm)

# 1. Read and Clean Data --------------------------------------------------

unemployment <- read_csv('data/maker-brexit.csv') %>%
  # Rename columns by position
  rename(
    con  = 1,
    val  = 2,
    rank = 3
  ) %>%
  # parse_number() automatically handles the "%" and converts to numeric
  mutate(val = parse_number(val))


# 2. Calculate Beeswarm Coordinates ---------------------------------------

# We run the calculation and store it in 'bs_coords'
bs_coords <- beeswarm(
  unemployment$val,
  cex        = 0.6,
  method     = "center",
  horizontal = TRUE,
  do.plot    = FALSE
)

# Add the calculated coordinates back to our main dataframe
# In beeswarm output: 'x' is the jitter/offset and 'y' is the original value
unemployment$swarm_offset <- bs_coords$x
unemployment$swarm_val    <- bs_coords$y


# 3. Export ---------------------------------------------------------------

# Export the full dataframe (includes con, val, rank, and the new coordinates)
write_csv(unemployment, "beeswarm_data_full.csv")


#########
library(tidyverse)
library(beeswarm)
library(googlesheets4) # You may need to install this: install.packages("googlesheets4")

# 1. Setup & Google Auth --------------------------------------------------

# This will prompt a window in your browser to log into your Google account
gs4_auth() 

input_files <- list.files(path = "data", pattern = "^maker-.*\\.csv$", full.names = TRUE)

# Create a brand new Google Sheet file
# You can name it whatever you like here
ss <- gs4_create("Beeswarm Data Master File")

# 2. The Processing & Upload Function -------------------------------------

process_and_upload <- function(file_path, sheet_id) {
  
  file_name <- basename(file_path)
  message(paste("Processing & Uploading:", file_name))
  
  # Clean and calculate (same logic as before)
  df <- read_csv(file_path, na = c("", "NA", "#N/A"), show_col_types = FALSE, name_repair = "minimal")
  colnames(df)[1:3] <- c("con", "val", "rank")
  
  df_clean <- df %>%
    mutate(val = as.numeric(parse_number(as.character(val)))) %>%
    drop_na(val)
  
  bs_coords <- try(beeswarm(df_clean$val, cex = 0.6, method = "center", horizontal = TRUE, do.plot = FALSE), silent = TRUE)
  
  if(inherits(bs_coords, "try-error")) {
    message(paste("   ⚠️ Skipping", file_name))
    return(NULL)
  }
  
  df_clean$swarm_offset <- bs_coords$x
  df_clean$swarm_val    <- bs_coords$y
  
  # Create a tab name from the filename (e.g., "maker-brexit")
  tab_name <- str_remove(file_name, ".csv")
  
  # Write the data to a new sheet (tab) in our Google Sheet
  sheet_write(df_clean, ss = sheet_id, sheet = tab_name)
}

# 3. Execution ------------------------------------------------------------

# Loop through and upload each file as a new tab
walk(input_files, ~process_and_upload(.x, ss))

# Optional: Delete the default "Sheet1" that Google creates automatically
sheet_delete(ss, "Sheet1")

message("--------------------------------------------------")
message("Done! Your Google Sheet is ready in your Drive.")
