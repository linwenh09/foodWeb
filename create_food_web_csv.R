
library(readxl)

# Function to generate column names (A, B, ..., Z, AA, AB, ...)
generate_column_names <- function(node_count) {
  if (node_count <= 26) {
    return(LETTERS[1:node_count])
  } else {
    # Generate names for columns beyond 'Z'
    extra_cols <- ceiling(node_count / 26) - 1
    col_names <- c(LETTERS)
    for (i in 1:extra_cols) {
      for (j in LETTERS) {
        col_names <- c(col_names, paste0(LETTERS[i], j))
      }
    }
    return(col_names[1:node_count])
  }
}

# Function to create a CSV file for a food web
create_food_web_csv <- function(food_web_id, node_count) {
  # Generating column names
  col_names <- generate_column_names(node_count)
  
  # Creating a data frame with all values set to 1
  data <- as.data.frame(matrix(1, nrow = 1, ncol = node_count))
  names(data) <- col_names
  
  # Adding the food web ID at the beginning
  data <- cbind(FoodWebID = food_web_id, data)
  
  # Writing the data frame to a CSV file
  write.csv(data, paste0(food_web_id, '_abun.csv'), row.names = FALSE)
}

# Read the Excel file
file_path <- 'foodWeb_structures.xlsx' # Update this with the correct path
food_web_data <- read_excel(file_path)

# Loop over each food web and create a CSV file
for (i in 1:nrow(food_web_data)) {
  food_web_id <- food_web_data$FoodWebID[i]
  node_count <- food_web_data$Node[i]
  create_food_web_csv(food_web_id, node_count)
}
