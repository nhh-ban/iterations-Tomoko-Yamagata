# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests


#Create a function to check the column names of input dataframe
test_stations_metadata_colnames <-
  function(df) {
    
    #Create a vector which contains the columns' names
    #of the dataframe created by the function "transform_metadata_to_df" 
    expected_colnames <- c("id", "name", "latestData", "lat", "lon")
    
    #If the column names of "df" is the same with the columnnames 
    #created by the function "transform_metadata_to_df", print the PASS message
    if (all(colnames(df) == expected_colnames) == TRUE) {
      print("PASS: Data has the correct columns")
      # otherwise, print the FAIL... massage
    } else{
      print("FAIL: Columns do not match the correct specification")
    }
  }


# Create a function to check the row numbers of the dataframe
test_stations_metadata_nrows <-
  function(df) {
    
    #Define the range of expected row volume of the input dataframe
    min_expected_rows <- 5000
    max_expected_rows <- 10000

    #If the total rows of the input dataframe is between 5000 and 10000
    #print "PASS~~~" message. Otherwise, print "FAIL" messages
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) {
      print("PASS: Data has a reasonable number of rows")
    } else if (nrow(df) <= min_expected_rows) {
      print("FAIL: Data has suspiciously few rows")
    } else {
      print("FAIL: Data has suspiciously many rows")
    }
  }

#Create a function to check the data types of each column of the input dataframe
test_stations_metadata_coltypes <-
  function(df) {
    expected_coltypes <-
      c("character", "character", "double", "double", "double")
    
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      print("PASS: All cols have the correct specifications")
    } else{
      print("FAIL: Columns do not have the correct specification")
    }
  }
  
#Create a function to check the missing values in the input dataframe
test_stations_metadata_nmissing <-
  function(df) {
    max_miss_vals <- 200

    #If the total number of NA values are smaller than 200,
    #print "PASS~~"message. Otherwise, print "FAIL" message
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {
      print("PASS: Amount of missing values is reasonable")
    } else {
      print("FAIL: Too many missing values in data set")
    }
  }

#Create a function to check timezone of latesteDate column in the input dataframe
test_stations_metadata_latestdata_timezone <-
  function(df) {
    
    if (attr(df$latestData,"tzone")=="UTC") {
      print("PASS: latestData has UTC-time zone")
    } else {
      print("FAIL: latestData does not have expected UTC-time zone")
    }
  }

# Create an aggregated function to set all test functions above
test_stations_metadata <- 
  function(df){
    test_stations_metadata_colnames(df)
    test_stations_metadata_coltypes(df)
    test_stations_metadata_nmissing(df)
    test_stations_metadata_nrows(df)
    test_stations_metadata_latestdata_timezone(df)
  }





