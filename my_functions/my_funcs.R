###########write a single csv file from a each excel sheet#######################

xls_2_csv <- function(sheet, path) {
  #extracts filename component without ext
  fname_no_ext <- path %>%
    basename() %>%              #extracts filename.ext from path
    tools::file_path_sans_ext() #removes .ext from filename
  
  #extracts sheet[[x]] from xls file and writes it to a csv file with the name <sheet[[x]]>.csv
  path %>%
    read_excel(sheet = sheet) %>% 
    write_csv(paste0("data\\", fname_no_ext, "_", sheet, ".csv"))
}


######################  convert all sheets in xls file to csv files and store in data\\ directory######################

extract_all_csvs <- function(fpath){
  
  sheet_names <- fpath %>%
    excel_sheets() %>%  #extracts sheet names from .xlsx and stores in char vector
    purrr::set_names()  # values given to attributes for all vector elements 
  
  # sheets from xls file saved as <sheet>.csv files
  map2(sheet_names, fpath, xls_2_csv) 
}


########################################################################################################################
############# csv_add_year function removes redundant first two lines of csv files ######################################
############# Extracts year from filename and stores in new "year" field of tibble #####################################
#######################################################(#################################################################

csv_add_year <- function(file){

df <- read_csv(file, skip=2, n_max = 2087) #extract records only from csv file
yr <- stringr::str_extract(file,"\\d{4}")  ## assumes that a four digit number in the name is the representative year
df %>% 
  mutate(year = as.integer(paste0(yr))) %>% 
  select(year, everything())
}

###########################################################################################################################
## df_tidy cleans  tibble.  tibble converted to long data, field names are cleaned, chars converted to factors and NA rows are dropped
########################################################################################################################### 


########### remove wspaces in fieldnames and replace with _ : convert to lower case ######
tidy_colnames <- function(df_loc){
  names(df_loc) <- names(df_loc) %>% 
    str_replace_all(" ", "_") %>% 
    tolower()
  df_loc
}

############## converts field type from char to factor : age levels are ordered.  Removes rows with NA values ###########
tidy_field_type <- function(df_loc){
  df_loc %>% 
    mutate_if(is.character, as_factor) %>% 
    drop_na()
}

########### df_tidy : each sub_function is piped into the next - hence only need to declare var for 1st sub_function ########
df_tidy <- function(df){
  df %>%  
    pivot_longer(cols = -c(1:6), names_to = "age", values_to = "freq") %>%   
     tidy_colnames() %>%
     tidy_field_type()
}

##############################################################################################################################
###############################################################################################################################
###########  csvpopfile converts population csv file into a tibble ready for  #################################################

csvpopfile <- function(file){
  #selects years 2012-2014
  df <- read_csv(file, skip=5) %>% 
    select(num_range("X",c(1:6))) 
  
  # rename colnames according to 1st row of data. remove 1st row
  names(df) <- df %>%   
    slice(1) %>%   
    as.character() 
  
  # extract gender from filename and create field with this value
  # remove 1st row from df
  # convert data into long format
  # tidy fieldnames
  sex <- str_extract(file,"(Fem|M)ale")
  df <- df %>%   
    mutate(Sex = sex ) %>%   
    slice(c(-1)) %>%   
    pivot_longer(4:6, names_to = "year", values_to = "pop") %>%   
    tidy_colnames()
}

##########################################################################################################
#######################  02- EDA functions ################################################################
############################################################################################################

# allows multiple arguments to be used by group_by -  place . in front of vars when using {{}}
sum_func <- function(..., data = df_css_eng, sumvar=freq ){
  data %>% 
    group_by(...) %>%
    summarise(freq = sum({{sumvar}}))
}
