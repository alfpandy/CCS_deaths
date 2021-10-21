###########write a single csv file from a each excel sheet#######################
## requires vroom and glue packages ##############################################
sheet_to_csv <- function(sheet_name, filepath, csv_dir){
  filename_noext <- filepath |> 
    basename() |>                   #extracts filename.ext from path 
    tools::file_path_sans_ext()     #removes .ext from filename - needed to write vroom_write command

  #extracts sheet[[x]] from xls file and writes it to a csv file with the name <sheet[[x]]>.csv
  filepath |> 
    read_excel(sheet = sheet_name) |> 
    vroom::vroom_write(glue::glue(csv_dir,"/", filename_noext , "_", sheet_name, ".csv"), delim = ",")
  # csv sent to subdirectory <csv_dir> defined in function (xls_to_csvs)
}

######################  convert all sheets in xls file to csv files and store in <dirname> directory##############

xls_to_csvs <- function(filepath, subdir = ""){
  
  csv_dir <- glue::glue(dirname(filepath),"/",subdir) #create subdirectory path to store csv files from xls 
  if(!file.exists(csv_dir)) dir.create(csv_dir)   #creates subdirectory
  
  sheet_names <- filepath  |> 
    excel_sheets()   #extracts sheet names from .xlsx and stores in char vector
  
  #sheets from xls file saved as <sheet>.csv files
  pmap(list(sheet_names, filepath, csv_dir), sheet_to_csv)
}

## removes values ... in filelist vector 
drop_csvs <- function(filelist,...) {
  drop_sheetnames <- list(...)
  filelist[!filelist %in% unlist(drop_sheetnames)]
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
  df <- read_csv(file, skip=5) |> 
    select(1:6) #select only 1st 6 columns containing 2012-14 pop data
  
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
sum_func <- function(..., data = df_deaths_pop, sumvar=freq ){
  data %>% 
    group_by(...) %>%
    summarise(freq = sum({{sumvar}}))
}

rate_func <- function(..., data = df_deaths_pop){
  data %>% 
    group_by(...) %>%
    summarise(death_rate = sum(freq)/sum(pop))
}




###############################################################################################################
#  EAR creates df of EASR values. CSV file required for input.
#  if datafile contains sex data then this is included in the output ##

EAR <- function(dat){
   df <- read_csv(dat) 
   df|>
    mutate(age = str_extract(AgeGroup, "^[0-9\\-]+"), .keep = c ("unused")) |> 
    mutate(EuropeanStandardPopulation = as.integer(EuropeanStandardPopulation)) |> 
    mutate(age = recode(age, "90" = "90+"))
    #mutate(sex = {if("Sex" %in% names(df)) Sex else NULL}, .keep = c ("unused")
   #The above codeline is interesting to know but not required)
}

#This function calculates Direct Standardised Rate for input variables
DSR <- function(..., data=LA_age_stand_rate){
  data |> 
    group_by(...) |> 
    summarise(total_expected_cases = sum(Expected_cases)) |> 
    mutate(age_adj_rate_1000 = total_expected_cases/100)
}

############################################################################
###### plots for EAR ######################################################

#need rlang package
#only doublebrace column names in tibble
plots <- function(dat,x, reorder_var, facet_var, y_lab, x_lab, Title){
  dat |> 
    ggplot(aes(x={{x}}, fct_reorder({{reorder_var}}, {{x}})))+
    geom_bar(stat="identity")+
    labs(y= y_lab, x = x_lab)+
    ggtitle(Title)+
    #coord_flip()+
    #scale_y_discrete(limits = rev)+
    theme(axis.text.x = element_text(angle = 0))+
    theme_bw()+
    facet_wrap(vars({{facet_var}}))
}

line_plot <- function(dat, x, y, grp, y_lab){
ggplot(dat, aes(x={{x}}, y = {{y}}, colour = {{grp}}))+
  geom_line()+
  scale_x_continuous(breaks = c(2012,2013,2014))+
  #scale_y_continuous(breaks = seq(1500, 4500, 1000))+
  labs(y=y_lab)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
}
##############
#determine the highest ranking levels for variables ... 
# n = top_n ranks

top_order <- function(n, ...) {
  DSR(...) |>
    arrange(desc(age_adj_rate_1000)) |> 
    distinct(local_authority) |> 
    head(n)
} 

######### old replaced functions #####################################################################


######### xls_2_csv replaced by sheet_to_csv ###############

# xls_2_csv <- function(sheet, path) {
#   #extracts filename component without ext
#   fname_no_ext <- path %>%
#     basename() %>%              #extracts filename.ext from path
#     tools::file_path_sans_ext() #removes .ext from filename
#   
#   #extracts sheet[[x]] from xls file and writes it to a csv file with the name <sheet[[x]]>.csv
#   path %>%
#     read_excel(sheet = sheet) %>% 
#     write_csv(paste0("data\\", fname_no_ext, "_", sheet, ".csv"))
# }

######################  convert all sheets in xls file to csv files and store in data\\ directory######################

# extract_all_csvs <- function(fpath){
#   
#   sheet_names <- fpath %>%
#     excel_sheets() %>%  #extracts sheet names from .xlsx and stores in char vector
#     purrr::set_names()  # values given to attributes for all vector elements 
#   
#   # sheets from xls file saved as <sheet>.csv files
#   map2(sheet_names, fpath, xls_2_csv) 
#}