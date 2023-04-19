# Social Data Commons Data Tracker
#   - Creates a spreadsheet of data we have collected for given years and geographies.  
#   - Tracks data in Social Data Commons PUBLIC repos only (not DEV repos).
# DID NOT USE THIS...PORTED CODE TO PYTHON TO BE RUN IN SDC.ALL_DEV

library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(crayon)

#
# set path for repo locations ----------------
#

repos_dir <- "./test"  # directory that contains repos

# if . in repos_dir
if(grepl(".", repos_dir))  
{
  repos_dir = sub(".", getwd(), repos_dir)  # input path
}


#
# extract paths for all data/distribution folders ---------------------
#

folders = list.dirs(repos_dir)

data_folders = folders[grepl("data/distribution", folders, fixed=TRUE)]


#
# CREATE TRACKER -------------------------------------
#

tracker_df = data.frame(CATEGORY = character(), MEASURE = character(), START_YEAR = character(), 
                        END_YEAR = character())

# loop through data folders ----------------------

for(data_folder in data_folders)
{
  #data_folder = "~/SDC/test/sdc.broadband/Food and Nutrition Assistance/Supplemental Nutrition Assistance Program (SNAP)/data/distribution"
  
  # extract category from data folder name
  data_cat <- str_extract(data_folder, "(?<=sdc.)(.*?)(?=/)")
  
  # loop through data files in each folder -----------------
  
  for(data_file in list.files(data_folder))
  {
    print(paste(data_folder, data_file, sep = "/"))  
    
    # do nothing for measure_info files or non-NCR files
    
    if(data_file != "measure_info.json") # && substr(data_file,1,3) == "ncr") 
    {
      # data file will have .csv.xz extension
      
      #data_file = "dcmdva_hdcttrbg_2019_2021q3_percent_above_threshold.csv.xz"
      
      f_ext <- tools::file_ext(data_file)
      
      if(f_ext == "csv")
      {
        df <- readr::read_csv(paste0(data_folder, "/", data_file))
      }
      else if(f_ext == "xz")
      {
        df <- readr::read_csv(xzfile(paste0(data_folder, "/", data_file)))  
      }
      
      state <- substr(df$geoid, 1, 2)
      df$state <- ifelse(state == "11", "DC",
                         ifelse(state == "24", "MD", "VA"))
      
      counts <- df %>% 
        group_by(measure, year, state, region_type) %>% 
        summarise(count = n()) 
      
      # loop through measures in each data file --------------------
      
      measures <- unique(df$measure)
      
      for(m in measures)
      {
        #m = measures[1]
        
        # inventory what has been collected for the measure
        
        # temp <- counts %>% 
        #   filter(measure == m) %>%
        #   mutate(
        #     yr_st_geo = paste(as.character(year), state, region_type, sep = "_"),
        #     collected = (count > 0)) %>%
        #   ungroup() %>%
        #   select(measure, yr_st_geo, collected) %>%
        #   pivot_wider(names_from = yr_st_geo, values_from = collected)
        
        temp <- counts %>% 
          filter(measure == m) %>%
          mutate(
            st_geo = paste(state, region_type, sep = "_"),
            collected = (count > 0)) %>%  # only checks if we have 1 measurement at geography level
          ungroup() %>%
          select(measure, year, st_geo, collected) %>%
          pivot_wider(names_from = st_geo, values_from = collected, values_fill = 0)

        
        # Delete any observations with a missing year --> needs to be fixed in dataset
        # this is only here so tracker can run
        
        if(sum(is.na(temp$year)) > 0)
        {
          temp <- temp[!is.na(temp$year), ] 
          cat(crayon::red(m, ": removed rows with missing year\n"))
        }
                
        # check if we have data for each geography and year listed
        
        nr <- nrow(temp)
        nc <- ncol(temp)
        csums <- colSums(temp[ ,3:nc])
        
        if(!all(csums == nr))
        {
          # some geographies are missing for some years
          
          miss_idx <- as.numeric(which(csums != nr))  # need as.num bc csums is a named list
          
          cat(crayon::red(m, "missing:\n"))
          cat(crayon::red(colnames(temp)[miss_idx]), sep = ", ")
        }
        
        
        # check if years are contiguous

        yrs <- unique(temp$year)
        yr_range = range(yrs)
        yr_check = yr_range[1]:yr_range[2]  
        
        if (!all(yrs == yr_check))
        {
          cat(crayon::red(m, ": not contiguous years\n")) 
        }
        
        # update tracker df --------------------
        
        collected_geos <- rep("", length(csums))
        collected_geos[csums == nr] <- "X"
        names(collected_geos) <- names(csums)
        
        temp_row <- c(CATEGORY = data_cat, MEASURE = m, START_YEAR = yr_range[1], END_YEAR = yr_range[2], 
                      collected_geos)
        tracker_df <- bind_rows(tracker_df, temp_row)
        
      }
    }
  }
}


#
# FORMAT TRACKER OUTPUT --------------------------
#

# combine information for measures listed more than once (e.g. in an NCR and VA file) 

tracker_formatted_df <- tracker_df %>%
  group_by(CATEGORY, MEASURE, START_YEAR, END_YEAR) %>%
  fill(everything(), .direction = "downup") %>%
  distinct()
  
# reorder columns - DO THIS AFTER PULLING ALL REPOS












# # custom DT container -- NEED TO ADJUST
# output_format = htmltools::withTags(table(
#   class = 'display',  # 'stripe hover cell-border order-column', 
#   thead(
#     tr( # 1st row
#       th(rowspan = 3, 'measure_code', style = "border-right: solid 2px;"),
#       th(colspan = 9, '2021', style = "border-right: solid 2px;"),
#       
#     ),
#     tr( # 2nd row
#       th(colspan = 3, 'VA', style = "border-right: solid 1px;"),
#       th(colspan = 3, 'MD', style = "border-right: solid 1px;"),
#       th(colspan = 3, 'DC', style = "border-right: solid 2px;"),
#     ),
#     tr( # 3rd row
#       th(style = "border-right: solid 1px;", span('county', style = "writing-mode: vertical-lr; transform: rotate(180deg);")),
#       th(style = "border-right: solid 1px;", span('tract', style = "writing-mode: vertical-lr; transform: rotate(180deg);")),
#       th(style = "border-right: solid 1px;", span('blockgroup', style = "writing-mode: vertical-lr; transform: rotate(180deg);")),
#       th(style = "border-right: solid 1px;", span('county', style = "writing-mode: vertical-lr; transform: rotate(180deg);")),
#       th(style = "border-right: solid 1px;", span('tract', style = "writing-mode: vertical-lr; transform: rotate(180deg);")),
#       th(style = "border-right: solid 1px;", span('blockgroup', style = "writing-mode: vertical-lr; transform: rotate(180deg);")),
#       th(style = "border-right: solid 1px;", span('county', style = "writing-mode: vertical-lr; transform: rotate(180deg);")),
#       th(style = "border-right: solid 1px;", span('tract', style = "writing-mode: vertical-lr; transform: rotate(180deg);")),
#       th(style = "border-right: solid 2px;", span('blockgroup', style = "writing-mode: vertical-lr; transform: rotate(180deg);")),
#       
#     )
#   )
#  )
# )
# print(output_format)
# 
# 
# # use rownames = FALSE here because we did not generate a cell for row names in
# # the header, and the header only contains five columns
# datatable(output_table, container = output_format, rownames = FALSE) %>%
#   formatStyle(c(1,10), `border-right` = "solid 2px") %>%
#   formatStyle(c(4,7), `border-right` = "solid 1px")
