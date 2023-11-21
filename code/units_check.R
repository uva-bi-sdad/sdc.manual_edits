# Social Data Commons Measure Info Checks
#   - unit, data_type, and measure_type fields

library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(crayon)
library(rjson)
library(readr)

#
# set path for repo locations ----------------
#

repos_dir <- "../../repos"  # directory that contains repos

# if . in repos_dir
#if(grepl(".", repos_dir))  
#{
#  repos_dir <- sub(".", getwd(), repos_dir)  # input path
#}


df = data.frame(MEASURE = character(), SHORT_NAME = character(), DESCRIPTION = character(), 
                DATA_TYPE = character(), AGGREGATION_METHOD = character(), MEASURE_TYPE = character(), 
                UNIT = character())

#
#  loop through data/distribution folders to get measure infos ---------------------
#

folders <- list.dirs(repos_dir)
data_folders <- folders[grepl("data/distribution", folders, fixed=TRUE)]

for(data_folder in data_folders)
{
  print(data_folder)
  
  # loop through data files in each folder 
  
  for(data_file in list.files(data_folder))
  {
    if(data_file == "measure_info.json")
    {
      m_info <- fromJSON(file = paste0(data_folder, "/", data_file))
      
      measures <- names(m_info)
      
      for(i in 1:length(m_info))
      {
        if(measures[i] == "_references")
        {
          next 
        }
        
        sn <- m_info[[i]]$short_name
        desc <- m_info[[i]]$short_description
        u <- m_info[[i]]$unit  
        dt <- m_info[[i]]$data_type 
        mt <- m_info[[i]]$measure_type 
        am <- m_info[[i]]$aggregation_method
        
        temp_row <- c(MEASURE = measures[i], SHORT_NAME = sn, DESCRIPTION = desc, DATA_TYPE = dt, 
                      AGGREGATION_METHOD = am, MEASURE_TYPE = mt, UNIT = u)        
        df <- bind_rows(df, temp_row)
      }
    }
  }  
}

write_csv(df, "units.csv")


# Recommendation - drop measure_type column? maybe
# If we have two aggregation methods, e.g. average percent - we could list this as "average percent" 
# aggregation method, or usse agg_method = average, and measure_type = percent.



