# Social Data Commons Measure Info Additions
#   - Adds unit, data_type, and measure_type fields

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

repos_dir <- "./repos"  # directory that contains repos

# if . in repos_dir
if(grepl(".", repos_dir))  
{
  repos_dir <- sub(".", getwd(), repos_dir)  # input path
}


#
# extract paths for all data/distribution folders ---------------------
#

folders <- list.dirs(repos_dir)

data_folders <- folders[grepl("data/distribution", folders, fixed=TRUE)]


# STEP 1: look for how many measure_info files there are in a folder (should only be 1)----------

# loop through data/distribution folders 

for(data_folder in data_folders)
{
  print(data_folder)
  
  # loop through data files in each folder 
  
  for(data_file in list.files(data_folder))
  {
    f_ext <- tools::file_ext(data_file)
  
    if(f_ext == "json")  
    {
      if(data_file == "measure_info.json")
      {
        cat(crayon::green(data_file, "\n"))
      }
      else
      {
        cat(crayon::red(data_file, "\n"))
      }
    }
  }  
}


# STEP 2: collect data_type and measure_type from data files ------------------

m_info_df <- data.frame(measure = character(), measure_type = character(), data_type = character())


for(data_folder in data_folders)
{
  for(data_file in list.files(data_folder))
  {
    if(data_file != "measure_info.json")
    {
      tryCatch(
        {
          print(paste0(data_folder, "/", data_file))
          df <- read_csv(paste0(data_folder, "/", data_file), col_types = cols(.default = "c")) 
          
          m_info_rows <- unique(df[ , c("measure", "measure_type")])
          
          # check: each measure has only one associated measure_type
          if(nrow(m_info_rows) != length(unique(m_info_rows$measure)))
          {
            print(paste0(data_folder, "/", data_file))
            cat(crayon::red("ERROR: measure has more than one associated type in single file"))
            next
          }
          
          m_info_rows$data_type = "numeric"  # change this line if other measure_types encountered
          
          m_info_df <- bind_rows(m_info_df, m_info_rows)
          
          cat(crayon::green("COMPLETE\n"))
        }, 
      
        warning = function(w_cond) 
        {
          print(w_cond)
          cat(crayon::red("WARNING: file skipped\n"))
        }, 
        error = function(e_cond) 
        {
          print(e_cond)
          cat(crayon::red("ERROR: file skipped\n"))
        }#, 
        #finally=
        #{
        #  cleanup-code
        #}
      )

    }
  }
}
  
# deduplicate m_info_df

unq_m_info_df <- dplyr::distinct(m_info_df)

# check: each measure has only one associated measure_type
if(nrow(unq_m_info_df) != length(unique(unq_m_info_df$measure)))
{
  cat(crayon::red("ERROR: measure has more than one associated type or data type in multiple files"))
}


# STEP 3: fill in new fields in measure_info files

for(data_folder in data_folders)
{
  for(data_file in list.files(data_folder))
  {
    if(data_file == "measure_info.json") 
    {
      print(paste0(data_folder, "/", data_file))
      
      m_info <- fromJSON(file = paste0(data_folder, "/", data_file))

      if(length(m_info) > 0)
      {
        measures <- names(m_info)
      
        # loop through each measure listed in measure_info.json - add three fields
      
        # check if measure info already has units, data_type, and measure_type? no, just rewrite
 
        # add these fields, automatically fill data_type and measure type
      
        for(i in 1:length(m_info))
        {
          if(measures[i] == "_references")
          {
            next 
          }
          
          idx <- which(unq_m_info_df$measure == measures[i])
        
          if(length(idx) == 0) # measure info name not in list of measures from datasets
          {
            cat(crayon::red("ERROR:", measures[i], "not in a data file\n"))
            next
          }

          m_info[[i]]$unit <- ""
          m_info[[i]]$data_type <- unq_m_info_df$data_type[idx]  # character, numeric, geography
          m_info[[i]]$measure_type <- unq_m_info_df$measure_type[idx] # percent, count, index, cost, date time, speed

        }

        # write m_info back to measure_info.json
        write(toJSON(m_info, indent = 4), paste(data_folder, data_file, sep = "/"))
        
        cat(crayon::green("COMPLETE\n"))
      }
      else
      {
        cat(crayon::red("EMPTY FILE\n"))
      }
    }
  }
}


