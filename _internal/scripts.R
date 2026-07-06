# Collection of scripts I use for managing the course

library(tutorial.helpers)
library(tidyverse)
library(janitor)

homework <- function(pattern = "."){


   x <- submissions_summary("~/Downloads/submissions", 
                           title = pattern, 
                           vars = c("name", 
                                    "email", 
                                    "minutes"), 
                           verbose = TRUE, 
                           keep_file_name = "Space") 
  

  
  ret_df <- x |> 
    distinct(source, email, .keep_all = TRUE) |> 
    arrange(source, email) |>
    mutate(minutes = as.numeric(minutes))
  
  emails <- sort(unique(ret_df$email))
  names <- sort(unique(ret_df$name))
    
  cat("\n")
  cat(paste0("There are ", length(emails), " unique emails."))
  cat("\n")
  cat(paste(emails, collapse = ","))
  cat("\n\n")
  cat(paste0('"', paste(names, collapse = " # "), '"'))
  cat("\n\n")
  
  return(ret_df)  
}


assignments <-
  c("getting-started_answers",
    "introduction-to-r_answers",
    "01-code_answers",
    "02-quarto_answers",
    "03-terminal_answers",
    "04-ai-1_answers",
    "05-github-1_answers",
    "06-github-2_answers",
    "07-ai-2_answers",
    "r4ds-1_answers",
    "08-websites-1_answers",
    "01-probability_answers",
    "02-sampling_answers",
    "r4ds-2_answers",
    "03-rubin-causal-model_answers")

# Not sure the below scripts still work.



z |> 
  janitor::clean_names() |> 
  filter(source %in% assignments) |> 
  mutate(source = factor(source, levels = assignments)) |> 
  group_by(source) |> 
  summarize(minutes = median(as.numeric(minutes)),
            N = n()) |> 
  print(n = 30)


z |> 
  select(information_name, information_email) |> 
  distinct(information_name, .keep_all = TRUE) |> 
  distinct(information_email, .keep_all = TRUE) |> 
  arrange(information_name) |> 
  write_csv("rough-list-demo-day.cvs")



