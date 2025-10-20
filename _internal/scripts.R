# Collection of scripts I use for managing the course

library(tutorial.helpers)
library(tidyverse)

homework <- function(pattern = "."){


   x <- submissions_summary("~/Downloads/responses", 
                           title = pattern, 
                           vars = c("name", 
                                    "email", 
                                    "minutes"), 
                           verbose = TRUE, 
                           keep_file_name = "Space") 
  

  
  ret_df <- x |> 
    distinct(source, email, .keep_all = TRUE) |> 
    arrange(source, email)
  
  emails <- sort(unique(ret_df$email))
  names <- sort(unique(ret_df$name))
    
  cat("\n")
  cat(paste0("There are ", length(emails), " unique emails."))
  cat("\n")
  cat(paste(emails, collapse = ","))
  cat("\n\n")
  cat(paste0('"', paste(names, collapse = " # "), '"'))
  
  
  return(ret_df)  
}


assignments <-
  c("01-code_answers",
    "02-quarto_answers",
    "03-terminal_answers",
    "04-github-1_answers",
    "05-github-2_answers",
    "06-websites-1_answers",
    "07-websites-2_answers")

# Not sure the below scripts still work.

assignments <- 
  c("data-visualization_answers",
    "data-transformation_answers", 
    "data-tidying_answers",
    "data-import_answers",
    "getting-help_answers",
    "layers_answers",
    "exploratory-data-analysis_answers",
    "communication_answers",
    "an-introduction-to-tidycensus_answers",
    "logical-vectors_answers",
    "quarto-websites_answers",
    "numbers_answers",
    "strings_answers",
    "regular-expressions_answers",
    "factors_answers",
    "dates-and-times_answers",
    "data-project_answers",
    "missing-values_answers",
    "joins_answers",
    "spreadsheets_answers",
    "databases_answers",
    "arrow_answers",
    "hierarchical-data_answers",
    "web-scraping_answers",
    "functions_answers",
    "iteration_answers")

assignments <- c(
  "rubin-causal-model_answers",
  "probability",
  "sampling",
  "models",
  "two-parameters",
  "three-parameters-causal",
  "mechanics",
  "four-parameters-categorical",
  "five-parameters",
  "n-parameters")

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



