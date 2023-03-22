library(wordbankr)
library(tidyverse)
library(glue)

## Read data

languages <- list.files("data/new_items") |> str_sub(end = -5)
pronouns <- read_csv("data/pronouns.csv") |> 
  mutate(uid = glue("{form}_{itemID}"))

get_new_items <- function(language) {
  lang <- language
  items <- read_csv(glue("data/new_items/{language}.csv"))
  forms <- setdiff(colnames(items), 
                   c("category", "definition", "gloss", "uni_lemma"))
  prons <- pronouns |> 
    filter(language == lang) |> 
    select(uid, new_uni_lemma)
  
  new_items <- items |> 
    pivot_longer(cols = all_of(forms),
                 names_to = "form",
                 values_to = "item_id") |> 
    mutate(uid = ifelse(is.na(item_id), NA, glue("{form}_{item_id}"))) |> 
    select(-item_id) |> 
    pivot_wider(names_from = form, values_from = uid)
  new_items <- new_items |> 
    mutate(uid = do.call(coalesce, select(new_items, all_of(forms)))) |> 
    select(-all_of(forms)) |> 
    cbind(items |> select(all_of(forms))) |> 
    left_join(prons, by = "uid") |> 
    mutate(uni_lemma = coalesce(new_uni_lemma, uni_lemma)) |> 
    select(-new_uni_lemma)
  
  list(forms = forms,
       new_items = new_items)
}

## Make data in correct format

make_matrix <- function(df) {
  d_vals <- df |> 
    select(data_id, uid, value) |> 
    pivot_wider(id_cols = data_id, 
                names_from = uid, 
                values_from = value) |> 
    data.frame()
  
  d_vals <- d_vals |> 
    `rownames<-`(value = d_vals$data_id) |> 
    select(-data_id) |> 
    data.matrix()
  
  d_vals
}

make_data <- function(language, forms, new_items) {
  all_demo <- c()
  all_long <- c()
  
  for (form in forms) {
    d_demo <- get_administration_data(language = language, form = form) 
    
    items <- get_item_data(language = language, form = form) |> 
      filter(item_kind == "word") 
    
    d_long <- get_instrument_data(language = language, form = form) |> 
      left_join(items |> select(-complexity_category), by = "item_id") |> 
      filter(item_kind == "word")
    
    d_long <- d_long |> 
      mutate(produces = as.numeric(produces),
             understands = as.numeric(understands)) |> 
      left_join(new_items |> select(uid, all_of(form)), by = c("item_id" = form))
    
    print(paste("Retrieved data for", length(unique(d_long$data_id)), 
                language, form, "participants"))
    
    all_demo <- c(all_demo, list(d_demo))
    all_long <- c(all_long, list(d_long))
  }
  
  all_demo <- bind_rows(all_demo)
  all_long <- bind_rows(all_long)
  all_prod <- all_long |> 
    mutate(value = produces) |> 
    make_matrix()
  
  list(all_demo = all_demo,
       all_long = all_long,
       all_prod = all_prod)
}


for (language in languages) {
  ni <- get_new_items(language)
  # Finnish WG/WGShort not yet imported
  if (language == "Finnish") ni$forms <- c("WS", "WSShort")
  df <- make_data(language, ni$forms, ni$new_items)
  
  output <- list(all_demo = df$all_demo,
                 items = ni$new_items,
                 all_long = df$all_long,
                 all_prod = df$all_prod)
  
  saveRDS(output, glue("data/all_forms/{language}_data.rds"))
}
