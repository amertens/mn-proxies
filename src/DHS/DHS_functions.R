
# A helper that tries to find the *first* variable in `df` (haven-labeled)
# whose variable NAME or LABEL matches *any* of the given regex patterns.
find_var_by_label <- function(df, patterns) {
  # Build a lookup of var name => label
  var_labels <- map(df, ~ attr(.x, "label"))
  var_info <- tibble(
    old_name = names(var_labels),
    label    = unlist(var_labels)
  )

  for (pat in patterns) {
    # Look for matches in either the label or the variable name
    hits <- var_info %>%
      filter(
        grepl(pat, old_name, ignore.case = TRUE) |
          grepl(pat, label,    ignore.case = TRUE)
      )
    if (nrow(hits) > 0) {
      # Return the first match
      return(hits$old_name[1])
    }
  }
  # If no match found, return NA
  return(NA_character_)
}


# rename_dhs_vars: use a "dictionary" approach of newname -> c(patterns...)
# and rename each if found. If not found, we skip it (or you can create an empty var).
rename_dhs_vars <- function(df, rename_spec) {
  # We'll store the matches in a named vector: c("old_name"="new_name", ...)
  matches <- c()
  for (new_name in names(rename_spec)) {
    patterns <- rename_spec[[new_name]]
    old_name <- find_var_by_label(df, patterns)
    if (!is.na(old_name)) {
      #matches[old_name] <- new_name
      matches[new_name] <- old_name
    }
  }
  # Now rename if we have any matches
  df_out <- df
  if (length(matches) > 0) {
    df_out <- df_out %>%
      rename(!!!matches)
  }
  return(df_out)
}




clean_HR <- function(data) {
  # 1) Rename using hr_rename_spec
  df2 <- rename_dhs_vars(data, hr_rename_spec)

  # 2) The "intended" new names are simply the keys of hr_rename_spec
  intended_new_vars <- names(hr_rename_spec)

  # 3) Keep only the variables that actually exist after renaming
  keep_vars <- intersect(intended_new_vars, names(df2))

  df3 <- df2 %>%
    select(all_of(keep_vars)) %>%
    distinct()

  return(df3)
}

# HR_clean <- clean_HR(df$HRdata )
# head(HR_clean)



clean_PR <- function(df) {
  # 1) Rename using pr_rename_spec
  df2 <- rename_dhs_vars(df, pr_rename_spec)

  # 2) The new names we intended are the keys of pr_rename_spec
  intended_new_vars <- names(pr_rename_spec)

  # 3) Only keep variables that actually exist
  keep_vars <- intersect(intended_new_vars, names(df2))

  df3 <- df2 %>%
    select(all_of(keep_vars))

  return(df3)
}





clean_IR <- function(df) {
  # 1) Rename based on your dictionary
  df2 <- rename_dhs_vars(df, ir_rename_spec)

  # 2) The new names we *intended* to have are simply the names of ir_rename_spec
  intended_new_vars <- names(ir_rename_spec)

  # 3) Keep only those that actually got created/renamed
  keep_vars <- intersect(intended_new_vars, names(df2))

  # 4) Subset
  df3 <- df2 %>% dplyr::select(all_of(keep_vars))
  return(df3)
}

