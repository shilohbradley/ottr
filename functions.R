##################################################
# A Shiny app for creating custom IPEDS peer     #
# reports.                                       #
##################################################

## Functions -----
preview_dt <- function(df) {
  df <- df %>%
    select()
  return(df)
}

R_vector_to_SQL_vector <- function(v)
{
  my_str <- paste0("(", v[1])
  for (i in v[2:length(v)]) {
    my_str <- paste(my_str, i, sep = ",")
  }
  my_str <- paste0(my_str, ")")
  return(my_str)
}