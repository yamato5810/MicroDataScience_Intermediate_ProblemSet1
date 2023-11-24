# installing some packages

# install.packages("tidyverse")
# install.packages("here")
# install.packages("openxlsx")
# install.packages("stringr")



# Data Cleaning

# (a) semester_dummy_tidy
read_semester_data <- function(data_name, skip_option){
  name <- paste0(data_name, ".csv")
  path <- here::here("raw_data", "semester_dummy", name)
  if(file.exists(path))
    data <- readr::read_csv(path, skip = skip_option)
  else
    data <- readr::read_csv(name, skip = skip_option)
  return(data)
}
semester_data_1 <- read_semester_data("semester_data_1", skip_option = 1)
semester_data_2 <- read_semester_data("semester_data_2", skip_option = 0)

names(semester_data_2) <- names(semester_data_1)

semester_dummy_tidy <- dplyr::bind_rows(semester_data_1, semester_data_2) |>
  dplyr::select(-Y)


# (b) gradrate_tidy
read_gradrate_tidy <- function(data_name){
  name <- paste0(data_name, " " ,".xlsx")
  path <- here::here("raw_data", "outcome", name)
  if(file.exists(path))
    data <- openxlsx::read.xlsx(path) |>
    dplyr::mutate_all(as.double)
  else
    data <- openxlsx::read.xlsx(name)|>
    dplyr::mutate_all(as.double)
  return(data)
}

years <- c(1991:1993, 1995:2016)
gradrate_tidy <- purrr::map(years, read_gradrate_tidy)|>
  dplyr::bind_rows() |>
  dplyr::mutate(women_gradrate_4yr = 0.01*women_gradrate_4yr)


# (c) covariates_tidy
read_covariates_tidy <- function(data_name) {
  name <- paste0(data_name, ".xlsx")
  path <- here::here("raw_data", "covariates", name)
  if(file.exists(path))
    data <- openxlsx::read.xlsx(path) 
  else
    data <- openxlsx::read.xlsx(name) 
  return(data)
}

covariates_tidy <- read_covariates_tidy("covariates") |>
  dplyr::rename(unitid = university_id) |>
  dplyr::mutate(unitid = stringr::str_replace(unitid, "aaaa", "")) |>
  tidyr::pivot_wider(names_from = "category",
                     values_from = "value")


# (d) gradrate_ready
gradrate_ready <- dplyr::mutate(gradrate_tidy, total_gradrate_4yr = tot4yrgrads/totcohortsize) |>
  dplyr::mutate(man_gradrate_4yr = m_4yrgrads/m_cohortsize) |>
  round(digits = 3) |>
  na.omit()

# (e) covariates_ready
year_gradrate_ready <- tidyr::expand(gradrate_ready, year) |>
  as.data.frame()
year_semester_dummy_tidy <- tidyr::expand(semester_dummy_tidy, year) |>
  as.data.frame()
unitid_gradrate_ready <- tidyr::expand(gradrate_ready, unitid) |>
  as.data.frame()

covariates_ready <- dplyr::mutate(covariates_tidy, dplyr::across(everything(), as.double, na.rm = TRUE)) |>
  dplyr::filter(year %in% year_gradrate_ready$year & year %in% year_semester_dummy_tidy$year) |>
  dplyr::filter(unitid %in% unitid_gradrate_ready$unitid)


# (f) master
master <- dplyr::inner_join(semester_dummy_tidy, gradrate_ready, by = c("unitid", "year")) |>
  dplyr::inner_join(covariates_ready, by = c("unitid", "year")) |>
  dplyr::mutate(rate_for_white_student = white_cohortsize/totcohortsize)
   
