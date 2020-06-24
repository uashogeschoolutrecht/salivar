## code to prepare `DATASET` dataset goes here

library(tidyverse)

## read data for different analytes in the paper
data_raw <- readxl::read_xlsx(here::here(
  "data-raw",
  "Grinta study all parameters.xlsx"),
  na = "No sample")


x <- names(data_raw)
rex <- " \\(.*"
str_view_all(string = x, pattern = rex)

names(data_raw) <- str_replace_all(string = names(data_raw),
                pattern = rex,
                replacement = "") %>%
  print()

names(data_raw)

## clean and tidy data_raw
data_tidy <- data_raw %>%
  gather(`IL-1ß`:Lactoferrin,
         key = "analyte",
         value = "concentration")

names(data_tidy) <- names(data_tidy) %>%
  tolower()

data_tidy <- data_tidy %>%
  dplyr::filter(subject != "8") %>%
  dplyr::mutate(concentration = as.numeric(concentration),
                time = as.character(time)) %>%
  dplyr::mutate(subject = str_replace_all(string = subject,
                                          pattern = "^S",
                                          replacement = "")) %>%
  dplyr::mutate(protocol = as_factor(protocol),
                time = as_factor(time)) %>%
  droplevels()

map(data_tidy, levels)

## check factor levels
#gramlyr::check_factor_levels(df = data_tidy,
#                             factor_variables = c("subject",
#                                                  "protocol",
#                                                  "time",
#                                                  "analyte"))

## getting serum markers, measured by Nutricia
data_serum_nutricia <- gramlyr::diagrams_clean %>%
  dplyr::filter(lab == "nutricia",
                matrix == "serum",
                study == "grinta") %>%
  dplyr::select(subject,
                protocol,
                time,
                analyte,
                concentration) %>%
  dplyr::mutate(subject = as.character(subject),
                protocol = as_factor(protocol),
                time = as_factor(time)) %>%
  droplevels()

names(data_tidy) == names(data_serum_nutricia)

map(data_serum_nutricia, levels)

levels(data_tidy$protocol) == levels(data_serum_nutricia$protocol)
levels(data_tidy$time) == levels(data_serum_nutricia$time)


data_all_tidy <- dplyr::bind_rows(data_tidy,
                                  data_serum_nutricia) %>%
  dplyr::mutate(subject = as_factor(subject))

## final check on factors
map(data_all_tidy, levels)



usethis::use_data(data_all_tidy, overwrite = TRUE)

## analyte annotations
analyte_annotations <- readxl::read_excel(
  path = here::here("data-raw",
                    "Copy of analytes_complete_ref_unit_SKa.xlsx"),
                    sheet = 1)

usethis::use_data(analyte_annotations, overwrite = TRUE)

## read data for order in which subjects cycled the protocols
#library(docxtractr)

## extract table from original MS Word file
# docx <- docxtractr::read_docx(path = here::here(
#   "data-raw",
#   "Randomisation scheme_15 volunteers_GRINTA.docx"))
# docx_tables <- docxtractr::docx_extract_all_tbls(
#   docx = docx)
# data_order <- docx_tables[[2]]


## using new datafile in Excel
data_order <- readxl::read_xlsx(
  here::here(
    "data-raw",
    "randomisatie protocol_subject_GRINTA.xlsx")
  )

names(data_order) <- c(
  "subject",
  "order_1",
  "order_2",
  "order_3",
  "order_4",
  "order_5"
  )
## tidy data
data_order_tidy <- data_order %>%
  pivot_longer(
    cols = order_1:order_5,
    names_to = "order",
    values_to = "protocol") %>%
## replace - for NA
  dplyr::mutate(
    protocol = ifelse(protocol == "-", NA, protocol)
    ) %>%
  ## remove subject 8
  dplyr::filter(subject != "8") %>%
  mutate(subject = as_factor(subject),
         order = as_factor(order),
         protocol = as_factor(protocol)) %>%
  print()

usethis::use_data(data_order_tidy, overwrite = TRUE)
