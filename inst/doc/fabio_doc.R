## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----table1, eval = T, echo = FALSE--------------------------------------
library(magrittr)
suppressMessages(
  readxl::read_excel(system.file("fabio_input/fabio_classifications.xlsx", package =  "fabio"), sheet = "Commodities") %>% 
  # openxlsx::read.xlsx(system.file("fabio_input/fabio_classifications.xlsx", package =  "fabio"), sheet = "Commodities") %>% 
  knitr::kable(format = "latex", longtable = T, booktabs = TRUE, caption = "List of commodities") %>%
  kableExtra::kable_styling(latex_options =c("repeat_header"))
)

## ----table2, eval = T, echo = FALSE--------------------------------------
suppressMessages(
  readxl::read_excel(system.file("fabio_input/fabio_classifications.xlsx", package =  "fabio"), sheet = "Processes") %>% 
  knitr::kable(format = "latex", longtable = T, booktabs = TRUE, caption = "List of processes") %>%
  kableExtra::kable_styling(latex_options =c("repeat_header"))
)

## ----table3, eval = T, echo = FALSE--------------------------------------
suppressMessages(
  readxl::read_excel(system.file("fabio_input/fabio_classifications.xlsx", package =  "fabio"), sheet = "Countries") %>% 
  knitr::kable(format = "latex", longtable = T, booktabs = TRUE, caption = "List of countries") %>%
  kableExtra::kable_styling(latex_options =c("repeat_header"))
)

