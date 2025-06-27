
benthos_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRaQpjiWfO2pMm_gv0BkQ_AqAQrVGgM6F35-iL4JwVu_yCdMy7XlW0ThNpmsJ7sCkBwmtjuU84PjPw_/pub?gid=1619420848&single=true&output=csv")
benthos_data <- benthos_data %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) |>
  group_by(Date, Species_ID) |>
  filter(!(Distance == "0")) |>
  filter(!(Transect_ID %in% c("4.1", "4.2", "4.3", "4.4", "4.5", "4.6")))

species_counts <- benthos_data %>%
  group_by(Date, Species_ID) %>%
  summarise(count = n(), .groups = "drop")
