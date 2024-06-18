library(tidyverse)
library(arrow)
library(gghdx)
gghdx()
adm1_actual <- read_parquet("data/eth_adm1.parquet") %>%
  mutate(median = median*1000,
         date = as.Date(time),
         month = month(date),
         year = year(date),
         monthly_total = case_when(
           month %in% c(1, 3, 5, 7, 8, 10, 12) ~ median * 31,
           month %in% c(4, 6, 9, 11) ~ median * 30,
           month %in% c(2) ~ median * 29,
           (month %in% c(2) & year %% 4) ~ median * 29,
           .default = median))

adm1_actual %>%
  filter(admin1Name_en == "Amhara" & month %in% c(6:9)) %>%
  group_by(year) %>%
  summarise(ssn = sum(monthly_total)) %>%
  ggplot() +
  geom_bar(aes(x=year, y=ssn), stat = "identity") + 
  labs(title = "Seasonal Rainfall for Amhara",
       x="Year",
       y="Rainfall(mm)") +
  geom_hline(yintercept = 1138, linetype = "dashed", color = "red", aes(label = "Forecast"))
