### this script looks at the WFP NEXTGEN data from:
# https://iridl.ldeo.columbia.edu/fbfmaproom2/ethiopia-ond
# we aim to get the overall RP for the trigger and look at the overall performance.

### libraries
library(tidyverse)
library(scales)
library(raster)
library(sf)
library(readxl)
library(gghdx)
gghdx()

### loading data
wfp_file_path <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public", "raw", "eth", "wfp_nextgen", "WFP Data.xlsx")

ecmwf_trigger_path <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public", "exploration", "eth", "ecmwf", "ecmwf_trigger.xlsx")

# Admin boundaries
eth_adm3_codab <- st_read(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public", "raw", "eth", "cod_ab", "Admin_2024.gdb.zip"), 
  layer = "eth_admbnda_adm3_csa_bofedb_2024")

# Filtering to have only Somali region
som_codab <- eth_adm3_codab |>
  filter(admin1Name_en == "Somali") |>
  group_by(admin2Name_en) |>
  summarise(woreda_num = n()) |>
  st_drop_geometry()

# loading results from the Drought App for the Somali region
# this excludes the Siti Zone
ecmwf_results <- read_excel(ecmwf_trigger_path, sheet = "Latest")

#################### Probability of non exceedance
### months
forecast_months <- c("July", "August", "September")
wfp_trigger_ls <- forecast_months |>
  map(\(mon){
    # reading in data
    wfp_data <- read_excel(wfp_file_path, sheet = paste0("Z-", mon), col_names = FALSE)
    zones <- wfp_data[1, seq(3, 35, 3)] |>
      as.character()
    zones <- zones[!zones %in% c("Siti", "Fafan")]
    # very rough data cleaning
    # selecting years and reported bad years
    wfp_prob_df <- wfp_data[8:41,1:2]
    # setting column names using row 8
    colnames(wfp_prob_df) <- wfp_prob_df[1, ]
    # selecting the columns with the probability of non-exceedence
    wfp_prob_df <- wfp_prob_df[-1, ] |>
      bind_cols(wfp_data[9:41, seq(10, 35, 3)])
    # adding zone names to columns
    colnames(wfp_prob_df)[3:11] <- zones 
    # convert columns from character to numeric
    wfp_prob_df <- wfp_prob_df |>
      mutate(across(all_of(zones), as.numeric))
    # compute percentiles where 80% is the threshold, 
    # since this is probability, 0% would be the year with the most rainfall
    # 100% would indicate the year with the least rainfall
    
    #percentiles <- wfp_prob_df |>
    #  summarise(across(all_of(zones), ~ quantile(., probs = 0.8, na.rm = T))) |>
    #  unlist()
    #names(percentiles) <- gsub("\\.80%", "", names(percentiles))
    # alternatively, use the triggers they have on the maproom
    percentiles <- wfp_data[7, seq(10, 35, 3)] |> as.numeric()
    percentiles <- setNames(percentiles, zones)
    
    # testing if WFP threshold would have activated
    wfp_events <- wfp_prob_df |>
      mutate(across(all_of(zones), ~ as.integer(. >= percentiles[cur_column()]))) |>
      rowwise() |>
      mutate(month = mon,
             zone_num = sum(across(zones), na.rm = T),
             metric = case_when(zone_num > 0 & !is.na(`Reported bad years`) ~ "TP",
                                zone_num > 0 & is.na(`Reported bad years`) ~ "FP",
                                zone_num == 0 & !is.na(`Reported bad years`) ~ "FN",
                                zone_num == 0 & is.na(`Reported bad years`) ~ "TN",
                                .default = NA))
  })

# computing the performance metrics
wfp_trigger_df <- wfp_trigger_ls |>
  map(\(mon_df){
    mon_df |>
      group_by(month) |>
      summarise(
        `Rate of Worthy Action` = sum(metric == "TP") / (sum(metric == "TP") + sum(metric == "FN")), 
        `Rate of Failing to Act` = sum(metric == "FN") / (sum(metric == "FN") + sum(metric == "TP")), 
        `Rate of Acting in Vain` = sum(metric == "FP") / (sum(metric == "FP") + sum(metric == "TN")))
  }) |>
  bind_rows() |>
  pivot_longer(!month, names_to = "Metric", values_to = "Value")

# plotting Performance Metrics for WFP trigger at each lead time
wfp_trigger_df |>  
  ggplot() +
  geom_line(aes(x = factor(month, levels = month.name), 
                y = Value * 100, color = Metric, group = Metric), linewidth = 1) + 
  labs(title = "Performance Metrics over Lead Time",
       subtitle = "Forecast Release month targetting OND Season", 
       x = "Month", y = "Rate (%)") + 
  ylim(0,100)

tes <- wfp_trigger_ls |>
  map(\(mon_df){
    mon_df |>
      dplyr::select(Year, zone_num, metric)
  }) |>
  bind_cols() |>
  rename(Year = `Year...1`, July = `zone_num...2`, August = `zone_num...5`, 
         September = `zone_num...8`, July_metric = `metric...3`, 
         August_metric = `metric...6`, September_metric = `metric...9`) |>
  dplyr::select(-c(`Year...4`, `Year...7`))
  

tes |>
  mutate(July = if_else(July > 0, 1 , 0),
         August = if_else(August > 0, 1 , 0),
         September = if_else(September > 0, 1 , 0)) |>
  pivot_longer(cols = c(July, August, September)) |>
  ggplot() +
  geom_bar(aes(x = as.integer(Year), y = value, 
               fill = factor(name, levels = month.name)),
           stat = "identity", position = "stack") +
  labs(title = "Years different months would activate (WFP)",
       subtitle = "High Severity at Zonal Level and Using the Risk Matrix",
       x = "Year", y = "Month Activated", fill = "Month") +
  scale_x_continuous(breaks = seq(min(tes$Year), max(tes$Year), by = 5))

### selecting each year and comparing to trigger
monthly_df <- wfp_trigger_ls |>
  map(\(df) df |> dplyr::select(Year, metric)) |>
  bind_cols() |>
  rename_with(~ forecast_months, starts_with("metric")) |>
  rename(Year = Year...1) |>
  dplyr::select(Year, all_of(forecast_months))

# Quick plot to see overall performance
monthly_df |>
  pivot_longer(cols = -Year, names_to = "Month", values_to = "Category") |>
  group_by(Year, Category) |>
  summarise(Count = n()) |>
  ungroup() |>
  ggplot(aes(x = as.numeric(Year), y = Count, color = Category, group = Category)) +
  geom_line(linewidth = 1) +
  labs(title = "Counts of TN, TP, FP, FN by Year",
       x = "Year", y = "Number of Months",
       fill = "Category")

# Computing the overall action in a season based on WFP risk matrix
# Comparing with the years from ECMWF
combined_df <- monthly_df |>
  mutate(across(!Year, ~ if_else(. %in% c("TP", "FP"), "Yes", "No"))) |>
  mutate(overall = case_when(July == "Yes" & August == "Yes" & September == "Yes" ~ "Yes",
                             July == "Yes" & August == "Yes" & September == "No" ~ "Yes",
                             July == "Yes" & August == "No" & September == "Yes" ~ "Yes",
                             July == "Yes" & August == "No" & September == "No" ~ "No",
                             July == "No" & August == "Yes" & September == "Yes" ~ "Yes",
                             July == "No" & August == "Yes" & September == "No" ~ "No",
                             July == "No" & August == "No" & September == "Yes" ~ "Yes",
                             July == "No" & August == "No" & September == "No" ~ "No",
                             .default = NA)) |>
  # bind_cols(wfp_prob_df) |>
  merge(ecmwf_results, all.y = T) |>
  mutate(wfp_metric = case_when(
    overall == "Yes" & !is.na(`Drought Years`) ~ "TP",
    overall == "Yes" & is.na(`Drought Years`) ~ "FP",
    overall == "No" & !is.na(`Drought Years`) ~ "FN",
    overall == "No" & is.na(`Drought Years`) ~ "TN",
    .default = NA),
    wfp_activated = if_else(overall %in% c("Yes"), 1, 0),
    ecmwf_activated = if_else(Metrics %in% c("TP", "FP"), 1, 0) * -1)

# what is the overall RP?
total_years <- 2022-1991+1
overall_act <- (combined_df |>
                   summarise(sum(wfp_activated, na.rm = T))) 
overall_rp <- total_years / overall_act
overall_rate <- overall_act / total_years

paste0("The Overall Return Period is 1-in-", round(overall_rp, 1), " years.")
paste0("The Overall Rate of Activation is ", round(overall_rate*100, 1), "%.")
paste0("The Overall Hit Rate is ", 
       round((sum(combined_df$wfp_metric == "TP", na.rm = T)/
                (sum(combined_df$wfp_metric == "TP", 
                     combined_df$wfp_metric == "FN", na.rm = T)))*100, 1), 
       "%.")
paste0("The Overall Miss Rate is ", 
       round((sum(combined_df$wfp_metric == "FN", na.rm = T)/
                (sum(combined_df$wfp_metric == "TP", 
                     combined_df$wfp_metric == "FN", na.rm = T)))*100, 1), 
       "%.")
paste0("The Overall False Alarm Rate is ", 
       round((sum(combined_df$wfp_metric == "FP", na.rm = T)/
                (sum(combined_df$wfp_metric == "FP", 
                     combined_df$wfp_metric == "TN", na.rm = T)))*100, 1), 
       "%.")


# plotting years where either ECMWF or WFP would have activated based on 1-in-3
combined_df |>
  #filter(Year >= 1991) |>
  pivot_longer(cols = c(wfp_activated, ecmwf_activated), 
               names_to = "Source", values_to = "Activated") |>
  mutate(Date = as.Date(Year, format = "%Y"),
         bad_year = if_else(!is.na(`Drought Years`), "Reported Bad Year", "")) |>
  ggplot() +
  geom_bar(aes(x = year(Date), y = Activated, fill = Source), 
           stat = "identity", position = "identity", width = 0.9) + 
  #geom_text(aes(x = year(Date), y = 0, label = bad_year), vjust = 0.4, size = 3.5, fontface = "bold", color = "black") +
  #coord_flip() +
  scale_fill_manual(values = c("ecmwf_activated" = "#B22222", "wfp_activated" = "steelblue"), 
                    labels = c("ECMWF", "WFP")) +
  labs(title = str_wrap("Years the respective frameworks/plans would have triggered Anticipatory Action", width = 50),
       x = "Year", y = "",
       fill = "Source") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14))
  
## adding an evaluation where we do not wait to act 
## we act when forecasts are released
normal_act <- combined_df |> 
  mutate(activation = case_when(July == "Yes" | August == "Yes" | September == "Yes" ~ "Yes",
                                .default = "No"),
         norm_metric = case_when(activation == "Yes" & !is.na(`Drought Years`) ~ "TP",
                                 activation == "Yes" & is.na(`Drought Years`) ~ "FP",
                                 activation == "No" & !is.na(`Drought Years`) ~ "FN",
                                 activation == "No" & is.na(`Drought Years`) ~ "TN",
                                 .default = NA)) 

normal_rp <- total_years / (normal_act |> summarise(sum(activation == "Yes", na.rm = T)))
normal_rate <- (normal_act |> summarise(sum(activation == "Yes", na.rm = T))) / total_years

paste0("The Return Period is 1-in-", round(normal_rp, 1), " years.")
paste0("The Rate of Activation is ", round(normal_rate*100, 1), "%.")
paste0("The Hit Rate is ", 
       round((sum(normal_act$norm_metric == "TP", na.rm = T)/
                (sum(normal_act$norm_metric == "TP", 
                     normal_act$norm_metric == "FN", na.rm = T)))*100, 1), 
       "%.")
paste0("The Miss Rate is ", 
       round((sum(normal_act$norm_metric == "FN", na.rm = T)/
                (sum(normal_act$norm_metric == "TP", 
                     normal_act$norm_metric == "FN", na.rm = T)))*100, 1), 
       "%.")
paste0("The False Alarm Rate is ", 
       round((sum(normal_act$norm_metric == "FP", na.rm = T)/
                (sum(normal_act$norm_metric == "FP", 
                     normal_act$norm_metric == "TN", na.rm = T)))*100, 1), 
       "%.")

