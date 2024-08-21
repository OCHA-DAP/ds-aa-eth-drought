# in this script, we want to analyse the performance of ECMWF data across the lead time 
# this may help with the argument of the miss rate 

# libraries
library(aws.s3)
library(tidyverse)
library(readxl)
library(arrow)
library(sf)
library(raster)
library(exactextractr)
library(gghdx)
gghdx()

# Function to read Ethiopia CODAB administrative boundaries
# Parameters:
#   level: Character string indicating the administrative level ("adm3" or "adm1")
# Returns:
#   Spatial dataset of the specified administrative level
read_ethiopia_codab <- function(level) {
  # Construct file path to the Ethiopia CODAB file
  file_path <- file.path(Sys.getenv("AA_DATA_DIR"), "public", "raw", "eth", "cod_ab", "Admin_2024.gdb.zip")
  
  # Determine layer name based on the specified level
  layer_name <- switch(level,
                       "adm3" = "eth_admbnda_adm3_csa_bofedb_2024",
                       "adm1" = "eth_admbnda_adm1_csa_bofedb_2024")
  
  # Read spatial data from the specified layer in the CODAB file
  eth_data <- st_read(file_path, layer = layer_name)
  
  # Return the spatial dataset
  return(eth_data)
}


eth_adm3_codab <- read_ethiopia_codab("adm3")  # Read administrative level 3 boundaries
eth_adm1_codab <- read_ethiopia_codab("adm1")  # Read administrative level 1 boundaries

# copied this from the drought app
trigger_proposal <- read_excel(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public", "exploration", "eth", "ecmwf", "trigger_proposal.xlsx"), 
  sheet = "Latest")

# these are not decided on and change a lot but this is the list I came up with 
# to be defined later
ond_zones <- c("ET0412", "ET0422", "ET0410", "ET0414", "ET0508", "ET0511", 
               "ET0507", "ET0504", "ET0503", "ET0505", "ET0509", "ET0510", 
               "ET0506", "ET0808", "ET0810", "ET0809", "ET0812")

# extracting woreda names
ond_woreda_names <- eth_adm3_codab |>
  filter(admin2Pcode %in% ond_zones) |>
  pull(admin3Name_en)

# extracting woreda pcodes
ond_woreda_pcodes <- eth_adm3_codab |>
  filter(admin2Pcode %in% ond_zones) |>
  pull(admin3Pcode)

# using the reported bad years since getting drought years for each lead time
# and zone would result in very varying lists
drought_years <- c(1983, 1984, 1985, 1992, 1999, 2000, 2005, 2006, 2008, 2009,
                   2014, 2016, 2017, 2021, 2022)

# read in MARS data at woreda level 
adm3_forecast <- read_parquet(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public", "processed", "eth", "ecmwf_mars", "df_eth_mars_zonal_adm3.parquet")
)
# read in July Forecast
july_forecast <- terra::rast(
  paste0("s3://", Sys.getenv("BUCKET_NAME"), 
         "/ECMWF_COGS/202407-ECMWF_SEAS-V_i_a-ensemble_mean.tif")
)
# aggregate the july forecast for Ethiopia at admin 3
july_forecast_crop <- crop(july_forecast, raster::extent(eth_adm3_codab))
july_forecast_adm3 <- exact_extract(july_forecast_crop, eth_adm3_codab, 
                                    "median", append_cols = c("admin3Pcode", "admin2Pcode"))


# reading in ERA5 data
# trying to find "bad" years
adm3_era5 <- read_parquet(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public", "processed", "eth", "era5", "eth_adm3.parquet")
)
# extracting years where the lowest value in history
# list of bad years from ERA5
era5_drought_years <- adm3_era5 |>
  mutate(d_month = month(time), d_year = year(time)) |>
  filter(d_month %in% c(10:12) & admin3Pcode %in% ond_woreda_pcodes) |>
  group_by(admin3Name_en, admin3Pcode, d_year) |>
  summarise(season_total = sum(median, na.rm = T)) |>
  group_by(admin3Name_en, admin3Pcode) |>
  mutate(lower20 = quantile(season_total, 0.2, na.rm = T),
         drought_year = season_total <= lower20) |>
  group_by(d_year) |>
  summarise(num_triggered = sum(drought_year, na.rm = T),
            pct_triggered = mean(drought_year, na.rm = T),
            drought_year = pct_triggered >= 0.3) |>
  filter(drought_year) |>
  pull(d_year)

# testing which years would be below the trigger and
# % of woredas below the threshold

# checking the RP by lead time
colnames(trigger_proposal)[5:8] <- c("October", "September", "August", "July")
total_years <- 2023-1981+1

### getting the forecast and getting the seasonal total by year
ond_forecast <- adm3_forecast |>
  filter(adm2_pcode %in% ond_zones & 
           valid_month %in% c(10:12) & 
           pub_month %in% c(7:10)) |>
  mutate(valid_year = year(valid_date),
         pub_month_name = month.name[pub_month]) |>
  group_by(adm3_pcode, adm3_en, adm2_pcode, adm2_en, valid_year, 
           pub_month, pub_month_name) |>
  summarise(seasonal_total = sum(value, na.rm = T))

############## 
### Using the Drought App Results
### getting the return period for each woreda and understand how to change this
woreda_rp <- trigger_proposal |> 
  filter(Woreda %in% ond_woreda_names) |>
  rowwise() |>
  mutate(July_RP = round(1 / (ecdf(ond_forecast |> 
                                     filter(adm3_en == Woreda & 
                                        pub_month_name == "July") |> 
                               pull(seasonal_total))(July)), 2),
         August_RP = round(1 / (ecdf(ond_forecast |> 
                                     filter(adm3_en == Woreda & 
                                              pub_month_name == "August") |> 
                                     pull(seasonal_total))(August)), 2),
         September_RP = round(1 / (ecdf(ond_forecast |> 
                                     filter(adm3_en == Woreda & 
                                              pub_month_name == "September") |> 
                                     pull(seasonal_total))(September)), 2),
         October_RP = round(1 / (ecdf(ond_forecast |> 
                                     filter(adm3_en == Woreda & 
                                              pub_month_name == "October") |> 
                                     pull(seasonal_total))(October)), 2))

# The overall RP looks to be between 1-in-8 and 1-in-22 depending on admin 3.

# looking at which years would have activated at each lead time
calculate_lt_activation <- function(woreda_rp_df, ond_forecast_df, percent_threshold = 0.2) {
  lt_activation_df <- woreda_rp_df |>
    pivot_longer(cols = month.name[7:10], names_to = "pub_month_name", values_to = "trigger") |>
    merge(ond_forecast_df, by.x = c("Woreda", "pub_month_name"), by.y = c("adm3_en", "pub_month_name")) |>
    mutate(threshold_reached = if_else(seasonal_total > trigger, FALSE, TRUE)) |>
    group_by(valid_year, pub_month_name) |>
    summarise(
      woredas_reached = sum(threshold_reached, na.rm = TRUE),
      percent_reached = mean(threshold_reached, na.rm = TRUE),
      trigger_reached = if_else(percent_reached > percent_threshold, TRUE, FALSE)
    ) |>
    ungroup()
  
  return(lt_activation_df)
}
lt_activation_df <- calculate_lt_activation(woreda_rp, 
                                            ond_forecast, 
                                            percent_threshold = 0.2)

### removing July
lt_activation_df |>
  #filter(pub_month_name != "July") |>
  group_by(valid_year, pub_month_name, trigger_reached) |>
  summarise(count = sum(trigger_reached, na.rm = T)) |>
  mutate(pub_month_name = factor(pub_month_name, levels = month.name)) |>
  ggplot() +
  geom_bar(aes(x = valid_year, y = count, fill = pub_month_name),
           stat = "identity", position = "stack") +
  labs(title = "Years different months would activate (ECMWF)",
       subtitle = "Using 20% of woredas as a threshold",
       x = "Year", y = "Month Activated", fill = "Month")

# getting metrics across time
calculate_lt_metrics <- function(lt_activation_df, drought_years) {
  lt_metrics_df <- lt_activation_df |>
    #filter(pub_month_name != "July") |>
    mutate(bad_year = valid_year %in% drought_years,
           metric = case_when(
             bad_year & trigger_reached ~ "TP",
             !bad_year & trigger_reached ~ "FP",
             bad_year & !trigger_reached ~ "FN",
             !bad_year & !trigger_reached ~ "TN",
             TRUE ~ NA_character_
           )) |>
    group_by(pub_month_name) |>
    summarise(
      TP_count = sum(metric == "TP", na.rm = TRUE),
      FP_count = sum(metric == "FP", na.rm = TRUE),
      FN_count = sum(metric == "FN", na.rm = TRUE),
      TN_count = sum(metric == "TN", na.rm = TRUE),
      hit_rate = TP_count / (TP_count + FN_count),
      miss_rate = FN_count / (TP_count + FN_count),
      false_alarm_rate = FP_count / (TN_count + FP_count)
    ) |>
    ungroup()
  
  return(lt_metrics_df)
}

calculate_lt_metrics(lt_activation_df, drought_years)
calculate_lt_metrics(lt_activation_df, era5_drought_years)

########## 
## computing the overall

ras <- lt_activation_df |>
  #filter(pub_month_name != "July") |>
  group_by(valid_year) |>
  summarise(trigger_reached = any(trigger_reached)) |>
  mutate(bad_year = valid_year %in% drought_years,
         metric = case_when(
           bad_year & trigger_reached ~ "TP",
           !bad_year & trigger_reached ~ "FP",
           bad_year & !trigger_reached ~ "FN",
           !bad_year & !trigger_reached ~ "TN",
           TRUE ~ NA_character_
         )) |>
  summarise(
    act_rate = sum(trigger_reached, na.rm = T),
    TP_count = sum(metric == "TP", na.rm = TRUE),
    FP_count = sum(metric == "FP", na.rm = TRUE),
    FN_count = sum(metric == "FN", na.rm = TRUE),
    TN_count = sum(metric == "TN", na.rm = TRUE),
    hit_rate = TP_count / (TP_count + FN_count),
    miss_rate = FN_count / (TP_count + FN_count),
    false_alarm_rate = FP_count / (TN_count + FP_count)
  ) |>
  ungroup()

paste0("The Overall Return Period is 1-in-", round(total_years / ras$act_rate, 1), " years.")
paste0("The Overall Rate of Activation is ", round(ras$act_rate, 1), "%.")
paste0("The Overall Hit Rate is ", round(ras$hit_rate *100, 1), "%.")
paste0("The Overall Miss Rate is ", round(ras$miss_rate *100, 1), "%.")
paste0("The Overall False Alarm Rate is ", round(ras$false_alarm_rate*100, 1), "%.")


############### 
####################### Testing setting the trigger using only the Jul-Sep forecasts
ecmwf_triggers <- ond_forecast |>
  group_by(adm3_pcode, adm3_en, pub_month_name) |>
  summarise(triggers = quantile(seasonal_total, 0.15, na.rm = T))

# testing with 2024 forecast
july_trigger <- july_forecast_adm3 |>
  rename_with(~ paste0(month.name[c(7:12, 1)]),
              starts_with("median")) |>
  pivot_longer(cols = month.name[c(7:12, 1)], 
               names_to = "month",
               values_to = "monthly_forecast") |> 
  filter(month %in% month.name[c(10:12)]) |>
  group_by(admin3Pcode, admin2Pcode) |>
  summarise(seasonal_forecast = sum(monthly_forecast, na.rm = T)) |>
  merge((ecmwf_triggers |> filter(pub_month_name == "July")), 
        by.x = "admin3Pcode", by.y = "adm3_pcode", all.y = T) |>
  mutate(threshold_reached = if_else(seasonal_forecast > triggers, "Above", "Below"))


july_trigger |>
  merge(eth_adm3_codab, by = "admin3Pcode", all.y = T) |>
  mutate(threshold_reached = if_else(admin3Pcode %in% ond_woreda_pcodes, threshold_reached, NA),
         threshold_reached = if_else(is.na(threshold_reached), "Out of Season", threshold_reached)) |>
  ggplot() +
  geom_sf(aes(fill = factor(threshold_reached, 
                            levels = c("Below", "Above", "Out of Season")), 
              geometry = Shape)) + 
  geom_sf(data = eth_adm1_codab, fill = NA, linewidth = 0.8, color = "black") +
  scale_fill_manual(values = c("Above" = "steelblue", 
                               "Below" = "tomato", 
                               "Out of Season" = "lightgrey"), 
                    na.value = "lightgrey") +
  labs(title = "Oct-Dec 2024 Season based on the July ECMWF Forecast",
       subtitle = "Assessing Forecasts for July 2024: Rainfall Below a 1-in-5 Year Return Period",
       caption = "Analysis conducted on Southern Pastoral areas",
       fill = "Status")

### getting percent of woredas where trigger is reached
july_trigger |>
  summarise(mean(threshold_reached == "Below", na.rm=T))

################## 
### Without October
###testing out which years would have activated using the WFP matrix
### testing using WFP method and risk matrix

calculate_ecmwf_triggers_metrics_matrix <- function(ond_forecast, drought_years, total_years, quantile_values) {
  
  # Define a function to calculate metrics for each quantile value
  calculate_metrics <- function(quantile_value) {
    ecmwf_triggers <- ond_forecast |>
      group_by(adm3_pcode, adm3_en, pub_month_name) |>
      summarise(triggers = quantile(seasonal_total, quantile_value, na.rm = TRUE), .groups = 'drop')
    
    ecmwf_triggers_wfp_method <- ecmwf_triggers |>
      pivot_wider(names_from = pub_month_name, values_from = triggers) |>
      merge(ond_forecast |>
              ungroup() |>
              dplyr::select(-pub_month) |>
              pivot_wider(names_from = pub_month_name, values_from = seasonal_total), 
            by = c("adm3_pcode", "adm3_en"), suffixes = c("_trig","_fore")) |>
      mutate(
        July_threshold = July_fore <= July_trig,
        August_threshold = August_fore <= August_trig,
        September_threshold = September_fore <= September_trig,
        October_threshold = October_fore <= October_trig,
        overall = case_when(
          July_threshold & August_threshold & September_threshold ~ TRUE,
          July_threshold & August_threshold & !September_threshold ~ TRUE,
          July_threshold & !August_threshold & September_threshold ~ TRUE,
          July_threshold & !August_threshold & !September_threshold ~ FALSE,
          !July_threshold & August_threshold & September_threshold ~ TRUE,
          !July_threshold & August_threshold & !September_threshold ~ FALSE,
          !July_threshold & !August_threshold & September_threshold ~ TRUE,
          !July_threshold & !August_threshold & !September_threshold ~ FALSE,
          .default = NA)) |>
      group_by(valid_year) |>
      summarise(
        woredas_reached = sum(overall, na.rm = TRUE),
        percent_reached = mean(overall, na.rm = TRUE), 
        trigger_reached = percent_reached > 0.2
      ) |>
      mutate(
        metric = case_when(
          trigger_reached & (valid_year %in% drought_years) ~ "TP",
          trigger_reached & !(valid_year %in% drought_years) ~ "FP",
          !trigger_reached & (valid_year %in% drought_years) ~ "FN",
          !trigger_reached & !(valid_year %in% drought_years) ~ "TN",
          TRUE ~ NA_character_
        )
      )
    
    overall_act <- (ecmwf_triggers_wfp_method |>
                      summarise(sum(trigger_reached, na.rm = TRUE)))$`sum(trigger_reached, na.rm = TRUE)`
    overall_rp <- total_years / overall_act
    overall_rate <- (overall_act / total_years) * 100
    overall_hit_rate <- round((sum(ecmwf_triggers_wfp_method$metric == "TP", na.rm = TRUE) /
                                 (sum(ecmwf_triggers_wfp_method$metric == "TP", 
                                      ecmwf_triggers_wfp_method$metric == "FN", na.rm = TRUE))) * 100, 1)
    overall_miss_rate <- round((sum(ecmwf_triggers_wfp_method$metric == "FN", na.rm = TRUE) /
                                  (sum(ecmwf_triggers_wfp_method$metric == "TP", 
                                       ecmwf_triggers_wfp_method$metric == "FN", na.rm = TRUE))) * 100, 1)
    overall_false_alarm_rate <- round((sum(ecmwf_triggers_wfp_method$metric == "FP", na.rm = TRUE) /
                                         (sum(ecmwf_triggers_wfp_method$metric == "FP", 
                                              ecmwf_triggers_wfp_method$metric == "TN", na.rm = TRUE))) * 100, 1)
    
    # Return a data frame with metrics for the current quantile value
    return(data.frame(
      quantile_value = quantile_value,
      return_period = overall_rp,
      rate_of_activation = overall_rate,
      hit_rate = overall_hit_rate,
      miss_rate = overall_miss_rate,
      false_alarm_rate = overall_false_alarm_rate
    ))
  }
  
  # Use map_dfr to iterate over quantile_values and combine results into a data frame
  results_df <- quantile_values |>
    map_dfr(~ calculate_metrics(.x))
  
  return(results_df)
}

quantile_values <- seq(0.075, 0.25, by = 0.005)
# Create a named vector for metric labels
metric_labels <- c(
  "rate_of_activation" = "Rate of Activation",
  "hit_rate" = "Hit Rate",
  "miss_rate" = "Miss Rate",
  "false_alarm_rate" = "False Alarm Rate",
  "return_period" = "Return Period",
  "Overall Return Period" = "Return Period"
)


results_df <- calculate_ecmwf_triggers_metrics_matrix(ond_forecast, drought_years, total_years, quantile_values)
# Plotting the results
results_long <- results_df |>
  pivot_longer(cols = -c(quantile_value, return_period), names_to = "metric", values_to = "value")
ggplot() +
  geom_line(data = results_long, aes(x = quantile_value, y = value, color = metric), linewidth = 1) +
  geom_line(data = results_df, aes(x = quantile_value, y = return_period * 10, color = "Overall Return Period"), linetype = "dashed", linewidth = 1) +
  scale_y_continuous(
    name = "Rate (%)",
    sec.axis = sec_axis(~ . / 10, name = "Return Period (years)")
  ) +
  scale_color_manual(values = setNames(scales::hue_pal()(length(unique(results_long$metric)) + 1), c(unique(results_long$metric), "Overall Return Period")),
                     labels = metric_labels) +
  labs(title = "ECMWF Trigger Metrics by Quantile",
       x = "Quantile for Each Lead Time",
       color = "Metric")


results_df <- calculate_ecmwf_triggers_metrics_matrix(ond_forecast, era5_drought_years, total_years, quantile_values)
# Plotting the results
results_long <- results_df |>
  pivot_longer(cols = -c(quantile_value, return_period), names_to = "metric", values_to = "value")
ggplot() +
  geom_line(data = results_long, aes(x = quantile_value, y = value, color = metric), linewidth = 1) +
  geom_line(data = results_df, aes(x = quantile_value, y = return_period * 10, color = "Overall Return Period"), linetype = "dashed", linewidth = 1) +
  scale_y_continuous(
    name = "Rate (%)",
    sec.axis = sec_axis(~ . / 10, name = "Return Period (years)")
  ) +
  scale_color_manual(values = setNames(scales::hue_pal()(length(unique(results_long$metric)) + 1), c(unique(results_long$metric), "Overall Return Period")),
                     labels = metric_labels) +
  labs(title = "ECMWF Trigger Metrics by Quantile",
       x = "Quantile for Each Lead Time",
       color = "Metric")


results_df <- calculate_ecmwf_triggers_metrics_matrix(ond_forecast, era5_drought_years, total_years, quantile_values = 0.125)
paste0("The Overall Return Period is 1-in-", round(results_df$return_period, 1), " years.")
paste0("The Overall Rate of Activation is ", round(results_df$rate_of_activation, 1), "%.")
paste0("The Overall Hit Rate is ", round(results_df$hit_rate, 1), "%.")
paste0("The Overall Miss Rate is ", round(results_df$miss_rate, 1), "%.")
paste0("The Overall False Alarm Rate is ", round(results_df$false_alarm_rate, 1), "%.")

############## 
# No matrix
calculate_ecmwf_triggers_metrics <- function(ond_forecast, drought_years, total_years, quantile_values) {
  
  # Define a function to calculate metrics for each quantile value
  calculate_metrics <- function(quantile_value) {
    ecmwf_triggers <- ond_forecast |>
      group_by(adm3_pcode, adm3_en, pub_month_name) |>
      summarise(triggers = quantile(seasonal_total, quantile_value, na.rm = TRUE), .groups = 'drop')
    
    ecmwf_triggers_wfp_method <- ecmwf_triggers |>
      pivot_wider(names_from = pub_month_name, values_from = triggers) |>
      merge(ond_forecast |>
              ungroup() |>
              dplyr::select(-pub_month) |>
              pivot_wider(names_from = pub_month_name, values_from = seasonal_total), 
            by = c("adm3_pcode", "adm3_en"), suffixes = c("_trig","_fore")) |>
      mutate(
        July_threshold = July_fore <= July_trig,
        August_threshold = August_fore <= August_trig,
        September_threshold = September_fore <= September_trig,
        October_threshold = October_fore <= October_trig,
        overall = case_when(
          July_threshold | August_threshold | September_threshold | October_threshold ~ TRUE,
          TRUE ~ FALSE  # Adjust if more months are added
        )
      ) |>
      group_by(valid_year) |>
      summarise(
        woredas_reached = sum(overall, na.rm = TRUE),
        percent_reached = mean(overall, na.rm = TRUE), 
        trigger_reached = percent_reached > 0.2
      ) |>
      mutate(
        metric = case_when(
          trigger_reached & (valid_year %in% drought_years) ~ "TP",
          trigger_reached & !(valid_year %in% drought_years) ~ "FP",
          !trigger_reached & (valid_year %in% drought_years) ~ "FN",
          !trigger_reached & !(valid_year %in% drought_years) ~ "TN",
          TRUE ~ NA_character_
        )
      )
    
    overall_act <- (ecmwf_triggers_wfp_method |>
                      summarise(sum(trigger_reached, na.rm = TRUE)))$`sum(trigger_reached, na.rm = TRUE)`
    overall_rp <- total_years / overall_act
    overall_rate <- (overall_act / total_years) * 100
    overall_hit_rate <- round((sum(ecmwf_triggers_wfp_method$metric == "TP", na.rm = TRUE) /
                                 (sum(ecmwf_triggers_wfp_method$metric == "TP", 
                                      ecmwf_triggers_wfp_method$metric == "FN", na.rm = TRUE))) * 100, 1)
    overall_miss_rate <- round((sum(ecmwf_triggers_wfp_method$metric == "FN", na.rm = TRUE) /
                                  (sum(ecmwf_triggers_wfp_method$metric == "TP", 
                                       ecmwf_triggers_wfp_method$metric == "FN", na.rm = TRUE))) * 100, 1)
    overall_false_alarm_rate <- round((sum(ecmwf_triggers_wfp_method$metric == "FP", na.rm = TRUE) /
                                         (sum(ecmwf_triggers_wfp_method$metric == "FP", 
                                              ecmwf_triggers_wfp_method$metric == "TN", na.rm = TRUE))) * 100, 1)
    
    # Return a data frame with metrics for the current quantile value
    return(data.frame(
      quantile_value = quantile_value,
      return_period = overall_rp,
      rate_of_activation = overall_rate,
      hit_rate = overall_hit_rate,
      miss_rate = overall_miss_rate,
      false_alarm_rate = overall_false_alarm_rate
    ))
  }
  
  # Use map_dfr to iterate over quantile_values and combine results into a data frame
  results_df <- quantile_values |>
    map_dfr(~ calculate_metrics(.x))
  
  return(results_df)
}
quantile_values <- seq(0.075, 0.25, by = 0.005)
# Create a named vector for metric labels
metric_labels <- c(
  "rate_of_activation" = "Rate of Activation",
  "hit_rate" = "Hit Rate",
  "miss_rate" = "Miss Rate",
  "false_alarm_rate" = "False Alarm Rate",
  "return_period" = "Return Period",
  "Overall Return Period" = "Return Period"
)


results_df <- calculate_ecmwf_triggers_metrics(ond_forecast, drought_years, total_years, quantile_values)
# Plotting the results
results_long <- results_df |>
  pivot_longer(cols = -c(quantile_value, return_period), names_to = "metric", values_to = "value")
ggplot() +
  geom_line(data = results_long, aes(x = quantile_value, y = value, color = metric), linewidth = 1) +
  geom_line(data = results_df, aes(x = quantile_value, y = return_period * 10, color = "Overall Return Period"), linetype = "dashed", linewidth = 1) +
  scale_y_continuous(
    name = "Rate (%)",
    sec.axis = sec_axis(~ . / 10, name = "Return Period (years)")
  ) +
  scale_color_manual(values = setNames(scales::hue_pal()(length(unique(results_long$metric)) + 1), c(unique(results_long$metric), "Overall Return Period")),
                     labels = metric_labels) +
  labs(title = "ECMWF Trigger Metrics by Quantile",
       x = "Quantile for Each Lead Time",
       color = "Metric")


results_df <- calculate_ecmwf_triggers_metrics(ond_forecast, era5_drought_years, total_years, quantile_values)
# Plotting the results
results_long <- results_df |>
  pivot_longer(cols = -c(quantile_value, return_period), names_to = "metric", values_to = "value")
ggplot() +
  geom_line(data = results_long, aes(x = quantile_value, y = value, color = metric), linewidth = 1) +
  geom_line(data = results_df, aes(x = quantile_value, y = return_period * 10, color = "Overall Return Period"), linetype = "dashed", linewidth = 1) +
  scale_y_continuous(
    name = "Rate (%)",
    sec.axis = sec_axis(~ . / 10, name = "Return Period (years)")
  ) +
  scale_color_manual(values = setNames(scales::hue_pal()(length(unique(results_long$metric)) + 1), c(unique(results_long$metric), "Overall Return Period")),
                     labels = metric_labels) +
  labs(title = "ECMWF Trigger Metrics by Quantile",
       x = "Quantile for Each Lead Time",
       color = "Metric")


results_df <- calculate_ecmwf_triggers_metrics(ond_forecast, era5_drought_years, total_years, quantile_values = 0.175)
paste0("The Overall Return Period is 1-in-", round(results_df$return_period, 1), " years.")
paste0("The Overall Rate of Activation is ", round(results_df$rate_of_activation, 1), "%.")
paste0("The Overall Hit Rate is ", round(results_df$hit_rate, 1), "%.")
paste0("The Overall Miss Rate is ", round(results_df$miss_rate, 1), "%.")
paste0("The Overall False Alarm Rate is ", round(results_df$false_alarm_rate, 1), "%.")


##################
# By month
calculate_ecmwf_triggers_monthly <- function(ond_forecast, drought_years, total_years, quantile_values) {
  
  # Define a function to calculate metrics for each quantile value
  calculate_metrics <- function(quantile_month) {
    quantile_value <- quantile_values[quantile_month]
    ecmwf_triggers <- ond_forecast |>
      filter(pub_month_name == quantile_month) |>
      group_by(adm3_pcode, adm3_en, pub_month_name) |>
      summarise(triggers = quantile(seasonal_total, quantile_value, na.rm = TRUE), .groups = 'drop')
  }
  # Use map_dfr to iterate over quantile_values and combine results into a data frame
  triggers_df <- names(quantile_values) |>
    map_dfr(~ calculate_metrics(.x))
  
  
  ecmwf_triggers_wfp_method <- triggers_df |>
    pivot_wider(names_from = pub_month_name, values_from = triggers) |>
    merge(ond_forecast |>
            ungroup() |>
            dplyr::select(-pub_month) |>
            pivot_wider(names_from = pub_month_name, values_from = seasonal_total), 
          by = c("adm3_pcode", "adm3_en"), suffixes = c("_trig","_fore")) |>
    mutate(
      July_threshold = July_fore <= July_trig,
      August_threshold = August_fore <= August_trig,
      September_threshold = September_fore <= September_trig,
      October_threshold = October_fore <= October_trig,
      overall = case_when(
        July_threshold | August_threshold | September_threshold | October_threshold ~ TRUE,
        TRUE ~ FALSE  # Adjust if more months are added
      )
    ) |>
    group_by(valid_year) |>
    summarise(
      woredas_reached = sum(overall, na.rm = TRUE),
      percent_reached = mean(overall, na.rm = TRUE), 
      trigger_reached = percent_reached > 0.2,
      July_woredas_reached = sum(July_threshold, na.rm = TRUE),
      July_percent_reached = mean(July_threshold, na.rm = TRUE), 
      July_trigger_reached = July_percent_reached > 0.2,
      August_woredas_reached = sum(August_threshold, na.rm = TRUE),
      August_percent_reached = mean(August_threshold, na.rm = TRUE), 
      August_trigger_reached = August_percent_reached > 0.2,
      September_woredas_reached = sum(September_threshold, na.rm = TRUE),
      September_percent_reached = mean(September_threshold, na.rm = TRUE), 
      September_trigger_reached = September_percent_reached > 0.2,
      October_woredas_reached = sum(October_threshold, na.rm = TRUE),
      October_percent_reached = mean(October_threshold, na.rm = TRUE), 
      October_trigger_reached = October_percent_reached > 0.2
    ) |>
    mutate(
      metric = case_when(
        trigger_reached & (valid_year %in% drought_years) ~ "TP",
        trigger_reached & !(valid_year %in% drought_years) ~ "FP",
        !trigger_reached & (valid_year %in% drought_years) ~ "FN",
        !trigger_reached & !(valid_year %in% drought_years) ~ "TN",
        TRUE ~ NA_character_
      ),
      July_metric = case_when(
        July_trigger_reached & (valid_year %in% drought_years) ~ "TP",
        July_trigger_reached & !(valid_year %in% drought_years) ~ "FP",
        !July_trigger_reached & (valid_year %in% drought_years) ~ "FN",
        !July_trigger_reached & !(valid_year %in% drought_years) ~ "TN",
        TRUE ~ NA_character_
      ),
      August_metric = case_when(
        August_trigger_reached & (valid_year %in% drought_years) ~ "TP",
        August_trigger_reached & !(valid_year %in% drought_years) ~ "FP",
        !August_trigger_reached & (valid_year %in% drought_years) ~ "FN",
        !August_trigger_reached & !(valid_year %in% drought_years) ~ "TN",
        TRUE ~ NA_character_
      ),
      September_metric = case_when(
        September_trigger_reached & (valid_year %in% drought_years) ~ "TP",
        September_trigger_reached & !(valid_year %in% drought_years) ~ "FP",
        !September_trigger_reached & (valid_year %in% drought_years) ~ "FN",
        !September_trigger_reached & !(valid_year %in% drought_years) ~ "TN",
        TRUE ~ NA_character_
      ),
      October_metric = case_when(
        October_trigger_reached & (valid_year %in% drought_years) ~ "TP",
        October_trigger_reached & !(valid_year %in% drought_years) ~ "FP",
        !October_trigger_reached & (valid_year %in% drought_years) ~ "FN",
        !October_trigger_reached & !(valid_year %in% drought_years) ~ "TN",
        TRUE ~ NA_character_
      )
    )
  
  overall_act <- (ecmwf_triggers_wfp_method |>
                    summarise(sum(trigger_reached, na.rm = TRUE)))$`sum(trigger_reached, na.rm = TRUE)`
  overall_rp <- total_years / overall_act
  overall_rate <- (overall_act / total_years) * 100
  overall_hit_rate <- round((sum(ecmwf_triggers_wfp_method$metric == "TP", na.rm = TRUE) /
                               (sum(ecmwf_triggers_wfp_method$metric == "TP", 
                                    ecmwf_triggers_wfp_method$metric == "FN", na.rm = TRUE))) * 100, 1)
  overall_miss_rate <- round((sum(ecmwf_triggers_wfp_method$metric == "FN", na.rm = TRUE) /
                                (sum(ecmwf_triggers_wfp_method$metric == "TP", 
                                     ecmwf_triggers_wfp_method$metric == "FN", na.rm = TRUE))) * 100, 1)
  overall_false_alarm_rate <- round((sum(ecmwf_triggers_wfp_method$metric == "FP", na.rm = TRUE) /
                                       (sum(ecmwf_triggers_wfp_method$metric == "FP", 
                                            ecmwf_triggers_wfp_method$metric == "TN", na.rm = TRUE))) * 100, 1)
  ecmwf_triggers_wfp_method |>
    summarise(overall_act = sum(trigger_reached, na.rm = T),
              return_period = total_years / overall_act,
              rate_of_activation = (overall_act / total_years) * 100,
              hit_rate = round((sum(metric == "TP", na.rm = T) /
                                  (sum(metric == "TP", 
                                       metric == "FN", na.rm = T))) * 100, 1),
              miss_rate = round((sum(metric == "FN", na.rm = T) /
                                   (sum(metric == "TP", 
                                        metric == "FN", na.rm = T))) * 100, 1),
              false_alarm_rate = round((sum(metric == "FP", na.rm = T) /
                                          (sum(metric == "TN", 
                                               metric == "FP", na.rm = T))) * 100, 1),
              July_overall_act = sum(July_trigger_reached, na.rm = T),
              July_return_period = total_years / July_overall_act,
              July_rate_of_activation = (July_overall_act / total_years) * 100,
              July_hit_rate = round((sum(July_metric == "TP", na.rm = T) /
                                       (sum(July_metric == "TP", 
                                            July_metric == "FN", na.rm = T))) * 100, 1),
              July_miss_rate = round((sum(July_metric == "FN", na.rm = T) /
                                        (sum(July_metric == "TP", 
                                             July_metric == "FN", na.rm = T))) * 100, 1),
              July_false_alarm_rate = round((sum(July_metric == "FP", na.rm = T) /
                                               (sum(July_metric == "TN", 
                                                    July_metric == "FP", na.rm = T))) * 100, 1),
              August_overall_act = sum(August_trigger_reached, na.rm = T),
              August_return_period = total_years / August_overall_act,
              August_rate_of_activation = (August_overall_act / total_years) * 100,
              August_hit_rate = round((sum(August_metric == "TP", na.rm = T) /
                                         (sum(August_metric == "TP", 
                                              August_metric == "FN", na.rm = T))) * 100, 1),
              August_miss_rate = round((sum(August_metric == "FN", na.rm = T) /
                                          (sum(August_metric == "TP", 
                                               August_metric == "FN", na.rm = T))) * 100, 1),
              August_false_alarm_rate = round((sum(August_metric == "FP", na.rm = T) /
                                                 (sum(August_metric == "TN", 
                                                      August_metric == "FP", na.rm = T))) * 100, 1),
              September_overall_act = sum(September_trigger_reached, na.rm = T),
              September_return_period = total_years / September_overall_act,
              September_rate_of_activation = (September_overall_act / total_years) * 100,
              September_hit_rate = round((sum(September_metric == "TP", na.rm = T) /
                                            (sum(September_metric == "TP", 
                                                 September_metric == "FN", na.rm = T))) * 100, 1),
              September_miss_rate = round((sum(September_metric == "FN", na.rm = T) /
                                             (sum(September_metric == "TP", 
                                                  September_metric == "FN", na.rm = T))) * 100, 1),
              September_false_alarm_rate = round((sum(September_metric == "FP", na.rm = T) /
                                                    (sum(September_metric == "TN", 
                                                         September_metric == "FP", na.rm = T))) * 100, 1),
              October_overall_act = sum(October_trigger_reached, na.rm = T),
              October_return_period = total_years / October_overall_act,
              October_rate_of_activation = (October_overall_act / total_years) * 100,
              October_hit_rate = round((sum(October_metric == "TP", na.rm = T) /
                                          (sum(October_metric == "TP", 
                                               October_metric == "FN", na.rm = T))) * 100, 1),
              October_miss_rate = round((sum(October_metric == "FN", na.rm = T) /
                                           (sum(October_metric == "TP", 
                                                October_metric == "FN", na.rm = T))) * 100, 1),
              October_false_alarm_rate = round((sum(October_metric == "FP", na.rm = T) /
                                                  (sum(October_metric == "TN", 
                                                       October_metric == "FP", na.rm = T))) * 100, 1)) |>
    pivot_longer(cols = everything(), 
                 names_to = "metric", values_to = "value") |>
    mutate(month = case_when(
      str_detect(metric, "^overall") ~ "Overall",
      str_detect(metric, "July") ~ "July",
      str_detect(metric, "August") ~ "August",
      str_detect(metric, "September") ~ "September",
      str_detect(metric, "October") ~ "October",
      .default = "Overall"
    ),
    metric = str_remove(metric, "^July_|^August_|^September_|^October_"))
}


quantile_values <- c("July" = 0.08, "August" = 0.07,
                     "September" = 0.075, "October" = 0.1)
# Create a named vector for metric labels
metric_labels <- c(
  "rate_of_activation" = "Rate of Activation",
  "hit_rate" = "Hit Rate",
  "miss_rate" = "Miss Rate",
  "false_alarm_rate" = "False Alarm Rate",
  "return_period" = "Return Period",
  "Overall Return Period" = "Return Period"
)

results_df <- calculate_ecmwf_triggers_monthly(ond_forecast, 
                                               drought_years, 
                                               total_years, 
                                               quantile_values)

ggplot(results_df |> filter(!metric %in% c("return_period", "overall_act")), 
       aes(x = factor(month, levels = c(month.name, "Overall")), 
                       y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Metrics across Lead Times", 
       x = "Month", y = "Rate (%)", fill = "Metric", color = "") +
  geom_line(data = results_df |> filter(metric == "return_period"), 
            aes(x = factor(month, levels = c(month.name, "Overall")), 
                y = value * 10, color = "Return Period", group = 1), 
            linetype = "dashed", linewidth = 1) +
  scale_y_continuous(
    name = "Rate (%)",
    sec.axis = sec_axis(~ . / 10, name = "Return Period (years)")
  ) +
  scale_fill_manual(values = setNames(scales::hue_pal()(6), 
                                       c(unique(results_df |> 
                                                  filter(metric != "return_period") |> 
                                                  pull(metric)), "Overall Return Period")),
                     labels = metric_labels)
