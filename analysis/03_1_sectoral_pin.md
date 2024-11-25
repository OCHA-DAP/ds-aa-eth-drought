---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.16.4
  kernelspec:
    display_name: R
    language: R
    name: ir
---

## PiN Summary

This notebook walks through a comparison of overall PiN values and sectoral PiNs from 2024/2025. It is intended to inform a dynamic drought trigger in Ethiopia, where PiN data is used as a proxy for vulnerability at the zonal (adm2) level. The overall PiN data is downloaded from HDX and sectoral PiN data is provided by the Ethiopia country office.

```R vscode={"languageId": "r"}
library(tidyverse)
library(sf)
library(readxl)
library(gghdx)
library(GGally)
gghdx()

options(repr.plot.width=20, repr.plot.height=10)
```

<!-- #region vscode={"languageId": "r"} -->
Read in all the datasets we'll be using.
<!-- #endregion -->

```R vscode={"languageId": "r"}
df_food_sec_2024 <- read_excel(
    path = file.path(Sys.getenv("AA_DATA_DIR"), "public", "exploration", "eth", "pin", "Food Security_PIN_Severity_2024.xlsx"),
    skip = 1,  
    col_names = TRUE,
    sheet = "Cluster PiN"
)

df_nutrition_2024 <- read_excel(
    path = file.path(Sys.getenv("AA_DATA_DIR"), "public", "exploration", "eth", "pin", "Nutrition PIN_Severity_2024.xlsx"),
    skip = 1,  
    col_names = TRUE,
    sheet = "Cluster PiN"
)

df_total_2024 <- read_excel(
    file.path(
        Sys.getenv("AA_DATA_DIR"),
        "public", "exploration", "eth", 
        "ethiopia-2024-humanitairan-needs-overview_hxl.xlsx"
    ),
    sheet = "Overall PIN"
) %>% 
    slice(-1) %>% 
    mutate(`Overall PIN` = as.numeric(`Overall PIN`)) 
```

## All population groups

Let's group by admin 2s. To start we'll sum up all population groups.

```R vscode={"languageId": "r"}
df_food_sec_2024_all <- df_food_sec_2024 %>%
    group_by(`Admin 2 P-Code`) %>%
    summarise(FoodSecPin2024 = sum(`Cluster PiN`, na.rm = TRUE)) %>%
    select("Admin 2 P-Code", "FoodSecPin2024")

df_nutrition_2024_all <- df_nutrition_2024 %>%
    group_by(`Admin 2 P-Code`) %>%
    summarise(NutritionPin2024 = sum(`Cluster PiN`, na.rm = TRUE)) %>%
    select("Admin 2 P-Code", "NutritionPin2024")

df_total_2024_all <- df_total_2024 %>%
    group_by(admin2Pcode) %>%
    summarise(TotalPin2024 = sum(`Overall PIN`, na.rm = TRUE)) %>%
    select("admin2Pcode", "TotalPin2024")

df_merged_all <- df_total_2024_all %>%
    full_join(df_food_sec_2024_all, by=c("admin2Pcode" = "Admin 2 P-Code")) %>%
    full_join(df_nutrition_2024_all, by=c("admin2Pcode" = "Admin 2 P-Code"))
```

Now we'll plot and compare the correlation between each of these.

```R vscode={"languageId": "r"}
correlation_plot <- ggpairs(
    df_merged_all %>% select(TotalPin2024, FoodSecPin2024, NutritionPin2024),
    title = "Correlation Matrix of 2024 PiN Indicators, by Zone in Ethiopia",
    lower = list(continuous = wrap("points", alpha = 0.5, size = 2)),
    diag = list(continuous = wrap("barDiag", bins = 30)),
    upper = list(continuous = wrap("cor", size = 5))
) + 
theme(axis.text.x = element_text(angle = 45))

cor_matrix <- cor(df_merged_all %>% select(TotalPin2024, FoodSecPin2024, NutritionPin2024),
                method = "pearson")
```

As we'd expect, the correlations between all indicators is highly significant (`***` = p < 0.001).

```R vscode={"languageId": "r"}
correlation_plot
```

```R vscode={"languageId": "r"}
cor_matrix
```
