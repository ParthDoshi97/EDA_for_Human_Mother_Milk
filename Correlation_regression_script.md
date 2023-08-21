---
title: "Milk Nutriention and Biomarker Assosition "
output: 
  html_document: 
    higlight: textamate
    theme: spacelab
    toc: yes
    fig_width: 10
    fig_height: 9
    keep_md: yes
  html_notebook: default
---

# Introduction

This notebook aims to explore the associations between nutrients and bio-markers using milk samples as the dataset. We will employ two main techniques for analysis: correlation and regression. Correlation will help us identify potential relationships between nutrients and bio-markers, while regression will allow us to predict bio-marker concentrations based on nutrient levels.




```r
source("C:/Users/Parth Doshi/Dropbox/Nutrishield_Study_II_Project (ParthD thesis)/R-script/EDA_for_Human_Mother_Milk/Functions.R")
```

```
## 
## Attaching package: 'psych'
```

```
## The following objects are masked from 'package:ggplot2':
## 
##     %+%, alpha
```

```r
# install required Libraries 
Libraies_requried <- c("tidyverse","caret","ggplot2","ggcorrplot","stats","psych","factoextra","FactoMineR","ranger","party")

check_and_install_libraries(Libraies_requried)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.2     ✔ readr     2.1.4
## ✔ forcats   1.0.0     ✔ stringr   1.5.0
## ✔ lubridate 1.9.2     ✔ tibble    3.2.1
## ✔ purrr     1.0.1     ✔ tidyr     1.3.0
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ psych::%+%()    masks ggplot2::%+%()
## ✖ psych::alpha()  masks ggplot2::alpha()
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
## Loading required package: lattice
## 
## 
## Attaching package: 'caret'
## 
## 
## The following object is masked from 'package:purrr':
## 
##     lift
## 
## 
## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
## 
## Loading required package: grid
## 
## Loading required package: mvtnorm
## 
## Loading required package: modeltools
## 
## Loading required package: stats4
## 
## Loading required package: strucchange
## 
## Loading required package: zoo
## 
## 
## Attaching package: 'zoo'
## 
## 
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
## 
## 
## Loading required package: sandwich
## 
## 
## Attaching package: 'strucchange'
## 
## 
## The following object is masked from 'package:stringr':
## 
##     boundary
## 
## 
## 
## Attaching package: 'party'
## 
## 
## The following object is masked from 'package:dplyr':
## 
##     where
```

```r
# load Required Libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(ggcorrplot)
library(stats)
library(psych)
library(factoextra)
library(FactoMineR)
library(ranger)
library(party)
```

# load Data sets

The data-sets include Short-chain Fatty Acids (SCFA), Qtrap Targeted and Semiquantitative Urinary Bio markers, Nutritional Data from Human Milk (HM) quality using (MIRIS), and Human Milk (HM) Fatty Acids (FAMES)


```r
NSII_SCFA_Urine <-  read.csv("C:/Users/Parth Doshi/Dropbox/Nutrishield_Study_II_Project (ParthD thesis)/NSII_Corrected_and_Clean_Data/NSII_SCFA_Urine.csv", sep = ",")
NSII_Qtrap_Targeted_Urine <-  read.csv("C:/Users/Parth Doshi/Dropbox/Nutrishield_Study_II_Project (ParthD thesis)/NSII_Corrected_and_Clean_Data/NSII_Qtrap_Targeted_Urine.csv", sep = ",") 
NSII_Qtrap_Semiquant_Urine <-  read.csv("C:/Users/Parth Doshi/Dropbox/Nutrishield_Study_II_Project (ParthD thesis)/NSII_Corrected_and_Clean_Data/NSII_Qtrap_Semiquant_Urine.csv", sep = ",")
NSII_MIRIS_HM <-  read.csv("C:/Users/Parth Doshi/Dropbox/Nutrishield_Study_II_Project (ParthD thesis)/NSII_Corrected_and_Clean_Data/NSII_MIRIS_HM_Subset.csv", sep = ",")
NSII_FAMES_HM <-  read.csv("C:/Users/Parth Doshi/Dropbox/Nutrishield_Study_II_Project (ParthD thesis)/NSII_Corrected_and_Clean_Data/NSII_FAMES_HM_subset.csv", sep = ",")
```

### Data preparation


```r
# remove class unwanted Column from data frame
NSII_SCFA_Urine <- NSII_SCFA_Urine %>%
select(-X,-Class,-Sample,-NewClass)

NSII_Qtrap_Targeted_Urine <- NSII_Qtrap_Targeted_Urine %>%
select(-X,-Class,-Sample,-NewClass)

NSII_Qtrap_Semiquant_Urine <- NSII_Qtrap_Semiquant_Urine %>%
select(-X,-Class,-Sample,-NewClass)

NSII_MIRIS_HM <- NSII_MIRIS_HM %>% 
  select(-c(1,2,3),-NewClass)

NSII_FAMES_HM <- NSII_FAMES_HM %>%
  select(-c(1,2,3),-NewClass)


# edit some of typos in categorial  columns

NSII_MIRIS_HM$Month[NSII_MIRIS_HM$Month == "6.1"] <- "6"
NSII_MIRIS_HM$Month[NSII_MIRIS_HM$Month == "L2"] <- "6"
NSII_MIRIS_HM$Month[NSII_MIRIS_HM$Month == "2B"] <- "2"
NSII_MIRIS_HM$ID[NSII_MIRIS_HM$ID == "P07.6"] <- "P7"
NSII_MIRIS_HM$ID[NSII_MIRIS_HM$ID == "T54.1HM"] <- "T54"
NSII_FAMES_HM$Month[NSII_FAMES_HM$Month == "2B"] <- "2"
NSII_FAMES_HM$Month[NSII_FAMES_HM$Month == "R"] <- "RBW"
NSII_FAMES_HM$Month[NSII_FAMES_HM$Month == "C"] <- "CEN"
NSII_FAMES_HM$Month[NSII_FAMES_HM$Month == "2B"] <- "2"
NSII_Qtrap_Targeted_Urine$Type[NSII_Qtrap_Targeted_Urine$Type == "IU2"] <- "IU"
```

#### Removing features

Removed the features that predominantly consist of missing data, remain constant throughout, or select those variable which show variation concerning months.


```r
# Targeted Biomarkers
NSII_Qtrap_Targeted_Urine <- NSII_Qtrap_Targeted_Urine %>%
  select(ID,Month,Type,Phloretin,O.DMA,Hesperetin,L.Tyrosine,X1.Methylhistidine,X3.Methylhistidine,Gallic.Acid)

# SCFA 
NSII_SCFA_Urine <- NSII_SCFA_Urine %>% 
  select(-Acetic)

# FAMES
NSII_FAMES_HM <- NSII_FAMES_HM %>% 
  select(-Undecanoate,-Tridecanoate,-Tricosanoate,-Heneicosanoate,-Docosanoate,-`cis.10.Pentadecenoic`,-`cis.11.14.17.Eicosatrienoic`)

# Semiquant Bio markers
# Calculate the percentage of missing values for each column
missing_percent <- colSums(NSII_Qtrap_Semiquant_Urine == 0 | NSII_Qtrap_Semiquant_Urine == 0.0) / nrow(NSII_Qtrap_Semiquant_Urine) * 100
# Get the names of columns with missing values exceeding 50%
missing_variables <- names(missing_percent[missing_percent > 50])
NSII_Qtrap_Semiquant_Urine <- NSII_Qtrap_Semiquant_Urine[, !names(NSII_Qtrap_Semiquant_Urine) %in% missing_variables]
```

#### Data Normalization


```r
# Scaling Data using Box-Cox transformation

NSII_SCFA_Urine_Scaled <- scale_data(NSII_SCFA_Urine)
NSII_Qtrap_Targeted_Urine_Scaled <- scale_data(NSII_Qtrap_Targeted_Urine)
NSII_Qtrap_Semiquant_Urine_Scaled <- scale_data(NSII_Qtrap_Semiquant_Urine)
NSII_MIRIS_HM_Scaled <- scale_data(NSII_MIRIS_HM)
NSII_FAMES_HM_Scaled <- scale_data(NSII_FAMES_HM)
```

#### spiting data Bio marker data set in Mother and Infants


```r
split_and_create_dataframes(NSII_SCFA_Urine_Scaled)
```

```
## <environment: R_GlobalEnv>
```

```r
split_and_create_dataframes(NSII_Qtrap_Targeted_Urine_Scaled)
```

```
## <environment: R_GlobalEnv>
```

```r
split_and_create_dataframes(NSII_Qtrap_Semiquant_Urine_Scaled)
```

```
## <environment: R_GlobalEnv>
```

# Correlation

In this we will find correlation between Bio markers And nutrients based on Month

### SCFA VS MILK(MIRIS)


```r
library(gridExtra)
```

```
## Warning: package 'gridExtra' was built under R version 4.2.3
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```r
# merge SCFA and MILK DATA based on ID and MONTH
merge_SCFA_MIRIS_IU <- merge(NSII_MIRIS_HM_Scaled, NSII_SCFA_Urine_Scaled_IU, by = c("ID","Month"))
merge_SCFA_MIRIS_MU <- merge(NSII_MIRIS_HM_Scaled, NSII_SCFA_Urine_Scaled_MU, by = c("ID","Month"))

# Converting CEN AND RBW to 0 as they are consider Time 0 
merge_SCFA_MIRIS_IU <- convert_month_to_zero(merge_SCFA_MIRIS_IU)
merge_SCFA_MIRIS_MU <- convert_month_to_zero(merge_SCFA_MIRIS_MU)

# split the Data set according to Month 
split_and_create_dataframe_month(merge_SCFA_MIRIS_IU)
```

```
## <environment: R_GlobalEnv>
```

```r
split_and_create_dataframe_month(merge_SCFA_MIRIS_MU)
```

```
## <environment: R_GlobalEnv>
```

```r
# Plot Correlation for each month for infants

Plot_1 <- calculate_and_visualize_correlation(merge_SCFA_MIRIS_IU_0,"pearson","Month 0")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
Plot_2 <- calculate_and_visualize_correlation(merge_SCFA_MIRIS_IU_1,"pearson","Month 1")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

```r
Plot_3 <- calculate_and_visualize_correlation(merge_SCFA_MIRIS_IU_2,"pearson","Month 2")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-7-3.png)<!-- -->

```r
Plot_4 <- calculate_and_visualize_correlation(merge_SCFA_MIRIS_IU_3,"pearson","Month 3")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-7-4.png)<!-- -->

```r
combined_plots <- grid.arrange(Plot_1,Plot_2,Plot_3,Plot_4, ncol = 2)
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-7-5.png)<!-- -->

```r
print(combined_plots)
```

```
## TableGrob (2 x 2) "arrange": 4 grobs
##   z     cells    name           grob
## 1 1 (1-1,1-1) arrange gtable[layout]
## 2 2 (1-1,2-2) arrange gtable[layout]
## 3 3 (2-2,1-1) arrange gtable[layout]
## 4 4 (2-2,2-2) arrange gtable[layout]
```

```r
# Plot Correlation for each month for Mothers
 calculate_and_visualize_correlation(merge_SCFA_MIRIS_MU_0,"pearson","Month 0")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-7-6.png)<!-- -->

```r
 calculate_and_visualize_correlation(merge_SCFA_MIRIS_MU_1,"pearson","Month 1")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-7-7.png)<!-- -->

```r
 calculate_and_visualize_correlation(merge_SCFA_MIRIS_MU_2,"pearson","Month 2")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-7-8.png)<!-- -->

```r
 calculate_and_visualize_correlation(merge_SCFA_MIRIS_MU_3,"pearson","Month 3")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-7-9.png)<!-- -->

### SCFA VS Fatty Acids (FAMES)


```r
# merge SCFA and MILK DATA based on ID and MONTH
merge_SCFA_FAMES_IU <- merge(NSII_FAMES_HM_Scaled, NSII_SCFA_Urine_Scaled_IU, by = c("ID","Month"))
merge_SCFA_FAMES_MU <- merge(NSII_FAMES_HM_Scaled, NSII_SCFA_Urine_Scaled_MU, by = c("ID","Month"))

# Converting CEN AND RBW to 0 as they are consider Time 0 
merge_SCFA_FAMES_IU <- convert_month_to_zero(merge_SCFA_FAMES_IU)
merge_SCFA_FAMES_MU <- convert_month_to_zero(merge_SCFA_FAMES_MU)

# split the Data set according to Month 
split_and_create_dataframe_month(merge_SCFA_FAMES_IU)
```

```
## <environment: R_GlobalEnv>
```

```r
split_and_create_dataframe_month(merge_SCFA_FAMES_MU)
```

```
## <environment: R_GlobalEnv>
```

```r
# Plot Correlation for each month for infants

calculate_and_visualize_correlation(merge_SCFA_FAMES_IU_0,"pearson","correlation plot SCFA(Infants) VS MILK(FAMES) for Month 0")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_SCFA_FAMES_IU_1,"pearson","correlation plot SCFA(Infants) VS MILK(FAMES) for Month 1")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_SCFA_FAMES_IU_2,"pearson","correlation plot SCFA(Infants) VS MILK(FAMES) for Month 2")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-8-3.png)<!-- -->

```r
 calculate_and_visualize_correlation(merge_SCFA_FAMES_IU_3,"pearson","correlation plot SCFA(Infants) VS MILK(FAMES) for Month 3")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-8-4.png)<!-- -->

```r
# Plot Correlation for each month for Mothers

calculate_and_visualize_correlation(merge_SCFA_FAMES_MU_0,"pearson","correlation plot SCFA(Mother) VS MILK(FAMES) for Month 0 ")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-8-5.png)<!-- -->

```r
 calculate_and_visualize_correlation(merge_SCFA_FAMES_MU_1,"pearson","correlation plot SCFA(Mother) VS MILK(FAMES) for Month 1")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-8-6.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_SCFA_FAMES_MU_2,"pearson","correlation plot SCFA(Mother) VS MILK(FAMES) for Month 2")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-8-7.png)<!-- -->

```r
 calculate_and_visualize_correlation(merge_SCFA_FAMES_MU_3,"pearson","correlation plot SCFA(Mother) VS MILK(FAMES) for Month 3")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-8-8.png)<!-- -->

#### Targeted bio markers vs MILK(miris)


```r
# merge Targeted Biomkers and MILK DATA based on ID and MONTH
merge_Targeted_MIRIS_IU <- merge(NSII_MIRIS_HM_Scaled, NSII_Qtrap_Targeted_Urine_Scaled_IU, by = c("ID","Month"))
merge_Targeted_MIRIS_MU <- merge(NSII_MIRIS_HM_Scaled, NSII_Qtrap_Targeted_Urine_Scaled_MU, by = c("ID","Month"))

# Converting CEN AND RBW to 0 as they are consider Time 0 
merge_Targeted_MIRIS_IU <- convert_month_to_zero(merge_Targeted_MIRIS_IU)
merge_Targeted_MIRIS_MU <- convert_month_to_zero(merge_Targeted_MIRIS_MU)

# split the Data set according to Month 
split_and_create_dataframe_month(merge_Targeted_MIRIS_IU)
```

```
## <environment: R_GlobalEnv>
```

```r
split_and_create_dataframe_month(merge_Targeted_MIRIS_MU)
```

```
## <environment: R_GlobalEnv>
```

```r
# Plot Correlation for each month for infants

calculate_and_visualize_correlation(merge_Targeted_MIRIS_IU_0,"pearson","correlation plot Targeted(Infants) VS MILK(MIRIS) for Month 0")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Targeted_MIRIS_IU_1,"pearson","correlation plot Targeted(Infants) VS MILK(MIRIS) for Month 1")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Targeted_MIRIS_IU_2,"pearson","correlation plot Targeted(Infants) VS MILK(MIRIS) for Month 2")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-9-3.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Targeted_MIRIS_IU_3,"pearson","correlation plot Targeted(Infants) VS MILK(MIRIS) for Month 3")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-9-4.png)<!-- -->

```r
# Plot Correlation for each month for Mothers

calculate_and_visualize_correlation(merge_Targeted_MIRIS_MU_0,"pearson","correlation plot Targeted(Mother) VS MILK(MIRIS) for Month 0 ")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-9-5.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Targeted_MIRIS_MU_1,"pearson","correlation plot Targeted(Mother) VS MILK(MIRIS) for Month 1")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-9-6.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Targeted_MIRIS_MU_2,"pearson","correlation plot Targeted(Mother) VS MILK(MIRIS) for Month 2")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-9-7.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Targeted_MIRIS_MU_3,"pearson","correlation plot Targeted(Mother) VS MILK(MIRIS) for Month 3")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-9-8.png)<!-- -->

#### Targeted bio-markers vs MILK(FAMES)


```r
# merge Targeted Biomkers and MILK DATA based on ID and MONTH
merge_Targeted_FAMES_IU <- merge(NSII_FAMES_HM_Scaled, NSII_Qtrap_Targeted_Urine_Scaled_IU, by = c("ID","Month"))
merge_Targeted_FAMES_MU <- merge(NSII_FAMES_HM_Scaled, NSII_Qtrap_Targeted_Urine_Scaled_MU, by = c("ID","Month"))

# Converting CEN AND RBW to 0 as they are consider Time 0 
merge_Targeted_FAMES_IU <- convert_month_to_zero(merge_Targeted_FAMES_IU)
merge_Targeted_FAMES_MU <- convert_month_to_zero(merge_Targeted_FAMES_MU)

# split the Data set according to Month 
split_and_create_dataframe_month(merge_Targeted_FAMES_IU)
```

```
## <environment: R_GlobalEnv>
```

```r
split_and_create_dataframe_month(merge_Targeted_FAMES_MU)
```

```
## <environment: R_GlobalEnv>
```

```r
# Plot Correlation for each month for infants

calculate_and_visualize_correlation(merge_Targeted_FAMES_IU_0,"pearson","correlation plot Targeted(Infants) VS MILK(FAMES) for Month 0")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Targeted_FAMES_IU_1,"pearson","correlation plot Targeted(Infants) VS MILK(FAMES) for Month 1")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-10-2.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Targeted_FAMES_IU_2,"pearson","correlation plot Targeted(Infants) VS MILK(FAMES) for Month 2")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-10-3.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Targeted_FAMES_IU_3,"pearson","correlation plot Targeted(Infants) VS MILK(FAMES) for Month 3")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-10-4.png)<!-- -->

```r
# Plot Correlation for each month for Mothers

calculate_and_visualize_correlation(merge_Targeted_FAMES_MU_0,"pearson","correlation plot Targeted(Mother) VS MILK(FAMES) for Month 0 ")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-10-5.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Targeted_FAMES_MU_1,"pearson","correlation plot Targeted(Mother) VS MILK(FAMES) for Month 1")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-10-6.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Targeted_FAMES_MU_2,"pearson","correlation plot Targeted(Mother) VS MILK(FAMES) for Month 2")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-10-7.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Targeted_FAMES_MU_3,"pearson","correlation plot Targeted(Mother) VS MILK(FAMES) for Month 3")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-10-8.png)<!-- -->

#### Semiquant bio-markers vs MILK(miris)


```r
# merge Semiquant Biomkers and MILK DATA based on ID and MONTH
merge_Semiquant_MIRIS_IU <- merge(NSII_MIRIS_HM_Scaled, NSII_Qtrap_Semiquant_Urine_Scaled_IU, by = c("ID","Month"))
merge_Semiquant_MIRIS_MU <- merge(NSII_MIRIS_HM_Scaled, NSII_Qtrap_Semiquant_Urine_Scaled_MU, by = c("ID","Month"))

# Converting CEN AND RBW to 0 as they are consider Time 0 
merge_Semiquant_MIRIS_IU <- convert_month_to_zero(merge_Semiquant_MIRIS_IU)
merge_Semiquant_MIRIS_MU <- convert_month_to_zero(merge_Semiquant_MIRIS_MU)

# split the Data set according to Month 
split_and_create_dataframe_month(merge_Semiquant_MIRIS_IU)
```

```
## <environment: R_GlobalEnv>
```

```r
split_and_create_dataframe_month(merge_Semiquant_MIRIS_MU)
```

```
## <environment: R_GlobalEnv>
```

```r
# Plot Correlation for each month for infants

calculate_and_visualize_correlation(merge_Semiquant_MIRIS_IU_0,"pearson","correlation plot Semiquant(Infants) VS MILK(MIRIS) for Month 0")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Semiquant_MIRIS_IU_1,"pearson","correlation plot Semiquant(Infants) VS MILK(MIRIS) for Month 1")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-11-2.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Semiquant_MIRIS_IU_2,"pearson","correlation plot Semiquant(Infants) VS MILK(MIRIS) for Month 2")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-11-3.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Semiquant_MIRIS_IU_3,"pearson","correlation plot Semiquant(Infants) VS MILK(MIRIS) for Month 3")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-11-4.png)<!-- -->

```r
# Plot Correlation for each month for Mothers

calculate_and_visualize_correlation(merge_Semiquant_MIRIS_MU_0,"pearson","correlation plot Semiquant(Mother) VS MILK(MIRIS) for Month 0")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-11-5.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Semiquant_MIRIS_MU_1,"pearson","correlation plot Semiquant(Mother) VS MILK(MIRIS) for Month 1")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-11-6.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Semiquant_MIRIS_MU_2,"pearson","correlation plot Semiquant(Mother) VS MILK(MIRIS) for Month 2")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-11-7.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Semiquant_MIRIS_MU_3,"pearson","correlation plot Semiquant(Mother) VS MILK(MIRIS) for Month 3")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-11-8.png)<!-- -->

#### Semiquant bio markers vs MILK(FAMES)


```r
# merge Semiquant Biomkers and MILK DATA based on ID and MONTH
merge_Semiquant_FAMES_IU <- merge(NSII_FAMES_HM_Scaled, NSII_Qtrap_Semiquant_Urine_Scaled_IU, by = c("ID","Month"))
merge_Semiquant_FAMES_MU <- merge(NSII_FAMES_HM_Scaled, NSII_Qtrap_Semiquant_Urine_Scaled_MU, by = c("ID","Month"))

# Converting CEN AND RBW to 0 as they are consider Time 0 
merge_Semiquant_FAMES_IU <- convert_month_to_zero(merge_Semiquant_FAMES_IU)
merge_Semiquant_FAMES_MU <- convert_month_to_zero(merge_Semiquant_FAMES_MU)

# split the Data set according to Month 
split_and_create_dataframe_month(merge_Semiquant_FAMES_IU)
```

```
## <environment: R_GlobalEnv>
```

```r
split_and_create_dataframe_month(merge_Semiquant_FAMES_MU)
```

```
## <environment: R_GlobalEnv>
```

```r
# Plot Correlation for each month for infants

calculate_and_visualize_correlation(merge_Semiquant_FAMES_IU_0,"pearson","correlation plot Semiquant(Infants) VS MILK(FAMES) for Month 0")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Semiquant_FAMES_IU_1,"pearson","correlation plot Semiquant(Infants) VS MILK(FAMES) for Month 1")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-12-2.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Semiquant_FAMES_IU_2,"pearson","correlation plot Semiquant(Infants) VS MILK(FAMES) for Month 2")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-12-3.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Semiquant_FAMES_IU_3,"pearson","correlation plot Semiquant(Infants) VS MILK(FAMES) for Month 3")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-12-4.png)<!-- -->

```r
# Plot Correlation for each month for Mothers

calculate_and_visualize_correlation(merge_Semiquant_FAMES_MU_0,"pearson","correlation plot Semiquant(Mother) VS MILK(FAMES) for Month 0 ")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-12-5.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Semiquant_FAMES_MU_1,"pearson","correlation plot Semiquant(Mother) VS MILK(FAMES) for Month 1")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-12-6.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Semiquant_FAMES_MU_2,"pearson","correlation plot Semiquant(Mother) VS MILK(FAMES) for Month 2")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-12-7.png)<!-- -->

```r
calculate_and_visualize_correlation(merge_Semiquant_FAMES_MU_3,"pearson","correlation plot Semiquant(Mother) VS MILK(FAMES) for Month 3")
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-12-8.png)<!-- -->

# Regression

In this study, we will employ a Multiple Regression model to predict the levels of Short-Chain Fatty Acids (SCFA) using Milk data, specifically MIRIS and FAMES, from various months. The primary objective is to develop a predictive model that can estimate SCFA levels in different months based on data collected from other months. By establishing such a model, we aim to explore the potential relationships between Milk components and SCFA levels and investigate whether these relationships hold true across different time periods.

## MILK (MIRIS)

#### Regression model for Month 0


```r
library(Metrics)
```

```
## Warning: package 'Metrics' was built under R version 4.2.3
```

```
## 
## Attaching package: 'Metrics'
```

```
## The following objects are masked from 'package:caret':
## 
##     precision, recall
```

```r
# Regression model for Month 1 
# remove Categorical variable
merge_SCFA_MIRIS_IU_0_new <- merge_SCFA_MIRIS_IU_0 %>% 
  select(-Month,-Type,-ID,)
merge_SCFA_MIRIS_IU_1_new <- merge_SCFA_MIRIS_IU_0 %>% 
  select(-Month,-Type,-ID,)
merge_SCFA_MIRIS_IU_2_new <- merge_SCFA_MIRIS_IU_0 %>% 
  select(-Month,-Type,-ID,)
merge_SCFA_MIRIS_IU_3_new <- merge_SCFA_MIRIS_IU_0 %>% 
  select(-Month,-Type,-ID,)


# Split the data into training (70%) and testing (30%) sets
train_index <- sample(1:nrow(merge_SCFA_MIRIS_IU_0_new), 0.7 * nrow(merge_SCFA_MIRIS_IU_0_new))
train_data <- merge_SCFA_MIRIS_IU_0_new[train_index, ]
test_data <- merge_SCFA_MIRIS_IU_0_new[-train_index, ]



# Create a matrix of the dependent variables (SCFAs)
scfa_matrix <- train_data %>% 
  select(7:17)
scfa_matrix <- as.matrix(scfa_matrix)

# Build the multiple linear regression model
model_miris_IU_0 <- lm(scfa_matrix ~ Fat + Crude_Protein + True_Protein, data = train_data)
summary(model_miris_IU_0)
```

```
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Fat + Crude_Protein + True_Protein, 
##     data = train_data)
## 
## Residuals:
##      21      10      14      25      24      13 
## -6.9356 -2.6606 -0.2024 10.9856  3.8249 -5.0118 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)     -5.646     49.930  -0.113    0.920
## Fat              4.887     15.747   0.310    0.786
## Crude_Protein   17.179     49.178   0.349    0.760
## True_Protein   -15.333     80.269  -0.191    0.866
## 
## Residual standard error: 10.38 on 2 degrees of freedom
## Multiple R-squared:  0.3244,	Adjusted R-squared:  -0.6891 
## F-statistic: 0.3201 on 3 and 2 DF,  p-value: 0.8153
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Fat + Crude_Protein + True_Protein, 
##     data = train_data)
## 
## Residuals:
##      21      10      14      25      24      13 
##  7139.1  2524.6 -3472.2 -9730.4  -568.3  4107.3 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)  
## (Intercept)    -141757      45777  -3.097   0.0904 .
## Fat              41176      14437   2.852   0.1041  
## Crude_Protein   -68421      45087  -1.518   0.2684  
## True_Protein    231429      73592   3.145   0.0880 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9520 on 2 degrees of freedom
## Multiple R-squared:  0.8625,	Adjusted R-squared:  0.6563 
## F-statistic: 4.182 on 3 and 2 DF,  p-value: 0.199
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Fat + Crude_Protein + True_Protein, data = train_data)
## 
## Residuals:
##      21      10      14      25      24      13 
## -4.2994 -1.2111  7.4072  3.5817 -4.5237 -0.9547 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)  
## (Intercept)     153.44      35.50   4.322   0.0496 *
## Fat             -52.40      11.20  -4.680   0.0427 *
## Crude_Protein   -29.26      34.97  -0.837   0.4908  
## True_Protein    -44.48      57.07  -0.779   0.5173  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.383 on 2 degrees of freedom
## Multiple R-squared:  0.9706,	Adjusted R-squared:  0.9264 
## F-statistic: 21.99 on 3 and 2 DF,  p-value: 0.04381
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Fat + Crude_Protein + True_Protein, 
##     data = train_data)
## 
## Residuals:
##     21     10     14     25     24     13 
##  5.566  1.723 -6.925 -5.780  3.417  1.998 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)      92.21      38.91   2.370    0.141
## Fat             -20.47      12.27  -1.668    0.237
## Crude_Protein    14.29      38.33   0.373    0.745
## True_Protein    -96.82      62.56  -1.548    0.262
## 
## Residual standard error: 8.092 on 2 degrees of freedom
## Multiple R-squared:  0.6875,	Adjusted R-squared:  0.2187 
## F-statistic: 1.466 on 3 and 2 DF,  p-value: 0.43
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Fat + Crude_Protein + True_Protein, 
##     data = train_data)
## 
## Residuals:
##       21       10       14       25       24       13 
##  0.02466 -0.01505 -0.42054  0.14148  0.37198 -0.10254 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)     1.4517     2.0016   0.725    0.544
## Fat            -0.6780     0.6313  -1.074    0.395
## Crude_Protein   0.5735     1.9715   0.291    0.799
## True_Protein   -1.4739     3.2179  -0.458    0.692
## 
## Residual standard error: 0.4163 on 2 degrees of freedom
## Multiple R-squared:  0.5237,	Adjusted R-squared:  -0.1907 
## F-statistic: 0.733 on 3 and 2 DF,  p-value: 0.621
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Fat + Crude_Protein + True_Protein, data = train_data)
## 
## Residuals:
##       21       10       14       25       24       13 
##  -4.3162  -0.5625  18.6683  -1.2180 -14.8223   2.2508 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)     46.625     82.845   0.563    0.630
## Fat             -6.572     26.127  -0.252    0.825
## Crude_Protein  -45.212     81.597  -0.554    0.635
## True_Protein    17.695    133.184   0.133    0.906
## 
## Residual standard error: 17.23 on 2 degrees of freedom
## Multiple R-squared:  0.2947,	Adjusted R-squared:  -0.7633 
## F-statistic: 0.2785 on 3 and 2 DF,  p-value: 0.84
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Fat + Crude_Protein + True_Protein, data = train_data)
## 
## Residuals:
##       21       10       14       25       24       13 
##   2.8835   0.4278 -11.5767   0.4302   9.0831  -1.2479 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)      40.64      51.20   0.794    0.511
## Fat             -24.73      16.15  -1.531    0.265
## Crude_Protein    62.22      50.43   1.234    0.343
## True_Protein    -86.04      82.31  -1.045    0.406
## 
## Residual standard error: 10.65 on 2 degrees of freedom
## Multiple R-squared:  0.6665,	Adjusted R-squared:  0.1662 
## F-statistic: 1.332 on 3 and 2 DF,  p-value: 0.4559
## 
## 
## Response Heptanoic :
## 
## Call:
## lm(formula = Heptanoic ~ Fat + Crude_Protein + True_Protein, 
##     data = train_data)
## 
## Residuals:
##      21      10      14      25      24      13 
##  0.9883  0.3231 -0.9338 -1.1529  0.3361  0.4392 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)     -5.786      6.440  -0.898    0.464
## Fat              2.211      2.031   1.088    0.390
## Crude_Protein   -5.876      6.343  -0.926    0.452
## True_Protein    13.722     10.354   1.325    0.316
## 
## Residual standard error: 1.339 on 2 degrees of freedom
## Multiple R-squared:  0.4792,	Adjusted R-squared:  -0.3019 
## F-statistic: 0.6135 on 3 and 2 DF,  p-value: 0.6682
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Fat + Crude_Protein + True_Protein, data = train_data)
## 
## Residuals:
##     21     10     14     25     24     13 
##  44.60  19.87  48.67 -90.95 -67.95  45.76 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)     633.42     477.65   1.326    0.316
## Fat            -101.69     150.64  -0.675    0.569
## Crude_Protein    58.22     470.46   0.124    0.913
## True_Protein   -653.40     767.89  -0.851    0.484
## 
## Residual standard error: 99.34 on 2 degrees of freedom
## Multiple R-squared:  0.4665,	Adjusted R-squared:  -0.3337 
## F-statistic: 0.583 on 3 and 2 DF,  p-value: 0.6813
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Fat + Crude_Protein + True_Protein, data = train_data)
## 
## Residuals:
##      21      10      14      25      24      13 
##  222.79   74.12 -188.63 -269.27   55.73  105.25 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)    -1676.0     1432.0  -1.170    0.362
## Fat              536.0      451.6   1.187    0.357
## Crude_Protein   -839.9     1410.5  -0.595    0.612
## True_Protein    2934.7     2302.2   1.275    0.330
## 
## Residual standard error: 297.8 on 2 degrees of freedom
## Multiple R-squared:  0.5115,	Adjusted R-squared:  -0.2214 
## F-statistic: 0.6979 on 3 and 2 DF,  p-value: 0.6342
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Fat + Crude_Protein + True_Protein, 
##     data = train_data)
## 
## Residuals:
##     21     10     14     25     24     13 
##  45.67  14.11 -57.27 -47.22  28.45  16.26 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)     681.81     320.18   2.129    0.167
## Fat            -134.59     100.98  -1.333    0.314
## Crude_Protein    31.82     315.36   0.101    0.929
## True_Protein   -641.76     514.73  -1.247    0.339
## 
## Residual standard error: 66.59 on 2 degrees of freedom
## Multiple R-squared:  0.6476,	Adjusted R-squared:  0.119 
## F-statistic: 1.225 on 3 and 2 DF,  p-value: 0.4788
```

```r
# Make predictions on the test data using the model
predictions <- predict(model_miris_IU_0, newdata = test_data)
# RMSE Values
model_stat_0 <- calculate_mse_and_rmse(test_data,predictions)
print(" rmse values for month 0")
```

```
## [1] " rmse values for month 0"
```

```r
print(model_stat_0$rmse)
```

```
##     Propionic    Isobutyric       Butyric X2.Me.butyric    Isovaleric 
##  7.809477e+00  2.432959e+04  3.031622e+01  1.769455e+01  2.982263e-01 
##       Valeric       Caproic     Heptanoic        Valine       Leucine 
##  2.254530e+01  2.035288e+01  2.163792e+00  8.823744e+01  3.554486e+02 
##    Isoleucine 
##  1.018401e+02
```

```r
# # Make predictions on the month 1 data using the model of month 0
prediction_month_1 <- predict(model_miris_IU_0, merge_SCFA_MIRIS_IU_1_new)
model_stat_0_1 <- calculate_mse_and_rmse(merge_SCFA_MIRIS_IU_1_new,prediction_month_1)
print(" rmse values for month 1")
```

```
## [1] " rmse values for month 1"
```

```r
print(model_stat_0_1$rmse)
```

```
##     Propionic    Isobutyric       Butyric X2.Me.butyric    Isovaleric 
##  6.655140e+00  1.474621e+04  1.784576e+01  1.090497e+01  2.610673e-01 
##       Valeric       Caproic     Heptanoic        Valine       Leucine 
##  1.534265e+01  1.277794e+01  1.399768e+00  6.919669e+01  2.486470e+02 
##    Isoleucine 
##  6.665182e+01
```

```r
# # Make predictions on the month 2 data using the model of month 0
prediction_month_2 <- predict(model_miris_IU_0, merge_SCFA_MIRIS_IU_2_new)
model_stat_0_2 <- calculate_mse_and_rmse(merge_SCFA_MIRIS_IU_2_new,prediction_month_2)
print(" rmse values for month 2")
```

```
## [1] " rmse values for month 2"
```

```r
print(model_stat_0_2$rmse)
```

```
##     Propionic    Isobutyric       Butyric X2.Me.butyric    Isovaleric 
##  6.655140e+00  1.474621e+04  1.784576e+01  1.090497e+01  2.610673e-01 
##       Valeric       Caproic     Heptanoic        Valine       Leucine 
##  1.534265e+01  1.277794e+01  1.399768e+00  6.919669e+01  2.486470e+02 
##    Isoleucine 
##  6.665182e+01
```

```r
# # Make predictions on the month 3 data using the model of month 0
prediction_month_3 <- predict(model_miris_IU_0, merge_SCFA_MIRIS_IU_3_new)
model_stat_0_3 <- calculate_mse_and_rmse(merge_SCFA_MIRIS_IU_3_new,prediction_month_3)
print(" rmse values for month 3")
```

```
## [1] " rmse values for month 3"
```

```r
print(model_stat_0_3$rmse)
```

```
##     Propionic    Isobutyric       Butyric X2.Me.butyric    Isovaleric 
##  6.655140e+00  1.474621e+04  1.784576e+01  1.090497e+01  2.610673e-01 
##       Valeric       Caproic     Heptanoic        Valine       Leucine 
##  1.534265e+01  1.277794e+01  1.399768e+00  6.919669e+01  2.486470e+02 
##    Isoleucine 
##  6.665182e+01
```

#### Regression model for Month 1


```r
# Split the data into training (70%) and testing (30%) sets
train_index_1 <- sample(1:nrow(merge_SCFA_MIRIS_IU_1_new), 0.7 * nrow(merge_SCFA_MIRIS_IU_1_new))
train_data_1 <- merge_SCFA_MIRIS_IU_0_new[train_index_1, ]
test_data_1 <- merge_SCFA_MIRIS_IU_0_new[-train_index_1, ]

scfa_matrix_1 <- train_data_1 %>% 
  select(7:17)
scfa_matrix_1 <- as.matrix(scfa_matrix_1)

# Build the multiple linear regression model
model_miris_IU_1 <- lm(scfa_matrix_1 ~ Fat + Crude_Protein + True_Protein + Carbohydates , data = train_data_1)
summary(model_miris_IU_1)
```

```
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Fat + Crude_Protein + True_Protein + 
##     Carbohydates, data = train_data_1)
## 
## Residuals:
##      26       6      10      21      14      25 
## -7.2340 -1.1193  0.3877 -5.3613  0.9145 12.4124 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)    -13.368    116.606  -0.115    0.927
## Fat              3.272     26.628   0.123    0.922
## Crude_Protein   15.044     45.176   0.333    0.795
## True_Protein   -16.583     85.865  -0.193    0.879
## Carbohydates     0.387      3.698   0.105    0.934
## 
## Residual standard error: 15.41 on 1 degrees of freedom
## Multiple R-squared:  0.2951,	Adjusted R-squared:  -2.525 
## F-statistic: 0.1046 on 4 and 1 DF,  p-value: 0.9635
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Fat + Crude_Protein + True_Protein + 
##     Carbohydates, data = train_data_1)
## 
## Residuals:
##      26       6      10      21      14      25 
##  420.78   65.10  -22.55  311.84  -53.19 -721.98 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)  
## (Intercept)    67657.8     6782.5   9.975   0.0636 .
## Fat            37888.1     1548.8  24.462   0.0260 *
## Crude_Protein -17009.6     2627.7  -6.473   0.0976 .
## True_Protein  119330.0     4994.4  23.893   0.0266 *
## Carbohydates   -6236.6      215.1 -28.994   0.0219 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 896.2 on 1 degrees of freedom
## Multiple R-squared:  0.9994,	Adjusted R-squared:  0.9971 
## F-statistic:   432 on 4 and 1 DF,  p-value: 0.03607
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Fat + Crude_Protein + True_Protein + Carbohydates, 
##     data = train_data_1)
## 
## Residuals:
##      26       6      10      21      14      25 
##  3.9758  0.6152 -0.2131  2.9465 -0.5026 -6.8218 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)    -41.157     64.086  -0.642    0.637
## Fat            -56.605     14.635  -3.868    0.161
## Crude_Protein  -44.189     24.829  -1.780    0.326
## True_Protein    29.581     47.191   0.627    0.644
## Carbohydates     5.914      2.032   2.910    0.211
## 
## Residual standard error: 8.468 on 1 degrees of freedom
## Multiple R-squared:  0.9744,	Adjusted R-squared:  0.872 
## F-statistic: 9.514 on 4 and 1 DF,  p-value: 0.238
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Fat + Crude_Protein + True_Protein + 
##     Carbohydates, data = train_data_1)
## 
## Residuals:
##      26       6      10      21      14      25 
##  3.9659  0.6136 -0.2126  2.9392 -0.5013 -6.8049 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)    51.9647    63.9271   0.813    0.565
## Fat            -2.9215    14.5983  -0.200    0.874
## Crude_Protein -55.6576    24.7668  -2.247    0.267
## True_Protein  -10.3327    47.0738  -0.220    0.862
## Carbohydates    0.6715     2.0273   0.331    0.796
## 
## Residual standard error: 8.447 on 1 degrees of freedom
## Multiple R-squared:  0.9008,	Adjusted R-squared:  0.5041 
## F-statistic: 2.271 on 4 and 1 DF,  p-value: 0.4568
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Fat + Crude_Protein + True_Protein + 
##     Carbohydates, data = train_data_1)
## 
## Residuals:
##        26         6        10        21        14        25 
##  0.037477  0.005799 -0.002009  0.027775 -0.004738 -0.064304 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -0.663816   0.604094  -1.099    0.470
## Fat            0.003231   0.137950   0.023    0.985
## Crude_Protein  0.274999   0.234040   1.175    0.449
## True_Protein   0.013328   0.444834   0.030    0.981
## Carbohydates   0.015202   0.019158   0.794    0.573
## 
## Residual standard error: 0.07982 on 1 degrees of freedom
## Multiple R-squared:  0.7355,	Adjusted R-squared:  -0.3227 
## F-statistic: 0.695 on 4 and 1 DF,  p-value: 0.7035
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Fat + Crude_Protein + True_Protein + Carbohydates, 
##     data = train_data_1)
## 
## Residuals:
##       26        6       10       21       14       25 
## -0.44991 -0.06961  0.02411 -0.33344  0.05687  0.77197 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)  
## (Intercept)    76.2354     7.2522  10.512   0.0604 .
## Fat           -39.4872     1.6561 -23.844   0.0267 *
## Crude_Protein  22.7394     2.8097   8.093   0.0783 .
## True_Protein  -73.8009     5.3403 -13.820   0.0460 *
## Carbohydates    0.7036     0.2300   3.059   0.2011  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9582 on 1 degrees of freedom
## Multiple R-squared:  0.9989,	Adjusted R-squared:  0.9943 
## F-statistic: 220.3 on 4 and 1 DF,  p-value: 0.05048
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Fat + Crude_Protein + True_Protein + Carbohydates, 
##     data = train_data_1)
## 
## Residuals:
##      26       6      10      21      14      25 
##  0.8471  0.1311 -0.0454  0.6278 -0.1071 -1.4534 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -25.1608    13.6538  -1.843    0.317
## Fat             0.2862     3.1179   0.092    0.942
## Crude_Protein -23.1976     5.2898  -4.385    0.143
## True_Protein   25.4574    10.0542   2.532    0.239
## Carbohydates    1.0578     0.4330   2.443    0.247
## 
## Residual standard error: 1.804 on 1 degrees of freedom
## Multiple R-squared:  0.968,	Adjusted R-squared:  0.8398 
## F-statistic: 7.551 on 4 and 1 DF,  p-value: 0.2657
## 
## 
## Response Heptanoic :
## 
## Call:
## lm(formula = Heptanoic ~ Fat + Crude_Protein + True_Protein + 
##     Carbohydates, data = train_data_1)
## 
## Residuals:
##       26        6       10       21       14       25 
##  0.33404  0.05168 -0.01790  0.24756 -0.04223 -0.57316 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)    11.6636     5.3844   2.166    0.275
## Fat             3.0036     1.2296   2.443    0.247
## Crude_Protein  -2.2335     2.0861  -1.071    0.478
## True_Protein    5.9233     3.9649   1.494    0.376
## Carbohydates   -0.5882     0.1708  -3.445    0.180
## 
## Residual standard error: 0.7114 on 1 degrees of freedom
## Multiple R-squared:  0.9351,	Adjusted R-squared:  0.6754 
## F-statistic: 3.601 on 4 and 1 DF,  p-value: 0.3739
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Fat + Crude_Protein + True_Protein + Carbohydates, 
##     data = train_data_1)
## 
## Residuals:
##      26       6      10      21      14      25 
##  52.607   8.140  -2.819  38.988  -6.650 -90.266 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)    839.759    847.986   0.990    0.503
## Fat           -171.742    193.644  -0.887    0.538
## Crude_Protein  212.800    328.529   0.648    0.634
## True_Protein  -871.327    624.428  -1.395    0.396
## Carbohydates    -2.893     26.892  -0.108    0.932
## 
## Residual standard error: 112 on 1 degrees of freedom
## Multiple R-squared:  0.6668,	Adjusted R-squared:  -0.6658 
## F-statistic: 0.5004 on 4 and 1 DF,  p-value: 0.7696
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Fat + Crude_Protein + True_Protein + Carbohydates, 
##     data = train_data_1)
## 
## Residuals:
##      26       6      10      21      14      25 
##  44.176   6.835  -2.368  32.740  -5.584 -75.799 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)    2415.82     712.08   3.393    0.182
## Fat             730.85     162.61   4.495    0.139
## Crude_Protein  -774.14     275.88  -2.806    0.218
## True_Protein   1739.21     524.35   3.317    0.186
## Carbohydates   -129.59      22.58  -5.739    0.110
## 
## Residual standard error: 94.09 on 1 degrees of freedom
## Multiple R-squared:  0.9779,	Adjusted R-squared:  0.8897 
## F-statistic: 11.08 on 4 and 1 DF,  p-value: 0.2212
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Fat + Crude_Protein + True_Protein + 
##     Carbohydates, data = train_data_1)
## 
## Residuals:
##      26       6      10      21      14      25 
##  21.345   3.303  -1.144  15.819  -2.698 -36.625 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)    1080.30     344.06   3.140    0.196
## Fat             -48.86      78.57  -0.622    0.646
## Crude_Protein   -33.95     133.30  -0.255    0.841
## True_Protein   -636.39     253.36  -2.512    0.241
## Carbohydates    -16.54      10.91  -1.516    0.371
## 
## Residual standard error: 45.46 on 1 degrees of freedom
## Multiple R-squared:  0.9223,	Adjusted R-squared:  0.6115 
## F-statistic: 2.967 on 4 and 1 DF,  p-value: 0.4073
```

```r
# Make predictions on the test data using the model
predictions_1 <- predict(model_miris_IU_1, newdata = test_data_1)
model_stat_1 <- calculate_mse_and_rmse(test_data_1,predictions_1)
print(" rmse values for month 1")
```

```
## [1] " rmse values for month 1"
```

```r
print(model_stat_1$rmse)
```

```
##     Propionic    Isobutyric       Butyric X2.Me.butyric    Isovaleric 
##  6.617248e+00  2.609286e+04  3.363918e+01  2.122848e+01  5.029077e-01 
##       Valeric       Caproic     Heptanoic        Valine       Leucine 
##  3.251290e+01  1.963195e+01  2.913191e+00  1.174762e+02  5.576034e+02 
##    Isoleucine 
##  1.408172e+02
```

```r
# Make predictions on the month 2 data using the model of month 1
prediction_month_1_2 <- predict(model_miris_IU_1, merge_SCFA_MIRIS_IU_2_new)
# Calculate the Mean Squared Error (MSE) for each SCFA
model_stat_1_2 <- calculate_mse_and_rmse(merge_SCFA_MIRIS_IU_2_new,prediction_month_1_2)
print(" rmse values for month 2")
```

```
## [1] " rmse values for month 2"
```

```r
print(model_stat_1_2$rmse)
```

```
##     Propionic    Isobutyric       Butyric X2.Me.butyric    Isovaleric 
##  6.400909e+00  1.506768e+04  1.962562e+01  1.257551e+01  2.915704e-01 
##       Valeric       Caproic     Heptanoic        Valine       Leucine 
##  1.877405e+01  1.135045e+01  1.698568e+00  7.742807e+01  3.234565e+02 
##    Isoleucine 
##  8.270106e+01
```

```r
# Make predictions on the month 3 data using the model of month 1
prediction_month_1_3 <- predict(model_miris_IU_1, merge_SCFA_MIRIS_IU_3_new)
# Calculate the Mean Squared Error (MSE) for each SCFA
model_stat_1_3 <- calculate_mse_and_rmse(merge_SCFA_MIRIS_IU_3_new,prediction_month_1_3)
print(" rmse values for month 3")
```

```
## [1] " rmse values for month 3"
```

```r
print(model_stat_1_3$rmse)
```

```
##     Propionic    Isobutyric       Butyric X2.Me.butyric    Isovaleric 
##  6.400909e+00  1.506768e+04  1.962562e+01  1.257551e+01  2.915704e-01 
##       Valeric       Caproic     Heptanoic        Valine       Leucine 
##  1.877405e+01  1.135045e+01  1.698568e+00  7.742807e+01  3.234565e+02 
##    Isoleucine 
##  8.270106e+01
```

#### Regression model for Month 2


```r
# Split the data into training (70%) and testing (30%) sets
train_index_2 <- sample(1:nrow(merge_SCFA_MIRIS_IU_2_new), 0.7 * nrow(merge_SCFA_MIRIS_IU_2_new))
train_data_2 <- merge_SCFA_MIRIS_IU_0_new[train_index_2, ]
test_data_2 <- merge_SCFA_MIRIS_IU_0_new[-train_index_2, ]

scfa_matrix_2 <- train_data_2 %>% 
  select(7:17)

scfa_matrix_2 <- as.matrix(scfa_matrix_2)

# Build the multiple linear regression model
model_miris_IU_2 <- lm(scfa_matrix_2 ~ Fat + Crude_Protein + True_Protein + Carbohydates , data = train_data_1)
summary(model_miris_IU_2)
```

```
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Fat + Crude_Protein + True_Protein + 
##     Carbohydates, data = train_data_1)
## 
## Residuals:
##        10        17        24        13        14        26 
## -0.106272 -0.016443  0.005696 -0.078760  0.013434  0.182346 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)  
## (Intercept)     9.68488    1.71302   5.654   0.1114  
## Fat             4.25201    0.39118  10.870   0.0584 .
## Crude_Protein -12.10460    0.66366 -18.239   0.0349 *
## True_Protein   10.01612    1.26141   7.940   0.0798 .
## Carbohydates   -0.33950    0.05433  -6.249   0.1010  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2263 on 1 degrees of freedom
## Multiple R-squared:  0.9974,	Adjusted R-squared:  0.9872 
## F-statistic:  97.7 on 4 and 1 DF,  p-value: 0.07572
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Fat + Crude_Protein + True_Protein + 
##     Carbohydates, data = train_data_1)
## 
## Residuals:
##       10       17       24       13       14       26 
##  10968.5   1697.1   -587.8   8128.9  -1386.5 -18820.1 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)     -82236     176802  -0.465    0.723
## Fat               8094      40374   0.200    0.874
## Crude_Protein    51854      68497   0.757    0.587
## True_Protein    -13830     130191  -0.106    0.933
## Carbohydates      1649       5607   0.294    0.818
## 
## Residual standard error: 23360 on 1 degrees of freedom
## Multiple R-squared:  0.5572,	Adjusted R-squared:  -1.214 
## F-statistic: 0.3146 on 4 and 1 DF,  p-value: 0.8508
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Fat + Crude_Protein + True_Protein + Carbohydates, 
##     data = train_data_1)
## 
## Residuals:
##       10       17       24       13       14       26 
## -0.28661 -0.04435  0.01536 -0.21241  0.03623  0.49177 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)  
## (Intercept)    22.2006     4.6199   4.805   0.1306  
## Fat           -21.8268     1.0550 -20.689   0.0307 *
## Crude_Protein -13.0057     1.7898  -7.266   0.0871 .
## True_Protein   99.1650     3.4019  29.150   0.0218 *
## Carbohydates   -0.2802     0.1465  -1.913   0.3067  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.6104 on 1 degrees of freedom
## Multiple R-squared:  0.9999,	Adjusted R-squared:  0.9993 
## F-statistic:  1797 on 4 and 1 DF,  p-value: 0.01769
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Fat + Crude_Protein + True_Protein + 
##     Carbohydates, data = train_data_1)
## 
## Residuals:
##        10        17        24        13        14        26 
## -0.115341 -0.017846  0.006182 -0.085481  0.014580  0.197906 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)  
## (Intercept)    71.03278    1.85920   38.21   0.0167 *
## Fat             6.02355    0.42456   14.19   0.0448 *
## Crude_Protein -11.35097    0.72030  -15.76   0.0403 *
## True_Protein   19.35672    1.36905   14.14   0.0450 *
## Carbohydates   -2.60482    0.05896  -44.18   0.0144 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2457 on 1 degrees of freedom
## Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9985 
## F-statistic: 861.4 on 4 and 1 DF,  p-value: 0.02555
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Fat + Crude_Protein + True_Protein + 
##     Carbohydates, data = train_data_1)
## 
## Residuals:
##        10        17        24        13        14        26 
## -0.070509 -0.010909  0.003779 -0.052255  0.008913  0.120981 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)    1.60740    1.13654   1.414    0.392
## Fat            0.88385    0.25954   3.405    0.182
## Crude_Protein -0.33068    0.44032  -0.751    0.590
## True_Protein   2.55854    0.83691   3.057    0.201
## Carbohydates  -0.14306    0.03604  -3.969    0.157
## 
## Residual standard error: 0.1502 on 1 degrees of freedom
## Multiple R-squared:  0.9676,	Adjusted R-squared:  0.8382 
## F-statistic: 7.475 on 4 and 1 DF,  p-value: 0.2669
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Fat + Crude_Protein + True_Protein + Carbohydates, 
##     data = train_data_1)
## 
## Residuals:
##       10       17       24       13       14       26 
##  0.45687  0.07069 -0.02449  0.33859 -0.05775 -0.78391 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)  
## (Intercept)    -6.6345     7.3643  -0.901   0.5332  
## Fat           -48.7930     1.6817 -29.014   0.0219 *
## Crude_Protein  44.3631     2.8531  15.549   0.0409 *
## True_Protein  -81.3903     5.4228 -15.009   0.0424 *
## Carbohydates    3.4807     0.2335  14.904   0.0427 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.973 on 1 degrees of freedom
## Multiple R-squared:  0.999,	Adjusted R-squared:  0.9949 
## F-statistic: 247.1 on 4 and 1 DF,  p-value: 0.04767
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Fat + Crude_Protein + True_Protein + Carbohydates, 
##     data = train_data_1)
## 
## Residuals:
##      10      17      24      13      14      26 
##  1.9886  0.3077 -0.1066  1.4738 -0.2514 -3.4121 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)     92.479     32.054   2.885    0.212
## Fat             28.077      7.320   3.836    0.162
## Crude_Protein  -41.733     12.418  -3.361    0.184
## True_Protein    75.738     23.604   3.209    0.192
## Carbohydates    -4.750      1.017  -4.673    0.134
## 
## Residual standard error: 4.235 on 1 degrees of freedom
## Multiple R-squared:  0.9712,	Adjusted R-squared:  0.8561 
## F-statistic: 8.438 on 4 and 1 DF,  p-value: 0.252
## 
## 
## Response Heptanoic :
## 
## Call:
## lm(formula = Heptanoic ~ Fat + Crude_Protein + True_Protein + 
##     Carbohydates, data = train_data_1)
## 
## Residuals:
##       10       17       24       13       14       26 
##  0.75602  0.11697 -0.04052  0.56030 -0.09557 -1.29720 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -3.66777   12.18633  -0.301    0.814
## Fat            1.37967    2.78285   0.496    0.707
## Crude_Protein  4.15966    4.72127   0.881    0.540
## True_Protein   0.73965    8.97361   0.082    0.948
## Carbohydates  -0.04283    0.38647  -0.111    0.930
## 
## Residual standard error: 1.61 on 1 degrees of freedom
## Multiple R-squared:  0.6271,	Adjusted R-squared:  -0.8646 
## F-statistic: 0.4204 on 4 and 1 DF,  p-value: 0.8021
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Fat + Crude_Protein + True_Protein + Carbohydates, 
##     data = train_data_1)
## 
## Residuals:
##      10      17      24      13      14      26 
## -30.232  -4.678   1.620 -22.405   3.822  51.873 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)    559.212    487.313   1.148    0.456
## Fat           -174.854    111.282  -1.571    0.361
## Crude_Protein  148.249    188.796   0.785    0.576
## True_Protein  -654.392    358.841  -1.824    0.319
## Carbohydates     2.922     15.454   0.189    0.881
## 
## Residual standard error: 64.39 on 1 degrees of freedom
## Multiple R-squared:  0.7897,	Adjusted R-squared:  -0.05132 
## F-statistic: 0.939 on 4 and 1 DF,  p-value: 0.6396
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Fat + Crude_Protein + True_Protein + Carbohydates, 
##     data = train_data_1)
## 
## Residuals:
##      10      17      24      13      14      26 
##  228.73   35.39  -12.26  169.51  -28.91 -392.46 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -678.352   3686.908  -0.184    0.884
## Fat            348.531    841.935   0.414    0.750
## Crude_Protein  479.581   1428.393   0.336    0.794
## True_Protein   289.986   2714.917   0.107    0.932
## Carbohydates    -4.736    116.924  -0.041    0.974
## 
## Residual standard error: 487.2 on 1 degrees of freedom
## Multiple R-squared:  0.363,	Adjusted R-squared:  -2.185 
## F-statistic: 0.1425 on 4 and 1 DF,  p-value: 0.943
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Fat + Crude_Protein + True_Protein + 
##     Carbohydates, data = train_data_1)
## 
## Residuals:
##      10      17      24      13      14      26 
##  4.6229  0.7153 -0.2478  3.4261 -0.5844 -7.9321 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)  
## (Intercept)    529.995     74.517   7.112   0.0889 .
## Fat             39.222     17.017   2.305   0.2606  
## Crude_Protein   -3.499     28.870  -0.121   0.9232  
## True_Protein    89.402     54.872   1.629   0.3504  
## Carbohydates   -20.550      2.363  -8.696   0.0729 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.846 on 1 degrees of freedom
## Multiple R-squared:  0.9926,	Adjusted R-squared:  0.9631 
## F-statistic: 33.62 on 4 and 1 DF,  p-value: 0.1286
```

```r
# Make predictions on the test data using the model
predictions_2 <- predict(model_miris_IU_2, newdata = test_data_2)
# Calculate the Mean Squared Error (MSE) for each SCFA
model_stat_2 <- calculate_mse_and_rmse(test_data_2,predictions_2)
print(" rmse values for month 2")
```

```
## [1] " rmse values for month 2"
```

```r
print(model_stat_2$rmse)
```

```
##     Propionic    Isobutyric       Butyric X2.Me.butyric    Isovaleric 
##  1.121445e+01  1.264486e+04  1.051715e+01  1.666838e+01  5.564101e-02 
##       Valeric       Caproic     Heptanoic        Valine       Leucine 
##  4.727163e+00  6.900728e+00  1.148539e+00  6.404759e+01  2.254401e+02 
##    Isoleucine 
##  8.647340e+01
```

```r
# Make predictions on the month 3 using the model for month 2
prediction_month_2_3 <- predict(model_miris_IU_1, merge_SCFA_MIRIS_IU_3_new)
# Calculate the Mean Squared Error (MSE) for each SCFA
model_stat_2_3 <- calculate_mse_and_rmse(merge_SCFA_MIRIS_IU_2_new,prediction_month_2_3)
print(" rmse values for month 3")
```

```
## [1] " rmse values for month 3"
```

```r
print(model_stat_2_3$rmse) 
```

```
##     Propionic    Isobutyric       Butyric X2.Me.butyric    Isovaleric 
##  6.400909e+00  1.506768e+04  1.962562e+01  1.257551e+01  2.915704e-01 
##       Valeric       Caproic     Heptanoic        Valine       Leucine 
##  1.877405e+01  1.135045e+01  1.698568e+00  7.742807e+01  3.234565e+02 
##    Isoleucine 
##  8.270106e+01
```

#### Regression model for Month 3



## MILK (FAMES)

regression using milk fatty acids to predict Short chain fatty acids

### Feature selection using carat as there to select fatty acids

##### Using correlation method


```r
# month 0 (RBW and CEN)
Corr_SCFA_FAMES_IU_0 <- cor(merge_SCFA_FAMES_IU_0[3:34])
highlycorrelareated_0 <- findCorrelation(Corr_SCFA_FAMES_IU_0, cutoff = 0.75)
# Get the names of highly correlated variables for month 0
highlycorrelated_names_0 <- colnames(merge_SCFA_FAMES_IU_0[3:34][, highlycorrelareated_0])
print("Month 0")
```

```
## [1] "Month 0"
```

```r
print(highlycorrelated_names_0)
```

```
## [1] "cis.5.8.11.14.17.Eicosapentaenoic" "Stearate"                         
## [3] "Hexanoate"                         "Pentadecanoate"                   
## [5] "Butyric"                           "Myristoleate"                     
## [7] "Myristate"                         "cis.11.14.Eicosadienoic"
```

```r
# month 1
Corr_SCFA_FAMES_IU_1 <- cor(merge_SCFA_FAMES_IU_1[3:42])
highlycorrelareated_1 <- findCorrelation(Corr_SCFA_FAMES_IU_1, cutoff = 0.75)
highlycorrelated_names_1 <- colnames(merge_SCFA_FAMES_IU_1[3:42][, highlycorrelareated_1])
print("Month 1")
```

```
## [1] "Month 1"
```

```r
print(highlycorrelated_names_1)
```

```
##  [1] "Octanoate"               "Isobutyric"             
##  [3] "Laurate"                 "Oleic"                  
##  [5] "Arachidonic"             "Propionic"              
##  [7] "Eicosanoic"              "Hexanoate"              
##  [9] "Myristoleate"            "cis.11.Eicosenoic"      
## [11] "Elaidic"                 "Stearate"               
## [13] "Valine"                  "Nervonic.acid"          
## [15] "cis.13.16.Docosadienoic" "Heptadecanoate"         
## [17] "Heptanoic"               "Valeric"                
## [19] "Myristate"               "Palmitate"              
## [21] "gamma.Linolenic"         "Linolenic"
```

```r
# month 2
Corr_SCFA_FAMES_IU_2 <- cor(merge_SCFA_FAMES_IU_2[3:42])
highlycorrelareated_2 <- findCorrelation(Corr_SCFA_FAMES_IU_2, cutoff = 0.75)
highlycorrelated_names_2 <- colnames(merge_SCFA_FAMES_IU_2[3:42][, highlycorrelareated_2])
print("Month 2")
```

```
## [1] "Month 2"
```

```r
print(highlycorrelated_names_2)
```

```
##  [1] "Decanoate"                           "Heptadecanoate"                     
##  [3] "Myristate"                           "Stearate"                           
##  [5] "Erucic.acid"                         "cis.8.11.14.Eicosatrienoic"         
##  [7] "cis.11.Eicosenoic"                   "X2.Me.butyric"                      
##  [9] "Isovaleric"                          "Nervonic.acid"                      
## [11] "Elaidic"                             "Propionic"                          
## [13] "Linolenic"                           "Laurate"                            
## [15] "Leucine"                             "Caproic"                            
## [17] "cis.13.16.Docosadienoic"             "cis.10.Heptadecenoic"               
## [19] "Lignocerate"                         "Isobutyric"                         
## [21] "Arachidonic"                         "Linolelaidic"                       
## [23] "Valine"                              "Hexanoate"                          
## [25] "Valeric"                             "Butyric"                            
## [27] "Palmitate"                           "Palmitoleate"                       
## [29] "cis.11.14.Eicosadienoic"             "Heptanoic"                          
## [31] "Myristoleate"                        "cis.4.7.10.13.16.19.Docosahexaenoic"
## [33] "Linoleic"                            "cis.5.8.11.14.17.Eicosapentaenoic"  
## [35] "Octanoate"
```

```r
# month 3
Corr_SCFA_FAMES_IU_3 <- cor(merge_SCFA_FAMES_IU_3[3:42])
highlycorrelareated_3 <- findCorrelation(Corr_SCFA_FAMES_IU_3, cutoff = 0.75)
highlycorrelated_names_3 <- colnames(merge_SCFA_FAMES_IU_3[3:42][, highlycorrelareated_3])
print("Month 3")
```

```
## [1] "Month 3"
```

```r
print(highlycorrelated_names_3)
```

```
##  [1] "Decanoate"                         "Isobutyric"                       
##  [3] "Isoleucine"                        "cis.11.14.Eicosadienoic"          
##  [5] "Octanoate"                         "cis.13.16.Docosadienoic"          
##  [7] "Hexanoate"                         "X2.Me.butyric"                    
##  [9] "Lignocerate"                       "Leucine"                          
## [11] "Eicosanoic"                        "cis.5.8.11.14.17.Eicosapentaenoic"
## [13] "Valeric"                           "Arachidonic"                      
## [15] "Myristoleate"                      "Stearate"                         
## [17] "Isovaleric"                        "Laurate"                          
## [19] "gamma.Linolenic"                   "Elaidic"                          
## [21] "Pentadecanoate"                    "Linolenic"                        
## [23] "cis.11.Eicosenoic"                 "cis.10.Heptadecenoic"             
## [25] "Myristate"                         "Valine"                           
## [27] "Propionic"                         "Heptanoic"                        
## [29] "Heptadecanoate"                    "Linolelaidic"                     
## [31] "Oleic"                             "Erucic.acid"                      
## [33] "Caproic"
```

#### Using intrinsic method (glmnet) for feature selection

###### Feature Selection Method for month 0


```r
set.seed(12345)

# Remove the fatty acids which show high correlation
merge_SCFA_FAMES_IU_0 <- merge_SCFA_FAMES_IU_0 %>% select(-cis.5.8.11.14.17.Eicosapentaenoic,-Stearate,-Hexanoate, -cis.10.Heptadecenoic,-Myristate,-Heptanoic,-Myristoleate,-cis.11.14.Eicosadienoic,-ID,-Month,-Type)


# Feature selection using glmnet method 

SCFA_name <- colnames(merge_SCFA_FAMES_IU_0[23:32])
# Loop through each SCFA
for (scfa in SCFA_name) {
  glmnet <- train(as.formula(paste(scfa, "~ .")), 
                  data = merge_SCFA_FAMES_IU_0, 
                  method = 'glmnet', 
                  metric = "MAE")
  
  # Compute variable importance
  impVar <- varImp(glmnet)
  
  # Create variable importance plot using ggplot
 # Create variable importance plot using ggplot
  vi_plot <- ggplot(impVar, aes(x = variable, y = Overall, label = scfa)) +
    geom_bar(stat = "identity", fill = "#53cfff", width = 0.50 ) +
    theme_light(base_size = 20) +
    theme(axis.title.x = element_text(size = 15, colour = "black"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 15, colour = "black"),
          axis.text.y = element_text(size = 15, colour = "black")) +
    ggtitle(paste("Variable importance for", scfa))
  
  
  # Print the variable importance plot
  print(vi_plot)
}
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-18-1.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-18-2.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-18-3.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-18-4.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-18-5.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-18-6.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-18-7.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-18-8.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-18-9.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-18-10.png)<!-- -->

##### Regression for Month 0


```r
set.seed(123)  # For reproducibility
FAMES_index_0 <- sample(1:nrow(merge_SCFA_FAMES_IU_0), 0.7 * nrow(merge_SCFA_FAMES_IU_0))
FAMES_train_data_0 <- merge_SCFA_FAMES_IU_0[FAMES_index_0, ]
FAMES_test_data_0 <- merge_SCFA_FAMES_IU_0[-FAMES_index_0, ]


scfa_matrix_0 <- merge_SCFA_FAMES_IU_0 %>% 
  select(23:32)
scfa_matrix_0 <- as.matrix(scfa_matrix_0)


Propionic_model <- lm(Propionic ~ gamma.Linolenic + Nervonic.acid + Octanoate + Lignocerate ,data = merge_SCFA_FAMES_IU_0 )
summary(Propionic_model)
```

```
## 
## Call:
## lm(formula = Propionic ~ gamma.Linolenic + Nervonic.acid + Octanoate + 
##     Lignocerate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##        6       10       14       17       21       22       23       24 
## -0.46804  2.16992  0.08370 -0.08973 -0.09569 -1.53451  0.68566 -0.75130 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)  
## (Intercept)       39.0056     7.2837   5.355   0.0127 *
## gamma.Linolenic   16.5409     9.1533   1.807   0.1685  
## Nervonic.acid   -137.0017    36.5151  -3.752   0.0331 *
## Octanoate          3.9696     0.8358   4.749   0.0177 *
## Lignocerate        4.1287     2.6389   1.565   0.2156  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.667 on 3 degrees of freedom
## Multiple R-squared:  0.9743,	Adjusted R-squared:  0.9399 
## F-statistic: 28.39 on 4 and 3 DF,  p-value: 0.01016
```

```r
plot(Propionic_model)
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-19-1.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-19-2.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-19-3.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-19-4.png)<!-- -->

```r
Butyric_model <- lm(Butyric ~ Arachidonic + Eicosanoic +  cis.11.Eicosenoic + Erucic.acid +  Palmitate + Palmitoleate + Octanoate + Linoleic + Heptadecanoate ,data = merge_SCFA_FAMES_IU_0)
summary(Butyric_model)
```

```
## 
## Call:
## lm(formula = Butyric ~ Arachidonic + Eicosanoic + cis.11.Eicosenoic + 
##     Erucic.acid + Palmitate + Palmitoleate + Octanoate + Linoleic + 
##     Heptadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
## ALL 8 residuals are 0: no residual degrees of freedom!
## 
## Coefficients: (2 not defined because of singularities)
##                   Estimate Std. Error t value Pr(>|t|)
## (Intercept)         71.762        NaN     NaN      NaN
## Arachidonic        233.021        NaN     NaN      NaN
## Eicosanoic          46.597        NaN     NaN      NaN
## cis.11.Eicosenoic  -57.890        NaN     NaN      NaN
## Erucic.acid         13.252        NaN     NaN      NaN
## Palmitate            5.050        NaN     NaN      NaN
## Palmitoleate       -16.296        NaN     NaN      NaN
## Octanoate            8.173        NaN     NaN      NaN
## Linoleic                NA         NA      NA       NA
## Heptadecanoate          NA         NA      NA       NA
## 
## Residual standard error: NaN on 0 degrees of freedom
## Multiple R-squared:      1,	Adjusted R-squared:    NaN 
## F-statistic:   NaN on 7 and 0 DF,  p-value: NA
```

```r
X2_Me_butyric_model <- lm(X2.Me.butyric ~ Arachidonic + Nervonic.acid +  cis.11.Eicosenoic + Erucic.acid +  Palmitate + Palmitoleate + Linoleic + cis.8.11.14.Eicosatrienoic + cis.4.7.10.13.16.19.Docosahexaenoic + Pentadecanoate+Decanoate + Laurate ,data = merge_SCFA_FAMES_IU_0)
summary(X2_Me_butyric_model)
```

```
## 
## Call:
## lm(formula = X2.Me.butyric ~ Arachidonic + Nervonic.acid + cis.11.Eicosenoic + 
##     Erucic.acid + Palmitate + Palmitoleate + Linoleic + cis.8.11.14.Eicosatrienoic + 
##     cis.4.7.10.13.16.19.Docosahexaenoic + Pentadecanoate + Decanoate + 
##     Laurate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
## ALL 8 residuals are 0: no residual degrees of freedom!
## 
## Coefficients: (5 not defined because of singularities)
##                                     Estimate Std. Error t value Pr(>|t|)
## (Intercept)                          -260.11        NaN     NaN      NaN
## Arachidonic                          -105.53        NaN     NaN      NaN
## Nervonic.acid                        1346.71        NaN     NaN      NaN
## cis.11.Eicosenoic                     158.18        NaN     NaN      NaN
## Erucic.acid                           -18.74        NaN     NaN      NaN
## Palmitate                              11.40        NaN     NaN      NaN
## Palmitoleate                           31.14        NaN     NaN      NaN
## Linoleic                               57.30        NaN     NaN      NaN
## cis.8.11.14.Eicosatrienoic                NA         NA      NA       NA
## cis.4.7.10.13.16.19.Docosahexaenoic       NA         NA      NA       NA
## Pentadecanoate                            NA         NA      NA       NA
## Decanoate                                 NA         NA      NA       NA
## Laurate                                   NA         NA      NA       NA
## 
## Residual standard error: NaN on 0 degrees of freedom
## Multiple R-squared:      1,	Adjusted R-squared:    NaN 
## F-statistic:   NaN on 7 and 0 DF,  p-value: NA
```

```r
Isovaleric_model <- lm(Isovaleric ~ gamma.Linolenic + Nervonic.acid + Octanoate + Arachidonic + cis.11.Eicosenoic + Linoleic ,data = merge_SCFA_FAMES_IU_0)
summary(Isovaleric_model)
```

```
## 
## Call:
## lm(formula = Isovaleric ~ gamma.Linolenic + Nervonic.acid + Octanoate + 
##     Arachidonic + cis.11.Eicosenoic + Linoleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##        6       10       14       17       21       22       23       24 
##  0.06943 -0.15795 -0.11291  0.03207  0.04703  0.04714 -0.01254  0.08774 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)
## (Intercept)         6.8266     5.5978   1.220    0.437
## gamma.Linolenic     1.7567     2.1189   0.829    0.559
## Nervonic.acid     -23.5197    10.7692  -2.184    0.273
## Octanoate          -0.4339     0.2555  -1.698    0.339
## Arachidonic        -3.3299     9.4516  -0.352    0.784
## cis.11.Eicosenoic  -3.0261     1.4303  -2.116    0.281
## Linoleic           -1.4421     1.0033  -1.437    0.387
## 
## Residual standard error: 0.2363 on 1 degrees of freedom
## Multiple R-squared:  0.9256,	Adjusted R-squared:  0.4795 
## F-statistic: 2.075 on 6 and 1 DF,  p-value: 0.4865
```

```r
Valeric_model <- lm(Valeric ~  Elaidic + Palmitoleate+ Linoleic + Eicosanoic + Linolelaidic + Linolenic + Oleic + Laurate, data = merge_SCFA_FAMES_IU_0)
summary(Valeric_model)
```

```
## 
## Call:
## lm(formula = Valeric ~ Elaidic + Palmitoleate + Linoleic + Eicosanoic + 
##     Linolelaidic + Linolenic + Oleic + Laurate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
## ALL 8 residuals are 0: no residual degrees of freedom!
## 
## Coefficients: (1 not defined because of singularities)
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)  -12.7555        NaN     NaN      NaN
## Elaidic      -11.8987        NaN     NaN      NaN
## Palmitoleate  -5.8191        NaN     NaN      NaN
## Linoleic       8.4879        NaN     NaN      NaN
## Eicosanoic    10.2400        NaN     NaN      NaN
## Linolelaidic  -6.2438        NaN     NaN      NaN
## Linolenic     15.0296        NaN     NaN      NaN
## Oleic         -0.1545        NaN     NaN      NaN
## Laurate            NA         NA      NA       NA
## 
## Residual standard error: NaN on 0 degrees of freedom
## Multiple R-squared:      1,	Adjusted R-squared:    NaN 
## F-statistic:   NaN on 7 and 0 DF,  p-value: NA
```

```r
Caporic_model <- lm(Caproic ~ Nervonic.acid + Lignocerate + Arachidonic + cis.11.Eicosenoic + Linoleic + Eicosanoic + Octanoate + Octanoate , data = merge_SCFA_FAMES_IU_0)
summary(Caporic_model)
```

```
## 
## Call:
## lm(formula = Caproic ~ Nervonic.acid + Lignocerate + Arachidonic + 
##     cis.11.Eicosenoic + Linoleic + Eicosanoic + Octanoate + Octanoate, 
##     data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
## ALL 8 residuals are 0: no residual degrees of freedom!
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)
## (Intercept)         245.698        NaN     NaN      NaN
## Nervonic.acid     -1189.450        NaN     NaN      NaN
## Lignocerate           9.653        NaN     NaN      NaN
## Arachidonic          72.040        NaN     NaN      NaN
## cis.11.Eicosenoic  -136.393        NaN     NaN      NaN
## Linoleic            -86.036        NaN     NaN      NaN
## Eicosanoic          -14.960        NaN     NaN      NaN
## Octanoate           -18.176        NaN     NaN      NaN
## 
## Residual standard error: NaN on 0 degrees of freedom
## Multiple R-squared:      1,	Adjusted R-squared:    NaN 
## F-statistic:   NaN on 7 and 0 DF,  p-value: NA
```

```r
Valine_model <- lm(Caproic ~ gamma.Linolenic + Arachidonic + Nervonic.acid +  cis.11.Eicosenoic + Linoleic + Eicosanoic ,data = merge_SCFA_FAMES_IU_0)
summary(Valine_model)
```

```
## 
## Call:
## lm(formula = Caproic ~ gamma.Linolenic + Arachidonic + Nervonic.acid + 
##     cis.11.Eicosenoic + Linoleic + Eicosanoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##       6      10      14      17      21      22      23      24 
##  1.6334 -3.9037 -2.8664  0.8891  0.4884  1.5927 -0.7424  2.9090 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)
## (Intercept)          12.15      67.17   0.181    0.886
## gamma.Linolenic     -51.80      32.48  -1.595    0.357
## Arachidonic         309.75     158.58   1.953    0.301
## Nervonic.acid      -518.27     193.53  -2.678    0.228
## cis.11.Eicosenoic   -49.54      24.58  -2.015    0.293
## Linoleic            -18.31      12.04  -1.521    0.370
## Eicosanoic           16.11      13.27   1.214    0.439
## 
## Residual standard error: 6.221 on 1 degrees of freedom
## Multiple R-squared:  0.9463,	Adjusted R-squared:  0.624 
## F-statistic: 2.936 on 6 and 1 DF,  p-value: 0.4192
```

###### Feature Selection Method for month 1


```r
# Remove the fatty acids which show high correlation
merge_SCFA_FAMES_IU_1 <- merge_SCFA_FAMES_IU_1 %>% select(-ID,-Month,-Type,-Octanoate, -Laurate, -Arachidonic, -Eicosanoic, -Myristoleate, -Myristate, -Elaidic, cis.13.16.Docosadienoic, -Heptanoic, -gamma.Linolenic, -Oleic, -Hexanoate, -cis.11.Eicosenoic, -Stearate, -Nervonic.acid, -Heptadecanoate, -Palmitate, -Linolenic )


# Feature selection using glmnet method 

# Loop through each SCFA
for (scfa in SCFA_name) {
  formula <- as.formula(paste(scfa, "~ Decanoate+Pentadecanoate+Palmitoleate+cis.10.Heptadecenoic + Linoleic+Linolelaidic+cis.11.14.Eicosadienoic+cis.8.11.14.Eicosatrienoic+cis.5.8.11.14.17.Eicosapentaenoic+Erucic.acid+cis.13.16.Docosadienoic+cis.4.7.10.13.16.19.Docosahexaenoic+Lignocerate"))
  glmnet <- train(formula, 
                  data = merge_SCFA_FAMES_IU_1, 
                  method = 'glmnet', 
                  metric = "MAE")
  
  # Compute variable importance
  impVar <- varImp(glmnet)
  
  # Create variable importance plot using ggplot
  # Create variable importance plot using ggplot
  vi_plot <- ggplot(impVar, aes(x = variable, y = Overall, label = scfa)) +
    geom_bar(stat = "identity", fill = "#53cfff", width = 0.50 ) +
    theme_light(base_size = 20) +
    theme(axis.title.x = element_text(size = 15, colour = "black"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 15, colour = "black"),
          axis.text.y = element_text(size = 15, colour = "black")) +
    ggtitle(paste("Variable importance for", scfa))  
  
  
  # Print the variable importance plot
  print(vi_plot)
}
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-20-1.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-20-2.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-20-3.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-20-4.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-20-5.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-20-6.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-20-7.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-20-8.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-20-9.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-20-10.png)<!-- -->

###### Feature Selection Method for month 2


```r
# Remove the fatty acids which show high correlation
merge_SCFA_FAMES_IU_2 <- merge_SCFA_FAMES_IU_2 %>% select(-ID,-Month,-Type,-Decanoate,-Heptadecanoate, -Myristate,-Stearate,-Erucic.acid,-cis.8.11.14.Eicosatrienoic,-cis.11.Eicosenoic,Nervonic.acid,-Elaidic,-cis.13.16.Docosadienoic,-cis.10.Heptadecenoic,-Lignocerate,-Arachidonic,-Linolelaidic,-Hexanoate,-Palmitate,-Palmitoleate,-cis.11.14.Eicosadienoic,-Heptanoic,-Myristoleate,-cis.4.7.10.13.16.19.Docosahexaenoic,-Linoleic,-cis.5.8.11.14.17.Eicosapentaenoic,-Octanoate)

# Feature selection using glmnet method 

# Loop through each SCFA
for (scfa in SCFA_name) {
  formula <- as.formula(paste(scfa, "~ ."))
  glmnet <- train(formula, 
                  data = merge_SCFA_FAMES_IU_2, 
                  method = 'glmnet', 
                  metric = "MAE")
  
  # Compute variable importance
  impVar <- varImp(glmnet)
  
  # Create a new variable importance plot using ggplot
  vi_plot <- ggplot(impVar, aes(x = variable, y = Overall, label = scfa)) +
    geom_bar(stat = "identity", fill = "#53cfff", width = 0.50 ) +
    theme(axis.title.x = element_text(size = 15, colour = "black"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 15, colour = "black"),
          axis.text.y = element_text(size = 15, colour = "black"))
  
  # Print the variable importance plot
  print(vi_plot)
}
```

![](Correlation_regression_script_files/figure-html/unnamed-chunk-21-1.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-21-2.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-21-3.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-21-4.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-21-5.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-21-6.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-21-7.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-21-8.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-21-9.png)<!-- -->![](Correlation_regression_script_files/figure-html/unnamed-chunk-21-10.png)<!-- -->

## linear Regression model

#### Month 0


```r
# Split the data into training (70%) and testing (30%) sets
set.seed(123)  # For reproducibility
FAMES_index_0 <- sample(1:nrow(merge_SCFA_FAMES_IU_0), 0.7 * nrow(merge_SCFA_FAMES_IU_0))
FAMES_train_data_0 <- merge_SCFA_FAMES_IU_0[FAMES_index_0, ]
FAMES_test_data_0 <- merge_SCFA_FAMES_IU_0[-FAMES_index_0, ]

scfa_matrix_0 <- merge_SCFA_FAMES_IU_0 %>% 
  select(23:32)
scfa_matrix_0 <- as.matrix(scfa_matrix_0)


# List of features to iterate through
features <- c("Octanoate", "Decanoate", "Laurate", "Pentadecanoate", "Palmitate", 
              "Palmitoleate", "Heptadecanoate", "Oleic", "Elaidic", "Linoleic", 
              "Linolelaidic", "gamma.Linolenic", "Linolenic", "Eicosanoic", 
              "cis.11.Eicosenoic", "cis.8.11.14.Eicosatrienoic", "Arachidonic", 
              "Erucic.acid", "cis.13.16.Docosadienoic", "cis.4.7.10.13.16.19.Docosahexaenoic", 
              "Lignocerate", "Nervonic.acid")

# Perform linear regression for each feature
results <- list()

for (feature in features) {
  formula <- paste("scfa_matrix_0 ~", feature)
  model <- lm(formula, data = merge_SCFA_FAMES_IU_0)
  results[[feature]] <- summary(model)
}

# Print the summary results for each feature
for (feature in features) {
  cat("Summary for", feature, ":\n")
  print(results[[feature]])
  cat("\n")
}
```

```
## Summary for Octanoate :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Octanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -5.865 -3.660 -1.376  2.335 10.374 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)  -14.530      9.353  -1.554   0.1713  
## Octanoate      4.638      2.258   2.054   0.0858 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.632 on 6 degrees of freedom
## Multiple R-squared:  0.4128,	Adjusted R-squared:  0.3149 
## F-statistic: 4.217 on 1 and 6 DF,  p-value: 0.08582
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Octanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -11866  -8029  -4425   3106  23859 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    36898      22563   1.635    0.153
## Octanoate      -6818       5448  -1.251    0.257
## 
## Residual standard error: 13590 on 6 degrees of freedom
## Multiple R-squared:  0.207,	Adjusted R-squared:  0.07481 
## F-statistic: 1.566 on 1 and 6 DF,  p-value: 0.2574
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Octanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -20.423 -13.092  -3.842  16.380  20.911 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   86.492     30.180   2.866   0.0286 *
## Octanoate    -13.175      7.288  -1.808   0.1207  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 18.17 on 6 degrees of freedom
## Multiple R-squared:  0.3526,	Adjusted R-squared:  0.2447 
## F-statistic: 3.268 on 1 and 6 DF,  p-value: 0.1207
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Octanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.900  -6.805  -2.904   6.793  16.748 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   19.644     19.171   1.025    0.345
## Octanoate     -1.943      4.629  -0.420    0.689
## 
## Residual standard error: 11.54 on 6 degrees of freedom
## Multiple R-squared:  0.02853,	Adjusted R-squared:  -0.1334 
## F-statistic: 0.1762 on 1 and 6 DF,  p-value: 0.6893
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Octanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.37292 -0.16297 -0.02770  0.07956  0.59141 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   0.8505     0.5076   1.676    0.145
## Octanoate    -0.1750     0.1226  -1.428    0.203
## 
## Residual standard error: 0.3057 on 6 degrees of freedom
## Multiple R-squared:  0.2536,	Adjusted R-squared:  0.1292 
## F-statistic: 2.038 on 1 and 6 DF,  p-value: 0.2033
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Octanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.3427 -2.3547 -0.9774  2.7349  4.7318 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   10.714      6.032   1.776    0.126
## Octanoate     -1.785      1.456  -1.226    0.266
## 
## Residual standard error: 3.632 on 6 degrees of freedom
## Multiple R-squared:  0.2003,	Adjusted R-squared:  0.06698 
## F-statistic: 1.503 on 1 and 6 DF,  p-value: 0.2662
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Octanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -11.677  -7.522  -1.516   5.356  17.466 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   18.958     17.769   1.067    0.327
## Octanoate     -2.327      4.291  -0.542    0.607
## 
## Residual standard error: 10.7 on 6 degrees of freedom
## Multiple R-squared:  0.04671,	Adjusted R-squared:  -0.1122 
## F-statistic: 0.294 on 1 and 6 DF,  p-value: 0.6072
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Octanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -60.45 -49.63 -37.10  32.50 154.67 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   28.754    135.069   0.213    0.838
## Octanoate      9.587     32.616   0.294    0.779
## 
## Residual standard error: 81.34 on 6 degrees of freedom
## Multiple R-squared:  0.0142,	Adjusted R-squared:  -0.1501 
## F-statistic: 0.0864 on 1 and 6 DF,  p-value: 0.7787
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Octanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -269.99 -116.30  -51.83   97.20  300.71 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   773.23     362.70   2.132    0.077 .
## Octanoate    -122.39      87.58  -1.397    0.212  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 218.4 on 6 degrees of freedom
## Multiple R-squared:  0.2455,	Adjusted R-squared:  0.1198 
## F-statistic: 1.953 on 1 and 6 DF,  p-value: 0.2118
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Octanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -67.71 -50.39 -22.32  26.59 139.43 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   126.21     125.80   1.003    0.354
## Octanoate     -16.13      30.38  -0.531    0.614
## 
## Residual standard error: 75.75 on 6 degrees of freedom
## Multiple R-squared:  0.04489,	Adjusted R-squared:  -0.1143 
## F-statistic: 0.282 on 1 and 6 DF,  p-value: 0.6145
## 
## 
## 
## Summary for Decanoate :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Decanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.7288 -3.7866 -2.0200  0.4408 14.6906 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   1.2172     5.6365   0.216    0.836
## Decanoate     0.9879     1.6491   0.599    0.571
## 
## Residual standard error: 7.139 on 6 degrees of freedom
## Multiple R-squared:  0.05643,	Adjusted R-squared:  -0.1008 
## F-statistic: 0.3588 on 1 and 6 DF,  p-value: 0.5711
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Decanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -16013  -5670  -2880   1144  27140 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)    21015      10796   1.947   0.0995 .
## Decanoate      -3830       3158  -1.213   0.2709  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13670 on 6 degrees of freedom
## Multiple R-squared:  0.1968,	Adjusted R-squared:  0.06297 
## F-statistic:  1.47 on 1 and 6 DF,  p-value: 0.2709
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Decanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -18.535 -14.237  -6.761   5.917  34.709 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   43.849     17.156   2.556   0.0431 *
## Decanoate     -3.489      5.019  -0.695   0.5129  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 21.73 on 6 degrees of freedom
## Multiple R-squared:  0.07455,	Adjusted R-squared:  -0.07969 
## F-statistic: 0.4833 on 1 and 6 DF,  p-value: 0.5129
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Decanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -11.559  -8.850  -1.923   9.169  13.786 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    5.344      8.768   0.609    0.565
## Decanoate      2.106      2.565   0.821    0.443
## 
## Residual standard error: 11.1 on 6 degrees of freedom
## Multiple R-squared:  0.101,	Adjusted R-squared:  -0.04879 
## F-statistic: 0.6743 on 1 and 6 DF,  p-value: 0.4429
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Decanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.17004 -0.14403 -0.12871 -0.07319  0.79323 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)  0.170304   0.279029   0.610    0.564
## Decanoate   -0.009098   0.081636  -0.111    0.915
## 
## Residual standard error: 0.3534 on 6 degrees of freedom
## Multiple R-squared:  0.002066,	Adjusted R-squared:  -0.1643 
## F-statistic: 0.01242 on 1 and 6 DF,  p-value: 0.9149
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Decanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -3.824 -2.401 -1.390  1.351  6.496 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   3.8271     3.2029   1.195    0.277
## Decanoate    -0.1103     0.9371  -0.118    0.910
## 
## Residual standard error: 4.057 on 6 degrees of freedom
## Multiple R-squared:  0.002304,	Adjusted R-squared:  -0.164 
## F-statistic: 0.01386 on 1 and 6 DF,  p-value: 0.9101
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Decanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -10.931  -6.461  -3.288   5.486  19.476 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   14.304      8.375   1.708    0.139
## Decanoate     -1.557      2.450  -0.636    0.548
## 
## Residual standard error: 10.61 on 6 degrees of freedom
## Multiple R-squared:  0.06309,	Adjusted R-squared:  -0.09306 
## F-statistic: 0.404 on 1 and 6 DF,  p-value: 0.5485
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Decanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -68.01 -51.09 -20.23  34.52 150.02 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    29.06      62.24   0.467    0.657
## Decanoate      12.59      18.21   0.691    0.515
## 
## Residual standard error: 78.84 on 6 degrees of freedom
## Multiple R-squared:  0.07381,	Adjusted R-squared:  -0.08055 
## F-statistic: 0.4782 on 1 and 6 DF,  p-value: 0.5151
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Decanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -287.66 -136.83   -5.26   91.42  349.96 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   507.94     168.50   3.014   0.0236 *
## Decanoate     -75.23      49.30  -1.526   0.1779  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 213.4 on 6 degrees of freedom
## Multiple R-squared:  0.2796,	Adjusted R-squared:  0.1595 
## F-statistic: 2.329 on 1 and 6 DF,  p-value: 0.1779
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Decanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -54.93 -44.05 -36.26  17.69 140.38 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   48.449     60.933   0.795    0.457
## Decanoate      4.087     17.827   0.229    0.826
## 
## Residual standard error: 77.18 on 6 degrees of freedom
## Multiple R-squared:  0.008685,	Adjusted R-squared:  -0.1565 
## F-statistic: 0.05257 on 1 and 6 DF,  p-value: 0.8263
## 
## 
## 
## Summary for Laurate :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Laurate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.9600 -3.3759 -2.5472  0.9728 15.5213 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   7.2938     7.9460   0.918    0.394
## Laurate      -0.7938     1.9527  -0.407    0.698
## 
## Residual standard error: 7.25 on 6 degrees of freedom
## Multiple R-squared:  0.02681,	Adjusted R-squared:  -0.1354 
## F-statistic: 0.1653 on 1 and 6 DF,  p-value: 0.6985
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Laurate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -13144  -8556  -2418   3316  22548 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    27561      14752   1.868    0.111
## Laurate        -4738       3625  -1.307    0.239
## 
## Residual standard error: 13460 on 6 degrees of freedom
## Multiple R-squared:  0.2216,	Adjusted R-squared:  0.0919 
## F-statistic: 1.708 on 1 and 6 DF,  p-value: 0.239
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Laurate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -26.064 -11.495  -9.808  14.883  32.404 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   43.277     24.369   1.776    0.126
## Laurate       -2.620      5.988  -0.438    0.677
## 
## Residual standard error: 22.24 on 6 degrees of freedom
## Multiple R-squared:  0.03092,	Adjusted R-squared:  -0.1306 
## F-statistic: 0.1914 on 1 and 6 DF,  p-value: 0.677
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Laurate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.767 -7.784 -4.666 10.256 13.517 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  -0.4436    11.7033  -0.038    0.971
## Laurate       3.1738     2.8760   1.104    0.312
## 
## Residual standard error: 10.68 on 6 degrees of freedom
## Multiple R-squared:  0.1687,	Adjusted R-squared:  0.03018 
## F-statistic: 1.218 on 1 and 6 DF,  p-value: 0.3121
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Laurate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.36268 -0.10010 -0.03297  0.02037  0.64498 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  0.53305    0.34922   1.526    0.178
## Laurate     -0.10140    0.08582  -1.182    0.282
## 
## Residual standard error: 0.3186 on 6 degrees of freedom
## Multiple R-squared:  0.1887,	Adjusted R-squared:  0.05354 
## F-statistic: 1.396 on 1 and 6 DF,  p-value: 0.2821
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Laurate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.9208 -1.6327 -0.8957  0.4566  6.1589 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   7.7939     4.0457   1.926    0.102
## Laurate      -1.1174     0.9942  -1.124    0.304
## 
## Residual standard error: 3.691 on 6 degrees of freedom
## Multiple R-squared:  0.1739,	Adjusted R-squared:  0.03624 
## F-statistic: 1.263 on 1 and 6 DF,  p-value: 0.304
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Laurate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -8.425 -7.674 -2.435  3.517 20.807 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   8.0055    11.9922   0.668    0.529
## Laurate       0.3994     2.9470   0.136    0.897
## 
## Residual standard error: 10.94 on 6 degrees of freedom
## Multiple R-squared:  0.003051,	Adjusted R-squared:  -0.1631 
## F-statistic: 0.01836 on 1 and 6 DF,  p-value: 0.8966
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Laurate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -75.50 -38.61 -11.26  20.59 127.28 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -26.38      80.12  -0.329    0.753
## Laurate        24.38      19.69   1.238    0.262
## 
## Residual standard error: 73.11 on 6 degrees of freedom
## Multiple R-squared:  0.2036,	Adjusted R-squared:  0.07086 
## F-statistic: 1.534 on 1 and 6 DF,  p-value: 0.2618
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Laurate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -286.41 -110.82  -36.14  127.32  368.43 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   443.76     266.15   1.667    0.146
## Laurate       -43.03      65.40  -0.658    0.535
## 
## Residual standard error: 242.8 on 6 degrees of freedom
## Multiple R-squared:  0.06728,	Adjusted R-squared:  -0.08817 
## F-statistic: 0.4328 on 1 and 6 DF,  p-value: 0.535
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Laurate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -63.13 -44.09 -28.06  12.57 135.71 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   40.196     84.480   0.476    0.651
## Laurate        5.386     20.760   0.259    0.804
## 
## Residual standard error: 77.08 on 6 degrees of freedom
## Multiple R-squared:  0.01109,	Adjusted R-squared:  -0.1537 
## F-statistic: 0.0673 on 1 and 6 DF,  p-value: 0.804
## 
## 
## 
## Summary for Pentadecanoate :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Pentadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.7628 -4.0455 -0.4946  1.2220 14.2429 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)       8.260      6.274   1.316    0.236
## Pentadecanoate    3.068      4.389   0.699    0.511
## 
## Residual standard error: 7.067 on 6 degrees of freedom
## Multiple R-squared:  0.07533,	Adjusted R-squared:  -0.07878 
## F-statistic: 0.4888 on 1 and 6 DF,  p-value: 0.5106
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Pentadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -13138  -5714  -4244   -890  31179 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)       858.5    13013.0   0.066    0.950
## Pentadecanoate  -6444.6     9102.3  -0.708    0.505
## 
## Residual standard error: 14660 on 6 degrees of freedom
## Multiple R-squared:  0.07711,	Adjusted R-squared:  -0.07671 
## F-statistic: 0.5013 on 1 and 6 DF,  p-value: 0.5055
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Pentadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.216 -14.697  -7.523  12.499  36.090 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)     31.8875    20.0452   1.591    0.163
## Pentadecanoate  -0.9894    14.0211  -0.071    0.946
## 
## Residual standard error: 22.58 on 6 degrees of freedom
## Multiple R-squared:  0.0008292,	Adjusted R-squared:  -0.1657 
## F-statistic: 0.004979 on 1 and 6 DF,  p-value: 0.946
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Pentadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -14.4298  -5.0167   0.7125   7.6403   9.8255 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)  
## (Intercept)      25.928      8.276   3.133   0.0202 *
## Pentadecanoate   10.788      5.789   1.864   0.1117  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.321 on 6 degrees of freedom
## Multiple R-squared:  0.3666,	Adjusted R-squared:  0.2611 
## F-statistic: 3.473 on 1 and 6 DF,  p-value: 0.1117
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Pentadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.19105 -0.15122 -0.09498 -0.03307  0.75624 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)    -0.06772    0.29983  -0.226    0.829
## Pentadecanoate -0.16030    0.20973  -0.764    0.474
## 
## Residual standard error: 0.3377 on 6 degrees of freedom
## Multiple R-squared:  0.08873,	Adjusted R-squared:  -0.06315 
## F-statistic: 0.5842 on 1 and 6 DF,  p-value: 0.4736
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Pentadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.2118 -2.6318 -1.3088  0.9851  6.8964 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)      4.6946     3.5658   1.317    0.236
## Pentadecanoate   0.9186     2.4942   0.368    0.725
## 
## Residual standard error: 4.016 on 6 degrees of freedom
## Multiple R-squared:  0.02211,	Adjusted R-squared:  -0.1409 
## F-statistic: 0.1356 on 1 and 6 DF,  p-value: 0.7253
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Pentadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.285 -8.081 -1.656  4.577 19.803 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)       7.492      9.687   0.773    0.469
## Pentadecanoate   -1.565      6.776  -0.231    0.825
## 
## Residual standard error: 10.91 on 6 degrees of freedom
## Multiple R-squared:  0.00881,	Adjusted R-squared:  -0.1564 
## F-statistic: 0.05333 on 1 and 6 DF,  p-value: 0.825
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Pentadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -52.88 -49.07 -44.80  38.46 156.24 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)      60.537     72.663   0.833    0.437
## Pentadecanoate   -5.344     50.826  -0.105    0.920
## 
## Residual standard error: 81.84 on 6 degrees of freedom
## Multiple R-squared:  0.001839,	Adjusted R-squared:  -0.1645 
## F-statistic: 0.01106 on 1 and 6 DF,  p-value: 0.9197
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Pentadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -290.4 -109.6  -35.5   85.1  453.7 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)      236.47     222.48   1.063    0.329
## Pentadecanoate   -31.68     155.62  -0.204    0.845
## 
## Residual standard error: 250.6 on 6 degrees of freedom
## Multiple R-squared:  0.006861,	Adjusted R-squared:  -0.1587 
## F-statistic: 0.04145 on 1 and 6 DF,  p-value: 0.8454
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Pentadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -64.21 -47.27 -26.97  23.95 124.28 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)      104.06      66.09   1.574    0.166
## Pentadecanoate    32.88      46.23   0.711    0.504
## 
## Residual standard error: 74.44 on 6 degrees of freedom
## Multiple R-squared:  0.07775,	Adjusted R-squared:  -0.07596 
## F-statistic: 0.5058 on 1 and 6 DF,  p-value: 0.5037
## 
## 
## 
## Summary for Palmitate :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Palmitate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.9070 -3.8415 -2.4969  0.5543 15.6193 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   6.2588     5.6252   1.113    0.308
## Palmitate    -0.5837     1.4449  -0.404    0.700
## 
## Residual standard error: 7.251 on 6 degrees of freedom
## Multiple R-squared:  0.02648,	Adjusted R-squared:  -0.1358 
## F-statistic: 0.1632 on 1 and 6 DF,  p-value: 0.7003
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Palmitate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -11527  -8205  -2592   2121  30232 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    17167      11274   1.523    0.179
## Palmitate      -2268       2896  -0.783    0.463
## 
## Residual standard error: 14530 on 6 degrees of freedom
## Multiple R-squared:  0.09271,	Adjusted R-squared:  -0.0585 
## F-statistic: 0.6131 on 1 and 6 DF,  p-value: 0.4634
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Palmitate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -21.171  -9.186   4.208   7.763  18.350 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)    3.987     11.299   0.353   0.7362  
## Palmitate      8.426      2.902   2.903   0.0272 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 14.57 on 6 degrees of freedom
## Multiple R-squared:  0.5841,	Adjusted R-squared:  0.5148 
## F-statistic: 8.428 on 1 and 6 DF,  p-value: 0.02722
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Palmitate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.473  -5.665  -2.017   4.856  15.632 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    2.153      7.940   0.271    0.795
## Palmitate      2.779      2.040   1.362    0.222
## 
## Residual standard error: 10.24 on 6 degrees of freedom
## Multiple R-squared:  0.2362,	Adjusted R-squared:  0.109 
## F-statistic: 1.856 on 1 and 6 DF,  p-value: 0.222
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Palmitate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.33586 -0.19713 -0.03582  0.12833  0.48484 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept) -0.20408    0.22371  -0.912    0.397
## Palmitate    0.10002    0.05746   1.741    0.132
## 
## Residual standard error: 0.2884 on 6 degrees of freedom
## Multiple R-squared:  0.3355,	Adjusted R-squared:  0.2248 
## F-statistic: 3.029 on 1 and 6 DF,  p-value: 0.1324
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Palmitate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.6141 -2.1595 -1.3628  0.9808  6.1680 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   4.9735     3.0763   1.617    0.157
## Palmitate    -0.4281     0.7902  -0.542    0.607
## 
## Residual standard error: 3.966 on 6 degrees of freedom
## Multiple R-squared:  0.04664,	Adjusted R-squared:  -0.1123 
## F-statistic: 0.2935 on 1 and 6 DF,  p-value: 0.6075
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Palmitate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.722 -4.708 -1.845  3.293 12.752 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   -2.788      6.347  -0.439   0.6758  
## Palmitate      3.559      1.630   2.183   0.0718 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.181 on 6 degrees of freedom
## Multiple R-squared:  0.4427,	Adjusted R-squared:  0.3498 
## F-statistic: 4.766 on 1 and 6 DF,  p-value: 0.07176
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Palmitate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -58.60 -47.63 -24.90  19.18 161.12 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   103.42      61.38   1.685    0.143
## Palmitate     -10.35      15.77  -0.657    0.536
## 
## Residual standard error: 79.12 on 6 degrees of freedom
## Multiple R-squared:  0.06706,	Adjusted R-squared:  -0.08843 
## F-statistic: 0.4313 on 1 and 6 DF,  p-value: 0.5357
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Palmitate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -270.84 -133.28  -12.57   74.30  455.40 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  297.533    194.854   1.527    0.178
## Palmitate     -5.631     50.052  -0.112    0.914
## 
## Residual standard error: 251.2 on 6 degrees of freedom
## Multiple R-squared:  0.002105,	Adjusted R-squared:  -0.1642 
## F-statistic: 0.01266 on 1 and 6 DF,  p-value: 0.9141
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Palmitate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -89.51 -32.28 -12.51  28.29 129.64 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    5.558     54.501   0.102    0.922
## Palmitate     15.982     14.000   1.142    0.297
## 
## Residual standard error: 70.26 on 6 degrees of freedom
## Multiple R-squared:  0.1785,	Adjusted R-squared:  0.04153 
## F-statistic: 1.303 on 1 and 6 DF,  p-value: 0.2971
## 
## 
## 
## Summary for Palmitoleate :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Palmitoleate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.1069 -2.3684 -0.2767  1.6979 12.0934 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)     3.066      2.459   1.247    0.259
## Palmitoleate   -6.425      4.904  -1.310    0.238
## 
## Residual standard error: 6.481 on 6 degrees of freedom
## Multiple R-squared:  0.2225,	Adjusted R-squared:  0.09286 
## F-statistic: 1.717 on 1 and 6 DF,  p-value: 0.2381
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Palmitoleate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -16518  -7953  -1422   3721  25863 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)      7201       5282   1.363    0.222
## Palmitoleate   -11577      10534  -1.099    0.314
## 
## Residual standard error: 13920 on 6 degrees of freedom
## Multiple R-squared:  0.1676,	Adjusted R-squared:  0.02884 
## F-statistic: 1.208 on 1 and 6 DF,  p-value: 0.3139
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Palmitoleate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -23.8572 -12.2719   0.3184  12.9456  24.5897 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)   
## (Intercept)    37.232      7.266   5.124  0.00217 **
## Palmitoleate   22.213     14.488   1.533  0.17613   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19.15 on 6 degrees of freedom
## Multiple R-squared:  0.2815,	Adjusted R-squared:  0.1617 
## F-statistic: 2.351 on 1 and 6 DF,  p-value: 0.1761
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Palmitoleate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.996  -4.755  -2.817   4.554  15.215 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)  
## (Intercept)    13.553      3.974   3.411   0.0143 *
## Palmitoleate    9.724      7.924   1.227   0.2657  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.47 on 6 degrees of freedom
## Multiple R-squared:  0.2006,	Adjusted R-squared:  0.06739 
## F-statistic: 1.506 on 1 and 6 DF,  p-value: 0.2657
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Palmitoleate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.30070 -0.16506 -0.05977  0.06830  0.61502 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)    0.2053     0.1142   1.798    0.122
## Palmitoleate   0.3448     0.2277   1.514    0.181
## 
## Residual standard error: 0.3009 on 6 degrees of freedom
## Multiple R-squared:  0.2765,	Adjusted R-squared:  0.1559 
## F-statistic: 2.293 on 1 and 6 DF,  p-value: 0.1807
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Palmitoleate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.599 -2.284 -0.078  2.045  4.152 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)  
## (Intercept)     2.731      1.284   2.127   0.0775 .
## Palmitoleate   -4.164      2.560  -1.626   0.1550  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.384 on 6 degrees of freedom
## Multiple R-squared:  0.3059,	Adjusted R-squared:  0.1903 
## F-statistic: 2.645 on 1 and 6 DF,  p-value: 0.155
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Palmitoleate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -14.396  -2.743  -1.693   4.524  14.238 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)  
## (Intercept)    11.598      3.459   3.353   0.0154 *
## Palmitoleate   11.278      6.897   1.635   0.1531  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.115 on 6 degrees of freedom
## Multiple R-squared:  0.3083,	Adjusted R-squared:  0.193 
## F-statistic: 2.674 on 1 and 6 DF,  p-value: 0.1531
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Palmitoleate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -60.37 -48.84 -37.13  36.23 157.02 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)  
## (Intercept)     70.60      30.90   2.285   0.0624 .
## Palmitoleate    16.75      61.61   0.272   0.7949  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 81.42 on 6 degrees of freedom
## Multiple R-squared:  0.01217,	Adjusted R-squared:  -0.1525 
## F-statistic: 0.07389 on 1 and 6 DF,  p-value: 0.7949
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Palmitoleate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -354.52 -118.25   -2.48  112.05  363.16 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)  
## (Intercept)    248.17      89.33   2.778   0.0321 *
## Palmitoleate  -163.85     178.13  -0.920   0.3931  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 235.4 on 6 degrees of freedom
## Multiple R-squared:  0.1236,	Adjusted R-squared:  -0.02249 
## F-statistic: 0.8461 on 1 and 6 DF,  p-value: 0.3931
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Palmitoleate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -56.95 -46.47 -27.32  14.14 144.54 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)  
## (Intercept)     63.54      29.27   2.171    0.073 .
## Palmitoleate    14.29      58.37   0.245    0.815  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 77.13 on 6 degrees of freedom
## Multiple R-squared:  0.009896,	Adjusted R-squared:  -0.1551 
## F-statistic: 0.05997 on 1 and 6 DF,  p-value: 0.8147
## 
## 
## 
## Summary for Heptadecanoate :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Heptadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -6.219 -3.371 -1.377  1.083 13.273 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)      -15.81      17.45  -0.906     0.40
## Heptadecanoate   -16.80      14.49  -1.159     0.29
## 
## Residual standard error: 6.643 on 6 degrees of freedom
## Multiple R-squared:  0.183,	Adjusted R-squared:  0.0468 
## F-statistic: 1.344 on 1 and 6 DF,  p-value: 0.2904
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Heptadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -12218  -5505  -2709   1676  25941 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)       63180      33382   1.893    0.107
## Heptadecanoate    45131      27712   1.629    0.155
## 
## Residual standard error: 12710 on 6 degrees of freedom
## Multiple R-squared:  0.3065,	Adjusted R-squared:  0.191 
## F-statistic: 2.652 on 1 and 6 DF,  p-value: 0.1545
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Heptadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -25.430 -13.406  -4.781  15.241  31.038 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)       84.62      55.43   1.527    0.178
## Heptadecanoate    43.09      46.02   0.936    0.385
## 
## Residual standard error: 21.1 on 6 degrees of freedom
## Multiple R-squared:  0.1275,	Adjusted R-squared:  -0.01789 
## F-statistic: 0.877 on 1 and 6 DF,  p-value: 0.3852
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Heptadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.806  -5.614  -4.481   7.315  15.548 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)       31.04      29.73   1.044    0.337
## Heptadecanoate    16.13      24.68   0.654    0.538
## 
## Residual standard error: 11.32 on 6 degrees of freedom
## Multiple R-squared:  0.06646,	Adjusted R-squared:  -0.08913 
## F-statistic: 0.4272 on 1 and 6 DF,  p-value: 0.5376
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Heptadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.20114 -0.18218 -0.10984 -0.02643  0.75062 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)      0.5969     0.9105   0.656    0.536
## Heptadecanoate   0.3807     0.7558   0.504    0.632
## 
## Residual standard error: 0.3465 on 6 degrees of freedom
## Multiple R-squared:  0.04056,	Adjusted R-squared:  -0.1194 
## F-statistic: 0.2536 on 1 and 6 DF,  p-value: 0.6325
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Heptadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -3.321 -2.589 -1.438  1.433  6.433 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)       5.265     10.646   0.495    0.638
## Heptadecanoate    1.487      8.838   0.168    0.872
## 
## Residual standard error: 4.052 on 6 degrees of freedom
## Multiple R-squared:  0.004698,	Adjusted R-squared:  -0.1612 
## F-statistic: 0.02832 on 1 and 6 DF,  p-value: 0.8719
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Heptadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -13.1288  -4.0609  -0.8501   2.0510  16.5673 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)       44.81      24.86   1.803    0.121
## Heptadecanoate    29.55      20.64   1.432    0.202
## 
## Residual standard error: 9.461 on 6 degrees of freedom
## Multiple R-squared:  0.2547,	Adjusted R-squared:  0.1305 
## F-statistic:  2.05 on 1 and 6 DF,  p-value: 0.2021
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Heptadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -64.20 -45.99 -36.53  40.45 148.18 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)      -40.81     210.55  -0.194    0.853
## Heptadecanoate   -90.77     174.79  -0.519    0.622
## 
## Residual standard error: 80.14 on 6 degrees of freedom
## Multiple R-squared:  0.04302,	Adjusted R-squared:  -0.1165 
## F-statistic: 0.2697 on 1 and 6 DF,  p-value: 0.6221
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Heptadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -182.98  -93.69  -42.62   66.85  309.64 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)  
## (Intercept)      1467.2      443.2   3.310   0.0162 *
## Heptadecanoate    996.2      368.0   2.707   0.0352 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 168.7 on 6 degrees of freedom
## Multiple R-squared:  0.5499,	Adjusted R-squared:  0.4749 
## F-statistic: 7.331 on 1 and 6 DF,  p-value: 0.03522
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Heptadecanoate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -56.470 -44.972 -24.889   6.496 145.288 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
## (Intercept)      139.06     201.10   0.691    0.515
## Heptadecanoate    65.44     166.95   0.392    0.709
## 
## Residual standard error: 76.54 on 6 degrees of freedom
## Multiple R-squared:  0.02497,	Adjusted R-squared:  -0.1375 
## F-statistic: 0.1537 on 1 and 6 DF,  p-value: 0.7086
## 
## 
## 
## Summary for Oleic :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Oleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.3636 -1.5319 -1.2842  0.1342 12.0761 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   18.196      9.670   1.882    0.109
## Oleic         -3.215      2.167  -1.483    0.189
## 
## Residual standard error: 6.287 on 6 degrees of freedom
## Multiple R-squared:  0.2683,	Adjusted R-squared:  0.1464 
## F-statistic:   2.2 on 1 and 6 DF,  p-value: 0.1885
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Oleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -14709.5  -5193.9  -2457.1    974.7  27812.2 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -11786      21735  -0.542    0.607
## Oleic           4859       4872   0.997    0.357
## 
## Residual standard error: 14130 on 6 degrees of freedom
## Multiple R-squared:  0.1422,	Adjusted R-squared:  -0.0007787 
## F-statistic: 0.9946 on 1 and 6 DF,  p-value: 0.3571
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Oleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -30.019 -11.969  -0.464   8.573  35.663 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   54.116     33.615    1.61    0.159
## Oleic         -4.821      7.535   -0.64    0.546
## 
## Residual standard error: 21.85 on 6 degrees of freedom
## Multiple R-squared:  0.06387,	Adjusted R-squared:  -0.09215 
## F-statistic: 0.4094 on 1 and 6 DF,  p-value: 0.5459
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Oleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -10.735  -8.752  -3.831   8.474  15.427 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    22.51      17.44   1.290    0.244
## Oleic          -2.47       3.91  -0.632    0.551
## 
## Residual standard error: 11.34 on 6 degrees of freedom
## Multiple R-squared:  0.06238,	Adjusted R-squared:  -0.0939 
## F-statistic: 0.3991 on 1 and 6 DF,  p-value: 0.5508
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Oleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.15955 -0.13811 -0.12137 -0.08142  0.79983 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  0.07094    0.54333   0.131    0.900
## Oleic        0.01648    0.12179   0.135    0.897
## 
## Residual standard error: 0.3532 on 6 degrees of freedom
## Multiple R-squared:  0.003043,	Adjusted R-squared:  -0.1631 
## F-statistic: 0.01831 on 1 and 6 DF,  p-value: 0.8968
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Oleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.1281 -2.2729 -0.8133  2.5695  4.7418 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -4.563      5.255  -0.868    0.419
## Oleic          1.855      1.178   1.575    0.166
## 
## Residual standard error: 3.416 on 6 degrees of freedom
## Multiple R-squared:  0.2924,	Adjusted R-squared:  0.1745 
## F-statistic: 2.479 on 1 and 6 DF,  p-value: 0.1664
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Oleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.913  -4.378  -1.755   2.143  19.748 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   23.609     15.790   1.495    0.185
## Oleic         -3.240      3.539  -0.915    0.395
## 
## Residual standard error: 10.27 on 6 degrees of freedom
## Multiple R-squared:  0.1225,	Adjusted R-squared:  -0.02373 
## F-statistic: 0.8378 on 1 and 6 DF,  p-value: 0.3953
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Oleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -89.293 -28.885  -2.715  14.333 115.218 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  -111.62     101.14  -1.104    0.312
## Oleic          41.26      22.67   1.820    0.119
## 
## Residual standard error: 65.75 on 6 degrees of freedom
## Multiple R-squared:  0.3558,	Adjusted R-squared:  0.2484 
## F-statistic: 3.313 on 1 and 6 DF,  p-value: 0.1186
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Oleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -365.65  -54.19  -10.94   51.84  394.52 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -4.721    368.135  -0.013     0.99
## Oleic         65.120     82.517   0.789     0.46
## 
## Residual standard error: 239.3 on 6 degrees of freedom
## Multiple R-squared:  0.09404,	Adjusted R-squared:  -0.05696 
## F-statistic: 0.6228 on 1 and 6 DF,  p-value: 0.4601
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Oleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -70.50 -52.36 -14.31  20.33 129.06 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    2.665    116.695   0.023    0.983
## Oleic         13.422     26.157   0.513    0.626
## 
## Residual standard error: 75.87 on 6 degrees of freedom
## Multiple R-squared:  0.04204,	Adjusted R-squared:  -0.1176 
## F-statistic: 0.2633 on 1 and 6 DF,  p-value: 0.6262
## 
## 
## 
## Summary for Elaidic :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Elaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -3.820 -3.332 -1.871 -1.185 15.629 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    5.720      3.059   1.870    0.111
## Elaidic        8.900     10.870   0.819    0.444
## 
## Residual standard error: 6.97 on 6 degrees of freedom
## Multiple R-squared:  0.1005,	Adjusted R-squared:  -0.04942 
## F-statistic: 0.6704 on 1 and 6 DF,  p-value: 0.4442
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Elaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -12134  -9024  -5519   6205  27100 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     5917       6274   0.943    0.382
## Elaidic       -20357      22294  -0.913    0.396
## 
## Residual standard error: 14300 on 6 degrees of freedom
## Multiple R-squared:  0.122,	Adjusted R-squared:  -0.02433 
## F-statistic: 0.8337 on 1 and 6 DF,  p-value: 0.3964
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Elaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -24.819 -14.318  -2.717   7.212  35.866 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   35.741      9.754   3.664   0.0105 *
## Elaidic       15.333     34.663   0.442   0.6737  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 22.23 on 6 degrees of freedom
## Multiple R-squared:  0.03158,	Adjusted R-squared:  -0.1298 
## F-statistic: 0.1957 on 1 and 6 DF,  p-value: 0.6737
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Elaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -11.897  -7.778  -4.402   8.566  16.877 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   11.453      5.135   2.230   0.0672 .
## Elaidic       -1.971     18.247  -0.108   0.9175  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.7 on 6 degrees of freedom
## Multiple R-squared:  0.001941,	Adjusted R-squared:  -0.1644 
## F-statistic: 0.01167 on 1 and 6 DF,  p-value: 0.9175
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Elaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.15060 -0.14426 -0.13473 -0.07668  0.79840 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  0.13771    0.15521   0.887    0.409
## Elaidic     -0.02873    0.55157  -0.052    0.960
## 
## Residual standard error: 0.3537 on 6 degrees of freedom
## Multiple R-squared:  0.0004519,	Adjusted R-squared:  -0.1661 
## F-statistic: 0.002713 on 1 and 6 DF,  p-value: 0.9602
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Elaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.7252 -2.0707 -0.4398  1.9118  3.9130 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)    1.829      1.366   1.339   0.2291  
## Elaidic       -9.965      4.854  -2.053   0.0859 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.113 on 6 degrees of freedom
## Multiple R-squared:  0.4126,	Adjusted R-squared:  0.3147 
## F-statistic: 4.214 on 1 and 6 DF,  p-value: 0.0859
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Elaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.918 -5.192 -3.209  3.376 19.639 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   12.574      4.332   2.902   0.0272 *
## Elaidic       18.177     15.395   1.181   0.2824  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.872 on 6 degrees of freedom
## Multiple R-squared:  0.1886,	Adjusted R-squared:  0.05331 
## F-statistic: 1.394 on 1 and 6 DF,  p-value: 0.2824
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Elaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -74.43 -49.72 -22.87  35.82 143.08 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    51.10      34.11   1.498    0.185
## Elaidic       -98.69     121.23  -0.814    0.447
## 
## Residual standard error: 77.74 on 6 degrees of freedom
## Multiple R-squared:  0.09946,	Adjusted R-squared:  -0.05062 
## F-statistic: 0.6627 on 1 and 6 DF,  p-value: 0.4467
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Elaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -315.81 -138.06   20.29   75.18  401.31 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)    242.2      107.5   2.252   0.0653 .
## Elaidic       -214.9      382.2  -0.562   0.5943  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 245.1 on 6 degrees of freedom
## Multiple R-squared:  0.05006,	Adjusted R-squared:  -0.1083 
## F-statistic: 0.3162 on 1 and 6 DF,  p-value: 0.5943
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Elaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -57.97 -45.30 -31.23  21.21 137.73 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    54.98      33.77   1.628    0.155
## Elaidic       -35.76     119.99  -0.298    0.776
## 
## Residual standard error: 76.95 on 6 degrees of freedom
## Multiple R-squared:  0.01459,	Adjusted R-squared:  -0.1496 
## F-statistic: 0.08882 on 1 and 6 DF,  p-value: 0.7757
## 
## 
## 
## Summary for Linoleic :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Linoleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -5.222 -3.673 -2.267  1.025 15.374 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -2.610     14.994  -0.174    0.868
## Linoleic       3.828      8.260   0.463    0.659
## 
## Residual standard error: 7.221 on 6 degrees of freedom
## Multiple R-squared:  0.03455,	Adjusted R-squared:  -0.1264 
## F-statistic: 0.2147 on 1 and 6 DF,  p-value: 0.6594
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Linoleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -10143  -7331  -5752   -385  31951 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    844.1    31484.4   0.027    0.979
## Linoleic      4732.7    17343.9   0.273    0.794
## 
## Residual standard error: 15160 on 6 degrees of freedom
## Multiple R-squared:  0.01226,	Adjusted R-squared:  -0.1524 
## F-statistic: 0.07446 on 1 and 6 DF,  p-value: 0.7941
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Linoleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -22.570 -14.411  -6.132  10.587  38.696 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   43.839     46.691   0.939    0.384
## Linoleic      -5.956     25.721  -0.232    0.825
## 
## Residual standard error: 22.49 on 6 degrees of freedom
## Multiple R-squared:  0.008857,	Adjusted R-squared:  -0.1563 
## F-statistic: 0.05362 on 1 and 6 DF,  p-value: 0.8246
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Linoleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -10.343  -8.022  -5.591  10.418  14.512 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   21.314     23.996   0.888    0.409
## Linoleic      -5.329     13.219  -0.403    0.701
## 
## Residual standard error: 11.56 on 6 degrees of freedom
## Multiple R-squared:  0.02637,	Adjusted R-squared:  -0.1359 
## F-statistic: 0.1625 on 1 and 6 DF,  p-value: 0.7008
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Linoleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.26814 -0.23709 -0.01156  0.07059  0.60501 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  -0.7299     0.6395  -1.141    0.297
## Linoleic      0.4877     0.3523   1.384    0.215
## 
## Residual standard error: 0.308 on 6 degrees of freedom
## Multiple R-squared:  0.2421,	Adjusted R-squared:  0.1158 
## F-statistic: 1.917 on 1 and 6 DF,  p-value: 0.2155
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Linoleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.5475 -1.6257 -0.9906  1.6046  5.5653 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -6.969      7.235  -0.963    0.373
## Linoleic       5.847      3.985   1.467    0.193
## 
## Residual standard error: 3.484 on 6 degrees of freedom
## Multiple R-squared:  0.264,	Adjusted R-squared:  0.1413 
## F-statistic: 2.152 on 1 and 6 DF,  p-value: 0.1927
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Linoleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -8.122 -7.651 -2.392  3.094 21.390 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   14.908     22.646   0.658    0.535
## Linoleic      -2.999     12.475  -0.240    0.818
## 
## Residual standard error: 10.91 on 6 degrees of freedom
## Multiple R-squared:  0.009538,	Adjusted R-squared:  -0.1555 
## F-statistic: 0.05778 on 1 and 6 DF,  p-value: 0.818
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Linoleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -67.38 -54.86 -34.22  50.28 143.09 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -4.383    167.461  -0.026    0.980
## Linoleic      40.211     92.250   0.436    0.678
## 
## Residual standard error: 80.65 on 6 degrees of freedom
## Multiple R-squared:  0.03069,	Adjusted R-squared:  -0.1309 
## F-statistic:  0.19 on 1 and 6 DF,  p-value: 0.6782
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Linoleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -263.10 -114.90  -30.64   68.00  451.19 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   176.02     520.39   0.338    0.747
## Linoleic       57.02     286.67   0.199    0.849
## 
## Residual standard error: 250.6 on 6 degrees of freedom
## Multiple R-squared:  0.006552,	Adjusted R-squared:  -0.159 
## F-statistic: 0.03957 on 1 and 6 DF,  p-value: 0.8489
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Linoleic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -67.205 -51.336  -4.862  34.483 112.961 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  -137.16     138.45  -0.991    0.360
## Linoleic      110.75      76.27   1.452    0.197
## 
## Residual standard error: 66.68 on 6 degrees of freedom
## Multiple R-squared:   0.26,	Adjusted R-squared:  0.1367 
## F-statistic: 2.109 on 1 and 6 DF,  p-value: 0.1967
## 
## 
## 
## Summary for Linolelaidic :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Linolelaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.6149 -3.3547 -1.7475 -0.1413 15.5869 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)    -2.641     13.313  -0.198    0.849
## Linolelaidic   -3.478      6.610  -0.526    0.618
## 
## Residual standard error: 7.185 on 6 degrees of freedom
## Multiple R-squared:  0.04412,	Adjusted R-squared:  -0.1152 
## F-statistic: 0.2769 on 1 and 6 DF,  p-value: 0.6176
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Linolelaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
##  -9864  -6599  -4364  -1049  33108 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)      1229      28068   0.044    0.966
## Linolelaidic    -4087      13936  -0.293    0.779
## 
## Residual standard error: 15150 on 6 degrees of freedom
## Multiple R-squared:  0.01413,	Adjusted R-squared:  -0.1502 
## F-statistic: 0.08602 on 1 and 6 DF,  p-value: 0.7792
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Linolelaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -22.538 -14.388  -6.724  12.521  33.931 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)    48.000     41.394   1.160    0.290
## Linolelaidic    7.493     20.552   0.365    0.728
## 
## Residual standard error: 22.34 on 6 degrees of freedom
## Multiple R-squared:  0.02168,	Adjusted R-squared:  -0.1414 
## F-statistic: 0.1329 on 1 and 6 DF,  p-value: 0.7279
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Linolelaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -11.408  -7.685  -4.692   8.206  17.368 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)    13.774     21.685   0.635    0.549
## Linolelaidic    1.008     10.766   0.094    0.928
## 
## Residual standard error: 11.7 on 6 degrees of freedom
## Multiple R-squared:  0.001459,	Adjusted R-squared:  -0.165 
## F-statistic: 0.008766 on 1 and 6 DF,  p-value: 0.9285
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Linolelaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.27879 -0.16570 -0.07540  0.00023  0.70701 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)    0.6980     0.6134   1.138    0.299
## Linolelaidic   0.2809     0.3046   0.922    0.392
## 
## Residual standard error: 0.3311 on 6 degrees of freedom
## Multiple R-squared:  0.1242,	Adjusted R-squared:  -0.02176 
## F-statistic: 0.8509 on 1 and 6 DF,  p-value: 0.3919
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Linolelaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.9547 -2.4588 -0.3898  1.2512  6.4270 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)    10.455      6.945   1.505    0.183
## Linolelaidic    3.523      3.448   1.022    0.346
## 
## Residual standard error: 3.748 on 6 degrees of freedom
## Multiple R-squared:  0.1482,	Adjusted R-squared:  0.006221 
## F-statistic: 1.044 on 1 and 6 DF,  p-value: 0.3463
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Linolelaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.223 -7.314 -1.887  3.604 21.054 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)     4.339     20.189   0.215    0.837
## Linolelaidic   -2.632     10.024  -0.263    0.802
## 
## Residual standard error: 10.9 on 6 degrees of freedom
## Multiple R-squared:  0.01136,	Adjusted R-squared:  -0.1534 
## F-statistic: 0.06896 on 1 and 6 DF,  p-value: 0.8016
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Linolelaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -70.05 -52.39 -32.27  49.84 125.51 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)    181.38     144.21   1.258    0.255
## Linolelaidic    57.58      71.60   0.804    0.452
## 
## Residual standard error: 77.83 on 6 degrees of freedom
## Multiple R-squared:  0.0973,	Adjusted R-squared:  -0.05316 
## F-statistic: 0.6467 on 1 and 6 DF,  p-value: 0.452
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Linolelaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -258.12 -146.55  -16.21   82.45  464.96 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)    185.62     464.30   0.400    0.703
## Linolelaidic   -46.74     230.52  -0.203    0.846
## 
## Residual standard error: 250.6 on 6 degrees of freedom
## Multiple R-squared:  0.006804,	Adjusted R-squared:  -0.1587 
## F-statistic: 0.0411 on 1 and 6 DF,  p-value: 0.846
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Linolelaidic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -71.313 -55.743  -9.967  53.180 104.845 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)    210.28     129.49   1.624    0.156
## Linolelaidic    75.54      64.29   1.175    0.285
## 
## Residual standard error: 69.89 on 6 degrees of freedom
## Multiple R-squared:  0.187,	Adjusted R-squared:  0.05154 
## F-statistic:  1.38 on 1 and 6 DF,  p-value: 0.2846
## 
## 
## 
## Summary for gamma.Linolenic :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ gamma.Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.3363 -4.1955 -0.6732  3.2330  7.6987 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)  
## (Intercept)        49.33      20.44   2.413   0.0523 .
## gamma.Linolenic    36.72      16.57   2.216   0.0686 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.45 on 6 degrees of freedom
## Multiple R-squared:  0.4501,	Adjusted R-squared:  0.3584 
## F-statistic: 4.911 on 1 and 6 DF,  p-value: 0.06857
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ gamma.Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
##  -8401  -7401  -6137   -199  32821 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)        -8560      56754  -0.151    0.885
## gamma.Linolenic   -14551      46006  -0.316    0.763
## 
## Residual standard error: 15130 on 6 degrees of freedom
## Multiple R-squared:  0.0164,	Adjusted R-squared:  -0.1475 
## F-statistic:   0.1 on 1 and 6 DF,  p-value: 0.7625
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ gamma.Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -25.489 -14.328  -6.812  13.287  35.455 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)       41.912     84.642   0.495    0.638
## gamma.Linolenic    7.106     68.613   0.104    0.921
## 
## Residual standard error: 22.57 on 6 degrees of freedom
## Multiple R-squared:  0.001785,	Adjusted R-squared:  -0.1646 
## F-statistic: 0.01073 on 1 and 6 DF,  p-value: 0.9209
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ gamma.Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -11.350  -8.278  -4.632   9.094  16.960 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)        2.199     43.753    0.05    0.962
## gamma.Linolenic   -7.802     35.468   -0.22    0.833
## 
## Residual standard error: 11.67 on 6 degrees of freedom
## Multiple R-squared:  0.008,	Adjusted R-squared:  -0.1573 
## F-statistic: 0.04839 on 1 and 6 DF,  p-value: 0.8332
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ gamma.Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.38025 -0.14136 -0.03202  0.01709  0.66876 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)       1.4046     1.2218   1.150    0.294
## gamma.Linolenic   1.0277     0.9904   1.038    0.339
## 
## Residual standard error: 0.3258 on 6 degrees of freedom
## Multiple R-squared:  0.1521,	Adjusted R-squared:  0.01083 
## F-statistic: 1.077 on 1 and 6 DF,  p-value: 0.3394
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ gamma.Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -3.606 -2.305 -1.571  1.336  6.659 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)       2.4986    15.2279   0.164    0.875
## gamma.Linolenic  -0.8072    12.3442  -0.065    0.950
## 
## Residual standard error: 4.06 on 6 degrees of freedom
## Multiple R-squared:  0.0007122,	Adjusted R-squared:  -0.1658 
## F-statistic: 0.004276 on 1 and 6 DF,  p-value: 0.95
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ gamma.Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.941 -7.857 -1.726  4.588 19.569 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)       15.792     41.024   0.385    0.714
## gamma.Linolenic    5.087     33.255   0.153    0.883
## 
## Residual standard error: 10.94 on 6 degrees of freedom
## Multiple R-squared:  0.003885,	Adjusted R-squared:  -0.1621 
## F-statistic: 0.0234 on 1 and 6 DF,  p-value: 0.8834
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ gamma.Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -91.823 -47.525  -1.652  26.153 122.483 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)       -284.3      271.3  -1.048    0.335
## gamma.Linolenic   -286.5      219.9  -1.303    0.240
## 
## Residual standard error: 72.32 on 6 degrees of freedom
## Multiple R-squared:  0.2205,	Adjusted R-squared:  0.09064 
## F-statistic: 1.698 on 1 and 6 DF,  p-value: 0.2404
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ gamma.Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -272.64 -127.27  -52.40   81.93  461.26 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)       -393.1      902.1  -0.436    0.678
## gamma.Linolenic   -546.4      731.2  -0.747    0.483
## 
## Residual standard error: 240.5 on 6 degrees of freedom
## Multiple R-squared:  0.08514,	Adjusted R-squared:  -0.06733 
## F-statistic: 0.5584 on 1 and 6 DF,  p-value: 0.4832
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ gamma.Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -60.80 -47.09 -29.77  17.14 139.85 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)        42.25     290.63   0.145    0.889
## gamma.Linolenic   -15.22     235.59  -0.065    0.951
## 
## Residual standard error: 77.49 on 6 degrees of freedom
## Multiple R-squared:  0.0006948,	Adjusted R-squared:  -0.1659 
## F-statistic: 0.004172 on 1 and 6 DF,  p-value: 0.9506
## 
## 
## 
## Summary for Linolenic :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.6195 -3.5314 -2.1910  0.8436 14.4625 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    7.620      4.666   1.633    0.154
## Linolenic     -8.700     10.202  -0.853    0.427
## 
## Residual standard error: 6.941 on 6 degrees of freedom
## Multiple R-squared:  0.1081,	Adjusted R-squared:  -0.04056 
## F-statistic: 0.7271 on 1 and 6 DF,  p-value: 0.4265
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -12905  -7489  -3006   2191  29822 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    16688       9626   1.734    0.134
## Linolenic     -18967      21047  -0.901    0.402
## 
## Residual standard error: 14320 on 6 degrees of freedom
## Multiple R-squared:  0.1192,	Adjusted R-squared:  -0.02759 
## F-statistic: 0.8121 on 1 and 6 DF,  p-value: 0.4022
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -25.242 -12.807  -7.389  13.824  35.110 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   35.915     15.128   2.374   0.0552 .
## Linolenic     -7.019     33.077  -0.212   0.8390  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 22.5 on 6 degrees of freedom
## Multiple R-squared:  0.007449,	Adjusted R-squared:  -0.158 
## F-statistic: 0.04503 on 1 and 6 DF,  p-value: 0.839
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -11.000  -8.651  -1.907   6.464  17.124 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     5.74       7.32   0.784    0.463
## Linolenic      15.53      16.01   0.970    0.369
## 
## Residual standard error: 10.89 on 6 degrees of freedom
## Multiple R-squared:  0.1356,	Adjusted R-squared:  -0.008409 
## F-statistic: 0.9416 on 1 and 6 DF,  p-value: 0.3693
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.21505 -0.19711 -0.09857  0.03640  0.73334 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   0.2843     0.2279   1.248    0.259
## Linolenic    -0.3646     0.4983  -0.732    0.492
## 
## Residual standard error: 0.339 on 6 degrees of freedom
## Multiple R-squared:  0.08192,	Adjusted R-squared:  -0.07109 
## F-statistic: 0.5354 on 1 and 6 DF,  p-value: 0.4919
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.344 -1.970 -1.401  1.299  7.201 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    2.166      2.655   0.816    0.446
## Linolenic      3.403      5.806   0.586    0.579
## 
## Residual standard error: 3.95 on 6 degrees of freedom
## Multiple R-squared:  0.05416,	Adjusted R-squared:  -0.1035 
## F-statistic: 0.3436 on 1 and 6 DF,  p-value: 0.5791
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.7335  -8.1913   0.4262   3.8924  18.4642 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   13.394      7.132   1.878    0.109
## Linolenic     -9.898     15.593  -0.635    0.549
## 
## Residual standard error: 10.61 on 6 degrees of freedom
## Multiple R-squared:  0.06293,	Adjusted R-squared:  -0.09325 
## F-statistic: 0.4029 on 1 and 6 DF,  p-value: 0.549
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -96.27 -26.45 -15.01  56.38  76.17 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -3.101     43.393  -0.071    0.945
## Linolenic    181.610     94.878   1.914    0.104
## 
## Residual standard error: 64.55 on 6 degrees of freedom
## Multiple R-squared:  0.3791,	Adjusted R-squared:  0.2757 
## F-statistic: 3.664 on 1 and 6 DF,  p-value: 0.1041
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -272.59 -136.44  -22.55  110.27  443.79 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    322.3      167.7   1.922    0.103
## Linolenic     -113.7      366.7  -0.310    0.767
## 
## Residual standard error: 249.5 on 6 degrees of freedom
## Multiple R-squared:  0.01578,	Adjusted R-squared:  -0.1483 
## F-statistic: 0.09621 on 1 and 6 DF,  p-value: 0.7669
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Linolenic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -88.42 -39.72 -14.93  39.04  98.54 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    14.73      47.15   0.312    0.765
## Linolenic     118.78     103.10   1.152    0.293
## 
## Residual standard error: 70.14 on 6 degrees of freedom
## Multiple R-squared:  0.1811,	Adjusted R-squared:  0.04467 
## F-statistic: 1.327 on 1 and 6 DF,  p-value: 0.2931
## 
## 
## 
## Summary for Eicosanoic :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Eicosanoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.6063 -1.9310 -0.4486  1.2959 12.1092 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -21.45      20.33  -1.055    0.332
## Eicosanoic    -13.49      10.60  -1.272    0.250
## 
## Residual standard error: 6.523 on 6 degrees of freedom
## Multiple R-squared:  0.2124,	Adjusted R-squared:  0.08108 
## F-statistic: 1.618 on 1 and 6 DF,  p-value: 0.2505
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Eicosanoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -11960  -7805  -3616   1674  30590 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    46624      45010   1.036    0.340
## Eicosanoic     19593      23481   0.834    0.436
## 
## Residual standard error: 14440 on 6 degrees of freedom
## Multiple R-squared:  0.104,	Adjusted R-squared:  -0.04536 
## F-statistic: 0.6962 on 1 and 6 DF,  p-value: 0.436
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Eicosanoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -23.54 -10.90   2.17  10.51  21.36 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   150.31      51.37   2.926   0.0264 *
## Eicosanoic     61.50      26.80   2.295   0.0615 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 16.48 on 6 degrees of freedom
## Multiple R-squared:  0.4674,	Adjusted R-squared:  0.3787 
## F-statistic: 5.266 on 1 and 6 DF,  p-value: 0.06154
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Eicosanoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -11.379  -7.671  -5.015   8.544  16.907 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  12.3999    36.5009   0.340    0.746
## Eicosanoic    0.3249    19.0424   0.017    0.987
## 
## Residual standard error: 11.71 on 6 degrees of freedom
## Multiple R-squared:  4.851e-05,	Adjusted R-squared:  -0.1666 
## F-statistic: 0.0002911 on 1 and 6 DF,  p-value: 0.9869
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Eicosanoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.26277 -0.20120 -0.06397  0.06465  0.64295 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   1.3502     0.9846   1.371    0.219
## Eicosanoic    0.6341     0.5136   1.235    0.263
## 
## Residual standard error: 0.3159 on 6 degrees of freedom
## Multiple R-squared:  0.2026,	Adjusted R-squared:  0.06967 
## F-statistic: 1.524 on 1 and 6 DF,  p-value: 0.2631
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Eicosanoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.6187 -1.9080 -0.8096  0.1038  7.4091 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -8.949     11.580  -0.773    0.469
## Eicosanoic    -6.531      6.041  -1.081    0.321
## 
## Residual standard error: 3.716 on 6 degrees of freedom
## Multiple R-squared:  0.1631,	Adjusted R-squared:  0.02356 
## F-statistic: 1.169 on 1 and 6 DF,  p-value: 0.3212
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Eicosanoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -11.750  -3.790   0.690   3.843  13.542 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)    61.62      26.62   2.315   0.0599 .
## Eicosanoic     27.34      13.89   1.969   0.0965 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.542 on 6 degrees of freedom
## Multiple R-squared:  0.3925,	Adjusted R-squared:  0.2912 
## F-statistic: 3.876 on 1 and 6 DF,  p-value: 0.09651
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Eicosanoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -101.01  -33.55  -12.61   59.48   84.29 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -263.2      216.1  -1.218    0.269
## Eicosanoic    -173.7      112.7  -1.541    0.174
## 
## Residual standard error: 69.34 on 6 degrees of freedom
## Multiple R-squared:  0.2834,	Adjusted R-squared:  0.164 
## F-statistic: 2.373 on 1 and 6 DF,  p-value: 0.1744
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Eicosanoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -269.83 -151.92  -57.11  121.96  430.85 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    793.1      754.5   1.051    0.334
## Eicosanoic     270.5      393.6   0.687    0.518
## 
## Residual standard error: 242.1 on 6 degrees of freedom
## Multiple R-squared:  0.07294,	Adjusted R-squared:  -0.08157 
## F-statistic: 0.472 on 1 and 6 DF,  p-value: 0.5177
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Eicosanoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -60.85 -48.05 -24.48  33.49 105.37 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -155.8      224.6  -0.694    0.514
## Eicosanoic    -113.8      117.1  -0.972    0.369
## 
## Residual standard error: 72.05 on 6 degrees of freedom
## Multiple R-squared:  0.136,	Adjusted R-squared:  -0.008048 
## F-statistic: 0.9441 on 1 and 6 DF,  p-value: 0.3687
## 
## 
## 
## Summary for cis.11.Eicosenoic :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ cis.11.Eicosenoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.4581 -3.1764 -1.7149 -0.1318 15.2465 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)
## (Intercept)          20.30      24.61   0.825    0.441
## cis.11.Eicosenoic    13.21      20.13   0.656    0.536
## 
## Residual standard error: 7.099 on 6 degrees of freedom
## Multiple R-squared:  0.06698,	Adjusted R-squared:  -0.08853 
## F-statistic: 0.4307 on 1 and 6 DF,  p-value: 0.536
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ cis.11.Eicosenoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
##  -9966  -7471  -5557   -437  31760 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)
## (Intercept)          23481      52571   0.447    0.671
## cis.11.Eicosenoic    11653      43002   0.271    0.795
## 
## Residual standard error: 15160 on 6 degrees of freedom
## Multiple R-squared:  0.01209,	Adjusted R-squared:  -0.1526 
## F-statistic: 0.07343 on 1 and 6 DF,  p-value: 0.7955
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ cis.11.Eicosenoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -26.912  -8.089  -6.936   4.830  37.865 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)
## (Intercept)          85.21      75.33   1.131    0.301
## cis.11.Eicosenoic    42.78      61.62   0.694    0.514
## 
## Residual standard error: 21.73 on 6 degrees of freedom
## Multiple R-squared:  0.07435,	Adjusted R-squared:  -0.07992 
## F-statistic: 0.4819 on 1 and 6 DF,  p-value: 0.5135
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ cis.11.Eicosenoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -10.490  -7.825  -3.676   7.175  17.971 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)
## (Intercept)         -18.11      38.70  -0.468    0.656
## cis.11.Eicosenoic   -24.58      31.66  -0.776    0.467
## 
## Residual standard error: 11.16 on 6 degrees of freedom
## Multiple R-squared:  0.09131,	Adjusted R-squared:  -0.06014 
## F-statistic: 0.6029 on 1 and 6 DF,  p-value: 0.467
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ cis.11.Eicosenoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.19523 -0.14174 -0.10659 -0.03578  0.78195 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)
## (Intercept)        -0.3895     1.2068  -0.323    0.758
## cis.11.Eicosenoic  -0.4375     0.9872  -0.443    0.673
## 
## Residual standard error: 0.3481 on 6 degrees of freedom
## Multiple R-squared:  0.03169,	Adjusted R-squared:  -0.1297 
## F-statistic: 0.1964 on 1 and 6 DF,  p-value: 0.6732
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ cis.11.Eicosenoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -3.175 -2.466 -1.595  1.250  6.789 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)
## (Intercept)          1.796     14.062   0.128    0.903
## cis.11.Eicosenoic   -1.393     11.503  -0.121    0.908
## 
## Residual standard error: 4.057 on 6 degrees of freedom
## Multiple R-squared:  0.002437,	Adjusted R-squared:  -0.1638 
## F-statistic: 0.01466 on 1 and 6 DF,  p-value: 0.9076
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ cis.11.Eicosenoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -8.364 -8.134 -2.226  4.517 20.008 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)
## (Intercept)          2.759     37.888   0.073    0.944
## cis.11.Eicosenoic   -5.579     30.993  -0.180    0.863
## 
## Residual standard error: 10.93 on 6 degrees of freedom
## Multiple R-squared:  0.005372,	Adjusted R-squared:  -0.1604 
## F-statistic: 0.03241 on 1 and 6 DF,  p-value: 0.8631
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ cis.11.Eicosenoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -65.67 -34.23 -14.80  26.27 105.40 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)  
## (Intercept)         -417.1      202.7  -2.058   0.0853 .
## cis.11.Eicosenoic   -398.5      165.8  -2.404   0.0530 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 58.47 on 6 degrees of freedom
## Multiple R-squared:  0.4905,	Adjusted R-squared:  0.4056 
## F-statistic: 5.777 on 1 and 6 DF,  p-value: 0.05304
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ cis.11.Eicosenoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -260.55 -133.09  -25.17   71.69  463.46 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)
## (Intercept)         257.04     871.65   0.295    0.778
## cis.11.Eicosenoic   -17.25     713.00  -0.024    0.981
## 
## Residual standard error: 251.4 on 6 degrees of freedom
## Multiple R-squared:  9.76e-05,	Adjusted R-squared:  -0.1666 
## F-statistic: 0.0005856 on 1 and 6 DF,  p-value: 0.9815
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ cis.11.Eicosenoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -74.77 -32.92 -12.52  21.78 106.73 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)
## (Intercept)         -289.8      226.9  -1.277    0.249
## cis.11.Eicosenoic   -288.4      185.6  -1.554    0.171
## 
## Residual standard error: 65.46 on 6 degrees of freedom
## Multiple R-squared:  0.2869,	Adjusted R-squared:  0.168 
## F-statistic: 2.413 on 1 and 6 DF,  p-value: 0.1713
## 
## 
## 
## Summary for cis.8.11.14.Eicosatrienoic :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ cis.8.11.14.Eicosatrienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.5603 -3.7146 -1.3580  0.0658 15.1602 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)
## (Intercept)                  -2.019     13.642  -0.148    0.887
## cis.8.11.14.Eicosatrienoic   -4.515      9.673  -0.467    0.657
## 
## Residual standard error: 7.22 on 6 degrees of freedom
## Multiple R-squared:  0.03504,	Adjusted R-squared:  -0.1258 
## F-statistic: 0.2179 on 1 and 6 DF,  p-value: 0.6571
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ cis.8.11.14.Eicosatrienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8753.8 -7516.1 -6425.4   309.3 31219.1 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)
## (Intercept)                   25424      28041   0.907     0.40
## cis.8.11.14.Eicosatrienoic    11631      19884   0.585     0.58
## 
## Residual standard error: 14840 on 6 degrees of freedom
## Multiple R-squared:  0.05395,	Adjusted R-squared:  -0.1037 
## F-statistic: 0.3422 on 1 and 6 DF,  p-value: 0.5799
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ cis.8.11.14.Eicosatrienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -21.700 -14.394  -8.067  11.108  34.293 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)
## (Intercept)                  46.169     42.338   1.090    0.317
## cis.8.11.14.Eicosatrienoic    9.372     30.022   0.312    0.765
## 
## Residual standard error: 22.41 on 6 degrees of freedom
## Multiple R-squared:  0.01598,	Adjusted R-squared:  -0.148 
## F-statistic: 0.09746 on 1 and 6 DF,  p-value: 0.7655
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ cis.8.11.14.Eicosatrienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -11.510  -7.021  -3.001   8.789  13.909 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)
## (Intercept)                  -12.05      19.79  -0.609    0.565
## cis.8.11.14.Eicosatrienoic   -17.21      14.03  -1.226    0.266
## 
## Residual standard error: 10.47 on 6 degrees of freedom
## Multiple R-squared:  0.2003,	Adjusted R-squared:  0.06705 
## F-statistic: 1.503 on 1 and 6 DF,  p-value: 0.2661
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ cis.8.11.14.Eicosatrienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.28971 -0.16952 -0.05450  0.00361  0.71097 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)
## (Intercept)                  0.6900     0.6286   1.098    0.314
## cis.8.11.14.Eicosatrienoic   0.3952     0.4457   0.887    0.409
## 
## Residual standard error: 0.3327 on 6 degrees of freedom
## Multiple R-squared:  0.1158,	Adjusted R-squared:  -0.03153 
## F-statistic: 0.7861 on 1 and 6 DF,  p-value: 0.4094
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ cis.8.11.14.Eicosatrienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -3.942 -2.522 -1.313  1.412  6.491 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)
## (Intercept)                   5.173      7.643   0.677    0.524
## cis.8.11.14.Eicosatrienoic    1.215      5.419   0.224    0.830
## 
## Residual standard error: 4.045 on 6 degrees of freedom
## Multiple R-squared:  0.008301,	Adjusted R-squared:  -0.157 
## F-statistic: 0.05022 on 1 and 6 DF,  p-value: 0.8301
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ cis.8.11.14.Eicosatrienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.362 -7.729 -2.951  5.944 19.029 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)
## (Intercept)                  16.994     20.475    0.83    0.438
## cis.8.11.14.Eicosatrienoic    5.378     14.519    0.37    0.724
## 
## Residual standard error: 10.84 on 6 degrees of freedom
## Multiple R-squared:  0.02236,	Adjusted R-squared:  -0.1406 
## F-statistic: 0.1372 on 1 and 6 DF,  p-value: 0.7238
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ cis.8.11.14.Eicosatrienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -56.03 -50.12 -43.36  41.60 152.80 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)
## (Intercept)                   87.43     154.57   0.566    0.592
## cis.8.11.14.Eicosatrienoic    14.35     109.61   0.131    0.900
## 
## Residual standard error: 81.8 on 6 degrees of freedom
## Multiple R-squared:  0.00285,	Adjusted R-squared:  -0.1633 
## F-statistic: 0.01715 on 1 and 6 DF,  p-value: 0.9001
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ cis.8.11.14.Eicosatrienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -279.12 -148.59  -17.08   74.33  436.11 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)
## (Intercept)                   534.5      463.0   1.154    0.292
## cis.8.11.14.Eicosatrienoic    185.1      328.3   0.564    0.593
## 
## Residual standard error: 245 on 6 degrees of freedom
## Multiple R-squared:  0.05032,	Adjusted R-squared:  -0.108 
## F-statistic: 0.3179 on 1 and 6 DF,  p-value: 0.5933
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ cis.8.11.14.Eicosatrienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -68.75 -50.56 -21.41  18.93 140.27 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)
## (Intercept)                   98.64     145.63   0.677    0.523
## cis.8.11.14.Eicosatrienoic    27.22     103.26   0.264    0.801
## 
## Residual standard error: 77.07 on 6 degrees of freedom
## Multiple R-squared:  0.01144,	Adjusted R-squared:  -0.1533 
## F-statistic: 0.06946 on 1 and 6 DF,  p-value: 0.8009
## 
## 
## 
## Summary for Arachidonic :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Arachidonic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.8433 -3.9511 -2.0216  0.1041 15.9255 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    2.923      6.760   0.432    0.681
## Arachidonic   22.589    107.442   0.210    0.840
## 
## Residual standard error: 7.323 on 6 degrees of freedom
## Multiple R-squared:  0.007313,	Adjusted R-squared:  -0.1581 
## F-statistic: 0.0442 on 1 and 6 DF,  p-value: 0.8404
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Arachidonic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
##  -9156  -7167  -5525  -1346  32829 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     9184      14086   0.652    0.539
## Arachidonic     2169     223864   0.010    0.993
## 
## Residual standard error: 15260 on 6 degrees of freedom
## Multiple R-squared:  1.565e-05,	Adjusted R-squared:  -0.1666 
## F-statistic: 9.389e-05 on 1 and 6 DF,  p-value: 0.9926
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Arachidonic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -29.7197  -7.0653  -0.9066  10.7395  23.1387 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)     4.43      16.53   0.268    0.798
## Arachidonic   494.71     262.77   1.883    0.109
## 
## Residual standard error: 17.91 on 6 degrees of freedom
## Multiple R-squared:  0.3714,	Adjusted R-squared:  0.2666 
## F-statistic: 3.544 on 1 and 6 DF,  p-value: 0.1088
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Arachidonic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.042  -4.802  -1.536   2.632  16.418 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    1.266      9.764   0.130    0.901
## Arachidonic  180.902    155.174   1.166    0.288
## 
## Residual standard error: 10.58 on 6 degrees of freedom
## Multiple R-squared:  0.1847,	Adjusted R-squared:  0.0488 
## F-statistic: 1.359 on 1 and 6 DF,  p-value: 0.2879
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Arachidonic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.34083 -0.18586 -0.02326  0.10755  0.48848 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  -0.2864     0.2660  -1.077    0.323
## Arachidonic   7.3796     4.2272   1.746    0.131
## 
## Residual standard error: 0.2881 on 6 degrees of freedom
## Multiple R-squared:  0.3368,	Adjusted R-squared:  0.2263 
## F-statistic: 3.048 on 1 and 6 DF,  p-value: 0.1315
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Arachidonic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -3.831 -2.015 -1.568  1.270  6.707 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    4.194      3.737   1.122    0.305
## Arachidonic  -12.110     59.387  -0.204    0.845
## 
## Residual standard error: 4.047 on 6 degrees of freedom
## Multiple R-squared:  0.006883,	Adjusted R-squared:  -0.1586 
## F-statistic: 0.04158 on 1 and 6 DF,  p-value: 0.8452
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Arachidonic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -11.283  -4.788  -2.196   8.074  11.323 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -2.787      8.525  -0.327    0.755
## Arachidonic  212.138    135.482   1.566    0.168
## 
## Residual standard error: 9.234 on 6 degrees of freedom
## Multiple R-squared:  0.2901,	Adjusted R-squared:  0.1718 
## F-statistic: 2.452 on 1 and 6 DF,  p-value: 0.1684
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Arachidonic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -96.527 -31.869   3.576  17.082 123.903 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   162.28      62.98   2.577    0.042 *
## Arachidonic -1629.93    1000.98  -1.628    0.155  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 68.22 on 6 degrees of freedom
## Multiple R-squared:  0.3065,	Adjusted R-squared:  0.1909 
## F-statistic: 2.651 on 1 and 6 DF,  p-value: 0.1546
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Arachidonic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -257.92 -144.98  -31.42   84.85  459.27 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    239.1      231.5   1.033    0.342
## Arachidonic    670.0     3679.3   0.182    0.862
## 
## Residual standard error: 250.8 on 6 degrees of freedom
## Multiple R-squared:  0.005496,	Adjusted R-squared:  -0.1603 
## F-statistic: 0.03316 on 1 and 6 DF,  p-value: 0.8615
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Arachidonic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -50.09 -45.88 -33.30  16.16 150.22 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    32.93      70.49   0.467    0.657
## Arachidonic   481.87    1120.20   0.430    0.682
## 
## Residual standard error: 76.35 on 6 degrees of freedom
## Multiple R-squared:  0.02992,	Adjusted R-squared:  -0.1318 
## F-statistic: 0.185 on 1 and 6 DF,  p-value: 0.6821
## 
## 
## 
## Summary for Erucic.acid :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Erucic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.6131 -4.0640 -0.2386  2.2614 10.7295 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  -15.614     11.824  -1.321    0.235
## Erucic.acid   -3.013      1.765  -1.707    0.139
## 
## Residual standard error: 6.03 on 6 degrees of freedom
## Multiple R-squared:  0.3268,	Adjusted R-squared:  0.2146 
## F-statistic: 2.913 on 1 and 6 DF,  p-value: 0.1387
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Erucic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -10658  -7187  -4892   -960  33014 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   3758.8    29828.6   0.126    0.904
## Erucic.acid   -842.5     4453.0  -0.189    0.856
## 
## Residual standard error: 15210 on 6 degrees of freedom
## Multiple R-squared:  0.005931,	Adjusted R-squared:  -0.1597 
## F-statistic: 0.0358 on 1 and 6 DF,  p-value: 0.8562
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Erucic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -20.084  -6.770  -1.576   8.039  21.240 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)   
## (Intercept)  117.526     27.134   4.331  0.00492 **
## Erucic.acid   12.801      4.051   3.160  0.01956 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.84 on 6 degrees of freedom
## Multiple R-squared:  0.6247,	Adjusted R-squared:  0.5621 
## F-statistic: 9.986 on 1 and 6 DF,  p-value: 0.01956
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Erucic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.8388  -9.4549  -0.7033   6.7093  14.1695 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   27.411     22.031   1.244    0.260
## Erucic.acid    2.372      3.289   0.721    0.498
## 
## Residual standard error: 11.24 on 6 degrees of freedom
## Multiple R-squared:  0.07979,	Adjusted R-squared:  -0.07358 
## F-statistic: 0.5202 on 1 and 6 DF,  p-value: 0.4979
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Erucic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.26114 -0.18138 -0.07408  0.05213  0.67886 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  0.80492    0.63691   1.264    0.253
## Erucic.acid  0.10054    0.09508   1.057    0.331
## 
## Residual standard error: 0.3248 on 6 degrees of freedom
## Multiple R-squared:  0.1571,	Adjusted R-squared:  0.01659 
## F-statistic: 1.118 on 1 and 6 DF,  p-value: 0.331
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Erucic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.4410 -2.1626 -1.2258  0.7066  6.7334 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   1.1850     7.9063   0.150    0.886
## Erucic.acid  -0.3498     1.1803  -0.296    0.777
## 
## Residual standard error: 4.032 on 6 degrees of freedom
## Multiple R-squared:  0.01443,	Adjusted R-squared:  -0.1498 
## F-statistic: 0.08785 on 1 and 6 DF,  p-value: 0.7769
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Erucic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.668 -6.810 -2.005  3.445 17.081 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   26.991     20.232   1.334    0.231
## Erucic.acid    2.648      3.020   0.877    0.414
## 
## Residual standard error: 10.32 on 6 degrees of freedom
## Multiple R-squared:  0.1136,	Adjusted R-squared:  -0.03418 
## F-statistic: 0.7686 on 1 and 6 DF,  p-value: 0.4144
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Erucic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -75.66 -38.54 -32.52  20.65 147.43 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -31.44     155.29  -0.202    0.846
## Erucic.acid   -15.02      23.18  -0.648    0.541
## 
## Residual standard error: 79.19 on 6 degrees of freedom
## Multiple R-squared:  0.06542,	Adjusted R-squared:  -0.09035 
## F-statistic:  0.42 on 1 and 6 DF,  p-value: 0.5409
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Erucic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -267.24 -126.01  -16.82   66.41  463.87 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  215.240    492.376   0.437    0.677
## Erucic.acid   -9.528     73.504  -0.130    0.901
## 
## Residual standard error: 251.1 on 6 degrees of freedom
## Multiple R-squared:  0.002793,	Adjusted R-squared:  -0.1634 
## F-statistic: 0.0168 on 1 and 6 DF,  p-value: 0.9011
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Erucic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -54.22 -45.13 -33.04  11.76 139.57 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   29.389    151.430   0.194    0.853
## Erucic.acid   -4.789     22.606  -0.212    0.839
## 
## Residual standard error: 77.23 on 6 degrees of freedom
## Multiple R-squared:  0.007423,	Adjusted R-squared:  -0.158 
## F-statistic: 0.04487 on 1 and 6 DF,  p-value: 0.8393
## 
## 
## 
## Summary for cis.13.16.Docosadienoic :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ cis.13.16.Docosadienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.2784 -4.0691 -1.3891 -0.6418 16.2067 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)
## (Intercept)               3.9089     2.8287   1.382    0.216
## cis.13.16.Docosadienoic  -0.8351     2.9515  -0.283    0.787
## 
## Residual standard error: 7.301 on 6 degrees of freedom
## Multiple R-squared:  0.01317,	Adjusted R-squared:  -0.1513 
## F-statistic: 0.08006 on 1 and 6 DF,  p-value: 0.7867
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ cis.13.16.Docosadienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
##  -9076  -7549  -4315   -200  32660 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)
## (Intercept)                 7937       5750   1.380    0.217
## cis.13.16.Docosadienoic    -3502       6000  -0.584    0.581
## 
## Residual standard error: 14840 on 6 degrees of freedom
## Multiple R-squared:  0.05374,	Adjusted R-squared:  -0.104 
## F-statistic: 0.3407 on 1 and 6 DF,  p-value: 0.5807
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ cis.13.16.Docosadienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -23.935 -15.969  -4.874  16.843  31.146 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)   
## (Intercept)               35.456      8.453   4.195  0.00572 **
## cis.13.16.Docosadienoic    5.793      8.820   0.657  0.53566   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 21.82 on 6 degrees of freedom
## Multiple R-squared:  0.06708,	Adjusted R-squared:  -0.08841 
## F-statistic: 0.4314 on 1 and 6 DF,  p-value: 0.5357
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ cis.13.16.Docosadienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -11.244  -8.437  -2.553   7.267  16.465 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)  
## (Intercept)               12.536      4.475   2.802   0.0311 *
## cis.13.16.Docosadienoic    1.926      4.669   0.413   0.6943  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.55 on 6 degrees of freedom
## Multiple R-squared:  0.02758,	Adjusted R-squared:  -0.1345 
## F-statistic: 0.1702 on 1 and 6 DF,  p-value: 0.6943
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ cis.13.16.Docosadienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.20949 -0.15557 -0.13954  0.06432  0.64533 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)
## (Intercept)               0.2090     0.1199   1.742    0.132
## cis.13.16.Docosadienoic   0.1696     0.1252   1.355    0.224
## 
## Residual standard error: 0.3096 on 6 degrees of freedom
## Multiple R-squared:  0.2343,	Adjusted R-squared:  0.1067 
## F-statistic: 1.836 on 1 and 6 DF,  p-value: 0.2242
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ cis.13.16.Docosadienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -2.882 -2.319 -1.899  1.209  6.688 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)  
## (Intercept)               3.7103     1.5581   2.381   0.0547 .
## cis.13.16.Docosadienoic   0.5621     1.6258   0.346   0.7413  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.022 on 6 degrees of freedom
## Multiple R-squared:  0.01953,	Adjusted R-squared:  -0.1439 
## F-statistic: 0.1195 on 1 and 6 DF,  p-value: 0.7413
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ cis.13.16.Docosadienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -8.755 -7.467 -2.067  3.586 20.701 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)  
## (Intercept)               9.3277     4.2405   2.200   0.0701 .
## cis.13.16.Docosadienoic  -0.5511     4.4246  -0.125   0.9049  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.94 on 6 degrees of freedom
## Multiple R-squared:  0.002579,	Adjusted R-squared:  -0.1637 
## F-statistic: 0.01552 on 1 and 6 DF,  p-value: 0.9049
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ cis.13.16.Docosadienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -56.02 -48.95 -45.80  44.22 153.37 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)  
## (Intercept)               69.517     31.678   2.195   0.0706 .
## cis.13.16.Docosadienoic    5.032     33.053   0.152   0.8840  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 81.76 on 6 degrees of freedom
## Multiple R-squared:  0.003848,	Adjusted R-squared:  -0.1622 
## F-statistic: 0.02317 on 1 and 6 DF,  p-value: 0.884
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ cis.13.16.Docosadienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -248.55 -138.70  -28.82   60.57  457.29 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)  
## (Intercept)               242.47      90.73   2.672   0.0369 *
## cis.13.16.Docosadienoic   -90.70      94.67  -0.958   0.3750  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 234.2 on 6 degrees of freedom
## Multiple R-squared:  0.1327,	Adjusted R-squared:  -0.01187 
## F-statistic: 0.9179 on 1 and 6 DF,  p-value: 0.375
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ cis.13.16.Docosadienoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -62.98 -46.31 -32.96  26.52 141.33 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)  
## (Intercept)                65.00      29.76   2.184   0.0716 .
## cis.13.16.Docosadienoic    10.35      31.05   0.333   0.7502  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 76.81 on 6 degrees of freedom
## Multiple R-squared:  0.01819,	Adjusted R-squared:  -0.1454 
## F-statistic: 0.1111 on 1 and 6 DF,  p-value: 0.7502
## 
## 
## 
## Summary for cis.4.7.10.13.16.19.Docosahexaenoic :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ cis.4.7.10.13.16.19.Docosahexaenoic, 
##     data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.7909 -2.7315 -0.7182 -0.1067 14.2218 
## 
## Coefficients:
##                                     Estimate Std. Error t value Pr(>|t|)
## (Intercept)                          12.8942     8.1326   1.585    0.164
## cis.4.7.10.13.16.19.Docosahexaenoic   0.7003     0.6294   1.113    0.308
## 
## Residual standard error: 6.691 on 6 degrees of freedom
## Multiple R-squared:  0.1711,	Adjusted R-squared:  0.0329 
## F-statistic: 1.238 on 1 and 6 DF,  p-value: 0.3084
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ cis.4.7.10.13.16.19.Docosahexaenoic, 
##     data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -10323  -6654  -5027  -1200  32222 
## 
## Coefficients:
##                                     Estimate Std. Error t value Pr(>|t|)
## (Intercept)                          14544.4    18408.3   0.790    0.460
## cis.4.7.10.13.16.19.Docosahexaenoic    423.4     1424.6   0.297    0.776
## 
## Residual standard error: 15150 on 6 degrees of freedom
## Multiple R-squared:  0.01451,	Adjusted R-squared:  -0.1497 
## F-statistic: 0.08833 on 1 and 6 DF,  p-value: 0.7763
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ cis.4.7.10.13.16.19.Docosahexaenoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -20.96 -17.83  -3.39  13.24  36.57 
## 
## Coefficients:
##                                     Estimate Std. Error t value Pr(>|t|)
## (Intercept)                           20.536     26.916   0.763    0.474
## cis.4.7.10.13.16.19.Docosahexaenoic   -1.023      2.083  -0.491    0.641
## 
## Residual standard error: 22.15 on 6 degrees of freedom
## Multiple R-squared:  0.03865,	Adjusted R-squared:  -0.1216 
## F-statistic: 0.2413 on 1 and 6 DF,  p-value: 0.6408
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ cis.4.7.10.13.16.19.Docosahexaenoic, 
##     data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.9162 -2.2764 -0.3073  3.8896  7.5688 
## 
## Coefficients:
##                                     Estimate Std. Error t value Pr(>|t|)   
## (Intercept)                          -17.365      6.926  -2.507  0.04607 * 
## cis.4.7.10.13.16.19.Docosahexaenoic   -2.358      0.536  -4.399  0.00458 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.698 on 6 degrees of freedom
## Multiple R-squared:  0.7633,	Adjusted R-squared:  0.7238 
## F-statistic: 19.35 on 1 and 6 DF,  p-value: 0.004575
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ cis.4.7.10.13.16.19.Docosahexaenoic, 
##     data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.16953 -0.15972 -0.09349 -0.07242  0.79538 
## 
## Coefficients:
##                                     Estimate Std. Error t value Pr(>|t|)
## (Intercept)                         0.261173   0.426983   0.612    0.563
## cis.4.7.10.13.16.19.Docosahexaenoic 0.009599   0.033044   0.290    0.781
## 
## Residual standard error: 0.3513 on 6 degrees of freedom
## Multiple R-squared:  0.01387,	Adjusted R-squared:  -0.1505 
## F-statistic: 0.08439 on 1 and 6 DF,  p-value: 0.7812
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ cis.4.7.10.13.16.19.Docosahexaenoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -3.012 -2.986 -1.187  1.337  6.907 
## 
## Coefficients:
##                                     Estimate Std. Error t value Pr(>|t|)
## (Intercept)                           1.3918     4.8544   0.287    0.784
## cis.4.7.10.13.16.19.Docosahexaenoic  -0.1697     0.3757  -0.452    0.667
## 
## Residual standard error: 3.994 on 6 degrees of freedom
## Multiple R-squared:  0.0329,	Adjusted R-squared:  -0.1283 
## F-statistic: 0.2041 on 1 and 6 DF,  p-value: 0.6673
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ cis.4.7.10.13.16.19.Docosahexaenoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.595 -7.624 -2.377  4.721 20.305 
## 
## Coefficients:
##                                     Estimate Std. Error t value Pr(>|t|)
## (Intercept)                           4.0371    13.1104   0.308    0.769
## cis.4.7.10.13.16.19.Docosahexaenoic  -0.4454     1.0146  -0.439    0.676
## 
## Residual standard error: 10.79 on 6 degrees of freedom
## Multiple R-squared:  0.03112,	Adjusted R-squared:  -0.1304 
## F-statistic: 0.1927 on 1 and 6 DF,  p-value: 0.676
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ cis.4.7.10.13.16.19.Docosahexaenoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -82.99 -40.93 -32.56  42.10 119.44 
## 
## Coefficients:
##                                     Estimate Std. Error t value Pr(>|t|)
## (Intercept)                           -7.034     94.340  -0.075    0.943
## cis.4.7.10.13.16.19.Docosahexaenoic   -6.033      7.301  -0.826    0.440
## 
## Residual standard error: 77.62 on 6 degrees of freedom
## Multiple R-squared:  0.1022,	Adjusted R-squared:  -0.04748 
## F-statistic: 0.6827 on 1 and 6 DF,  p-value: 0.4403
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ cis.4.7.10.13.16.19.Docosahexaenoic, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -211.09 -175.85  -24.40   38.49  481.92 
## 
## Coefficients:
##                                     Estimate Std. Error t value Pr(>|t|)
## (Intercept)                           107.36     296.81   0.362     0.73
## cis.4.7.10.13.16.19.Docosahexaenoic   -13.80      22.97  -0.601     0.57
## 
## Residual standard error: 244.2 on 6 degrees of freedom
## Multiple R-squared:  0.05678,	Adjusted R-squared:  -0.1004 
## F-statistic: 0.3612 on 1 and 6 DF,  p-value: 0.5698
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ cis.4.7.10.13.16.19.Docosahexaenoic, 
##     data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -93.790 -30.703  -7.326  24.417  84.840 
## 
## Coefficients:
##                                     Estimate Std. Error t value Pr(>|t|)
## (Intercept)                          -63.295     77.879  -0.813    0.447
## cis.4.7.10.13.16.19.Docosahexaenoic  -10.049      6.027  -1.667    0.146
## 
## Residual standard error: 64.08 on 6 degrees of freedom
## Multiple R-squared:  0.3166,	Adjusted R-squared:  0.2027 
## F-statistic:  2.78 on 1 and 6 DF,  p-value: 0.1465
## 
## 
## 
## Summary for Lignocerate :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Lignocerate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.6481 -2.2118  0.1779  2.9379  7.4194 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   27.975     10.490   2.667   0.0372 *
## Lignocerate   10.778      4.685   2.301   0.0610 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.357 on 6 degrees of freedom
## Multiple R-squared:  0.4687,	Adjusted R-squared:  0.3802 
## F-statistic: 5.293 on 1 and 6 DF,  p-value: 0.06104
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Lignocerate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -11184  -8338  -2647   2250  28481 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -23369      26621  -0.878    0.414
## Lignocerate   -14838      11888  -1.248    0.258
## 
## Residual standard error: 13590 on 6 degrees of freedom
## Multiple R-squared:  0.2061,	Adjusted R-squared:  0.0738 
## F-statistic: 1.558 on 1 and 6 DF,  p-value: 0.2585
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Lignocerate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -19.776 -13.598  -6.579  10.415  31.964 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   0.3166    42.0747   0.008    0.994
## Lignocerate -14.9235    18.7896  -0.794    0.457
## 
## Residual standard error: 21.49 on 6 degrees of freedom
## Multiple R-squared:  0.09513,	Adjusted R-squared:  -0.05568 
## F-statistic: 0.6308 on 1 and 6 DF,  p-value: 0.4573
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Lignocerate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.068  -7.519  -4.587   7.844  17.753 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    6.327     22.824   0.277    0.791
## Lignocerate   -2.476     10.192  -0.243    0.816
## 
## Residual standard error: 11.66 on 6 degrees of freedom
## Multiple R-squared:  0.009743,	Adjusted R-squared:  -0.1553 
## F-statistic: 0.05903 on 1 and 6 DF,  p-value: 0.8161
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Lignocerate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.19356 -0.19356 -0.12550  0.02086  0.74644 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  -0.2406     0.6743  -0.357    0.733
## Lignocerate  -0.1739     0.3011  -0.578    0.585
## 
## Residual standard error: 0.3443 on 6 degrees of freedom
## Multiple R-squared:  0.05268,	Adjusted R-squared:  -0.1052 
## F-statistic: 0.3337 on 1 and 6 DF,  p-value: 0.5845
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Lignocerate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -3.930 -2.805 -1.070  2.022  6.220 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   0.1897     7.8345   0.024    0.981
## Lignocerate  -1.4984     3.4987  -0.428    0.683
## 
## Residual standard error: 4.001 on 6 degrees of freedom
## Multiple R-squared:  0.02966,	Adjusted R-squared:  -0.1321 
## F-statistic: 0.1834 on 1 and 6 DF,  p-value: 0.6834
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Lignocerate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.4670  -5.8264  -0.1392   3.5129  17.3530 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  -11.862     19.535  -0.607    0.566
## Lignocerate   -9.719      8.724  -1.114    0.308
## 
## Residual standard error: 9.976 on 6 degrees of freedom
## Multiple R-squared:  0.1714,	Adjusted R-squared:  0.03332 
## F-statistic: 1.241 on 1 and 6 DF,  p-value: 0.3079
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Lignocerate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -62.02 -58.14 -28.46  35.62 142.93 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -11.31     157.04  -0.072    0.945
## Lignocerate   -35.80      70.13  -0.511    0.628
## 
## Residual standard error: 80.2 on 6 degrees of freedom
## Multiple R-squared:  0.04163,	Adjusted R-squared:  -0.1181 
## F-statistic: 0.2607 on 1 and 6 DF,  p-value: 0.6279
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Lignocerate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -171.85 -111.78  -18.19   40.55  349.98 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   -561.4      348.0  -1.613   0.1578  
## Lignocerate   -381.1      155.4  -2.453   0.0496 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 177.7 on 6 degrees of freedom
## Multiple R-squared:  0.5007,	Adjusted R-squared:  0.4174 
## F-statistic: 6.016 on 1 and 6 DF,  p-value: 0.04961
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Lignocerate, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -77.01 -36.75 -19.09  33.65 123.09 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   -77.11     140.56  -0.549    0.603
## Lignocerate   -62.68      62.77  -0.999    0.357
## 
## Residual standard error: 71.78 on 6 degrees of freedom
## Multiple R-squared:  0.1425,	Adjusted R-squared:  -0.0004168 
## F-statistic: 0.9971 on 1 and 6 DF,  p-value: 0.3566
## 
## 
## 
## Summary for Nervonic.acid :
## Response Propionic :
## 
## Call:
## lm(formula = Propionic ~ Nervonic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -6.081 -3.680 -1.142  1.641 12.178 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)      30.00      16.72   1.794    0.123
## Nervonic.acid  -164.73     105.99  -1.554    0.171
## 
## Residual standard error: 6.206 on 6 degrees of freedom
## Multiple R-squared:  0.287,	Adjusted R-squared:  0.1682 
## F-statistic: 2.415 on 1 and 6 DF,  p-value: 0.1711
## 
## 
## Response Isobutyric :
## 
## Call:
## lm(formula = Isobutyric ~ Nervonic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
##  -9478  -7497  -5336   -738  32851 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)       3571      41038   0.087    0.933
## Nervonic.acid    36698     260161   0.141    0.892
## 
## Residual standard error: 15230 on 6 degrees of freedom
## Multiple R-squared:  0.003305,	Adjusted R-squared:  -0.1628 
## F-statistic: 0.0199 on 1 and 6 DF,  p-value: 0.8924
## 
## 
## Response Butyric :
## 
## Call:
## lm(formula = Butyric ~ Nervonic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -26.179 -13.757  -6.271  11.862  33.819 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)      48.16      60.54   0.795    0.457
## Nervonic.acid   -95.75     383.80  -0.249    0.811
## 
## Residual standard error: 22.47 on 6 degrees of freedom
## Multiple R-squared:  0.01027,	Adjusted R-squared:  -0.1547 
## F-statistic: 0.06224 on 1 and 6 DF,  p-value: 0.8113
## 
## 
## Response X2.Me.butyric :
## 
## Call:
## lm(formula = X2.Me.butyric ~ Nervonic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -11.274  -7.455  -3.356  11.176  12.316 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)     -16.04      29.40  -0.545    0.605
## Nervonic.acid   177.88     186.40   0.954    0.377
## 
## Residual standard error: 10.91 on 6 degrees of freedom
## Multiple R-squared:  0.1318,	Adjusted R-squared:  -0.01292 
## F-statistic: 0.9107 on 1 and 6 DF,  p-value: 0.3768
## 
## 
## Response Isovaleric :
## 
## Call:
## lm(formula = Isovaleric ~ Nervonic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.29814 -0.17682 -0.05008  0.08277  0.62909 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)     1.1410     0.8599   1.327    0.233
## Nervonic.acid  -6.3852     5.4513  -1.171    0.286
## 
## Residual standard error: 0.3192 on 6 degrees of freedom
## Multiple R-squared:  0.1861,	Adjusted R-squared:  0.05045 
## F-statistic: 1.372 on 1 and 6 DF,  p-value: 0.2859
## 
## 
## Response Valeric :
## 
## Call:
## lm(formula = Valeric ~ Nervonic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -3.404 -2.335 -1.638  1.290  6.662 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)      2.829     10.939   0.259    0.805
## Nervonic.acid    4.224     69.348   0.061    0.953
## 
## Residual standard error: 4.06 on 6 degrees of freedom
## Multiple R-squared:  0.000618,	Adjusted R-squared:  -0.1659 
## F-statistic: 0.00371 on 1 and 6 DF,  p-value: 0.9534
## 
## 
## Response Caproic :
## 
## Call:
## lm(formula = Caproic ~ Nervonic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -10.989  -7.158  -2.462   6.429  17.799 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)      23.82      28.93   0.823    0.442
## Nervonic.acid   -91.29     183.43  -0.498    0.636
## 
## Residual standard error: 10.74 on 6 degrees of freedom
## Multiple R-squared:  0.03964,	Adjusted R-squared:  -0.1204 
## F-statistic: 0.2477 on 1 and 6 DF,  p-value: 0.6364
## 
## 
## Response Valine :
## 
## Call:
## lm(formula = Valine ~ Nervonic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -89.42 -31.18 -19.44  18.68 147.88 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)     -120.5      206.7  -0.583    0.581
## Nervonic.acid   1202.7     1310.2   0.918    0.394
## 
## Residual standard error: 76.71 on 6 degrees of freedom
## Multiple R-squared:  0.1231,	Adjusted R-squared:  -0.023 
## F-statistic: 0.8426 on 1 and 6 DF,  p-value: 0.394
## 
## 
## Response Leucine :
## 
## Call:
## lm(formula = Leucine ~ Nervonic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -272.24 -152.55   -4.06   65.89  462.23 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)      125.8      674.5   0.186    0.858
## Nervonic.acid    973.5     4276.4   0.228    0.827
## 
## Residual standard error: 250.4 on 6 degrees of freedom
## Multiple R-squared:  0.008563,	Adjusted R-squared:  -0.1567 
## F-statistic: 0.05182 on 1 and 6 DF,  p-value: 0.8275
## 
## 
## Response Isoleucine :
## 
## Call:
## lm(formula = Isoleucine ~ Nervonic.acid, data = merge_SCFA_FAMES_IU_0)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -67.58 -43.55 -29.49  21.12 143.53 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)      129.8      206.9   0.627    0.554
## Nervonic.acid   -440.0     1311.7  -0.335    0.749
## 
## Residual standard error: 76.8 on 6 degrees of freedom
## Multiple R-squared:  0.01841,	Adjusted R-squared:  -0.1452 
## F-statistic: 0.1125 on 1 and 6 DF,  p-value: 0.7487
```
