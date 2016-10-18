---
title: "Extortion in Mexico: Who pays and why?"
author: "Patricio R. Estevez Soto"
email: "patricio.estevez.14@ucl.ac.uk"
date: "30/9/2016"
output:
  md_document:
    variant: "markdown"
pandoc_args: "--smart"
---



# Introduction

This document contains the script and results for a research project on extortion against businesses in Mexico.

Previous research has found that extortion against businesses is acutely concentrated on a few businesses who experience more than half of all extortion incidents in the country as repeat extortion victimizations. That project found that the rate at which businesses suffer extortion is mostly determined by their characteristics---such as their age, whether they are a restaurant, the number of corruption incidents they have suffered, and their size---and to a lesser extent, to the characteristics of the state where they are located---which include the state homicide rate and other unmeasured between-state differences.

Nonetheless, that research did not consider that most extortion victims do not comply with extortion demands. Thus this project aims to explore the distribution of successful extortion incidents, and identify the incident, victim and area characteristics associated with a higher likelihood of complying with extortion.

The incident-level characteristics that will be explored are:

- month
- time of day
- number of offenders
- victim's relationship to the offender
- use of a weapon
- type of weapon
- use of violence
- whether the incident was reported to the authorities (MP + other)
- type of extortion
- what was requested
- retaliation against business for not complying
- whether the victim complied with the extortion

The victim-level characteristics are:
- business type
- business age
- business size
- Victim of corruption
- number of corruption victimizations, repeat corruption victim
- number of extortion victimizations
- repeat extortion victim

Area-level characteristics:
- State
- State homicides
- State population (control)

# Set up, data input and pre-process

## Session info

We first check details of the session and system, and for reproducibility, we set the random seed.


```r
starttime <- proc.time()
date()
```

```
[1] "Tue Oct 18 17:16:09 2016"
```

```r
sessionInfo()
```

```
R version 3.3.0 (2016-05-03)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Red Hat Enterprise Linux Server 7.2 (Maipo)

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] sjPlot_2.1.0    sjstats_0.5.0   pscl_1.4.9      lattice_0.20-33
 [5] MASS_7.3-45     lmtest_0.9-34   zoo_1.7-13      xtable_1.8-2   
 [9] texreg_1.36.7   car_2.1-3       classInt_0.1-23 lme4_1.1-12    
[13] Matrix_1.2-6    Cairo_1.5-9     ggplot2_2.1.0   foreign_0.8-67 
[17] knitr_1.14     

loaded via a namespace (and not attached):
 [1] stringdist_0.9.4.2 modeltools_0.2-21  coin_1.1-2        
 [4] purrr_0.2.2        reshape2_1.4.1     haven_1.0.0       
 [7] splines_3.3.0      colorspace_1.2-6   stats4_3.3.0      
[10] mgcv_1.8-12        survival_2.39-5    e1071_1.6-7       
[13] nloptr_1.0.4       DBI_0.5-1          multcomp_1.4-6    
[16] plyr_1.8.4         effects_3.1-1      stringr_1.1.0     
[19] MatrixModels_0.4-1 sjmisc_2.0.0       munsell_0.4.3     
[22] gtable_0.2.0       mvtnorm_1.0-5      psych_1.6.9       
[25] codetools_0.2-14   evaluate_0.9       labeling_0.3      
[28] SparseM_1.72       quantreg_5.29      pbkrtest_0.4-6    
[31] parallel_3.3.0     class_7.3-14       highr_0.6         
[34] broom_0.4.1        TH.data_1.0-7      Rcpp_0.12.7       
[37] scales_0.4.0       formatR_1.4        mnormt_1.5-4      
[40] digest_0.6.10      stringi_1.1.1      dplyr_0.5.0       
[43] grid_3.3.0         tools_3.3.0        sandwich_2.3-4    
[46] magrittr_1.5       tibble_1.2         tidyr_0.6.0       
[49] assertthat_0.1     minqa_1.2.4        R6_2.1.2          
[52] nnet_7.3-12        nlme_3.1-128      
```

```r
set.seed(42)
#options(scipen=99999)
```

## Load packages and functions

Next we load the packages that we will use.


```r
library(foreign)
library(ggplot2)
library(Cairo)
library(knitr)
library(lme4)
```

```
Loading required package: Matrix
```

```r
library(classInt)
library(car)
library(texreg)
```

```
Version:  1.36.7
Date:     2016-06-21
Author:   Philip Leifeld (University of Glasgow)

Please cite the JSS article in your publications -- see citation("texreg").
```

```r
library(xtable)
library(lmtest)
```

```
Loading required package: zoo
```

```

Attaching package: 'zoo'
```

```
The following objects are masked from 'package:base':

    as.Date, as.Date.numeric
```

```r
library(pscl)
```

```
Loading required package: MASS
```

```
Loading required package: lattice
```

```
Classes and Methods for R developed in the
```

```
Political Science Computational Laboratory
```

```
Department of Political Science
```

```
Stanford University
```

```
Simon Jackman
```

```
hurdle and zeroinfl functions by Achim Zeileis
```

```r
library(sjstats)
library(sjPlot)
```


```r
cv.test = function(df) {
  CV = sqrt(chisq.test(df)$statistic /
              (sum(df) * (min(ncol(df),nrow(df)) - 1)))
  return(as.numeric(CV))
}

id.test <- function(x)
{
  # Takes a vector and calculates the index of dispersion
  # Then evaluates its significance for Overdispersion only
  # Returns a list with named numbers
  mu <- mean(x)
  names(mu) <- "Mean"
  v <- var(x)
  names(v) <- "Variance"
  n <- length(x)
  df <- n-1
  names(df) <- "df"
  index <- ((df)*v)/mu
  names(index) <- "I"
  pval <- pchisq(index, df, lower.tail=FALSE)
  names(pval) <- "P-value"
  pv95 <- qchisq(.95, df)
  names(pv95) <- "95% Chi-sq"

  return(c(index, pval, df, pv95))
}
```

## Load data

We first load and arrange the area and victim level data


```r
enve_all <- read.dbf("TR_ENVE_CUES_2014.dbf")
cat_entidades <- read.csv("cat_entidades.csv", head=TRUE)
homicidios <- read.csv("homicidios_values.csv", header=TRUE)
homicidios <- merge(homicidios, cat_entidades, by="CVE_ENT", all.x=TRUE)
scode <- read.csv("secode.csv", head=TRUE)
scode$Code <- scode$Code*10000

# Prepare data for analysis
# Selecting only the relevant variables

enve_test <- data.frame(extortions=as.integer(as.character(enve_all$P26_10)))

enve_test$extortion_victim <- factor(enve_test$extortions)
levels(enve_test$extortion_victim) <- c(0, rep(1, length(levels(enve_test$extortion_victim)) - 1))

enve_test$rep_extortion_victim <- factor(enve_test$extortions)
levels(enve_test$rep_extortion_victim) <- c(0, 0,
                                            rep(1, length(levels(enve_test$rep_extortion_victim)) - 2))

enve_test$CVE_UNICA <- as.integer(as.character(enve_all$CVE_UNICA))

enve_test$bribes <- as.integer(as.character(enve_all$P33))
enve_test$bribe_victim <- factor(enve_test$bribes)
levels(enve_test$bribe_victim) <- c(0, rep(1, length(levels(enve_test$bribe_victim)) - 1))

enve_test$rep_bribe <- factor(enve_test$bribes)
levels(enve_test$rep_bribe) <- c(0, 0, rep(1, length(levels(enve_test$rep_bribe)) - 2))

enve_test$bribe_cats <- factor(enve_test$bribes)
levels(enve_test$bribe_cats) <- c(0, 1, 2, rep("3+", length(levels(enve_test$bribe_cats)) - 3))

enve_test$CVE_ENT <- as.integer(as.character(enve_all$CVE_ENT))

enve_test$size <- enve_all$ID_ESTRATO
levels(enve_test$size) <- c("Large", "Medium", "Small", "Micro")

enve_test$sector <- enve_all$SECTOR_FIN

enve_test$tempsub <- as.integer(as.character(enve_all$P1_1B))
enve_test$subsector <- cut(enve_test$tempsub, scode$Code, right=FALSE)
levels(enve_test$subsector) <- scode$Sector
enve_test$subsector <- droplevels(enve_test$subsector)
enve_test$subsector <- relevel(enve_test$subsector, ref="Retail")
levels(enve_test$subsector)
```

```
 [1] "Retail"         "Mining"         "Utilities"      "Construction"  
 [5] "Manufacturing"  "Wholesale"      "Transport"      "Media"         
 [9] "Finance"        "Real estate"    "Prof. services" "Corporate"     
[13] "Maintenance"    "Education"      "Health"         "Leisure"       
[17] "HotelsRestBar"  "Other"         
```

```r
enve_test$hotrestbar <- enve_test$subsector
hotindx <- which(levels(enve_test$hotrestbar) == "HotelsRestBar")
levels(enve_test$hotrestbar)[-hotindx] <- 0
levels(enve_test$hotrestbar) <- c(0,1)

enve_test$years <- 2013 - as.numeric(as.character(enve_all$P3))
intyears <- classIntervals(enve_test$years, 5, style="quantile")
enve_test$yearsquant <- cut(enve_test$years, intyears$brks, right=TRUE,
                            include.lowest = TRUE)

enve_test <- merge(enve_test, homicidios, by="CVE_ENT", all.x=TRUE)

length(enve_test$extortions[is.na(enve_test$extortions)])
```

```
[1] 0
```

```r
length(enve_test$bribes[is.na(enve_test$bribes)])
```

```
[1] 0
```

```r
enve_test$extortions[is.na(enve_test$extortions)] <- 0
enve_test$bribes[is.na(enve_test$bribes)] <- 0
```

Next we load incident-level data:


```r
enve_incidents_all <- read.dbf("TR_ENVE_DELITOS2014.dbf")

# Selecting only those relevant for extortion (code 10)

enve_incidents_all$delito <- as.integer(as.character(enve_incidents_all$ID_DELITO))

enve_incidents <- enve_incidents_all[enve_incidents_all$delito == 10,]

# Selecting those relevant for our study

incident_df <- data.frame(CVE_UNICA=as.integer(as.character(enve_incidents$CVE_UNICA)))

incident_df$delito <- enve_incidents$delito

meses <- c("January", "February", "March",
           "April", "May", "June", "July",
           "August", "September", "October",
           "November", "December", "NONE")

incident_df$month <- enve_incidents$M1_1
levels(incident_df$month) <- meses

incident_df$time <- enve_incidents$M1_3
levels(incident_df$time) <- c("Morning", "Afternoon", "Evening", "Night", "DK/DA")

incident_df$n_offenders <- enve_incidents$M1_8
levels(incident_df$n_offenders) <- c(1:5, "6+", "DK/DA")

## Data imputation for missing variables?

incident_df$rel_offenders <- enve_incidents$M1_11
levels(incident_df$rel_offenders) <- c("Employee", "Barely known",
                                       "Somewhat known", "Close acquaintance",
                                       "Total stranger", "DK/DA")
incident_df$rel_offenders <- relevel(incident_df$rel_offenders, ref="Total stranger")

incident_df$had_weapon <- enve_incidents$M1_13
levels(incident_df$had_weapon) <- c("Yes", "No", "DK/DA")
incident_df$had_weapon <- relevel(incident_df$had_weapon, ref="No")

incident_df$gun <- enve_incidents$M1_14_1
levels(incident_df$gun) <- c("Yes", "No", "DK/DA")
incident_df$gun <- relevel(incident_df$gun, ref="No")

incident_df$knife <- enve_incidents$M1_14_2
levels(incident_df$knife) <- c("Yes", "No", "DK/DA")
incident_df$knife <- relevel(incident_df$knife, ref="No")

incident_df$blunt <- enve_incidents$M1_14_3
levels(incident_df$blunt) <- c("Yes", "No", "DK/DA")
incident_df$blunt <- relevel(incident_df$blunt, ref="No")

incident_df$other <- enve_incidents$M1_14_4
levels(incident_df$other)
```

```
[1] "1" "2"
```

```r
levels(incident_df$other) <- c("Yes", "No")
incident_df$other <- relevel(incident_df$other, ref="No")

incident_df$weapon_type <- NA
incident_df[which(incident_df$other=="Yes"),"weapon_type"] <- "Other"
incident_df$weapon_type[which(incident_df$blunt=="Yes")] <- "Blunt"
incident_df$weapon_type[which(incident_df$knife=="Yes")] <- "Knife"
incident_df$weapon_type[which(incident_df$gun=="Yes")] <- "Gun"
incident_df$weapon_type[which(incident_df$had_weapon=="No")] <- "None"
incident_df$weapon_type[which(incident_df$had_weapon=="DK/DA")] <- "DK/DA"
incident_df$weapon_type <- as.factor(incident_df$weapon_type)
incident_df$weapon_type <- relevel(incident_df$weapon_type, ref="None")

incident_df$violenceI <- as.character(enve_incidents$M1_15)
incident_df$violenceI <- as.factor(incident_df$violenceI)
levels(incident_df$violenceI) <- c("Yes", "No", "DK/DA")
incident_df$violenceI <- relevel(incident_df$violenceI, ref="No")

incident_df$violenceII <- as.character(enve_incidents$M1_16)
incident_df$violenceII <- as.factor(incident_df$violenceII)
levels(incident_df$violenceII) <- c("Yes", "No", "DK/DA")
incident_df$violenceII <- relevel(incident_df$violenceII, ref="No")

incident_df$with_violence <- NA
incident_df$with_violence[which(incident_df$violenceI == "DK/DA" |
                                  incident_df$violenceII == "DK/DA")] <- 9
incident_df$with_violence[which(incident_df$violenceI == "No" |
                                  incident_df$violenceII == "No")] <- 0
incident_df$with_violence[which(incident_df$violenceI == "Yes" |
                                  incident_df$violenceII == "Yes")] <- 1
incident_df$with_violence <- as.factor(incident_df$with_violence)
levels(incident_df$with_violence) <- c("No", "Yes", "DK/DA")

incident_df$mp <- as.factor(as.character(enve_incidents$M1_18))
levels(incident_df$mp) <- c("Yes", "No", "DK/DA")
incident_df$mp <- relevel(incident_df$mp, ref="No")

incident_df$auto <- as.factor(as.character(enve_incidents$M1_26))
levels(incident_df$auto) <- c("Yes", "No", "DK/DA")
incident_df$auto <- relevel(incident_df$auto, ref="No")

incident_df$reported <- NA
incident_df$reported[which(incident_df$mp == "DK/DA" |
                             incident_df$auto == "DK/DA")] <- 9
incident_df$reported[which(incident_df$mp == "No" |
                             incident_df$auto == "No")] <- 0
incident_df$reported[which(incident_df$mp == "Yes" |
                             incident_df$auto == "Yes")] <- 1
incident_df$reported <- as.factor(incident_df$reported)
levels(incident_df$reported) <- c("No", "Yes", "DK/DA")

incident_df$extortion_type <- as.character(enve_incidents$M5_1)
incident_df$extortion_type <- as.factor(incident_df$extortion_type)
levels(incident_df$extortion_type) <- c("Telephone", "Internet", "Street",
                                        "Premises", "Cobro de piso", "Other")

requests <- with(enve_incidents, data.frame(money=M5_2_1, product=M5_2_2,
                                            nothing=M5_2_3, other=M5_2_4))

incident_df$request <- "Nothing"
incident_df$request[which(requests$other==1)] <- "Other"
incident_df$request[which(requests$nothing==1)] <- "Nothing"
incident_df$request[which(requests$product==1)] <- "Product"
incident_df$request[which(requests$money==1)] <- "Money"
incident_df$request <- as.factor(incident_df$request)
incident_df$request <- relevel(incident_df$request, ref="Other")

incident_df$complied <- enve_incidents$M5_3
levels(incident_df$complied) <-  c("Yes", "No", "DK/DA")
incident_df$complied <- relevel(incident_df$complied, ref="No")

incident_df$complied_bin <- enve_incidents$M5_3
levels(incident_df$complied_bin) <-  c("Yes", "No", NA)
incident_df$complied_bin <- relevel(incident_df$complied_bin, ref="No")

incident_df$retaliation <- enve_incidents$M5_4
levels(incident_df$retaliation) <-  c("Yes", "No", "DK/DA")
incident_df$retaliation <- relevel(incident_df$retaliation, ref="No")
```

Next we merge both incident-level and victim-level tables.


```r
enve_incvic <- merge(incident_df, enve_test, by="CVE_UNICA")
```

# EDA: Incident level

Before progressing to the modeling exercise, we need to explore the distribution of the different variables. See how many missing values are present, as well as exploring possible bivariate relationships, both between the IV and the DV, as well as between the IVs.


```r
summary(enve_incvic)
```

```
   CVE_UNICA         delito         month            time    
 Min.   :   60   Min.   :10   December :179   Morning  :494  
 1st Qu.: 7472   1st Qu.:10   November :177   Afternoon:822  
 Median :11826   Median :10   June     :162   Evening  :248  
 Mean   :12095   Mean   :10   September:132   Night    : 22  
 3rd Qu.:17614   3rd Qu.:10   March    :128   DK/DA    : 14  
 Max.   :23355   Max.   :10   October  :126                  
                              (Other)  :696                  
  n_offenders             rel_offenders had_weapon     gun      
 1      :570   Total stranger    :754   No   :375   No   :   9  
 2      :163   Employee          :  4   Yes  : 44   Yes  :  34  
 DK/DA  : 47   Barely known      : 21   DK/DA:428   DK/DA:   1  
 3      : 38   Somewhat known    :  9   NA's :753   NA's :1556  
 5      : 10   Close acquaintance:  9                           
 (Other): 19   DK/DA             : 50                           
 NA's   :753   NA's              :753                           
   knife        blunt       other      weapon_type violenceI   
 No   :  36   No   :  43   No  :  44   None :375   No   :  42  
 Yes  :   8   Yes  :   1   Yes :   0   Blunt:  1   Yes  :   2  
 DK/DA:   0   DK/DA:   0   NA's:1556   DK/DA:428   DK/DA:   0  
 NA's :1556   NA's :1556               Gun  : 34   NA's :1556  
                                       Knife:  7               
                                       Other:  0               
                                       NA's :755               
 violenceII  with_violence     mp          auto       reported   
 No   :770   No   :770     No   :1537   No   :1466   No   :1415  
 Yes  : 17   Yes  : 19     Yes  :  61   Yes  : 133   Yes  : 185  
 DK/DA: 58   DK/DA: 58     DK/DA:   2   DK/DA:   1   DK/DA:   0  
 NA's :755   NA's :753                                           
                                                                 
                                                                 
                                                                 
       extortion_type    request      complied    complied_bin
 Telephone    :1475   Other  : 363   No   :1132   No  :1132   
 Internet     :   6   Money  :1138   Yes  : 100   Yes : 100   
 Street       :  63   Nothing:  63   DK/DA:   8   NA's: 368   
 Premises     :  16   Product:  36   NA's : 360               
 Cobro de piso:   7                                           
 Other        :  33                                           
                                                              
 retaliation     CVE_ENT        extortions      extortion_victim
 No   :   3   Min.   : 1.00   Min.   : 0.0000   0:1416          
 Yes  :   2   1st Qu.: 9.00   1st Qu.: 0.0000   1: 184          
 DK/DA:   2   Median :16.00   Median : 0.0000                   
 NA's :1593   Mean   :16.55   Mean   : 0.5875                   
              3rd Qu.:24.00   3rd Qu.: 0.0000                   
              Max.   :32.00   Max.   :33.0000                   
                                                                
 rep_extortion_victim     bribes        bribe_victim rep_bribe bribe_cats
 0:1493               Min.   : 0.0000   0:1416       0:1501    0 :1416   
 1: 107               1st Qu.: 0.0000   1: 184       1:  99    1 :  85   
                      Median : 0.0000                          2 :  30   
                      Mean   : 0.3125                          3+:  69   
                      3rd Qu.: 0.0000                                    
                      Max.   :16.0000                                    
                                                                         
     size     sector     tempsub               subsector   hotrestbar
 Large :394   C:516   Min.   :212410   Manufacturing:339   0:1365    
 Medium:386   I:503   1st Qu.:366185   Health       :266   1: 235    
 Small :369   S:581   Median :514510   HotelsRestBar:235             
 Micro :451           Mean   :514018   Construction :202             
                      3rd Qu.:666735   Maintenance  :129             
                      Max.   :812910   Transport    : 87             
                                       (Other)      :342             
     years         yearsquant        X         denuncias_homs  
 Min.   : 0.00   [0,8]  :371   Min.   : 1.00   Min.   :  39.0  
 1st Qu.: 9.00   (8,16] :314   1st Qu.: 9.00   1st Qu.: 151.0  
 Median :20.00   (16,25]:318   Median :16.00   Median : 536.0  
 Mean   :20.36   (25,34]:319   Mean   :16.55   Mean   : 596.8  
 3rd Qu.:31.00   (34,43]:278   3rd Qu.:24.00   3rd Qu.: 775.0  
 Max.   :43.00                 Max.   :32.00   Max.   :2087.0  
                                                               
   poblacion           tasahom          loghoms             logpop       
 Min.   :  698295   Min.   : 1.938   Min.   :-2.68705   Min.   :-1.6674  
 1st Qu.: 1874188   1st Qu.: 7.797   1st Qu.:-1.33333   1st Qu.:-0.6801  
 Median : 2932313   Median :12.814   Median :-0.06648   Median :-0.2325  
 Mean   : 3963777   Mean   :15.862   Mean   :-0.42794   Mean   :-0.2032  
 3rd Qu.: 4941059   3rd Qu.:20.165   3rd Qu.: 0.30225   3rd Qu.: 0.2893  
 Max.   :16364210   Max.   :59.225   Max.   : 1.29287   Max.   : 1.4868  
                                                                         
  tasahom_cntr         logtasa              NOM_ENT        NOM_ABR    
 Min.   :-13.5451   Min.   :-2.0782   JALISCO   :  74   JAL.   :  74  
 1st Qu.: -7.6856   1st Qu.:-0.6860   OAXACA    :  67   OAX.   :  67  
 Median : -2.6690   Median :-0.1892   GUANAJUATO:  64   GTO.   :  64  
 Mean   :  0.3789   Mean   :-0.2247   CHIHUAHUA :  63   CHIH.  :  63  
 3rd Qu.:  4.6822   3rd Qu.: 0.2642   ZACATECAS :  62   ZAC.   :  62  
 Max.   : 43.7420   Max.   : 1.3416   HIDALGO   :  60   HGO.   :  60  
                                      (Other)   :1210   (Other):1210  
```

Using **compliance and of type of extortion** as DVs, we explore the relationship to different variables.

First we start with incident-level variables.

## Months

### Compliance


```r
## basic summary first

t0 <- table(enve_incvic$month)
t0
```

```

  January  February     March     April       May      June      July 
       85       120       128        83       125       162       111 
   August September   October  November  December      NONE 
      113       132       126       177       179        59 
```

```r
round(t0/sum(t0)*100, 2)
```

```

  January  February     March     April       May      June      July 
     5.31      7.50      8.00      5.19      7.81     10.12      6.94 
   August September   October  November  December      NONE 
     7.06      8.25      7.88     11.06     11.19      3.69 
```

```r
## first No NA then with NA

## Use complied_bin

t1 <- table(enve_incvic$month, enve_incvic$complied_bin)
t1
```

```
           
             No Yes
  January    69   5
  February   88   7
  March      94  11
  April      59   5
  May        85   7
  June      107  12
  July       68   4
  August     83   4
  September  96   9
  October    86   7
  November  129  15
  December  129  11
  NONE       39   3
```

```r
kable(t1)
```



|          |  No| Yes|
|:---------|---:|---:|
|January   |  69|   5|
|February  |  88|   7|
|March     |  94|  11|
|April     |  59|   5|
|May       |  85|   7|
|June      | 107|  12|
|July      |  68|   4|
|August    |  83|   4|
|September |  96|   9|
|October   |  86|   7|
|November  | 129|  15|
|December  | 129|  11|
|NONE      |  39|   3|

```r
t1/as.integer(margin.table(t1, margin=1))*100
```

```
           
                   No       Yes
  January   93.243243  6.756757
  February  92.631579  7.368421
  March     89.523810 10.476190
  April     92.187500  7.812500
  May       92.391304  7.608696
  June      89.915966 10.084034
  July      94.444444  5.555556
  August    95.402299  4.597701
  September 91.428571  8.571429
  October   92.473118  7.526882
  November  89.583333 10.416667
  December  92.142857  7.857143
  NONE      92.857143  7.142857
```

```r
kable(round(t1/as.integer(margin.table(t1, margin=1))*100, 2))
```



|          |    No|   Yes|
|:---------|-----:|-----:|
|January   | 93.24|  6.76|
|February  | 92.63|  7.37|
|March     | 89.52| 10.48|
|April     | 92.19|  7.81|
|May       | 92.39|  7.61|
|June      | 89.92| 10.08|
|July      | 94.44|  5.56|
|August    | 95.40|  4.60|
|September | 91.43|  8.57|
|October   | 92.47|  7.53|
|November  | 89.58| 10.42|
|December  | 92.14|  7.86|
|NONE      | 92.86|  7.14|

```r
t2 <- table(enve_incvic$month, enve_incvic$complied_bin, useNA = "ifany")
t2
```

```
           
             No Yes <NA>
  January    69   5   11
  February   88   7   25
  March      94  11   23
  April      59   5   19
  May        85   7   33
  June      107  12   43
  July       68   4   39
  August     83   4   26
  September  96   9   27
  October    86   7   33
  November  129  15   33
  December  129  11   39
  NONE       39   3   17
```

```r
kable(t2)
```



|          |  No| Yes| NA|
|:---------|---:|---:|--:|
|January   |  69|   5| 11|
|February  |  88|   7| 25|
|March     |  94|  11| 23|
|April     |  59|   5| 19|
|May       |  85|   7| 33|
|June      | 107|  12| 43|
|July      |  68|   4| 39|
|August    |  83|   4| 26|
|September |  96|   9| 27|
|October   |  86|   7| 33|
|November  | 129|  15| 33|
|December  | 129|  11| 39|
|NONE      |  39|   3| 17|

```r
t2/as.integer(margin.table(t2, margin=1))*100
```

```
           
                   No       Yes      <NA>
  January   81.176471  5.882353 12.941176
  February  73.333333  5.833333 20.833333
  March     73.437500  8.593750 17.968750
  April     71.084337  6.024096 22.891566
  May       68.000000  5.600000 26.400000
  June      66.049383  7.407407 26.543210
  July      61.261261  3.603604 35.135135
  August    73.451327  3.539823 23.008850
  September 72.727273  6.818182 20.454545
  October   68.253968  5.555556 26.190476
  November  72.881356  8.474576 18.644068
  December  72.067039  6.145251 21.787709
  NONE      66.101695  5.084746 28.813559
```

```r
kable(round(t2/as.integer(margin.table(t2, margin=1))*100, 2))
```



|          |    No|  Yes|    NA|
|:---------|-----:|----:|-----:|
|January   | 81.18| 5.88| 12.94|
|February  | 73.33| 5.83| 20.83|
|March     | 73.44| 8.59| 17.97|
|April     | 71.08| 6.02| 22.89|
|May       | 68.00| 5.60| 26.40|
|June      | 66.05| 7.41| 26.54|
|July      | 61.26| 3.60| 35.14|
|August    | 73.45| 3.54| 23.01|
|September | 72.73| 6.82| 20.45|
|October   | 68.25| 5.56| 26.19|
|November  | 72.88| 8.47| 18.64|
|December  | 72.07| 6.15| 21.79|
|NONE      | 66.10| 5.08| 28.81|

### Extortion type


```r
t3 <- table(enve_incvic$extortion_type, enve_incvic$month)
t3
```

```
               
                January February March April May June July August
  Telephone          81      109   116    78 118  146  107    103
  Internet            0        1     1     0   0    1    1      2
  Street              2        7     6     4   2   10    2      4
  Premises            0        0     2     0   2    0    0      1
  Cobro de piso       2        1     1     0   1    1    0      0
  Other               0        2     2     1   2    4    1      3
               
                September October November December NONE
  Telephone           120     117      160      165   55
  Internet              0       0        0        0    0
  Street                6       4        9        5    2
  Premises              2       1        1        6    1
  Cobro de piso         0       1        0        0    0
  Other                 4       3        7        3    1
```

```r
kable(t3)
```



|              | January| February| March| April| May| June| July| August| September| October| November| December| NONE|
|:-------------|-------:|--------:|-----:|-----:|---:|----:|----:|------:|---------:|-------:|--------:|--------:|----:|
|Telephone     |      81|      109|   116|    78| 118|  146|  107|    103|       120|     117|      160|      165|   55|
|Internet      |       0|        1|     1|     0|   0|    1|    1|      2|         0|       0|        0|        0|    0|
|Street        |       2|        7|     6|     4|   2|   10|    2|      4|         6|       4|        9|        5|    2|
|Premises      |       0|        0|     2|     0|   2|    0|    0|      1|         2|       1|        1|        6|    1|
|Cobro de piso |       2|        1|     1|     0|   1|    1|    0|      0|         0|       1|        0|        0|    0|
|Other         |       0|        2|     2|     1|   2|    4|    1|      3|         4|       3|        7|        3|    1|

```r
t3/as.integer(margin.table(t3, margin=1))*100
```

```
               
                  January  February     March     April       May
  Telephone      5.491525  7.389831  7.864407  5.288136  8.000000
  Internet       0.000000 16.666667 16.666667  0.000000  0.000000
  Street         3.174603 11.111111  9.523810  6.349206  3.174603
  Premises       0.000000  0.000000 12.500000  0.000000 12.500000
  Cobro de piso 28.571429 14.285714 14.285714  0.000000 14.285714
  Other          0.000000  6.060606  6.060606  3.030303  6.060606
               
                     June      July    August September   October
  Telephone      9.898305  7.254237  6.983051  8.135593  7.932203
  Internet      16.666667 16.666667 33.333333  0.000000  0.000000
  Street        15.873016  3.174603  6.349206  9.523810  6.349206
  Premises       0.000000  0.000000  6.250000 12.500000  6.250000
  Cobro de piso 14.285714  0.000000  0.000000  0.000000 14.285714
  Other         12.121212  3.030303  9.090909 12.121212  9.090909
               
                 November  December      NONE
  Telephone     10.847458 11.186441  3.728814
  Internet       0.000000  0.000000  0.000000
  Street        14.285714  7.936508  3.174603
  Premises       6.250000 37.500000  6.250000
  Cobro de piso  0.000000  0.000000  0.000000
  Other         21.212121  9.090909  3.030303
```

```r
kable(round(t3/as.integer(margin.table(t3, margin=1))*100, 2))
```



|              | January| February| March| April|   May|  June|  July| August| September| October| November| December| NONE|
|:-------------|-------:|--------:|-----:|-----:|-----:|-----:|-----:|------:|---------:|-------:|--------:|--------:|----:|
|Telephone     |    5.49|     7.39|  7.86|  5.29|  8.00|  9.90|  7.25|   6.98|      8.14|    7.93|    10.85|    11.19| 3.73|
|Internet      |    0.00|    16.67| 16.67|  0.00|  0.00| 16.67| 16.67|  33.33|      0.00|    0.00|     0.00|     0.00| 0.00|
|Street        |    3.17|    11.11|  9.52|  6.35|  3.17| 15.87|  3.17|   6.35|      9.52|    6.35|    14.29|     7.94| 3.17|
|Premises      |    0.00|     0.00| 12.50|  0.00| 12.50|  0.00|  0.00|   6.25|     12.50|    6.25|     6.25|    37.50| 6.25|
|Cobro de piso |   28.57|    14.29| 14.29|  0.00| 14.29| 14.29|  0.00|   0.00|      0.00|   14.29|     0.00|     0.00| 0.00|
|Other         |    0.00|     6.06|  6.06|  3.03|  6.06| 12.12|  3.03|   9.09|     12.12|    9.09|    21.21|     9.09| 3.03|

## Time of day

### Compliance


```r
## basic summary first

t4 <- table(enve_incvic$time, useNA="ifany")
t4
```

```

  Morning Afternoon   Evening     Night     DK/DA 
      494       822       248        22        14 
```

```r
round(t4/sum(t4)*100, 2)
```

```

  Morning Afternoon   Evening     Night     DK/DA 
    30.88     51.38     15.50      1.38      0.88 
```

```r
## first No NA then with NA

## Use complied_bin

t5 <- table(enve_incvic$time, enve_incvic$complied_bin)
t5
```

```
           
             No Yes
  Morning   366  28
  Afternoon 566  47
  Evening   178  22
  Night      10   2
  DK/DA      12   1
```

```r
kable(t5)
```



|          |  No| Yes|
|:---------|---:|---:|
|Morning   | 366|  28|
|Afternoon | 566|  47|
|Evening   | 178|  22|
|Night     |  10|   2|
|DK/DA     |  12|   1|

```r
t5/as.integer(margin.table(t5, margin=1))*100
```

```
           
                   No       Yes
  Morning   92.893401  7.106599
  Afternoon 92.332790  7.667210
  Evening   89.000000 11.000000
  Night     83.333333 16.666667
  DK/DA     92.307692  7.692308
```

```r
kable(round(t5/as.integer(margin.table(t5, margin=1))*100, 2))
```



|          |    No|   Yes|
|:---------|-----:|-----:|
|Morning   | 92.89|  7.11|
|Afternoon | 92.33|  7.67|
|Evening   | 89.00| 11.00|
|Night     | 83.33| 16.67|
|DK/DA     | 92.31|  7.69|

```r
t6 <- table(enve_incvic$time, enve_incvic$complied_bin, useNA = "ifany")
t6
```

```
           
             No Yes <NA>
  Morning   366  28  100
  Afternoon 566  47  209
  Evening   178  22   48
  Night      10   2   10
  DK/DA      12   1    1
```

```r
kable(t6)
```



|          |  No| Yes|  NA|
|:---------|---:|---:|---:|
|Morning   | 366|  28| 100|
|Afternoon | 566|  47| 209|
|Evening   | 178|  22|  48|
|Night     |  10|   2|  10|
|DK/DA     |  12|   1|   1|

```r
t6/as.integer(margin.table(t6, margin=1))*100
```

```
           
                   No       Yes      <NA>
  Morning   74.089069  5.668016 20.242915
  Afternoon 68.856448  5.717762 25.425791
  Evening   71.774194  8.870968 19.354839
  Night     45.454545  9.090909 45.454545
  DK/DA     85.714286  7.142857  7.142857
```

```r
kable(round(t6/as.integer(margin.table(t6, margin=1))*100, 2))
```



|          |    No|  Yes|    NA|
|:---------|-----:|----:|-----:|
|Morning   | 74.09| 5.67| 20.24|
|Afternoon | 68.86| 5.72| 25.43|
|Evening   | 71.77| 8.87| 19.35|
|Night     | 45.45| 9.09| 45.45|
|DK/DA     | 85.71| 7.14|  7.14|

### Extortion type


```r
t7 <- table(enve_incvic$extortion_type, enve_incvic$time)
t7
```

```
               
                Morning Afternoon Evening Night DK/DA
  Telephone         463       766     212    20    14
  Internet            0         3       3     0     0
  Street             18        24      21     0     0
  Premises            3        11       2     0     0
  Cobro de piso       4         3       0     0     0
  Other               6        15      10     2     0
```

```r
kable(t7)
```



|              | Morning| Afternoon| Evening| Night| DK/DA|
|:-------------|-------:|---------:|-------:|-----:|-----:|
|Telephone     |     463|       766|     212|    20|    14|
|Internet      |       0|         3|       3|     0|     0|
|Street        |      18|        24|      21|     0|     0|
|Premises      |       3|        11|       2|     0|     0|
|Cobro de piso |       4|         3|       0|     0|     0|
|Other         |       6|        15|      10|     2|     0|

```r
t7/as.integer(margin.table(t7, margin=1))*100
```

```
               
                   Morning  Afternoon    Evening      Night      DK/DA
  Telephone     31.3898305 51.9322034 14.3728814  1.3559322  0.9491525
  Internet       0.0000000 50.0000000 50.0000000  0.0000000  0.0000000
  Street        28.5714286 38.0952381 33.3333333  0.0000000  0.0000000
  Premises      18.7500000 68.7500000 12.5000000  0.0000000  0.0000000
  Cobro de piso 57.1428571 42.8571429  0.0000000  0.0000000  0.0000000
  Other         18.1818182 45.4545455 30.3030303  6.0606061  0.0000000
```

```r
kable(round(t7/as.integer(margin.table(t7, margin=1))*100, 2))
```



|              | Morning| Afternoon| Evening| Night| DK/DA|
|:-------------|-------:|---------:|-------:|-----:|-----:|
|Telephone     |   31.39|     51.93|   14.37|  1.36|  0.95|
|Internet      |    0.00|     50.00|   50.00|  0.00|  0.00|
|Street        |   28.57|     38.10|   33.33|  0.00|  0.00|
|Premises      |   18.75|     68.75|   12.50|  0.00|  0.00|
|Cobro de piso |   57.14|     42.86|    0.00|  0.00|  0.00|
|Other         |   18.18|     45.45|   30.30|  6.06|  0.00|

## Number of offenders

### Compliance


```r
## basic summary first

t8 <- table(enve_incvic$n_offenders, useNA="ifany")
t8
```

```

    1     2     3     4     5    6+ DK/DA  <NA> 
  570   163    38     9    10    10    47   753 
```

```r
round(t8/sum(t8)*100, 2)
```

```

    1     2     3     4     5    6+ DK/DA  <NA> 
35.62 10.19  2.38  0.56  0.62  0.62  2.94 47.06 
```

```r
## first No NA then with NA

## Use complied_bin

t9 <- table(enve_incvic$n_offenders, enve_incvic$complied_bin)
t9
```

```
       
         No Yes
  1     392  28
  2     101  22
  3      26   5
  4       2   5
  5       1   8
  6+      5   4
  DK/DA  33   3
```

```r
kable(t9)
```



|      |  No| Yes|
|:-----|---:|---:|
|1     | 392|  28|
|2     | 101|  22|
|3     |  26|   5|
|4     |   2|   5|
|5     |   1|   8|
|6+    |   5|   4|
|DK/DA |  33|   3|

```r
t9/as.integer(margin.table(t9, margin=1))*100
```

```
       
               No       Yes
  1     93.333333  6.666667
  2     82.113821 17.886179
  3     83.870968 16.129032
  4     28.571429 71.428571
  5     11.111111 88.888889
  6+    55.555556 44.444444
  DK/DA 91.666667  8.333333
```

```r
kable(round(t9/as.integer(margin.table(t9, margin=1))*100, 2))
```



|      |    No|   Yes|
|:-----|-----:|-----:|
|1     | 93.33|  6.67|
|2     | 82.11| 17.89|
|3     | 83.87| 16.13|
|4     | 28.57| 71.43|
|5     | 11.11| 88.89|
|6+    | 55.56| 44.44|
|DK/DA | 91.67|  8.33|

```r
t10 <- table(enve_incvic$n_offenders, enve_incvic$complied_bin, useNA = "ifany")
t10
```

```
       
         No Yes <NA>
  1     392  28  150
  2     101  22   40
  3      26   5    7
  4       2   5    2
  5       1   8    1
  6+      5   4    1
  DK/DA  33   3   11
  <NA>  572  25  156
```

```r
kable(t10)
```



|      |  No| Yes|  NA|
|:-----|---:|---:|---:|
|1     | 392|  28| 150|
|2     | 101|  22|  40|
|3     |  26|   5|   7|
|4     |   2|   5|   2|
|5     |   1|   8|   1|
|6+    |   5|   4|   1|
|DK/DA |  33|   3|  11|
|NA    | 572|  25| 156|

```r
t10/as.integer(margin.table(t10, margin=1))*100
```

```
       
               No       Yes      <NA>
  1     68.771930  4.912281 26.315789
  2     61.963190 13.496933 24.539877
  3     68.421053 13.157895 18.421053
  4     22.222222 55.555556 22.222222
  5     10.000000 80.000000 10.000000
  6+    50.000000 40.000000 10.000000
  DK/DA 70.212766  6.382979 23.404255
  <NA>  75.962815  3.320053 20.717131
```

```r
kable(round(t10/as.integer(margin.table(t10, margin=1))*100, 2))
```



|      |    No|   Yes|    NA|
|:-----|-----:|-----:|-----:|
|1     | 68.77|  4.91| 26.32|
|2     | 61.96| 13.50| 24.54|
|3     | 68.42| 13.16| 18.42|
|4     | 22.22| 55.56| 22.22|
|5     | 10.00| 80.00| 10.00|
|6+    | 50.00| 40.00| 10.00|
|DK/DA | 70.21|  6.38| 23.40|
|NA    | 75.96|  3.32| 20.72|

### Extortion type


```r
t11 <- table(enve_incvic$extortion_type, enve_incvic$n_offenders)
t11
```

```
               
                  1   2   3   4   5  6+ DK/DA
  Telephone     529 135  24   3   2   0    45
  Internet        2   1   1   0   0   1     0
  Street         26  13  10   3   3   4     0
  Premises        3   3   1   1   5   1     0
  Cobro de piso   4   2   1   0   0   0     0
  Other           6   9   1   2   0   4     2
```

```r
kable(t11)
```



|              |   1|   2|  3|  4|  5| 6+| DK/DA|
|:-------------|---:|---:|--:|--:|--:|--:|-----:|
|Telephone     | 529| 135| 24|  3|  2|  0|    45|
|Internet      |   2|   1|  1|  0|  0|  1|     0|
|Street        |  26|  13| 10|  3|  3|  4|     0|
|Premises      |   3|   3|  1|  1|  5|  1|     0|
|Cobro de piso |   4|   2|  1|  0|  0|  0|     0|
|Other         |   6|   9|  1|  2|  0|  4|     2|

```r
t11/as.integer(margin.table(t11, margin=1))*100
```

```
               
                         1          2          3          4          5
  Telephone     71.6802168 18.2926829  3.2520325  0.4065041  0.2710027
  Internet      40.0000000 20.0000000 20.0000000  0.0000000  0.0000000
  Street        44.0677966 22.0338983 16.9491525  5.0847458  5.0847458
  Premises      21.4285714 21.4285714  7.1428571  7.1428571 35.7142857
  Cobro de piso 57.1428571 28.5714286 14.2857143  0.0000000  0.0000000
  Other         25.0000000 37.5000000  4.1666667  8.3333333  0.0000000
               
                        6+      DK/DA
  Telephone      0.0000000  6.0975610
  Internet      20.0000000  0.0000000
  Street         6.7796610  0.0000000
  Premises       7.1428571  0.0000000
  Cobro de piso  0.0000000  0.0000000
  Other         16.6666667  8.3333333
```

```r
kable(round(t11/as.integer(margin.table(t11, margin=1))*100, 2))
```



|              |     1|     2|     3|    4|     5|    6+| DK/DA|
|:-------------|-----:|-----:|-----:|----:|-----:|-----:|-----:|
|Telephone     | 71.68| 18.29|  3.25| 0.41|  0.27|  0.00|  6.10|
|Internet      | 40.00| 20.00| 20.00| 0.00|  0.00| 20.00|  0.00|
|Street        | 44.07| 22.03| 16.95| 5.08|  5.08|  6.78|  0.00|
|Premises      | 21.43| 21.43|  7.14| 7.14| 35.71|  7.14|  0.00|
|Cobro de piso | 57.14| 28.57| 14.29| 0.00|  0.00|  0.00|  0.00|
|Other         | 25.00| 37.50|  4.17| 8.33|  0.00| 16.67|  8.33|

```r
t12 <- table(enve_incvic$extortion_type, enve_incvic$n_offenders, useNA="ifany")
t12
```

```
               
                  1   2   3   4   5  6+ DK/DA <NA>
  Telephone     529 135  24   3   2   0    45  737
  Internet        2   1   1   0   0   1     0    1
  Street         26  13  10   3   3   4     0    4
  Premises        3   3   1   1   5   1     0    2
  Cobro de piso   4   2   1   0   0   0     0    0
  Other           6   9   1   2   0   4     2    9
```

```r
kable(t12)
```



|              |   1|   2|  3|  4|  5| 6+| DK/DA|  NA|
|:-------------|---:|---:|--:|--:|--:|--:|-----:|---:|
|Telephone     | 529| 135| 24|  3|  2|  0|    45| 737|
|Internet      |   2|   1|  1|  0|  0|  1|     0|   1|
|Street        |  26|  13| 10|  3|  3|  4|     0|   4|
|Premises      |   3|   3|  1|  1|  5|  1|     0|   2|
|Cobro de piso |   4|   2|  1|  0|  0|  0|     0|   0|
|Other         |   6|   9|  1|  2|  0|  4|     2|   9|

```r
t12/as.integer(margin.table(t12, margin=1))*100
```

```
               
                         1          2          3          4          5
  Telephone     35.8644068  9.1525424  1.6271186  0.2033898  0.1355932
  Internet      33.3333333 16.6666667 16.6666667  0.0000000  0.0000000
  Street        41.2698413 20.6349206 15.8730159  4.7619048  4.7619048
  Premises      18.7500000 18.7500000  6.2500000  6.2500000 31.2500000
  Cobro de piso 57.1428571 28.5714286 14.2857143  0.0000000  0.0000000
  Other         18.1818182 27.2727273  3.0303030  6.0606061  0.0000000
               
                        6+      DK/DA       <NA>
  Telephone      0.0000000  3.0508475 49.9661017
  Internet      16.6666667  0.0000000 16.6666667
  Street         6.3492063  0.0000000  6.3492063
  Premises       6.2500000  0.0000000 12.5000000
  Cobro de piso  0.0000000  0.0000000  0.0000000
  Other         12.1212121  6.0606061 27.2727273
```

```r
kable(round(t12/as.integer(margin.table(t12, margin=1))*100, 2))
```



|              |     1|     2|     3|    4|     5|    6+| DK/DA|    NA|
|:-------------|-----:|-----:|-----:|----:|-----:|-----:|-----:|-----:|
|Telephone     | 35.86|  9.15|  1.63| 0.20|  0.14|  0.00|  3.05| 49.97|
|Internet      | 33.33| 16.67| 16.67| 0.00|  0.00| 16.67|  0.00| 16.67|
|Street        | 41.27| 20.63| 15.87| 4.76|  4.76|  6.35|  0.00|  6.35|
|Premises      | 18.75| 18.75|  6.25| 6.25| 31.25|  6.25|  0.00| 12.50|
|Cobro de piso | 57.14| 28.57| 14.29| 0.00|  0.00|  0.00|  0.00|  0.00|
|Other         | 18.18| 27.27|  3.03| 6.06|  0.00| 12.12|  6.06| 27.27|

## Relationship to offender

## Compliance


```r
## basic summary first

t13 <- table(enve_incvic$rel_offenders, useNA="ifany")
t13
```

```

    Total stranger           Employee       Barely known 
               754                  4                 21 
    Somewhat known Close acquaintance              DK/DA 
                 9                  9                 50 
              <NA> 
               753 
```

```r
round(t13/sum(t13)*100, 2)
```

```

    Total stranger           Employee       Barely known 
             47.12               0.25               1.31 
    Somewhat known Close acquaintance              DK/DA 
              0.56               0.56               3.12 
              <NA> 
             47.06 
```

```r
## first No NA then with NA

## Use complied_bin

t14 <- table(enve_incvic$rel_offenders, enve_incvic$complied_bin)
t14
```

```
                    
                      No Yes
  Total stranger     502  65
  Employee             2   1
  Barely known        13   5
  Somewhat known       4   1
  Close acquaintance   2   2
  DK/DA               37   1
```

```r
kable(t14)
```



|                   |  No| Yes|
|:------------------|---:|---:|
|Total stranger     | 502|  65|
|Employee           |   2|   1|
|Barely known       |  13|   5|
|Somewhat known     |   4|   1|
|Close acquaintance |   2|   2|
|DK/DA              |  37|   1|

```r
t14/as.integer(margin.table(t14, margin=1))*100
```

```
                    
                            No       Yes
  Total stranger     88.536155 11.463845
  Employee           66.666667 33.333333
  Barely known       72.222222 27.777778
  Somewhat known     80.000000 20.000000
  Close acquaintance 50.000000 50.000000
  DK/DA              97.368421  2.631579
```

```r
kable(round(t14/as.integer(margin.table(t14, margin=1))*100, 2))
```



|                   |    No|   Yes|
|:------------------|-----:|-----:|
|Total stranger     | 88.54| 11.46|
|Employee           | 66.67| 33.33|
|Barely known       | 72.22| 27.78|
|Somewhat known     | 80.00| 20.00|
|Close acquaintance | 50.00| 50.00|
|DK/DA              | 97.37|  2.63|

```r
t15 <- table(enve_incvic$rel_offenders, enve_incvic$complied_bin, useNA = "ifany")
t15
```

```
                    
                      No Yes <NA>
  Total stranger     502  65  187
  Employee             2   1    1
  Barely known        13   5    3
  Somewhat known       4   1    4
  Close acquaintance   2   2    5
  DK/DA               37   1   12
  <NA>               572  25  156
```

```r
kable(t15)
```



|                   |  No| Yes|  NA|
|:------------------|---:|---:|---:|
|Total stranger     | 502|  65| 187|
|Employee           |   2|   1|   1|
|Barely known       |  13|   5|   3|
|Somewhat known     |   4|   1|   4|
|Close acquaintance |   2|   2|   5|
|DK/DA              |  37|   1|  12|
|NA                 | 572|  25| 156|

```r
t15/as.integer(margin.table(t15, margin=1))*100
```

```
                    
                            No       Yes      <NA>
  Total stranger     66.578249  8.620690 24.801061
  Employee           50.000000 25.000000 25.000000
  Barely known       61.904762 23.809524 14.285714
  Somewhat known     44.444444 11.111111 44.444444
  Close acquaintance 22.222222 22.222222 55.555556
  DK/DA              74.000000  2.000000 24.000000
  <NA>               75.962815  3.320053 20.717131
```

```r
kable(round(t15/as.integer(margin.table(t15, margin=1))*100, 2))
```



|                   |    No|   Yes|    NA|
|:------------------|-----:|-----:|-----:|
|Total stranger     | 66.58|  8.62| 24.80|
|Employee           | 50.00| 25.00| 25.00|
|Barely known       | 61.90| 23.81| 14.29|
|Somewhat known     | 44.44| 11.11| 44.44|
|Close acquaintance | 22.22| 22.22| 55.56|
|DK/DA              | 74.00|  2.00| 24.00|
|NA                 | 75.96|  3.32| 20.72|

### Extortion type


```r
t16 <- table(enve_incvic$extortion_type, enve_incvic$rel_offenders)
t16
```

```
               
                Total stranger Employee Barely known Somewhat known
  Telephone                685        1            0              3
  Internet                   4        0            0              1
  Street                    35        0           13              4
  Premises                  11        2            0              0
  Cobro de piso              7        0            0              0
  Other                     12        1            8              1
               
                Close acquaintance DK/DA
  Telephone                      0    49
  Internet                       0     0
  Street                         6     1
  Premises                       1     0
  Cobro de piso                  0     0
  Other                          2     0
```

```r
kable(t16)
```



|              | Total stranger| Employee| Barely known| Somewhat known| Close acquaintance| DK/DA|
|:-------------|--------------:|--------:|------------:|--------------:|------------------:|-----:|
|Telephone     |            685|        1|            0|              3|                  0|    49|
|Internet      |              4|        0|            0|              1|                  0|     0|
|Street        |             35|        0|           13|              4|                  6|     1|
|Premises      |             11|        2|            0|              0|                  1|     0|
|Cobro de piso |              7|        0|            0|              0|                  0|     0|
|Other         |             12|        1|            8|              1|                  2|     0|

```r
t16/as.integer(margin.table(t16, margin=1))*100
```

```
               
                Total stranger    Employee Barely known Somewhat known
  Telephone         92.8184282   0.1355014    0.0000000      0.4065041
  Internet          80.0000000   0.0000000    0.0000000     20.0000000
  Street            59.3220339   0.0000000   22.0338983      6.7796610
  Premises          78.5714286  14.2857143    0.0000000      0.0000000
  Cobro de piso    100.0000000   0.0000000    0.0000000      0.0000000
  Other             50.0000000   4.1666667   33.3333333      4.1666667
               
                Close acquaintance       DK/DA
  Telephone              0.0000000   6.6395664
  Internet               0.0000000   0.0000000
  Street                10.1694915   1.6949153
  Premises               7.1428571   0.0000000
  Cobro de piso          0.0000000   0.0000000
  Other                  8.3333333   0.0000000
```

```r
kable(round(t16/as.integer(margin.table(t16, margin=1))*100, 2))
```



|              | Total stranger| Employee| Barely known| Somewhat known| Close acquaintance| DK/DA|
|:-------------|--------------:|--------:|------------:|--------------:|------------------:|-----:|
|Telephone     |          92.82|     0.14|         0.00|           0.41|               0.00|  6.64|
|Internet      |          80.00|     0.00|         0.00|          20.00|               0.00|  0.00|
|Street        |          59.32|     0.00|        22.03|           6.78|              10.17|  1.69|
|Premises      |          78.57|    14.29|         0.00|           0.00|               7.14|  0.00|
|Cobro de piso |         100.00|     0.00|         0.00|           0.00|               0.00|  0.00|
|Other         |          50.00|     4.17|        33.33|           4.17|               8.33|  0.00|

```r
t17 <- table(enve_incvic$extortion_type, enve_incvic$rel_offenders, useNA="ifany")
t17
```

```
               
                Total stranger Employee Barely known Somewhat known
  Telephone                685        1            0              3
  Internet                   4        0            0              1
  Street                    35        0           13              4
  Premises                  11        2            0              0
  Cobro de piso              7        0            0              0
  Other                     12        1            8              1
               
                Close acquaintance DK/DA <NA>
  Telephone                      0    49  737
  Internet                       0     0    1
  Street                         6     1    4
  Premises                       1     0    2
  Cobro de piso                  0     0    0
  Other                          2     0    9
```

```r
kable(t17)
```



|              | Total stranger| Employee| Barely known| Somewhat known| Close acquaintance| DK/DA|  NA|
|:-------------|--------------:|--------:|------------:|--------------:|------------------:|-----:|---:|
|Telephone     |            685|        1|            0|              3|                  0|    49| 737|
|Internet      |              4|        0|            0|              1|                  0|     0|   1|
|Street        |             35|        0|           13|              4|                  6|     1|   4|
|Premises      |             11|        2|            0|              0|                  1|     0|   2|
|Cobro de piso |              7|        0|            0|              0|                  0|     0|   0|
|Other         |             12|        1|            8|              1|                  2|     0|   9|

```r
t17/as.integer(margin.table(t17, margin=1))*100
```

```
               
                Total stranger     Employee Barely known Somewhat known
  Telephone        46.44067797   0.06779661   0.00000000     0.20338983
  Internet         66.66666667   0.00000000   0.00000000    16.66666667
  Street           55.55555556   0.00000000  20.63492063     6.34920635
  Premises         68.75000000  12.50000000   0.00000000     0.00000000
  Cobro de piso   100.00000000   0.00000000   0.00000000     0.00000000
  Other            36.36363636   3.03030303  24.24242424     3.03030303
               
                Close acquaintance        DK/DA         <NA>
  Telephone             0.00000000   3.32203390  49.96610169
  Internet              0.00000000   0.00000000  16.66666667
  Street                9.52380952   1.58730159   6.34920635
  Premises              6.25000000   0.00000000  12.50000000
  Cobro de piso         0.00000000   0.00000000   0.00000000
  Other                 6.06060606   0.00000000  27.27272727
```

```r
kable(round(t17/as.integer(margin.table(t17, margin=1))*100, 2))
```



|              | Total stranger| Employee| Barely known| Somewhat known| Close acquaintance| DK/DA|    NA|
|:-------------|--------------:|--------:|------------:|--------------:|------------------:|-----:|-----:|
|Telephone     |          46.44|     0.07|         0.00|           0.20|               0.00|  3.32| 49.97|
|Internet      |          66.67|     0.00|         0.00|          16.67|               0.00|  0.00| 16.67|
|Street        |          55.56|     0.00|        20.63|           6.35|               9.52|  1.59|  6.35|
|Premises      |          68.75|    12.50|         0.00|           0.00|               6.25|  0.00| 12.50|
|Cobro de piso |         100.00|     0.00|         0.00|           0.00|               0.00|  0.00|  0.00|
|Other         |          36.36|     3.03|        24.24|           3.03|               6.06|  0.00| 27.27|

## Use of weapon

### Compliance


```r
## basic summary first

t18 <- table(enve_incvic$had_weapon, useNA="ifany")
t18
```

```

   No   Yes DK/DA  <NA> 
  375    44   428   753 
```

```r
round(t18/sum(t18)*100, 2)
```

```

   No   Yes DK/DA  <NA> 
23.44  2.75 26.75 47.06 
```

```r
## first No NA then with NA

## Use complied_bin

t19 <- table(enve_incvic$had_weapon, enve_incvic$complied_bin)
t19
```

```
       
         No Yes
  No    260  31
  Yes    12  23
  DK/DA 288  21
```

```r
kable(t19)
```



|      |  No| Yes|
|:-----|---:|---:|
|No    | 260|  31|
|Yes   |  12|  23|
|DK/DA | 288|  21|

```r
t19/as.integer(margin.table(t19, margin=1))*100
```

```
       
               No       Yes
  No    89.347079 10.652921
  Yes   34.285714 65.714286
  DK/DA 93.203883  6.796117
```

```r
kable(round(t19/as.integer(margin.table(t19, margin=1))*100, 2))
```



|      |    No|   Yes|
|:-----|-----:|-----:|
|No    | 89.35| 10.65|
|Yes   | 34.29| 65.71|
|DK/DA | 93.20|  6.80|

```r
t20 <- table(enve_incvic$had_weapon, enve_incvic$complied_bin, useNA = "ifany")

kable(t20)
```



|      |  No| Yes|  NA|
|:-----|---:|---:|---:|
|No    | 260|  31|  84|
|Yes   |  12|  23|   9|
|DK/DA | 288|  21| 119|
|NA    | 572|  25| 156|

```r
t20/as.integer(margin.table(t20, margin=1))*100
```

```
       
               No       Yes      <NA>
  No    69.333333  8.266667 22.400000
  Yes   27.272727 52.272727 20.454545
  DK/DA 67.289720  4.906542 27.803738
  <NA>  75.962815  3.320053 20.717131
```

```r
kable(round(t20/as.integer(margin.table(t20, margin=1))*100, 2))
```



|      |    No|   Yes|    NA|
|:-----|-----:|-----:|-----:|
|No    | 69.33|  8.27| 22.40|
|Yes   | 27.27| 52.27| 20.45|
|DK/DA | 67.29|  4.91| 27.80|
|NA    | 75.96|  3.32| 20.72|

### Extortion type


```r
t21 <- table(enve_incvic$extortion_type, enve_incvic$had_weapon)
t21
```

```
               
                 No Yes DK/DA
  Telephone     310   2   426
  Internet        3   2     0
  Street         36  21     2
  Premises        4  10     0
  Cobro de piso   5   2     0
  Other          17   7     0
```

```r
kable(t21)
```



|              |  No| Yes| DK/DA|
|:-------------|---:|---:|-----:|
|Telephone     | 310|   2|   426|
|Internet      |   3|   2|     0|
|Street        |  36|  21|     2|
|Premises      |   4|  10|     0|
|Cobro de piso |   5|   2|     0|
|Other         |  17|   7|     0|

```r
t21/as.integer(margin.table(t21, margin=1))*100
```

```
               
                        No        Yes      DK/DA
  Telephone     42.0054201  0.2710027 57.7235772
  Internet      60.0000000 40.0000000  0.0000000
  Street        61.0169492 35.5932203  3.3898305
  Premises      28.5714286 71.4285714  0.0000000
  Cobro de piso 71.4285714 28.5714286  0.0000000
  Other         70.8333333 29.1666667  0.0000000
```

```r
kable(round(t21/as.integer(margin.table(t21, margin=1))*100, 2))
```



|              |    No|   Yes| DK/DA|
|:-------------|-----:|-----:|-----:|
|Telephone     | 42.01|  0.27| 57.72|
|Internet      | 60.00| 40.00|  0.00|
|Street        | 61.02| 35.59|  3.39|
|Premises      | 28.57| 71.43|  0.00|
|Cobro de piso | 71.43| 28.57|  0.00|
|Other         | 70.83| 29.17|  0.00|

```r
t22 <- table(enve_incvic$extortion_type, enve_incvic$had_weapon, useNA="ifany")
t22
```

```
               
                 No Yes DK/DA <NA>
  Telephone     310   2   426  737
  Internet        3   2     0    1
  Street         36  21     2    4
  Premises        4  10     0    2
  Cobro de piso   5   2     0    0
  Other          17   7     0    9
```

```r
kable(t22)
```



|              |  No| Yes| DK/DA|  NA|
|:-------------|---:|---:|-----:|---:|
|Telephone     | 310|   2|   426| 737|
|Internet      |   3|   2|     0|   1|
|Street        |  36|  21|     2|   4|
|Premises      |   4|  10|     0|   2|
|Cobro de piso |   5|   2|     0|   0|
|Other         |  17|   7|     0|   9|

```r
t22/as.integer(margin.table(t22, margin=1))*100
```

```
               
                        No        Yes      DK/DA       <NA>
  Telephone     21.0169492  0.1355932 28.8813559 49.9661017
  Internet      50.0000000 33.3333333  0.0000000 16.6666667
  Street        57.1428571 33.3333333  3.1746032  6.3492063
  Premises      25.0000000 62.5000000  0.0000000 12.5000000
  Cobro de piso 71.4285714 28.5714286  0.0000000  0.0000000
  Other         51.5151515 21.2121212  0.0000000 27.2727273
```

```r
kable(round(t22/as.integer(margin.table(t22, margin=1))*100, 2))
```



|              |    No|   Yes| DK/DA|    NA|
|:-------------|-----:|-----:|-----:|-----:|
|Telephone     | 21.02|  0.14| 28.88| 49.97|
|Internet      | 50.00| 33.33|  0.00| 16.67|
|Street        | 57.14| 33.33|  3.17|  6.35|
|Premises      | 25.00| 62.50|  0.00| 12.50|
|Cobro de piso | 71.43| 28.57|  0.00|  0.00|
|Other         | 51.52| 21.21|  0.00| 27.27|

## Type of weapon

### Compliance


```r
## basic summary first

t23 <- table(enve_incvic$weapon_type, useNA="ifany")
t23
```

```

 None Blunt DK/DA   Gun Knife Other  <NA> 
  375     1   428    34     7     0   755 
```

```r
round(t23/sum(t23)*100, 2)
```

```

 None Blunt DK/DA   Gun Knife Other  <NA> 
23.44  0.06 26.75  2.12  0.44  0.00 47.19 
```

```r
## first No NA then with NA

## Use complied_bin

t24 <- table(enve_incvic$weapon_type, enve_incvic$complied_bin)
t24
```

```
       
         No Yes
  None  260  31
  Blunt   0   0
  DK/DA 288  21
  Gun     8  21
  Knife   2   2
  Other   0   0
```

```r
kable(t24)
```



|      |  No| Yes|
|:-----|---:|---:|
|None  | 260|  31|
|Blunt |   0|   0|
|DK/DA | 288|  21|
|Gun   |   8|  21|
|Knife |   2|   2|
|Other |   0|   0|

```r
t24/as.integer(margin.table(t24, margin=1))*100
```

```
       
               No       Yes
  None  89.347079 10.652921
  Blunt                    
  DK/DA 93.203883  6.796117
  Gun   27.586207 72.413793
  Knife 50.000000 50.000000
  Other                    
```

```r
kable(round(t24/as.integer(margin.table(t24, margin=1))*100, 2))
```



|      |    No|   Yes|
|:-----|-----:|-----:|
|None  | 89.35| 10.65|
|Blunt |   NaN|   NaN|
|DK/DA | 93.20|  6.80|
|Gun   | 27.59| 72.41|
|Knife | 50.00| 50.00|
|Other |   NaN|   NaN|

```r
t25 <- table(enve_incvic$weapon_type, enve_incvic$complied_bin, useNA = "ifany")
t25
```

```
       
         No Yes <NA>
  None  260  31   84
  Blunt   0   0    1
  DK/DA 288  21  119
  Gun     8  21    5
  Knife   2   2    3
  Other   0   0    0
  <NA>  574  25  156
```

```r
kable(t25)
```



|      |  No| Yes|  NA|
|:-----|---:|---:|---:|
|None  | 260|  31|  84|
|Blunt |   0|   0|   1|
|DK/DA | 288|  21| 119|
|Gun   |   8|  21|   5|
|Knife |   2|   2|   3|
|Other |   0|   0|   0|
|NA    | 574|  25| 156|

```r
t25/as.integer(margin.table(t25, margin=1))*100
```

```
       
                No        Yes       <NA>
  None   69.333333   8.266667  22.400000
  Blunt   0.000000   0.000000 100.000000
  DK/DA  67.289720   4.906542  27.803738
  Gun    23.529412  61.764706  14.705882
  Knife  28.571429  28.571429  42.857143
  Other                                 
  <NA>   76.026490   3.311258  20.662252
```

```r
kable(round(t25/as.integer(margin.table(t25, margin=1))*100, 2))
```



|      |    No|   Yes|     NA|
|:-----|-----:|-----:|------:|
|None  | 69.33|  8.27|  22.40|
|Blunt |  0.00|  0.00| 100.00|
|DK/DA | 67.29|  4.91|  27.80|
|Gun   | 23.53| 61.76|  14.71|
|Knife | 28.57| 28.57|  42.86|
|Other |   NaN|   NaN|    NaN|
|NA    | 76.03|  3.31|  20.66|

### Extortion type


```r
t26 <- table(enve_incvic$extortion_type, enve_incvic$weapon_type)
t26
```

```
               
                None Blunt DK/DA Gun Knife Other
  Telephone      310     0   426   1     0     0
  Internet         3     0     0   1     0     0
  Street          36     1     2  13     7     0
  Premises         4     0     0  10     0     0
  Cobro de piso    5     0     0   2     0     0
  Other           17     0     0   7     0     0
```

```r
kable(t26)
```



|              | None| Blunt| DK/DA| Gun| Knife| Other|
|:-------------|----:|-----:|-----:|---:|-----:|-----:|
|Telephone     |  310|     0|   426|   1|     0|     0|
|Internet      |    3|     0|     0|   1|     0|     0|
|Street        |   36|     1|     2|  13|     7|     0|
|Premises      |    4|     0|     0|  10|     0|     0|
|Cobro de piso |    5|     0|     0|   2|     0|     0|
|Other         |   17|     0|     0|   7|     0|     0|

```r
t26/as.integer(margin.table(t26, margin=1))*100
```

```
               
                      None      Blunt      DK/DA        Gun      Knife
  Telephone     42.0624152  0.0000000 57.8018996  0.1356852  0.0000000
  Internet      75.0000000  0.0000000  0.0000000 25.0000000  0.0000000
  Street        61.0169492  1.6949153  3.3898305 22.0338983 11.8644068
  Premises      28.5714286  0.0000000  0.0000000 71.4285714  0.0000000
  Cobro de piso 71.4285714  0.0000000  0.0000000 28.5714286  0.0000000
  Other         70.8333333  0.0000000  0.0000000 29.1666667  0.0000000
               
                     Other
  Telephone      0.0000000
  Internet       0.0000000
  Street         0.0000000
  Premises       0.0000000
  Cobro de piso  0.0000000
  Other          0.0000000
```

```r
kable(round(t26/as.integer(margin.table(t26, margin=1))*100, 2))
```



|              |  None| Blunt| DK/DA|   Gun| Knife| Other|
|:-------------|-----:|-----:|-----:|-----:|-----:|-----:|
|Telephone     | 42.06|  0.00| 57.80|  0.14|  0.00|     0|
|Internet      | 75.00|  0.00|  0.00| 25.00|  0.00|     0|
|Street        | 61.02|  1.69|  3.39| 22.03| 11.86|     0|
|Premises      | 28.57|  0.00|  0.00| 71.43|  0.00|     0|
|Cobro de piso | 71.43|  0.00|  0.00| 28.57|  0.00|     0|
|Other         | 70.83|  0.00|  0.00| 29.17|  0.00|     0|

```r
t27 <- table(enve_incvic$extortion_type, enve_incvic$weapon_type, useNA="ifany")
t27
```

```
               
                None Blunt DK/DA Gun Knife Other <NA>
  Telephone      310     0   426   1     0     0  738
  Internet         3     0     0   1     0     0    2
  Street          36     1     2  13     7     0    4
  Premises         4     0     0  10     0     0    2
  Cobro de piso    5     0     0   2     0     0    0
  Other           17     0     0   7     0     0    9
```

```r
kable(t27)
```



|              | None| Blunt| DK/DA| Gun| Knife| Other|  NA|
|:-------------|----:|-----:|-----:|---:|-----:|-----:|---:|
|Telephone     |  310|     0|   426|   1|     0|     0| 738|
|Internet      |    3|     0|     0|   1|     0|     0|   2|
|Street        |   36|     1|     2|  13|     7|     0|   4|
|Premises      |    4|     0|     0|  10|     0|     0|   2|
|Cobro de piso |    5|     0|     0|   2|     0|     0|   0|
|Other         |   17|     0|     0|   7|     0|     0|   9|

```r
t27/as.integer(margin.table(t27, margin=1))*100
```

```
               
                       None       Blunt       DK/DA         Gun
  Telephone     21.01694915  0.00000000 28.88135593  0.06779661
  Internet      50.00000000  0.00000000  0.00000000 16.66666667
  Street        57.14285714  1.58730159  3.17460317 20.63492063
  Premises      25.00000000  0.00000000  0.00000000 62.50000000
  Cobro de piso 71.42857143  0.00000000  0.00000000 28.57142857
  Other         51.51515152  0.00000000  0.00000000 21.21212121
               
                      Knife       Other        <NA>
  Telephone      0.00000000  0.00000000 50.03389831
  Internet       0.00000000  0.00000000 33.33333333
  Street        11.11111111  0.00000000  6.34920635
  Premises       0.00000000  0.00000000 12.50000000
  Cobro de piso  0.00000000  0.00000000  0.00000000
  Other          0.00000000  0.00000000 27.27272727
```

```r
kable(round(t27/as.integer(margin.table(t27, margin=1))*100, 2))
```



|              |  None| Blunt| DK/DA|   Gun| Knife| Other|    NA|
|:-------------|-----:|-----:|-----:|-----:|-----:|-----:|-----:|
|Telephone     | 21.02|  0.00| 28.88|  0.07|  0.00|     0| 50.03|
|Internet      | 50.00|  0.00|  0.00| 16.67|  0.00|     0| 33.33|
|Street        | 57.14|  1.59|  3.17| 20.63| 11.11|     0|  6.35|
|Premises      | 25.00|  0.00|  0.00| 62.50|  0.00|     0| 12.50|
|Cobro de piso | 71.43|  0.00|  0.00| 28.57|  0.00|     0|  0.00|
|Other         | 51.52|  0.00|  0.00| 21.21|  0.00|     0| 27.27|

## Use of violence

### Compliance


```r
## basic summary first

t28 <- table(enve_incvic$with_violence, useNA="ifany")
t28
```

```

   No   Yes DK/DA  <NA> 
  770    19    58   753 
```

```r
round(t28/sum(t28)*100, 2)
```

```

   No   Yes DK/DA  <NA> 
48.12  1.19  3.62 47.06 
```

```r
## first No NA then with NA

## Use complied_bin

t29 <- table(enve_incvic$with_violence, enve_incvic$complied_bin)
t29
```

```
       
         No Yes
  No    517  64
  Yes     7   7
  DK/DA  36   4
```

```r
kable(t29)
```



|      |  No| Yes|
|:-----|---:|---:|
|No    | 517|  64|
|Yes   |   7|   7|
|DK/DA |  36|   4|

```r
t29/as.integer(margin.table(t29, margin=1))*100
```

```
       
              No      Yes
  No    88.98451 11.01549
  Yes   50.00000 50.00000
  DK/DA 90.00000 10.00000
```

```r
kable(round(t29/as.integer(margin.table(t29, margin=1))*100, 2))
```



|      |    No|   Yes|
|:-----|-----:|-----:|
|No    | 88.98| 11.02|
|Yes   | 50.00| 50.00|
|DK/DA | 90.00| 10.00|

```r
t30 <- table(enve_incvic$with_violence, enve_incvic$complied_bin, useNA = "ifany")
t30
```

```
       
         No Yes <NA>
  No    517  64  189
  Yes     7   7    5
  DK/DA  36   4   18
  <NA>  572  25  156
```

```r
kable(t30)
```



|      |  No| Yes|  NA|
|:-----|---:|---:|---:|
|No    | 517|  64| 189|
|Yes   |   7|   7|   5|
|DK/DA |  36|   4|  18|
|NA    | 572|  25| 156|

```r
t30/as.integer(margin.table(t30, margin=1))*100
```

```
       
               No       Yes      <NA>
  No    67.142857  8.311688 24.545455
  Yes   36.842105 36.842105 26.315789
  DK/DA 62.068966  6.896552 31.034483
  <NA>  75.962815  3.320053 20.717131
```

```r
kable(round(t30/as.integer(margin.table(t30, margin=1))*100, 2))
```



|      |    No|   Yes|    NA|
|:-----|-----:|-----:|-----:|
|No    | 67.14|  8.31| 24.55|
|Yes   | 36.84| 36.84| 26.32|
|DK/DA | 62.07|  6.90| 31.03|
|NA    | 75.96|  3.32| 20.72|

### Extortion type


```r
t31 <- table(enve_incvic$extortion_type, enve_incvic$with_violence)
t31
```

```
               
                 No Yes DK/DA
  Telephone     678   2    58
  Internet        4   1     0
  Street         46  13     0
  Premises       13   1     0
  Cobro de piso   7   0     0
  Other          22   2     0
```

```r
kable(t31)
```



|              |  No| Yes| DK/DA|
|:-------------|---:|---:|-----:|
|Telephone     | 678|   2|    58|
|Internet      |   4|   1|     0|
|Street        |  46|  13|     0|
|Premises      |  13|   1|     0|
|Cobro de piso |   7|   0|     0|
|Other         |  22|   2|     0|

```r
t31/as.integer(margin.table(t31, margin=1))*100
```

```
               
                         No         Yes       DK/DA
  Telephone      91.8699187   0.2710027   7.8590786
  Internet       80.0000000  20.0000000   0.0000000
  Street         77.9661017  22.0338983   0.0000000
  Premises       92.8571429   7.1428571   0.0000000
  Cobro de piso 100.0000000   0.0000000   0.0000000
  Other          91.6666667   8.3333333   0.0000000
```

```r
kable(round(t31/as.integer(margin.table(t31, margin=1))*100, 2))
```



|              |     No|   Yes| DK/DA|
|:-------------|------:|-----:|-----:|
|Telephone     |  91.87|  0.27|  7.86|
|Internet      |  80.00| 20.00|  0.00|
|Street        |  77.97| 22.03|  0.00|
|Premises      |  92.86|  7.14|  0.00|
|Cobro de piso | 100.00|  0.00|  0.00|
|Other         |  91.67|  8.33|  0.00|

```r
t32 <- table(enve_incvic$extortion_type, enve_incvic$with_violence, useNA="ifany")
t32
```

```
               
                 No Yes DK/DA <NA>
  Telephone     678   2    58  737
  Internet        4   1     0    1
  Street         46  13     0    4
  Premises       13   1     0    2
  Cobro de piso   7   0     0    0
  Other          22   2     0    9
```

```r
kable(t32)
```



|              |  No| Yes| DK/DA|  NA|
|:-------------|---:|---:|-----:|---:|
|Telephone     | 678|   2|    58| 737|
|Internet      |   4|   1|     0|   1|
|Street        |  46|  13|     0|   4|
|Premises      |  13|   1|     0|   2|
|Cobro de piso |   7|   0|     0|   0|
|Other         |  22|   2|     0|   9|

```r
t32/as.integer(margin.table(t32, margin=1))*100
```

```
               
                         No         Yes       DK/DA        <NA>
  Telephone      45.9661017   0.1355932   3.9322034  49.9661017
  Internet       66.6666667  16.6666667   0.0000000  16.6666667
  Street         73.0158730  20.6349206   0.0000000   6.3492063
  Premises       81.2500000   6.2500000   0.0000000  12.5000000
  Cobro de piso 100.0000000   0.0000000   0.0000000   0.0000000
  Other          66.6666667   6.0606061   0.0000000  27.2727273
```

```r
kable(round(t32/as.integer(margin.table(t32, margin=1))*100, 2))
```



|              |     No|   Yes| DK/DA|    NA|
|:-------------|------:|-----:|-----:|-----:|
|Telephone     |  45.97|  0.14|  3.93| 49.97|
|Internet      |  66.67| 16.67|  0.00| 16.67|
|Street        |  73.02| 20.63|  0.00|  6.35|
|Premises      |  81.25|  6.25|  0.00| 12.50|
|Cobro de piso | 100.00|  0.00|  0.00|  0.00|
|Other         |  66.67|  6.06|  0.00| 27.27|

## Reported to the authorities


```r
## basic summary first

t33 <- table(enve_incvic$reported, useNA="ifany")
t33
```

```

   No   Yes DK/DA 
 1415   185     0 
```

```r
round(t33/sum(t33)*100, 2)
```

```

   No   Yes DK/DA 
88.44 11.56  0.00 
```

```r
## first No NA then with NA

## Use complied_bin

t34 <- table(enve_incvic$reported, enve_incvic$complied_bin)
t34
```

```
       
         No Yes
  No    995  82
  Yes   137  18
  DK/DA   0   0
```

```r
kable(t34)
```



|      |  No| Yes|
|:-----|---:|---:|
|No    | 995|  82|
|Yes   | 137|  18|
|DK/DA |   0|   0|

```r
t34/as.integer(margin.table(t34, margin=1))*100
```

```
       
               No       Yes
  No    92.386258  7.613742
  Yes   88.387097 11.612903
  DK/DA                    
```

```r
kable(round(t34/as.integer(margin.table(t34, margin=1))*100, 2))
```



|      |    No|   Yes|
|:-----|-----:|-----:|
|No    | 92.39|  7.61|
|Yes   | 88.39| 11.61|
|DK/DA |   NaN|   NaN|

```r
t35 <- table(enve_incvic$reported, enve_incvic$complied_bin, useNA = "ifany")
t35
```

```
       
         No Yes <NA>
  No    995  82  338
  Yes   137  18   30
  DK/DA   0   0    0
```

```r
kable(t35)
```



|      |  No| Yes|  NA|
|:-----|---:|---:|---:|
|No    | 995|  82| 338|
|Yes   | 137|  18|  30|
|DK/DA |   0|   0|   0|

```r
t35/as.integer(margin.table(t35, margin=1))*100
```

```
       
               No       Yes      <NA>
  No    70.318021  5.795053 23.886926
  Yes   74.054054  9.729730 16.216216
  DK/DA                              
```

```r
kable(round(t35/as.integer(margin.table(t35, margin=1))*100, 2))
```



|      |    No|  Yes|    NA|
|:-----|-----:|----:|-----:|
|No    | 70.32| 5.80| 23.89|
|Yes   | 74.05| 9.73| 16.22|
|DK/DA |   NaN|  NaN|   NaN|

### Extortion type


```r
t36 <- table(enve_incvic$extortion_type, enve_incvic$reported)
t36
```

```
               
                  No  Yes DK/DA
  Telephone     1315  160     0
  Internet         5    1     0
  Street          49   14     0
  Premises        13    3     0
  Cobro de piso    7    0     0
  Other           26    7     0
```

```r
kable(t36)
```



|              |   No| Yes| DK/DA|
|:-------------|----:|---:|-----:|
|Telephone     | 1315| 160|     0|
|Internet      |    5|   1|     0|
|Street        |   49|  14|     0|
|Premises      |   13|   3|     0|
|Cobro de piso |    7|   0|     0|
|Other         |   26|   7|     0|

```r
t36/as.integer(margin.table(t36, margin=1))*100
```

```
               
                       No       Yes     DK/DA
  Telephone      89.15254  10.84746   0.00000
  Internet       83.33333  16.66667   0.00000
  Street         77.77778  22.22222   0.00000
  Premises       81.25000  18.75000   0.00000
  Cobro de piso 100.00000   0.00000   0.00000
  Other          78.78788  21.21212   0.00000
```

```r
kable(round(t36/as.integer(margin.table(t36, margin=1))*100, 2))
```



|              |     No|   Yes| DK/DA|
|:-------------|------:|-----:|-----:|
|Telephone     |  89.15| 10.85|     0|
|Internet      |  83.33| 16.67|     0|
|Street        |  77.78| 22.22|     0|
|Premises      |  81.25| 18.75|     0|
|Cobro de piso | 100.00|  0.00|     0|
|Other         |  78.79| 21.21|     0|

```r
t37 <- table(enve_incvic$extortion_type, enve_incvic$reported, useNA="ifany")
t37
```

```
               
                  No  Yes DK/DA
  Telephone     1315  160     0
  Internet         5    1     0
  Street          49   14     0
  Premises        13    3     0
  Cobro de piso    7    0     0
  Other           26    7     0
```

```r
kable(t37)
```



|              |   No| Yes| DK/DA|
|:-------------|----:|---:|-----:|
|Telephone     | 1315| 160|     0|
|Internet      |    5|   1|     0|
|Street        |   49|  14|     0|
|Premises      |   13|   3|     0|
|Cobro de piso |    7|   0|     0|
|Other         |   26|   7|     0|

```r
t37/as.integer(margin.table(t37, margin=1))*100
```

```
               
                       No       Yes     DK/DA
  Telephone      89.15254  10.84746   0.00000
  Internet       83.33333  16.66667   0.00000
  Street         77.77778  22.22222   0.00000
  Premises       81.25000  18.75000   0.00000
  Cobro de piso 100.00000   0.00000   0.00000
  Other          78.78788  21.21212   0.00000
```

```r
kable(round(t37/as.integer(margin.table(t37, margin=1))*100, 2))
```



|              |     No|   Yes| DK/DA|
|:-------------|------:|-----:|-----:|
|Telephone     |  89.15| 10.85|     0|
|Internet      |  83.33| 16.67|     0|
|Street        |  77.78| 22.22|     0|
|Premises      |  81.25| 18.75|     0|
|Cobro de piso | 100.00|  0.00|     0|
|Other         |  78.79| 21.21|     0|

## Requested items

### Compliance


```r
## basic summary first

t38 <- table(enve_incvic$request, useNA="ifany")
t38
```

```

  Other   Money Nothing Product 
    363    1138      63      36 
```

```r
round(t38/sum(t38)*100, 2)
```

```

  Other   Money Nothing Product 
  22.69   71.12    3.94    2.25 
```

```r
## first No NA then with NA

## Use complied_bin

t39 <- table(enve_incvic$request, enve_incvic$complied_bin)
t39
```

```
         
            No  Yes
  Other      3    0
  Money   1047   88
  Nothing   51    8
  Product   31    4
```

```r
kable(t39)
```



|        |   No| Yes|
|:-------|----:|---:|
|Other   |    3|   0|
|Money   | 1047|  88|
|Nothing |   51|   8|
|Product |   31|   4|

```r
t39/as.integer(margin.table(t39, margin=1))*100
```

```
         
                  No        Yes
  Other   100.000000   0.000000
  Money    92.246696   7.753304
  Nothing  86.440678  13.559322
  Product  88.571429  11.428571
```

```r
kable(round(t39/as.integer(margin.table(t39, margin=1))*100, 2))
```



|        |     No|   Yes|
|:-------|------:|-----:|
|Other   | 100.00|  0.00|
|Money   |  92.25|  7.75|
|Nothing |  86.44| 13.56|
|Product |  88.57| 11.43|

```r
t40 <- table(enve_incvic$request, enve_incvic$complied_bin, useNA = "ifany")
t40
```

```
         
            No  Yes <NA>
  Other      3    0  360
  Money   1047   88    3
  Nothing   51    8    4
  Product   31    4    1
```

```r
kable(t40)
```



|        |   No| Yes|  NA|
|:-------|----:|---:|---:|
|Other   |    3|   0| 360|
|Money   | 1047|  88|   3|
|Nothing |   51|   8|   4|
|Product |   31|   4|   1|

```r
t40/as.integer(margin.table(t40, margin=1))*100
```

```
         
                  No        Yes       <NA>
  Other    0.8264463  0.0000000 99.1735537
  Money   92.0035149  7.7328647  0.2636204
  Nothing 80.9523810 12.6984127  6.3492063
  Product 86.1111111 11.1111111  2.7777778
```

```r
kable(round(t40/as.integer(margin.table(t40, margin=1))*100, 2))
```



|        |    No|   Yes|    NA|
|:-------|-----:|-----:|-----:|
|Other   |  0.83|  0.00| 99.17|
|Money   | 92.00|  7.73|  0.26|
|Nothing | 80.95| 12.70|  6.35|
|Product | 86.11| 11.11|  2.78|

### Extortion type


```r
t41 <- table(enve_incvic$extortion_type, enve_incvic$request)
t41
```

```
               
                Other Money Nothing Product
  Telephone       338  1063      39      35
  Internet          2     2       2       0
  Street           16    36      11       0
  Premises          2    13       1       0
  Cobro de piso     0     7       0       0
  Other             5    17      10       1
```

```r
kable(t41)
```



|              | Other| Money| Nothing| Product|
|:-------------|-----:|-----:|-------:|-------:|
|Telephone     |   338|  1063|      39|      35|
|Internet      |     2|     2|       2|       0|
|Street        |    16|    36|      11|       0|
|Premises      |     2|    13|       1|       0|
|Cobro de piso |     0|     7|       0|       0|
|Other         |     5|    17|      10|       1|

```r
t41/as.integer(margin.table(t41, margin=1))*100
```

```
               
                     Other      Money    Nothing    Product
  Telephone      22.915254  72.067797   2.644068   2.372881
  Internet       33.333333  33.333333  33.333333   0.000000
  Street         25.396825  57.142857  17.460317   0.000000
  Premises       12.500000  81.250000   6.250000   0.000000
  Cobro de piso   0.000000 100.000000   0.000000   0.000000
  Other          15.151515  51.515152  30.303030   3.030303
```

```r
kable(round(t41/as.integer(margin.table(t41, margin=1))*100, 2))
```



|              | Other|  Money| Nothing| Product|
|:-------------|-----:|------:|-------:|-------:|
|Telephone     | 22.92|  72.07|    2.64|    2.37|
|Internet      | 33.33|  33.33|   33.33|    0.00|
|Street        | 25.40|  57.14|   17.46|    0.00|
|Premises      | 12.50|  81.25|    6.25|    0.00|
|Cobro de piso |  0.00| 100.00|    0.00|    0.00|
|Other         | 15.15|  51.52|   30.30|    3.03|

```r
t42 <- table(enve_incvic$extortion_type, enve_incvic$request, useNA="ifany")
t42
```

```
               
                Other Money Nothing Product
  Telephone       338  1063      39      35
  Internet          2     2       2       0
  Street           16    36      11       0
  Premises          2    13       1       0
  Cobro de piso     0     7       0       0
  Other             5    17      10       1
```

```r
kable(t42)
```



|              | Other| Money| Nothing| Product|
|:-------------|-----:|-----:|-------:|-------:|
|Telephone     |   338|  1063|      39|      35|
|Internet      |     2|     2|       2|       0|
|Street        |    16|    36|      11|       0|
|Premises      |     2|    13|       1|       0|
|Cobro de piso |     0|     7|       0|       0|
|Other         |     5|    17|      10|       1|

```r
t42/as.integer(margin.table(t42, margin=1))*100
```

```
               
                     Other      Money    Nothing    Product
  Telephone      22.915254  72.067797   2.644068   2.372881
  Internet       33.333333  33.333333  33.333333   0.000000
  Street         25.396825  57.142857  17.460317   0.000000
  Premises       12.500000  81.250000   6.250000   0.000000
  Cobro de piso   0.000000 100.000000   0.000000   0.000000
  Other          15.151515  51.515152  30.303030   3.030303
```

```r
kable(round(t42/as.integer(margin.table(t42, margin=1))*100, 2))
```



|              | Other|  Money| Nothing| Product|
|:-------------|-----:|------:|-------:|-------:|
|Telephone     | 22.92|  72.07|    2.64|    2.37|
|Internet      | 33.33|  33.33|   33.33|    0.00|
|Street        | 25.40|  57.14|   17.46|    0.00|
|Premises      | 12.50|  81.25|    6.25|    0.00|
|Cobro de piso |  0.00| 100.00|    0.00|    0.00|
|Other         | 15.15|  51.52|   30.30|    3.03|

## Retaliation

### Compliance


```r
## basic summary first

t43 <- table(enve_incvic$retaliation, useNA="ifany")
t43
```

```

   No   Yes DK/DA  <NA> 
    3     2     2  1593 
```

```r
round(t43/sum(t43)*100, 2)
```

```

   No   Yes DK/DA  <NA> 
 0.19  0.12  0.12 99.56 
```

```r
t44 <- table(enve_incvic$retaliation, useNA="no")
t44
```

```

   No   Yes DK/DA 
    3     2     2 
```

```r
round(t44/sum(t44)*100, 2)
```

```

   No   Yes DK/DA 
42.86 28.57 28.57 
```

```r
## first No NA then with NA

## Use complied_bin

t45 <- table(enve_incvic$retaliation, enve_incvic$complied_bin)
t45
```

```
       
        No Yes
  No     3   0
  Yes    2   0
  DK/DA  1   1
```

```r
kable(t45)
```



|      | No| Yes|
|:-----|--:|---:|
|No    |  3|   0|
|Yes   |  2|   0|
|DK/DA |  1|   1|

```r
t45/as.integer(margin.table(t45, margin=1))*100
```

```
       
         No Yes
  No    100   0
  Yes   100   0
  DK/DA  50  50
```

```r
kable(round(t45/as.integer(margin.table(t45, margin=1))*100, 2))
```



|      |  No| Yes|
|:-----|---:|---:|
|No    | 100|   0|
|Yes   | 100|   0|
|DK/DA |  50|  50|

```r
t46 <- table(enve_incvic$retaliation, enve_incvic$complied_bin, useNA = "ifany")
t46
```

```
       
          No  Yes <NA>
  No       3    0    0
  Yes      2    0    0
  DK/DA    1    1    0
  <NA>  1126   99  368
```

```r
kable(t46)
```



|      |   No| Yes|  NA|
|:-----|----:|---:|---:|
|No    |    3|   0|   0|
|Yes   |    2|   0|   0|
|DK/DA |    1|   1|   0|
|NA    | 1126|  99| 368|

```r
t46/as.integer(margin.table(t46, margin=1))*100
```

```
       
                No        Yes       <NA>
  No    100.000000   0.000000   0.000000
  Yes   100.000000   0.000000   0.000000
  DK/DA  50.000000  50.000000   0.000000
  <NA>   70.684244   6.214689  23.101067
```

```r
kable(round(t46/as.integer(margin.table(t46, margin=1))*100, 2))
```



|      |     No|   Yes|   NA|
|:-----|------:|-----:|----:|
|No    | 100.00|  0.00|  0.0|
|Yes   | 100.00|  0.00|  0.0|
|DK/DA |  50.00| 50.00|  0.0|
|NA    |  70.68|  6.21| 23.1|

### Extortion type


```r
t47 <- table(enve_incvic$extortion_type, enve_incvic$retaliation)
t47
```

```
               
                No Yes DK/DA
  Telephone      2   2     1
  Internet       0   0     0
  Street         0   0     0
  Premises       0   0     1
  Cobro de piso  0   0     0
  Other          1   0     0
```

```r
kable(t47)
```



|              | No| Yes| DK/DA|
|:-------------|--:|---:|-----:|
|Telephone     |  2|   2|     1|
|Internet      |  0|   0|     0|
|Street        |  0|   0|     0|
|Premises      |  0|   0|     1|
|Cobro de piso |  0|   0|     0|
|Other         |  1|   0|     0|

```r
t47/as.integer(margin.table(t47, margin=1))*100
```

```
               
                 No Yes DK/DA
  Telephone      40  40    20
  Internet                   
  Street                     
  Premises        0   0   100
  Cobro de piso              
  Other         100   0     0
```

```r
kable(round(t47/as.integer(margin.table(t47, margin=1))*100, 2))
```



|              |  No| Yes| DK/DA|
|:-------------|---:|---:|-----:|
|Telephone     |  40|  40|    20|
|Internet      | NaN| NaN|   NaN|
|Street        | NaN| NaN|   NaN|
|Premises      |   0|   0|   100|
|Cobro de piso | NaN| NaN|   NaN|
|Other         | 100|   0|     0|

```r
t48 <- table(enve_incvic$extortion_type, enve_incvic$retaliation, useNA="ifany")
t48
```

```
               
                  No  Yes DK/DA <NA>
  Telephone        2    2     1 1470
  Internet         0    0     0    6
  Street           0    0     0   63
  Premises         0    0     1   15
  Cobro de piso    0    0     0    7
  Other            1    0     0   32
```

```r
kable(t48)
```



|              | No| Yes| DK/DA|   NA|
|:-------------|--:|---:|-----:|----:|
|Telephone     |  2|   2|     1| 1470|
|Internet      |  0|   0|     0|    6|
|Street        |  0|   0|     0|   63|
|Premises      |  0|   0|     1|   15|
|Cobro de piso |  0|   0|     0|    7|
|Other         |  1|   0|     0|   32|

```r
t48/as.integer(margin.table(t48, margin=1))*100
```

```
               
                          No          Yes        DK/DA         <NA>
  Telephone       0.13559322   0.13559322   0.06779661  99.66101695
  Internet        0.00000000   0.00000000   0.00000000 100.00000000
  Street          0.00000000   0.00000000   0.00000000 100.00000000
  Premises        0.00000000   0.00000000   6.25000000  93.75000000
  Cobro de piso   0.00000000   0.00000000   0.00000000 100.00000000
  Other           3.03030303   0.00000000   0.00000000  96.96969697
```

```r
kable(round(t48/as.integer(margin.table(t48, margin=1))*100, 2))
```



|              |   No|  Yes| DK/DA|     NA|
|:-------------|----:|----:|-----:|------:|
|Telephone     | 0.14| 0.14|  0.07|  99.66|
|Internet      | 0.00| 0.00|  0.00| 100.00|
|Street        | 0.00| 0.00|  0.00| 100.00|
|Premises      | 0.00| 0.00|  6.25|  93.75|
|Cobro de piso | 0.00| 0.00|  0.00| 100.00|
|Other         | 3.03| 0.00|  0.00|  96.97|

## Univariate and Bivariate analysis of Compliance and Extortion type

Now we run some univariate and bivariate analysis of extortion compliance and extortion type.

### Univariate

#### Compliance


```r
t49 <- table(enve_incvic$complied, useNA="ifany")
t49
```

```

   No   Yes DK/DA  <NA> 
 1132   100     8   360 
```

```r
round(t49/sum(t49)*100, 2)
```

```

   No   Yes DK/DA  <NA> 
70.75  6.25  0.50 22.50 
```

```r
t50 <- table(enve_incvic$complied, useNA="no")
t50
```

```

   No   Yes DK/DA 
 1132   100     8 
```

```r
round(t50/sum(t50)*100, 2)
```

```

   No   Yes DK/DA 
91.29  8.06  0.65 
```

```r
t51 <- table(enve_incvic$complied_bin, useNA="ifany")
t51
```

```

  No  Yes <NA> 
1132  100  368 
```

```r
round(t51/sum(t51)*100, 2)
```

```

   No   Yes  <NA> 
70.75  6.25 23.00 
```

```r
t52 <- table(enve_incvic$complied_bin, useNA="no")
t52
```

```

  No  Yes 
1132  100 
```

```r
round(t52/sum(t52)*100, 2)
```

```

   No   Yes 
91.88  8.12 
```

#### Extortion Type


```r
t53 <- table(enve_incvic$extortion_type, useNA="ifany")
t53
```

```

    Telephone      Internet        Street      Premises Cobro de piso 
         1475             6            63            16             7 
        Other 
           33 
```

```r
round(t53/sum(t53)*100, 2)
```

```

    Telephone      Internet        Street      Premises Cobro de piso 
        92.19          0.38          3.94          1.00          0.44 
        Other 
         2.06 
```

```r
t54 <- table(enve_incvic$extortion_type, useNA="no")
t54
```

```

    Telephone      Internet        Street      Premises Cobro de piso 
         1475             6            63            16             7 
        Other 
           33 
```

```r
round(t54/sum(t54)*100, 2)
```

```

    Telephone      Internet        Street      Premises Cobro de piso 
        92.19          0.38          3.94          1.00          0.44 
        Other 
         2.06 
```

### Bivariate

#### Compliance vs. Extortion type


```r
t55 <- table(enve_incvic$extortion_type, enve_incvic$complied_bin)
t55
```

```
               
                  No  Yes
  Telephone     1078   55
  Internet         3    1
  Street          29   19
  Premises         1   13
  Cobro de piso    3    4
  Other           18    8
```

```r
kable(t55)
```



|              |   No| Yes|
|:-------------|----:|---:|
|Telephone     | 1078|  55|
|Internet      |    3|   1|
|Street        |   29|  19|
|Premises      |    1|  13|
|Cobro de piso |    3|   4|
|Other         |   18|   8|

```r
t55/as.integer(margin.table(t55, margin=1))*100
```

```
               
                       No       Yes
  Telephone     95.145631  4.854369
  Internet      75.000000 25.000000
  Street        60.416667 39.583333
  Premises       7.142857 92.857143
  Cobro de piso 42.857143 57.142857
  Other         69.230769 30.769231
```

```r
kable(round(t55/as.integer(margin.table(t55, margin=1))*100, 2))
```



|              |    No|   Yes|
|:-------------|-----:|-----:|
|Telephone     | 95.15|  4.85|
|Internet      | 75.00| 25.00|
|Street        | 60.42| 39.58|
|Premises      |  7.14| 92.86|
|Cobro de piso | 42.86| 57.14|
|Other         | 69.23| 30.77|

```r
t56 <- table(enve_incvic$extortion_type, enve_incvic$complied_bin, useNA="ifany")
t56
```

```
               
                  No  Yes <NA>
  Telephone     1078   55  342
  Internet         3    1    2
  Street          29   19   15
  Premises         1   13    2
  Cobro de piso    3    4    0
  Other           18    8    7
```

```r
kable(t56)
```



|              |   No| Yes|  NA|
|:-------------|----:|---:|---:|
|Telephone     | 1078|  55| 342|
|Internet      |    3|   1|   2|
|Street        |   29|  19|  15|
|Premises      |    1|  13|   2|
|Cobro de piso |    3|   4|   0|
|Other         |   18|   8|   7|

```r
t56/as.integer(margin.table(t56, margin=1))*100
```

```
               
                       No       Yes      <NA>
  Telephone     73.084746  3.728814 23.186441
  Internet      50.000000 16.666667 33.333333
  Street        46.031746 30.158730 23.809524
  Premises       6.250000 81.250000 12.500000
  Cobro de piso 42.857143 57.142857  0.000000
  Other         54.545455 24.242424 21.212121
```

```r
kable(round(t56/as.integer(margin.table(t56, margin=1))*100, 2))
```



|              |    No|   Yes|    NA|
|:-------------|-----:|-----:|-----:|
|Telephone     | 73.08|  3.73| 23.19|
|Internet      | 50.00| 16.67| 33.33|
|Street        | 46.03| 30.16| 23.81|
|Premises      |  6.25| 81.25| 12.50|
|Cobro de piso | 42.86| 57.14|  0.00|
|Other         | 54.55| 24.24| 21.21|

## Complete cases


```r
### total rows of complete cases and excluding certain variables

a <- "complete cases"

paste(length(which(complete.cases(enve_incvic) == TRUE)), a)
```

```
[1] "0 complete cases"
```

```r
## Subsets

### those for sure excluded

excnames <- quote(c(gun,knife,blunt,other,violenceI,violenceII,mp,auto))

paste(length(which(complete.cases(subset(enve_incvic, select=-eval(excnames))) == TRUE)), a)
```

```
[1] "5 complete cases"
```

```r
### excluding retailiation too

excnames <- quote(c(gun,knife,blunt,other,violenceI,violenceII,mp,auto,retaliation))

paste(length(which(complete.cases(subset(enve_incvic, select=-eval(excnames))) == TRUE)), a)
```

```
[1] "633 complete cases"
```

```r
### excluding offender related variables

excnames <- quote(c(gun,knife,blunt,other,violenceI,violenceII,mp,auto,
                    retaliation,n_offenders,rel_offenders))

paste(length(which(complete.cases(subset(enve_incvic, select=-eval(excnames))) == TRUE)), a)
```

```
[1] "633 complete cases"
```

```r
### excluding weapons

excnames <- quote(c(gun,knife,blunt,other,violenceI,violenceII,mp,auto,
                    retaliation,weapon_type))

paste(length(which(complete.cases(subset(enve_incvic, select=-eval(excnames))) == TRUE)), a)
```

```
[1] "635 complete cases"
```

```r
excnames <- quote(c(gun,knife,blunt,other,violenceI,violenceII,mp,auto,
                    retaliation,weapon_type,had_weapon))

paste(length(which(complete.cases(subset(enve_incvic, select=-eval(excnames))) == TRUE)), a)
```

```
[1] "635 complete cases"
```

```r
### excluding weapons and offender related

excnames <- quote(c(gun,knife,blunt,other,violenceI,violenceII,mp,auto,
                    retaliation,n_offenders,rel_offenders,weapon_type))

paste(length(which(complete.cases(subset(enve_incvic, select=-eval(excnames))) == TRUE)), a)
```

```
[1] "635 complete cases"
```

```r
excnames <- quote(c(gun,knife,blunt,other,violenceI,violenceII,mp,auto,
                    retaliation,n_offenders,rel_offenders,weapon_type,had_weapon))

paste(length(which(complete.cases(subset(enve_incvic, select=-eval(excnames))) == TRUE)), a)
```

```
[1] "635 complete cases"
```

```r
### excluding with violence

excnames <- quote(c(gun,knife,blunt,other,violenceI,violenceII,mp,auto,
                    retaliation,with_violence))

paste(length(which(complete.cases(subset(enve_incvic, select=-eval(excnames))) == TRUE)), a)
```

```
[1] "633 complete cases"
```

```r
### excluding with violence and offender and weapons

excnames <- quote(c(gun,knife,blunt,other,violenceI,violenceII,mp,auto,
                    retaliation,n_offenders,rel_offenders,weapon_type,
                    with_violence,had_weapon))

paste(length(which(complete.cases(subset(enve_incvic, select=-eval(excnames))) == TRUE)), a)
```

```
[1] "1232 complete cases"
```

## Chi-square tests

Chi-square tests for all the tables calculated


```r
for (i in 1:56)
{
  print(paste("t",i,sep=""))
  print(chisq.test(get(paste("t",i,sep=""))))
  print("expected")
  print(chisq.test(get(paste("t",i,sep="")))$expected)
  #print(kable(chisq.test(get(paste("t",i,sep="")))$expected))
  print("simulated p-value")
  print(chisq.test(get(paste("t",i,sep="")),simulate.p.value = TRUE, B = 9999))
  print("Cramer's V'")
  print(cv.test(get(paste("t",i,sep=""))))
}
```

```
[1] "t1"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 4.9337, df = 12, p-value = 0.9602

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
           
                   No       Yes
  January    67.99351  6.006494
  February   87.28896  7.711039
  March      96.47727  8.522727
  April      58.80519  5.194805
  May        84.53247  7.467532
  June      109.34091  9.659091
  July       66.15584  5.844156
  August     79.93831  7.061688
  September  96.47727  8.522727
  October    85.45130  7.548701
  November  132.31169 11.688312
  December  128.63636 11.363636
  NONE       38.59091  3.409091
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 4.9337, df = NA, p-value = 0.9652

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.06328189
[1] "t2"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 27.511, df = 24, p-value = 0.2812

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
           
                  No     Yes  <NA>
  January    60.1375  5.3125 19.55
  February   84.9000  7.5000 27.60
  March      90.5600  8.0000 29.44
  April      58.7225  5.1875 19.09
  May        88.4375  7.8125 28.75
  June      114.6150 10.1250 37.26
  July       78.5325  6.9375 25.53
  August     79.9475  7.0625 25.99
  September  93.3900  8.2500 30.36
  October    89.1450  7.8750 28.98
  November  125.2275 11.0625 40.71
  December  126.6425 11.1875 41.17
  NONE       41.7425  3.6875 13.57
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 27.511, df = NA, p-value = 0.2734

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.09272043
[1] "t3"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 58.364, df = 60, p-value = 0.5357

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                  January February  March     April        May      June
  Telephone     78.359375  110.625 118.00 76.515625 115.234375 149.34375
  Internet       0.318750    0.450   0.48  0.311250   0.468750   0.60750
  Street         3.346875    4.725   5.04  3.268125   4.921875   6.37875
  Premises       0.850000    1.200   1.28  0.830000   1.250000   1.62000
  Cobro de piso  0.371875    0.525   0.56  0.363125   0.546875   0.70875
  Other          1.753125    2.475   2.64  1.711875   2.578125   3.34125
               
                      July     August September   October   November
  Telephone     102.328125 104.171875  121.6875 116.15625 163.171875
  Internet        0.416250   0.423750    0.4950   0.47250   0.663750
  Street          4.370625   4.449375    5.1975   4.96125   6.969375
  Premises        1.110000   1.130000    1.3200   1.26000   1.770000
  Cobro de piso   0.485625   0.494375    0.5775   0.55125   0.774375
  Other           2.289375   2.330625    2.7225   2.59875   3.650625
               
                  December      NONE
  Telephone     165.015625 54.390625
  Internet        0.671250  0.221250
  Street          7.048125  2.323125
  Premises        1.790000  0.590000
  Cobro de piso   0.783125  0.258125
  Other           3.691875  1.216875
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 58.364, df = NA, p-value = 0.5319

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.08541406
[1] "t4"

	Chi-squared test for given probabilities

data:  get(paste("t", i, sep = ""))
X-squared = 1468.4, df = 4, p-value < 0.00000000000000022

[1] "expected"
  Morning Afternoon   Evening     Night     DK/DA 
      320       320       320       320       320 
[1] "simulated p-value"

	Chi-squared test for given probabilities with simulated p-value
	(based on 9999 replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 1468.4, df = NA, p-value = 0.0001

[1] "Cramer's V'"
[1] NA
[1] "t5"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 4.1138, df = 4, p-value = 0.3908

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
           
                   No       Yes
  Morning   362.01948 31.980519
  Afternoon 563.24351 49.756494
  Evening   183.76623 16.233766
  Night      11.02597  0.974026
  DK/DA      11.94481  1.055195
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 4.1138, df = NA, p-value = 0.3638

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.05778523
[1] "t6"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 18.854, df = 8, p-value = 0.01566

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
           
                 No    Yes   <NA>
  Morning   349.505 30.875 113.62
  Afternoon 581.565 51.375 189.06
  Evening   175.460 15.500  57.04
  Night      15.565  1.375   5.06
  DK/DA       9.905  0.875   3.22
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 18.854, df = NA, p-value = 0.0186

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.07675956
[1] "t7"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 41.964, df = 20, p-value = 0.002795

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                  Morning Afternoon Evening    Night    DK/DA
  Telephone     455.40625 757.78125 228.625 20.28125 12.90625
  Internet        1.85250   3.08250   0.930  0.08250  0.05250
  Street         19.45125  32.36625   9.765  0.86625  0.55125
  Premises        4.94000   8.22000   2.480  0.22000  0.14000
  Cobro de piso   2.16125   3.59625   1.085  0.09625  0.06125
  Other          10.18875  16.95375   5.115  0.45375  0.28875
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 41.964, df = NA, p-value = 0.0327

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.08097473
[1] "t8"

	Chi-squared test for given probabilities

data:  get(paste("t", i, sep = ""))
X-squared = 3012.1, df = 7, p-value < 0.00000000000000022

[1] "expected"
    1     2     3     4     5    6+ DK/DA  <NA> 
  200   200   200   200   200   200   200   200 
[1] "simulated p-value"

	Chi-squared test for given probabilities with simulated p-value
	(based on 9999 replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 3012.1, df = NA, p-value = 0.0001

[1] "Cramer's V'"
[1] NA
[1] "t9"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 100.42, df = 6, p-value < 0.00000000000000022

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
       
                No        Yes
  1     370.393701 49.6062992
  2     108.472441 14.5275591
  3      27.338583  3.6614173
  4       6.173228  0.8267717
  5       7.937008  1.0629921
  6+      7.937008  1.0629921
  DK/DA  31.748031  4.2519685
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 100.42, df = NA, p-value = 0.0001

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.3976772
[1] "t10"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 189.09, df = 14, p-value < 0.00000000000000022

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
       
              No     Yes   <NA>
  1     403.2750 35.6250 131.10
  2     115.3225 10.1875  37.49
  3      26.8850  2.3750   8.74
  4       6.3675  0.5625   2.07
  5       7.0750  0.6250   2.30
  6+      7.0750  0.6250   2.30
  DK/DA  33.2525  2.9375  10.81
  <NA>  532.7475 47.0625 173.19
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 189.09, df = NA, p-value = 0.0001

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.2430853
[1] "t11"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 332.11, df = 30, p-value < 0.00000000000000022

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                         1           2          3          4          5
  Telephone     496.646989 142.0236128 33.1097993 7.84179457 8.71310508
  Internet        3.364817   0.9622196  0.2243211 0.05312869 0.05903188
  Street         39.704841  11.3541913  2.6469894 0.62691854 0.69657615
  Premises        9.421488   2.6942149  0.6280992 0.14876033 0.16528926
  Cobro de piso   4.710744   1.3471074  0.3140496 0.07438017 0.08264463
  Other          16.151122   4.6186541  1.0767414 0.25501771 0.28335301
               
                        6+      DK/DA
  Telephone     8.71310508 40.9515939
  Internet      0.05903188  0.2774498
  Street        0.69657615  3.2739079
  Premises      0.16528926  0.7768595
  Cobro de piso 0.08264463  0.3884298
  Other         0.28335301  1.3317591
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 332.11, df = NA, p-value = 0.0001

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.2800352
[1] "t12"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 587.58, df = 35, p-value < 0.00000000000000022

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                        1          2        3        4       5      6+
  Telephone     525.46875 150.265625 35.03125 8.296875 9.21875 9.21875
  Internet        2.13750   0.611250  0.14250 0.033750 0.03750 0.03750
  Street         22.44375   6.418125  1.49625 0.354375 0.39375 0.39375
  Premises        5.70000   1.630000  0.38000 0.090000 0.10000 0.10000
  Cobro de piso   2.49375   0.713125  0.16625 0.039375 0.04375 0.04375
  Other          11.75625   3.361875  0.78375 0.185625 0.20625 0.20625
               
                    DK/DA       <NA>
  Telephone     43.328125 694.171875
  Internet       0.176250   2.823750
  Street         1.850625  29.649375
  Premises       0.470000   7.530000
  Cobro de piso  0.205625   3.294375
  Other          0.969375  15.530625
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 587.58, df = NA, p-value = 0.0001

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.2710121
[1] "t13"

	Chi-squared test for given probabilities

data:  get(paste("t", i, sep = ""))
X-squared = 3381.6, df = 6, p-value < 0.00000000000000022

[1] "expected"
    Total stranger           Employee       Barely known 
          228.5714           228.5714           228.5714 
    Somewhat known Close acquaintance              DK/DA 
          228.5714           228.5714           228.5714 
              <NA> 
          228.5714 
[1] "simulated p-value"

	Chi-squared test for given probabilities with simulated p-value
	(based on 9999 replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 3381.6, df = NA, p-value = 0.0001

[1] "Cramer's V'"
[1] NA
[1] "t14"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 14.802, df = 5, p-value = 0.01124

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
                    
                             No        Yes
  Total stranger     500.031496 66.9685039
  Employee             2.645669  0.3543307
  Barely known        15.874016  2.1259843
  Somewhat known       4.409449  0.5905512
  Close acquaintance   3.527559  0.4724409
  DK/DA               33.511811  4.4881890
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 14.802, df = NA, p-value = 0.0242

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.1526764
[1] "t15"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 53.803, df = 12, p-value = 0.0000002963

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
                    
                           No     Yes   <NA>
  Total stranger     533.4550 47.1250 173.42
  Employee             2.8300  0.2500   0.92
  Barely known        14.8575  1.3125   4.83
  Somewhat known       6.3675  0.5625   2.07
  Close acquaintance   6.3675  0.5625   2.07
  DK/DA               35.3750  3.1250  11.50
  <NA>               532.7475 47.0625 173.19
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 53.803, df = NA, p-value = 0.0002

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.1296671
[1] "t16"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 395.8, df = 25, p-value < 0.00000000000000022

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                Total stranger   Employee Barely known Somewhat known
  Telephone         656.968123 3.48524203   18.2975207     7.84179457
  Internet            4.451004 0.02361275    0.1239669     0.05312869
  Street             52.521842 0.27863046    1.4628099     0.62691854
  Premises           12.462810 0.06611570    0.3471074     0.14876033
  Cobro de piso       6.231405 0.03305785    0.1735537     0.07438017
  Other              21.364817 0.11334120    0.5950413     0.25501771
               
                Close acquaintance      DK/DA
  Telephone             7.84179457 43.5655254
  Internet              0.05312869  0.2951594
  Street                0.62691854  3.4828808
  Premises              0.14876033  0.8264463
  Cobro de piso         0.07438017  0.4132231
  Other                 0.25501771  1.4167651
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 395.8, df = NA, p-value = 0.0001

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.3057105
[1] "t17"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 683.88, df = 30, p-value < 0.00000000000000022

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                Total stranger Employee Barely known Somewhat known
  Telephone          695.09375   3.6875    19.359375       8.296875
  Internet             2.82750   0.0150     0.078750       0.033750
  Street              29.68875   0.1575     0.826875       0.354375
  Premises             7.54000   0.0400     0.210000       0.090000
  Cobro de piso        3.29875   0.0175     0.091875       0.039375
  Other               15.55125   0.0825     0.433125       0.185625
               
                Close acquaintance    DK/DA       <NA>
  Telephone               8.296875 46.09375 694.171875
  Internet                0.033750  0.18750   2.823750
  Street                  0.354375  1.96875  29.649375
  Premises                0.090000  0.50000   7.530000
  Cobro de piso           0.039375  0.21875   3.294375
  Other                   0.185625  1.03125  15.530625
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 683.88, df = NA, p-value = 0.0001

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.2923785
[1] "t18"

	Chi-squared test for given probabilities

data:  get(paste("t", i, sep = ""))
X-squared = 631.88, df = 3, p-value < 0.00000000000000022

[1] "expected"
   No   Yes DK/DA  <NA> 
  400   400   400   400 
[1] "simulated p-value"

	Chi-squared test for given probabilities with simulated p-value
	(based on 9999 replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 631.88, df = NA, p-value = 0.0001

[1] "Cramer's V'"
[1] NA
[1] "t19"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 105.47, df = 2, p-value < 0.00000000000000022

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
       
               No       Yes
  No    256.62992 34.370079
  Yes    30.86614  4.133858
  DK/DA 272.50394 36.496063
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 105.47, df = NA, p-value = 0.0001

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.4075438
[1] "t20"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 184.79, df = 6, p-value < 0.00000000000000022

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
       
              No     Yes   <NA>
  No    265.3125 23.4375  86.25
  Yes    31.1300  2.7500  10.12
  DK/DA 302.8100 26.7500  98.44
  <NA>  532.7475 47.0625 173.19
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 184.79, df = NA, p-value = 0.0001

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.2403084
[1] "t21"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 372.6, df = 10, p-value < 0.00000000000000022

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                        No        Yes      DK/DA
  Telephone     326.741440 38.3376623 372.920897
  Internet        2.213695  0.2597403   2.526564
  Street         26.121606  3.0649351  29.813459
  Premises        6.198347  0.7272727   7.074380
  Cobro de piso   3.099174  0.3636364   3.537190
  Other          10.625738  1.2467532  12.127509
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 372.6, df = NA, p-value = 0.0001

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.4689927
[1] "t22"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 659.25, df = 15, p-value < 0.00000000000000022

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                        No     Yes    DK/DA       <NA>
  Telephone     345.703125 40.5625 394.5625 694.171875
  Internet        1.406250  0.1650   1.6050   2.823750
  Street         14.765625  1.7325  16.8525  29.649375
  Premises        3.750000  0.4400   4.2800   7.530000
  Cobro de piso   1.640625  0.1925   1.8725   3.294375
  Other           7.734375  0.9075   8.8275  15.530625
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 659.25, df = NA, p-value = 0.0001

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.3705991
[1] "t23"

	Chi-squared test for given probabilities

data:  get(paste("t", i, sep = ""))
X-squared = 2315.8, df = 6, p-value < 0.00000000000000022

[1] "expected"
    None    Blunt    DK/DA      Gun    Knife    Other     <NA> 
228.5714 228.5714 228.5714 228.5714 228.5714 228.5714 228.5714 
[1] "simulated p-value"

	Chi-squared test for given probabilities with simulated p-value
	(based on 9999 replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 2315.8, df = NA, p-value = 0.0001

[1] "Cramer's V'"
[1] NA
[1] "t24"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = NaN, df = 5, p-value = NA

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
       
                No        Yes
  None  256.521327 34.4786730
  Blunt   0.000000  0.0000000
  DK/DA 272.388626 36.6113744
  Gun    25.563981  3.4360190
  Knife   3.526066  0.4739336
  Other   0.000000  0.0000000
[1] "simulated p-value"
```

```
Warning in chisq.test(get(paste("t", i, sep = "")), simulate.p.value =
TRUE, : cannot compute simulated p-value with zero marginals
```

```
Warning in chisq.test(get(paste("t", i, sep = "")), simulate.p.value =
TRUE, : Chi-squared approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = NaN, df = 5, p-value = NA

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] NaN
[1] "t25"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = NaN, df = 12, p-value = NA

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
       
              No     Yes   <NA>
  None  265.3125 23.4375  86.25
  Blunt   0.7075  0.0625   0.23
  DK/DA 302.8100 26.7500  98.44
  Gun    24.0550  2.1250   7.82
  Knife   4.9525  0.4375   1.61
  Other   0.0000  0.0000   0.00
  <NA>  534.1625 47.1875 173.65
[1] "simulated p-value"
```

```
Warning in chisq.test(get(paste("t", i, sep = "")), simulate.p.value =
TRUE, : cannot compute simulated p-value with zero marginals

Warning in chisq.test(get(paste("t", i, sep = "")), simulate.p.value =
TRUE, : Chi-squared approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = NaN, df = 12, p-value = NA

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] NaN
[1] "t26"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = NaN, df = 25, p-value = NA

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                      None       Blunt      DK/DA        Gun      Knife
  Telephone     327.071006 0.872189349 373.297041 29.6544379 6.10532544
  Internet        1.775148 0.004733728   2.026036  0.1609467 0.03313609
  Street         26.183432 0.069822485  29.884024  2.3739645 0.48875740
  Premises        6.213018 0.016568047   7.091124  0.5633136 0.11597633
  Cobro de piso   3.106509 0.008284024   3.545562  0.2816568 0.05798817
  Other          10.650888 0.028402367  12.156213  0.9656805 0.19881657
               
                Other
  Telephone         0
  Internet          0
  Street            0
  Premises          0
  Cobro de piso     0
  Other             0
[1] "simulated p-value"
```

```
Warning in chisq.test(get(paste("t", i, sep = "")), simulate.p.value =
TRUE, : cannot compute simulated p-value with zero marginals

Warning in chisq.test(get(paste("t", i, sep = "")), simulate.p.value =
TRUE, : Chi-squared approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = NaN, df = 25, p-value = NA

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] NaN
[1] "t27"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = NaN, df = 30, p-value = NA

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                      None    Blunt    DK/DA      Gun    Knife Other
  Telephone     345.703125 0.921875 394.5625 31.34375 6.453125     0
  Internet        1.406250 0.003750   1.6050  0.12750 0.026250     0
  Street         14.765625 0.039375  16.8525  1.33875 0.275625     0
  Premises        3.750000 0.010000   4.2800  0.34000 0.070000     0
  Cobro de piso   1.640625 0.004375   1.8725  0.14875 0.030625     0
  Other           7.734375 0.020625   8.8275  0.70125 0.144375     0
               
                      <NA>
  Telephone     696.015625
  Internet        2.831250
  Street         29.728125
  Premises        7.550000
  Cobro de piso   3.303125
  Other          15.571875
[1] "simulated p-value"
```

```
Warning in chisq.test(get(paste("t", i, sep = "")), simulate.p.value =
TRUE, : cannot compute simulated p-value with zero marginals

Warning in chisq.test(get(paste("t", i, sep = "")), simulate.p.value =
TRUE, : Chi-squared approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = NaN, df = 30, p-value = NA

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] NaN
[1] "t28"

	Chi-squared test for given probabilities

data:  get(paste("t", i, sep = ""))
X-squared = 1309.1, df = 3, p-value < 0.00000000000000022

[1] "expected"
   No   Yes DK/DA  <NA> 
  400   400   400   400 
[1] "simulated p-value"

	Chi-squared test for given probabilities with simulated p-value
	(based on 9999 replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 1309.1, df = NA, p-value = 0.0001

[1] "Cramer's V'"
[1] NA
[1] "t29"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 20.081, df = 2, p-value = 0.0000436

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
       
               No       Yes
  No    512.37795 68.622047
  Yes    12.34646  1.653543
  DK/DA  35.27559  4.724409
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 20.081, df = NA, p-value = 0.0006

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.1778305
[1] "t30"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 56.307, df = 6, p-value = 0.0000000002523

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
       
              No     Yes   <NA>
  No    544.7750 48.1250 177.10
  Yes    13.4425  1.1875   4.37
  DK/DA  41.0350  3.6250  13.34
  <NA>  532.7475 47.0625 173.19
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 56.307, df = NA, p-value = 0.0001

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.1326496
[1] "t31"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 138.33, df = 10, p-value < 0.00000000000000022

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                        No        Yes      DK/DA
  Telephone     670.909091 16.5548996 50.5360094
  Internet        4.545455  0.1121606  0.3423849
  Street         53.636364  1.3234947  4.0401417
  Premises       12.727273  0.3140496  0.9586777
  Cobro de piso   6.363636  0.1570248  0.4793388
  Other          21.818182  0.5383707  1.6434475
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 138.33, df = NA, p-value = 0.0001

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.2857587
[1] "t32"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 297.55, df = 15, p-value < 0.00000000000000022

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                       No       Yes    DK/DA       <NA>
  Telephone     709.84375 17.515625 53.46875 694.171875
  Internet        2.88750  0.071250  0.21750   2.823750
  Street         30.31875  0.748125  2.28375  29.649375
  Premises        7.70000  0.190000  0.58000   7.530000
  Cobro de piso   3.36875  0.083125  0.25375   3.294375
  Other          15.88125  0.391875  1.19625  15.530625
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 297.55, df = NA, p-value = 0.0001

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.2489752
[1] "t33"

	Chi-squared test for given probabilities

data:  get(paste("t", i, sep = ""))
X-squared = 2218.3, df = 2, p-value < 0.00000000000000022

[1] "expected"
      No      Yes    DK/DA 
533.3333 533.3333 533.3333 
[1] "simulated p-value"

	Chi-squared test for given probabilities with simulated p-value
	(based on 9999 replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 2218.3, df = NA, p-value = 0.0001

[1] "Cramer's V'"
[1] NA
[1] "t34"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = NaN, df = 2, p-value = NA

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
       
              No      Yes
  No    989.5812 87.41883
  Yes   142.4188 12.58117
  DK/DA   0.0000  0.00000
[1] "simulated p-value"
```

```
Warning in chisq.test(get(paste("t", i, sep = "")), simulate.p.value =
TRUE, : cannot compute simulated p-value with zero marginals

Warning in chisq.test(get(paste("t", i, sep = "")), simulate.p.value =
TRUE, : Chi-squared approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = NaN, df = 2, p-value = NA

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] NaN
[1] "t35"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = NaN, df = 4, p-value = NA

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
       
               No     Yes   <NA>
  No    1001.1125 88.4375 325.45
  Yes    130.8875 11.5625  42.55
  DK/DA    0.0000  0.0000   0.00
[1] "simulated p-value"
```

```
Warning in chisq.test(get(paste("t", i, sep = "")), simulate.p.value =
TRUE, : cannot compute simulated p-value with zero marginals

Warning in chisq.test(get(paste("t", i, sep = "")), simulate.p.value =
TRUE, : Chi-squared approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = NaN, df = 4, p-value = NA

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] NaN
[1] "t36"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = NaN, df = 10, p-value = NA

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                         No        Yes DK/DA
  Telephone     1304.453125 170.546875     0
  Internet         5.306250   0.693750     0
  Street          55.715625   7.284375     0
  Premises        14.150000   1.850000     0
  Cobro de piso    6.190625   0.809375     0
  Other           29.184375   3.815625     0
[1] "simulated p-value"
```

```
Warning in chisq.test(get(paste("t", i, sep = "")), simulate.p.value =
TRUE, : cannot compute simulated p-value with zero marginals

Warning in chisq.test(get(paste("t", i, sep = "")), simulate.p.value =
TRUE, : Chi-squared approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = NaN, df = 10, p-value = NA

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] NaN
[1] "t37"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = NaN, df = 10, p-value = NA

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                         No        Yes DK/DA
  Telephone     1304.453125 170.546875     0
  Internet         5.306250   0.693750     0
  Street          55.715625   7.284375     0
  Premises        14.150000   1.850000     0
  Cobro de piso    6.190625   0.809375     0
  Other           29.184375   3.815625     0
[1] "simulated p-value"
```

```
Warning in chisq.test(get(paste("t", i, sep = "")), simulate.p.value =
TRUE, : cannot compute simulated p-value with zero marginals

Warning in chisq.test(get(paste("t", i, sep = "")), simulate.p.value =
TRUE, : Chi-squared approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = NaN, df = 10, p-value = NA

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] NaN
[1] "t38"

	Chi-squared test for given probabilities

data:  get(paste("t", i, sep = ""))
X-squared = 1980.2, df = 3, p-value < 0.00000000000000022

[1] "expected"
  Other   Money Nothing Product 
    400     400     400     400 
[1] "simulated p-value"

	Chi-squared test for given probabilities with simulated p-value
	(based on 9999 replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 1980.2, df = NA, p-value = 0.0001

[1] "Cramer's V'"
[1] NA
[1] "t39"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 3.3241, df = 3, p-value = 0.3443

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
         
                   No        Yes
  Other      2.756494  0.2435065
  Money   1042.873377 92.1266234
  Nothing   54.211039  4.7889610
  Product   32.159091  2.8409091
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 3.3241, df = NA, p-value = 0.3364

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.05194357
[1] "t40"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 1543.4, df = 6, p-value < 0.00000000000000022

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
         
                No     Yes   <NA>
  Other   256.8225 22.6875  83.49
  Money   805.1350 71.1250 261.74
  Nothing  44.5725  3.9375  14.49
  Product  25.4700  2.2500   8.28
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 1543.4, df = NA, p-value = 0.0001

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.6944933
[1] "t41"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 119.34, df = 15, p-value < 0.00000000000000022

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                     Other      Money   Nothing Product
  Telephone     334.640625 1049.09375 58.078125 33.1875
  Internet        1.361250    4.26750  0.236250  0.1350
  Street         14.293125   44.80875  2.480625  1.4175
  Premises        3.630000   11.38000  0.630000  0.3600
  Cobro de piso   1.588125    4.97875  0.275625  0.1575
  Other           7.486875   23.47125  1.299375  0.7425
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 119.34, df = NA, p-value = 0.0001

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.1576807
[1] "t42"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 119.34, df = 15, p-value < 0.00000000000000022

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                     Other      Money   Nothing Product
  Telephone     334.640625 1049.09375 58.078125 33.1875
  Internet        1.361250    4.26750  0.236250  0.1350
  Street         14.293125   44.80875  2.480625  1.4175
  Premises        3.630000   11.38000  0.630000  0.3600
  Cobro de piso   1.588125    4.97875  0.275625  0.1575
  Other           7.486875   23.47125  1.299375  0.7425
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 119.34, df = NA, p-value = 0.0001

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.1576807
[1] "t43"

	Chi-squared test for given probabilities

data:  get(paste("t", i, sep = ""))
X-squared = 4744.2, df = 3, p-value < 0.00000000000000022

[1] "expected"
   No   Yes DK/DA  <NA> 
  400   400   400   400 
[1] "simulated p-value"

	Chi-squared test for given probabilities with simulated p-value
	(based on 9999 replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 4744.2, df = NA, p-value = 0.0001

[1] "Cramer's V'"
[1] NA
[1] "t44"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Chi-squared test for given probabilities

data:  get(paste("t", i, sep = ""))
X-squared = 0.28571, df = 2, p-value = 0.8669

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
      No      Yes    DK/DA 
2.333333 2.333333 2.333333 
[1] "simulated p-value"

	Chi-squared test for given probabilities with simulated p-value
	(based on 9999 replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 0.28571, df = NA, p-value = 1

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] NA
[1] "t45"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 2.9167, df = 2, p-value = 0.2326

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
       
              No       Yes
  No    2.571429 0.4285714
  Yes   1.714286 0.2857143
  DK/DA 1.714286 0.2857143
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 2.9167, df = NA, p-value = 0.5772

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.6454972
[1] "t46"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 8.7851, df = 6, p-value = 0.186

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
       
               No     Yes   <NA>
  No       2.1225  0.1875   0.69
  Yes      1.4150  0.1250   0.46
  DK/DA    1.4150  0.1250   0.46
  <NA>  1127.0475 99.5625 366.39
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 8.7851, df = NA, p-value = 0.2362

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.05239596
[1] "t47"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = NaN, df = 10, p-value = NA

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                       No       Yes     DK/DA
  Telephone     2.1428571 1.4285714 1.4285714
  Internet      0.0000000 0.0000000 0.0000000
  Street        0.0000000 0.0000000 0.0000000
  Premises      0.4285714 0.2857143 0.2857143
  Cobro de piso 0.0000000 0.0000000 0.0000000
  Other         0.4285714 0.2857143 0.2857143
[1] "simulated p-value"
```

```
Warning in chisq.test(get(paste("t", i, sep = "")), simulate.p.value =
TRUE, : cannot compute simulated p-value with zero marginals

Warning in chisq.test(get(paste("t", i, sep = "")), simulate.p.value =
TRUE, : Chi-squared approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = NaN, df = 10, p-value = NA

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] NaN
[1] "t48"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 63.399, df = 15, p-value = 0.00000006507

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                      No     Yes   DK/DA        <NA>
  Telephone     2.765625 1.84375 1.84375 1468.546875
  Internet      0.011250 0.00750 0.00750    5.973750
  Street        0.118125 0.07875 0.07875   62.724375
  Premises      0.030000 0.02000 0.02000   15.930000
  Cobro de piso 0.013125 0.00875 0.00875    6.969375
  Other         0.061875 0.04125 0.04125   32.855625
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 63.399, df = NA, p-value = 0.0671

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.1149269
[1] "t49"

	Chi-squared test for given probabilities

data:  get(paste("t", i, sep = ""))
X-squared = 1952.7, df = 3, p-value < 0.00000000000000022

[1] "expected"
   No   Yes DK/DA  <NA> 
  400   400   400   400 
[1] "simulated p-value"

	Chi-squared test for given probabilities with simulated p-value
	(based on 9999 replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 1952.7, df = NA, p-value = 0.0001

[1] "Cramer's V'"
[1] NA
[1] "t50"

	Chi-squared test for given probabilities

data:  get(paste("t", i, sep = ""))
X-squared = 1884.6, df = 2, p-value < 0.00000000000000022

[1] "expected"
      No      Yes    DK/DA 
413.3333 413.3333 413.3333 
[1] "simulated p-value"

	Chi-squared test for given probabilities with simulated p-value
	(based on 9999 replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 1884.6, df = NA, p-value = 0.0001

[1] "Cramer's V'"
[1] NA
[1] "t51"

	Chi-squared test for given probabilities

data:  get(paste("t", i, sep = ""))
X-squared = 1075.3, df = 2, p-value < 0.00000000000000022

[1] "expected"
      No      Yes     <NA> 
533.3333 533.3333 533.3333 
[1] "simulated p-value"

	Chi-squared test for given probabilities with simulated p-value
	(based on 9999 replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 1075.3, df = NA, p-value = 0.0001

[1] "Cramer's V'"
[1] NA
[1] "t52"

	Chi-squared test for given probabilities

data:  get(paste("t", i, sep = ""))
X-squared = 864.47, df = 1, p-value < 0.00000000000000022

[1] "expected"
 No Yes 
616 616 
[1] "simulated p-value"

	Chi-squared test for given probabilities with simulated p-value
	(based on 9999 replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 864.47, df = NA, p-value = 0.0001

[1] "Cramer's V'"
[1] NA
[1] "t53"

	Chi-squared test for given probabilities

data:  get(paste("t", i, sep = ""))
X-squared = 6578.8, df = 5, p-value < 0.00000000000000022

[1] "expected"
    Telephone      Internet        Street      Premises Cobro de piso 
     266.6667      266.6667      266.6667      266.6667      266.6667 
        Other 
     266.6667 
[1] "simulated p-value"

	Chi-squared test for given probabilities with simulated p-value
	(based on 9999 replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 6578.8, df = NA, p-value = 0.0001

[1] "Cramer's V'"
[1] NA
[1] "t54"

	Chi-squared test for given probabilities

data:  get(paste("t", i, sep = ""))
X-squared = 6578.8, df = 5, p-value < 0.00000000000000022

[1] "expected"
    Telephone      Internet        Street      Premises Cobro de piso 
     266.6667      266.6667      266.6667      266.6667      266.6667 
        Other 
     266.6667 
[1] "simulated p-value"

	Chi-squared test for given probabilities with simulated p-value
	(based on 9999 replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 6578.8, df = NA, p-value = 0.0001

[1] "Cramer's V'"
[1] NA
[1] "t55"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 256.67, df = 5, p-value < 0.00000000000000022

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                         No        Yes
  Telephone     1041.035714 91.9642857
  Internet         3.675325  0.3246753
  Street          44.103896  3.8961039
  Premises        12.863636  1.1363636
  Cobro de piso    6.431818  0.5681818
  Other           23.889610  2.1103896
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 256.67, df = NA, p-value = 0.0001

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.4564381
[1] "t56"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  get(paste("t", i, sep = ""))
X-squared = 284.85, df = 10, p-value < 0.00000000000000022

[1] "expected"
```

```
Warning in chisq.test(get(paste("t", i, sep = ""))): Chi-squared
approximation may be incorrect
```

```
               
                       No     Yes   <NA>
  Telephone     1043.5625 92.1875 339.25
  Internet         4.2450  0.3750   1.38
  Street          44.5725  3.9375  14.49
  Premises        11.3200  1.0000   3.68
  Cobro de piso    4.9525  0.4375   1.61
  Other           23.3475  2.0625   7.59
[1] "simulated p-value"

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  get(paste("t", i, sep = ""))
X-squared = 284.85, df = NA, p-value = 0.0001

[1] "Cramer's V'"
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.2983555
```

```r
warnings()
```

```
Warning message:
In readLines(if (is.character(input2)) { ... :
  cannot open file 'ASC2016_Extortion_report.Rmd': No such file or directory
```

# EDA: Business and State level


```r
business_level_eda <- (subset(enve_test, CVE_UNICA %in%
    enve_incvic$CVE_UNICA))

# Distribution of extortion victimisations

ext_dist <- data.frame(table(business_level_eda$extortions))

colnames(ext_dist) <- c("Events", "Prevalence")

ext_dist$Events<- as.integer(as.character(ext_dist$Events))

ext_dist$Incidence <- ext_dist$Events * ext_dist$Prevalence

ext_dist$preval_per <- prop.table(ext_dist$Prevalence)*100

ext_dist$victim_per[2:length(ext_dist$Events)] <- prop.table(
                                                   ext_dist[2:length(
                                                    ext_dist$Events),2])*100

ext_dist$incid_per <- prop.table(ext_dist$Incidence)*100

ext_dist
```

```
   Events Prevalence Incidence  preval_per victim_per incid_per
1       0       1128         0 88.19390149         NA  0.000000
2       1         64        64  5.00390930 42.3841060  8.731241
3       2         20        40  1.56372166 13.2450331  5.457026
4       3         15        45  1.17279124  9.9337748  6.139154
5       4         14        56  1.09460516  9.2715232  7.639836
6       5          5        25  0.39093041  3.3112583  3.410641
7       6          3        18  0.23455825  1.9867550  2.455662
8       7          3        21  0.23455825  1.9867550  2.864939
9       8          1         8  0.07818608  0.6622517  1.091405
10      9          4        36  0.31274433  2.6490066  4.911323
11     10          1        10  0.07818608  0.6622517  1.364256
12     11          1        11  0.07818608  0.6622517  1.500682
13     12          2        24  0.15637217  1.3245033  3.274216
14     13          2        26  0.15637217  1.3245033  3.547067
15     14          2        28  0.15637217  1.3245033  3.819918
16     15          2        30  0.15637217  1.3245033  4.092769
17     19          2        38  0.15637217  1.3245033  5.184175
18     20          2        40  0.15637217  1.3245033  5.457026
19     21          2        42  0.15637217  1.3245033  5.729877
20     22          1        22  0.07818608  0.6622517  3.001364
21     25          1        25  0.07818608  0.6622517  3.410641
22     27          1        27  0.07818608  0.6622517  3.683492
23     32          2        64  0.15637217  1.3245033  8.731241
24     33          1        33  0.07818608  0.6622517  4.502046
```

```r
kable(ext_dist)
```



| Events| Prevalence| Incidence| preval_per| victim_per| incid_per|
|------:|----------:|---------:|----------:|----------:|---------:|
|      0|       1128|         0| 88.1939015|         NA|  0.000000|
|      1|         64|        64|  5.0039093| 42.3841060|  8.731241|
|      2|         20|        40|  1.5637217| 13.2450331|  5.457026|
|      3|         15|        45|  1.1727912|  9.9337748|  6.139154|
|      4|         14|        56|  1.0946052|  9.2715232|  7.639836|
|      5|          5|        25|  0.3909304|  3.3112583|  3.410641|
|      6|          3|        18|  0.2345582|  1.9867550|  2.455662|
|      7|          3|        21|  0.2345582|  1.9867550|  2.864939|
|      8|          1|         8|  0.0781861|  0.6622517|  1.091405|
|      9|          4|        36|  0.3127443|  2.6490066|  4.911323|
|     10|          1|        10|  0.0781861|  0.6622517|  1.364256|
|     11|          1|        11|  0.0781861|  0.6622517|  1.500682|
|     12|          2|        24|  0.1563722|  1.3245033|  3.274216|
|     13|          2|        26|  0.1563722|  1.3245033|  3.547067|
|     14|          2|        28|  0.1563722|  1.3245033|  3.819918|
|     15|          2|        30|  0.1563722|  1.3245033|  4.092769|
|     19|          2|        38|  0.1563722|  1.3245033|  5.184175|
|     20|          2|        40|  0.1563722|  1.3245033|  5.457026|
|     21|          2|        42|  0.1563722|  1.3245033|  5.729877|
|     22|          1|        22|  0.0781861|  0.6622517|  3.001364|
|     25|          1|        25|  0.0781861|  0.6622517|  3.410641|
|     27|          1|        27|  0.0781861|  0.6622517|  3.683492|
|     32|          2|        64|  0.1563722|  1.3245033|  8.731241|
|     33|          1|        33|  0.0781861|  0.6622517|  4.502046|

```r
#Poissonnes tests
n <- length(business_level_eda$extortions)
mean_ext <- mean(business_level_eda$extortions)
var_ext <- var(business_level_eda$extortions)

mean_ext
```

```
[1] 0.573104
```

```r
var_ext
```

```
[1] 7.761279
```

```r
var_mean_ratio <- var_ext/mean_ext
var_mean_ratio
```

```
[1] 13.54253
```

```r
index_ext <- id.test(enve_test$extortions)
index_ext
```

```
         I    P-value         df 95% Chi-sq 
 30360.204      0.000   2499.000   2616.411 
```

```r
vmr_df <- data.frame(Mean=mean_ext, Variance=var_ext, Ratio=var_mean_ratio,
                     Index=unname(index_ext[1]), Pvalue=unname(index_ext[2]),
                     DF=unname(index_ext[3]))

vmr_df
```

```
      Mean Variance    Ratio   Index Pvalue   DF
1 0.573104 7.761279 13.54253 30360.2      0 2499
```

```r
kable(vmr_df)
```



|     Mean| Variance|    Ratio|   Index| Pvalue|   DF|
|--------:|--------:|--------:|-------:|------:|----:|
| 0.573104| 7.761279| 13.54253| 30360.2|      0| 2499|

```r
# plot of extortions
ggplot(business_level_eda, aes(extortions)) +
    geom_histogram(binwidth = 1) +
    coord_trans(y="sqrt") +
    theme_bw() +
    ylab("Frequency") +
    xlab("Extortions") +
    ggtitle("Extortion counts frequency")
```

![plot of chunk business-level-eda](figure/business-level-eda-1.pdf)

```r
ggplot(business_level_eda, aes(extortions)) +
  geom_density() +
  coord_trans(y="sqrt") +
  theme_bw() +
  ggtitle("Extortion counts density")
```

![plot of chunk business-level-eda](figure/business-level-eda-2.pdf)

```r
ggplot(business_level_eda, aes(extortions)) +
  geom_histogram(aes(y=..density..), binwidth = 1) + geom_density() +
  coord_trans(y="sqrt") +
  theme_bw() +
  ggtitle("Extortion counts density")
```

![plot of chunk business-level-eda](figure/business-level-eda-3.pdf)

```r
## plot the capped distrbution of extortion

capped_ext_count <- table(enve_incvic$CVE_UNICA)

capped_ext_count_df <- data.frame(capped_ext_count)

names(capped_ext_count_df) <- c("CVE_UNICA", "Extortions")

table(capped_ext_count)
```

```
capped_ext_count
   1    2    3    4    5 
1073  131   48   14   13 
```

```r
ggplot(capped_ext_count_df, aes(Extortions)) +
  geom_histogram(binwidth = 1) +
  coord_trans(y="sqrt") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Capped extortion counts") +
  ggtitle("Capped extortion counts (> 0) frequency") +
  expand_limits(y=1)
```

![plot of chunk business-level-eda](figure/business-level-eda-4.pdf)

```r
ggplot(capped_ext_count_df, aes(Extortions)) +
  geom_histogram(binwidth = 1) +
  #coord_trans(y="sqrt") +
  theme_bw() +
  ylab("Frequency") +
  ggtitle("Capped extortion counts (> 0) frequency") +
  expand_limits(y=1)
```

![plot of chunk business-level-eda](figure/business-level-eda-5.pdf)

```r
ggplot(business_level_eda[business_level_eda$extortions>0,], aes(extortions)) +
  geom_histogram(binwidth = 1) +
  coord_trans(y="sqrt") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Extortions") +
  ggtitle("Uncapped extortion counts (> 0) frequency") +
  expand_limits(y=1)
```

![plot of chunk business-level-eda](figure/business-level-eda-6.pdf)

```r
ggplot(business_level_eda[business_level_eda$extortions>0,], aes(extortions)) +
  geom_histogram(binwidth = 1) +
  #coord_trans(y="sqrt") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Extortions") +
  ggtitle("Uncapped extortion counts (> 0) frequency") +
  expand_limits(y=1)
```

![plot of chunk business-level-eda](figure/business-level-eda-7.pdf)

```r
# other extortion variables
table(business_level_eda$extortion_victim)
```

```

   0    1 
1128  151 
```

```r
table(business_level_eda$rep_extortion_victim)
```

```

   0    1 
1192   87 
```

Now we create a summary table of business and state level variables


```r
### Summary table of business and state level variables

summ_table <- data.frame(variable=0, N=0, prevalence=0, incidence=0, mean=0,
                         sd=0, min=0, max=0)

ind <- 1
for (i in 1:length(business_level_eda))
{
  if (colnames(business_level_eda)[i] %in% c("extortions", "bribes"))
  {
    summ_table[ind,1] <- colnames(business_level_eda)[i]
    summ_table[ind,2] <- length(business_level_eda[,i])
    summ_table[ind,3] <- length(business_level_eda[business_level_eda[,i] != 0,i])
    summ_table[ind,4] <- sum(business_level_eda[,i])
    summ_table[ind,5] <- mean(business_level_eda[,i])
    summ_table[ind,6] <- sd(business_level_eda[,i])
    summ_table[ind,7] <- min(business_level_eda[,i])
    summ_table[ind,8] <- max(business_level_eda[,i])
  }

  else if (colnames(business_level_eda)[i] %in%
           c("rep_extortion_victim", "bribe_victim", "rep_bribe"))
  {
    summ_table[ind,1] <- colnames(business_level_eda)[i]
    summ_table[ind,2] <- length(business_level_eda[,i])
    summ_table[ind,3] <- length(business_level_eda[business_level_eda[,i] != 0,i])
    summ_table[ind,5] <- summ_table[ind,3]/summ_table[ind,2]
  }

  else if (colnames(business_level_eda)[i] == "years")
  {
    summ_table[ind,1] <- colnames(business_level_eda)[i]
    summ_table[ind,2] <- length(business_level_eda[,i])
    summ_table[ind,5] <- mean(business_level_eda[,i])
    summ_table[ind,6] <- sd(business_level_eda[,i])
    summ_table[ind,7] <- min(business_level_eda[,i])
    summ_table[ind,8] <- max(business_level_eda[,i])
  }

  else if (colnames(business_level_eda)[i] %in% c("size", "subsector",
                                         "restbar", "yearsquant"))
  {
    for (a in 1:length(levels(business_level_eda[,i])))
    {
      summ_table[ind,1] <- levels(business_level_eda[,i])[a]
      summ_table[ind,2] <- length(business_level_eda[business_level_eda[,i] ==
                                        levels(business_level_eda[,i])[a],i])
      summ_table[ind,5] <- summ_table[ind,2]/length(business_level_eda[,1])
      ind <- ind + 1
    }
  }
  ind <- ind + 1
}


summ_table[length(summ_table[,1])+1,] <- c(
            NA, length(homicidios[,"tasahom"]), NA, NA,
            mean(homicidios[,"tasahom"]), sd(homicidios[,"tasahom"]),
            min(homicidios[,"tasahom"]), max(homicidios[,"tasahom"]))

summ_table[length(summ_table[,1]),1] <- "state murder rt"

# homicido absoluto
summ_table[length(summ_table[,1])+1,] <- c(
            NA, length(homicidios[,"denuncias_homs"]), NA, NA,
            mean(homicidios[,"denuncias_homs"]), sd(homicidios[,"denuncias_homs"]),
            min(homicidios[,"denuncias_homs"]), max(homicidios[,"denuncias_homs"]))

summ_table[length(summ_table[,1]),1] <- "Murders"

#population
summ_table[length(summ_table[,1])+1,] <- c(
            NA, length(homicidios[,"poblacion"]), NA, NA,
            mean(homicidios[,"poblacion"])/1000, sd(homicidios[,"poblacion"])/1000,
            min(homicidios[,"poblacion"])/1000, max(homicidios[,"poblacion"])/1000)

summ_table[length(summ_table[,1]),1] <- "Population (in thousands)"

summ_table <- summ_table[!is.na(summ_table[,1]),]

summ_table[,c(5:8)] <- round(summ_table[,c(5:8)],3)

summ_table
```

```
                    variable    N prevalence incidence     mean       sd
1                          0    0          0         0    0.000    0.000
2                 extortions 1279        151       733    0.573    2.786
4       rep_extortion_victim 1279         87        NA    0.068       NA
6                     bribes 1279        135       362    0.283    1.160
7               bribe_victim 1279        135        NA    0.106       NA
8                  rep_bribe 1279         74        NA    0.058       NA
10                     Large  314         NA        NA    0.246       NA
11                    Medium  310         NA        NA    0.242       NA
12                     Small  288         NA        NA    0.225       NA
13                     Micro  367         NA        NA    0.287       NA
17                    Retail   36         NA        NA    0.028       NA
18                    Mining   19         NA        NA    0.015       NA
19                 Utilities   20         NA        NA    0.016       NA
20              Construction  163         NA        NA    0.127       NA
21             Manufacturing  262         NA        NA    0.205       NA
22                 Wholesale   54         NA        NA    0.042       NA
23                 Transport   67         NA        NA    0.052       NA
24                     Media   23         NA        NA    0.018       NA
25                   Finance   19         NA        NA    0.015       NA
26               Real estate   22         NA        NA    0.017       NA
27            Prof. services   18         NA        NA    0.014       NA
28                 Corporate   22         NA        NA    0.017       NA
29               Maintenance  106         NA        NA    0.083       NA
30                 Education   20         NA        NA    0.016       NA
31                    Health  213         NA        NA    0.167       NA
32                   Leisure   19         NA        NA    0.015       NA
33             HotelsRestBar  189         NA        NA    0.148       NA
34                     Other    7         NA        NA    0.005       NA
37                     years 1279         NA        NA   20.586   12.603
38                     [0,8]  287         NA        NA    0.224       NA
39                    (8,16]  250         NA        NA    0.195       NA
40                   (16,25]  257         NA        NA    0.201       NA
41                   (25,34]  258         NA        NA    0.202       NA
42                   (34,43]  227         NA        NA    0.177       NA
43           state murder rt   32         NA        NA   16.168   12.634
44                   Murders   32         NA        NA  572.844  526.363
45 Population (in thousands)   32         NA        NA 3699.845 3152.545
       min       max
1    0.000     0.000
2    0.000    33.000
4       NA        NA
6    0.000    16.000
7       NA        NA
8       NA        NA
10      NA        NA
11      NA        NA
12      NA        NA
13      NA        NA
17      NA        NA
18      NA        NA
19      NA        NA
20      NA        NA
21      NA        NA
22      NA        NA
23      NA        NA
24      NA        NA
25      NA        NA
26      NA        NA
27      NA        NA
28      NA        NA
29      NA        NA
30      NA        NA
31      NA        NA
32      NA        NA
33      NA        NA
34      NA        NA
37   0.000    43.000
38      NA        NA
39      NA        NA
40      NA        NA
41      NA        NA
42      NA        NA
43   1.938    59.225
44  39.000  2087.000
45 698.295 16364.210
```

```r
kable(summ_table, row.names = F)
```



|variable                  |    N| prevalence| incidence|     mean|       sd|     min|       max|
|:-------------------------|----:|----------:|---------:|--------:|--------:|-------:|---------:|
|0                         |    0|          0|         0|    0.000|    0.000|   0.000|     0.000|
|extortions                | 1279|        151|       733|    0.573|    2.786|   0.000|    33.000|
|rep_extortion_victim      | 1279|         87|        NA|    0.068|       NA|      NA|        NA|
|bribes                    | 1279|        135|       362|    0.283|    1.160|   0.000|    16.000|
|bribe_victim              | 1279|        135|        NA|    0.106|       NA|      NA|        NA|
|rep_bribe                 | 1279|         74|        NA|    0.058|       NA|      NA|        NA|
|Large                     |  314|         NA|        NA|    0.246|       NA|      NA|        NA|
|Medium                    |  310|         NA|        NA|    0.242|       NA|      NA|        NA|
|Small                     |  288|         NA|        NA|    0.225|       NA|      NA|        NA|
|Micro                     |  367|         NA|        NA|    0.287|       NA|      NA|        NA|
|Retail                    |   36|         NA|        NA|    0.028|       NA|      NA|        NA|
|Mining                    |   19|         NA|        NA|    0.015|       NA|      NA|        NA|
|Utilities                 |   20|         NA|        NA|    0.016|       NA|      NA|        NA|
|Construction              |  163|         NA|        NA|    0.127|       NA|      NA|        NA|
|Manufacturing             |  262|         NA|        NA|    0.205|       NA|      NA|        NA|
|Wholesale                 |   54|         NA|        NA|    0.042|       NA|      NA|        NA|
|Transport                 |   67|         NA|        NA|    0.052|       NA|      NA|        NA|
|Media                     |   23|         NA|        NA|    0.018|       NA|      NA|        NA|
|Finance                   |   19|         NA|        NA|    0.015|       NA|      NA|        NA|
|Real estate               |   22|         NA|        NA|    0.017|       NA|      NA|        NA|
|Prof. services            |   18|         NA|        NA|    0.014|       NA|      NA|        NA|
|Corporate                 |   22|         NA|        NA|    0.017|       NA|      NA|        NA|
|Maintenance               |  106|         NA|        NA|    0.083|       NA|      NA|        NA|
|Education                 |   20|         NA|        NA|    0.016|       NA|      NA|        NA|
|Health                    |  213|         NA|        NA|    0.167|       NA|      NA|        NA|
|Leisure                   |   19|         NA|        NA|    0.015|       NA|      NA|        NA|
|HotelsRestBar             |  189|         NA|        NA|    0.148|       NA|      NA|        NA|
|Other                     |    7|         NA|        NA|    0.005|       NA|      NA|        NA|
|years                     | 1279|         NA|        NA|   20.586|   12.603|   0.000|    43.000|
|[0,8]                     |  287|         NA|        NA|    0.224|       NA|      NA|        NA|
|(8,16]                    |  250|         NA|        NA|    0.195|       NA|      NA|        NA|
|(16,25]                   |  257|         NA|        NA|    0.201|       NA|      NA|        NA|
|(25,34]                   |  258|         NA|        NA|    0.202|       NA|      NA|        NA|
|(34,43]                   |  227|         NA|        NA|    0.177|       NA|      NA|        NA|
|state murder rt           |   32|         NA|        NA|   16.168|   12.634|   1.938|    59.225|
|Murders                   |   32|         NA|        NA|  572.844|  526.363|  39.000|  2087.000|
|Population (in thousands) |   32|         NA|        NA| 3699.845| 3152.545| 698.295| 16364.210|

As a precaution, explore the distribution of the dependent variable with the categorical business-level variables (size, subsector, yearsquant). Since if some categories have very few observations, they will probably have to be aggregated or the models might not converge.


```r
# size

t100 <- table(enve_incvic$size, enve_incvic$complied_bin)
t100
```

```
        
          No Yes
  Large  272  22
  Medium 296  24
  Small  232  36
  Micro  332  18
```

```r
kable(t100)
```



|       |  No| Yes|
|:------|---:|---:|
|Large  | 272|  22|
|Medium | 296|  24|
|Small  | 232|  36|
|Micro  | 332|  18|

```r
t100/as.integer(margin.table(t100, margin=1))*100
```

```
        
                No       Yes
  Large  92.517007  7.482993
  Medium 92.500000  7.500000
  Small  86.567164 13.432836
  Micro  94.857143  5.142857
```

```r
kable(round(t100/as.integer(margin.table(t100, margin=1))*100, 2))
```



|       |    No|   Yes|
|:------|-----:|-----:|
|Large  | 92.52|  7.48|
|Medium | 92.50|  7.50|
|Small  | 86.57| 13.43|
|Micro  | 94.86|  5.14|

```r
chisq.test(t100)
```

```

	Pearson's Chi-squared test

data:  t100
X-squared = 14.627, df = 3, p-value = 0.002165
```

```r
chisq.test(t100)$expected
```

```
        
               No      Yes
  Large  270.1364 23.86364
  Medium 294.0260 25.97403
  Small  246.2468 21.75325
  Micro  321.5909 28.40909
```

```r
chisq.test(t100, simulate.p.value=TRUE, B=9999)
```

```

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  t100
X-squared = 14.627, df = NA, p-value = 0.0016
```

```r
print("Cramer's V'")
```

```
[1] "Cramer's V'"
```

```r
cv.test(t100)
```

```
[1] 0.1089624
```

```r
# USE NA

t101 <- table(enve_incvic$size, enve_incvic$complied_bin, useNA = "ifany")
t101
```

```
        
          No Yes <NA>
  Large  272  22  100
  Medium 296  24   66
  Small  232  36  101
  Micro  332  18  101
```

```r
kable(t101)
```



|       |  No| Yes|  NA|
|:------|---:|---:|---:|
|Large  | 272|  22| 100|
|Medium | 296|  24|  66|
|Small  | 232|  36| 101|
|Micro  | 332|  18| 101|

```r
t101/as.integer(margin.table(t101, margin=1))*100
```

```
        
                No       Yes      <NA>
  Large  69.035533  5.583756 25.380711
  Medium 76.683938  6.217617 17.098446
  Small  62.872629  9.756098 27.371274
  Micro  73.614191  3.991131 22.394678
```

```r
kable(round(t101/as.integer(margin.table(t101, margin=1))*100, 2))
```



|       |    No|  Yes|    NA|
|:------|-----:|----:|-----:|
|Large  | 69.04| 5.58| 25.38|
|Medium | 76.68| 6.22| 17.10|
|Small  | 62.87| 9.76| 27.37|
|Micro  | 73.61| 3.99| 22.39|

```r
chisq.test(t101)
```

```

	Pearson's Chi-squared test

data:  t101
X-squared = 27.018, df = 6, p-value = 0.0001437
```

```r
chisq.test(t101)$expected
```

```
        
               No     Yes   <NA>
  Large  278.7550 24.6250  90.62
  Medium 273.0950 24.1250  88.78
  Small  261.0675 23.0625  84.87
  Micro  319.0825 28.1875 103.73
```

```r
chisq.test(t101, simulate.p.value=TRUE, B=9999)
```

```

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  t101
X-squared = 27.018, df = NA, p-value = 0.0002
```

```r
print("Cramer's V'")
```

```
[1] "Cramer's V'"
```

```r
cv.test(t101)
```

```
[1] 0.09188586
```

```r
# subsector

t200 <- table(enve_incvic$subsector, enve_incvic$complied_bin)
t200
```

```
                
                  No Yes
  Retail          28   4
  Mining          16   1
  Utilities       17   0
  Construction   143  13
  Manufacturing  254  23
  Wholesale       39   4
  Transport       60   5
  Media           22   3
  Finance         14   5
  Real estate     19   2
  Prof. services  21   0
  Corporate        9   5
  Maintenance     90   9
  Education       18   0
  Health         178  18
  Leisure         22   0
  HotelsRestBar  175   8
  Other            7   0
```

```r
kable(t200)
```



|               |  No| Yes|
|:--------------|---:|---:|
|Retail         |  28|   4|
|Mining         |  16|   1|
|Utilities      |  17|   0|
|Construction   | 143|  13|
|Manufacturing  | 254|  23|
|Wholesale      |  39|   4|
|Transport      |  60|   5|
|Media          |  22|   3|
|Finance        |  14|   5|
|Real estate    |  19|   2|
|Prof. services |  21|   0|
|Corporate      |   9|   5|
|Maintenance    |  90|   9|
|Education      |  18|   0|
|Health         | 178|  18|
|Leisure        |  22|   0|
|HotelsRestBar  | 175|   8|
|Other          |   7|   0|

```r
t200/as.integer(margin.table(t200, margin=1))*100
```

```
                
                         No        Yes
  Retail          87.500000  12.500000
  Mining          94.117647   5.882353
  Utilities      100.000000   0.000000
  Construction    91.666667   8.333333
  Manufacturing   91.696751   8.303249
  Wholesale       90.697674   9.302326
  Transport       92.307692   7.692308
  Media           88.000000  12.000000
  Finance         73.684211  26.315789
  Real estate     90.476190   9.523810
  Prof. services 100.000000   0.000000
  Corporate       64.285714  35.714286
  Maintenance     90.909091   9.090909
  Education      100.000000   0.000000
  Health          90.816327   9.183673
  Leisure        100.000000   0.000000
  HotelsRestBar   95.628415   4.371585
  Other          100.000000   0.000000
```

```r
kable(round(t200/as.integer(margin.table(t200, margin=1))*100, 2))
```



|               |     No|   Yes|
|:--------------|------:|-----:|
|Retail         |  87.50| 12.50|
|Mining         |  94.12|  5.88|
|Utilities      | 100.00|  0.00|
|Construction   |  91.67|  8.33|
|Manufacturing  |  91.70|  8.30|
|Wholesale      |  90.70|  9.30|
|Transport      |  92.31|  7.69|
|Media          |  88.00| 12.00|
|Finance        |  73.68| 26.32|
|Real estate    |  90.48|  9.52|
|Prof. services | 100.00|  0.00|
|Corporate      |  64.29| 35.71|
|Maintenance    |  90.91|  9.09|
|Education      | 100.00|  0.00|
|Health         |  90.82|  9.18|
|Leisure        | 100.00|  0.00|
|HotelsRestBar  |  95.63|  4.37|
|Other          | 100.00|  0.00|

```r
chisq.test(t200)
```

```
Warning in chisq.test(t200): Chi-squared approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  t200
X-squared = 35.729, df = 17, p-value = 0.004984
```

```r
chisq.test(t200)$expected
```

```
Warning in chisq.test(t200): Chi-squared approximation may be incorrect
```

```
                
                         No        Yes
  Retail          29.402597  2.5974026
  Mining          15.620130  1.3798701
  Utilities       15.620130  1.3798701
  Construction   143.337662 12.6623377
  Manufacturing  254.516234 22.4837662
  Wholesale       39.509740  3.4902597
  Transport       59.724026  5.2759740
  Media           22.970779  2.0292208
  Finance         17.457792  1.5422078
  Real estate     19.295455  1.7045455
  Prof. services  19.295455  1.7045455
  Corporate       12.863636  1.1363636
  Maintenance     90.964286  8.0357143
  Education       16.538961  1.4610390
  Health         180.090909 15.9090909
  Leisure         20.214286  1.7857143
  HotelsRestBar  168.146104 14.8538961
  Other            6.431818  0.5681818
```

```r
chisq.test(t200, simulate.p.value=TRUE, B=9999)
```

```

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  t200
X-squared = 35.729, df = NA, p-value = 0.0085
```

```r
print("Cramer's V'")
```

```
[1] "Cramer's V'"
```

```r
cv.test(t200)
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.1702961
```

```r
# USE NA

t201 <- table(enve_incvic$subsector, enve_incvic$complied_bin, useNA = "ifany")
t201
```

```
                
                  No Yes <NA>
  Retail          28   4   18
  Mining          16   1    4
  Utilities       17   0    6
  Construction   143  13   46
  Manufacturing  254  23   62
  Wholesale       39   4   20
  Transport       60   5   22
  Media           22   3    3
  Finance         14   5    3
  Real estate     19   2    5
  Prof. services  21   0    5
  Corporate        9   5   10
  Maintenance     90   9   30
  Education       18   0    7
  Health         178  18   70
  Leisure         22   0    3
  HotelsRestBar  175   8   52
  Other            7   0    2
```

```r
kable(t201)
```



|               |  No| Yes| NA|
|:--------------|---:|---:|--:|
|Retail         |  28|   4| 18|
|Mining         |  16|   1|  4|
|Utilities      |  17|   0|  6|
|Construction   | 143|  13| 46|
|Manufacturing  | 254|  23| 62|
|Wholesale      |  39|   4| 20|
|Transport      |  60|   5| 22|
|Media          |  22|   3|  3|
|Finance        |  14|   5|  3|
|Real estate    |  19|   2|  5|
|Prof. services |  21|   0|  5|
|Corporate      |   9|   5| 10|
|Maintenance    |  90|   9| 30|
|Education      |  18|   0|  7|
|Health         | 178|  18| 70|
|Leisure        |  22|   0|  3|
|HotelsRestBar  | 175|   8| 52|
|Other          |   7|   0|  2|

```r
t201/as.integer(margin.table(t201, margin=1))*100
```

```
                
                        No       Yes      <NA>
  Retail         56.000000  8.000000 36.000000
  Mining         76.190476  4.761905 19.047619
  Utilities      73.913043  0.000000 26.086957
  Construction   70.792079  6.435644 22.772277
  Manufacturing  74.926254  6.784661 18.289086
  Wholesale      61.904762  6.349206 31.746032
  Transport      68.965517  5.747126 25.287356
  Media          78.571429 10.714286 10.714286
  Finance        63.636364 22.727273 13.636364
  Real estate    73.076923  7.692308 19.230769
  Prof. services 80.769231  0.000000 19.230769
  Corporate      37.500000 20.833333 41.666667
  Maintenance    69.767442  6.976744 23.255814
  Education      72.000000  0.000000 28.000000
  Health         66.917293  6.766917 26.315789
  Leisure        88.000000  0.000000 12.000000
  HotelsRestBar  74.468085  3.404255 22.127660
  Other          77.777778  0.000000 22.222222
```

```r
kable(round(t201/as.integer(margin.table(t201, margin=1))*100, 2))
```



|               |    No|   Yes|    NA|
|:--------------|-----:|-----:|-----:|
|Retail         | 56.00|  8.00| 36.00|
|Mining         | 76.19|  4.76| 19.05|
|Utilities      | 73.91|  0.00| 26.09|
|Construction   | 70.79|  6.44| 22.77|
|Manufacturing  | 74.93|  6.78| 18.29|
|Wholesale      | 61.90|  6.35| 31.75|
|Transport      | 68.97|  5.75| 25.29|
|Media          | 78.57| 10.71| 10.71|
|Finance        | 63.64| 22.73| 13.64|
|Real estate    | 73.08|  7.69| 19.23|
|Prof. services | 80.77|  0.00| 19.23|
|Corporate      | 37.50| 20.83| 41.67|
|Maintenance    | 69.77|  6.98| 23.26|
|Education      | 72.00|  0.00| 28.00|
|Health         | 66.92|  6.77| 26.32|
|Leisure        | 88.00|  0.00| 12.00|
|HotelsRestBar  | 74.47|  3.40| 22.13|
|Other          | 77.78|  0.00| 22.22|

```r
chisq.test(t201)
```

```
Warning in chisq.test(t201): Chi-squared approximation may be incorrect
```

```

	Pearson's Chi-squared test

data:  t201
X-squared = 58.215, df = 34, p-value = 0.005998
```

```r
chisq.test(t201)$expected
```

```
Warning in chisq.test(t201): Chi-squared approximation may be incorrect
```

```
                
                       No     Yes  <NA>
  Retail          35.3750  3.1250 11.50
  Mining          14.8575  1.3125  4.83
  Utilities       16.2725  1.4375  5.29
  Construction   142.9150 12.6250 46.46
  Manufacturing  239.8425 21.1875 77.97
  Wholesale       44.5725  3.9375 14.49
  Transport       61.5525  5.4375 20.01
  Media           19.8100  1.7500  6.44
  Finance         15.5650  1.3750  5.06
  Real estate     18.3950  1.6250  5.98
  Prof. services  18.3950  1.6250  5.98
  Corporate       16.9800  1.5000  5.52
  Maintenance     91.2675  8.0625 29.67
  Education       17.6875  1.5625  5.75
  Health         188.1950 16.6250 61.18
  Leisure         17.6875  1.5625  5.75
  HotelsRestBar  166.2625 14.6875 54.05
  Other            6.3675  0.5625  2.07
```

```r
chisq.test(t201, simulate.p.value=TRUE, B=9999)
```

```

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  t201
X-squared = 58.215, df = NA, p-value = 0.0092
```

```r
print("Cramer's V'")
```

```
[1] "Cramer's V'"
```

```r
cv.test(t201)
```

```
Warning in chisq.test(df): Chi-squared approximation may be incorrect
```

```
[1] 0.134879
```

```r
## hotrestbar

t300 <- table(enve_incvic$hotrestbar, enve_incvic$complied_bin)
t300
```

```
   
     No Yes
  0 957  92
  1 175   8
```

```r
kable(t300)
```



|   |  No| Yes|
|:--|---:|---:|
|0  | 957|  92|
|1  | 175|   8|

```r
t300/as.integer(margin.table(t300, margin=1))*100
```

```
   
           No       Yes
  0 91.229743  8.770257
  1 95.628415  4.371585
```

```r
kable(round(t300/as.integer(margin.table(t300, margin=1))*100, 2))
```



|   |    No|  Yes|
|:--|-----:|----:|
|0  | 91.23| 8.77|
|1  | 95.63| 4.37|

```r
chisq.test(t300)
```

```

	Pearson's Chi-squared test with Yates' continuity correction

data:  t300
X-squared = 3.4741, df = 1, p-value = 0.06234
```

```r
chisq.test(t300)$expected
```

```
   
          No     Yes
  0 963.8539 85.1461
  1 168.1461 14.8539
```

```r
chisq.test(t300, simulate.p.value=TRUE, B=9999)
```

```

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  t300
X-squared = 4.0424, df = NA, p-value = 0.0548
```

```r
print("Cramer's V'")
```

```
[1] "Cramer's V'"
```

```r
cv.test(t300)
```

```
[1] 0.05310242
```

```r
# USE NA

t301 <- table(enve_incvic$hotrestbar, enve_incvic$complied_bin, useNA = "ifany")
t301
```

```
   
     No Yes <NA>
  0 957  92  316
  1 175   8   52
```

```r
kable(t301)
```



|   |  No| Yes|  NA|
|:--|---:|---:|---:|
|0  | 957|  92| 316|
|1  | 175|   8|  52|

```r
t301/as.integer(margin.table(t301, margin=1))*100
```

```
   
           No       Yes      <NA>
  0 70.109890  6.739927 23.150183
  1 74.468085  3.404255 22.127660
```

```r
kable(round(t301/as.integer(margin.table(t301, margin=1))*100, 2))
```



|   |    No|  Yes|    NA|
|:--|-----:|----:|-----:|
|0  | 70.11| 6.74| 23.15|
|1  | 74.47| 3.40| 22.13|

```r
chisq.test(t301)
```

```

	Pearson's Chi-squared test

data:  t301
X-squared = 4.1985, df = 2, p-value = 0.1225
```

```r
chisq.test(t301)$expected
```

```
   
          No     Yes   <NA>
  0 965.7375 85.3125 313.95
  1 166.2625 14.6875  54.05
```

```r
chisq.test(t301, simulate.p.value=TRUE, B=9999)
```

```

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  t301
X-squared = 4.1985, df = NA, p-value = 0.126
```

```r
print("Cramer's V'")
```

```
[1] "Cramer's V'"
```

```r
cv.test(t301)
```

```
[1] 0.05122582
```

```r
## years quant

t400 <- table(enve_incvic$yearsquant, enve_incvic$complied_bin)
t400
```

```
         
           No Yes
  [0,8]   256  23
  (8,16]  226  17
  (16,25] 229  21
  (25,34] 223  15
  (34,43] 198  24
```

```r
kable(t400)
```



|        |  No| Yes|
|:-------|---:|---:|
|[0,8]   | 256|  23|
|(8,16]  | 226|  17|
|(16,25] | 229|  21|
|(25,34] | 223|  15|
|(34,43] | 198|  24|

```r
t400/as.integer(margin.table(t400, margin=1))*100
```

```
         
                 No       Yes
  [0,8]   91.756272  8.243728
  (8,16]  93.004115  6.995885
  (16,25] 91.600000  8.400000
  (25,34] 93.697479  6.302521
  (34,43] 89.189189 10.810811
```

```r
kable(round(t400/as.integer(margin.table(t400, margin=1))*100, 2))
```



|        |    No|   Yes|
|:-------|-----:|-----:|
|[0,8]   | 91.76|  8.24|
|(8,16]  | 93.00|  7.00|
|(16,25] | 91.60|  8.40|
|(25,34] | 93.70|  6.30|
|(34,43] | 89.19| 10.81|

```r
chisq.test(t400)
```

```

	Pearson's Chi-squared test

data:  t400
X-squared = 3.6531, df = 4, p-value = 0.455
```

```r
chisq.test(t400)$expected
```

```
         
                No      Yes
  [0,8]   256.3539 22.64610
  (8,16]  223.2760 19.72403
  (16,25] 229.7078 20.29221
  (25,34] 218.6818 19.31818
  (34,43] 203.9805 18.01948
```

```r
chisq.test(t400, simulate.p.value=TRUE, B=9999)
```

```

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  t400
X-squared = 3.6531, df = NA, p-value = 0.4553
```

```r
print("Cramer's V'")
```

```
[1] "Cramer's V'"
```

```r
cv.test(t400)
```

```
[1] 0.05445321
```

```r
# USE NA

t401 <- table(enve_incvic$yearsquant, enve_incvic$complied_bin, useNA = "ifany")
t401
```

```
         
           No Yes <NA>
  [0,8]   256  23   92
  (8,16]  226  17   71
  (16,25] 229  21   68
  (25,34] 223  15   81
  (34,43] 198  24   56
```

```r
kable(t401)
```



|        |  No| Yes| NA|
|:-------|---:|---:|--:|
|[0,8]   | 256|  23| 92|
|(8,16]  | 226|  17| 71|
|(16,25] | 229|  21| 68|
|(25,34] | 223|  15| 81|
|(34,43] | 198|  24| 56|

```r
t401/as.integer(margin.table(t401, margin=1))*100
```

```
         
                 No       Yes      <NA>
  [0,8]   69.002695  6.199461 24.797844
  (8,16]  71.974522  5.414013 22.611465
  (16,25] 72.012579  6.603774 21.383648
  (25,34] 69.905956  4.702194 25.391850
  (34,43] 71.223022  8.633094 20.143885
```

```r
kable(round(t401/as.integer(margin.table(t401, margin=1))*100, 2))
```



|        |    No|  Yes|    NA|
|:-------|-----:|----:|-----:|
|[0,8]   | 69.00| 6.20| 24.80|
|(8,16]  | 71.97| 5.41| 22.61|
|(16,25] | 72.01| 6.60| 21.38|
|(25,34] | 69.91| 4.70| 25.39|
|(34,43] | 71.22| 8.63| 20.14|

```r
chisq.test(t401)
```

```

	Pearson's Chi-squared test

data:  t401
X-squared = 7.187, df = 8, p-value = 0.5166
```

```r
chisq.test(t401)$expected
```

```
         
                No     Yes  <NA>
  [0,8]   262.4825 23.1875 85.33
  (8,16]  222.1550 19.6250 72.22
  (16,25] 224.9850 19.8750 73.14
  (25,34] 225.6925 19.9375 73.37
  (34,43] 196.6850 17.3750 63.94
```

```r
chisq.test(t401, simulate.p.value=TRUE, B=9999)
```

```

	Pearson's Chi-squared test with simulated p-value (based on 9999
	replicates)

data:  t401
X-squared = 7.187, df = NA, p-value = 0.5236
```

```r
print("Cramer's V'")
```

```
[1] "Cramer's V'"
```

```r
cv.test(t401)
```

```
[1] 0.04739139
```

# Models

First we'll try a single level logistic and see how post-hoc analyses work.

As we have many missing values, will start with a saturated model and drop them sequentially, either by hand or through a function.


First we start with the most restricted not excluding weapons, violence or offender variables.


```r
excnames <- quote(c(complied_bin,
                      extortion_type, request,
                      month, time,
                      n_offenders, rel_offenders,
                      had_weapon,
                      with_violence,
                      reported,
                      rep_extortion_victim,
                      bribe_victim,
                      size, hotrestbar, yearsquant,
                      loghoms, logpop))


m1df <- enve_incvic[which(complete.cases(subset(enve_incvic,
                                                select=eval(excnames)))),]

## Single level first
m1 <- glm(complied_bin ~
           extortion_type + request +
           month + time +
           n_offenders + rel_offenders +
           had_weapon +
           with_violence +
           reported +
           rep_extortion_victim +
           bribe_victim +
           size + hotrestbar + yearsquant +
           loghoms + logpop,
         data=m1df,
         family = "binomial")
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```r
summary(m1)
```

```

Call:
glm(formula = complied_bin ~ extortion_type + request + month + 
    time + n_offenders + rel_offenders + had_weapon + with_violence + 
    reported + rep_extortion_victim + bribe_victim + size + hotrestbar + 
    yearsquant + loghoms + logpop, family = "binomial", data = m1df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.0275  -0.3828  -0.2120  -0.1070   2.9208  

Coefficients:
                                  Estimate Std. Error z value Pr(>|z|)   
(Intercept)                      -23.00808 3420.48305  -0.007  0.99463   
extortion_typeInternet             3.45872    1.82917   1.891  0.05864 . 
extortion_typeStreet               1.74660    0.73124   2.389  0.01691 * 
extortion_typePremises            34.97416 1829.78259   0.019  0.98475   
extortion_typeCobro de piso        3.11620    1.15217   2.705  0.00684 **
extortion_typeOther                2.29691    0.79660   2.883  0.00393 **
requestMoney                      19.73523 3420.48286   0.006  0.99540   
requestNothing                    19.60804 3420.48290   0.006  0.99543   
requestProduct                    20.09446 3420.48301   0.006  0.99531   
monthFebruary                     -1.49972    0.97330  -1.541  0.12335   
monthMarch                        -0.45563    0.85461  -0.533  0.59394   
monthApril                        -0.41755    0.94088  -0.444  0.65720   
monthMay                          -1.47454    1.07293  -1.374  0.16934   
monthJune                         -0.30320    0.77850  -0.389  0.69693   
monthJuly                         -0.51905    0.93978  -0.552  0.58073   
monthAugust                       -2.16868    1.16951  -1.854  0.06369 . 
monthSeptember                    -1.19653    0.92449  -1.294  0.19558   
monthOctober                      -2.11149    1.05509  -2.001  0.04537 * 
monthNovember                      0.69016    0.75342   0.916  0.35964   
monthDecember                     -1.75019    0.98551  -1.776  0.07575 . 
monthNONE                         -1.48782    1.37454  -1.082  0.27907   
timeAfternoon                      0.18412    0.41499   0.444  0.65727   
timeEvening                       -0.11572    0.56664  -0.204  0.83818   
timeNight                          2.30227    1.16393   1.978  0.04793 * 
timeDK/DA                          2.77762    1.79192   1.550  0.12112   
n_offenders2                       0.83139    0.42063   1.977  0.04809 * 
n_offenders3                       0.79492    0.75327   1.055  0.29129   
n_offenders4                       3.73842    1.38572   2.698  0.00698 **
n_offenders5                       3.67276    2.02259   1.816  0.06939 . 
n_offenders6+                      0.09550    1.44447   0.066  0.94729   
n_offendersDK/DA                  -0.10749    0.84182  -0.128  0.89840   
rel_offendersEmployee            -14.95652 1278.54222  -0.012  0.99067   
rel_offendersBarely known         -0.04015    0.92649  -0.043  0.96543   
rel_offendersSomewhat known       -1.02072    1.68324  -0.606  0.54425   
rel_offendersClose acquaintance    1.53121    1.74338   0.878  0.37978   
rel_offendersDK/DA                -1.53012    1.16685  -1.311  0.18975   
had_weaponYes                      1.02838    1.01543   1.013  0.31118   
had_weaponDK/DA                    0.30102    0.42792   0.703  0.48178   
with_violenceYes                   1.34049    1.02458   1.308  0.19076   
with_violenceDK/DA                 1.23082    0.70599   1.743  0.08126 . 
reportedYes                        0.64350    0.46180   1.393  0.16348   
rep_extortion_victim1              1.07540    0.64973   1.655  0.09789 . 
bribe_victim1                      0.11807    0.54136   0.218  0.82735   
sizeMedium                        -0.26661    0.53070  -0.502  0.61540   
sizeSmall                          0.91917    0.50353   1.825  0.06794 . 
sizeMicro                         -0.28025    0.57251  -0.490  0.62448   
hotrestbar1                       -0.93761    0.65602  -1.429  0.15294   
yearsquant(8,16]                  -0.34507    0.59359  -0.581  0.56102   
yearsquant(16,25]                  0.86546    0.52951   1.634  0.10216   
yearsquant(25,34]                 -0.80317    0.68455  -1.173  0.24068   
yearsquant(34,43]                  1.28150    0.56725   2.259  0.02388 * 
loghoms                           -0.27769    0.26342  -1.054  0.29181   
logpop                             0.59047    0.39950   1.478  0.13941   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 461.19  on 634  degrees of freedom
Residual deviance: 265.56  on 582  degrees of freedom
AIC: 371.56

Number of Fisher Scoring iterations: 17
```

```r
pR2(m1)
```

```
         llh      llhNull           G2     McFadden         r2ML 
-132.7789523 -230.5956669  195.6334292    0.4241915    0.2651465 
        r2CU 
   0.5135536 
```

```r
print(xtable(m1), type="html")
```

```
<!-- html table generated in R 3.3.0 by xtable 1.8-2 package -->
<!-- Tue Oct 18 17:16:09 2016 -->
<table border=1>
<tr> <th>  </th> <th> Estimate </th> <th> Std. Error </th> <th> z value </th> <th> Pr(&gt;|z|) </th>  </tr>
  <tr> <td align="right"> (Intercept) </td> <td align="right"> -23.0081 </td> <td align="right"> 3420.4830 </td> <td align="right"> -0.01 </td> <td align="right"> 0.9946 </td> </tr>
  <tr> <td align="right"> extortion_typeInternet </td> <td align="right"> 3.4587 </td> <td align="right"> 1.8292 </td> <td align="right"> 1.89 </td> <td align="right"> 0.0586 </td> </tr>
  <tr> <td align="right"> extortion_typeStreet </td> <td align="right"> 1.7466 </td> <td align="right"> 0.7312 </td> <td align="right"> 2.39 </td> <td align="right"> 0.0169 </td> </tr>
  <tr> <td align="right"> extortion_typePremises </td> <td align="right"> 34.9742 </td> <td align="right"> 1829.7826 </td> <td align="right"> 0.02 </td> <td align="right"> 0.9848 </td> </tr>
  <tr> <td align="right"> extortion_typeCobro de piso </td> <td align="right"> 3.1162 </td> <td align="right"> 1.1522 </td> <td align="right"> 2.70 </td> <td align="right"> 0.0068 </td> </tr>
  <tr> <td align="right"> extortion_typeOther </td> <td align="right"> 2.2969 </td> <td align="right"> 0.7966 </td> <td align="right"> 2.88 </td> <td align="right"> 0.0039 </td> </tr>
  <tr> <td align="right"> requestMoney </td> <td align="right"> 19.7352 </td> <td align="right"> 3420.4829 </td> <td align="right"> 0.01 </td> <td align="right"> 0.9954 </td> </tr>
  <tr> <td align="right"> requestNothing </td> <td align="right"> 19.6080 </td> <td align="right"> 3420.4829 </td> <td align="right"> 0.01 </td> <td align="right"> 0.9954 </td> </tr>
  <tr> <td align="right"> requestProduct </td> <td align="right"> 20.0945 </td> <td align="right"> 3420.4830 </td> <td align="right"> 0.01 </td> <td align="right"> 0.9953 </td> </tr>
  <tr> <td align="right"> monthFebruary </td> <td align="right"> -1.4997 </td> <td align="right"> 0.9733 </td> <td align="right"> -1.54 </td> <td align="right"> 0.1234 </td> </tr>
  <tr> <td align="right"> monthMarch </td> <td align="right"> -0.4556 </td> <td align="right"> 0.8546 </td> <td align="right"> -0.53 </td> <td align="right"> 0.5939 </td> </tr>
  <tr> <td align="right"> monthApril </td> <td align="right"> -0.4176 </td> <td align="right"> 0.9409 </td> <td align="right"> -0.44 </td> <td align="right"> 0.6572 </td> </tr>
  <tr> <td align="right"> monthMay </td> <td align="right"> -1.4745 </td> <td align="right"> 1.0729 </td> <td align="right"> -1.37 </td> <td align="right"> 0.1693 </td> </tr>
  <tr> <td align="right"> monthJune </td> <td align="right"> -0.3032 </td> <td align="right"> 0.7785 </td> <td align="right"> -0.39 </td> <td align="right"> 0.6969 </td> </tr>
  <tr> <td align="right"> monthJuly </td> <td align="right"> -0.5191 </td> <td align="right"> 0.9398 </td> <td align="right"> -0.55 </td> <td align="right"> 0.5807 </td> </tr>
  <tr> <td align="right"> monthAugust </td> <td align="right"> -2.1687 </td> <td align="right"> 1.1695 </td> <td align="right"> -1.85 </td> <td align="right"> 0.0637 </td> </tr>
  <tr> <td align="right"> monthSeptember </td> <td align="right"> -1.1965 </td> <td align="right"> 0.9245 </td> <td align="right"> -1.29 </td> <td align="right"> 0.1956 </td> </tr>
  <tr> <td align="right"> monthOctober </td> <td align="right"> -2.1115 </td> <td align="right"> 1.0551 </td> <td align="right"> -2.00 </td> <td align="right"> 0.0454 </td> </tr>
  <tr> <td align="right"> monthNovember </td> <td align="right"> 0.6902 </td> <td align="right"> 0.7534 </td> <td align="right"> 0.92 </td> <td align="right"> 0.3596 </td> </tr>
  <tr> <td align="right"> monthDecember </td> <td align="right"> -1.7502 </td> <td align="right"> 0.9855 </td> <td align="right"> -1.78 </td> <td align="right"> 0.0757 </td> </tr>
  <tr> <td align="right"> monthNONE </td> <td align="right"> -1.4878 </td> <td align="right"> 1.3745 </td> <td align="right"> -1.08 </td> <td align="right"> 0.2791 </td> </tr>
  <tr> <td align="right"> timeAfternoon </td> <td align="right"> 0.1841 </td> <td align="right"> 0.4150 </td> <td align="right"> 0.44 </td> <td align="right"> 0.6573 </td> </tr>
  <tr> <td align="right"> timeEvening </td> <td align="right"> -0.1157 </td> <td align="right"> 0.5666 </td> <td align="right"> -0.20 </td> <td align="right"> 0.8382 </td> </tr>
  <tr> <td align="right"> timeNight </td> <td align="right"> 2.3023 </td> <td align="right"> 1.1639 </td> <td align="right"> 1.98 </td> <td align="right"> 0.0479 </td> </tr>
  <tr> <td align="right"> timeDK/DA </td> <td align="right"> 2.7776 </td> <td align="right"> 1.7919 </td> <td align="right"> 1.55 </td> <td align="right"> 0.1211 </td> </tr>
  <tr> <td align="right"> n_offenders2 </td> <td align="right"> 0.8314 </td> <td align="right"> 0.4206 </td> <td align="right"> 1.98 </td> <td align="right"> 0.0481 </td> </tr>
  <tr> <td align="right"> n_offenders3 </td> <td align="right"> 0.7949 </td> <td align="right"> 0.7533 </td> <td align="right"> 1.06 </td> <td align="right"> 0.2913 </td> </tr>
  <tr> <td align="right"> n_offenders4 </td> <td align="right"> 3.7384 </td> <td align="right"> 1.3857 </td> <td align="right"> 2.70 </td> <td align="right"> 0.0070 </td> </tr>
  <tr> <td align="right"> n_offenders5 </td> <td align="right"> 3.6728 </td> <td align="right"> 2.0226 </td> <td align="right"> 1.82 </td> <td align="right"> 0.0694 </td> </tr>
  <tr> <td align="right"> n_offenders6+ </td> <td align="right"> 0.0955 </td> <td align="right"> 1.4445 </td> <td align="right"> 0.07 </td> <td align="right"> 0.9473 </td> </tr>
  <tr> <td align="right"> n_offendersDK/DA </td> <td align="right"> -0.1075 </td> <td align="right"> 0.8418 </td> <td align="right"> -0.13 </td> <td align="right"> 0.8984 </td> </tr>
  <tr> <td align="right"> rel_offendersEmployee </td> <td align="right"> -14.9565 </td> <td align="right"> 1278.5422 </td> <td align="right"> -0.01 </td> <td align="right"> 0.9907 </td> </tr>
  <tr> <td align="right"> rel_offendersBarely known </td> <td align="right"> -0.0402 </td> <td align="right"> 0.9265 </td> <td align="right"> -0.04 </td> <td align="right"> 0.9654 </td> </tr>
  <tr> <td align="right"> rel_offendersSomewhat known </td> <td align="right"> -1.0207 </td> <td align="right"> 1.6832 </td> <td align="right"> -0.61 </td> <td align="right"> 0.5442 </td> </tr>
  <tr> <td align="right"> rel_offendersClose acquaintance </td> <td align="right"> 1.5312 </td> <td align="right"> 1.7434 </td> <td align="right"> 0.88 </td> <td align="right"> 0.3798 </td> </tr>
  <tr> <td align="right"> rel_offendersDK/DA </td> <td align="right"> -1.5301 </td> <td align="right"> 1.1669 </td> <td align="right"> -1.31 </td> <td align="right"> 0.1897 </td> </tr>
  <tr> <td align="right"> had_weaponYes </td> <td align="right"> 1.0284 </td> <td align="right"> 1.0154 </td> <td align="right"> 1.01 </td> <td align="right"> 0.3112 </td> </tr>
  <tr> <td align="right"> had_weaponDK/DA </td> <td align="right"> 0.3010 </td> <td align="right"> 0.4279 </td> <td align="right"> 0.70 </td> <td align="right"> 0.4818 </td> </tr>
  <tr> <td align="right"> with_violenceYes </td> <td align="right"> 1.3405 </td> <td align="right"> 1.0246 </td> <td align="right"> 1.31 </td> <td align="right"> 0.1908 </td> </tr>
  <tr> <td align="right"> with_violenceDK/DA </td> <td align="right"> 1.2308 </td> <td align="right"> 0.7060 </td> <td align="right"> 1.74 </td> <td align="right"> 0.0813 </td> </tr>
  <tr> <td align="right"> reportedYes </td> <td align="right"> 0.6435 </td> <td align="right"> 0.4618 </td> <td align="right"> 1.39 </td> <td align="right"> 0.1635 </td> </tr>
  <tr> <td align="right"> rep_extortion_victim1 </td> <td align="right"> 1.0754 </td> <td align="right"> 0.6497 </td> <td align="right"> 1.66 </td> <td align="right"> 0.0979 </td> </tr>
  <tr> <td align="right"> bribe_victim1 </td> <td align="right"> 0.1181 </td> <td align="right"> 0.5414 </td> <td align="right"> 0.22 </td> <td align="right"> 0.8273 </td> </tr>
  <tr> <td align="right"> sizeMedium </td> <td align="right"> -0.2666 </td> <td align="right"> 0.5307 </td> <td align="right"> -0.50 </td> <td align="right"> 0.6154 </td> </tr>
  <tr> <td align="right"> sizeSmall </td> <td align="right"> 0.9192 </td> <td align="right"> 0.5035 </td> <td align="right"> 1.83 </td> <td align="right"> 0.0679 </td> </tr>
  <tr> <td align="right"> sizeMicro </td> <td align="right"> -0.2803 </td> <td align="right"> 0.5725 </td> <td align="right"> -0.49 </td> <td align="right"> 0.6245 </td> </tr>
  <tr> <td align="right"> hotrestbar1 </td> <td align="right"> -0.9376 </td> <td align="right"> 0.6560 </td> <td align="right"> -1.43 </td> <td align="right"> 0.1529 </td> </tr>
  <tr> <td align="right"> yearsquant(8,16] </td> <td align="right"> -0.3451 </td> <td align="right"> 0.5936 </td> <td align="right"> -0.58 </td> <td align="right"> 0.5610 </td> </tr>
  <tr> <td align="right"> yearsquant(16,25] </td> <td align="right"> 0.8655 </td> <td align="right"> 0.5295 </td> <td align="right"> 1.63 </td> <td align="right"> 0.1022 </td> </tr>
  <tr> <td align="right"> yearsquant(25,34] </td> <td align="right"> -0.8032 </td> <td align="right"> 0.6846 </td> <td align="right"> -1.17 </td> <td align="right"> 0.2407 </td> </tr>
  <tr> <td align="right"> yearsquant(34,43] </td> <td align="right"> 1.2815 </td> <td align="right"> 0.5673 </td> <td align="right"> 2.26 </td> <td align="right"> 0.0239 </td> </tr>
  <tr> <td align="right"> loghoms </td> <td align="right"> -0.2777 </td> <td align="right"> 0.2634 </td> <td align="right"> -1.05 </td> <td align="right"> 0.2918 </td> </tr>
  <tr> <td align="right"> logpop </td> <td align="right"> 0.5905 </td> <td align="right"> 0.3995 </td> <td align="right"> 1.48 </td> <td align="right"> 0.1394 </td> </tr>
   </table>
```

```r
htmlreg(m1)
```

```

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
<caption align="bottom" style="margin-top:0.3em;">Statistical models</caption>
<tr>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b></b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 1</b></th>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">(Intercept)</td>
<td style="padding-right: 12px; border: none;">-23.01</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeInternet</td>
<td style="padding-right: 12px; border: none;">3.46</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.83)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeStreet</td>
<td style="padding-right: 12px; border: none;">1.75<sup style="vertical-align: 0px;">*</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.73)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typePremises</td>
<td style="padding-right: 12px; border: none;">34.97</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1829.78)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeCobro de piso</td>
<td style="padding-right: 12px; border: none;">3.12<sup style="vertical-align: 0px;">**</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.15)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeOther</td>
<td style="padding-right: 12px; border: none;">2.30<sup style="vertical-align: 0px;">**</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.80)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestMoney</td>
<td style="padding-right: 12px; border: none;">19.74</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestNothing</td>
<td style="padding-right: 12px; border: none;">19.61</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestProduct</td>
<td style="padding-right: 12px; border: none;">20.09</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthFebruary</td>
<td style="padding-right: 12px; border: none;">-1.50</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.97)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthMarch</td>
<td style="padding-right: 12px; border: none;">-0.46</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.85)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthApril</td>
<td style="padding-right: 12px; border: none;">-0.42</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.94)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthMay</td>
<td style="padding-right: 12px; border: none;">-1.47</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.07)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthJune</td>
<td style="padding-right: 12px; border: none;">-0.30</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.78)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthJuly</td>
<td style="padding-right: 12px; border: none;">-0.52</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.94)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthAugust</td>
<td style="padding-right: 12px; border: none;">-2.17</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.17)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthSeptember</td>
<td style="padding-right: 12px; border: none;">-1.20</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.92)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthOctober</td>
<td style="padding-right: 12px; border: none;">-2.11<sup style="vertical-align: 0px;">*</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.06)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthNovember</td>
<td style="padding-right: 12px; border: none;">0.69</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.75)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthDecember</td>
<td style="padding-right: 12px; border: none;">-1.75</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.99)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthNONE</td>
<td style="padding-right: 12px; border: none;">-1.49</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.37)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeAfternoon</td>
<td style="padding-right: 12px; border: none;">0.18</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.41)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeEvening</td>
<td style="padding-right: 12px; border: none;">-0.12</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeNight</td>
<td style="padding-right: 12px; border: none;">2.30<sup style="vertical-align: 0px;">*</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.16)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeDK/DA</td>
<td style="padding-right: 12px; border: none;">2.78</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.79)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders2</td>
<td style="padding-right: 12px; border: none;">0.83<sup style="vertical-align: 0px;">*</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.42)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders3</td>
<td style="padding-right: 12px; border: none;">0.79</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.75)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders4</td>
<td style="padding-right: 12px; border: none;">3.74<sup style="vertical-align: 0px;">**</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.39)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders5</td>
<td style="padding-right: 12px; border: none;">3.67</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(2.02)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders6+</td>
<td style="padding-right: 12px; border: none;">0.10</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.44)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offendersDK/DA</td>
<td style="padding-right: 12px; border: none;">-0.11</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.84)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersEmployee</td>
<td style="padding-right: 12px; border: none;">-14.96</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1278.54)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersBarely known</td>
<td style="padding-right: 12px; border: none;">-0.04</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.93)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersSomewhat known</td>
<td style="padding-right: 12px; border: none;">-1.02</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.68)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersClose acquaintance</td>
<td style="padding-right: 12px; border: none;">1.53</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.74)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersDK/DA</td>
<td style="padding-right: 12px; border: none;">-1.53</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.17)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">had_weaponYes</td>
<td style="padding-right: 12px; border: none;">1.03</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.02)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">had_weaponDK/DA</td>
<td style="padding-right: 12px; border: none;">0.30</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.43)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">with_violenceYes</td>
<td style="padding-right: 12px; border: none;">1.34</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.02)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">with_violenceDK/DA</td>
<td style="padding-right: 12px; border: none;">1.23</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.71)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">reportedYes</td>
<td style="padding-right: 12px; border: none;">0.64</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.46)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rep_extortion_victim1</td>
<td style="padding-right: 12px; border: none;">1.08</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.65)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">bribe_victim1</td>
<td style="padding-right: 12px; border: none;">0.12</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.54)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeMedium</td>
<td style="padding-right: 12px; border: none;">-0.27</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.53)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeSmall</td>
<td style="padding-right: 12px; border: none;">0.92</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.50)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeMicro</td>
<td style="padding-right: 12px; border: none;">-0.28</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">hotrestbar1</td>
<td style="padding-right: 12px; border: none;">-0.94</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.66)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(8,16]</td>
<td style="padding-right: 12px; border: none;">-0.35</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.59)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(16,25]</td>
<td style="padding-right: 12px; border: none;">0.87</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.53)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(25,34]</td>
<td style="padding-right: 12px; border: none;">-0.80</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.68)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(34,43]</td>
<td style="padding-right: 12px; border: none;">1.28<sup style="vertical-align: 0px;">*</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">loghoms</td>
<td style="padding-right: 12px; border: none;">-0.28</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.26)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">logpop</td>
<td style="padding-right: 12px; border: none;">0.59</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.40)</td>
</tr>
<tr>
<td style="border-top: 1px solid black;">AIC</td>
<td style="border-top: 1px solid black;">371.56</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">BIC</td>
<td style="padding-right: 12px; border: none;">607.60</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Log Likelihood</td>
<td style="padding-right: 12px; border: none;">-132.78</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Deviance</td>
<td style="padding-right: 12px; border: none;">265.56</td>
</tr>
<tr>
<td style="border-bottom: 2px solid black;">Num. obs.</td>
<td style="border-bottom: 2px solid black;">635</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;" colspan="3"><span style="font-size:0.8em"><sup style="vertical-align: 0px;">***</sup>p &lt; 0.001, <sup style="vertical-align: 0px;">**</sup>p &lt; 0.01, <sup style="vertical-align: 0px;">*</sup>p &lt; 0.05</span></td>
</tr>
</table>
```

```r
screenreg(m1)
```

```

============================================
                                 Model 1    
--------------------------------------------
(Intercept)                        -23.01   
                                 (3420.48)  
extortion_typeInternet               3.46   
                                    (1.83)  
extortion_typeStreet                 1.75 * 
                                    (0.73)  
extortion_typePremises              34.97   
                                 (1829.78)  
extortion_typeCobro de piso          3.12 **
                                    (1.15)  
extortion_typeOther                  2.30 **
                                    (0.80)  
requestMoney                        19.74   
                                 (3420.48)  
requestNothing                      19.61   
                                 (3420.48)  
requestProduct                      20.09   
                                 (3420.48)  
monthFebruary                       -1.50   
                                    (0.97)  
monthMarch                          -0.46   
                                    (0.85)  
monthApril                          -0.42   
                                    (0.94)  
monthMay                            -1.47   
                                    (1.07)  
monthJune                           -0.30   
                                    (0.78)  
monthJuly                           -0.52   
                                    (0.94)  
monthAugust                         -2.17   
                                    (1.17)  
monthSeptember                      -1.20   
                                    (0.92)  
monthOctober                        -2.11 * 
                                    (1.06)  
monthNovember                        0.69   
                                    (0.75)  
monthDecember                       -1.75   
                                    (0.99)  
monthNONE                           -1.49   
                                    (1.37)  
timeAfternoon                        0.18   
                                    (0.41)  
timeEvening                         -0.12   
                                    (0.57)  
timeNight                            2.30 * 
                                    (1.16)  
timeDK/DA                            2.78   
                                    (1.79)  
n_offenders2                         0.83 * 
                                    (0.42)  
n_offenders3                         0.79   
                                    (0.75)  
n_offenders4                         3.74 **
                                    (1.39)  
n_offenders5                         3.67   
                                    (2.02)  
n_offenders6+                        0.10   
                                    (1.44)  
n_offendersDK/DA                    -0.11   
                                    (0.84)  
rel_offendersEmployee              -14.96   
                                 (1278.54)  
rel_offendersBarely known           -0.04   
                                    (0.93)  
rel_offendersSomewhat known         -1.02   
                                    (1.68)  
rel_offendersClose acquaintance      1.53   
                                    (1.74)  
rel_offendersDK/DA                  -1.53   
                                    (1.17)  
had_weaponYes                        1.03   
                                    (1.02)  
had_weaponDK/DA                      0.30   
                                    (0.43)  
with_violenceYes                     1.34   
                                    (1.02)  
with_violenceDK/DA                   1.23   
                                    (0.71)  
reportedYes                          0.64   
                                    (0.46)  
rep_extortion_victim1                1.08   
                                    (0.65)  
bribe_victim1                        0.12   
                                    (0.54)  
sizeMedium                          -0.27   
                                    (0.53)  
sizeSmall                            0.92   
                                    (0.50)  
sizeMicro                           -0.28   
                                    (0.57)  
hotrestbar1                         -0.94   
                                    (0.66)  
yearsquant(8,16]                    -0.35   
                                    (0.59)  
yearsquant(16,25]                    0.87   
                                    (0.53)  
yearsquant(25,34]                   -0.80   
                                    (0.68)  
yearsquant(34,43]                    1.28 * 
                                    (0.57)  
loghoms                             -0.28   
                                    (0.26)  
logpop                               0.59   
                                    (0.40)  
--------------------------------------------
AIC                                371.56   
BIC                                607.60   
Log Likelihood                    -132.78   
Deviance                           265.56   
Num. obs.                          635      
============================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
nobs(m1)
```

```
[1] 635
```

```r
confint(m1)
```

```
Waiting for profiling to be done...
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                        2.5 %       97.5 %
(Intercept)                                NA 504.28654793
extortion_typeInternet            -0.47878920   7.05452952
extortion_typeStreet               0.28291290   3.17187355
extortion_typePremises           -75.49416004           NA
extortion_typeCobro de piso        0.85565266   5.42951587
extortion_typeOther                0.71783149   3.87700709
requestMoney                    -511.00167404           NA
requestNothing                  -508.64108091           NA
requestProduct                  -396.30669868           NA
monthFebruary                     -3.54122767   0.36806634
monthMarch                        -2.13658907   1.26907159
monthApril                        -2.36225655   1.42881548
monthMay                          -3.79752864   0.54061088
monthJune                         -1.79420856   1.30442450
monthJuly                         -2.45553738   1.32669733
monthAugust                       -4.78478448  -0.02588744
monthSeptember                    -3.06459981   0.62653060
monthOctober                      -4.38436202  -0.12780548
monthNovember                     -0.73200061   2.26984988
monthDecember                     -3.86260215   0.13353450
monthNONE                         -4.52184347   1.03566160
timeAfternoon                     -0.62187828   1.01526596
timeEvening                       -1.26496680   0.97436769
timeNight                         -0.28007052   4.46164497
timeDK/DA                         -1.11479886   6.15220542
n_offenders2                      -0.01065961   1.65006793
n_offenders3                      -0.77871723   2.20716631
n_offenders4                       1.05299997   6.56631354
n_offenders5                      -0.05091056   7.90665594
n_offenders6+                     -2.88529663   2.85086906
n_offendersDK/DA                  -2.01590145   1.38499796
rel_offendersEmployee           -457.32975686  35.82149219
rel_offendersBarely known         -1.91165976   1.76397328
rel_offendersSomewhat known       -4.81124184   1.92214275
rel_offendersClose acquaintance   -2.25440089   4.93765413
rel_offendersDK/DA                -4.58697650   0.36556392
had_weaponYes                     -0.89915231   3.09788138
had_weaponDK/DA                   -0.53062815   1.16087336
with_violenceYes                  -0.66436955   3.38712480
with_violenceDK/DA                -0.25288764   2.56308052
reportedYes                       -0.29960188   1.52507970
rep_extortion_victim1             -0.27772993   2.29869245
bribe_victim1                     -1.02572323   1.12246026
sizeMedium                        -1.31760312   0.78323452
sizeSmall                         -0.05242738   1.93849675
sizeMicro                         -1.42497776   0.84479621
hotrestbar1                       -2.38043377   0.23489015
yearsquant(8,16]                  -1.55289145   0.80278908
yearsquant(16,25]                 -0.16485271   1.92995614
yearsquant(25,34]                 -2.22837468   0.49423094
yearsquant(34,43]                  0.18406758   2.42665519
loghoms                           -0.80389546   0.23397616
logpop                            -0.17909334   1.39457691
```

```r
# compare with null
lrtest(m1)
```

```
Likelihood ratio test

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
Model 2: complied_bin ~ 1
  #Df  LogLik  Df  Chisq            Pr(>Chisq)    
1  53 -132.78                                     
2   1 -230.60 -52 195.63 < 0.00000000000000022 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(lrtest(m1))
```



| #Df|    LogLik|  Df|    Chisq| Pr(>Chisq)|
|---:|---------:|---:|--------:|----------:|
|  53| -132.7790|  NA|       NA|         NA|
|   1| -230.5957| -52| 195.6334|          0|

```r
waldtest(m1)
```

```
Wald test

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
Model 2: complied_bin ~ 1
  Res.Df  Df      F   Pr(>F)   
1    582                       
2    634 -52 1.5745 0.007829 **
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(waldtest(m1))
```



| Res.Df|  Df|        F|    Pr(>F)|
|------:|---:|--------:|---------:|
|    582|  NA|       NA|        NA|
|    634| -52| 1.574471| 0.0078285|

```r
waldtest(m1, test="Chisq")
```

```
Wald test

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
Model 2: complied_bin ~ 1
  Res.Df  Df  Chisq Pr(>Chisq)   
1    582                         
2    634 -52 81.873   0.005137 **
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(waldtest(m1, test="Chisq"))
```



| Res.Df|  Df|    Chisq| Pr(>Chisq)|
|------:|---:|--------:|----------:|
|    582|  NA|       NA|         NA|
|    634| -52| 81.87249|  0.0051366|

```r
# compare sequentially
anova(m1, test="Rao")
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Analysis of Deviance Table

Model: binomial, link: logit

Response: complied_bin

Terms added sequentially (first to last)

                     Df Deviance Resid. Df Resid. Dev     Rao
NULL                                   634     461.19        
extortion_type        5  113.579       629     347.61 169.717
request               3    3.059       626     344.55   2.669
month                12   14.095       614     330.46  14.365
time                  4    5.078       610     325.38   7.274
n_offenders           6   18.742       604     306.64  20.287
rel_offenders         5    3.724       599     302.92   3.161
had_weapon            2    2.567       597     300.35   2.509
with_violence         2    4.577       595     295.77   5.263
reported              1    1.101       594     294.67   1.168
rep_extortion_victim  1    1.576       593     293.09   1.765
bribe_victim          1    0.014       592     293.08   0.014
size                  3    9.410       589     283.67  10.453
hotrestbar            1    0.992       588     282.68   0.919
yearsquant            4   14.856       584     267.82  15.086
loghoms               1    0.017       583     267.81   0.017
logpop                1    2.247       582     265.56   2.210
                                  Pr(>Chi)    
NULL                                          
extortion_type       < 0.00000000000000022 ***
request                           0.445570    
month                             0.278015    
time                              0.122090    
n_offenders                       0.002462 ** 
rel_offenders                     0.675148    
had_weapon                        0.285191    
with_violence                     0.071982 .  
reported                          0.279786    
rep_extortion_victim              0.184000    
bribe_victim                      0.905226    
size                              0.015082 *  
hotrestbar                        0.337643    
yearsquant                        0.004527 ** 
loghoms                           0.895860    
logpop                            0.137082    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(anova(m1, test="Rao"))
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```



|                     | Df|    Deviance| Resid. Df| Resid. Dev|         Rao|  Pr(>Chi)|
|:--------------------|--:|-----------:|---------:|----------:|-----------:|---------:|
|NULL                 | NA|          NA|       634|   461.1913|          NA|        NA|
|extortion_type       |  5| 113.5790952|       629|   347.6122| 169.7168416| 0.0000000|
|request              |  3|   3.0587413|       626|   344.5535|   2.6687176| 0.4455696|
|month                | 12|  14.0946137|       614|   330.4589|  14.3649146| 0.2780149|
|time                 |  4|   5.0779145|       610|   325.3810|   7.2741742| 0.1220895|
|n_offenders          |  6|  18.7417323|       604|   306.6392|  20.2870370| 0.0024616|
|rel_offenders        |  5|   3.7235300|       599|   302.9157|   3.1612064| 0.6751485|
|had_weapon           |  2|   2.5670675|       597|   300.3486|   2.5091957| 0.2851905|
|with_violence        |  2|   4.5765832|       595|   295.7721|   5.2626919| 0.0719815|
|reported             |  1|   1.1013230|       594|   294.6707|   1.1681279| 0.2797862|
|rep_extortion_victim |  1|   1.5764483|       593|   293.0943|   1.7650134| 0.1840004|
|bribe_victim         |  1|   0.0140594|       592|   293.0802|   0.0141759| 0.9052257|
|size                 |  3|   9.4100075|       589|   283.6702|  10.4532216| 0.0150816|
|hotrestbar           |  1|   0.9922158|       588|   282.6780|   0.9193589| 0.3376434|
|yearsquant           |  4|  14.8557847|       584|   267.8222|  15.0857804| 0.0045266|
|loghoms              |  1|   0.0171549|       583|   267.8051|   0.0171331| 0.8958596|
|logpop               |  1|   2.2471580|       582|   265.5579|   2.2104134| 0.1370819|

```r
anova(m1, test="LRT")
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Analysis of Deviance Table

Model: binomial, link: logit

Response: complied_bin

Terms added sequentially (first to last)

                     Df Deviance Resid. Df Resid. Dev
NULL                                   634     461.19
extortion_type        5  113.579       629     347.61
request               3    3.059       626     344.55
month                12   14.095       614     330.46
time                  4    5.078       610     325.38
n_offenders           6   18.742       604     306.64
rel_offenders         5    3.724       599     302.92
had_weapon            2    2.567       597     300.35
with_violence         2    4.577       595     295.77
reported              1    1.101       594     294.67
rep_extortion_victim  1    1.576       593     293.09
bribe_victim          1    0.014       592     293.08
size                  3    9.410       589     283.67
hotrestbar            1    0.992       588     282.68
yearsquant            4   14.856       584     267.82
loghoms               1    0.017       583     267.81
logpop                1    2.247       582     265.56
                                  Pr(>Chi)    
NULL                                          
extortion_type       < 0.00000000000000022 ***
request                           0.382657    
month                             0.294707    
time                              0.279396    
n_offenders                       0.004623 ** 
rel_offenders                     0.589866    
had_weapon                        0.277057    
with_violence                     0.101440    
reported                          0.293976    
rep_extortion_victim              0.209273    
bribe_victim                      0.905614    
size                              0.024308 *  
hotrestbar                        0.319201    
yearsquant                        0.005010 ** 
loghoms                           0.895794    
logpop                            0.133860    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(anova(m1, test="LRT"))
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```



|                     | Df|    Deviance| Resid. Df| Resid. Dev|  Pr(>Chi)|
|:--------------------|--:|-----------:|---------:|----------:|---------:|
|NULL                 | NA|          NA|       634|   461.1913|        NA|
|extortion_type       |  5| 113.5790952|       629|   347.6122| 0.0000000|
|request              |  3|   3.0587413|       626|   344.5535| 0.3826568|
|month                | 12|  14.0946137|       614|   330.4589| 0.2947073|
|time                 |  4|   5.0779145|       610|   325.3810| 0.2793960|
|n_offenders          |  6|  18.7417323|       604|   306.6392| 0.0046228|
|rel_offenders        |  5|   3.7235300|       599|   302.9157| 0.5898663|
|had_weapon           |  2|   2.5670675|       597|   300.3486| 0.2770565|
|with_violence        |  2|   4.5765832|       595|   295.7721| 0.1014396|
|reported             |  1|   1.1013230|       594|   294.6707| 0.2939759|
|rep_extortion_victim |  1|   1.5764483|       593|   293.0943| 0.2092730|
|bribe_victim         |  1|   0.0140594|       592|   293.0802| 0.9056143|
|size                 |  3|   9.4100075|       589|   283.6702| 0.0243083|
|hotrestbar           |  1|   0.9922158|       588|   282.6780| 0.3192014|
|yearsquant           |  4|  14.8557847|       584|   267.8222| 0.0050099|
|loghoms              |  1|   0.0171549|       583|   267.8051| 0.8957937|
|logpop               |  1|   2.2471580|       582|   265.5579| 0.1338600|

```r
# Test for multicollinearity
vif(m1)
```

```
                          GVIF Df GVIF^(1/(2*Df))
extortion_type       18.862159  5        1.341403
request               1.705418  3        1.093046
month                11.398311 12        1.106713
time                  3.416772  4        1.166009
n_offenders          10.708077  6        1.218454
rel_offenders         9.661695  5        1.254600
had_weapon            5.115078  2        1.503880
with_violence         2.891424  2        1.304001
reported              1.340013  1        1.157589
rep_extortion_victim  1.329973  1        1.153245
bribe_victim          1.176352  1        1.084597
size                  2.198235  3        1.140282
hotrestbar            1.174707  1        1.083839
yearsquant            3.206718  4        1.156798
loghoms               3.160473  1        1.777772
logpop                3.136699  1        1.771073
```

```r
# another test for multicollinearity, but using an lm instead
lm1 <- lm(as.integer(complied_bin) ~
           extortion_type + request +
           month + time +
           n_offenders + rel_offenders +
           had_weapon +
           with_violence +
           reported +
           rep_extortion_victim +
           bribe_victim +
           size + hotrestbar + yearsquant +
           loghoms + logpop,
         data=m1df)

summary(lm1)
```

```

Call:
lm(formula = as.integer(complied_bin) ~ extortion_type + request + 
    month + time + n_offenders + rel_offenders + had_weapon + 
    with_violence + reported + rep_extortion_victim + bribe_victim + 
    size + hotrestbar + yearsquant + loghoms + logpop, data = m1df)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.61180 -0.11871 -0.04511  0.02588  0.99099 

Coefficients:
                                 Estimate Std. Error t value
(Intercept)                      0.703952   0.208489   3.376
extortion_typeInternet           0.293303   0.169391   1.732
extortion_typeStreet             0.221170   0.060889   3.632
extortion_typePremises           0.676970   0.104478   6.480
extortion_typeCobro de piso      0.422365   0.109441   3.859
extortion_typeOther              0.306342   0.077230   3.967
requestMoney                     0.348625   0.200984   1.735
requestNothing                   0.310659   0.205768   1.510
requestProduct                   0.386275   0.210293   1.837
monthFebruary                   -0.075620   0.062316  -1.213
monthMarch                      -0.019151   0.060708  -0.315
monthApril                      -0.035941   0.066040  -0.544
monthMay                        -0.067511   0.063241  -1.068
monthJune                       -0.003698   0.058322  -0.063
monthJuly                       -0.044709   0.064324  -0.695
monthAugust                     -0.115921   0.062418  -1.857
monthSeptember                  -0.059144   0.060661  -0.975
monthOctober                    -0.094798   0.060294  -1.572
monthNovember                    0.062584   0.058494   1.070
monthDecember                   -0.081051   0.057110  -1.419
monthNONE                       -0.055596   0.089704  -0.620
timeAfternoon                   -0.005702   0.025436  -0.224
timeEvening                      0.002251   0.034116   0.066
timeNight                        0.167592   0.106076   1.580
timeDK/DA                        0.282253   0.166561   1.695
n_offenders2                     0.065969   0.028800   2.291
n_offenders3                     0.052198   0.053675   0.972
n_offenders4                     0.383947   0.111000   3.459
n_offenders5                     0.245155   0.120271   2.038
n_offenders6+                   -0.046921   0.125436  -0.374
n_offendersDK/DA                -0.017110   0.050381  -0.340
rel_offendersEmployee            0.016616   0.163920   0.101
rel_offendersBarely known       -0.080109   0.083241  -0.962
rel_offendersSomewhat known     -0.112760   0.129799  -0.869
rel_offendersClose acquaintance  0.104061   0.149429   0.696
rel_offendersDK/DA              -0.049277   0.047724  -1.033
had_weaponYes                    0.164135   0.079450   2.066
had_weaponDK/DA                  0.010816   0.024244   0.446
with_violenceYes                 0.184183   0.089663   2.054
with_violenceDK/DA               0.083240   0.048011   1.734
reportedYes                      0.036136   0.031464   1.148
rep_extortion_victim1            0.079254   0.045296   1.750
bribe_victim1                    0.013881   0.032000   0.434
sizeMedium                      -0.018426   0.031474  -0.585
sizeSmall                        0.063449   0.033981   1.867
sizeMicro                       -0.016820   0.031068  -0.541
hotrestbar1                     -0.031565   0.031887  -0.990
yearsquant(8,16]                -0.004108   0.033683  -0.122
yearsquant(16,25]                0.057361   0.034095   1.682
yearsquant(25,34]               -0.022613   0.034197  -0.661
yearsquant(34,43]                0.085258   0.035857   2.378
loghoms                         -0.005723   0.015835  -0.361
logpop                           0.024094   0.023542   1.023
                                      Pr(>|t|)    
(Intercept)                           0.000783 ***
extortion_typeInternet                0.083890 .  
extortion_typeStreet                  0.000306 ***
extortion_typePremises          0.000000000196 ***
extortion_typeCobro de piso           0.000126 ***
extortion_typeOther             0.000081984003 ***
requestMoney                          0.083342 .  
requestNothing                        0.131648    
requestProduct                        0.066743 .  
monthFebruary                         0.225439    
monthMarch                            0.752526    
monthApril                            0.586488    
monthMay                              0.286183    
monthJune                             0.949459    
monthJuly                             0.487293    
monthAugust                           0.063791 .  
monthSeptember                        0.329965    
monthOctober                          0.116430    
monthNovember                         0.285095    
monthDecember                         0.156371    
monthNONE                             0.535653    
timeAfternoon                         0.822687    
timeEvening                           0.947423    
timeNight                             0.114667    
timeDK/DA                             0.090688 .  
n_offenders2                          0.022344 *  
n_offenders3                          0.331219    
n_offenders4                          0.000582 ***
n_offenders5                          0.041966 *  
n_offenders6+                         0.708492    
n_offendersDK/DA                      0.734279    
rel_offendersEmployee                 0.919296    
rel_offendersBarely known             0.336260    
rel_offendersSomewhat known           0.385353    
rel_offendersClose acquaintance       0.486463    
rel_offendersDK/DA                    0.302250    
had_weaponYes                         0.039281 *  
had_weaponDK/DA                       0.655685    
with_violenceYes                      0.040405 *  
with_violenceDK/DA                    0.083487 .  
reportedYes                           0.251246    
rep_extortion_victim1                 0.080702 .  
bribe_victim1                         0.664614    
sizeMedium                            0.558485    
sizeSmall                             0.062378 .  
sizeMicro                             0.588441    
hotrestbar1                           0.322637    
yearsquant(8,16]                      0.902982    
yearsquant(16,25]                     0.093034 .  
yearsquant(25,34]                     0.508707    
yearsquant(34,43]                     0.017741 *  
loghoms                               0.717924    
logpop                                0.306528    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.2665 on 582 degrees of freedom
Multiple R-squared:  0.3751,	Adjusted R-squared:  0.3193 
F-statistic: 6.718 on 52 and 582 DF,  p-value: < 0.00000000000000022
```

```r
vif(lm1)
```

```
                         GVIF Df GVIF^(1/(2*Df))
extortion_type       6.612419  5        1.207914
request              1.553550  3        1.076186
month                2.658082 12        1.041575
time                 1.599094  4        1.060435
n_offenders          5.531435  6        1.153196
rel_offenders        3.076566  5        1.118940
had_weapon           3.644503  2        1.381687
with_violence        1.882216  2        1.171298
reported             1.096697  1        1.047233
rep_extortion_victim 1.133165  1        1.064502
bribe_victim         1.134400  1        1.065082
size                 1.410324  3        1.058977
hotrestbar           1.095670  1        1.046742
yearsquant           1.512677  4        1.053097
loghoms              2.644405  1        1.626163
logpop               2.718500  1        1.648787
```

```r
## RMSE for the fitted values

rmses <- data.frame(model=NA)

m1_residuals <- residuals(m1, type="response")

rmses$model[1] <- "m1"
rmses$RMSE <- sqrt(mean(m1_residuals^2))
rmses$NRMSE <- sqrt(mean(m1_residuals^2))/sd(m1_residuals)
rmses$CVRMSE <- sqrt(mean(m1_residuals^2))/(max(m1_residuals)-
                                             min(m1_residuals))

rmses
```

```
  model      RMSE     NRMSE    CVRMSE
1    m1 0.2432529 0.9992123 0.1309278
```

```r
kable(rmses)
```



|model |      RMSE|     NRMSE|    CVRMSE|
|:-----|---------:|---------:|---------:|
|m1    | 0.2432529| 0.9992123| 0.1309278|

```r
# Plot observed vs fitted
m1_ob_pred <- data.frame(Observed=m1df$complied_bin,
                        Predicted=fitted(m1, type=response))

ggplot(m1_ob_pred, aes(Observed, Predicted)) +
 geom_boxplot() +
 theme_bw() +
 ggtitle("Compliance with extortion demands:\nObserved vs. predicted")
```

![plot of chunk models-with-all](figure/models-with-all-1.pdf)

```r
## Stepwise selection of variables

sm1 <- step(m1)
```

```
Start:  AIC=371.56
complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                       Df Deviance    AIC
- rel_offenders         5   270.05 366.05
- request               3   269.01 369.01
- had_weapon            2   267.06 369.06
- time                  4   271.15 369.15
- bribe_victim          1   265.61 369.60
- loghoms               1   266.68 370.68
- reported              1   267.40 371.40
<none>                      265.56 371.56
- month                12   289.78 371.78
- logpop                1   267.81 371.81
- hotrestbar            1   267.93 371.93
- with_violence         2   270.04 372.04
- rep_extortion_victim  1   268.05 372.05
- n_offenders           6   278.91 372.91
- size                  3   273.50 373.50
- yearsquant            4   280.65 378.65
- extortion_type        5   296.95 392.95

Step:  AIC=366.05
complied_bin ~ extortion_type + request + month + time + n_offenders + 
    had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop

                       Df Deviance    AIC
- had_weapon            2   270.94 362.94
- time                  4   275.61 363.61
- bribe_victim          1   270.09 364.09
- request               3   274.13 364.13
- month                12   292.71 364.71
- loghoms               1   271.07 365.07
- reported              1   271.43 365.43
<none>                      270.05 366.05
- with_violence         2   274.13 366.13
- logpop                1   272.16 366.16
- hotrestbar            1   272.50 366.50
- rep_extortion_victim  1   272.62 366.62
- size                  3   277.77 367.77
- n_offenders           6   283.91 367.91
- yearsquant            4   285.39 373.39
- extortion_type        5   307.27 393.27

Step:  AIC=362.94
complied_bin ~ extortion_type + request + month + time + n_offenders + 
    with_violence + reported + rep_extortion_victim + bribe_victim + 
    size + hotrestbar + yearsquant + loghoms + logpop

                       Df Deviance    AIC
- request               3   274.44 360.44
- time                  4   276.67 360.67
- bribe_victim          1   271.00 361.00
- loghoms               1   271.95 361.95
- month                12   294.00 362.00
- reported              1   272.19 362.19
<none>                      270.94 362.94
- logpop                1   273.05 363.05
- hotrestbar            1   273.64 363.64
- rep_extortion_victim  1   273.67 363.67
- with_violence         2   276.02 364.02
- size                  3   278.87 364.87
- n_offenders           6   288.95 368.95
- yearsquant            4   286.40 370.40
- extortion_type        5   323.61 405.61

Step:  AIC=360.44
complied_bin ~ extortion_type + month + time + n_offenders + 
    with_violence + reported + rep_extortion_victim + bribe_victim + 
    size + hotrestbar + yearsquant + loghoms + logpop

                       Df Deviance    AIC
- time                  4   280.24 358.24
- bribe_victim          1   274.51 358.51
- month                12   296.81 358.81
- loghoms               1   275.29 359.29
- reported              1   275.62 359.62
- logpop                1   276.20 360.20
<none>                      274.44 360.44
- with_violence         2   278.51 360.51
- hotrestbar            1   276.98 360.98
- rep_extortion_victim  1   277.25 361.25
- size                  3   281.79 361.79
- yearsquant            4   289.19 367.19
- n_offenders           6   294.10 368.10
- extortion_type        5   327.12 403.12

Step:  AIC=358.24
complied_bin ~ extortion_type + month + n_offenders + with_violence + 
    reported + rep_extortion_victim + bribe_victim + size + hotrestbar + 
    yearsquant + loghoms + logpop

                       Df Deviance    AIC
- month                12   301.04 355.04
- bribe_victim          1   280.35 356.35
- loghoms               1   281.04 357.04
- reported              1   281.47 357.47
- logpop                1   281.71 357.71
- with_violence         2   284.01 358.01
<none>                      280.24 358.24
- size                  3   286.41 358.41
- hotrestbar            1   283.19 359.19
- rep_extortion_victim  1   283.22 359.22
- yearsquant            4   294.18 364.18
- n_offenders           6   301.54 367.54
- extortion_type        5   331.92 399.92

Step:  AIC=355.04
complied_bin ~ extortion_type + n_offenders + with_violence + 
    reported + rep_extortion_victim + bribe_victim + size + hotrestbar + 
    yearsquant + loghoms + logpop

                       Df Deviance    AIC
- loghoms               1   301.21 353.21
- bribe_victim          1   301.23 353.23
- reported              1   302.00 354.00
- logpop                1   302.00 354.00
- with_violence         2   304.16 354.16
<none>                      301.04 355.04
- size                  3   307.50 355.50
- hotrestbar            1   304.51 356.51
- rep_extortion_victim  1   304.65 356.65
- yearsquant            4   312.86 358.86
- n_offenders           6   317.62 359.62
- extortion_type        5   352.41 396.41

Step:  AIC=353.21
complied_bin ~ extortion_type + n_offenders + with_violence + 
    reported + rep_extortion_victim + bribe_victim + size + hotrestbar + 
    yearsquant + logpop

                       Df Deviance    AIC
- bribe_victim          1   301.41 351.41
- reported              1   302.19 352.19
- logpop                1   302.29 352.29
- with_violence         2   304.50 352.50
<none>                      301.21 353.21
- size                  3   307.76 353.76
- hotrestbar            1   304.60 354.60
- rep_extortion_victim  1   304.82 354.82
- yearsquant            4   313.33 357.33
- n_offenders           6   317.62 357.62
- extortion_type        5   352.47 394.47

Step:  AIC=351.41
complied_bin ~ extortion_type + n_offenders + with_violence + 
    reported + rep_extortion_victim + size + hotrestbar + yearsquant + 
    logpop

                       Df Deviance    AIC
- reported              1   302.36 350.36
- logpop                1   302.54 350.54
- with_violence         2   304.63 350.63
<none>                      301.41 351.41
- size                  3   308.06 352.06
- hotrestbar            1   304.79 352.79
- rep_extortion_victim  1   304.94 352.94
- yearsquant            4   313.49 355.49
- n_offenders           6   317.99 355.99
- extortion_type        5   352.47 392.47

Step:  AIC=350.36
complied_bin ~ extortion_type + n_offenders + with_violence + 
    rep_extortion_victim + size + hotrestbar + yearsquant + logpop

                       Df Deviance    AIC
- logpop                1   303.58 349.58
- with_violence         2   305.80 349.80
<none>                      302.36 350.36
- size                  3   308.67 350.67
- rep_extortion_victim  1   305.71 351.71
- hotrestbar            1   305.84 351.84
- yearsquant            4   314.20 354.20
- n_offenders           6   320.58 356.58
- extortion_type        5   353.78 391.78

Step:  AIC=349.58
complied_bin ~ extortion_type + n_offenders + with_violence + 
    rep_extortion_victim + size + hotrestbar + yearsquant

                       Df Deviance    AIC
<none>                      303.58 349.58
- with_violence         2   307.81 349.81
- size                  3   310.13 350.13
- rep_extortion_victim  1   306.78 350.78
- hotrestbar            1   306.83 350.83
- yearsquant            4   315.52 353.52
- n_offenders           6   321.22 355.22
- extortion_type        5   357.55 393.55
```

```r
summary(sm1)
```

```

Call:
glm(formula = complied_bin ~ extortion_type + n_offenders + with_violence + 
    rep_extortion_victim + size + hotrestbar + yearsquant, family = "binomial", 
    data = m1df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.4717  -0.3783  -0.2788  -0.1873   2.8467  

Coefficients:
                            Estimate Std. Error z value       Pr(>|z|)    
(Intercept)                  -3.2281     0.5041  -6.404 0.000000000151 ***
extortion_typeInternet        2.6551     1.4956   1.775        0.07586 .  
extortion_typeStreet          1.6855     0.5191   3.247        0.00117 ** 
extortion_typePremises       19.5989   960.5940   0.020        0.98372    
extortion_typeCobro de piso   2.6817     0.9121   2.940        0.00328 ** 
extortion_typeOther           2.2862     0.5795   3.945 0.000079733184 ***
n_offenders2                  0.7938     0.3713   2.138        0.03255 *  
n_offenders3                  0.5266     0.6276   0.839        0.40143    
n_offenders4                  3.2246     1.0427   3.093        0.00198 ** 
n_offenders5                  3.5612     1.5250   2.335        0.01953 *  
n_offenders6+                 0.4580     0.9448   0.485        0.62786    
n_offendersDK/DA             -0.1363     0.7130  -0.191        0.84835    
with_violenceYes              1.1198     0.7964   1.406        0.15970    
with_violenceDK/DA            0.9751     0.6055   1.610        0.10732    
rep_extortion_victim1         1.0622     0.5547   1.915        0.05551 .  
sizeMedium                   -0.3112     0.4506  -0.691        0.48981    
sizeSmall                     0.4847     0.4245   1.142        0.25356    
sizeMicro                    -0.6538     0.4945  -1.322        0.18612    
hotrestbar1                  -0.9655     0.5832  -1.656        0.09779 .  
yearsquant(8,16]             -0.3306     0.5421  -0.610        0.54200    
yearsquant(16,25]             0.6076     0.4745   1.281        0.20037    
yearsquant(25,34]            -0.4951     0.5764  -0.859        0.39035    
yearsquant(34,43]             1.0774     0.4823   2.234        0.02549 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 461.19  on 634  degrees of freedom
Residual deviance: 303.58  on 612  degrees of freedom
AIC: 349.58

Number of Fisher Scoring iterations: 16
```

```r
print(xtable(sm1), type="html")
```

```
<!-- html table generated in R 3.3.0 by xtable 1.8-2 package -->
<!-- Tue Oct 18 17:16:17 2016 -->
<table border=1>
<tr> <th>  </th> <th> Estimate </th> <th> Std. Error </th> <th> z value </th> <th> Pr(&gt;|z|) </th>  </tr>
  <tr> <td align="right"> (Intercept) </td> <td align="right"> -3.2281 </td> <td align="right"> 0.5041 </td> <td align="right"> -6.40 </td> <td align="right"> 0.0000 </td> </tr>
  <tr> <td align="right"> extortion_typeInternet </td> <td align="right"> 2.6551 </td> <td align="right"> 1.4956 </td> <td align="right"> 1.78 </td> <td align="right"> 0.0759 </td> </tr>
  <tr> <td align="right"> extortion_typeStreet </td> <td align="right"> 1.6855 </td> <td align="right"> 0.5191 </td> <td align="right"> 3.25 </td> <td align="right"> 0.0012 </td> </tr>
  <tr> <td align="right"> extortion_typePremises </td> <td align="right"> 19.5989 </td> <td align="right"> 960.5940 </td> <td align="right"> 0.02 </td> <td align="right"> 0.9837 </td> </tr>
  <tr> <td align="right"> extortion_typeCobro de piso </td> <td align="right"> 2.6817 </td> <td align="right"> 0.9121 </td> <td align="right"> 2.94 </td> <td align="right"> 0.0033 </td> </tr>
  <tr> <td align="right"> extortion_typeOther </td> <td align="right"> 2.2862 </td> <td align="right"> 0.5795 </td> <td align="right"> 3.95 </td> <td align="right"> 0.0001 </td> </tr>
  <tr> <td align="right"> n_offenders2 </td> <td align="right"> 0.7938 </td> <td align="right"> 0.3713 </td> <td align="right"> 2.14 </td> <td align="right"> 0.0326 </td> </tr>
  <tr> <td align="right"> n_offenders3 </td> <td align="right"> 0.5266 </td> <td align="right"> 0.6276 </td> <td align="right"> 0.84 </td> <td align="right"> 0.4014 </td> </tr>
  <tr> <td align="right"> n_offenders4 </td> <td align="right"> 3.2246 </td> <td align="right"> 1.0427 </td> <td align="right"> 3.09 </td> <td align="right"> 0.0020 </td> </tr>
  <tr> <td align="right"> n_offenders5 </td> <td align="right"> 3.5612 </td> <td align="right"> 1.5250 </td> <td align="right"> 2.34 </td> <td align="right"> 0.0195 </td> </tr>
  <tr> <td align="right"> n_offenders6+ </td> <td align="right"> 0.4580 </td> <td align="right"> 0.9448 </td> <td align="right"> 0.48 </td> <td align="right"> 0.6279 </td> </tr>
  <tr> <td align="right"> n_offendersDK/DA </td> <td align="right"> -0.1363 </td> <td align="right"> 0.7130 </td> <td align="right"> -0.19 </td> <td align="right"> 0.8484 </td> </tr>
  <tr> <td align="right"> with_violenceYes </td> <td align="right"> 1.1198 </td> <td align="right"> 0.7964 </td> <td align="right"> 1.41 </td> <td align="right"> 0.1597 </td> </tr>
  <tr> <td align="right"> with_violenceDK/DA </td> <td align="right"> 0.9751 </td> <td align="right"> 0.6055 </td> <td align="right"> 1.61 </td> <td align="right"> 0.1073 </td> </tr>
  <tr> <td align="right"> rep_extortion_victim1 </td> <td align="right"> 1.0622 </td> <td align="right"> 0.5547 </td> <td align="right"> 1.91 </td> <td align="right"> 0.0555 </td> </tr>
  <tr> <td align="right"> sizeMedium </td> <td align="right"> -0.3112 </td> <td align="right"> 0.4506 </td> <td align="right"> -0.69 </td> <td align="right"> 0.4898 </td> </tr>
  <tr> <td align="right"> sizeSmall </td> <td align="right"> 0.4847 </td> <td align="right"> 0.4245 </td> <td align="right"> 1.14 </td> <td align="right"> 0.2536 </td> </tr>
  <tr> <td align="right"> sizeMicro </td> <td align="right"> -0.6538 </td> <td align="right"> 0.4945 </td> <td align="right"> -1.32 </td> <td align="right"> 0.1861 </td> </tr>
  <tr> <td align="right"> hotrestbar1 </td> <td align="right"> -0.9655 </td> <td align="right"> 0.5832 </td> <td align="right"> -1.66 </td> <td align="right"> 0.0978 </td> </tr>
  <tr> <td align="right"> yearsquant(8,16] </td> <td align="right"> -0.3306 </td> <td align="right"> 0.5421 </td> <td align="right"> -0.61 </td> <td align="right"> 0.5420 </td> </tr>
  <tr> <td align="right"> yearsquant(16,25] </td> <td align="right"> 0.6076 </td> <td align="right"> 0.4745 </td> <td align="right"> 1.28 </td> <td align="right"> 0.2004 </td> </tr>
  <tr> <td align="right"> yearsquant(25,34] </td> <td align="right"> -0.4951 </td> <td align="right"> 0.5764 </td> <td align="right"> -0.86 </td> <td align="right"> 0.3903 </td> </tr>
  <tr> <td align="right"> yearsquant(34,43] </td> <td align="right"> 1.0774 </td> <td align="right"> 0.4823 </td> <td align="right"> 2.23 </td> <td align="right"> 0.0255 </td> </tr>
   </table>
```

```r
htmlreg(sm1)
```

```

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
<caption align="bottom" style="margin-top:0.3em;">Statistical models</caption>
<tr>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b></b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 1</b></th>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">(Intercept)</td>
<td style="padding-right: 12px; border: none;">-3.23<sup style="vertical-align: 0px;">***</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.50)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeInternet</td>
<td style="padding-right: 12px; border: none;">2.66</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.50)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeStreet</td>
<td style="padding-right: 12px; border: none;">1.69<sup style="vertical-align: 0px;">**</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.52)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typePremises</td>
<td style="padding-right: 12px; border: none;">19.60</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(960.59)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeCobro de piso</td>
<td style="padding-right: 12px; border: none;">2.68<sup style="vertical-align: 0px;">**</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.91)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeOther</td>
<td style="padding-right: 12px; border: none;">2.29<sup style="vertical-align: 0px;">***</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.58)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders2</td>
<td style="padding-right: 12px; border: none;">0.79<sup style="vertical-align: 0px;">*</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.37)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders3</td>
<td style="padding-right: 12px; border: none;">0.53</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.63)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders4</td>
<td style="padding-right: 12px; border: none;">3.22<sup style="vertical-align: 0px;">**</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.04)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders5</td>
<td style="padding-right: 12px; border: none;">3.56<sup style="vertical-align: 0px;">*</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.52)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders6+</td>
<td style="padding-right: 12px; border: none;">0.46</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.94)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offendersDK/DA</td>
<td style="padding-right: 12px; border: none;">-0.14</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.71)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">with_violenceYes</td>
<td style="padding-right: 12px; border: none;">1.12</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.80)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">with_violenceDK/DA</td>
<td style="padding-right: 12px; border: none;">0.98</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.61)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rep_extortion_victim1</td>
<td style="padding-right: 12px; border: none;">1.06</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.55)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeMedium</td>
<td style="padding-right: 12px; border: none;">-0.31</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.45)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeSmall</td>
<td style="padding-right: 12px; border: none;">0.48</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.42)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeMicro</td>
<td style="padding-right: 12px; border: none;">-0.65</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.49)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">hotrestbar1</td>
<td style="padding-right: 12px; border: none;">-0.97</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.58)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(8,16]</td>
<td style="padding-right: 12px; border: none;">-0.33</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.54)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(16,25]</td>
<td style="padding-right: 12px; border: none;">0.61</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.47)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(25,34]</td>
<td style="padding-right: 12px; border: none;">-0.50</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.58)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(34,43]</td>
<td style="padding-right: 12px; border: none;">1.08<sup style="vertical-align: 0px;">*</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.48)</td>
</tr>
<tr>
<td style="border-top: 1px solid black;">AIC</td>
<td style="border-top: 1px solid black;">349.58</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">BIC</td>
<td style="padding-right: 12px; border: none;">452.01</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Log Likelihood</td>
<td style="padding-right: 12px; border: none;">-151.79</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Deviance</td>
<td style="padding-right: 12px; border: none;">303.58</td>
</tr>
<tr>
<td style="border-bottom: 2px solid black;">Num. obs.</td>
<td style="border-bottom: 2px solid black;">635</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;" colspan="3"><span style="font-size:0.8em"><sup style="vertical-align: 0px;">***</sup>p &lt; 0.001, <sup style="vertical-align: 0px;">**</sup>p &lt; 0.01, <sup style="vertical-align: 0px;">*</sup>p &lt; 0.05</span></td>
</tr>
</table>
```

```r
screenreg(sm1)
```

```

========================================
                             Model 1    
----------------------------------------
(Intercept)                    -3.23 ***
                               (0.50)   
extortion_typeInternet          2.66    
                               (1.50)   
extortion_typeStreet            1.69 ** 
                               (0.52)   
extortion_typePremises         19.60    
                             (960.59)   
extortion_typeCobro de piso     2.68 ** 
                               (0.91)   
extortion_typeOther             2.29 ***
                               (0.58)   
n_offenders2                    0.79 *  
                               (0.37)   
n_offenders3                    0.53    
                               (0.63)   
n_offenders4                    3.22 ** 
                               (1.04)   
n_offenders5                    3.56 *  
                               (1.52)   
n_offenders6+                   0.46    
                               (0.94)   
n_offendersDK/DA               -0.14    
                               (0.71)   
with_violenceYes                1.12    
                               (0.80)   
with_violenceDK/DA              0.98    
                               (0.61)   
rep_extortion_victim1           1.06    
                               (0.55)   
sizeMedium                     -0.31    
                               (0.45)   
sizeSmall                       0.48    
                               (0.42)   
sizeMicro                      -0.65    
                               (0.49)   
hotrestbar1                    -0.97    
                               (0.58)   
yearsquant(8,16]               -0.33    
                               (0.54)   
yearsquant(16,25]               0.61    
                               (0.47)   
yearsquant(25,34]              -0.50    
                               (0.58)   
yearsquant(34,43]               1.08 *  
                               (0.48)   
----------------------------------------
AIC                           349.58    
BIC                           452.01    
Log Likelihood               -151.79    
Deviance                      303.58    
Num. obs.                     635       
========================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
htmlreg(list(m1, sm1))
```

```

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
<caption align="bottom" style="margin-top:0.3em;">Statistical models</caption>
<tr>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b></b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 1</b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 2</b></th>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">(Intercept)</td>
<td style="padding-right: 12px; border: none;">-23.01</td>
<td style="padding-right: 12px; border: none;">-3.23<sup style="vertical-align: 0px;">***</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;">(0.50)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeInternet</td>
<td style="padding-right: 12px; border: none;">3.46</td>
<td style="padding-right: 12px; border: none;">2.66</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.83)</td>
<td style="padding-right: 12px; border: none;">(1.50)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeStreet</td>
<td style="padding-right: 12px; border: none;">1.75<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">1.69<sup style="vertical-align: 0px;">**</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.73)</td>
<td style="padding-right: 12px; border: none;">(0.52)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typePremises</td>
<td style="padding-right: 12px; border: none;">34.97</td>
<td style="padding-right: 12px; border: none;">19.60</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1829.78)</td>
<td style="padding-right: 12px; border: none;">(960.59)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeCobro de piso</td>
<td style="padding-right: 12px; border: none;">3.12<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">2.68<sup style="vertical-align: 0px;">**</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.15)</td>
<td style="padding-right: 12px; border: none;">(0.91)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeOther</td>
<td style="padding-right: 12px; border: none;">2.30<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">2.29<sup style="vertical-align: 0px;">***</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.80)</td>
<td style="padding-right: 12px; border: none;">(0.58)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestMoney</td>
<td style="padding-right: 12px; border: none;">19.74</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestNothing</td>
<td style="padding-right: 12px; border: none;">19.61</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestProduct</td>
<td style="padding-right: 12px; border: none;">20.09</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthFebruary</td>
<td style="padding-right: 12px; border: none;">-1.50</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.97)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthMarch</td>
<td style="padding-right: 12px; border: none;">-0.46</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.85)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthApril</td>
<td style="padding-right: 12px; border: none;">-0.42</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.94)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthMay</td>
<td style="padding-right: 12px; border: none;">-1.47</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.07)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthJune</td>
<td style="padding-right: 12px; border: none;">-0.30</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.78)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthJuly</td>
<td style="padding-right: 12px; border: none;">-0.52</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.94)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthAugust</td>
<td style="padding-right: 12px; border: none;">-2.17</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.17)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthSeptember</td>
<td style="padding-right: 12px; border: none;">-1.20</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.92)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthOctober</td>
<td style="padding-right: 12px; border: none;">-2.11<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.06)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthNovember</td>
<td style="padding-right: 12px; border: none;">0.69</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.75)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthDecember</td>
<td style="padding-right: 12px; border: none;">-1.75</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.99)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthNONE</td>
<td style="padding-right: 12px; border: none;">-1.49</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.37)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeAfternoon</td>
<td style="padding-right: 12px; border: none;">0.18</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.41)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeEvening</td>
<td style="padding-right: 12px; border: none;">-0.12</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeNight</td>
<td style="padding-right: 12px; border: none;">2.30<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.16)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeDK/DA</td>
<td style="padding-right: 12px; border: none;">2.78</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.79)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders2</td>
<td style="padding-right: 12px; border: none;">0.83<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">0.79<sup style="vertical-align: 0px;">*</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.42)</td>
<td style="padding-right: 12px; border: none;">(0.37)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders3</td>
<td style="padding-right: 12px; border: none;">0.79</td>
<td style="padding-right: 12px; border: none;">0.53</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.75)</td>
<td style="padding-right: 12px; border: none;">(0.63)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders4</td>
<td style="padding-right: 12px; border: none;">3.74<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">3.22<sup style="vertical-align: 0px;">**</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.39)</td>
<td style="padding-right: 12px; border: none;">(1.04)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders5</td>
<td style="padding-right: 12px; border: none;">3.67</td>
<td style="padding-right: 12px; border: none;">3.56<sup style="vertical-align: 0px;">*</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(2.02)</td>
<td style="padding-right: 12px; border: none;">(1.52)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders6+</td>
<td style="padding-right: 12px; border: none;">0.10</td>
<td style="padding-right: 12px; border: none;">0.46</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.44)</td>
<td style="padding-right: 12px; border: none;">(0.94)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offendersDK/DA</td>
<td style="padding-right: 12px; border: none;">-0.11</td>
<td style="padding-right: 12px; border: none;">-0.14</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.84)</td>
<td style="padding-right: 12px; border: none;">(0.71)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersEmployee</td>
<td style="padding-right: 12px; border: none;">-14.96</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1278.54)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersBarely known</td>
<td style="padding-right: 12px; border: none;">-0.04</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.93)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersSomewhat known</td>
<td style="padding-right: 12px; border: none;">-1.02</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.68)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersClose acquaintance</td>
<td style="padding-right: 12px; border: none;">1.53</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.74)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersDK/DA</td>
<td style="padding-right: 12px; border: none;">-1.53</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.17)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">had_weaponYes</td>
<td style="padding-right: 12px; border: none;">1.03</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.02)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">had_weaponDK/DA</td>
<td style="padding-right: 12px; border: none;">0.30</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.43)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">with_violenceYes</td>
<td style="padding-right: 12px; border: none;">1.34</td>
<td style="padding-right: 12px; border: none;">1.12</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.02)</td>
<td style="padding-right: 12px; border: none;">(0.80)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">with_violenceDK/DA</td>
<td style="padding-right: 12px; border: none;">1.23</td>
<td style="padding-right: 12px; border: none;">0.98</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.71)</td>
<td style="padding-right: 12px; border: none;">(0.61)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">reportedYes</td>
<td style="padding-right: 12px; border: none;">0.64</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.46)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rep_extortion_victim1</td>
<td style="padding-right: 12px; border: none;">1.08</td>
<td style="padding-right: 12px; border: none;">1.06</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.65)</td>
<td style="padding-right: 12px; border: none;">(0.55)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">bribe_victim1</td>
<td style="padding-right: 12px; border: none;">0.12</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.54)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeMedium</td>
<td style="padding-right: 12px; border: none;">-0.27</td>
<td style="padding-right: 12px; border: none;">-0.31</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.53)</td>
<td style="padding-right: 12px; border: none;">(0.45)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeSmall</td>
<td style="padding-right: 12px; border: none;">0.92</td>
<td style="padding-right: 12px; border: none;">0.48</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.50)</td>
<td style="padding-right: 12px; border: none;">(0.42)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeMicro</td>
<td style="padding-right: 12px; border: none;">-0.28</td>
<td style="padding-right: 12px; border: none;">-0.65</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
<td style="padding-right: 12px; border: none;">(0.49)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">hotrestbar1</td>
<td style="padding-right: 12px; border: none;">-0.94</td>
<td style="padding-right: 12px; border: none;">-0.97</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.66)</td>
<td style="padding-right: 12px; border: none;">(0.58)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(8,16]</td>
<td style="padding-right: 12px; border: none;">-0.35</td>
<td style="padding-right: 12px; border: none;">-0.33</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.59)</td>
<td style="padding-right: 12px; border: none;">(0.54)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(16,25]</td>
<td style="padding-right: 12px; border: none;">0.87</td>
<td style="padding-right: 12px; border: none;">0.61</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.53)</td>
<td style="padding-right: 12px; border: none;">(0.47)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(25,34]</td>
<td style="padding-right: 12px; border: none;">-0.80</td>
<td style="padding-right: 12px; border: none;">-0.50</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.68)</td>
<td style="padding-right: 12px; border: none;">(0.58)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(34,43]</td>
<td style="padding-right: 12px; border: none;">1.28<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">1.08<sup style="vertical-align: 0px;">*</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
<td style="padding-right: 12px; border: none;">(0.48)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">loghoms</td>
<td style="padding-right: 12px; border: none;">-0.28</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.26)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">logpop</td>
<td style="padding-right: 12px; border: none;">0.59</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.40)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="border-top: 1px solid black;">AIC</td>
<td style="border-top: 1px solid black;">371.56</td>
<td style="border-top: 1px solid black;">349.58</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">BIC</td>
<td style="padding-right: 12px; border: none;">607.60</td>
<td style="padding-right: 12px; border: none;">452.01</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Log Likelihood</td>
<td style="padding-right: 12px; border: none;">-132.78</td>
<td style="padding-right: 12px; border: none;">-151.79</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Deviance</td>
<td style="padding-right: 12px; border: none;">265.56</td>
<td style="padding-right: 12px; border: none;">303.58</td>
</tr>
<tr>
<td style="border-bottom: 2px solid black;">Num. obs.</td>
<td style="border-bottom: 2px solid black;">635</td>
<td style="border-bottom: 2px solid black;">635</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;" colspan="4"><span style="font-size:0.8em"><sup style="vertical-align: 0px;">***</sup>p &lt; 0.001, <sup style="vertical-align: 0px;">**</sup>p &lt; 0.01, <sup style="vertical-align: 0px;">*</sup>p &lt; 0.05</span></td>
</tr>
</table>
```

```r
screenreg(list(m1, sm1))
```

```

=========================================================
                                 Model 1      Model 2    
---------------------------------------------------------
(Intercept)                        -23.01       -3.23 ***
                                 (3420.48)      (0.50)   
extortion_typeInternet               3.46        2.66    
                                    (1.83)      (1.50)   
extortion_typeStreet                 1.75 *      1.69 ** 
                                    (0.73)      (0.52)   
extortion_typePremises              34.97       19.60    
                                 (1829.78)    (960.59)   
extortion_typeCobro de piso          3.12 **     2.68 ** 
                                    (1.15)      (0.91)   
extortion_typeOther                  2.30 **     2.29 ***
                                    (0.80)      (0.58)   
requestMoney                        19.74                
                                 (3420.48)               
requestNothing                      19.61                
                                 (3420.48)               
requestProduct                      20.09                
                                 (3420.48)               
monthFebruary                       -1.50                
                                    (0.97)               
monthMarch                          -0.46                
                                    (0.85)               
monthApril                          -0.42                
                                    (0.94)               
monthMay                            -1.47                
                                    (1.07)               
monthJune                           -0.30                
                                    (0.78)               
monthJuly                           -0.52                
                                    (0.94)               
monthAugust                         -2.17                
                                    (1.17)               
monthSeptember                      -1.20                
                                    (0.92)               
monthOctober                        -2.11 *              
                                    (1.06)               
monthNovember                        0.69                
                                    (0.75)               
monthDecember                       -1.75                
                                    (0.99)               
monthNONE                           -1.49                
                                    (1.37)               
timeAfternoon                        0.18                
                                    (0.41)               
timeEvening                         -0.12                
                                    (0.57)               
timeNight                            2.30 *              
                                    (1.16)               
timeDK/DA                            2.78                
                                    (1.79)               
n_offenders2                         0.83 *      0.79 *  
                                    (0.42)      (0.37)   
n_offenders3                         0.79        0.53    
                                    (0.75)      (0.63)   
n_offenders4                         3.74 **     3.22 ** 
                                    (1.39)      (1.04)   
n_offenders5                         3.67        3.56 *  
                                    (2.02)      (1.52)   
n_offenders6+                        0.10        0.46    
                                    (1.44)      (0.94)   
n_offendersDK/DA                    -0.11       -0.14    
                                    (0.84)      (0.71)   
rel_offendersEmployee              -14.96                
                                 (1278.54)               
rel_offendersBarely known           -0.04                
                                    (0.93)               
rel_offendersSomewhat known         -1.02                
                                    (1.68)               
rel_offendersClose acquaintance      1.53                
                                    (1.74)               
rel_offendersDK/DA                  -1.53                
                                    (1.17)               
had_weaponYes                        1.03                
                                    (1.02)               
had_weaponDK/DA                      0.30                
                                    (0.43)               
with_violenceYes                     1.34        1.12    
                                    (1.02)      (0.80)   
with_violenceDK/DA                   1.23        0.98    
                                    (0.71)      (0.61)   
reportedYes                          0.64                
                                    (0.46)               
rep_extortion_victim1                1.08        1.06    
                                    (0.65)      (0.55)   
bribe_victim1                        0.12                
                                    (0.54)               
sizeMedium                          -0.27       -0.31    
                                    (0.53)      (0.45)   
sizeSmall                            0.92        0.48    
                                    (0.50)      (0.42)   
sizeMicro                           -0.28       -0.65    
                                    (0.57)      (0.49)   
hotrestbar1                         -0.94       -0.97    
                                    (0.66)      (0.58)   
yearsquant(8,16]                    -0.35       -0.33    
                                    (0.59)      (0.54)   
yearsquant(16,25]                    0.87        0.61    
                                    (0.53)      (0.47)   
yearsquant(25,34]                   -0.80       -0.50    
                                    (0.68)      (0.58)   
yearsquant(34,43]                    1.28 *      1.08 *  
                                    (0.57)      (0.48)   
loghoms                             -0.28                
                                    (0.26)               
logpop                               0.59                
                                    (0.40)               
---------------------------------------------------------
AIC                                371.56      349.58    
BIC                                607.60      452.01    
Log Likelihood                    -132.78     -151.79    
Deviance                           265.56      303.58    
Num. obs.                          635         635       
=========================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
sm1$anova
```

```
              Step Df   Deviance Resid. Df Resid. Dev      AIC
1                  NA         NA       582   265.5579 371.5579
2  - rel_offenders  5  4.4935884       587   270.0515 366.0515
3     - had_weapon  2  0.8895257       589   270.9410 362.9410
4        - request  3  3.4971538       592   274.4382 360.4382
5           - time  4  5.7995164       596   280.2377 358.2377
6          - month 12 20.8007142       608   301.0384 355.0384
7        - loghoms  1  0.1715766       609   301.2100 353.2100
8   - bribe_victim  1  0.2021958       610   301.4122 351.4122
9       - reported  1  0.9434216       611   302.3556 350.3556
10        - logpop  1  1.2234432       612   303.5790 349.5790
```

```r
kable(sm1$anova)
```



|Step            | Df|   Deviance| Resid. Df| Resid. Dev|      AIC|
|:---------------|--:|----------:|---------:|----------:|--------:|
|                | NA|         NA|       582|   265.5579| 371.5579|
|- rel_offenders |  5|  4.4935884|       587|   270.0515| 366.0515|
|- had_weapon    |  2|  0.8895257|       589|   270.9410| 362.9410|
|- request       |  3|  3.4971538|       592|   274.4382| 360.4382|
|- time          |  4|  5.7995164|       596|   280.2377| 358.2377|
|- month         | 12| 20.8007142|       608|   301.0384| 355.0384|
|- loghoms       |  1|  0.1715766|       609|   301.2100| 353.2100|
|- bribe_victim  |  1|  0.2021958|       610|   301.4122| 351.4122|
|- reported      |  1|  0.9434216|       611|   302.3556| 350.3556|
|- logpop        |  1|  1.2234432|       612|   303.5790| 349.5790|

```r
## Using drop1 to test single variable exclusions

drop1(m1, test="LRT")
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Single term deletions

Model:
complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
                     Df Deviance    AIC     LRT    Pr(>Chi)    
<none>                    265.56 371.56                        
extortion_type        5   296.95 392.95 31.3964 0.000007822 ***
request               3   269.01 369.01  3.4541    0.326770    
month                12   289.78 371.78 24.2261    0.018947 *  
time                  4   271.15 369.15  5.5892    0.232003    
n_offenders           6   278.91 372.91 13.3483    0.037826 *  
rel_offenders         5   270.05 366.05  4.4936    0.480742    
had_weapon            2   267.06 369.06  1.5047    0.471267    
with_violence         2   270.04 372.04  4.4843    0.106231    
reported              1   267.40 371.40  1.8398    0.174972    
rep_extortion_victim  1   268.05 372.05  2.4920    0.114425    
bribe_victim          1   265.61 369.60  0.0469    0.828585    
size                  3   273.50 373.50  7.9463    0.047135 *  
hotrestbar            1   267.93 371.93  2.3703    0.123666    
yearsquant            4   280.65 378.65 15.0925    0.004513 ** 
loghoms               1   266.68 370.68  1.1249    0.288860    
logpop                1   267.81 371.81  2.2472    0.133860    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(drop1(m1, test="LRT"))
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```



|                     | Df| Deviance|      AIC|        LRT|  Pr(>Chi)|
|:--------------------|--:|--------:|--------:|----------:|---------:|
|<none>               | NA| 265.5579| 371.5579|         NA|        NA|
|extortion_type       |  5| 296.9543| 392.9543| 31.3963715| 0.0000078|
|request              |  3| 269.0120| 369.0120|  3.4540537| 0.3267703|
|month                | 12| 289.7840| 371.7840| 24.2261333| 0.0189471|
|time                 |  4| 271.1471| 369.1471|  5.5891520| 0.2320034|
|n_offenders          |  6| 278.9062| 372.9062| 13.3483109| 0.0378264|
|rel_offenders        |  5| 270.0515| 366.0515|  4.4935884| 0.4807418|
|had_weapon           |  2| 267.0626| 369.0626|  1.5046599| 0.4712672|
|with_violence        |  2| 270.0422| 372.0422|  4.4842746| 0.1062312|
|reported             |  1| 267.3977| 371.3977|  1.8398206| 0.1749720|
|rep_extortion_victim |  1| 268.0499| 372.0499|  2.4920233| 0.1144245|
|bribe_victim         |  1| 265.6048| 369.6048|  0.0468793| 0.8285854|
|size                 |  3| 273.5042| 373.5042|  7.9463033| 0.0471346|
|hotrestbar           |  1| 267.9282| 371.9282|  2.3702624| 0.1236663|
|yearsquant           |  4| 280.6504| 378.6504| 15.0925164| 0.0045131|
|loghoms              |  1| 266.6828| 370.6828|  1.1249252| 0.2888604|
|logpop               |  1| 267.8051| 371.8051|  2.2471580| 0.1338600|

```r
## Exclude state then business level variables

m1_nostate <- glm(complied_bin ~
                    extortion_type + request +
                    month + time +
                    n_offenders + rel_offenders +
                    had_weapon +
                    with_violence +
                    reported +
                    rep_extortion_victim +
                    bribe_victim +
                    size + hotrestbar + yearsquant,
                  data=m1df,
                  family = "binomial")
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```r
summary(m1_nostate)
```

```

Call:
glm(formula = complied_bin ~ extortion_type + request + month + 
    time + n_offenders + rel_offenders + had_weapon + with_violence + 
    reported + rep_extortion_victim + bribe_victim + size + hotrestbar + 
    yearsquant, family = "binomial", data = m1df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.0385  -0.3663  -0.2156  -0.1169   3.0279  

Coefficients:
                                  Estimate Std. Error z value Pr(>|z|)   
(Intercept)                      -22.70863 3442.23722  -0.007  0.99474   
extortion_typeInternet             3.35332    1.81806   1.844  0.06512 . 
extortion_typeStreet               1.63948    0.71570   2.291  0.02198 * 
extortion_typePremises            35.39134 1805.96897   0.020  0.98436   
extortion_typeCobro de piso        2.88885    1.12710   2.563  0.01037 * 
extortion_typeOther                2.45734    0.78858   3.116  0.00183 **
requestMoney                      19.42409 3442.23704   0.006  0.99550   
requestNothing                    19.34282 3442.23709   0.006  0.99552   
requestProduct                    19.69729 3442.23719   0.006  0.99543   
monthFebruary                     -1.40997    0.95928  -1.470  0.14161   
monthMarch                        -0.47508    0.84358  -0.563  0.57332   
monthApril                        -0.45270    0.93225  -0.486  0.62725   
monthMay                          -1.32255    1.05832  -1.250  0.21142   
monthJune                         -0.34404    0.76645  -0.449  0.65352   
monthJuly                         -0.69561    0.92570  -0.751  0.45239   
monthAugust                       -2.15894    1.11867  -1.930  0.05362 . 
monthSeptember                    -1.20758    0.90988  -1.327  0.18445   
monthOctober                      -2.08741    1.02690  -2.033  0.04208 * 
monthNovember                      0.64491    0.73880   0.873  0.38271   
monthDecember                     -1.79480    0.97559  -1.840  0.06581 . 
monthNONE                         -1.32447    1.33962  -0.989  0.32282   
timeAfternoon                      0.22546    0.40973   0.550  0.58213   
timeEvening                       -0.10585    0.56187  -0.188  0.85058   
timeNight                          2.14939    1.16364   1.847  0.06473 . 
timeDK/DA                          2.68064    1.72984   1.550  0.12123   
n_offenders2                       0.80373    0.41918   1.917  0.05519 . 
n_offenders3                       0.79095    0.74435   1.063  0.28796   
n_offenders4                       3.37355    1.34616   2.506  0.01221 * 
n_offenders5                       3.92834    2.04145   1.924  0.05432 . 
n_offenders6+                      0.02000    1.41196   0.014  0.98870   
n_offendersDK/DA                  -0.06808    0.83154  -0.082  0.93474   
rel_offendersEmployee            -15.26943 1263.91874  -0.012  0.99036   
rel_offendersBarely known         -0.12087    0.91422  -0.132  0.89482   
rel_offendersSomewhat known       -0.92768    1.66072  -0.559  0.57644   
rel_offendersClose acquaintance    1.74750    1.74343   1.002  0.31618   
rel_offendersDK/DA                -1.32992    1.14509  -1.161  0.24547   
had_weaponYes                      1.00479    0.99018   1.015  0.31022   
had_weaponDK/DA                    0.25442    0.42419   0.600  0.54864   
with_violenceYes                   1.60290    1.00308   1.598  0.11005   
with_violenceDK/DA                 1.23827    0.70318   1.761  0.07824 . 
reportedYes                        0.65375    0.45694   1.431  0.15251   
rep_extortion_victim1              1.05451    0.64480   1.635  0.10196   
bribe_victim1                      0.11888    0.54491   0.218  0.82730   
sizeMedium                        -0.27280    0.52977  -0.515  0.60660   
sizeSmall                          0.96372    0.49762   1.937  0.05279 . 
sizeMicro                         -0.25927    0.56652  -0.458  0.64720   
hotrestbar1                       -0.84239    0.64593  -1.304  0.19218   
yearsquant(8,16]                  -0.35947    0.59933  -0.600  0.54865   
yearsquant(16,25]                  0.91014    0.52585   1.731  0.08349 . 
yearsquant(25,34]                 -0.67242    0.66945  -1.004  0.31517   
yearsquant(34,43]                  1.30063    0.56576   2.299  0.02151 * 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 461.19  on 634  degrees of freedom
Residual deviance: 267.82  on 584  degrees of freedom
AIC: 369.82

Number of Fisher Scoring iterations: 17
```

```r
lrtest(m1_nostate, m1)
```

```
Likelihood ratio test

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
  #Df  LogLik Df  Chisq Pr(>Chisq)
1  51 -133.91                     
2  53 -132.78  2 2.2643     0.3223
```

```r
kable(lrtest(m1_nostate, m1))
```



| #Df|    LogLik| Df|    Chisq| Pr(>Chisq)|
|---:|---------:|--:|--------:|----------:|
|  51| -133.9111| NA|       NA|         NA|
|  53| -132.7790|  2| 2.264313|  0.3223374|

```r
waldtest(m1_nostate, m1)
```

```
Wald test

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
  Res.Df Df      F Pr(>F)
1    584                 
2    582  2 1.1026 0.3327
```

```r
kable(waldtest(m1_nostate, m1))
```



| Res.Df| Df|        F|    Pr(>F)|
|------:|--:|--------:|---------:|
|    584| NA|       NA|        NA|
|    582|  2| 1.102606| 0.3326973|

```r
waldtest(m1_nostate, m1, test="Chisq")
```

```
Wald test

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
  Res.Df Df  Chisq Pr(>Chisq)
1    584                     
2    582  2 2.2052      0.332
```

```r
kable(waldtest(m1_nostate, m1, test="Chisq"))
```



| Res.Df| Df|    Chisq| Pr(>Chisq)|
|------:|--:|--------:|----------:|
|    584| NA|       NA|         NA|
|    582|  2| 2.205212|  0.3320048|

```r
m1_nostate_nobus <- glm(complied_bin ~
                    extortion_type + request +
                    month + time +
                    n_offenders + rel_offenders +
                    had_weapon +
                    with_violence +
                    reported,
                  data=m1df,
                  family = "binomial")
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```r
summary(m1_nostate_nobus)
```

```

Call:
glm(formula = complied_bin ~ extortion_type + request + month + 
    time + n_offenders + rel_offenders + had_weapon + with_violence + 
    reported, family = "binomial", data = m1df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.6722  -0.4071  -0.2584  -0.1681   3.0446  

Coefficients:
                                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)                      -20.88636 3757.10536  -0.006 0.995564    
extortion_typeInternet             2.28606    1.59336   1.435 0.151361    
extortion_typeStreet               1.66241    0.63795   2.606 0.009165 ** 
extortion_typePremises            34.47695 1886.39252   0.018 0.985418    
extortion_typeCobro de piso        3.11849    0.93829   3.324 0.000889 ***
extortion_typeOther                2.49343    0.77035   3.237 0.001209 ** 
requestMoney                      17.99783 3757.10530   0.005 0.996178    
requestNothing                    18.08902 3757.10534   0.005 0.996159    
requestProduct                    18.56714 3757.10541   0.005 0.996057    
monthFebruary                     -0.95788    0.87590  -1.094 0.274130    
monthMarch                        -0.16259    0.77120  -0.211 0.833027    
monthApril                        -0.50449    0.90800  -0.556 0.578484    
monthMay                          -0.91582    0.96172  -0.952 0.340955    
monthJune                          0.26183    0.69851   0.375 0.707776    
monthJuly                         -0.52994    0.87905  -0.603 0.546603    
monthAugust                       -1.63569    1.03946  -1.574 0.115580    
monthSeptember                    -0.81504    0.85113  -0.958 0.338264    
monthOctober                      -1.54155    0.96480  -1.598 0.110088    
monthNovember                      0.56874    0.69634   0.817 0.414064    
monthDecember                     -1.69011    0.97705  -1.730 0.083665 .  
monthNONE                         -0.45452    1.22398  -0.371 0.710377    
timeAfternoon                     -0.04655    0.38634  -0.120 0.904091    
timeEvening                        0.04312    0.50509   0.085 0.931967    
timeNight                          1.51576    1.11133   1.364 0.172596    
timeDK/DA                          2.84647    1.63216   1.744 0.081161 .  
n_offenders2                       0.77496    0.39635   1.955 0.050557 .  
n_offenders3                       0.58696    0.68888   0.852 0.394182    
n_offenders4                       2.90843    1.17176   2.482 0.013061 *  
n_offenders5                       3.42400    1.68121   2.037 0.041687 *  
n_offenders6+                     -0.78896    1.29119  -0.611 0.541176    
n_offendersDK/DA                   0.11297    0.77848   0.145 0.884622    
rel_offendersEmployee            -16.05122 1316.17517  -0.012 0.990270    
rel_offendersBarely known         -0.34981    0.79576  -0.440 0.660234    
rel_offendersSomewhat known       -1.26141    1.57536  -0.801 0.423297    
rel_offendersClose acquaintance    0.73305    1.50479   0.487 0.626154    
rel_offendersDK/DA                -1.24144    1.08440  -1.145 0.252285    
had_weaponYes                      1.04323    0.88720   1.176 0.239647    
had_weaponDK/DA                    0.22447    0.40009   0.561 0.574771    
with_violenceYes                   1.17405    0.90416   1.298 0.194120    
with_violenceDK/DA                 1.10588    0.65563   1.687 0.091652 .  
reportedYes                        0.46280    0.43003   1.076 0.281837    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 461.19  on 634  degrees of freedom
Residual deviance: 294.67  on 594  degrees of freedom
AIC: 376.67

Number of Fisher Scoring iterations: 17
```

```r
lrtest(m1_nostate_nobus, m1_nostate)
```

```
Likelihood ratio test

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant
  #Df  LogLik Df  Chisq Pr(>Chisq)   
1  41 -147.34                        
2  51 -133.91 10 26.849   0.002752 **
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(lrtest(m1_nostate_nobus, m1_nostate))
```



| #Df|    LogLik| Df|    Chisq| Pr(>Chisq)|
|---:|---------:|--:|--------:|----------:|
|  41| -147.3354| NA|       NA|         NA|
|  51| -133.9111| 10| 26.84852|  0.0027519|

```r
waldtest(m1_nostate_nobus, m1_nostate)
```

```
Wald test

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant
  Res.Df Df      F  Pr(>F)  
1    594                    
2    584 10 2.3461 0.01018 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(waldtest(m1_nostate_nobus, m1_nostate))
```



| Res.Df| Df|        F|    Pr(>F)|
|------:|--:|--------:|---------:|
|    594| NA|       NA|        NA|
|    584| 10| 2.346057| 0.0101769|

```r
waldtest(m1_nostate_nobus, m1_nostate, test="Chisq")
```

```
Wald test

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant
  Res.Df Df  Chisq Pr(>Chisq)   
1    594                        
2    584 10 23.461   0.009168 **
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(waldtest(m1_nostate_nobus, m1_nostate, test="Chisq"))
```



| Res.Df| Df|    Chisq| Pr(>Chisq)|
|------:|--:|--------:|----------:|
|    594| NA|       NA|         NA|
|    584| 10| 23.46057|  0.0091684|

```r
lrtest(m1_nostate_nobus, m1_nostate, m1)
```

```
Likelihood ratio test

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant
Model 3: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
  #Df  LogLik Df   Chisq Pr(>Chisq)   
1  41 -147.34                         
2  51 -133.91 10 26.8485   0.002752 **
3  53 -132.78  2  2.2643   0.322337   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(lrtest(m1_nostate_nobus, m1_nostate, m1))
```



| #Df|    LogLik| Df|     Chisq| Pr(>Chisq)|
|---:|---------:|--:|---------:|----------:|
|  41| -147.3354| NA|        NA|         NA|
|  51| -133.9111| 10| 26.848516|  0.0027519|
|  53| -132.7790|  2|  2.264313|  0.3223374|

```r
waldtest(m1_nostate_nobus, m1_nostate, m1)
```

```
Wald test

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant
Model 3: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
  Res.Df Df      F  Pr(>F)  
1    594                    
2    584 10 2.3461 0.01018 *
3    582  2 1.1026 0.33270  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(waldtest(m1_nostate_nobus, m1_nostate, m1))
```



| Res.Df| Df|        F|    Pr(>F)|
|------:|--:|--------:|---------:|
|    594| NA|       NA|        NA|
|    584| 10| 2.346057| 0.0101769|
|    582|  2| 1.102606| 0.3326973|

```r
waldtest(m1_nostate_nobus, m1_nostate, m1, test="Chisq")
```

```
Wald test

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant
Model 3: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
  Res.Df Df   Chisq Pr(>Chisq)   
1    594                         
2    584 10 23.4606   0.009168 **
3    582  2  2.2052   0.332005   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(waldtest(m1_nostate_nobus, m1_nostate, m1, test="Chisq"))
```



| Res.Df| Df|     Chisq| Pr(>Chisq)|
|------:|--:|---------:|----------:|
|    594| NA|        NA|         NA|
|    584| 10| 23.460571|  0.0091684|
|    582|  2|  2.205212|  0.3320048|

```r
## without the vars with a lot of missing

m1_no_viol_off_wea <- glm(complied_bin ~
                          extortion_type + request +
                          month + time +
                          reported,
                        data=m1df,
                        family = "binomial")

summary(m1_no_viol_off_wea)
```

```

Call:
glm(formula = complied_bin ~ extortion_type + request + month + 
    time + reported, family = "binomial", data = m1df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.5610  -0.4042  -0.3020  -0.2193   2.9033  

Coefficients:
                              Estimate Std. Error z value       Pr(>|z|)
(Intercept)                  -18.77587 2419.06440  -0.008         0.9938
extortion_typeInternet         3.05524    1.43487   2.129         0.0332
extortion_typeStreet           2.47534    0.40284   6.145 0.000000000801
extortion_typePremises        21.01778 1130.91267   0.019         0.9852
extortion_typeCobro de piso    3.30462    0.84737   3.900 0.000096243486
extortion_typeOther            2.46410    0.56778   4.340 0.000014253979
requestMoney                  16.33900 2419.06431   0.007         0.9946
requestNothing                15.85942 2419.06435   0.007         0.9948
requestProduct                17.36600 2419.06443   0.007         0.9943
monthFebruary                 -0.78129    0.80406  -0.972         0.3312
monthMarch                    -0.35569    0.74412  -0.478         0.6326
monthApril                    -0.39139    0.85747  -0.456         0.6481
monthMay                      -1.27840    0.95111  -1.344         0.1789
monthJune                      0.10124    0.67196   0.151         0.8802
monthJuly                     -0.57034    0.86102  -0.662         0.5077
monthAugust                   -1.57607    1.02092  -1.544         0.1226
monthSeptember                -0.44154    0.75224  -0.587         0.5572
monthOctober                  -1.05367    0.84602  -1.245         0.2130
monthNovember                  0.48185    0.67535   0.713         0.4755
monthDecember                 -1.09579    0.80478  -1.362         0.1733
monthNONE                     -0.24308    1.15562  -0.210         0.8334
timeAfternoon                 -0.18681    0.35051  -0.533         0.5941
timeEvening                    0.03596    0.44298   0.081         0.9353
timeNight                      1.62053    0.96064   1.687         0.0916
timeDK/DA                      2.35665    1.53884   1.531         0.1257
reportedYes                    0.52450    0.38223   1.372         0.1700
                               
(Intercept)                    
extortion_typeInternet      *  
extortion_typeStreet        ***
extortion_typePremises         
extortion_typeCobro de piso ***
extortion_typeOther         ***
requestMoney                   
requestNothing                 
requestProduct                 
monthFebruary                  
monthMarch                     
monthApril                     
monthMay                       
monthJune                      
monthJuly                      
monthAugust                    
monthSeptember                 
monthOctober                   
monthNovember                  
monthDecember                  
monthNONE                      
timeAfternoon                  
timeEvening                    
timeNight                   .  
timeDK/DA                      
reportedYes                    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 461.19  on 634  degrees of freedom
Residual deviance: 323.59  on 609  degrees of freedom
AIC: 375.59

Number of Fisher Scoring iterations: 16
```

```r
lrtest(m1_no_viol_off_wea, m1_nostate_nobus)
```

```
Likelihood ratio test

Model 1: complied_bin ~ extortion_type + request + month + time + reported
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported
  #Df  LogLik Df  Chisq Pr(>Chisq)  
1  26 -161.80                       
2  41 -147.34 15 28.923    0.01646 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(lrtest(m1_no_viol_off_wea, m1_nostate_nobus))
```



| #Df|    LogLik| Df|    Chisq| Pr(>Chisq)|
|---:|---------:|--:|--------:|----------:|
|  26| -161.7968| NA|       NA|         NA|
|  41| -147.3354| 15| 28.92295|   0.016456|

```r
waldtest(m1_no_viol_off_wea, m1_nostate_nobus)
```

```
Wald test

Model 1: complied_bin ~ extortion_type + request + month + time + reported
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported
  Res.Df Df      F Pr(>F)
1    609                 
2    594 15 1.4909 0.1029
```

```r
kable(waldtest(m1_no_viol_off_wea, m1_nostate_nobus))
```



| Res.Df| Df|      F|    Pr(>F)|
|------:|--:|------:|---------:|
|    609| NA|     NA|        NA|
|    594| 15| 1.4909| 0.1028745|

```r
waldtest(m1_no_viol_off_wea, m1_nostate_nobus, test="Chisq")
```

```
Wald test

Model 1: complied_bin ~ extortion_type + request + month + time + reported
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported
  Res.Df Df  Chisq Pr(>Chisq)  
1    609                       
2    594 15 22.363    0.09862 .
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(waldtest(m1_no_viol_off_wea, m1_nostate_nobus, test="Chisq"))
```



| Res.Df| Df|    Chisq| Pr(>Chisq)|
|------:|--:|--------:|----------:|
|    609| NA|       NA|         NA|
|    594| 15| 22.36351|  0.0986208|

```r
lrtest(m1_no_viol_off_wea, m1_nostate_nobus, m1_nostate, m1)
```

```
Likelihood ratio test

Model 1: complied_bin ~ extortion_type + request + month + time + reported
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported
Model 3: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant
Model 4: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
  #Df  LogLik Df   Chisq Pr(>Chisq)   
1  26 -161.80                         
2  41 -147.34 15 28.9230   0.016456 * 
3  51 -133.91 10 26.8485   0.002752 **
4  53 -132.78  2  2.2643   0.322337   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(lrtest(m1_no_viol_off_wea, m1_nostate_nobus, m1_nostate, m1))
```



| #Df|    LogLik| Df|     Chisq| Pr(>Chisq)|
|---:|---------:|--:|---------:|----------:|
|  26| -161.7968| NA|        NA|         NA|
|  41| -147.3354| 15| 28.922955|  0.0164560|
|  51| -133.9111| 10| 26.848516|  0.0027519|
|  53| -132.7790|  2|  2.264313|  0.3223374|

```r
waldtest(m1_no_viol_off_wea, m1_nostate_nobus, m1_nostate, m1)
```

```
Wald test

Model 1: complied_bin ~ extortion_type + request + month + time + reported
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported
Model 3: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant
Model 4: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
  Res.Df Df      F  Pr(>F)  
1    609                    
2    594 15 1.4909 0.10287  
3    584 10 2.3461 0.01018 *
4    582  2 1.1026 0.33270  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(waldtest(m1_no_viol_off_wea, m1_nostate_nobus, m1_nostate, m1))
```



| Res.Df| Df|        F|    Pr(>F)|
|------:|--:|--------:|---------:|
|    609| NA|       NA|        NA|
|    594| 15| 1.490900| 0.1028745|
|    584| 10| 2.346057| 0.0101769|
|    582|  2| 1.102606| 0.3326973|

```r
waldtest(m1_no_viol_off_wea, m1_nostate_nobus, m1_nostate, m1, test="Chisq")
```

```
Wald test

Model 1: complied_bin ~ extortion_type + request + month + time + reported
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported
Model 3: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant
Model 4: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
  Res.Df Df   Chisq Pr(>Chisq)   
1    609                         
2    594 15 22.3635   0.098621 . 
3    584 10 23.4606   0.009168 **
4    582  2  2.2052   0.332005   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(waldtest(m1_no_viol_off_wea, m1_nostate_nobus, m1_nostate, m1, test="Chisq"))
```



| Res.Df| Df|     Chisq| Pr(>Chisq)|
|------:|--:|---------:|----------:|
|    609| NA|        NA|         NA|
|    594| 15| 22.363506|  0.0986208|
|    584| 10| 23.460571|  0.0091684|
|    582|  2|  2.205212|  0.3320048|

```r
htmlreg(list(m1,sm1, m1_nostate, m1_nostate_nobus,m1_no_viol_off_wea))
```

```

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
<caption align="bottom" style="margin-top:0.3em;">Statistical models</caption>
<tr>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b></b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 1</b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 2</b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 3</b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 4</b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 5</b></th>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">(Intercept)</td>
<td style="padding-right: 12px; border: none;">-23.01</td>
<td style="padding-right: 12px; border: none;">-3.23<sup style="vertical-align: 0px;">***</sup></td>
<td style="padding-right: 12px; border: none;">-22.71</td>
<td style="padding-right: 12px; border: none;">-20.89</td>
<td style="padding-right: 12px; border: none;">-18.78</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;">(0.50)</td>
<td style="padding-right: 12px; border: none;">(3442.24)</td>
<td style="padding-right: 12px; border: none;">(3757.11)</td>
<td style="padding-right: 12px; border: none;">(2419.06)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeInternet</td>
<td style="padding-right: 12px; border: none;">3.46</td>
<td style="padding-right: 12px; border: none;">2.66</td>
<td style="padding-right: 12px; border: none;">3.35</td>
<td style="padding-right: 12px; border: none;">2.29</td>
<td style="padding-right: 12px; border: none;">3.06<sup style="vertical-align: 0px;">*</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.83)</td>
<td style="padding-right: 12px; border: none;">(1.50)</td>
<td style="padding-right: 12px; border: none;">(1.82)</td>
<td style="padding-right: 12px; border: none;">(1.59)</td>
<td style="padding-right: 12px; border: none;">(1.43)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeStreet</td>
<td style="padding-right: 12px; border: none;">1.75<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">1.69<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">1.64<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">1.66<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">2.48<sup style="vertical-align: 0px;">***</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.73)</td>
<td style="padding-right: 12px; border: none;">(0.52)</td>
<td style="padding-right: 12px; border: none;">(0.72)</td>
<td style="padding-right: 12px; border: none;">(0.64)</td>
<td style="padding-right: 12px; border: none;">(0.40)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typePremises</td>
<td style="padding-right: 12px; border: none;">34.97</td>
<td style="padding-right: 12px; border: none;">19.60</td>
<td style="padding-right: 12px; border: none;">35.39</td>
<td style="padding-right: 12px; border: none;">34.48</td>
<td style="padding-right: 12px; border: none;">21.02</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1829.78)</td>
<td style="padding-right: 12px; border: none;">(960.59)</td>
<td style="padding-right: 12px; border: none;">(1805.97)</td>
<td style="padding-right: 12px; border: none;">(1886.39)</td>
<td style="padding-right: 12px; border: none;">(1130.91)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeCobro de piso</td>
<td style="padding-right: 12px; border: none;">3.12<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">2.68<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">2.89<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">3.12<sup style="vertical-align: 0px;">***</sup></td>
<td style="padding-right: 12px; border: none;">3.30<sup style="vertical-align: 0px;">***</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.15)</td>
<td style="padding-right: 12px; border: none;">(0.91)</td>
<td style="padding-right: 12px; border: none;">(1.13)</td>
<td style="padding-right: 12px; border: none;">(0.94)</td>
<td style="padding-right: 12px; border: none;">(0.85)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeOther</td>
<td style="padding-right: 12px; border: none;">2.30<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">2.29<sup style="vertical-align: 0px;">***</sup></td>
<td style="padding-right: 12px; border: none;">2.46<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">2.49<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">2.46<sup style="vertical-align: 0px;">***</sup></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.80)</td>
<td style="padding-right: 12px; border: none;">(0.58)</td>
<td style="padding-right: 12px; border: none;">(0.79)</td>
<td style="padding-right: 12px; border: none;">(0.77)</td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestMoney</td>
<td style="padding-right: 12px; border: none;">19.74</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">19.42</td>
<td style="padding-right: 12px; border: none;">18.00</td>
<td style="padding-right: 12px; border: none;">16.34</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3442.24)</td>
<td style="padding-right: 12px; border: none;">(3757.11)</td>
<td style="padding-right: 12px; border: none;">(2419.06)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestNothing</td>
<td style="padding-right: 12px; border: none;">19.61</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">19.34</td>
<td style="padding-right: 12px; border: none;">18.09</td>
<td style="padding-right: 12px; border: none;">15.86</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3442.24)</td>
<td style="padding-right: 12px; border: none;">(3757.11)</td>
<td style="padding-right: 12px; border: none;">(2419.06)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestProduct</td>
<td style="padding-right: 12px; border: none;">20.09</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">19.70</td>
<td style="padding-right: 12px; border: none;">18.57</td>
<td style="padding-right: 12px; border: none;">17.37</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3442.24)</td>
<td style="padding-right: 12px; border: none;">(3757.11)</td>
<td style="padding-right: 12px; border: none;">(2419.06)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthFebruary</td>
<td style="padding-right: 12px; border: none;">-1.50</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">-1.41</td>
<td style="padding-right: 12px; border: none;">-0.96</td>
<td style="padding-right: 12px; border: none;">-0.78</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.97)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.96)</td>
<td style="padding-right: 12px; border: none;">(0.88)</td>
<td style="padding-right: 12px; border: none;">(0.80)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthMarch</td>
<td style="padding-right: 12px; border: none;">-0.46</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">-0.48</td>
<td style="padding-right: 12px; border: none;">-0.16</td>
<td style="padding-right: 12px; border: none;">-0.36</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.85)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.84)</td>
<td style="padding-right: 12px; border: none;">(0.77)</td>
<td style="padding-right: 12px; border: none;">(0.74)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthApril</td>
<td style="padding-right: 12px; border: none;">-0.42</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">-0.45</td>
<td style="padding-right: 12px; border: none;">-0.50</td>
<td style="padding-right: 12px; border: none;">-0.39</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.94)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.93)</td>
<td style="padding-right: 12px; border: none;">(0.91)</td>
<td style="padding-right: 12px; border: none;">(0.86)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthMay</td>
<td style="padding-right: 12px; border: none;">-1.47</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">-1.32</td>
<td style="padding-right: 12px; border: none;">-0.92</td>
<td style="padding-right: 12px; border: none;">-1.28</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.07)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.06)</td>
<td style="padding-right: 12px; border: none;">(0.96)</td>
<td style="padding-right: 12px; border: none;">(0.95)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthJune</td>
<td style="padding-right: 12px; border: none;">-0.30</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">-0.34</td>
<td style="padding-right: 12px; border: none;">0.26</td>
<td style="padding-right: 12px; border: none;">0.10</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.78)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.77)</td>
<td style="padding-right: 12px; border: none;">(0.70)</td>
<td style="padding-right: 12px; border: none;">(0.67)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthJuly</td>
<td style="padding-right: 12px; border: none;">-0.52</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">-0.70</td>
<td style="padding-right: 12px; border: none;">-0.53</td>
<td style="padding-right: 12px; border: none;">-0.57</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.94)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.93)</td>
<td style="padding-right: 12px; border: none;">(0.88)</td>
<td style="padding-right: 12px; border: none;">(0.86)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthAugust</td>
<td style="padding-right: 12px; border: none;">-2.17</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">-2.16</td>
<td style="padding-right: 12px; border: none;">-1.64</td>
<td style="padding-right: 12px; border: none;">-1.58</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.17)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.12)</td>
<td style="padding-right: 12px; border: none;">(1.04)</td>
<td style="padding-right: 12px; border: none;">(1.02)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthSeptember</td>
<td style="padding-right: 12px; border: none;">-1.20</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">-1.21</td>
<td style="padding-right: 12px; border: none;">-0.82</td>
<td style="padding-right: 12px; border: none;">-0.44</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.92)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.91)</td>
<td style="padding-right: 12px; border: none;">(0.85)</td>
<td style="padding-right: 12px; border: none;">(0.75)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthOctober</td>
<td style="padding-right: 12px; border: none;">-2.11<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">-2.09<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">-1.54</td>
<td style="padding-right: 12px; border: none;">-1.05</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.06)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.03)</td>
<td style="padding-right: 12px; border: none;">(0.96)</td>
<td style="padding-right: 12px; border: none;">(0.85)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthNovember</td>
<td style="padding-right: 12px; border: none;">0.69</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">0.64</td>
<td style="padding-right: 12px; border: none;">0.57</td>
<td style="padding-right: 12px; border: none;">0.48</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.75)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.74)</td>
<td style="padding-right: 12px; border: none;">(0.70)</td>
<td style="padding-right: 12px; border: none;">(0.68)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthDecember</td>
<td style="padding-right: 12px; border: none;">-1.75</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">-1.79</td>
<td style="padding-right: 12px; border: none;">-1.69</td>
<td style="padding-right: 12px; border: none;">-1.10</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.99)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.98)</td>
<td style="padding-right: 12px; border: none;">(0.98)</td>
<td style="padding-right: 12px; border: none;">(0.80)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthNONE</td>
<td style="padding-right: 12px; border: none;">-1.49</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">-1.32</td>
<td style="padding-right: 12px; border: none;">-0.45</td>
<td style="padding-right: 12px; border: none;">-0.24</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.37)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.34)</td>
<td style="padding-right: 12px; border: none;">(1.22)</td>
<td style="padding-right: 12px; border: none;">(1.16)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeAfternoon</td>
<td style="padding-right: 12px; border: none;">0.18</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">0.23</td>
<td style="padding-right: 12px; border: none;">-0.05</td>
<td style="padding-right: 12px; border: none;">-0.19</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.41)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.41)</td>
<td style="padding-right: 12px; border: none;">(0.39)</td>
<td style="padding-right: 12px; border: none;">(0.35)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeEvening</td>
<td style="padding-right: 12px; border: none;">-0.12</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">-0.11</td>
<td style="padding-right: 12px; border: none;">0.04</td>
<td style="padding-right: 12px; border: none;">0.04</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.56)</td>
<td style="padding-right: 12px; border: none;">(0.51)</td>
<td style="padding-right: 12px; border: none;">(0.44)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeNight</td>
<td style="padding-right: 12px; border: none;">2.30<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">2.15</td>
<td style="padding-right: 12px; border: none;">1.52</td>
<td style="padding-right: 12px; border: none;">1.62</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.16)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.16)</td>
<td style="padding-right: 12px; border: none;">(1.11)</td>
<td style="padding-right: 12px; border: none;">(0.96)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeDK/DA</td>
<td style="padding-right: 12px; border: none;">2.78</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">2.68</td>
<td style="padding-right: 12px; border: none;">2.85</td>
<td style="padding-right: 12px; border: none;">2.36</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.79)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.73)</td>
<td style="padding-right: 12px; border: none;">(1.63)</td>
<td style="padding-right: 12px; border: none;">(1.54)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders2</td>
<td style="padding-right: 12px; border: none;">0.83<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">0.79<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">0.80</td>
<td style="padding-right: 12px; border: none;">0.77</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.42)</td>
<td style="padding-right: 12px; border: none;">(0.37)</td>
<td style="padding-right: 12px; border: none;">(0.42)</td>
<td style="padding-right: 12px; border: none;">(0.40)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders3</td>
<td style="padding-right: 12px; border: none;">0.79</td>
<td style="padding-right: 12px; border: none;">0.53</td>
<td style="padding-right: 12px; border: none;">0.79</td>
<td style="padding-right: 12px; border: none;">0.59</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.75)</td>
<td style="padding-right: 12px; border: none;">(0.63)</td>
<td style="padding-right: 12px; border: none;">(0.74)</td>
<td style="padding-right: 12px; border: none;">(0.69)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders4</td>
<td style="padding-right: 12px; border: none;">3.74<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">3.22<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">3.37<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">2.91<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.39)</td>
<td style="padding-right: 12px; border: none;">(1.04)</td>
<td style="padding-right: 12px; border: none;">(1.35)</td>
<td style="padding-right: 12px; border: none;">(1.17)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders5</td>
<td style="padding-right: 12px; border: none;">3.67</td>
<td style="padding-right: 12px; border: none;">3.56<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">3.93</td>
<td style="padding-right: 12px; border: none;">3.42<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(2.02)</td>
<td style="padding-right: 12px; border: none;">(1.52)</td>
<td style="padding-right: 12px; border: none;">(2.04)</td>
<td style="padding-right: 12px; border: none;">(1.68)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders6+</td>
<td style="padding-right: 12px; border: none;">0.10</td>
<td style="padding-right: 12px; border: none;">0.46</td>
<td style="padding-right: 12px; border: none;">0.02</td>
<td style="padding-right: 12px; border: none;">-0.79</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.44)</td>
<td style="padding-right: 12px; border: none;">(0.94)</td>
<td style="padding-right: 12px; border: none;">(1.41)</td>
<td style="padding-right: 12px; border: none;">(1.29)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offendersDK/DA</td>
<td style="padding-right: 12px; border: none;">-0.11</td>
<td style="padding-right: 12px; border: none;">-0.14</td>
<td style="padding-right: 12px; border: none;">-0.07</td>
<td style="padding-right: 12px; border: none;">0.11</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.84)</td>
<td style="padding-right: 12px; border: none;">(0.71)</td>
<td style="padding-right: 12px; border: none;">(0.83)</td>
<td style="padding-right: 12px; border: none;">(0.78)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersEmployee</td>
<td style="padding-right: 12px; border: none;">-14.96</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">-15.27</td>
<td style="padding-right: 12px; border: none;">-16.05</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1278.54)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1263.92)</td>
<td style="padding-right: 12px; border: none;">(1316.18)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersBarely known</td>
<td style="padding-right: 12px; border: none;">-0.04</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">-0.12</td>
<td style="padding-right: 12px; border: none;">-0.35</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.93)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.91)</td>
<td style="padding-right: 12px; border: none;">(0.80)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersSomewhat known</td>
<td style="padding-right: 12px; border: none;">-1.02</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">-0.93</td>
<td style="padding-right: 12px; border: none;">-1.26</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.68)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.66)</td>
<td style="padding-right: 12px; border: none;">(1.58)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersClose acquaintance</td>
<td style="padding-right: 12px; border: none;">1.53</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">1.75</td>
<td style="padding-right: 12px; border: none;">0.73</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.74)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.74)</td>
<td style="padding-right: 12px; border: none;">(1.50)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersDK/DA</td>
<td style="padding-right: 12px; border: none;">-1.53</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">-1.33</td>
<td style="padding-right: 12px; border: none;">-1.24</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.17)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.15)</td>
<td style="padding-right: 12px; border: none;">(1.08)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">had_weaponYes</td>
<td style="padding-right: 12px; border: none;">1.03</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">1.00</td>
<td style="padding-right: 12px; border: none;">1.04</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.02)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.99)</td>
<td style="padding-right: 12px; border: none;">(0.89)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">had_weaponDK/DA</td>
<td style="padding-right: 12px; border: none;">0.30</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">0.25</td>
<td style="padding-right: 12px; border: none;">0.22</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.43)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.42)</td>
<td style="padding-right: 12px; border: none;">(0.40)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">with_violenceYes</td>
<td style="padding-right: 12px; border: none;">1.34</td>
<td style="padding-right: 12px; border: none;">1.12</td>
<td style="padding-right: 12px; border: none;">1.60</td>
<td style="padding-right: 12px; border: none;">1.17</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.02)</td>
<td style="padding-right: 12px; border: none;">(0.80)</td>
<td style="padding-right: 12px; border: none;">(1.00)</td>
<td style="padding-right: 12px; border: none;">(0.90)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">with_violenceDK/DA</td>
<td style="padding-right: 12px; border: none;">1.23</td>
<td style="padding-right: 12px; border: none;">0.98</td>
<td style="padding-right: 12px; border: none;">1.24</td>
<td style="padding-right: 12px; border: none;">1.11</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.71)</td>
<td style="padding-right: 12px; border: none;">(0.61)</td>
<td style="padding-right: 12px; border: none;">(0.70)</td>
<td style="padding-right: 12px; border: none;">(0.66)</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">reportedYes</td>
<td style="padding-right: 12px; border: none;">0.64</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">0.65</td>
<td style="padding-right: 12px; border: none;">0.46</td>
<td style="padding-right: 12px; border: none;">0.52</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.46)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.46)</td>
<td style="padding-right: 12px; border: none;">(0.43)</td>
<td style="padding-right: 12px; border: none;">(0.38)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rep_extortion_victim1</td>
<td style="padding-right: 12px; border: none;">1.08</td>
<td style="padding-right: 12px; border: none;">1.06</td>
<td style="padding-right: 12px; border: none;">1.05</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.65)</td>
<td style="padding-right: 12px; border: none;">(0.55)</td>
<td style="padding-right: 12px; border: none;">(0.64)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">bribe_victim1</td>
<td style="padding-right: 12px; border: none;">0.12</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">0.12</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.54)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.54)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeMedium</td>
<td style="padding-right: 12px; border: none;">-0.27</td>
<td style="padding-right: 12px; border: none;">-0.31</td>
<td style="padding-right: 12px; border: none;">-0.27</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.53)</td>
<td style="padding-right: 12px; border: none;">(0.45)</td>
<td style="padding-right: 12px; border: none;">(0.53)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeSmall</td>
<td style="padding-right: 12px; border: none;">0.92</td>
<td style="padding-right: 12px; border: none;">0.48</td>
<td style="padding-right: 12px; border: none;">0.96</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.50)</td>
<td style="padding-right: 12px; border: none;">(0.42)</td>
<td style="padding-right: 12px; border: none;">(0.50)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeMicro</td>
<td style="padding-right: 12px; border: none;">-0.28</td>
<td style="padding-right: 12px; border: none;">-0.65</td>
<td style="padding-right: 12px; border: none;">-0.26</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
<td style="padding-right: 12px; border: none;">(0.49)</td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">hotrestbar1</td>
<td style="padding-right: 12px; border: none;">-0.94</td>
<td style="padding-right: 12px; border: none;">-0.97</td>
<td style="padding-right: 12px; border: none;">-0.84</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.66)</td>
<td style="padding-right: 12px; border: none;">(0.58)</td>
<td style="padding-right: 12px; border: none;">(0.65)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(8,16]</td>
<td style="padding-right: 12px; border: none;">-0.35</td>
<td style="padding-right: 12px; border: none;">-0.33</td>
<td style="padding-right: 12px; border: none;">-0.36</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.59)</td>
<td style="padding-right: 12px; border: none;">(0.54)</td>
<td style="padding-right: 12px; border: none;">(0.60)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(16,25]</td>
<td style="padding-right: 12px; border: none;">0.87</td>
<td style="padding-right: 12px; border: none;">0.61</td>
<td style="padding-right: 12px; border: none;">0.91</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.53)</td>
<td style="padding-right: 12px; border: none;">(0.47)</td>
<td style="padding-right: 12px; border: none;">(0.53)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(25,34]</td>
<td style="padding-right: 12px; border: none;">-0.80</td>
<td style="padding-right: 12px; border: none;">-0.50</td>
<td style="padding-right: 12px; border: none;">-0.67</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.68)</td>
<td style="padding-right: 12px; border: none;">(0.58)</td>
<td style="padding-right: 12px; border: none;">(0.67)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(34,43]</td>
<td style="padding-right: 12px; border: none;">1.28<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">1.08<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">1.30<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
<td style="padding-right: 12px; border: none;">(0.48)</td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">loghoms</td>
<td style="padding-right: 12px; border: none;">-0.28</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.26)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">logpop</td>
<td style="padding-right: 12px; border: none;">0.59</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.40)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="border-top: 1px solid black;">AIC</td>
<td style="border-top: 1px solid black;">371.56</td>
<td style="border-top: 1px solid black;">349.58</td>
<td style="border-top: 1px solid black;">369.82</td>
<td style="border-top: 1px solid black;">376.67</td>
<td style="border-top: 1px solid black;">375.59</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">BIC</td>
<td style="padding-right: 12px; border: none;">607.60</td>
<td style="padding-right: 12px; border: none;">452.01</td>
<td style="padding-right: 12px; border: none;">596.96</td>
<td style="padding-right: 12px; border: none;">559.27</td>
<td style="padding-right: 12px; border: none;">491.39</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Log Likelihood</td>
<td style="padding-right: 12px; border: none;">-132.78</td>
<td style="padding-right: 12px; border: none;">-151.79</td>
<td style="padding-right: 12px; border: none;">-133.91</td>
<td style="padding-right: 12px; border: none;">-147.34</td>
<td style="padding-right: 12px; border: none;">-161.80</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Deviance</td>
<td style="padding-right: 12px; border: none;">265.56</td>
<td style="padding-right: 12px; border: none;">303.58</td>
<td style="padding-right: 12px; border: none;">267.82</td>
<td style="padding-right: 12px; border: none;">294.67</td>
<td style="padding-right: 12px; border: none;">323.59</td>
</tr>
<tr>
<td style="border-bottom: 2px solid black;">Num. obs.</td>
<td style="border-bottom: 2px solid black;">635</td>
<td style="border-bottom: 2px solid black;">635</td>
<td style="border-bottom: 2px solid black;">635</td>
<td style="border-bottom: 2px solid black;">635</td>
<td style="border-bottom: 2px solid black;">635</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;" colspan="7"><span style="font-size:0.8em"><sup style="vertical-align: 0px;">***</sup>p &lt; 0.001, <sup style="vertical-align: 0px;">**</sup>p &lt; 0.01, <sup style="vertical-align: 0px;">*</sup>p &lt; 0.05</span></td>
</tr>
</table>
```

```r
screenreg(list(m1,sm1, m1_nostate, m1_nostate_nobus,m1_no_viol_off_wea))
```

```

==================================================================================================
                                 Model 1      Model 2      Model 3      Model 4       Model 5     
--------------------------------------------------------------------------------------------------
(Intercept)                        -23.01       -3.23 ***    -22.71       -20.89        -18.78    
                                 (3420.48)      (0.50)     (3442.24)    (3757.11)     (2419.06)   
extortion_typeInternet               3.46        2.66          3.35         2.29          3.06 *  
                                    (1.83)      (1.50)        (1.82)       (1.59)        (1.43)   
extortion_typeStreet                 1.75 *      1.69 **       1.64 *       1.66 **       2.48 ***
                                    (0.73)      (0.52)        (0.72)       (0.64)        (0.40)   
extortion_typePremises              34.97       19.60         35.39        34.48         21.02    
                                 (1829.78)    (960.59)     (1805.97)    (1886.39)     (1130.91)   
extortion_typeCobro de piso          3.12 **     2.68 **       2.89 *       3.12 ***      3.30 ***
                                    (1.15)      (0.91)        (1.13)       (0.94)        (0.85)   
extortion_typeOther                  2.30 **     2.29 ***      2.46 **      2.49 **       2.46 ***
                                    (0.80)      (0.58)        (0.79)       (0.77)        (0.57)   
requestMoney                        19.74                     19.42        18.00         16.34    
                                 (3420.48)                 (3442.24)    (3757.11)     (2419.06)   
requestNothing                      19.61                     19.34        18.09         15.86    
                                 (3420.48)                 (3442.24)    (3757.11)     (2419.06)   
requestProduct                      20.09                     19.70        18.57         17.37    
                                 (3420.48)                 (3442.24)    (3757.11)     (2419.06)   
monthFebruary                       -1.50                     -1.41        -0.96         -0.78    
                                    (0.97)                    (0.96)       (0.88)        (0.80)   
monthMarch                          -0.46                     -0.48        -0.16         -0.36    
                                    (0.85)                    (0.84)       (0.77)        (0.74)   
monthApril                          -0.42                     -0.45        -0.50         -0.39    
                                    (0.94)                    (0.93)       (0.91)        (0.86)   
monthMay                            -1.47                     -1.32        -0.92         -1.28    
                                    (1.07)                    (1.06)       (0.96)        (0.95)   
monthJune                           -0.30                     -0.34         0.26          0.10    
                                    (0.78)                    (0.77)       (0.70)        (0.67)   
monthJuly                           -0.52                     -0.70        -0.53         -0.57    
                                    (0.94)                    (0.93)       (0.88)        (0.86)   
monthAugust                         -2.17                     -2.16        -1.64         -1.58    
                                    (1.17)                    (1.12)       (1.04)        (1.02)   
monthSeptember                      -1.20                     -1.21        -0.82         -0.44    
                                    (0.92)                    (0.91)       (0.85)        (0.75)   
monthOctober                        -2.11 *                   -2.09 *      -1.54         -1.05    
                                    (1.06)                    (1.03)       (0.96)        (0.85)   
monthNovember                        0.69                      0.64         0.57          0.48    
                                    (0.75)                    (0.74)       (0.70)        (0.68)   
monthDecember                       -1.75                     -1.79        -1.69         -1.10    
                                    (0.99)                    (0.98)       (0.98)        (0.80)   
monthNONE                           -1.49                     -1.32        -0.45         -0.24    
                                    (1.37)                    (1.34)       (1.22)        (1.16)   
timeAfternoon                        0.18                      0.23        -0.05         -0.19    
                                    (0.41)                    (0.41)       (0.39)        (0.35)   
timeEvening                         -0.12                     -0.11         0.04          0.04    
                                    (0.57)                    (0.56)       (0.51)        (0.44)   
timeNight                            2.30 *                    2.15         1.52          1.62    
                                    (1.16)                    (1.16)       (1.11)        (0.96)   
timeDK/DA                            2.78                      2.68         2.85          2.36    
                                    (1.79)                    (1.73)       (1.63)        (1.54)   
n_offenders2                         0.83 *      0.79 *        0.80         0.77                  
                                    (0.42)      (0.37)        (0.42)       (0.40)                 
n_offenders3                         0.79        0.53          0.79         0.59                  
                                    (0.75)      (0.63)        (0.74)       (0.69)                 
n_offenders4                         3.74 **     3.22 **       3.37 *       2.91 *                
                                    (1.39)      (1.04)        (1.35)       (1.17)                 
n_offenders5                         3.67        3.56 *        3.93         3.42 *                
                                    (2.02)      (1.52)        (2.04)       (1.68)                 
n_offenders6+                        0.10        0.46          0.02        -0.79                  
                                    (1.44)      (0.94)        (1.41)       (1.29)                 
n_offendersDK/DA                    -0.11       -0.14         -0.07         0.11                  
                                    (0.84)      (0.71)        (0.83)       (0.78)                 
rel_offendersEmployee              -14.96                    -15.27       -16.05                  
                                 (1278.54)                 (1263.92)    (1316.18)                 
rel_offendersBarely known           -0.04                     -0.12        -0.35                  
                                    (0.93)                    (0.91)       (0.80)                 
rel_offendersSomewhat known         -1.02                     -0.93        -1.26                  
                                    (1.68)                    (1.66)       (1.58)                 
rel_offendersClose acquaintance      1.53                      1.75         0.73                  
                                    (1.74)                    (1.74)       (1.50)                 
rel_offendersDK/DA                  -1.53                     -1.33        -1.24                  
                                    (1.17)                    (1.15)       (1.08)                 
had_weaponYes                        1.03                      1.00         1.04                  
                                    (1.02)                    (0.99)       (0.89)                 
had_weaponDK/DA                      0.30                      0.25         0.22                  
                                    (0.43)                    (0.42)       (0.40)                 
with_violenceYes                     1.34        1.12          1.60         1.17                  
                                    (1.02)      (0.80)        (1.00)       (0.90)                 
with_violenceDK/DA                   1.23        0.98          1.24         1.11                  
                                    (0.71)      (0.61)        (0.70)       (0.66)                 
reportedYes                          0.64                      0.65         0.46          0.52    
                                    (0.46)                    (0.46)       (0.43)        (0.38)   
rep_extortion_victim1                1.08        1.06          1.05                               
                                    (0.65)      (0.55)        (0.64)                              
bribe_victim1                        0.12                      0.12                               
                                    (0.54)                    (0.54)                              
sizeMedium                          -0.27       -0.31         -0.27                               
                                    (0.53)      (0.45)        (0.53)                              
sizeSmall                            0.92        0.48          0.96                               
                                    (0.50)      (0.42)        (0.50)                              
sizeMicro                           -0.28       -0.65         -0.26                               
                                    (0.57)      (0.49)        (0.57)                              
hotrestbar1                         -0.94       -0.97         -0.84                               
                                    (0.66)      (0.58)        (0.65)                              
yearsquant(8,16]                    -0.35       -0.33         -0.36                               
                                    (0.59)      (0.54)        (0.60)                              
yearsquant(16,25]                    0.87        0.61          0.91                               
                                    (0.53)      (0.47)        (0.53)                              
yearsquant(25,34]                   -0.80       -0.50         -0.67                               
                                    (0.68)      (0.58)        (0.67)                              
yearsquant(34,43]                    1.28 *      1.08 *        1.30 *                             
                                    (0.57)      (0.48)        (0.57)                              
loghoms                             -0.28                                                         
                                    (0.26)                                                        
logpop                               0.59                                                         
                                    (0.40)                                                        
--------------------------------------------------------------------------------------------------
AIC                                371.56      349.58        369.82       376.67        375.59    
BIC                                607.60      452.01        596.96       559.27        491.39    
Log Likelihood                    -132.78     -151.79       -133.91      -147.34       -161.80    
Deviance                           265.56      303.58        267.82       294.67        323.59    
Num. obs.                          635         635           635          635           635       
==================================================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
m1_null <- glm(complied_bin ~ 1, data=m1df, family="binomial")

# Null model
anova(m1, m1_null, test="Chisq")
```

```
Analysis of Deviance Table

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
Model 2: complied_bin ~ 1
  Resid. Df Resid. Dev  Df Deviance              Pr(>Chi)    
1       582     265.56                                       
2       634     461.19 -52  -195.63 < 0.00000000000000022 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(anova(m1, m1_null, test="Chisq"))
```



| Resid. Df| Resid. Dev|  Df|  Deviance| Pr(>Chi)|
|---------:|----------:|---:|---------:|--------:|
|       582|   265.5579|  NA|        NA|       NA|
|       634|   461.1913| -52| -195.6334|        0|

```r
anova(m1, m1_null, test="LRT")
```

```
Analysis of Deviance Table

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
Model 2: complied_bin ~ 1
  Resid. Df Resid. Dev  Df Deviance              Pr(>Chi)    
1       582     265.56                                       
2       634     461.19 -52  -195.63 < 0.00000000000000022 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(anova(m1, m1_null, test="LRT"))
```



| Resid. Df| Resid. Dev|  Df|  Deviance| Pr(>Chi)|
|---------:|----------:|---:|---------:|--------:|
|       582|   265.5579|  NA|        NA|       NA|
|       634|   461.1913| -52| -195.6334|        0|

```r
lrtest(m1_null, m1)
```

```
Likelihood ratio test

Model 1: complied_bin ~ 1
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
  #Df  LogLik Df  Chisq            Pr(>Chisq)    
1   1 -230.60                                    
2  53 -132.78 52 195.63 < 0.00000000000000022 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(lrtest(m1_null, m1))
```



| #Df|    LogLik| Df|    Chisq| Pr(>Chisq)|
|---:|---------:|--:|--------:|----------:|
|   1| -230.5957| NA|       NA|         NA|
|  53| -132.7790| 52| 195.6334|          0|

Now we try a two-level model


```r
m1_l2 <- glmer(complied_bin ~
            extortion_type + request +
            month + time +
            n_offenders + rel_offenders +
            had_weapon +
            with_violence +
            reported +
            rep_extortion_victim +
            bribe_victim +
            size + hotrestbar + yearsquant +
            loghoms + logpop +
            (1|CVE_UNICA),
          data=m1df,
          family = "binomial")
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge: degenerate Hessian with 11 negative
eigenvalues
```

```r
summary(m1_l2)
```

```
Warning in vcov.merMod(object, use.hessian = use.hessian): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```
Warning in vcov.merMod(object, correlation = correlation, sigm = sig): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```
Generalized linear mixed model fit by maximum likelihood (Laplace
  Approximation) [glmerMod]
 Family: binomial  ( logit )
Formula: 
complied_bin ~ extortion_type + request + month + time + n_offenders +  
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim +  
    bribe_victim + size + hotrestbar + yearsquant + loghoms +  
    logpop + (1 | CVE_UNICA)
   Data: m1df

     AIC      BIC   logLik deviance df.resid 
   324.7    565.2   -108.3    216.7      581 

Scaled residuals: 
     Min       1Q   Median       3Q      Max 
-0.24130 -0.00016  0.00000  0.00000  0.32271 

Random effects:
 Groups    Name        Variance Std.Dev.
 CVE_UNICA (Intercept) 414.7    20.36   
Number of obs: 635, groups:  CVE_UNICA, 522

Fixed effects:
                                     Estimate    Std. Error z value
(Intercept)                          -30.7988       72.6582  -0.424
extortion_typeInternet                29.2325       52.7938   0.554
extortion_typeStreet                   9.4512       11.3803   0.830
extortion_typePremises               119.5421 18612475.2438   0.000
extortion_typeCobro de piso            5.6337       25.8438   0.218
extortion_typeOther                   21.8831       13.6986   1.597
requestMoney                          28.7471       68.9834   0.417
requestNothing                        27.8258       69.2329   0.402
requestProduct                        30.1831       72.1578   0.418
monthFebruary                        -30.3246       18.3866  -1.649
monthMarch                           -12.3114       15.2383  -0.808
monthApril                           -24.2133       18.2354  -1.328
monthMay                             -16.6686       24.2838  -0.686
monthJune                             -9.5787       12.1368  -0.789
monthJuly                            -12.7931       13.5205  -0.946
monthAugust                          -27.5280       24.6930  -1.115
monthSeptember                       -11.2135       12.3983  -0.904
monthOctober                         -17.1208       22.5434  -0.760
monthNovember                         -5.3725       11.3595  -0.473
monthDecember                        -18.8169       18.0524  -1.042
monthNONE                            -17.8191       33.5971  -0.530
timeAfternoon                          0.5654        8.3259   0.068
timeEvening                           -1.5746       11.8816  -0.132
timeNight                              2.3512       31.3379   0.075
timeDK/DA                             -6.3414       44.0124  -0.144
n_offenders2                           0.4632        8.9016   0.052
n_offenders3                           6.5500       14.0902   0.465
n_offenders4                          23.6939       25.5443   0.928
n_offenders5                           6.3157       90.2835   0.070
n_offenders6+                        -33.4359       21.4440  -1.559
n_offendersDK/DA                      -8.4631       19.1463  -0.442
rel_offendersEmployee                -87.9783 18612475.2439   0.000
rel_offendersBarely known              1.8957       14.9693   0.127
rel_offendersSomewhat known          -36.0990       37.0300  -0.975
rel_offendersClose acquaintance        3.7387       33.1723   0.113
rel_offendersDK/DA                    -5.4114       30.2767  -0.179
had_weaponYes                         19.0876       18.0650   1.057
had_weaponDK/DA                       -2.8166        9.2469  -0.305
with_violenceYes                       3.9140       17.5495   0.223
with_violenceDK/DA                    11.3647       14.9030   0.763
reportedYes                            0.9579       10.5785   0.091
rep_extortion_victim1                  3.2016       15.5150   0.206
bribe_victim1                          0.5840       13.3323   0.044
sizeMedium                            -8.9891       10.1890  -0.882
sizeSmall                             -0.3190       10.0278  -0.032
sizeMicro                             -8.0075       12.4984  -0.641
hotrestbar1                           -4.9344       19.8655  -0.248
yearsquant(8,16]                      -4.3022       10.7793  -0.399
yearsquant(16,25]                     -0.8125        9.9636  -0.082
yearsquant(25,34]                     -3.7709       14.4867  -0.260
yearsquant(34,43]                      1.0537       11.2289   0.094
loghoms                               -4.0323        5.0539  -0.798
logpop                                 5.1555        8.1277   0.634
                                Pr(>|z|)  
(Intercept)                       0.6716  
extortion_typeInternet            0.5798  
extortion_typeStreet              0.4063  
extortion_typePremises            1.0000  
extortion_typeCobro de piso       0.8274  
extortion_typeOther               0.1102  
requestMoney                      0.6769  
requestNothing                    0.6877  
requestProduct                    0.6757  
monthFebruary                     0.0991 .
monthMarch                        0.4191  
monthApril                        0.1842  
monthMay                          0.4925  
monthJune                         0.4300  
monthJuly                         0.3440  
monthAugust                       0.2649  
monthSeptember                    0.3658  
monthOctober                      0.4476  
monthNovember                     0.6362  
monthDecember                     0.2973  
monthNONE                         0.5959  
timeAfternoon                     0.9459  
timeEvening                       0.8946  
timeNight                         0.9402  
timeDK/DA                         0.8854  
n_offenders2                      0.9585  
n_offenders3                      0.6420  
n_offenders4                      0.3536  
n_offenders5                      0.9442  
n_offenders6+                     0.1189  
n_offendersDK/DA                  0.6585  
rel_offendersEmployee             1.0000  
rel_offendersBarely known         0.8992  
rel_offendersSomewhat known       0.3296  
rel_offendersClose acquaintance   0.9103  
rel_offendersDK/DA                0.8581  
had_weaponYes                     0.2907  
had_weaponDK/DA                   0.7607  
with_violenceYes                  0.8235  
with_violenceDK/DA                0.4457  
reportedYes                       0.9278  
rep_extortion_victim1             0.8365  
bribe_victim1                     0.9651  
sizeMedium                        0.3776  
sizeSmall                         0.9746  
sizeMicro                         0.5217  
hotrestbar1                       0.8038  
yearsquant(8,16]                  0.6898  
yearsquant(16,25]                 0.9350  
yearsquant(25,34]                 0.7946  
yearsquant(34,43]                 0.9252  
loghoms                           0.4249  
logpop                            0.5259  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```

Correlation matrix not shown by default, as p = 53 > 12.
Use print(x, correlation=TRUE)  or
	 vcov(x)	 if you need it
```

```
convergence code: 0
unable to evaluate scaled gradient
Model failed to converge: degenerate  Hessian with 11 negative eigenvalues
failure to converge in 10000 evaluations
```

```r
### ICC

icc(m1_l2)
```

```
Generalized linear mixed model
 Family: binomial (logit)
Formula: complied_bin ~ extortion_type + request + month + time + n_offenders + rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + bribe_victim + size + hotrestbar + yearsquant + loghoms + logpop + (1 | CVE_UNICA)

  ICC (CVE_UNICA): 0.992129
```

```r
print(icc(m1_l2), comp="var")
```

```
Generalized linear mixed model
 Family: binomial (logit)
Formula: complied_bin ~ extortion_type + request + month + time + n_offenders + rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + bribe_victim + size + hotrestbar + yearsquant + loghoms + logpop + (1 | CVE_UNICA)

      Within-group-variance:    3.290
     Between-group-variance:  414.677 (CVE_UNICA)
```

```r
## Describe data structure

## number of observations per individuals

length(unique(m1df$CVE_UNICA))
```

```
[1] 522
```

```r
table(table(m1df$CVE_UNICA))
```

```

  1   2   3   4   5 
451  43  19   4   5 
```

```r
min(table(m1df$CVE_UNICA))
```

```
[1] 1
```

```r
max(table(m1df$CVE_UNICA))
```

```
[1] 5
```

```r
mean(table(m1df$CVE_UNICA))
```

```
[1] 1.216475
```

```r
sum(table(m1df$CVE_UNICA))
```

```
[1] 635
```

```r
# number of observations per state
length(unique(m1df$CVE_ENT))
```

```
[1] 32
```

```r
table(m1df$CVE_ENT)
```

```

 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 
27 16 14 18 19 23 20 17 26 17 17 18 23 29 17 22 13 13 18 22 26 21 15 22 14 
26 27 28 29 30 31 32 
29 26 13 16 22 12 30 
```

```r
sum(table(m1df$CVE_ENT))
```

```
[1] 635
```

```r
min(table(m1df$CVE_ENT))
```

```
[1] 12
```

```r
max(table(m1df$CVE_ENT))
```

```
[1] 30
```

```r
mean(table(m1df$CVE_ENT))
```

```
[1] 19.84375
```

```r
# number of individuals per state

IperG <- with(m1df, tapply(CVE_UNICA, CVE_ENT,
                           FUN = function(x) length(unique(x))))
sum(IperG)
```

```
[1] 522
```

```r
min(IperG)
```

```
[1] 11
```

```r
max(IperG)
```

```
[1] 24
```

```r
mean(IperG)
```

```
[1] 16.3125
```

```r
## print m1_l2
htmlreg(m1_l2)
```

```
Warning in vcov.merMod(model, useScale = FALSE, ...): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
<caption align="bottom" style="margin-top:0.3em;">Statistical models</caption>
<tr>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b></b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 1</b></th>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">(Intercept)</td>
<td style="padding-right: 12px; border: none;">-30.80</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(72.66)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeInternet</td>
<td style="padding-right: 12px; border: none;">29.23</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(52.79)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeStreet</td>
<td style="padding-right: 12px; border: none;">9.45</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(11.38)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typePremises</td>
<td style="padding-right: 12px; border: none;">119.54</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(18612475.24)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeCobro de piso</td>
<td style="padding-right: 12px; border: none;">5.63</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(25.84)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeOther</td>
<td style="padding-right: 12px; border: none;">21.88</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(13.70)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestMoney</td>
<td style="padding-right: 12px; border: none;">28.75</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(68.98)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestNothing</td>
<td style="padding-right: 12px; border: none;">27.83</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(69.23)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestProduct</td>
<td style="padding-right: 12px; border: none;">30.18</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(72.16)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthFebruary</td>
<td style="padding-right: 12px; border: none;">-30.32</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(18.39)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthMarch</td>
<td style="padding-right: 12px; border: none;">-12.31</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(15.24)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthApril</td>
<td style="padding-right: 12px; border: none;">-24.21</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(18.24)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthMay</td>
<td style="padding-right: 12px; border: none;">-16.67</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(24.28)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthJune</td>
<td style="padding-right: 12px; border: none;">-9.58</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(12.14)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthJuly</td>
<td style="padding-right: 12px; border: none;">-12.79</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(13.52)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthAugust</td>
<td style="padding-right: 12px; border: none;">-27.53</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(24.69)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthSeptember</td>
<td style="padding-right: 12px; border: none;">-11.21</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(12.40)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthOctober</td>
<td style="padding-right: 12px; border: none;">-17.12</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(22.54)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthNovember</td>
<td style="padding-right: 12px; border: none;">-5.37</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(11.36)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthDecember</td>
<td style="padding-right: 12px; border: none;">-18.82</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(18.05)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthNONE</td>
<td style="padding-right: 12px; border: none;">-17.82</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(33.60)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeAfternoon</td>
<td style="padding-right: 12px; border: none;">0.57</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(8.33)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeEvening</td>
<td style="padding-right: 12px; border: none;">-1.57</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(11.88)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeNight</td>
<td style="padding-right: 12px; border: none;">2.35</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(31.34)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeDK/DA</td>
<td style="padding-right: 12px; border: none;">-6.34</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(44.01)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders2</td>
<td style="padding-right: 12px; border: none;">0.46</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(8.90)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders3</td>
<td style="padding-right: 12px; border: none;">6.55</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(14.09)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders4</td>
<td style="padding-right: 12px; border: none;">23.69</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(25.54)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders5</td>
<td style="padding-right: 12px; border: none;">6.32</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(90.28)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders6+</td>
<td style="padding-right: 12px; border: none;">-33.44</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(21.44)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offendersDK/DA</td>
<td style="padding-right: 12px; border: none;">-8.46</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(19.15)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersEmployee</td>
<td style="padding-right: 12px; border: none;">-87.98</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(18612475.24)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersBarely known</td>
<td style="padding-right: 12px; border: none;">1.90</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(14.97)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersSomewhat known</td>
<td style="padding-right: 12px; border: none;">-36.10</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(37.03)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersClose acquaintance</td>
<td style="padding-right: 12px; border: none;">3.74</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(33.17)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersDK/DA</td>
<td style="padding-right: 12px; border: none;">-5.41</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(30.28)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">had_weaponYes</td>
<td style="padding-right: 12px; border: none;">19.09</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(18.07)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">had_weaponDK/DA</td>
<td style="padding-right: 12px; border: none;">-2.82</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(9.25)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">with_violenceYes</td>
<td style="padding-right: 12px; border: none;">3.91</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(17.55)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">with_violenceDK/DA</td>
<td style="padding-right: 12px; border: none;">11.36</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(14.90)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">reportedYes</td>
<td style="padding-right: 12px; border: none;">0.96</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(10.58)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rep_extortion_victim1</td>
<td style="padding-right: 12px; border: none;">3.20</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(15.51)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">bribe_victim1</td>
<td style="padding-right: 12px; border: none;">0.58</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(13.33)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeMedium</td>
<td style="padding-right: 12px; border: none;">-8.99</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(10.19)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeSmall</td>
<td style="padding-right: 12px; border: none;">-0.32</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(10.03)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeMicro</td>
<td style="padding-right: 12px; border: none;">-8.01</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(12.50)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">hotrestbar1</td>
<td style="padding-right: 12px; border: none;">-4.93</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(19.87)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(8,16]</td>
<td style="padding-right: 12px; border: none;">-4.30</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(10.78)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(16,25]</td>
<td style="padding-right: 12px; border: none;">-0.81</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(9.96)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(25,34]</td>
<td style="padding-right: 12px; border: none;">-3.77</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(14.49)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(34,43]</td>
<td style="padding-right: 12px; border: none;">1.05</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(11.23)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">loghoms</td>
<td style="padding-right: 12px; border: none;">-4.03</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(5.05)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">logpop</td>
<td style="padding-right: 12px; border: none;">5.16</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(8.13)</td>
</tr>
<tr>
<td style="border-top: 1px solid black;">AIC</td>
<td style="border-top: 1px solid black;">324.69</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">BIC</td>
<td style="padding-right: 12px; border: none;">565.19</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Log Likelihood</td>
<td style="padding-right: 12px; border: none;">-108.35</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Num. obs.</td>
<td style="padding-right: 12px; border: none;">635</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Num. groups: CVE_UNICA</td>
<td style="padding-right: 12px; border: none;">522</td>
</tr>
<tr>
<td style="border-bottom: 2px solid black;">Var: CVE_UNICA (Intercept)</td>
<td style="border-bottom: 2px solid black;">414.68</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;" colspan="3"><span style="font-size:0.8em"><sup style="vertical-align: 0px;">***</sup>p &lt; 0.001, <sup style="vertical-align: 0px;">**</sup>p &lt; 0.01, <sup style="vertical-align: 0px;">*</sup>p &lt; 0.05</span></td>
</tr>
</table>
```

```r
screenreg(m1_l2)
```

```
Warning in vcov.merMod(model, useScale = FALSE, ...): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```

==============================================
                                 Model 1      
----------------------------------------------
(Intercept)                            -30.80 
                                       (72.66)
extortion_typeInternet                  29.23 
                                       (52.79)
extortion_typeStreet                     9.45 
                                       (11.38)
extortion_typePremises                 119.54 
                                 (18612475.24)
extortion_typeCobro de piso              5.63 
                                       (25.84)
extortion_typeOther                     21.88 
                                       (13.70)
requestMoney                            28.75 
                                       (68.98)
requestNothing                          27.83 
                                       (69.23)
requestProduct                          30.18 
                                       (72.16)
monthFebruary                          -30.32 
                                       (18.39)
monthMarch                             -12.31 
                                       (15.24)
monthApril                             -24.21 
                                       (18.24)
monthMay                               -16.67 
                                       (24.28)
monthJune                               -9.58 
                                       (12.14)
monthJuly                              -12.79 
                                       (13.52)
monthAugust                            -27.53 
                                       (24.69)
monthSeptember                         -11.21 
                                       (12.40)
monthOctober                           -17.12 
                                       (22.54)
monthNovember                           -5.37 
                                       (11.36)
monthDecember                          -18.82 
                                       (18.05)
monthNONE                              -17.82 
                                       (33.60)
timeAfternoon                            0.57 
                                        (8.33)
timeEvening                             -1.57 
                                       (11.88)
timeNight                                2.35 
                                       (31.34)
timeDK/DA                               -6.34 
                                       (44.01)
n_offenders2                             0.46 
                                        (8.90)
n_offenders3                             6.55 
                                       (14.09)
n_offenders4                            23.69 
                                       (25.54)
n_offenders5                             6.32 
                                       (90.28)
n_offenders6+                          -33.44 
                                       (21.44)
n_offendersDK/DA                        -8.46 
                                       (19.15)
rel_offendersEmployee                  -87.98 
                                 (18612475.24)
rel_offendersBarely known                1.90 
                                       (14.97)
rel_offendersSomewhat known            -36.10 
                                       (37.03)
rel_offendersClose acquaintance          3.74 
                                       (33.17)
rel_offendersDK/DA                      -5.41 
                                       (30.28)
had_weaponYes                           19.09 
                                       (18.07)
had_weaponDK/DA                         -2.82 
                                        (9.25)
with_violenceYes                         3.91 
                                       (17.55)
with_violenceDK/DA                      11.36 
                                       (14.90)
reportedYes                              0.96 
                                       (10.58)
rep_extortion_victim1                    3.20 
                                       (15.51)
bribe_victim1                            0.58 
                                       (13.33)
sizeMedium                              -8.99 
                                       (10.19)
sizeSmall                               -0.32 
                                       (10.03)
sizeMicro                               -8.01 
                                       (12.50)
hotrestbar1                             -4.93 
                                       (19.87)
yearsquant(8,16]                        -4.30 
                                       (10.78)
yearsquant(16,25]                       -0.81 
                                        (9.96)
yearsquant(25,34]                       -3.77 
                                       (14.49)
yearsquant(34,43]                        1.05 
                                       (11.23)
loghoms                                 -4.03 
                                        (5.05)
logpop                                   5.16 
                                        (8.13)
----------------------------------------------
AIC                                    324.69 
BIC                                    565.19 
Log Likelihood                        -108.35 
Num. obs.                              635    
Num. groups: CVE_UNICA                 522    
Var: CVE_UNICA (Intercept)             414.68 
==============================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
nobs(m1_l2)
```

```
[1] 635
```

```r
confint(m1_l2)
```

```
Computing profile confidence intervals ...
```

```
Warning in vcov.merMod(object, use.hessian = use.hessian): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX

Warning in vcov.merMod(object, use.hessian = use.hessian): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Error in zeta(shiftpar, start = opt[seqpar1][-w]): profiling detected new, lower deviance
```

```r
## Two-level Null

m1_l2_null <- glmer(complied_bin ~
                      (1|CVE_UNICA),
                    data=m1df,
                    family = "binomial")

summary(m1_l2_null)
```

```
Generalized linear mixed model fit by maximum likelihood (Laplace
  Approximation) [glmerMod]
 Family: binomial  ( logit )
Formula: complied_bin ~ (1 | CVE_UNICA)
   Data: m1df

     AIC      BIC   logLik deviance df.resid 
   235.5    244.4   -115.7    231.5      633 

Scaled residuals: 
     Min       1Q   Median       3Q      Max 
-0.99211 -0.00243 -0.00243 -0.00241  1.74041 

Random effects:
 Groups    Name        Variance Std.Dev.
 CVE_UNICA (Intercept) 1516     38.93   
Number of obs: 635, groups:  CVE_UNICA, 522

Fixed effects:
            Estimate Std. Error z value            Pr(>|z|)    
(Intercept)  -12.027      1.012  -11.88 <0.0000000000000002 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
lrtest(m1_l2_null, m1_l2)
```

```
Likelihood ratio test

Model 1: complied_bin ~ (1 | CVE_UNICA)
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop + (1 | CVE_UNICA)
  #Df  LogLik Df  Chisq Pr(>Chisq)
1   2 -115.73                     
2  54 -108.35 52 14.777          1
```

```r
kable(lrtest(m1_l2_null, m1_l2))
```



| #Df|    LogLik| Df|    Chisq| Pr(>Chisq)|
|---:|---------:|--:|--------:|----------:|
|   2| -115.7342| NA|       NA|         NA|
|  54| -108.3458| 52| 14.77681|  0.9999999|

```r
lrtest(m1_null,m1_l2_null, m1_l2)
```

```
Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"
```

```
Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"
```

```
Likelihood ratio test

Model 1: complied_bin ~ 1
Model 2: complied_bin ~ (1 | CVE_UNICA)
Model 3: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop + (1 | CVE_UNICA)
  #Df  LogLik Df   Chisq          Pr(>Chisq)    
1   1 -230.60                                   
2   2 -115.73  1 229.723 <0.0000000000000002 ***
3  54 -108.35 52  14.777                   1    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(lrtest(m1_null,m1_l2_null, m1_l2))
```

```
Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"

Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"
```



| #Df|    LogLik| Df|     Chisq| Pr(>Chisq)|
|---:|---------:|--:|---------:|----------:|
|   1| -230.5957| NA|        NA|         NA|
|   2| -115.7342|  1| 229.72286|  0.0000000|
|  54| -108.3458| 52|  14.77681|  0.9999999|

```r
# compare sequentially
anova(m1_l2, m1_l2_null, test="LRT")
```

```
Data: m1df
Models:
m1_l2_null: complied_bin ~ (1 | CVE_UNICA)
m1_l2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l2:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1_l2:     bribe_victim + size + hotrestbar + yearsquant + loghoms + 
m1_l2:     logpop + (1 | CVE_UNICA)
           Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
m1_l2_null  2 235.47 244.38 -115.73   231.47                         
m1_l2      54 324.69 565.19 -108.35   216.69 14.777     52          1
```

```r
kable(anova(m1_l2, m1_l2_null, test="LRT"))
```



|           | Df|      AIC|      BIC|    logLik| deviance|    Chisq| Chi Df| Pr(>Chisq)|
|:----------|--:|--------:|--------:|---------:|--------:|--------:|------:|----------:|
|m1_l2_null |  2| 235.4685| 244.3757| -115.7342| 231.4685|       NA|     NA|         NA|
|m1_l2      | 54| 324.6917| 565.1874| -108.3458| 216.6917| 14.77681|     52|  0.9999999|

```r
anova(m1_l2, m1_l2_null, m1_null, test="LRT")
```

```
Data: m1df
Models:
m1_null: complied_bin ~ 1
m1_l2_null: complied_bin ~ (1 | CVE_UNICA)
m1_l2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l2:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1_l2:     bribe_victim + size + hotrestbar + yearsquant + loghoms + 
m1_l2:     logpop + (1 | CVE_UNICA)
           Df    AIC    BIC  logLik deviance   Chisq Chi Df
m1_null     1 463.19 467.64 -230.60   461.19               
m1_l2_null  2 235.47 244.38 -115.73   231.47 229.723      1
m1_l2      54 324.69 565.19 -108.35   216.69  14.777     52
                    Pr(>Chisq)    
m1_null                           
m1_l2_null <0.0000000000000002 ***
m1_l2                        1    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(anova(m1_l2, m1_l2_null, m1_null, test="LRT"))
```



|           | Df|      AIC|      BIC|    logLik| deviance|     Chisq| Chi Df| Pr(>Chisq)|
|:----------|--:|--------:|--------:|---------:|--------:|---------:|------:|----------:|
|m1_null    |  1| 463.1913| 467.6450| -230.5957| 461.1913|        NA|     NA|         NA|
|m1_l2_null |  2| 235.4685| 244.3757| -115.7342| 231.4685| 229.72286|      1|  0.0000000|
|m1_l2      | 54| 324.6917| 565.1874| -108.3458| 216.6917|  14.77681|     52|  0.9999999|

```r
### RMSES
m1_l2_residuals <- residuals(m1_l2, type="response")

m1_l2_rmses <- c("m1_l2",
              sqrt(mean(m1_l2_residuals^2)),
              sqrt(mean(m1_l2_residuals^2))/sd(m1_l2_residuals),
              sqrt(mean(m1_l2_residuals^2))/
                          (max(m1_l2_residuals)-min(m1_l2_residuals)))

rmses <- rbind(rmses, m1_l2_rmses)

rmses
```

```
  model               RMSE             NRMSE             CVRMSE
1    m1  0.243252928442848 0.999212288180241  0.130927789988535
2 m1_l2 0.0139962318496888  1.01926416725973 0.0937193240873146
```

```r
kable(rmses)
```



|model |RMSE               |NRMSE             |CVRMSE             |
|:-----|:------------------|:-----------------|:------------------|
|m1    |0.243252928442848  |0.999212288180241 |0.130927789988535  |
|m1_l2 |0.0139962318496888 |1.01926416725973  |0.0937193240873146 |

```r
# Plot observed vs fitted
m1_l2_ob_pred <- data.frame(Observed=m1df$complied_bin,
                         Predicted=fitted(m1_l2, type=response))

ggplot(m1_l2_ob_pred, aes(Observed, Predicted)) +
  geom_boxplot() +
  theme_bw() +
  ggtitle("Compliance with extortion demands:\nObserved vs. predicted")
```

![plot of chunk two-level-models-with-all](figure/two-level-models-with-all-1.pdf)

```r
# FOREST PLOTS?
# customize sjplots
set_theme(theme = "blank",
          geom.label.size = 0,
          axis.textsize.x = .7,
          axis.title.size = .9,
          axis.angle.x=90,
          axis.textsize.y = 0)

# Random intercepts
sjp.glmer(m1_l2, show.values = FALSE, sort.est= TRUE)
```

```
Error: Package `arm` needed for this function to work. Please install it.
```

```r
# Forest plots
set_theme(theme = "blank",
          geom.label.size = 0,
          axis.textsize.x = .7,
          axis.title.size = .9,
          axis.angle.x=90,
          axis.textsize.y = .7)
sjp.glmer(m1_l2, type="fe", show.values = FALSE)
```

```
Error: Package `arm` needed for this function to work. Please install it.
```

```r
sjp.glmer(m1_l2, type="fe", show.values = FALSE, sort.est = TRUE)
```

```
Error: Package `arm` needed for this function to work. Please install it.
```

```r
htmlreg(list(m1, m1_l2))
```

```
Warning in vcov.merMod(model, useScale = FALSE, ...): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
<caption align="bottom" style="margin-top:0.3em;">Statistical models</caption>
<tr>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b></b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 1</b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 2</b></th>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">(Intercept)</td>
<td style="padding-right: 12px; border: none;">-23.01</td>
<td style="padding-right: 12px; border: none;">-30.80</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;">(72.66)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeInternet</td>
<td style="padding-right: 12px; border: none;">3.46</td>
<td style="padding-right: 12px; border: none;">29.23</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.83)</td>
<td style="padding-right: 12px; border: none;">(52.79)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeStreet</td>
<td style="padding-right: 12px; border: none;">1.75<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">9.45</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.73)</td>
<td style="padding-right: 12px; border: none;">(11.38)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typePremises</td>
<td style="padding-right: 12px; border: none;">34.97</td>
<td style="padding-right: 12px; border: none;">119.54</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1829.78)</td>
<td style="padding-right: 12px; border: none;">(18612475.24)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeCobro de piso</td>
<td style="padding-right: 12px; border: none;">3.12<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">5.63</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.15)</td>
<td style="padding-right: 12px; border: none;">(25.84)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeOther</td>
<td style="padding-right: 12px; border: none;">2.30<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">21.88</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.80)</td>
<td style="padding-right: 12px; border: none;">(13.70)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestMoney</td>
<td style="padding-right: 12px; border: none;">19.74</td>
<td style="padding-right: 12px; border: none;">28.75</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;">(68.98)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestNothing</td>
<td style="padding-right: 12px; border: none;">19.61</td>
<td style="padding-right: 12px; border: none;">27.83</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;">(69.23)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestProduct</td>
<td style="padding-right: 12px; border: none;">20.09</td>
<td style="padding-right: 12px; border: none;">30.18</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;">(72.16)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthFebruary</td>
<td style="padding-right: 12px; border: none;">-1.50</td>
<td style="padding-right: 12px; border: none;">-30.32</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.97)</td>
<td style="padding-right: 12px; border: none;">(18.39)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthMarch</td>
<td style="padding-right: 12px; border: none;">-0.46</td>
<td style="padding-right: 12px; border: none;">-12.31</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.85)</td>
<td style="padding-right: 12px; border: none;">(15.24)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthApril</td>
<td style="padding-right: 12px; border: none;">-0.42</td>
<td style="padding-right: 12px; border: none;">-24.21</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.94)</td>
<td style="padding-right: 12px; border: none;">(18.24)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthMay</td>
<td style="padding-right: 12px; border: none;">-1.47</td>
<td style="padding-right: 12px; border: none;">-16.67</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.07)</td>
<td style="padding-right: 12px; border: none;">(24.28)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthJune</td>
<td style="padding-right: 12px; border: none;">-0.30</td>
<td style="padding-right: 12px; border: none;">-9.58</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.78)</td>
<td style="padding-right: 12px; border: none;">(12.14)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthJuly</td>
<td style="padding-right: 12px; border: none;">-0.52</td>
<td style="padding-right: 12px; border: none;">-12.79</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.94)</td>
<td style="padding-right: 12px; border: none;">(13.52)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthAugust</td>
<td style="padding-right: 12px; border: none;">-2.17</td>
<td style="padding-right: 12px; border: none;">-27.53</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.17)</td>
<td style="padding-right: 12px; border: none;">(24.69)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthSeptember</td>
<td style="padding-right: 12px; border: none;">-1.20</td>
<td style="padding-right: 12px; border: none;">-11.21</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.92)</td>
<td style="padding-right: 12px; border: none;">(12.40)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthOctober</td>
<td style="padding-right: 12px; border: none;">-2.11<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">-17.12</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.06)</td>
<td style="padding-right: 12px; border: none;">(22.54)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthNovember</td>
<td style="padding-right: 12px; border: none;">0.69</td>
<td style="padding-right: 12px; border: none;">-5.37</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.75)</td>
<td style="padding-right: 12px; border: none;">(11.36)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthDecember</td>
<td style="padding-right: 12px; border: none;">-1.75</td>
<td style="padding-right: 12px; border: none;">-18.82</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.99)</td>
<td style="padding-right: 12px; border: none;">(18.05)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthNONE</td>
<td style="padding-right: 12px; border: none;">-1.49</td>
<td style="padding-right: 12px; border: none;">-17.82</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.37)</td>
<td style="padding-right: 12px; border: none;">(33.60)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeAfternoon</td>
<td style="padding-right: 12px; border: none;">0.18</td>
<td style="padding-right: 12px; border: none;">0.57</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.41)</td>
<td style="padding-right: 12px; border: none;">(8.33)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeEvening</td>
<td style="padding-right: 12px; border: none;">-0.12</td>
<td style="padding-right: 12px; border: none;">-1.57</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
<td style="padding-right: 12px; border: none;">(11.88)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeNight</td>
<td style="padding-right: 12px; border: none;">2.30<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">2.35</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.16)</td>
<td style="padding-right: 12px; border: none;">(31.34)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeDK/DA</td>
<td style="padding-right: 12px; border: none;">2.78</td>
<td style="padding-right: 12px; border: none;">-6.34</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.79)</td>
<td style="padding-right: 12px; border: none;">(44.01)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders2</td>
<td style="padding-right: 12px; border: none;">0.83<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">0.46</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.42)</td>
<td style="padding-right: 12px; border: none;">(8.90)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders3</td>
<td style="padding-right: 12px; border: none;">0.79</td>
<td style="padding-right: 12px; border: none;">6.55</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.75)</td>
<td style="padding-right: 12px; border: none;">(14.09)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders4</td>
<td style="padding-right: 12px; border: none;">3.74<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">23.69</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.39)</td>
<td style="padding-right: 12px; border: none;">(25.54)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders5</td>
<td style="padding-right: 12px; border: none;">3.67</td>
<td style="padding-right: 12px; border: none;">6.32</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(2.02)</td>
<td style="padding-right: 12px; border: none;">(90.28)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders6+</td>
<td style="padding-right: 12px; border: none;">0.10</td>
<td style="padding-right: 12px; border: none;">-33.44</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.44)</td>
<td style="padding-right: 12px; border: none;">(21.44)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offendersDK/DA</td>
<td style="padding-right: 12px; border: none;">-0.11</td>
<td style="padding-right: 12px; border: none;">-8.46</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.84)</td>
<td style="padding-right: 12px; border: none;">(19.15)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersEmployee</td>
<td style="padding-right: 12px; border: none;">-14.96</td>
<td style="padding-right: 12px; border: none;">-87.98</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1278.54)</td>
<td style="padding-right: 12px; border: none;">(18612475.24)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersBarely known</td>
<td style="padding-right: 12px; border: none;">-0.04</td>
<td style="padding-right: 12px; border: none;">1.90</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.93)</td>
<td style="padding-right: 12px; border: none;">(14.97)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersSomewhat known</td>
<td style="padding-right: 12px; border: none;">-1.02</td>
<td style="padding-right: 12px; border: none;">-36.10</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.68)</td>
<td style="padding-right: 12px; border: none;">(37.03)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersClose acquaintance</td>
<td style="padding-right: 12px; border: none;">1.53</td>
<td style="padding-right: 12px; border: none;">3.74</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.74)</td>
<td style="padding-right: 12px; border: none;">(33.17)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersDK/DA</td>
<td style="padding-right: 12px; border: none;">-1.53</td>
<td style="padding-right: 12px; border: none;">-5.41</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.17)</td>
<td style="padding-right: 12px; border: none;">(30.28)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">had_weaponYes</td>
<td style="padding-right: 12px; border: none;">1.03</td>
<td style="padding-right: 12px; border: none;">19.09</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.02)</td>
<td style="padding-right: 12px; border: none;">(18.07)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">had_weaponDK/DA</td>
<td style="padding-right: 12px; border: none;">0.30</td>
<td style="padding-right: 12px; border: none;">-2.82</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.43)</td>
<td style="padding-right: 12px; border: none;">(9.25)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">with_violenceYes</td>
<td style="padding-right: 12px; border: none;">1.34</td>
<td style="padding-right: 12px; border: none;">3.91</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.02)</td>
<td style="padding-right: 12px; border: none;">(17.55)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">with_violenceDK/DA</td>
<td style="padding-right: 12px; border: none;">1.23</td>
<td style="padding-right: 12px; border: none;">11.36</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.71)</td>
<td style="padding-right: 12px; border: none;">(14.90)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">reportedYes</td>
<td style="padding-right: 12px; border: none;">0.64</td>
<td style="padding-right: 12px; border: none;">0.96</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.46)</td>
<td style="padding-right: 12px; border: none;">(10.58)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rep_extortion_victim1</td>
<td style="padding-right: 12px; border: none;">1.08</td>
<td style="padding-right: 12px; border: none;">3.20</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.65)</td>
<td style="padding-right: 12px; border: none;">(15.51)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">bribe_victim1</td>
<td style="padding-right: 12px; border: none;">0.12</td>
<td style="padding-right: 12px; border: none;">0.58</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.54)</td>
<td style="padding-right: 12px; border: none;">(13.33)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeMedium</td>
<td style="padding-right: 12px; border: none;">-0.27</td>
<td style="padding-right: 12px; border: none;">-8.99</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.53)</td>
<td style="padding-right: 12px; border: none;">(10.19)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeSmall</td>
<td style="padding-right: 12px; border: none;">0.92</td>
<td style="padding-right: 12px; border: none;">-0.32</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.50)</td>
<td style="padding-right: 12px; border: none;">(10.03)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeMicro</td>
<td style="padding-right: 12px; border: none;">-0.28</td>
<td style="padding-right: 12px; border: none;">-8.01</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
<td style="padding-right: 12px; border: none;">(12.50)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">hotrestbar1</td>
<td style="padding-right: 12px; border: none;">-0.94</td>
<td style="padding-right: 12px; border: none;">-4.93</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.66)</td>
<td style="padding-right: 12px; border: none;">(19.87)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(8,16]</td>
<td style="padding-right: 12px; border: none;">-0.35</td>
<td style="padding-right: 12px; border: none;">-4.30</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.59)</td>
<td style="padding-right: 12px; border: none;">(10.78)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(16,25]</td>
<td style="padding-right: 12px; border: none;">0.87</td>
<td style="padding-right: 12px; border: none;">-0.81</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.53)</td>
<td style="padding-right: 12px; border: none;">(9.96)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(25,34]</td>
<td style="padding-right: 12px; border: none;">-0.80</td>
<td style="padding-right: 12px; border: none;">-3.77</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.68)</td>
<td style="padding-right: 12px; border: none;">(14.49)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(34,43]</td>
<td style="padding-right: 12px; border: none;">1.28<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">1.05</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
<td style="padding-right: 12px; border: none;">(11.23)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">loghoms</td>
<td style="padding-right: 12px; border: none;">-0.28</td>
<td style="padding-right: 12px; border: none;">-4.03</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.26)</td>
<td style="padding-right: 12px; border: none;">(5.05)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">logpop</td>
<td style="padding-right: 12px; border: none;">0.59</td>
<td style="padding-right: 12px; border: none;">5.16</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.40)</td>
<td style="padding-right: 12px; border: none;">(8.13)</td>
</tr>
<tr>
<td style="border-top: 1px solid black;">AIC</td>
<td style="border-top: 1px solid black;">371.56</td>
<td style="border-top: 1px solid black;">324.69</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">BIC</td>
<td style="padding-right: 12px; border: none;">607.60</td>
<td style="padding-right: 12px; border: none;">565.19</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Log Likelihood</td>
<td style="padding-right: 12px; border: none;">-132.78</td>
<td style="padding-right: 12px; border: none;">-108.35</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Deviance</td>
<td style="padding-right: 12px; border: none;">265.56</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Num. obs.</td>
<td style="padding-right: 12px; border: none;">635</td>
<td style="padding-right: 12px; border: none;">635</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Num. groups: CVE_UNICA</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">522</td>
</tr>
<tr>
<td style="border-bottom: 2px solid black;">Var: CVE_UNICA (Intercept)</td>
<td style="border-bottom: 2px solid black;"></td>
<td style="border-bottom: 2px solid black;">414.68</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;" colspan="4"><span style="font-size:0.8em"><sup style="vertical-align: 0px;">***</sup>p &lt; 0.001, <sup style="vertical-align: 0px;">**</sup>p &lt; 0.01, <sup style="vertical-align: 0px;">*</sup>p &lt; 0.05</span></td>
</tr>
</table>
```

```r
screenreg(list(m1, m1_l2))
```

```
Warning in vcov.merMod(model, useScale = FALSE, ...): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```

===========================================================
                                 Model 1      Model 2      
-----------------------------------------------------------
(Intercept)                        -23.01           -30.80 
                                 (3420.48)          (72.66)
extortion_typeInternet               3.46            29.23 
                                    (1.83)          (52.79)
extortion_typeStreet                 1.75 *           9.45 
                                    (0.73)          (11.38)
extortion_typePremises              34.97           119.54 
                                 (1829.78)    (18612475.24)
extortion_typeCobro de piso          3.12 **          5.63 
                                    (1.15)          (25.84)
extortion_typeOther                  2.30 **         21.88 
                                    (0.80)          (13.70)
requestMoney                        19.74            28.75 
                                 (3420.48)          (68.98)
requestNothing                      19.61            27.83 
                                 (3420.48)          (69.23)
requestProduct                      20.09            30.18 
                                 (3420.48)          (72.16)
monthFebruary                       -1.50           -30.32 
                                    (0.97)          (18.39)
monthMarch                          -0.46           -12.31 
                                    (0.85)          (15.24)
monthApril                          -0.42           -24.21 
                                    (0.94)          (18.24)
monthMay                            -1.47           -16.67 
                                    (1.07)          (24.28)
monthJune                           -0.30            -9.58 
                                    (0.78)          (12.14)
monthJuly                           -0.52           -12.79 
                                    (0.94)          (13.52)
monthAugust                         -2.17           -27.53 
                                    (1.17)          (24.69)
monthSeptember                      -1.20           -11.21 
                                    (0.92)          (12.40)
monthOctober                        -2.11 *         -17.12 
                                    (1.06)          (22.54)
monthNovember                        0.69            -5.37 
                                    (0.75)          (11.36)
monthDecember                       -1.75           -18.82 
                                    (0.99)          (18.05)
monthNONE                           -1.49           -17.82 
                                    (1.37)          (33.60)
timeAfternoon                        0.18             0.57 
                                    (0.41)           (8.33)
timeEvening                         -0.12            -1.57 
                                    (0.57)          (11.88)
timeNight                            2.30 *           2.35 
                                    (1.16)          (31.34)
timeDK/DA                            2.78            -6.34 
                                    (1.79)          (44.01)
n_offenders2                         0.83 *           0.46 
                                    (0.42)           (8.90)
n_offenders3                         0.79             6.55 
                                    (0.75)          (14.09)
n_offenders4                         3.74 **         23.69 
                                    (1.39)          (25.54)
n_offenders5                         3.67             6.32 
                                    (2.02)          (90.28)
n_offenders6+                        0.10           -33.44 
                                    (1.44)          (21.44)
n_offendersDK/DA                    -0.11            -8.46 
                                    (0.84)          (19.15)
rel_offendersEmployee              -14.96           -87.98 
                                 (1278.54)    (18612475.24)
rel_offendersBarely known           -0.04             1.90 
                                    (0.93)          (14.97)
rel_offendersSomewhat known         -1.02           -36.10 
                                    (1.68)          (37.03)
rel_offendersClose acquaintance      1.53             3.74 
                                    (1.74)          (33.17)
rel_offendersDK/DA                  -1.53            -5.41 
                                    (1.17)          (30.28)
had_weaponYes                        1.03            19.09 
                                    (1.02)          (18.07)
had_weaponDK/DA                      0.30            -2.82 
                                    (0.43)           (9.25)
with_violenceYes                     1.34             3.91 
                                    (1.02)          (17.55)
with_violenceDK/DA                   1.23            11.36 
                                    (0.71)          (14.90)
reportedYes                          0.64             0.96 
                                    (0.46)          (10.58)
rep_extortion_victim1                1.08             3.20 
                                    (0.65)          (15.51)
bribe_victim1                        0.12             0.58 
                                    (0.54)          (13.33)
sizeMedium                          -0.27            -8.99 
                                    (0.53)          (10.19)
sizeSmall                            0.92            -0.32 
                                    (0.50)          (10.03)
sizeMicro                           -0.28            -8.01 
                                    (0.57)          (12.50)
hotrestbar1                         -0.94            -4.93 
                                    (0.66)          (19.87)
yearsquant(8,16]                    -0.35            -4.30 
                                    (0.59)          (10.78)
yearsquant(16,25]                    0.87            -0.81 
                                    (0.53)           (9.96)
yearsquant(25,34]                   -0.80            -3.77 
                                    (0.68)          (14.49)
yearsquant(34,43]                    1.28 *           1.05 
                                    (0.57)          (11.23)
loghoms                             -0.28            -4.03 
                                    (0.26)           (5.05)
logpop                               0.59             5.16 
                                    (0.40)           (8.13)
-----------------------------------------------------------
AIC                                371.56           324.69 
BIC                                607.60           565.19 
Log Likelihood                    -132.78          -108.35 
Deviance                           265.56                  
Num. obs.                          635              635    
Num. groups: CVE_UNICA                              522    
Var: CVE_UNICA (Intercept)                          414.68 
===========================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
lrtest(m1_l2, m1)
```

```
Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glmerMod", updated model is of class "glm"
```

```
Likelihood ratio test

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop + (1 | CVE_UNICA)
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
  #Df  LogLik Df  Chisq       Pr(>Chisq)    
1  54 -108.35                               
2  53 -132.78 -1 48.866 0.00000000000274 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(lrtest(m1_l2, m1))
```

```
Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glmerMod", updated model is of class "glm"
```



| #Df|    LogLik| Df|    Chisq| Pr(>Chisq)|
|---:|---------:|--:|--------:|----------:|
|  54| -108.3458| NA|       NA|         NA|
|  53| -132.7790| -1| 48.86623|          0|

```r
anova(m1_l2, m1, test="LRT")
```

```
Data: m1df
Models:
m1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1:     bribe_victim + size + hotrestbar + yearsquant + loghoms + 
m1:     logpop
m1_l2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l2:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1_l2:     bribe_victim + size + hotrestbar + yearsquant + loghoms + 
m1_l2:     logpop + (1 | CVE_UNICA)
      Df    AIC    BIC  logLik deviance  Chisq Chi Df       Pr(>Chisq)    
m1    53 371.56 607.60 -132.78   265.56                                   
m1_l2 54 324.69 565.19 -108.35   216.69 48.866      1 0.00000000000274 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(anova(m1_l2, m1, test="LRT"))
```



|      | Df|      AIC|      BIC|    logLik| deviance|    Chisq| Chi Df| Pr(>Chisq)|
|:-----|--:|--------:|--------:|---------:|--------:|--------:|------:|----------:|
|m1    | 53| 371.5579| 607.6000| -132.7790| 265.5579|       NA|     NA|         NA|
|m1_l2 | 54| 324.6917| 565.1874| -108.3458| 216.6917| 48.86623|      1|          0|

Now we try the three-level configuration


```r
m1_l3 <- glmer(complied_bin ~
            extortion_type + request +
            month + time +
            n_offenders + rel_offenders +
            had_weapon +
            with_violence +
            reported +
            rep_extortion_victim +
            bribe_victim +
            size + hotrestbar + yearsquant +
            loghoms + logpop +
            (1|CVE_UNICA) +
            (1|CVE_ENT),
          data=m1df,
          family = "binomial")
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge: degenerate Hessian with 10 negative
eigenvalues
```

```r
summary(m1_l3)
```

```
Warning in vcov.merMod(object, use.hessian = use.hessian): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```
Warning in vcov.merMod(object, correlation = correlation, sigm = sig): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```
Generalized linear mixed model fit by maximum likelihood (Laplace
  Approximation) [glmerMod]
 Family: binomial  ( logit )
Formula: 
complied_bin ~ extortion_type + request + month + time + n_offenders +  
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim +  
    bribe_victim + size + hotrestbar + yearsquant + loghoms +  
    logpop + (1 | CVE_UNICA) + (1 | CVE_ENT)
   Data: m1df

     AIC      BIC   logLik deviance df.resid 
   334.2    579.2   -112.1    224.2      580 

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-0.2700  0.0000  0.0000  0.0000  0.3206 

Random effects:
 Groups    Name        Variance Std.Dev.
 CVE_UNICA (Intercept) 605.9462 24.6160 
 CVE_ENT   (Intercept)   0.7591  0.8712 
Number of obs: 635, groups:  CVE_UNICA, 522; CVE_ENT, 32

Fixed effects:
                                      Estimate     Std. Error z value
(Intercept)                          -35.73771       98.74923  -0.362
extortion_typeInternet                61.86237       46.69424   1.325
extortion_typeStreet                  21.69003       18.49764   1.173
extortion_typePremises               574.83928 19647463.58199   0.000
extortion_typeCobro de piso           21.03016       43.15014   0.487
extortion_typeOther                   31.83477       23.63693   1.347
requestMoney                          24.12639       93.19772   0.259
requestNothing                        15.45822       92.69698   0.167
requestProduct                        21.92689       98.78786   0.222
monthFebruary                        -33.28964       23.26528  -1.431
monthMarch                           -16.58410       20.90581  -0.793
monthApril                            -9.27964       22.27293  -0.417
monthMay                             -26.29600       31.42211  -0.837
monthJune                            -13.30757       18.01759  -0.739
monthJuly                            -12.35686       22.86986  -0.540
monthAugust                          -25.77139       34.97857  -0.737
monthSeptember                       -30.02185       29.70859  -1.010
monthOctober                         -43.30212       33.46308  -1.294
monthNovember                          2.63476       16.10906   0.164
monthDecember                        -27.35119       29.21466  -0.936
monthNONE                            -28.62178       30.86357  -0.927
timeAfternoon                          5.37128       10.95604   0.490
timeEvening                           -2.72940       14.57786  -0.187
timeNight                             26.43110       41.59654   0.635
timeDK/DA                             19.50147       47.61919   0.410
n_offenders2                           5.52194       11.42394   0.483
n_offenders3                          10.12789       19.12395   0.530
n_offenders4                          45.49227       64.29624   0.708
n_offenders5                          33.75404       96.19409   0.351
n_offenders6+                        -14.92840       28.07225  -0.532
n_offendersDK/DA                     -16.86001       25.32774  -0.666
rel_offendersEmployee               -442.69262 39294927.16387   0.000
rel_offendersBarely known             -2.14171       23.23624  -0.092
rel_offendersSomewhat known          -37.71825       50.93213  -0.741
rel_offendersClose acquaintance       19.12001      102.25395   0.187
rel_offendersDK/DA                   -20.35658       39.12285  -0.520
had_weaponYes                         16.93613       28.62457   0.592
had_weaponDK/DA                        1.00991       11.84565   0.085
with_violenceYes                       0.02293       25.80279   0.001
with_violenceDK/DA                    18.49127       17.58366   1.052
reportedYes                            5.79390       15.78514   0.367
rep_extortion_victim1                 10.79962       14.94063   0.723
bribe_victim1                          3.18558       14.60942   0.218
sizeMedium                           -13.12028       12.38501  -1.059
sizeSmall                             -2.66044       13.89621  -0.192
sizeMicro                            -14.24798       16.70815  -0.853
hotrestbar1                          -14.64626       29.68114  -0.494
yearsquant(8,16]                     -16.19350       16.26835  -0.995
yearsquant(16,25]                      1.90001       15.11543   0.126
yearsquant(25,34]                    -16.91414       19.96634  -0.847
yearsquant(34,43]                     12.76828       12.44734   1.026
loghoms                               -7.90512        6.77496  -1.167
logpop                                14.58304       11.19389   1.303
                                Pr(>|z|)
(Intercept)                        0.717
extortion_typeInternet             0.185
extortion_typeStreet               0.241
extortion_typePremises             1.000
extortion_typeCobro de piso        0.626
extortion_typeOther                0.178
requestMoney                       0.796
requestNothing                     0.868
requestProduct                     0.824
monthFebruary                      0.152
monthMarch                         0.428
monthApril                         0.677
monthMay                           0.403
monthJune                          0.460
monthJuly                          0.589
monthAugust                        0.461
monthSeptember                     0.312
monthOctober                       0.196
monthNovember                      0.870
monthDecember                      0.349
monthNONE                          0.354
timeAfternoon                      0.624
timeEvening                        0.851
timeNight                          0.525
timeDK/DA                          0.682
n_offenders2                       0.629
n_offenders3                       0.596
n_offenders4                       0.479
n_offenders5                       0.726
n_offenders6+                      0.595
n_offendersDK/DA                   0.506
rel_offendersEmployee              1.000
rel_offendersBarely known          0.927
rel_offendersSomewhat known        0.459
rel_offendersClose acquaintance    0.852
rel_offendersDK/DA                 0.603
had_weaponYes                      0.554
had_weaponDK/DA                    0.932
with_violenceYes                   0.999
with_violenceDK/DA                 0.293
reportedYes                        0.714
rep_extortion_victim1              0.470
bribe_victim1                      0.827
sizeMedium                         0.289
sizeSmall                          0.848
sizeMicro                          0.394
hotrestbar1                        0.622
yearsquant(8,16]                   0.320
yearsquant(16,25]                  0.900
yearsquant(25,34]                  0.397
yearsquant(34,43]                  0.305
loghoms                            0.243
logpop                             0.193
```

```

Correlation matrix not shown by default, as p = 53 > 12.
Use print(x, correlation=TRUE)  or
	 vcov(x)	 if you need it
```

```
convergence code: 0
unable to evaluate scaled gradient
Model failed to converge: degenerate  Hessian with 10 negative eigenvalues
failure to converge in 10000 evaluations
```

```r
### ICC for level 2
sum(get_re_var(m1_l3)) / (sum(get_re_var(m1_l3)) +
                        get_re_var(m1_l3, "sigma_2"))
```

```
[1] 0.9946067
```

```r
### ICC for level 3
get_re_var(m1_l3)[2] /( sum(get_re_var(m1_l3)) +
                        get_re_var(m1_l3, "sigma_2"))
```

```
    CVE_ENT 
0.001244358 
```

```r
print(icc(m1_l3), comp="var")
```

```
Generalized linear mixed model
 Family: binomial (logit)
Formula: complied_bin ~ extortion_type + request + month + time + n_offenders + rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + bribe_victim + size + hotrestbar + yearsquant + loghoms + logpop + (1 | CVE_UNICA) + (1 | CVE_ENT)

      Within-group-variance:    3.290
     Between-group-variance:  605.946 (CVE_UNICA)
     Between-group-variance:    0.759 (CVE_ENT)
```

```r
## Describe data structure

## number of observations per individuals

length(unique(m1df$CVE_UNICA))
```

```
[1] 522
```

```r
table(table(m1df$CVE_UNICA))
```

```

  1   2   3   4   5 
451  43  19   4   5 
```

```r
min(table(m1df$CVE_UNICA))
```

```
[1] 1
```

```r
max(table(m1df$CVE_UNICA))
```

```
[1] 5
```

```r
mean(table(m1df$CVE_UNICA))
```

```
[1] 1.216475
```

```r
sum(table(m1df$CVE_UNICA))
```

```
[1] 635
```

```r
# number of observations per state
length(unique(m1df$CVE_ENT))
```

```
[1] 32
```

```r
table(m1df$CVE_ENT)
```

```

 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 
27 16 14 18 19 23 20 17 26 17 17 18 23 29 17 22 13 13 18 22 26 21 15 22 14 
26 27 28 29 30 31 32 
29 26 13 16 22 12 30 
```

```r
sum(table(m1df$CVE_ENT))
```

```
[1] 635
```

```r
min(table(m1df$CVE_ENT))
```

```
[1] 12
```

```r
max(table(m1df$CVE_ENT))
```

```
[1] 30
```

```r
mean(table(m1df$CVE_ENT))
```

```
[1] 19.84375
```

```r
# number of individuals per state

IperG <- with(m1df, tapply(CVE_UNICA, CVE_ENT,
                           FUN = function(x) length(unique(x))))
sum(IperG)
```

```
[1] 522
```

```r
min(IperG)
```

```
[1] 11
```

```r
max(IperG)
```

```
[1] 24
```

```r
mean(IperG)
```

```
[1] 16.3125
```

```r
## print m1_l3
htmlreg(m1_l3)
```

```
Warning in vcov.merMod(model, useScale = FALSE, ...): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
<caption align="bottom" style="margin-top:0.3em;">Statistical models</caption>
<tr>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b></b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 1</b></th>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">(Intercept)</td>
<td style="padding-right: 12px; border: none;">-35.74</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(98.75)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeInternet</td>
<td style="padding-right: 12px; border: none;">61.86</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(46.69)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeStreet</td>
<td style="padding-right: 12px; border: none;">21.69</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(18.50)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typePremises</td>
<td style="padding-right: 12px; border: none;">574.84</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(19647463.58)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeCobro de piso</td>
<td style="padding-right: 12px; border: none;">21.03</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(43.15)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeOther</td>
<td style="padding-right: 12px; border: none;">31.83</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(23.64)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestMoney</td>
<td style="padding-right: 12px; border: none;">24.13</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(93.20)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestNothing</td>
<td style="padding-right: 12px; border: none;">15.46</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(92.70)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestProduct</td>
<td style="padding-right: 12px; border: none;">21.93</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(98.79)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthFebruary</td>
<td style="padding-right: 12px; border: none;">-33.29</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(23.27)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthMarch</td>
<td style="padding-right: 12px; border: none;">-16.58</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(20.91)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthApril</td>
<td style="padding-right: 12px; border: none;">-9.28</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(22.27)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthMay</td>
<td style="padding-right: 12px; border: none;">-26.30</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(31.42)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthJune</td>
<td style="padding-right: 12px; border: none;">-13.31</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(18.02)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthJuly</td>
<td style="padding-right: 12px; border: none;">-12.36</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(22.87)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthAugust</td>
<td style="padding-right: 12px; border: none;">-25.77</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(34.98)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthSeptember</td>
<td style="padding-right: 12px; border: none;">-30.02</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(29.71)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthOctober</td>
<td style="padding-right: 12px; border: none;">-43.30</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(33.46)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthNovember</td>
<td style="padding-right: 12px; border: none;">2.63</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(16.11)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthDecember</td>
<td style="padding-right: 12px; border: none;">-27.35</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(29.21)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthNONE</td>
<td style="padding-right: 12px; border: none;">-28.62</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(30.86)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeAfternoon</td>
<td style="padding-right: 12px; border: none;">5.37</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(10.96)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeEvening</td>
<td style="padding-right: 12px; border: none;">-2.73</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(14.58)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeNight</td>
<td style="padding-right: 12px; border: none;">26.43</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(41.60)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeDK/DA</td>
<td style="padding-right: 12px; border: none;">19.50</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(47.62)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders2</td>
<td style="padding-right: 12px; border: none;">5.52</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(11.42)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders3</td>
<td style="padding-right: 12px; border: none;">10.13</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(19.12)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders4</td>
<td style="padding-right: 12px; border: none;">45.49</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(64.30)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders5</td>
<td style="padding-right: 12px; border: none;">33.75</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(96.19)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders6+</td>
<td style="padding-right: 12px; border: none;">-14.93</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(28.07)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offendersDK/DA</td>
<td style="padding-right: 12px; border: none;">-16.86</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(25.33)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersEmployee</td>
<td style="padding-right: 12px; border: none;">-442.69</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(39294927.16)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersBarely known</td>
<td style="padding-right: 12px; border: none;">-2.14</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(23.24)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersSomewhat known</td>
<td style="padding-right: 12px; border: none;">-37.72</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(50.93)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersClose acquaintance</td>
<td style="padding-right: 12px; border: none;">19.12</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(102.25)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersDK/DA</td>
<td style="padding-right: 12px; border: none;">-20.36</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(39.12)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">had_weaponYes</td>
<td style="padding-right: 12px; border: none;">16.94</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(28.62)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">had_weaponDK/DA</td>
<td style="padding-right: 12px; border: none;">1.01</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(11.85)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">with_violenceYes</td>
<td style="padding-right: 12px; border: none;">0.02</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(25.80)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">with_violenceDK/DA</td>
<td style="padding-right: 12px; border: none;">18.49</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(17.58)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">reportedYes</td>
<td style="padding-right: 12px; border: none;">5.79</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(15.79)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rep_extortion_victim1</td>
<td style="padding-right: 12px; border: none;">10.80</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(14.94)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">bribe_victim1</td>
<td style="padding-right: 12px; border: none;">3.19</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(14.61)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeMedium</td>
<td style="padding-right: 12px; border: none;">-13.12</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(12.39)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeSmall</td>
<td style="padding-right: 12px; border: none;">-2.66</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(13.90)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeMicro</td>
<td style="padding-right: 12px; border: none;">-14.25</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(16.71)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">hotrestbar1</td>
<td style="padding-right: 12px; border: none;">-14.65</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(29.68)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(8,16]</td>
<td style="padding-right: 12px; border: none;">-16.19</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(16.27)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(16,25]</td>
<td style="padding-right: 12px; border: none;">1.90</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(15.12)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(25,34]</td>
<td style="padding-right: 12px; border: none;">-16.91</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(19.97)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(34,43]</td>
<td style="padding-right: 12px; border: none;">12.77</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(12.45)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">loghoms</td>
<td style="padding-right: 12px; border: none;">-7.91</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(6.77)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">logpop</td>
<td style="padding-right: 12px; border: none;">14.58</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(11.19)</td>
</tr>
<tr>
<td style="border-top: 1px solid black;">AIC</td>
<td style="border-top: 1px solid black;">334.20</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">BIC</td>
<td style="padding-right: 12px; border: none;">579.15</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Log Likelihood</td>
<td style="padding-right: 12px; border: none;">-112.10</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Num. obs.</td>
<td style="padding-right: 12px; border: none;">635</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Num. groups: CVE_UNICA</td>
<td style="padding-right: 12px; border: none;">522</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Num. groups: CVE_ENT</td>
<td style="padding-right: 12px; border: none;">32</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Var: CVE_UNICA (Intercept)</td>
<td style="padding-right: 12px; border: none;">605.95</td>
</tr>
<tr>
<td style="border-bottom: 2px solid black;">Var: CVE_ENT (Intercept)</td>
<td style="border-bottom: 2px solid black;">0.76</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;" colspan="3"><span style="font-size:0.8em"><sup style="vertical-align: 0px;">***</sup>p &lt; 0.001, <sup style="vertical-align: 0px;">**</sup>p &lt; 0.01, <sup style="vertical-align: 0px;">*</sup>p &lt; 0.05</span></td>
</tr>
</table>
```

```r
screenreg(m1_l3)
```

```
Warning in vcov.merMod(model, useScale = FALSE, ...): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```

==============================================
                                 Model 1      
----------------------------------------------
(Intercept)                            -35.74 
                                       (98.75)
extortion_typeInternet                  61.86 
                                       (46.69)
extortion_typeStreet                    21.69 
                                       (18.50)
extortion_typePremises                 574.84 
                                 (19647463.58)
extortion_typeCobro de piso             21.03 
                                       (43.15)
extortion_typeOther                     31.83 
                                       (23.64)
requestMoney                            24.13 
                                       (93.20)
requestNothing                          15.46 
                                       (92.70)
requestProduct                          21.93 
                                       (98.79)
monthFebruary                          -33.29 
                                       (23.27)
monthMarch                             -16.58 
                                       (20.91)
monthApril                              -9.28 
                                       (22.27)
monthMay                               -26.30 
                                       (31.42)
monthJune                              -13.31 
                                       (18.02)
monthJuly                              -12.36 
                                       (22.87)
monthAugust                            -25.77 
                                       (34.98)
monthSeptember                         -30.02 
                                       (29.71)
monthOctober                           -43.30 
                                       (33.46)
monthNovember                            2.63 
                                       (16.11)
monthDecember                          -27.35 
                                       (29.21)
monthNONE                              -28.62 
                                       (30.86)
timeAfternoon                            5.37 
                                       (10.96)
timeEvening                             -2.73 
                                       (14.58)
timeNight                               26.43 
                                       (41.60)
timeDK/DA                               19.50 
                                       (47.62)
n_offenders2                             5.52 
                                       (11.42)
n_offenders3                            10.13 
                                       (19.12)
n_offenders4                            45.49 
                                       (64.30)
n_offenders5                            33.75 
                                       (96.19)
n_offenders6+                          -14.93 
                                       (28.07)
n_offendersDK/DA                       -16.86 
                                       (25.33)
rel_offendersEmployee                 -442.69 
                                 (39294927.16)
rel_offendersBarely known               -2.14 
                                       (23.24)
rel_offendersSomewhat known            -37.72 
                                       (50.93)
rel_offendersClose acquaintance         19.12 
                                      (102.25)
rel_offendersDK/DA                     -20.36 
                                       (39.12)
had_weaponYes                           16.94 
                                       (28.62)
had_weaponDK/DA                          1.01 
                                       (11.85)
with_violenceYes                         0.02 
                                       (25.80)
with_violenceDK/DA                      18.49 
                                       (17.58)
reportedYes                              5.79 
                                       (15.79)
rep_extortion_victim1                   10.80 
                                       (14.94)
bribe_victim1                            3.19 
                                       (14.61)
sizeMedium                             -13.12 
                                       (12.39)
sizeSmall                               -2.66 
                                       (13.90)
sizeMicro                              -14.25 
                                       (16.71)
hotrestbar1                            -14.65 
                                       (29.68)
yearsquant(8,16]                       -16.19 
                                       (16.27)
yearsquant(16,25]                        1.90 
                                       (15.12)
yearsquant(25,34]                      -16.91 
                                       (19.97)
yearsquant(34,43]                       12.77 
                                       (12.45)
loghoms                                 -7.91 
                                        (6.77)
logpop                                  14.58 
                                       (11.19)
----------------------------------------------
AIC                                    334.20 
BIC                                    579.15 
Log Likelihood                        -112.10 
Num. obs.                              635    
Num. groups: CVE_UNICA                 522    
Num. groups: CVE_ENT                    32    
Var: CVE_UNICA (Intercept)             605.95 
Var: CVE_ENT (Intercept)                 0.76 
==============================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
nobs(m1_l3)
```

```
[1] 635
```

```r
confint(m1_l3)
```

```
Computing profile confidence intervals ...
```

```
Warning in vcov.merMod(object, use.hessian = use.hessian): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX

Warning in vcov.merMod(object, use.hessian = use.hessian): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Error in zeta(shiftpar, start = opt[seqpar1][-w]): profiling detected new, lower deviance
```

```r
# droptest
m1_l3_dropped <- drop1(m1_l3, test="Chisq")
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge: degenerate Hessian with 1 negative
eigenvalues
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge: degenerate Hessian with 11 negative
eigenvalues
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge: degenerate Hessian with 1 negative
eigenvalues
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge: degenerate Hessian with 6 negative
eigenvalues
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge: degenerate Hessian with 1 negative
eigenvalues
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge: degenerate Hessian with 4 negative
eigenvalues
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge: degenerate Hessian with 8 negative
eigenvalues
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge: degenerate Hessian with 3 negative
eigenvalues
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge: degenerate Hessian with 5 negative
eigenvalues
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge: degenerate Hessian with 8 negative
eigenvalues
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge: degenerate Hessian with 6 negative
eigenvalues
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge: degenerate Hessian with 2 negative
eigenvalues
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge: degenerate Hessian with 7 negative
eigenvalues
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge: degenerate Hessian with 3 negative
eigenvalues
```

```
Error in pwrssUpdate(pp, resp, tol = tolPwrss, GQmat = GQmat, compDev = compDev, : Downdated VtV is not positive definite
```

```r
kable(m1_l3_dropped)
```

```
Error in inherits(x, "list"): object 'm1_l3_dropped' not found
```

```r
## Three-level Null

m1_l3_null <- glmer(complied_bin ~
                      (1|CVE_UNICA) +
                      (1|CVE_ENT),
                    data=m1df,
                    family = "binomial")

summary(m1_l3_null)
```

```
Generalized linear mixed model fit by maximum likelihood (Laplace
  Approximation) [glmerMod]
 Family: binomial  ( logit )
Formula: complied_bin ~ (1 | CVE_UNICA) + (1 | CVE_ENT)
   Data: m1df

     AIC      BIC   logLik deviance df.resid 
   237.5    250.8   -115.7    231.5      632 

Scaled residuals: 
     Min       1Q   Median       3Q      Max 
-0.99211 -0.00243 -0.00243 -0.00241  1.74041 

Random effects:
 Groups    Name        Variance Std.Dev.
 CVE_UNICA (Intercept) 1516     38.93   
 CVE_ENT   (Intercept)    0      0.00   
Number of obs: 635, groups:  CVE_UNICA, 522; CVE_ENT, 32

Fixed effects:
            Estimate Std. Error z value            Pr(>|z|)    
(Intercept)  -12.027      1.011  -11.89 <0.0000000000000002 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
lrtest(m1_l3_null, m1_l3)
```

```
Likelihood ratio test

Model 1: complied_bin ~ (1 | CVE_UNICA) + (1 | CVE_ENT)
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop + (1 | CVE_UNICA) + (1 | CVE_ENT)
  #Df  LogLik Df  Chisq Pr(>Chisq)
1   3 -115.73                     
2  55 -112.10 52 7.2661          1
```

```r
kable(lrtest(m1_l3_null, m1_l3))
```



| #Df|    LogLik| Df|    Chisq| Pr(>Chisq)|
|---:|---------:|--:|--------:|----------:|
|   3| -115.7342| NA|       NA|         NA|
|  55| -112.1012| 52| 7.266051|          1|

```r
lrtest(m1_null,m1_l2_null,m1_l3_null, m1_l3)
```

```
Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"
```

```
Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"

Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"
```

```
Likelihood ratio test

Model 1: complied_bin ~ 1
Model 2: complied_bin ~ (1 | CVE_UNICA)
Model 3: complied_bin ~ (1 | CVE_UNICA) + (1 | CVE_ENT)
Model 4: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop + (1 | CVE_UNICA) + (1 | CVE_ENT)
  #Df  LogLik Df    Chisq          Pr(>Chisq)    
1   1 -230.60                                    
2   2 -115.73  1 229.7229 <0.0000000000000002 ***
3   3 -115.73  1   0.0000              0.9995    
4  55 -112.10 52   7.2661              1.0000    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(lrtest(m1_null,m1_l2_null,m1_l3_null, m1_l3))
```

```
Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"

Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"

Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"
```



| #Df|    LogLik| Df|       Chisq| Pr(>Chisq)|
|---:|---------:|--:|-----------:|----------:|
|   1| -230.5957| NA|          NA|         NA|
|   2| -115.7342|  1| 229.7228560|  0.0000000|
|   3| -115.7342|  1|   0.0000004|  0.9995208|
|  55| -112.1012| 52|   7.2660514|  1.0000000|

```r
lrtest(m1_null,m1_l2_null,m1_l3_null, m1_l3)
```

```
Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"

Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"

Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"
```

```
Likelihood ratio test

Model 1: complied_bin ~ 1
Model 2: complied_bin ~ (1 | CVE_UNICA)
Model 3: complied_bin ~ (1 | CVE_UNICA) + (1 | CVE_ENT)
Model 4: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop + (1 | CVE_UNICA) + (1 | CVE_ENT)
  #Df  LogLik Df    Chisq          Pr(>Chisq)    
1   1 -230.60                                    
2   2 -115.73  1 229.7229 <0.0000000000000002 ***
3   3 -115.73  1   0.0000              0.9995    
4  55 -112.10 52   7.2661              1.0000    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(lrtest(m1_null,m1_l2_null,m1_l3_null, m1_l3))
```

```
Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"

Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"

Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"
```



| #Df|    LogLik| Df|       Chisq| Pr(>Chisq)|
|---:|---------:|--:|-----------:|----------:|
|   1| -230.5957| NA|          NA|         NA|
|   2| -115.7342|  1| 229.7228560|  0.0000000|
|   3| -115.7342|  1|   0.0000004|  0.9995208|
|  55| -112.1012| 52|   7.2660514|  1.0000000|

```r
lrtest(m1_null,m1_l2_null,m1_l3_null)
```

```
Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"

Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"
```

```
Likelihood ratio test

Model 1: complied_bin ~ 1
Model 2: complied_bin ~ (1 | CVE_UNICA)
Model 3: complied_bin ~ (1 | CVE_UNICA) + (1 | CVE_ENT)
  #Df  LogLik Df  Chisq          Pr(>Chisq)    
1   1 -230.60                                  
2   2 -115.73  1 229.72 <0.0000000000000002 ***
3   3 -115.73  1   0.00              0.9995    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(lrtest(m1_null,m1_l2_null,m1_l3_null))
```

```
Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"

Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glm", updated model is of class "glmerMod"
```



| #Df|    LogLik| Df|       Chisq| Pr(>Chisq)|
|---:|---------:|--:|-----------:|----------:|
|   1| -230.5957| NA|          NA|         NA|
|   2| -115.7342|  1| 229.7228560|  0.0000000|
|   3| -115.7342|  1|   0.0000004|  0.9995208|

```r
# compare sequentially
anova(m1_l3, m1_l3_null, test="LRT")
```

```
Data: m1df
Models:
m1_l3_null: complied_bin ~ (1 | CVE_UNICA) + (1 | CVE_ENT)
m1_l3: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l3:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1_l3:     bribe_victim + size + hotrestbar + yearsquant + loghoms + 
m1_l3:     logpop + (1 | CVE_UNICA) + (1 | CVE_ENT)
           Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
m1_l3_null  3 237.47 250.83 -115.73   231.47                         
m1_l3      55 334.20 579.15 -112.10   224.20 7.2661     52          1
```

```r
kable(anova(m1_l3, m1_l3_null, test="LRT"))
```



|           | Df|      AIC|      BIC|    logLik| deviance|    Chisq| Chi Df| Pr(>Chisq)|
|:----------|--:|--------:|--------:|---------:|--------:|--------:|------:|----------:|
|m1_l3_null |  3| 237.4685| 250.8294| -115.7342| 231.4685|       NA|     NA|         NA|
|m1_l3      | 55| 334.2024| 579.1518| -112.1012| 224.2024| 7.266051|     52|          1|

```r
anova(m1_l3, m1_l3_null, m1_null, test="LRT")
```

```
Data: m1df
Models:
m1_null: complied_bin ~ 1
m1_l3_null: complied_bin ~ (1 | CVE_UNICA) + (1 | CVE_ENT)
m1_l3: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l3:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1_l3:     bribe_victim + size + hotrestbar + yearsquant + loghoms + 
m1_l3:     logpop + (1 | CVE_UNICA) + (1 | CVE_ENT)
           Df    AIC    BIC  logLik deviance    Chisq Chi Df
m1_null     1 463.19 467.64 -230.60   461.19                
m1_l3_null  3 237.47 250.83 -115.73   231.47 229.7229      2
m1_l3      55 334.20 579.15 -112.10   224.20   7.2661     52
                    Pr(>Chisq)    
m1_null                           
m1_l3_null <0.0000000000000002 ***
m1_l3                        1    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(anova(m1_l3, m1_l3_null, m1_null, test="LRT"))
```



|           | Df|      AIC|      BIC|    logLik| deviance|      Chisq| Chi Df| Pr(>Chisq)|
|:----------|--:|--------:|--------:|---------:|--------:|----------:|------:|----------:|
|m1_null    |  1| 463.1913| 467.6450| -230.5957| 461.1913|         NA|     NA|         NA|
|m1_l3_null |  3| 237.4685| 250.8294| -115.7342| 231.4685| 229.722856|      2|          0|
|m1_l3      | 55| 334.2024| 579.1518| -112.1012| 224.2024|   7.266051|     52|          1|

```r
anova(m1_l3_null, m1_null, test="LRT")
```

```
Data: m1df
Models:
m1_null: complied_bin ~ 1
m1_l3_null: complied_bin ~ (1 | CVE_UNICA) + (1 | CVE_ENT)
           Df    AIC    BIC  logLik deviance  Chisq Chi Df
m1_null     1 463.19 467.64 -230.60   461.19              
m1_l3_null  3 237.47 250.83 -115.73   231.47 229.72      2
                      Pr(>Chisq)    
m1_null                             
m1_l3_null < 0.00000000000000022 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(anova(m1_l3_null, m1_null, test="LRT"))
```



|           | Df|      AIC|      BIC|    logLik| deviance|    Chisq| Chi Df| Pr(>Chisq)|
|:----------|--:|--------:|--------:|---------:|--------:|--------:|------:|----------:|
|m1_null    |  1| 463.1913| 467.6450| -230.5957| 461.1913|       NA|     NA|         NA|
|m1_l3_null |  3| 237.4685| 250.8294| -115.7342| 231.4685| 229.7229|      2|          0|

```r
anova(m1_l3_null, m1_l2_null, m1_null, test="LRT")
```

```
Data: m1df
Models:
m1_null: complied_bin ~ 1
m1_l2_null: complied_bin ~ (1 | CVE_UNICA)
m1_l3_null: complied_bin ~ (1 | CVE_UNICA) + (1 | CVE_ENT)
           Df    AIC    BIC  logLik deviance  Chisq Chi Df
m1_null     1 463.19 467.64 -230.60   461.19              
m1_l2_null  2 235.47 244.38 -115.73   231.47 229.72      1
m1_l3_null  3 237.47 250.83 -115.73   231.47   0.00      1
                    Pr(>Chisq)    
m1_null                           
m1_l2_null <0.0000000000000002 ***
m1_l3_null              0.9995    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(anova(m1_l3_null, m1_l2_null, m1_null, test="LRT"))
```



|           | Df|      AIC|      BIC|    logLik| deviance|       Chisq| Chi Df| Pr(>Chisq)|
|:----------|--:|--------:|--------:|---------:|--------:|-----------:|------:|----------:|
|m1_null    |  1| 463.1913| 467.6450| -230.5957| 461.1913|          NA|     NA|         NA|
|m1_l2_null |  2| 235.4685| 244.3757| -115.7342| 231.4685| 229.7228560|      1|  0.0000000|
|m1_l3_null |  3| 237.4685| 250.8294| -115.7342| 231.4685|   0.0000004|      1|  0.9995208|

```r
### RMSES
m1_l3_residuals <- residuals(m1_l3, type="response")

m1_l3_rmses <- c("m1_l3",
              sqrt(mean(m1_l3_residuals^2)),
              sqrt(mean(m1_l3_residuals^2))/sd(m1_l3_residuals),
              sqrt(mean(m1_l3_residuals^2))/
                          (max(m1_l3_residuals)-min(m1_l3_residuals)))

rmses <- rbind(rmses, m1_l3_rmses)

rmses
```

```
  model               RMSE             NRMSE             CVRMSE
1    m1  0.243252928442848 0.999212288180241  0.130927789988535
2 m1_l2 0.0139962318496888  1.01926416725973 0.0937193240873146
3 m1_l3 0.0118545830991087  1.01352194762789 0.0735666060929421
```

```r
kable(rmses)
```



|model |RMSE               |NRMSE             |CVRMSE             |
|:-----|:------------------|:-----------------|:------------------|
|m1    |0.243252928442848  |0.999212288180241 |0.130927789988535  |
|m1_l2 |0.0139962318496888 |1.01926416725973  |0.0937193240873146 |
|m1_l3 |0.0118545830991087 |1.01352194762789  |0.0735666060929421 |

```r
# Plot observed vs fitted
m1_l3_ob_pred <- data.frame(Observed=m1df$complied_bin,
                         Predicted=fitted(m1_l3, type=response))

ggplot(m1_l3_ob_pred, aes(Observed, Predicted)) +
  geom_boxplot() +
  theme_bw() +
  ggtitle("Compliance with extortion demands:\nObserved vs. predicted")
```

![plot of chunk three-level-with-all](figure/three-level-with-all-1.pdf)

```r
# FOREST PLOTS?
# customize sjplots
set_theme(theme = "blank",
          geom.label.size = 0,
          axis.textsize.x = .7,
          axis.title.size = .9,
          axis.angle.x=90,
          axis.textsize.y = 0)

# Random intercepts
sjp.glmer(m1_l3, show.values = FALSE, sort.est= TRUE)
```

```
Error: Package `arm` needed for this function to work. Please install it.
```

```r
# Forest plots
set_theme(theme = "blank",
          geom.label.size = 0,
          axis.textsize.x = .7,
          axis.title.size = .9,
          axis.angle.x=90,
          axis.textsize.y = .7)
sjp.glmer(m1_l3, type="fe", show.values = FALSE)
```

```
Error: Package `arm` needed for this function to work. Please install it.
```

```r
sjp.glmer(m1_l3, type="fe", show.values = FALSE, sort.est = TRUE)
```

```
Error: Package `arm` needed for this function to work. Please install it.
```

```r
htmlreg(list(m1, m1_l3))
```

```
Warning in vcov.merMod(model, useScale = FALSE, ...): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
<caption align="bottom" style="margin-top:0.3em;">Statistical models</caption>
<tr>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b></b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 1</b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 2</b></th>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">(Intercept)</td>
<td style="padding-right: 12px; border: none;">-23.01</td>
<td style="padding-right: 12px; border: none;">-35.74</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;">(98.75)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeInternet</td>
<td style="padding-right: 12px; border: none;">3.46</td>
<td style="padding-right: 12px; border: none;">61.86</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.83)</td>
<td style="padding-right: 12px; border: none;">(46.69)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeStreet</td>
<td style="padding-right: 12px; border: none;">1.75<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">21.69</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.73)</td>
<td style="padding-right: 12px; border: none;">(18.50)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typePremises</td>
<td style="padding-right: 12px; border: none;">34.97</td>
<td style="padding-right: 12px; border: none;">574.84</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1829.78)</td>
<td style="padding-right: 12px; border: none;">(19647463.58)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeCobro de piso</td>
<td style="padding-right: 12px; border: none;">3.12<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">21.03</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.15)</td>
<td style="padding-right: 12px; border: none;">(43.15)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeOther</td>
<td style="padding-right: 12px; border: none;">2.30<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">31.83</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.80)</td>
<td style="padding-right: 12px; border: none;">(23.64)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestMoney</td>
<td style="padding-right: 12px; border: none;">19.74</td>
<td style="padding-right: 12px; border: none;">24.13</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;">(93.20)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestNothing</td>
<td style="padding-right: 12px; border: none;">19.61</td>
<td style="padding-right: 12px; border: none;">15.46</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;">(92.70)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestProduct</td>
<td style="padding-right: 12px; border: none;">20.09</td>
<td style="padding-right: 12px; border: none;">21.93</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;">(98.79)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthFebruary</td>
<td style="padding-right: 12px; border: none;">-1.50</td>
<td style="padding-right: 12px; border: none;">-33.29</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.97)</td>
<td style="padding-right: 12px; border: none;">(23.27)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthMarch</td>
<td style="padding-right: 12px; border: none;">-0.46</td>
<td style="padding-right: 12px; border: none;">-16.58</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.85)</td>
<td style="padding-right: 12px; border: none;">(20.91)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthApril</td>
<td style="padding-right: 12px; border: none;">-0.42</td>
<td style="padding-right: 12px; border: none;">-9.28</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.94)</td>
<td style="padding-right: 12px; border: none;">(22.27)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthMay</td>
<td style="padding-right: 12px; border: none;">-1.47</td>
<td style="padding-right: 12px; border: none;">-26.30</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.07)</td>
<td style="padding-right: 12px; border: none;">(31.42)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthJune</td>
<td style="padding-right: 12px; border: none;">-0.30</td>
<td style="padding-right: 12px; border: none;">-13.31</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.78)</td>
<td style="padding-right: 12px; border: none;">(18.02)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthJuly</td>
<td style="padding-right: 12px; border: none;">-0.52</td>
<td style="padding-right: 12px; border: none;">-12.36</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.94)</td>
<td style="padding-right: 12px; border: none;">(22.87)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthAugust</td>
<td style="padding-right: 12px; border: none;">-2.17</td>
<td style="padding-right: 12px; border: none;">-25.77</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.17)</td>
<td style="padding-right: 12px; border: none;">(34.98)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthSeptember</td>
<td style="padding-right: 12px; border: none;">-1.20</td>
<td style="padding-right: 12px; border: none;">-30.02</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.92)</td>
<td style="padding-right: 12px; border: none;">(29.71)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthOctober</td>
<td style="padding-right: 12px; border: none;">-2.11<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">-43.30</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.06)</td>
<td style="padding-right: 12px; border: none;">(33.46)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthNovember</td>
<td style="padding-right: 12px; border: none;">0.69</td>
<td style="padding-right: 12px; border: none;">2.63</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.75)</td>
<td style="padding-right: 12px; border: none;">(16.11)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthDecember</td>
<td style="padding-right: 12px; border: none;">-1.75</td>
<td style="padding-right: 12px; border: none;">-27.35</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.99)</td>
<td style="padding-right: 12px; border: none;">(29.21)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthNONE</td>
<td style="padding-right: 12px; border: none;">-1.49</td>
<td style="padding-right: 12px; border: none;">-28.62</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.37)</td>
<td style="padding-right: 12px; border: none;">(30.86)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeAfternoon</td>
<td style="padding-right: 12px; border: none;">0.18</td>
<td style="padding-right: 12px; border: none;">5.37</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.41)</td>
<td style="padding-right: 12px; border: none;">(10.96)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeEvening</td>
<td style="padding-right: 12px; border: none;">-0.12</td>
<td style="padding-right: 12px; border: none;">-2.73</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
<td style="padding-right: 12px; border: none;">(14.58)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeNight</td>
<td style="padding-right: 12px; border: none;">2.30<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">26.43</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.16)</td>
<td style="padding-right: 12px; border: none;">(41.60)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeDK/DA</td>
<td style="padding-right: 12px; border: none;">2.78</td>
<td style="padding-right: 12px; border: none;">19.50</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.79)</td>
<td style="padding-right: 12px; border: none;">(47.62)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders2</td>
<td style="padding-right: 12px; border: none;">0.83<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">5.52</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.42)</td>
<td style="padding-right: 12px; border: none;">(11.42)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders3</td>
<td style="padding-right: 12px; border: none;">0.79</td>
<td style="padding-right: 12px; border: none;">10.13</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.75)</td>
<td style="padding-right: 12px; border: none;">(19.12)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders4</td>
<td style="padding-right: 12px; border: none;">3.74<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">45.49</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.39)</td>
<td style="padding-right: 12px; border: none;">(64.30)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders5</td>
<td style="padding-right: 12px; border: none;">3.67</td>
<td style="padding-right: 12px; border: none;">33.75</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(2.02)</td>
<td style="padding-right: 12px; border: none;">(96.19)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders6+</td>
<td style="padding-right: 12px; border: none;">0.10</td>
<td style="padding-right: 12px; border: none;">-14.93</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.44)</td>
<td style="padding-right: 12px; border: none;">(28.07)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offendersDK/DA</td>
<td style="padding-right: 12px; border: none;">-0.11</td>
<td style="padding-right: 12px; border: none;">-16.86</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.84)</td>
<td style="padding-right: 12px; border: none;">(25.33)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersEmployee</td>
<td style="padding-right: 12px; border: none;">-14.96</td>
<td style="padding-right: 12px; border: none;">-442.69</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1278.54)</td>
<td style="padding-right: 12px; border: none;">(39294927.16)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersBarely known</td>
<td style="padding-right: 12px; border: none;">-0.04</td>
<td style="padding-right: 12px; border: none;">-2.14</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.93)</td>
<td style="padding-right: 12px; border: none;">(23.24)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersSomewhat known</td>
<td style="padding-right: 12px; border: none;">-1.02</td>
<td style="padding-right: 12px; border: none;">-37.72</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.68)</td>
<td style="padding-right: 12px; border: none;">(50.93)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersClose acquaintance</td>
<td style="padding-right: 12px; border: none;">1.53</td>
<td style="padding-right: 12px; border: none;">19.12</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.74)</td>
<td style="padding-right: 12px; border: none;">(102.25)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersDK/DA</td>
<td style="padding-right: 12px; border: none;">-1.53</td>
<td style="padding-right: 12px; border: none;">-20.36</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.17)</td>
<td style="padding-right: 12px; border: none;">(39.12)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">had_weaponYes</td>
<td style="padding-right: 12px; border: none;">1.03</td>
<td style="padding-right: 12px; border: none;">16.94</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.02)</td>
<td style="padding-right: 12px; border: none;">(28.62)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">had_weaponDK/DA</td>
<td style="padding-right: 12px; border: none;">0.30</td>
<td style="padding-right: 12px; border: none;">1.01</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.43)</td>
<td style="padding-right: 12px; border: none;">(11.85)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">with_violenceYes</td>
<td style="padding-right: 12px; border: none;">1.34</td>
<td style="padding-right: 12px; border: none;">0.02</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.02)</td>
<td style="padding-right: 12px; border: none;">(25.80)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">with_violenceDK/DA</td>
<td style="padding-right: 12px; border: none;">1.23</td>
<td style="padding-right: 12px; border: none;">18.49</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.71)</td>
<td style="padding-right: 12px; border: none;">(17.58)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">reportedYes</td>
<td style="padding-right: 12px; border: none;">0.64</td>
<td style="padding-right: 12px; border: none;">5.79</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.46)</td>
<td style="padding-right: 12px; border: none;">(15.79)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rep_extortion_victim1</td>
<td style="padding-right: 12px; border: none;">1.08</td>
<td style="padding-right: 12px; border: none;">10.80</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.65)</td>
<td style="padding-right: 12px; border: none;">(14.94)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">bribe_victim1</td>
<td style="padding-right: 12px; border: none;">0.12</td>
<td style="padding-right: 12px; border: none;">3.19</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.54)</td>
<td style="padding-right: 12px; border: none;">(14.61)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeMedium</td>
<td style="padding-right: 12px; border: none;">-0.27</td>
<td style="padding-right: 12px; border: none;">-13.12</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.53)</td>
<td style="padding-right: 12px; border: none;">(12.39)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeSmall</td>
<td style="padding-right: 12px; border: none;">0.92</td>
<td style="padding-right: 12px; border: none;">-2.66</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.50)</td>
<td style="padding-right: 12px; border: none;">(13.90)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeMicro</td>
<td style="padding-right: 12px; border: none;">-0.28</td>
<td style="padding-right: 12px; border: none;">-14.25</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
<td style="padding-right: 12px; border: none;">(16.71)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">hotrestbar1</td>
<td style="padding-right: 12px; border: none;">-0.94</td>
<td style="padding-right: 12px; border: none;">-14.65</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.66)</td>
<td style="padding-right: 12px; border: none;">(29.68)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(8,16]</td>
<td style="padding-right: 12px; border: none;">-0.35</td>
<td style="padding-right: 12px; border: none;">-16.19</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.59)</td>
<td style="padding-right: 12px; border: none;">(16.27)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(16,25]</td>
<td style="padding-right: 12px; border: none;">0.87</td>
<td style="padding-right: 12px; border: none;">1.90</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.53)</td>
<td style="padding-right: 12px; border: none;">(15.12)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(25,34]</td>
<td style="padding-right: 12px; border: none;">-0.80</td>
<td style="padding-right: 12px; border: none;">-16.91</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.68)</td>
<td style="padding-right: 12px; border: none;">(19.97)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(34,43]</td>
<td style="padding-right: 12px; border: none;">1.28<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">12.77</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
<td style="padding-right: 12px; border: none;">(12.45)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">loghoms</td>
<td style="padding-right: 12px; border: none;">-0.28</td>
<td style="padding-right: 12px; border: none;">-7.91</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.26)</td>
<td style="padding-right: 12px; border: none;">(6.77)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">logpop</td>
<td style="padding-right: 12px; border: none;">0.59</td>
<td style="padding-right: 12px; border: none;">14.58</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.40)</td>
<td style="padding-right: 12px; border: none;">(11.19)</td>
</tr>
<tr>
<td style="border-top: 1px solid black;">AIC</td>
<td style="border-top: 1px solid black;">371.56</td>
<td style="border-top: 1px solid black;">334.20</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">BIC</td>
<td style="padding-right: 12px; border: none;">607.60</td>
<td style="padding-right: 12px; border: none;">579.15</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Log Likelihood</td>
<td style="padding-right: 12px; border: none;">-132.78</td>
<td style="padding-right: 12px; border: none;">-112.10</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Deviance</td>
<td style="padding-right: 12px; border: none;">265.56</td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Num. obs.</td>
<td style="padding-right: 12px; border: none;">635</td>
<td style="padding-right: 12px; border: none;">635</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Num. groups: CVE_UNICA</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">522</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Num. groups: CVE_ENT</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">32</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Var: CVE_UNICA (Intercept)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">605.95</td>
</tr>
<tr>
<td style="border-bottom: 2px solid black;">Var: CVE_ENT (Intercept)</td>
<td style="border-bottom: 2px solid black;"></td>
<td style="border-bottom: 2px solid black;">0.76</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;" colspan="4"><span style="font-size:0.8em"><sup style="vertical-align: 0px;">***</sup>p &lt; 0.001, <sup style="vertical-align: 0px;">**</sup>p &lt; 0.01, <sup style="vertical-align: 0px;">*</sup>p &lt; 0.05</span></td>
</tr>
</table>
```

```r
screenreg(list(m1, m1_l3))
```

```
Warning in vcov.merMod(model, useScale = FALSE, ...): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```

===========================================================
                                 Model 1      Model 2      
-----------------------------------------------------------
(Intercept)                        -23.01           -35.74 
                                 (3420.48)          (98.75)
extortion_typeInternet               3.46            61.86 
                                    (1.83)          (46.69)
extortion_typeStreet                 1.75 *          21.69 
                                    (0.73)          (18.50)
extortion_typePremises              34.97           574.84 
                                 (1829.78)    (19647463.58)
extortion_typeCobro de piso          3.12 **         21.03 
                                    (1.15)          (43.15)
extortion_typeOther                  2.30 **         31.83 
                                    (0.80)          (23.64)
requestMoney                        19.74            24.13 
                                 (3420.48)          (93.20)
requestNothing                      19.61            15.46 
                                 (3420.48)          (92.70)
requestProduct                      20.09            21.93 
                                 (3420.48)          (98.79)
monthFebruary                       -1.50           -33.29 
                                    (0.97)          (23.27)
monthMarch                          -0.46           -16.58 
                                    (0.85)          (20.91)
monthApril                          -0.42            -9.28 
                                    (0.94)          (22.27)
monthMay                            -1.47           -26.30 
                                    (1.07)          (31.42)
monthJune                           -0.30           -13.31 
                                    (0.78)          (18.02)
monthJuly                           -0.52           -12.36 
                                    (0.94)          (22.87)
monthAugust                         -2.17           -25.77 
                                    (1.17)          (34.98)
monthSeptember                      -1.20           -30.02 
                                    (0.92)          (29.71)
monthOctober                        -2.11 *         -43.30 
                                    (1.06)          (33.46)
monthNovember                        0.69             2.63 
                                    (0.75)          (16.11)
monthDecember                       -1.75           -27.35 
                                    (0.99)          (29.21)
monthNONE                           -1.49           -28.62 
                                    (1.37)          (30.86)
timeAfternoon                        0.18             5.37 
                                    (0.41)          (10.96)
timeEvening                         -0.12            -2.73 
                                    (0.57)          (14.58)
timeNight                            2.30 *          26.43 
                                    (1.16)          (41.60)
timeDK/DA                            2.78            19.50 
                                    (1.79)          (47.62)
n_offenders2                         0.83 *           5.52 
                                    (0.42)          (11.42)
n_offenders3                         0.79            10.13 
                                    (0.75)          (19.12)
n_offenders4                         3.74 **         45.49 
                                    (1.39)          (64.30)
n_offenders5                         3.67            33.75 
                                    (2.02)          (96.19)
n_offenders6+                        0.10           -14.93 
                                    (1.44)          (28.07)
n_offendersDK/DA                    -0.11           -16.86 
                                    (0.84)          (25.33)
rel_offendersEmployee              -14.96          -442.69 
                                 (1278.54)    (39294927.16)
rel_offendersBarely known           -0.04            -2.14 
                                    (0.93)          (23.24)
rel_offendersSomewhat known         -1.02           -37.72 
                                    (1.68)          (50.93)
rel_offendersClose acquaintance      1.53            19.12 
                                    (1.74)         (102.25)
rel_offendersDK/DA                  -1.53           -20.36 
                                    (1.17)          (39.12)
had_weaponYes                        1.03            16.94 
                                    (1.02)          (28.62)
had_weaponDK/DA                      0.30             1.01 
                                    (0.43)          (11.85)
with_violenceYes                     1.34             0.02 
                                    (1.02)          (25.80)
with_violenceDK/DA                   1.23            18.49 
                                    (0.71)          (17.58)
reportedYes                          0.64             5.79 
                                    (0.46)          (15.79)
rep_extortion_victim1                1.08            10.80 
                                    (0.65)          (14.94)
bribe_victim1                        0.12             3.19 
                                    (0.54)          (14.61)
sizeMedium                          -0.27           -13.12 
                                    (0.53)          (12.39)
sizeSmall                            0.92            -2.66 
                                    (0.50)          (13.90)
sizeMicro                           -0.28           -14.25 
                                    (0.57)          (16.71)
hotrestbar1                         -0.94           -14.65 
                                    (0.66)          (29.68)
yearsquant(8,16]                    -0.35           -16.19 
                                    (0.59)          (16.27)
yearsquant(16,25]                    0.87             1.90 
                                    (0.53)          (15.12)
yearsquant(25,34]                   -0.80           -16.91 
                                    (0.68)          (19.97)
yearsquant(34,43]                    1.28 *          12.77 
                                    (0.57)          (12.45)
loghoms                             -0.28            -7.91 
                                    (0.26)           (6.77)
logpop                               0.59            14.58 
                                    (0.40)          (11.19)
-----------------------------------------------------------
AIC                                371.56           334.20 
BIC                                607.60           579.15 
Log Likelihood                    -132.78          -112.10 
Deviance                           265.56                  
Num. obs.                          635              635    
Num. groups: CVE_UNICA                              522    
Num. groups: CVE_ENT                                 32    
Var: CVE_UNICA (Intercept)                          605.95 
Var: CVE_ENT (Intercept)                              0.76 
===========================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
htmlreg(list(m1, m1_l2, m1_l3))
```

```
Warning in vcov.merMod(model, useScale = FALSE, ...): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```
Warning in vcov.merMod(model, useScale = FALSE, ...): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
<caption align="bottom" style="margin-top:0.3em;">Statistical models</caption>
<tr>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b></b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 1</b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 2</b></th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"><b>Model 3</b></th>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">(Intercept)</td>
<td style="padding-right: 12px; border: none;">-23.01</td>
<td style="padding-right: 12px; border: none;">-30.80</td>
<td style="padding-right: 12px; border: none;">-35.74</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;">(72.66)</td>
<td style="padding-right: 12px; border: none;">(98.75)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeInternet</td>
<td style="padding-right: 12px; border: none;">3.46</td>
<td style="padding-right: 12px; border: none;">29.23</td>
<td style="padding-right: 12px; border: none;">61.86</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.83)</td>
<td style="padding-right: 12px; border: none;">(52.79)</td>
<td style="padding-right: 12px; border: none;">(46.69)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeStreet</td>
<td style="padding-right: 12px; border: none;">1.75<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">9.45</td>
<td style="padding-right: 12px; border: none;">21.69</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.73)</td>
<td style="padding-right: 12px; border: none;">(11.38)</td>
<td style="padding-right: 12px; border: none;">(18.50)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typePremises</td>
<td style="padding-right: 12px; border: none;">34.97</td>
<td style="padding-right: 12px; border: none;">119.54</td>
<td style="padding-right: 12px; border: none;">574.84</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1829.78)</td>
<td style="padding-right: 12px; border: none;">(18612475.24)</td>
<td style="padding-right: 12px; border: none;">(19647463.58)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeCobro de piso</td>
<td style="padding-right: 12px; border: none;">3.12<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">5.63</td>
<td style="padding-right: 12px; border: none;">21.03</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.15)</td>
<td style="padding-right: 12px; border: none;">(25.84)</td>
<td style="padding-right: 12px; border: none;">(43.15)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">extortion_typeOther</td>
<td style="padding-right: 12px; border: none;">2.30<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">21.88</td>
<td style="padding-right: 12px; border: none;">31.83</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.80)</td>
<td style="padding-right: 12px; border: none;">(13.70)</td>
<td style="padding-right: 12px; border: none;">(23.64)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestMoney</td>
<td style="padding-right: 12px; border: none;">19.74</td>
<td style="padding-right: 12px; border: none;">28.75</td>
<td style="padding-right: 12px; border: none;">24.13</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;">(68.98)</td>
<td style="padding-right: 12px; border: none;">(93.20)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestNothing</td>
<td style="padding-right: 12px; border: none;">19.61</td>
<td style="padding-right: 12px; border: none;">27.83</td>
<td style="padding-right: 12px; border: none;">15.46</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;">(69.23)</td>
<td style="padding-right: 12px; border: none;">(92.70)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">requestProduct</td>
<td style="padding-right: 12px; border: none;">20.09</td>
<td style="padding-right: 12px; border: none;">30.18</td>
<td style="padding-right: 12px; border: none;">21.93</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(3420.48)</td>
<td style="padding-right: 12px; border: none;">(72.16)</td>
<td style="padding-right: 12px; border: none;">(98.79)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthFebruary</td>
<td style="padding-right: 12px; border: none;">-1.50</td>
<td style="padding-right: 12px; border: none;">-30.32</td>
<td style="padding-right: 12px; border: none;">-33.29</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.97)</td>
<td style="padding-right: 12px; border: none;">(18.39)</td>
<td style="padding-right: 12px; border: none;">(23.27)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthMarch</td>
<td style="padding-right: 12px; border: none;">-0.46</td>
<td style="padding-right: 12px; border: none;">-12.31</td>
<td style="padding-right: 12px; border: none;">-16.58</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.85)</td>
<td style="padding-right: 12px; border: none;">(15.24)</td>
<td style="padding-right: 12px; border: none;">(20.91)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthApril</td>
<td style="padding-right: 12px; border: none;">-0.42</td>
<td style="padding-right: 12px; border: none;">-24.21</td>
<td style="padding-right: 12px; border: none;">-9.28</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.94)</td>
<td style="padding-right: 12px; border: none;">(18.24)</td>
<td style="padding-right: 12px; border: none;">(22.27)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthMay</td>
<td style="padding-right: 12px; border: none;">-1.47</td>
<td style="padding-right: 12px; border: none;">-16.67</td>
<td style="padding-right: 12px; border: none;">-26.30</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.07)</td>
<td style="padding-right: 12px; border: none;">(24.28)</td>
<td style="padding-right: 12px; border: none;">(31.42)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthJune</td>
<td style="padding-right: 12px; border: none;">-0.30</td>
<td style="padding-right: 12px; border: none;">-9.58</td>
<td style="padding-right: 12px; border: none;">-13.31</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.78)</td>
<td style="padding-right: 12px; border: none;">(12.14)</td>
<td style="padding-right: 12px; border: none;">(18.02)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthJuly</td>
<td style="padding-right: 12px; border: none;">-0.52</td>
<td style="padding-right: 12px; border: none;">-12.79</td>
<td style="padding-right: 12px; border: none;">-12.36</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.94)</td>
<td style="padding-right: 12px; border: none;">(13.52)</td>
<td style="padding-right: 12px; border: none;">(22.87)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthAugust</td>
<td style="padding-right: 12px; border: none;">-2.17</td>
<td style="padding-right: 12px; border: none;">-27.53</td>
<td style="padding-right: 12px; border: none;">-25.77</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.17)</td>
<td style="padding-right: 12px; border: none;">(24.69)</td>
<td style="padding-right: 12px; border: none;">(34.98)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthSeptember</td>
<td style="padding-right: 12px; border: none;">-1.20</td>
<td style="padding-right: 12px; border: none;">-11.21</td>
<td style="padding-right: 12px; border: none;">-30.02</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.92)</td>
<td style="padding-right: 12px; border: none;">(12.40)</td>
<td style="padding-right: 12px; border: none;">(29.71)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthOctober</td>
<td style="padding-right: 12px; border: none;">-2.11<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">-17.12</td>
<td style="padding-right: 12px; border: none;">-43.30</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.06)</td>
<td style="padding-right: 12px; border: none;">(22.54)</td>
<td style="padding-right: 12px; border: none;">(33.46)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthNovember</td>
<td style="padding-right: 12px; border: none;">0.69</td>
<td style="padding-right: 12px; border: none;">-5.37</td>
<td style="padding-right: 12px; border: none;">2.63</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.75)</td>
<td style="padding-right: 12px; border: none;">(11.36)</td>
<td style="padding-right: 12px; border: none;">(16.11)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthDecember</td>
<td style="padding-right: 12px; border: none;">-1.75</td>
<td style="padding-right: 12px; border: none;">-18.82</td>
<td style="padding-right: 12px; border: none;">-27.35</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.99)</td>
<td style="padding-right: 12px; border: none;">(18.05)</td>
<td style="padding-right: 12px; border: none;">(29.21)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">monthNONE</td>
<td style="padding-right: 12px; border: none;">-1.49</td>
<td style="padding-right: 12px; border: none;">-17.82</td>
<td style="padding-right: 12px; border: none;">-28.62</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.37)</td>
<td style="padding-right: 12px; border: none;">(33.60)</td>
<td style="padding-right: 12px; border: none;">(30.86)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeAfternoon</td>
<td style="padding-right: 12px; border: none;">0.18</td>
<td style="padding-right: 12px; border: none;">0.57</td>
<td style="padding-right: 12px; border: none;">5.37</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.41)</td>
<td style="padding-right: 12px; border: none;">(8.33)</td>
<td style="padding-right: 12px; border: none;">(10.96)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeEvening</td>
<td style="padding-right: 12px; border: none;">-0.12</td>
<td style="padding-right: 12px; border: none;">-1.57</td>
<td style="padding-right: 12px; border: none;">-2.73</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
<td style="padding-right: 12px; border: none;">(11.88)</td>
<td style="padding-right: 12px; border: none;">(14.58)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeNight</td>
<td style="padding-right: 12px; border: none;">2.30<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">2.35</td>
<td style="padding-right: 12px; border: none;">26.43</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.16)</td>
<td style="padding-right: 12px; border: none;">(31.34)</td>
<td style="padding-right: 12px; border: none;">(41.60)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">timeDK/DA</td>
<td style="padding-right: 12px; border: none;">2.78</td>
<td style="padding-right: 12px; border: none;">-6.34</td>
<td style="padding-right: 12px; border: none;">19.50</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.79)</td>
<td style="padding-right: 12px; border: none;">(44.01)</td>
<td style="padding-right: 12px; border: none;">(47.62)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders2</td>
<td style="padding-right: 12px; border: none;">0.83<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">0.46</td>
<td style="padding-right: 12px; border: none;">5.52</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.42)</td>
<td style="padding-right: 12px; border: none;">(8.90)</td>
<td style="padding-right: 12px; border: none;">(11.42)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders3</td>
<td style="padding-right: 12px; border: none;">0.79</td>
<td style="padding-right: 12px; border: none;">6.55</td>
<td style="padding-right: 12px; border: none;">10.13</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.75)</td>
<td style="padding-right: 12px; border: none;">(14.09)</td>
<td style="padding-right: 12px; border: none;">(19.12)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders4</td>
<td style="padding-right: 12px; border: none;">3.74<sup style="vertical-align: 0px;">**</sup></td>
<td style="padding-right: 12px; border: none;">23.69</td>
<td style="padding-right: 12px; border: none;">45.49</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.39)</td>
<td style="padding-right: 12px; border: none;">(25.54)</td>
<td style="padding-right: 12px; border: none;">(64.30)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders5</td>
<td style="padding-right: 12px; border: none;">3.67</td>
<td style="padding-right: 12px; border: none;">6.32</td>
<td style="padding-right: 12px; border: none;">33.75</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(2.02)</td>
<td style="padding-right: 12px; border: none;">(90.28)</td>
<td style="padding-right: 12px; border: none;">(96.19)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offenders6+</td>
<td style="padding-right: 12px; border: none;">0.10</td>
<td style="padding-right: 12px; border: none;">-33.44</td>
<td style="padding-right: 12px; border: none;">-14.93</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.44)</td>
<td style="padding-right: 12px; border: none;">(21.44)</td>
<td style="padding-right: 12px; border: none;">(28.07)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">n_offendersDK/DA</td>
<td style="padding-right: 12px; border: none;">-0.11</td>
<td style="padding-right: 12px; border: none;">-8.46</td>
<td style="padding-right: 12px; border: none;">-16.86</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.84)</td>
<td style="padding-right: 12px; border: none;">(19.15)</td>
<td style="padding-right: 12px; border: none;">(25.33)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersEmployee</td>
<td style="padding-right: 12px; border: none;">-14.96</td>
<td style="padding-right: 12px; border: none;">-87.98</td>
<td style="padding-right: 12px; border: none;">-442.69</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1278.54)</td>
<td style="padding-right: 12px; border: none;">(18612475.24)</td>
<td style="padding-right: 12px; border: none;">(39294927.16)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersBarely known</td>
<td style="padding-right: 12px; border: none;">-0.04</td>
<td style="padding-right: 12px; border: none;">1.90</td>
<td style="padding-right: 12px; border: none;">-2.14</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.93)</td>
<td style="padding-right: 12px; border: none;">(14.97)</td>
<td style="padding-right: 12px; border: none;">(23.24)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersSomewhat known</td>
<td style="padding-right: 12px; border: none;">-1.02</td>
<td style="padding-right: 12px; border: none;">-36.10</td>
<td style="padding-right: 12px; border: none;">-37.72</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.68)</td>
<td style="padding-right: 12px; border: none;">(37.03)</td>
<td style="padding-right: 12px; border: none;">(50.93)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersClose acquaintance</td>
<td style="padding-right: 12px; border: none;">1.53</td>
<td style="padding-right: 12px; border: none;">3.74</td>
<td style="padding-right: 12px; border: none;">19.12</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.74)</td>
<td style="padding-right: 12px; border: none;">(33.17)</td>
<td style="padding-right: 12px; border: none;">(102.25)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rel_offendersDK/DA</td>
<td style="padding-right: 12px; border: none;">-1.53</td>
<td style="padding-right: 12px; border: none;">-5.41</td>
<td style="padding-right: 12px; border: none;">-20.36</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.17)</td>
<td style="padding-right: 12px; border: none;">(30.28)</td>
<td style="padding-right: 12px; border: none;">(39.12)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">had_weaponYes</td>
<td style="padding-right: 12px; border: none;">1.03</td>
<td style="padding-right: 12px; border: none;">19.09</td>
<td style="padding-right: 12px; border: none;">16.94</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.02)</td>
<td style="padding-right: 12px; border: none;">(18.07)</td>
<td style="padding-right: 12px; border: none;">(28.62)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">had_weaponDK/DA</td>
<td style="padding-right: 12px; border: none;">0.30</td>
<td style="padding-right: 12px; border: none;">-2.82</td>
<td style="padding-right: 12px; border: none;">1.01</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.43)</td>
<td style="padding-right: 12px; border: none;">(9.25)</td>
<td style="padding-right: 12px; border: none;">(11.85)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">with_violenceYes</td>
<td style="padding-right: 12px; border: none;">1.34</td>
<td style="padding-right: 12px; border: none;">3.91</td>
<td style="padding-right: 12px; border: none;">0.02</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(1.02)</td>
<td style="padding-right: 12px; border: none;">(17.55)</td>
<td style="padding-right: 12px; border: none;">(25.80)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">with_violenceDK/DA</td>
<td style="padding-right: 12px; border: none;">1.23</td>
<td style="padding-right: 12px; border: none;">11.36</td>
<td style="padding-right: 12px; border: none;">18.49</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.71)</td>
<td style="padding-right: 12px; border: none;">(14.90)</td>
<td style="padding-right: 12px; border: none;">(17.58)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">reportedYes</td>
<td style="padding-right: 12px; border: none;">0.64</td>
<td style="padding-right: 12px; border: none;">0.96</td>
<td style="padding-right: 12px; border: none;">5.79</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.46)</td>
<td style="padding-right: 12px; border: none;">(10.58)</td>
<td style="padding-right: 12px; border: none;">(15.79)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">rep_extortion_victim1</td>
<td style="padding-right: 12px; border: none;">1.08</td>
<td style="padding-right: 12px; border: none;">3.20</td>
<td style="padding-right: 12px; border: none;">10.80</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.65)</td>
<td style="padding-right: 12px; border: none;">(15.51)</td>
<td style="padding-right: 12px; border: none;">(14.94)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">bribe_victim1</td>
<td style="padding-right: 12px; border: none;">0.12</td>
<td style="padding-right: 12px; border: none;">0.58</td>
<td style="padding-right: 12px; border: none;">3.19</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.54)</td>
<td style="padding-right: 12px; border: none;">(13.33)</td>
<td style="padding-right: 12px; border: none;">(14.61)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeMedium</td>
<td style="padding-right: 12px; border: none;">-0.27</td>
<td style="padding-right: 12px; border: none;">-8.99</td>
<td style="padding-right: 12px; border: none;">-13.12</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.53)</td>
<td style="padding-right: 12px; border: none;">(10.19)</td>
<td style="padding-right: 12px; border: none;">(12.39)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeSmall</td>
<td style="padding-right: 12px; border: none;">0.92</td>
<td style="padding-right: 12px; border: none;">-0.32</td>
<td style="padding-right: 12px; border: none;">-2.66</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.50)</td>
<td style="padding-right: 12px; border: none;">(10.03)</td>
<td style="padding-right: 12px; border: none;">(13.90)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">sizeMicro</td>
<td style="padding-right: 12px; border: none;">-0.28</td>
<td style="padding-right: 12px; border: none;">-8.01</td>
<td style="padding-right: 12px; border: none;">-14.25</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
<td style="padding-right: 12px; border: none;">(12.50)</td>
<td style="padding-right: 12px; border: none;">(16.71)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">hotrestbar1</td>
<td style="padding-right: 12px; border: none;">-0.94</td>
<td style="padding-right: 12px; border: none;">-4.93</td>
<td style="padding-right: 12px; border: none;">-14.65</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.66)</td>
<td style="padding-right: 12px; border: none;">(19.87)</td>
<td style="padding-right: 12px; border: none;">(29.68)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(8,16]</td>
<td style="padding-right: 12px; border: none;">-0.35</td>
<td style="padding-right: 12px; border: none;">-4.30</td>
<td style="padding-right: 12px; border: none;">-16.19</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.59)</td>
<td style="padding-right: 12px; border: none;">(10.78)</td>
<td style="padding-right: 12px; border: none;">(16.27)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(16,25]</td>
<td style="padding-right: 12px; border: none;">0.87</td>
<td style="padding-right: 12px; border: none;">-0.81</td>
<td style="padding-right: 12px; border: none;">1.90</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.53)</td>
<td style="padding-right: 12px; border: none;">(9.96)</td>
<td style="padding-right: 12px; border: none;">(15.12)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(25,34]</td>
<td style="padding-right: 12px; border: none;">-0.80</td>
<td style="padding-right: 12px; border: none;">-3.77</td>
<td style="padding-right: 12px; border: none;">-16.91</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.68)</td>
<td style="padding-right: 12px; border: none;">(14.49)</td>
<td style="padding-right: 12px; border: none;">(19.97)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">yearsquant(34,43]</td>
<td style="padding-right: 12px; border: none;">1.28<sup style="vertical-align: 0px;">*</sup></td>
<td style="padding-right: 12px; border: none;">1.05</td>
<td style="padding-right: 12px; border: none;">12.77</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.57)</td>
<td style="padding-right: 12px; border: none;">(11.23)</td>
<td style="padding-right: 12px; border: none;">(12.45)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">loghoms</td>
<td style="padding-right: 12px; border: none;">-0.28</td>
<td style="padding-right: 12px; border: none;">-4.03</td>
<td style="padding-right: 12px; border: none;">-7.91</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.26)</td>
<td style="padding-right: 12px; border: none;">(5.05)</td>
<td style="padding-right: 12px; border: none;">(6.77)</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">logpop</td>
<td style="padding-right: 12px; border: none;">0.59</td>
<td style="padding-right: 12px; border: none;">5.16</td>
<td style="padding-right: 12px; border: none;">14.58</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">(0.40)</td>
<td style="padding-right: 12px; border: none;">(8.13)</td>
<td style="padding-right: 12px; border: none;">(11.19)</td>
</tr>
<tr>
<td style="border-top: 1px solid black;">AIC</td>
<td style="border-top: 1px solid black;">371.56</td>
<td style="border-top: 1px solid black;">324.69</td>
<td style="border-top: 1px solid black;">334.20</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">BIC</td>
<td style="padding-right: 12px; border: none;">607.60</td>
<td style="padding-right: 12px; border: none;">565.19</td>
<td style="padding-right: 12px; border: none;">579.15</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Log Likelihood</td>
<td style="padding-right: 12px; border: none;">-132.78</td>
<td style="padding-right: 12px; border: none;">-108.35</td>
<td style="padding-right: 12px; border: none;">-112.10</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Deviance</td>
<td style="padding-right: 12px; border: none;">265.56</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Num. obs.</td>
<td style="padding-right: 12px; border: none;">635</td>
<td style="padding-right: 12px; border: none;">635</td>
<td style="padding-right: 12px; border: none;">635</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Num. groups: CVE_UNICA</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">522</td>
<td style="padding-right: 12px; border: none;">522</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Var: CVE_UNICA (Intercept)</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">414.68</td>
<td style="padding-right: 12px; border: none;">605.95</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">Num. groups: CVE_ENT</td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;"></td>
<td style="padding-right: 12px; border: none;">32</td>
</tr>
<tr>
<td style="border-bottom: 2px solid black;">Var: CVE_ENT (Intercept)</td>
<td style="border-bottom: 2px solid black;"></td>
<td style="border-bottom: 2px solid black;"></td>
<td style="border-bottom: 2px solid black;">0.76</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;" colspan="5"><span style="font-size:0.8em"><sup style="vertical-align: 0px;">***</sup>p &lt; 0.001, <sup style="vertical-align: 0px;">**</sup>p &lt; 0.01, <sup style="vertical-align: 0px;">*</sup>p &lt; 0.05</span></td>
</tr>
</table>
```

```r
screenreg(list(m1, m1_l2, m1_l3))
```

```
Warning in vcov.merMod(model, useScale = FALSE, ...): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX

Warning in vcov.merMod(model, useScale = FALSE, ...): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```

==========================================================================
                                 Model 1      Model 2        Model 3      
--------------------------------------------------------------------------
(Intercept)                        -23.01           -30.80         -35.74 
                                 (3420.48)          (72.66)        (98.75)
extortion_typeInternet               3.46            29.23          61.86 
                                    (1.83)          (52.79)        (46.69)
extortion_typeStreet                 1.75 *           9.45          21.69 
                                    (0.73)          (11.38)        (18.50)
extortion_typePremises              34.97           119.54         574.84 
                                 (1829.78)    (18612475.24)  (19647463.58)
extortion_typeCobro de piso          3.12 **          5.63          21.03 
                                    (1.15)          (25.84)        (43.15)
extortion_typeOther                  2.30 **         21.88          31.83 
                                    (0.80)          (13.70)        (23.64)
requestMoney                        19.74            28.75          24.13 
                                 (3420.48)          (68.98)        (93.20)
requestNothing                      19.61            27.83          15.46 
                                 (3420.48)          (69.23)        (92.70)
requestProduct                      20.09            30.18          21.93 
                                 (3420.48)          (72.16)        (98.79)
monthFebruary                       -1.50           -30.32         -33.29 
                                    (0.97)          (18.39)        (23.27)
monthMarch                          -0.46           -12.31         -16.58 
                                    (0.85)          (15.24)        (20.91)
monthApril                          -0.42           -24.21          -9.28 
                                    (0.94)          (18.24)        (22.27)
monthMay                            -1.47           -16.67         -26.30 
                                    (1.07)          (24.28)        (31.42)
monthJune                           -0.30            -9.58         -13.31 
                                    (0.78)          (12.14)        (18.02)
monthJuly                           -0.52           -12.79         -12.36 
                                    (0.94)          (13.52)        (22.87)
monthAugust                         -2.17           -27.53         -25.77 
                                    (1.17)          (24.69)        (34.98)
monthSeptember                      -1.20           -11.21         -30.02 
                                    (0.92)          (12.40)        (29.71)
monthOctober                        -2.11 *         -17.12         -43.30 
                                    (1.06)          (22.54)        (33.46)
monthNovember                        0.69            -5.37           2.63 
                                    (0.75)          (11.36)        (16.11)
monthDecember                       -1.75           -18.82         -27.35 
                                    (0.99)          (18.05)        (29.21)
monthNONE                           -1.49           -17.82         -28.62 
                                    (1.37)          (33.60)        (30.86)
timeAfternoon                        0.18             0.57           5.37 
                                    (0.41)           (8.33)        (10.96)
timeEvening                         -0.12            -1.57          -2.73 
                                    (0.57)          (11.88)        (14.58)
timeNight                            2.30 *           2.35          26.43 
                                    (1.16)          (31.34)        (41.60)
timeDK/DA                            2.78            -6.34          19.50 
                                    (1.79)          (44.01)        (47.62)
n_offenders2                         0.83 *           0.46           5.52 
                                    (0.42)           (8.90)        (11.42)
n_offenders3                         0.79             6.55          10.13 
                                    (0.75)          (14.09)        (19.12)
n_offenders4                         3.74 **         23.69          45.49 
                                    (1.39)          (25.54)        (64.30)
n_offenders5                         3.67             6.32          33.75 
                                    (2.02)          (90.28)        (96.19)
n_offenders6+                        0.10           -33.44         -14.93 
                                    (1.44)          (21.44)        (28.07)
n_offendersDK/DA                    -0.11            -8.46         -16.86 
                                    (0.84)          (19.15)        (25.33)
rel_offendersEmployee              -14.96           -87.98        -442.69 
                                 (1278.54)    (18612475.24)  (39294927.16)
rel_offendersBarely known           -0.04             1.90          -2.14 
                                    (0.93)          (14.97)        (23.24)
rel_offendersSomewhat known         -1.02           -36.10         -37.72 
                                    (1.68)          (37.03)        (50.93)
rel_offendersClose acquaintance      1.53             3.74          19.12 
                                    (1.74)          (33.17)       (102.25)
rel_offendersDK/DA                  -1.53            -5.41         -20.36 
                                    (1.17)          (30.28)        (39.12)
had_weaponYes                        1.03            19.09          16.94 
                                    (1.02)          (18.07)        (28.62)
had_weaponDK/DA                      0.30            -2.82           1.01 
                                    (0.43)           (9.25)        (11.85)
with_violenceYes                     1.34             3.91           0.02 
                                    (1.02)          (17.55)        (25.80)
with_violenceDK/DA                   1.23            11.36          18.49 
                                    (0.71)          (14.90)        (17.58)
reportedYes                          0.64             0.96           5.79 
                                    (0.46)          (10.58)        (15.79)
rep_extortion_victim1                1.08             3.20          10.80 
                                    (0.65)          (15.51)        (14.94)
bribe_victim1                        0.12             0.58           3.19 
                                    (0.54)          (13.33)        (14.61)
sizeMedium                          -0.27            -8.99         -13.12 
                                    (0.53)          (10.19)        (12.39)
sizeSmall                            0.92            -0.32          -2.66 
                                    (0.50)          (10.03)        (13.90)
sizeMicro                           -0.28            -8.01         -14.25 
                                    (0.57)          (12.50)        (16.71)
hotrestbar1                         -0.94            -4.93         -14.65 
                                    (0.66)          (19.87)        (29.68)
yearsquant(8,16]                    -0.35            -4.30         -16.19 
                                    (0.59)          (10.78)        (16.27)
yearsquant(16,25]                    0.87            -0.81           1.90 
                                    (0.53)           (9.96)        (15.12)
yearsquant(25,34]                   -0.80            -3.77         -16.91 
                                    (0.68)          (14.49)        (19.97)
yearsquant(34,43]                    1.28 *           1.05          12.77 
                                    (0.57)          (11.23)        (12.45)
loghoms                             -0.28            -4.03          -7.91 
                                    (0.26)           (5.05)         (6.77)
logpop                               0.59             5.16          14.58 
                                    (0.40)           (8.13)        (11.19)
--------------------------------------------------------------------------
AIC                                371.56           324.69         334.20 
BIC                                607.60           565.19         579.15 
Log Likelihood                    -132.78          -108.35        -112.10 
Deviance                           265.56                                 
Num. obs.                          635              635            635    
Num. groups: CVE_UNICA                              522            522    
Var: CVE_UNICA (Intercept)                          414.68         605.95 
Num. groups: CVE_ENT                                                32    
Var: CVE_ENT (Intercept)                                             0.76 
==========================================================================
*** p < 0.001, ** p < 0.01, * p < 0.05
```

```r
lrtest(m1_l3, m1)
```

```
Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glmerMod", updated model is of class "glm"
```

```
Likelihood ratio test

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop + (1 | CVE_UNICA) + (1 | CVE_ENT)
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
  #Df  LogLik Df  Chisq     Pr(>Chisq)    
1  55 -112.10                             
2  53 -132.78 -2 41.355 0.000000001047 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(lrtest(m1_l3, m1))
```

```
Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glmerMod", updated model is of class "glm"
```



| #Df|    LogLik| Df|    Chisq| Pr(>Chisq)|
|---:|---------:|--:|--------:|----------:|
|  55| -112.1012| NA|       NA|         NA|
|  53| -132.7790| -2| 41.35548|          0|

```r
anova(m1_l3, m1, test="LRT")
```

```
Data: m1df
Models:
m1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1:     bribe_victim + size + hotrestbar + yearsquant + loghoms + 
m1:     logpop
m1_l3: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l3:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1_l3:     bribe_victim + size + hotrestbar + yearsquant + loghoms + 
m1_l3:     logpop + (1 | CVE_UNICA) + (1 | CVE_ENT)
      Df    AIC    BIC  logLik deviance  Chisq Chi Df     Pr(>Chisq)    
m1    53 371.56 607.60 -132.78   265.56                                 
m1_l3 55 334.20 579.15 -112.10   224.20 41.355      2 0.000000001047 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(anova(m1_l3, m1, test="LRT"))
```



|      | Df|      AIC|      BIC|    logLik| deviance|    Chisq| Chi Df| Pr(>Chisq)|
|:-----|--:|--------:|--------:|---------:|--------:|--------:|------:|----------:|
|m1    | 53| 371.5579| 607.6000| -132.7790| 265.5579|       NA|     NA|         NA|
|m1_l3 | 55| 334.2024| 579.1518| -112.1012| 224.2024| 41.35548|      2|          0|

```r
lrtest(m1_l3, m1_l2, m1)
```

```
Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glmerMod", updated model is of class "glm"
```

```
Likelihood ratio test

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop + (1 | CVE_UNICA) + (1 | CVE_ENT)
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop + (1 | CVE_UNICA)
Model 3: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop
  #Df  LogLik Df   Chisq       Pr(>Chisq)    
1  55 -112.10                                
2  54 -108.35 -1  7.5108         0.006133 ** 
3  53 -132.78 -1 48.8662 0.00000000000274 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(lrtest(m1_l3, m1_l2, m1))
```

```
Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was
of class "glmerMod", updated model is of class "glm"
```



| #Df|    LogLik| Df|     Chisq| Pr(>Chisq)|
|---:|---------:|--:|---------:|----------:|
|  55| -112.1012| NA|        NA|         NA|
|  54| -108.3458| -1|  7.510756|  0.0061332|
|  53| -132.7790| -1| 48.866234|  0.0000000|

```r
anova(m1_l3, m1_l2, m1, test="LRT")
```

```
Data: m1df
Models:
m1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1:     bribe_victim + size + hotrestbar + yearsquant + loghoms + 
m1:     logpop
m1_l2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l2:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1_l2:     bribe_victim + size + hotrestbar + yearsquant + loghoms + 
m1_l2:     logpop + (1 | CVE_UNICA)
m1_l3: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l3:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1_l3:     bribe_victim + size + hotrestbar + yearsquant + loghoms + 
m1_l3:     logpop + (1 | CVE_UNICA) + (1 | CVE_ENT)
      Df    AIC    BIC  logLik deviance  Chisq Chi Df       Pr(>Chisq)    
m1    53 371.56 607.60 -132.78   265.56                                   
m1_l2 54 324.69 565.19 -108.35   216.69 48.866      1 0.00000000000274 ***
m1_l3 55 334.20 579.15 -112.10   224.20  0.000      1                1    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(anova(m1_l3, m1_l2, m1, test="LRT"))
```



|      | Df|      AIC|      BIC|    logLik| deviance|    Chisq| Chi Df| Pr(>Chisq)|
|:-----|--:|--------:|--------:|---------:|--------:|--------:|------:|----------:|
|m1    | 53| 371.5579| 607.6000| -132.7790| 265.5579|       NA|     NA|         NA|
|m1_l2 | 54| 324.6917| 565.1874| -108.3458| 216.6917| 48.86623|      1|          0|
|m1_l3 | 55| 334.2024| 579.1518| -112.1012| 224.2024|  0.00000|      1|          1|

```r
lrtest(m1_l3, m1_l2)
```

```
Likelihood ratio test

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop + (1 | CVE_UNICA) + (1 | CVE_ENT)
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop + (1 | CVE_UNICA)
  #Df  LogLik Df  Chisq Pr(>Chisq)   
1  55 -112.10                        
2  54 -108.35 -1 7.5108   0.006133 **
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(lrtest(m1_l3, m1_l2))
```



| #Df|    LogLik| Df|    Chisq| Pr(>Chisq)|
|---:|---------:|--:|--------:|----------:|
|  55| -112.1012| NA|       NA|         NA|
|  54| -108.3458| -1| 7.510756|  0.0061332|

```r
anova(m1_l3, m1_l2, test="LRT")
```

```
Data: m1df
Models:
m1_l2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l2:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1_l2:     bribe_victim + size + hotrestbar + yearsquant + loghoms + 
m1_l2:     logpop + (1 | CVE_UNICA)
m1_l3: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l3:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1_l3:     bribe_victim + size + hotrestbar + yearsquant + loghoms + 
m1_l3:     logpop + (1 | CVE_UNICA) + (1 | CVE_ENT)
      Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
m1_l2 54 324.69 565.19 -108.35   216.69                        
m1_l3 55 334.20 579.15 -112.10   224.20     0      1          1
```

```r
kable(anova(m1_l3, m1_l2, test="LRT"))
```



|      | Df|      AIC|      BIC|    logLik| deviance| Chisq| Chi Df| Pr(>Chisq)|
|:-----|--:|--------:|--------:|---------:|--------:|-----:|------:|----------:|
|m1_l2 | 54| 324.6917| 565.1874| -108.3458| 216.6917|    NA|     NA|         NA|
|m1_l3 | 55| 334.2024| 579.1518| -112.1012| 224.2024|     0|      1|          1|

Now test excluding all three level vars and all two level vars to test their influence.


```r
## Exclude state then business level variables

m1_l3_nostate <- glmer(complied_bin ~
                    extortion_type + request +
                    month + time +
                    n_offenders + rel_offenders +
                    had_weapon +
                    with_violence +
                    reported +
                    rep_extortion_victim +
                    bribe_victim +
                    size + hotrestbar + yearsquant +
                    (1|CVE_UNICA) + (1|CVE_ENT),
                  data=m1df,
                  family = "binomial")
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge: degenerate Hessian with 13 negative
eigenvalues
```

```r
summary(m1_l3_nostate)
```

```
Warning in vcov.merMod(object, use.hessian = use.hessian): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```
Warning in vcov.merMod(object, correlation = correlation, sigm = sig): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```
Generalized linear mixed model fit by maximum likelihood (Laplace
  Approximation) [glmerMod]
 Family: binomial  ( logit )
Formula: 
complied_bin ~ extortion_type + request + month + time + n_offenders +  
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim +  
    bribe_victim + size + hotrestbar + yearsquant + (1 | CVE_UNICA) +  
    (1 | CVE_ENT)
   Data: m1df

     AIC      BIC   logLik deviance df.resid 
   330.5    566.5   -112.2    224.5      582 

Scaled residuals: 
      Min        1Q    Median        3Q       Max 
-0.275891 -0.000043  0.000000  0.000000  0.289413 

Random effects:
 Groups    Name        Variance Std.Dev.
 CVE_UNICA (Intercept) 462.330  21.502  
 CVE_ENT   (Intercept)   4.514   2.125  
Number of obs: 635, groups:  CVE_UNICA, 522; CVE_ENT, 32

Fixed effects:
                                     Estimate    Std. Error z value
(Intercept)                          -31.8216     3898.7470  -0.008
extortion_typeInternet                 4.5192       37.5903   0.120
extortion_typeStreet                  16.1958       12.4643   1.299
extortion_typePremises               307.9011 18612671.8610   0.000
extortion_typeCobro de piso          -11.3458       28.1810  -0.403
extortion_typeOther                   23.3542       17.7709   1.314
requestMoney                          29.8596     3898.6693   0.008
requestNothing                        26.8610     3898.6869   0.007
requestProduct                        28.6457     3898.7687   0.007
monthFebruary                        -26.5584       24.7456  -1.073
monthMarch                            -8.3190       15.7418  -0.528
monthApril                           -24.8553       20.5324  -1.210
monthMay                             -13.9202       14.8351  -0.938
monthJune                            -13.3334       12.9426  -1.030
monthJuly                            -15.1077       14.6532  -1.031
monthAugust                          -32.1456       29.5819  -1.087
monthSeptember                       -14.3944       14.8097  -0.972
monthOctober                         -15.0748       21.1436  -0.713
monthNovember                         -5.4850       12.5052  -0.439
monthDecember                        -20.2894       21.3335  -0.951
monthNONE                            -15.1146       51.6991  -0.292
timeAfternoon                         -0.6609        8.7190  -0.076
timeEvening                           -0.2850       12.7344  -0.022
timeNight                             13.3693       24.3385   0.549
timeDK/DA                             16.2652       58.0272   0.280
n_offenders2                           3.9493        9.6307   0.410
n_offenders3                           5.1294       15.0231   0.341
n_offenders4                          17.4046       46.1819   0.377
n_offenders5                          29.3976       61.7434   0.476
n_offenders6+                         -2.9931       31.8842  -0.094
n_offendersDK/DA                     -17.6287       19.5043  -0.904
rel_offendersEmployee               -273.7898 18612671.8613   0.000
rel_offendersBarely known             -5.7935       20.4006  -0.284
rel_offendersSomewhat known          -52.0926       41.0229  -1.270
rel_offendersClose acquaintance      -23.0308       33.9461  -0.678
rel_offendersDK/DA                   -23.0164       33.6634  -0.684
had_weaponYes                         15.6990       26.2908   0.597
had_weaponDK/DA                       -4.1508       10.1764  -0.408
with_violenceYes                      -0.7947       27.6187  -0.029
with_violenceDK/DA                    11.2926       16.9289   0.667
reportedYes                           -1.5405       11.0873  -0.139
rep_extortion_victim1                  1.6932       16.2462   0.104
bribe_victim1                         -2.8942       15.5188  -0.186
sizeMedium                            -9.5017       11.8817  -0.800
sizeSmall                             -4.0249       12.0880  -0.333
sizeMicro                            -10.0811       13.2052  -0.763
hotrestbar1                           -4.7919       22.7125  -0.211
yearsquant(8,16]                      -1.6311       14.1181  -0.116
yearsquant(16,25]                      2.6624       11.4796   0.232
yearsquant(25,34]                     -3.8547       15.6888  -0.246
yearsquant(34,43]                      9.3532       12.0005   0.779
                                Pr(>|z|)
(Intercept)                        0.993
extortion_typeInternet             0.904
extortion_typeStreet               0.194
extortion_typePremises             1.000
extortion_typeCobro de piso        0.687
extortion_typeOther                0.189
requestMoney                       0.994
requestNothing                     0.995
requestProduct                     0.994
monthFebruary                      0.283
monthMarch                         0.597
monthApril                         0.226
monthMay                           0.348
monthJune                          0.303
monthJuly                          0.303
monthAugust                        0.277
monthSeptember                     0.331
monthOctober                       0.476
monthNovember                      0.661
monthDecember                      0.342
monthNONE                          0.770
timeAfternoon                      0.940
timeEvening                        0.982
timeNight                          0.583
timeDK/DA                          0.779
n_offenders2                       0.682
n_offenders3                       0.733
n_offenders4                       0.706
n_offenders5                       0.634
n_offenders6+                      0.925
n_offendersDK/DA                   0.366
rel_offendersEmployee              1.000
rel_offendersBarely known          0.776
rel_offendersSomewhat known        0.204
rel_offendersClose acquaintance    0.497
rel_offendersDK/DA                 0.494
had_weaponYes                      0.550
had_weaponDK/DA                    0.683
with_violenceYes                   0.977
with_violenceDK/DA                 0.505
reportedYes                        0.889
rep_extortion_victim1              0.917
bribe_victim1                      0.852
sizeMedium                         0.424
sizeSmall                          0.739
sizeMicro                          0.445
hotrestbar1                        0.833
yearsquant(8,16]                   0.908
yearsquant(16,25]                  0.817
yearsquant(25,34]                  0.806
yearsquant(34,43]                  0.436
```

```

Correlation matrix not shown by default, as p = 51 > 12.
Use print(x, correlation=TRUE)  or
	 vcov(x)	 if you need it
```

```
convergence code: 0
unable to evaluate scaled gradient
Model failed to converge: degenerate  Hessian with 13 negative eigenvalues
failure to converge in 10000 evaluations
```

```r
lrtest(m1_l3_nostate, m1_l3)
```

```
Likelihood ratio test

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + (1 | CVE_UNICA) + 
    (1 | CVE_ENT)
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop + (1 | CVE_UNICA) + (1 | CVE_ENT)
  #Df  LogLik Df  Chisq Pr(>Chisq)
1  53 -112.25                     
2  55 -112.10  2 0.2933     0.8636
```

```r
kable(lrtest(m1_l3_nostate, m1_l3))
```



| #Df|    LogLik| Df|     Chisq| Pr(>Chisq)|
|---:|---------:|--:|---------:|----------:|
|  53| -112.2479| NA|        NA|         NA|
|  55| -112.1012|  2| 0.2933163|  0.8635891|

```r
anova(m1_l3_nostate, m1_l3, test="LRT")
```

```
Data: m1df
Models:
m1_l3_nostate: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l3_nostate:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1_l3_nostate:     bribe_victim + size + hotrestbar + yearsquant + (1 | CVE_UNICA) + 
m1_l3_nostate:     (1 | CVE_ENT)
m1_l3: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l3:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1_l3:     bribe_victim + size + hotrestbar + yearsquant + loghoms + 
m1_l3:     logpop + (1 | CVE_UNICA) + (1 | CVE_ENT)
              Df   AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
m1_l3_nostate 53 330.5 566.54 -112.25    224.5                         
m1_l3         55 334.2 579.15 -112.10    224.2 0.2933      2     0.8636
```

```r
kable(anova(m1_l3_nostate, m1_l3, test="LRT"))
```



|              | Df|      AIC|      BIC|    logLik| deviance|     Chisq| Chi Df| Pr(>Chisq)|
|:-------------|--:|--------:|--------:|---------:|--------:|---------:|------:|----------:|
|m1_l3_nostate | 53| 330.4957| 566.5379| -112.2479| 224.4957|        NA|     NA|         NA|
|m1_l3         | 55| 334.2024| 579.1518| -112.1012| 224.2024| 0.2933163|      2|  0.8635891|

```r
## Only incident level

m1_l3_nostate_nobus <- glmer(complied_bin ~
                  extortion_type + request +
                    month + time +
                    n_offenders + rel_offenders +
                    had_weapon +
                    with_violence +
                    reported +
                    (1|CVE_UNICA) + (1|CVE_ENT),
                  data=m1df,
                  family = "binomial")
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge: degenerate Hessian with 3 negative
eigenvalues
```

```r
summary(m1_l3_nostate_nobus)
```

```
Warning in vcov.merMod(object, use.hessian = use.hessian): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```
Warning in vcov.merMod(object, correlation = correlation, sigm = sig): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```
Generalized linear mixed model fit by maximum likelihood (Laplace
  Approximation) [glmerMod]
 Family: binomial  ( logit )
Formula: 
complied_bin ~ extortion_type + request + month + time + n_offenders +  
    rel_offenders + had_weapon + with_violence + reported + (1 |  
    CVE_UNICA) + (1 | CVE_ENT)
   Data: m1df

     AIC      BIC   logLik deviance df.resid 
   330.7    522.2   -122.4    244.7      592 

Scaled residuals: 
     Min       1Q   Median       3Q      Max 
-0.81480 -0.00418 -0.00034 -0.00001  0.74159 

Random effects:
 Groups    Name        Variance Std.Dev.
 CVE_UNICA (Intercept) 215.7433 14.6882 
 CVE_ENT   (Intercept)   0.1482  0.3849 
Number of obs: 635, groups:  CVE_UNICA, 522; CVE_ENT, 32

Fixed effects:
                                Estimate Std. Error z value Pr(>|z|)  
(Intercept)                      -4.8741    21.7984  -0.224   0.8231  
extortion_typeInternet            8.5434    19.0168   0.449   0.6532  
extortion_typeStreet              2.0246     7.0963   0.285   0.7754  
extortion_typePremises           59.6311    52.0068   1.147   0.2515  
extortion_typeCobro de piso       9.2433    12.2057   0.757   0.4489  
extortion_typeOther              15.0044     8.5675   1.751   0.0799 .
requestMoney                     -2.3806    20.2451  -0.118   0.9064  
requestNothing                   -1.6984    20.1309  -0.084   0.9328  
requestProduct                    0.7434    24.2583   0.031   0.9756  
monthFebruary                   -15.8439    10.1126  -1.567   0.1172  
monthMarch                       -2.7600     9.1662  -0.301   0.7633  
monthApril                       -0.3619    10.1179  -0.036   0.9715  
monthMay                         -3.6151    10.8498  -0.333   0.7390  
monthJune                         0.2999     7.3272   0.041   0.9673  
monthJuly                        -8.2302    11.1556  -0.738   0.4607  
monthAugust                      -9.9956    12.4933  -0.800   0.4237  
monthSeptember                   -1.4546     7.6180  -0.191   0.8486  
monthOctober                    -14.6709    12.9838  -1.130   0.2585  
monthNovember                    -0.4470     7.3067  -0.061   0.9512  
monthDecember                    -3.7988     9.8312  -0.386   0.6992  
monthNONE                       -14.1321    12.4753  -1.133   0.2573  
timeAfternoon                    -5.9768     4.9559  -1.206   0.2278  
timeEvening                      -0.4865     6.4464  -0.076   0.9398  
timeNight                        -1.3781    16.7915  -0.082   0.9346  
timeDK/DA                         9.2215    21.4954   0.429   0.6679  
n_offenders2                      1.1499     5.0692   0.227   0.8206  
n_offenders3                      0.8516     8.9220   0.095   0.9240  
n_offenders4                      3.4663    12.3229   0.281   0.7785  
n_offenders5                      2.9003    18.5393   0.156   0.8757  
n_offenders6+                   -12.4306    13.3053  -0.934   0.3502  
n_offendersDK/DA                 -0.6598     9.9930  -0.066   0.9474  
rel_offendersEmployee           -39.0517   166.7362  -0.234   0.8148  
rel_offendersBarely known         3.6762     8.8254   0.417   0.6770  
rel_offendersSomewhat known     -12.2108    22.1352  -0.552   0.5812  
rel_offendersClose acquaintance -39.1624    19.8384  -1.974   0.0484 *
rel_offendersDK/DA               -7.0094    14.8486  -0.472   0.6369  
had_weaponYes                    11.5644     9.0488   1.278   0.2012  
had_weaponDK/DA                  -0.2666     5.4036  -0.049   0.9606  
with_violenceYes                  3.3030     9.2665   0.356   0.7215  
with_violenceDK/DA               -4.2129    10.3146  -0.408   0.6829  
reportedYes                      -0.4935     5.8261  -0.085   0.9325  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```

Correlation matrix not shown by default, as p = 41 > 12.
Use print(x, correlation=TRUE)  or
	 vcov(x)	 if you need it
```

```
convergence code: 0
unable to evaluate scaled gradient
Model failed to converge: degenerate  Hessian with 3 negative eigenvalues
failure to converge in 10000 evaluations
```

```r
lrtest(m1_l3_nostate_nobus, m1_l3_nostate)
```

```
Likelihood ratio test

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + (1 | 
    CVE_UNICA) + (1 | CVE_ENT)
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + (1 | CVE_UNICA) + 
    (1 | CVE_ENT)
  #Df  LogLik Df  Chisq Pr(>Chisq)  
1  43 -122.36                       
2  53 -112.25 10 20.235    0.02711 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(lrtest(m1_l3_nostate_nobus, m1_l3_nostate))
```



| #Df|    LogLik| Df|    Chisq| Pr(>Chisq)|
|---:|---------:|--:|--------:|----------:|
|  43| -122.3654| NA|       NA|         NA|
|  53| -112.2479| 10| 20.23506|  0.0271062|

```r
anova(m1_l3_nostate_nobus, m1_l3_nostate, test="LRT")
```

```
Data: m1df
Models:
m1_l3_nostate_nobus: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l3_nostate_nobus:     rel_offenders + had_weapon + with_violence + reported + (1 | 
m1_l3_nostate_nobus:     CVE_UNICA) + (1 | CVE_ENT)
m1_l3_nostate: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l3_nostate:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1_l3_nostate:     bribe_victim + size + hotrestbar + yearsquant + (1 | CVE_UNICA) + 
m1_l3_nostate:     (1 | CVE_ENT)
                    Df    AIC    BIC  logLik deviance  Chisq Chi Df
m1_l3_nostate_nobus 43 330.73 522.24 -122.36   244.73              
m1_l3_nostate       53 330.50 566.54 -112.25   224.50 20.235     10
                    Pr(>Chisq)  
m1_l3_nostate_nobus             
m1_l3_nostate          0.02711 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(anova(m1_l3_nostate_nobus, m1_l3_nostate, test="LRT"))
```



|                    | Df|      AIC|      BIC|    logLik| deviance|    Chisq| Chi Df| Pr(>Chisq)|
|:-------------------|--:|--------:|--------:|---------:|--------:|--------:|------:|----------:|
|m1_l3_nostate_nobus | 43| 330.7308| 522.2367| -122.3654| 244.7308|       NA|     NA|         NA|
|m1_l3_nostate       | 53| 330.4957| 566.5379| -112.2479| 224.4957| 20.23506|     10|  0.0271062|

```r
lrtest(m1_l3_nostate_nobus, m1_l3_nostate, m1_l3)
```

```
Likelihood ratio test

Model 1: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + (1 | 
    CVE_UNICA) + (1 | CVE_ENT)
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + (1 | CVE_UNICA) + 
    (1 | CVE_ENT)
Model 3: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop + (1 | CVE_UNICA) + (1 | CVE_ENT)
  #Df  LogLik Df   Chisq Pr(>Chisq)  
1  43 -122.36                        
2  53 -112.25 10 20.2351    0.02711 *
3  55 -112.10  2  0.2933    0.86359  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(lrtest(m1_l3_nostate_nobus, m1_l3_nostate, m1_l3))
```



| #Df|    LogLik| Df|      Chisq| Pr(>Chisq)|
|---:|---------:|--:|----------:|----------:|
|  43| -122.3654| NA|         NA|         NA|
|  53| -112.2479| 10| 20.2350634|  0.0271062|
|  55| -112.1012|  2|  0.2933163|  0.8635891|

```r
anova(m1_l3_nostate_nobus, m1_l3_nostate, m1_l3, test="LRT")
```

```
Data: m1df
Models:
m1_l3_nostate_nobus: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l3_nostate_nobus:     rel_offenders + had_weapon + with_violence + reported + (1 | 
m1_l3_nostate_nobus:     CVE_UNICA) + (1 | CVE_ENT)
m1_l3_nostate: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l3_nostate:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1_l3_nostate:     bribe_victim + size + hotrestbar + yearsquant + (1 | CVE_UNICA) + 
m1_l3_nostate:     (1 | CVE_ENT)
m1_l3: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l3:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1_l3:     bribe_victim + size + hotrestbar + yearsquant + loghoms + 
m1_l3:     logpop + (1 | CVE_UNICA) + (1 | CVE_ENT)
                    Df    AIC    BIC  logLik deviance   Chisq Chi Df
m1_l3_nostate_nobus 43 330.73 522.24 -122.36   244.73               
m1_l3_nostate       53 330.50 566.54 -112.25   224.50 20.2351     10
m1_l3               55 334.20 579.15 -112.10   224.20  0.2933      2
                    Pr(>Chisq)  
m1_l3_nostate_nobus             
m1_l3_nostate          0.02711 *
m1_l3                  0.86359  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(anova(m1_l3_nostate_nobus, m1_l3_nostate, m1_l3, test="LRT"))
```



|                    | Df|      AIC|      BIC|    logLik| deviance|      Chisq| Chi Df| Pr(>Chisq)|
|:-------------------|--:|--------:|--------:|---------:|--------:|----------:|------:|----------:|
|m1_l3_nostate_nobus | 43| 330.7308| 522.2367| -122.3654| 244.7308|         NA|     NA|         NA|
|m1_l3_nostate       | 53| 330.4957| 566.5379| -112.2479| 224.4957| 20.2350634|     10|  0.0271062|
|m1_l3               | 55| 334.2024| 579.1518| -112.1012| 224.2024|  0.2933163|      2|  0.8635891|

```r
lrtest(m1_l3_null, m1_l3_nostate_nobus, m1_l3_nostate, m1_l3)
```

```
Likelihood ratio test

Model 1: complied_bin ~ (1 | CVE_UNICA) + (1 | CVE_ENT)
Model 2: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + (1 | 
    CVE_UNICA) + (1 | CVE_ENT)
Model 3: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + (1 | CVE_UNICA) + 
    (1 | CVE_ENT)
Model 4: complied_bin ~ extortion_type + request + month + time + n_offenders + 
    rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
    bribe_victim + size + hotrestbar + yearsquant + loghoms + 
    logpop + (1 | CVE_UNICA) + (1 | CVE_ENT)
  #Df  LogLik Df   Chisq Pr(>Chisq)  
1   3 -115.73                        
2  43 -122.36 40 13.2623    0.99998  
3  53 -112.25 10 20.2351    0.02711 *
4  55 -112.10  2  0.2933    0.86359  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(lrtest(m1_l3_null, m1_l3_nostate_nobus, m1_l3_nostate, m1_l3))
```



| #Df|    LogLik| Df|      Chisq| Pr(>Chisq)|
|---:|---------:|--:|----------:|----------:|
|   3| -115.7342| NA|         NA|         NA|
|  43| -122.3654| 40| 13.2623283|  0.9999788|
|  53| -112.2479| 10| 20.2350634|  0.0271062|
|  55| -112.1012|  2|  0.2933163|  0.8635891|

```r
anova(m1_l3_null, m1_l3_nostate_nobus, m1_l3_nostate, m1_l3, test="LRT")
```

```
Data: m1df
Models:
m1_l3_null: complied_bin ~ (1 | CVE_UNICA) + (1 | CVE_ENT)
m1_l3_nostate_nobus: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l3_nostate_nobus:     rel_offenders + had_weapon + with_violence + reported + (1 | 
m1_l3_nostate_nobus:     CVE_UNICA) + (1 | CVE_ENT)
m1_l3_nostate: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l3_nostate:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1_l3_nostate:     bribe_victim + size + hotrestbar + yearsquant + (1 | CVE_UNICA) + 
m1_l3_nostate:     (1 | CVE_ENT)
m1_l3: complied_bin ~ extortion_type + request + month + time + n_offenders + 
m1_l3:     rel_offenders + had_weapon + with_violence + reported + rep_extortion_victim + 
m1_l3:     bribe_victim + size + hotrestbar + yearsquant + loghoms + 
m1_l3:     logpop + (1 | CVE_UNICA) + (1 | CVE_ENT)
                    Df    AIC    BIC  logLik deviance   Chisq Chi Df
m1_l3_null           3 237.47 250.83 -115.73   231.47               
m1_l3_nostate_nobus 43 330.73 522.24 -122.36   244.73  0.0000     40
m1_l3_nostate       53 330.50 566.54 -112.25   224.50 20.2351     10
m1_l3               55 334.20 579.15 -112.10   224.20  0.2933      2
                    Pr(>Chisq)  
m1_l3_null                      
m1_l3_nostate_nobus    1.00000  
m1_l3_nostate          0.02711 *
m1_l3                  0.86359  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
kable(anova(m1_l3_null, m1_l3_nostate_nobus, m1_l3_nostate,
              m1_l3, test="LRT"))
```



|                    | Df|      AIC|      BIC|    logLik| deviance|      Chisq| Chi Df| Pr(>Chisq)|
|:-------------------|--:|--------:|--------:|---------:|--------:|----------:|------:|----------:|
|m1_l3_null          |  3| 237.4685| 250.8294| -115.7342| 231.4685|         NA|     NA|         NA|
|m1_l3_nostate_nobus | 43| 330.7308| 522.2367| -122.3654| 244.7308|  0.0000000|     40|  1.0000000|
|m1_l3_nostate       | 53| 330.4957| 566.5379| -112.2479| 224.4957| 20.2350634|     10|  0.0271062|
|m1_l3               | 55| 334.2024| 579.1518| -112.1012| 224.2024|  0.2933163|      2|  0.8635891|

```r
## without the vars with a lot of missing

m1_l3_no_viol_off_wea <- glmer(complied_bin ~
                          extortion_type + request +
                          month + time +
                          reported +
                          (1|CVE_UNICA) + (1|CVE_ENT),
                        data=m1df,
                        family = "binomial")
```

```
Error in pwrssUpdate(pp, resp, tol = tolPwrss, GQmat = GQmat, compDev = compDev, : Downdated VtV is not positive definite
```

```r
summary(m1_l3_no_viol_off_wea)
```

```
Error in summary(m1_l3_no_viol_off_wea): object 'm1_l3_no_viol_off_wea' not found
```

```r
lrtest(m1_l3_no_viol_off_wea, m1_l3_nostate_nobus)
```

```
Error in lrtest(m1_l3_no_viol_off_wea, m1_l3_nostate_nobus): object 'm1_l3_no_viol_off_wea' not found
```

```r
kable(lrtest(m1_l3_no_viol_off_wea, m1_l3_nostate_nobus))
```

```
Error in lrtest(m1_l3_no_viol_off_wea, m1_l3_nostate_nobus): object 'm1_l3_no_viol_off_wea' not found
```

```r
anova(m1_l3_no_viol_off_wea, m1_l3_nostate_nobus, test="LRT")
```

```
Error in anova(m1_l3_no_viol_off_wea, m1_l3_nostate_nobus, test = "LRT"): object 'm1_l3_no_viol_off_wea' not found
```

```r
kable(anova(m1_l3_no_viol_off_wea, m1_l3_nostate_nobus, test="LRT"))
```

```
Error in anova(m1_l3_no_viol_off_wea, m1_l3_nostate_nobus, test = "LRT"): object 'm1_l3_no_viol_off_wea' not found
```

```r
lrtest(m1_l3_no_viol_off_wea, m1_l3_nostate_nobus, m1_l3_nostate, m1_l3)
```

```
Error in lrtest(m1_l3_no_viol_off_wea, m1_l3_nostate_nobus, m1_l3_nostate, : object 'm1_l3_no_viol_off_wea' not found
```

```r
kable(lrtest(m1_l3_no_viol_off_wea, m1_l3_nostate_nobus,
             m1_l3_nostate, m1_l3))
```

```
Error in lrtest(m1_l3_no_viol_off_wea, m1_l3_nostate_nobus, m1_l3_nostate, : object 'm1_l3_no_viol_off_wea' not found
```

```r
anova(m1_l3_no_viol_off_wea, m1_l3_nostate_nobus,
         m1_l3_nostate, m1_l3, test="LRT")
```

```
Error in anova(m1_l3_no_viol_off_wea, m1_l3_nostate_nobus, m1_l3_nostate, : object 'm1_l3_no_viol_off_wea' not found
```

```r
kable(anova(m1_l3_no_viol_off_wea, m1_l3_nostate_nobus,
               m1_l3_nostate, m1_l3, test="LRT"))
```

```
Error in anova(m1_l3_no_viol_off_wea, m1_l3_nostate_nobus, m1_l3_nostate, : object 'm1_l3_no_viol_off_wea' not found
```

```r
lrtest(m1_l3_null, m1_l3_no_viol_off_wea, m1_l3_nostate_nobus,
        m1_l3_nostate, m1_l3)
```

```
Error in lrtest.default(m1_l3_null, m1_l3_no_viol_off_wea, m1_l3_nostate_nobus, : object 'm1_l3_no_viol_off_wea' not found
```

```r
kable(lrtest(m1_l3, m1_l3_no_viol_off_wea, m1_l3_nostate_nobus,
                m1_l3_nostate, m1_l3))
```

```
Error in lrtest.default(m1_l3, m1_l3_no_viol_off_wea, m1_l3_nostate_nobus, : object 'm1_l3_no_viol_off_wea' not found
```

```r
anova(m1_l3_null, m1_l3_no_viol_off_wea, m1_l3_nostate_nobus,
        m1_l3_nostate, m1_l3, test="LRT")
```

```
Error in anova.merMod(m1_l3_null, m1_l3_no_viol_off_wea, m1_l3_nostate_nobus, : object 'm1_l3_no_viol_off_wea' not found
```

```r
kable(anova(m1_l3_null, m1_l3_no_viol_off_wea, m1_l3_nostate_nobus,
        m1_l3_nostate, m1_l3, test="LRT"))
```

```
Error in anova.merMod(m1_l3_null, m1_l3_no_viol_off_wea, m1_l3_nostate_nobus, : object 'm1_l3_no_viol_off_wea' not found
```

```r
htmlreg(list(m1_l3,sm1_l3, m1_l3_nostate, m1_l3_nostate_nobus,
             m1_l3_no_viol_off_wea, m1_l3_null))
```

```
Error in match(x, table, nomatch = 0L): object 'sm1_l3' not found
```

```r
screenreg(list(m1_l3,sm1_l3, m1_l3_nostate, m1_l3_nostate_nobus,
               m1_l3_no_viol_off_wea, m1_l3_null))
```

```
Error in match(x, table, nomatch = 0L): object 'sm1_l3' not found
```


 FOR LATER: the least missing excluding "with violence" and "offender" and "weapons" variables.



```r
endtime <- proc.time()
time <- endtime - starttime
time
```

```
    user   system  elapsed 
5436.826  105.503 5487.583 
```

```r
print(paste("the script took", round(time[3]/60,2),
              "minutes to run.", sep=" "))
```

```
[1] "the script took 91.46 minutes to run."
```
