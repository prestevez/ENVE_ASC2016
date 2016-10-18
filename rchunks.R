###############################################################################
# packages

library(foreign)
library(ggplot2)
library(Cairo)
library(knitr)
library(lme4)
library(classInt)
library(car)
library(texreg)
library(xtable)
library(lmtest)
library(pscl)
library(sjstats)
library(sjPlot)

###############################################################################
# {r victim-level}

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
levels(enve_test$extortion_victim) <- c(0,
                      rep(1, length(levels(enve_test$extortion_victim)) - 1))

enve_test$rep_extortion_victim <- factor(enve_test$extortions)
levels(enve_test$rep_extortion_victim) <- c(0, 0,
                    rep(1, length(levels(enve_test$rep_extortion_victim)) - 2))

enve_test$CVE_UNICA <- as.integer(as.character(enve_all$CVE_UNICA))

enve_test$bribes <- as.integer(as.character(enve_all$P33))
enve_test$bribe_victim <- factor(enve_test$bribes)
levels(enve_test$bribe_victim) <- c(0,
                  rep(1, length(levels(enve_test$bribe_victim)) - 1))

enve_test$rep_bribe <- factor(enve_test$bribes)
levels(enve_test$rep_bribe) <- c(0, 0, rep(1,
                        length(levels(enve_test$rep_bribe)) - 2))

enve_test$bribe_cats <- factor(enve_test$bribes)
levels(enve_test$bribe_cats) <- c(0, 1, 2, rep("3+",
                          length(levels(enve_test$bribe_cats)) - 3))

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
length(enve_test$bribes[is.na(enve_test$bribes)])

enve_test$extortions[is.na(enve_test$extortions)] <- 0
enve_test$bribes[is.na(enve_test$bribes)] <- 0



###############################################################################

# {r incident-level}

enve_incidents_all <- read.dbf("TR_ENVE_DELITOS2014.dbf")

# Selecting only those relevant for extortion (code 10)

enve_incidents_all$delito <- as.integer(as.character(enve_incidents_all$ID_DELITO))

enve_incidents <- enve_incidents_all[enve_incidents_all$delito == 10,]

# Selecting those relevant for our study

incident_df <- data.frame(CVE_UNICA=as.integer(
  as.character(enve_incidents$CVE_UNICA)))

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

###############################################################################

# {r incident-victim-merge}

enve_incvic <- merge(incident_df, enve_test, by="CVE_UNICA")

###############################################################################

# EDA

###############################################################################
# {r summary}
summary(enve_incvic)

# {r compliance-v-months}

## basic summary first

t0 <- table(enve_incvic$month, useNA="ifany")
t0
round(t0/sum(t0)*100, 2)

## first No NA then with NA

## Use complied_bin

t1 <- table(enve_incvic$month, enve_incvic$complied_bin)
t1
chisq.test(t1)
chisq.test(t1,simulate.p.value = TRUE, B = 9999)
kable(t1)

t1/as.integer(margin.table(t1, margin=1))*100
kable(round(t1/as.integer(margin.table(t1, margin=1))*100, 2))

t2 <- table(enve_incvic$month, enve_incvic$complied_bin, useNA = "ifany")
t2
chisq.test(t2)
chisq.test(t2,simulate.p.value = TRUE, B = 9999)
kable(t2)

t2/as.integer(margin.table(t2, margin=1))*100
kable(round(t2/as.integer(margin.table(t2, margin=1))*100, 2))


# {r ext_type-v-months}

t3 <- table(enve_incvic$extortion_type, enve_incvic$month)
t3
kable(t3)

t3/as.integer(margin.table(t3, margin=1))*100
kable(round(t3/as.integer(margin.table(t3, margin=1))*100, 2))

t3.1 <- table(enve_incvic$extortion_type, enve_incvic$month, useNA="ifany")
t3.1
kable(t3.1)

t3.1/as.integer(margin.table(t3.1, margin=1))*100
kable(round(t3.1/as.integer(margin.table(t3.1, margin=1))*100, 2))


# {r compliance-v-time}

## basic summary first

t4 <- table(enve_incvic$time, useNA="ifany")
t4
round(t4/sum(t4)*100, 2)

## first No NA then with NA

## Use complied_bin

t5 <- table(enve_incvic$time, enve_incvic$complied_bin)
t5
kable(t5)

t5/as.integer(margin.table(t5, margin=1))*100
kable(round(t5/as.integer(margin.table(t5, margin=1))*100, 2))

t6 <- table(enve_incvic$time, enve_incvic$complied_bin, useNA = "ifany")
t6
kable(t6)

t6/as.integer(margin.table(t6, margin=1))*100
kable(round(t6/as.integer(margin.table(t6, margin=1))*100, 2))


# {r ext_type-v-time}

t7 <- table(enve_incvic$extortion_type, enve_incvic$time)
t7
kable(t7)

t7/as.integer(margin.table(t7, margin=1))*100
kable(round(t7/as.integer(margin.table(t7, margin=1))*100, 2))

t7.1 <- table(enve_incvic$extortion_type, enve_incvic$time, useNA="inany")
t7.1
kable(t7.1)

t7.1/as.integer(margin.table(t7.1, margin=1))*100
kable(round(t7.1/as.integer(margin.table(t7.1, margin=1))*100, 2))

# {r compliance-v-numoffenders}

## basic summary first

t8 <- table(enve_incvic$n_offenders, useNA="ifany")
t8
round(t8/sum(t8)*100, 2)

## first No NA then with NA

## Use complied_bin

t9 <- table(enve_incvic$n_offenders, enve_incvic$complied_bin)
t9
kable(t9)

t9/as.integer(margin.table(t9, margin=1))*100
kable(round(t9/as.integer(margin.table(t9, margin=1))*100, 2))

t10 <- table(enve_incvic$n_offenders, enve_incvic$complied_bin, useNA = "ifany")
t10
kable(t10)

t10/as.integer(margin.table(t10, margin=1))*100
kable(round(t10/as.integer(margin.table(t10, margin=1))*100, 2))


# {r ext_type-v-numoffenders}

t11 <- table(enve_incvic$extortion_type, enve_incvic$n_offenders)
t11
kable(t11)

t11/as.integer(margin.table(t11, margin=1))*100
kable(round(t11/as.integer(margin.table(t11, margin=1))*100, 2))

t12 <- table(enve_incvic$extortion_type, enve_incvic$n_offenders, useNA="ifany")
t12
kable(t12)

t12/as.integer(margin.table(t12, margin=1))*100
kable(round(t12/as.integer(margin.table(t12, margin=1))*100, 2))


# {r compliance-v-relationship}

## basic summary first

t13 <- table(enve_incvic$rel_offenders, useNA="ifany")
t13
round(t13/sum(t13)*100, 2)

## first No NA then with NA

## Use complied_bin

t14 <- table(enve_incvic$rel_offenders, enve_incvic$complied_bin)
t14
kable(t14)

t14/as.integer(margin.table(t14, margin=1))*100
kable(round(t14/as.integer(margin.table(t14, margin=1))*100, 2))

t15 <- table(enve_incvic$rel_offenders, enve_incvic$complied_bin, useNA = "ifany")
t15
kable(t15)

t15/as.integer(margin.table(t15, margin=1))*100
kable(round(t15/as.integer(margin.table(t15, margin=1))*100, 2))


# {r ext_type-v-relationship}

t16 <- table(enve_incvic$extortion_type, enve_incvic$rel_offenders)
t16
kable(t16)

t16/as.integer(margin.table(t16, margin=1))*100
kable(round(t16/as.integer(margin.table(t16, margin=1))*100, 2))

t17 <- table(enve_incvic$extortion_type, enve_incvic$rel_offenders, useNA="ifany")
t17
kable(t17)

t17/as.integer(margin.table(t17, margin=1))*100
kable(round(t17/as.integer(margin.table(t17, margin=1))*100, 2))


# {r compliance-v-weaponuse}

## basic summary first

t18 <- table(enve_incvic$had_weapon, useNA="ifany")
t18
round(t18/sum(t18)*100, 2)

## first No NA then with NA

## Use complied_bin

t19 <- table(enve_incvic$had_weapon, enve_incvic$complied_bin)
t19
kable(t19)

t19/as.integer(margin.table(t19, margin=1))*100
kable(round(t19/as.integer(margin.table(t19, margin=1))*100, 2))

t20 <- table(enve_incvic$had_weapon, enve_incvic$complied_bin, useNA = "ifany")
t20
kable(t20)

t20/as.integer(margin.table(t20, margin=1))*100
kable(round(t20/as.integer(margin.table(t20, margin=1))*100, 2))


# {r ext_type-v-weaponuse}

t21 <- table(enve_incvic$extortion_type, enve_incvic$had_weapon)
t21
kable(t21)

t21/as.integer(margin.table(t21, margin=1))*100
kable(round(t21/as.integer(margin.table(t21, margin=1))*100, 2))

t22 <- table(enve_incvic$extortion_type, enve_incvic$had_weapon, useNA="ifany")
t22
kable(t22)

t22/as.integer(margin.table(t22, margin=1))*100
kable(round(t22/as.integer(margin.table(t22, margin=1))*100, 2))

# {r compliance-v-weapontype}

## basic summary first

t23 <- table(enve_incvic$weapon_type, useNA="ifany")
t23
round(t23/sum(t23)*100, 2)

## first No NA then with NA

## Use complied_bin

t24 <- table(enve_incvic$weapon_type, enve_incvic$complied_bin)
t24
kable(t24)

t24/as.integer(margin.table(t24, margin=1))*100
kable(round(t24/as.integer(margin.table(t24, margin=1))*100, 2))

t25 <- table(enve_incvic$weapon_type, enve_incvic$complied_bin, useNA = "ifany")
t25
kable(t25)

t25/as.integer(margin.table(t25, margin=1))*100
kable(round(t25/as.integer(margin.table(t25, margin=1))*100, 2))


# {r ext_type-v-weapontype}

t26 <- table(enve_incvic$extortion_type, enve_incvic$weapon_type)
t26
kable(t26)

t26/as.integer(margin.table(t26, margin=1))*100
kable(round(t26/as.integer(margin.table(t26, margin=1))*100, 2))

t27 <- table(enve_incvic$extortion_type, enve_incvic$weapon_type, useNA="ifany")
t27
kable(t27)

t27/as.integer(margin.table(t27, margin=1))*100
kable(round(t27/as.integer(margin.table(t27, margin=1))*100, 2))


# {r compliance-v-violence}

## basic summary first

t28 <- table(enve_incvic$with_violence, useNA="ifany")
t28
round(t28/sum(t28)*100, 2)

## first No NA then with NA

## Use complied_bin

t29 <- table(enve_incvic$with_violence, enve_incvic$complied_bin)
t29
kable(t29)

t29/as.integer(margin.table(t29, margin=1))*100
kable(round(t29/as.integer(margin.table(t29, margin=1))*100, 2))

t30 <- table(enve_incvic$with_violence, enve_incvic$complied_bin, useNA = "ifany")
t30
kable(t30)

t30/as.integer(margin.table(t30, margin=1))*100
kable(round(t30/as.integer(margin.table(t30, margin=1))*100, 2))


# {r ext_type-v-violence}

t31 <- table(enve_incvic$extortion_type, enve_incvic$with_violence)
t31
kable(t31)

t31/as.integer(margin.table(t31, margin=1))*100
kable(round(t31/as.integer(margin.table(t31, margin=1))*100, 2))

t32 <- table(enve_incvic$extortion_type, enve_incvic$with_violence, useNA="ifany")
t32
kable(t32)

t32/as.integer(margin.table(t32, margin=1))*100
kable(round(t32/as.integer(margin.table(t32, margin=1))*100, 2))


# {r compliance-v-reported}

## basic summary first

t33 <- table(enve_incvic$reported, useNA="ifany")
t33
round(t33/sum(t33)*100, 2)

## first No NA then with NA

## Use complied_bin

t34 <- table(enve_incvic$reported, enve_incvic$complied_bin)
t34
kable(t34)

t34/as.integer(margin.table(t34, margin=1))*100
kable(round(t34/as.integer(margin.table(t34, margin=1))*100, 2))

t35 <- table(enve_incvic$reported, enve_incvic$complied_bin, useNA = "ifany")
t35
kable(t35)

t35/as.integer(margin.table(t35, margin=1))*100
kable(round(t35/as.integer(margin.table(t35, margin=1))*100, 2))


# {r ext_type-v-reported}

t36 <- table(enve_incvic$extortion_type, enve_incvic$reported)
t36
kable(t36)

t36/as.integer(margin.table(t36, margin=1))*100
kable(round(t36/as.integer(margin.table(t36, margin=1))*100, 2))

t37 <- table(enve_incvic$extortion_type, enve_incvic$reported, useNA="ifany")
t37
kable(t37)

t37/as.integer(margin.table(t37, margin=1))*100
kable(round(t37/as.integer(margin.table(t37, margin=1))*100, 2))

# {r compliance-v-requested}

## basic summary first

t38 <- table(enve_incvic$request, useNA="ifany")
t38
round(t38/sum(t38)*100, 2)

## first No NA then with NA

## Use complied_bin

t39 <- table(enve_incvic$request, enve_incvic$complied_bin)
t39
kable(t39)

t39/as.integer(margin.table(t39, margin=1))*100
kable(round(t39/as.integer(margin.table(t39, margin=1))*100, 2))

t40 <- table(enve_incvic$request, enve_incvic$complied_bin, useNA = "ifany")
t40
kable(t40)

t40/as.integer(margin.table(t40, margin=1))*100
kable(round(t40/as.integer(margin.table(t40, margin=1))*100, 2))


# {r ext_type-v-requested}

t41 <- table(enve_incvic$extortion_type, enve_incvic$request)
t41
kable(t41)

t41/as.integer(margin.table(t41, margin=1))*100
kable(round(t41/as.integer(margin.table(t41, margin=1))*100, 2))

t42 <- table(enve_incvic$extortion_type, enve_incvic$request, useNA="ifany")
t42
kable(t42)

t42/as.integer(margin.table(t42, margin=1))*100
kable(round(t42/as.integer(margin.table(t42, margin=1))*100, 2))


# {r compliance-v-retaliation}

## basic summary first

t43 <- table(enve_incvic$retaliation, useNA="ifany")
t43
round(t43/sum(t43)*100, 2)

t44 <- table(enve_incvic$retaliation, useNA="no")
t44
round(t44/sum(t44)*100, 2)

## first No NA then with NA

## Use complied_bin

t45 <- table(enve_incvic$retaliation, enve_incvic$complied_bin)
t45
kable(t45)

t45/as.integer(margin.table(t45, margin=1))*100
kable(round(t45/as.integer(margin.table(t45, margin=1))*100, 2))

t46 <- table(enve_incvic$retaliation, enve_incvic$complied_bin, useNA = "ifany")
t46
kable(t46)

t46/as.integer(margin.table(t46, margin=1))*100
kable(round(t46/as.integer(margin.table(t46, margin=1))*100, 2))


# {r ext_type-v-retaliation}

t47 <- table(enve_incvic$extortion_type, enve_incvic$retaliation)
t47
kable(t47)

t47/as.integer(margin.table(t47, margin=1))*100
kable(round(t47/as.integer(margin.table(t47, margin=1))*100, 2))

t48 <- table(enve_incvic$extortion_type, enve_incvic$retaliation, useNA="ifany")
t48
kable(t48)

t48/as.integer(margin.table(t48, margin=1))*100
kable(round(t48/as.integer(margin.table(t48, margin=1))*100, 2))


#####################################################

# {r univariate-compliance}

t49 <- table(enve_incvic$complied, useNA="ifany")
t49
round(t49/sum(t49)*100, 2)

t50 <- table(enve_incvic$complied, useNA="no")
t50
round(t50/sum(t50)*100, 2)

t51 <- table(enve_incvic$complied_bin, useNA="ifany")
t51
round(t51/sum(t51)*100, 2)

t52 <- table(enve_incvic$complied_bin, useNA="no")
t52
round(t52/sum(t52)*100, 2)


# {r univariate-ext_type}

t53 <- table(enve_incvic$extortion_type, useNA="ifany")
t53
round(t53/sum(t53)*100, 2)

t54 <- table(enve_incvic$extortion_type, useNA="no")
t54
round(t54/sum(t54)*100, 2)

# {r complied-v-ext_type}

t55 <- table(enve_incvic$extortion_type, enve_incvic$complied_bin)
t55
kable(t55)

t55/as.integer(margin.table(t55, margin=1))*100
kable(round(t55/as.integer(margin.table(t55, margin=1))*100, 2))

t56 <- table(enve_incvic$extortion_type, enve_incvic$complied_bin, useNA="ifany")
t56
kable(t56)

t56/as.integer(margin.table(t56, margin=1))*100
kable(round(t56/as.integer(margin.table(t56, margin=1))*100, 2))


#####################################################
# Complete cases

# {r complete-cases}

### total rows of complete cases and excluding certain variables

a <- "complete cases"

paste(length(which(complete.cases(enve_incvic) == TRUE)), a)

## Subsets

### those for sure excluded

excnames <- quote(c(gun,knife,blunt,other,violenceI,violenceII,mp,auto))

paste(length(which(complete.cases(subset(enve_incvic,
                                         select=-eval(excnames))) == TRUE)), a)

### excluding retailiation too

excnames <- quote(c(gun,knife,blunt,other,violenceI,violenceII,mp,auto,retaliation))

paste(length(which(complete.cases(subset(enve_incvic,
                                         select=-eval(excnames))) == TRUE)), a)

### excluding offender related variables

excnames <- quote(c(gun,knife,blunt,other,violenceI,violenceII,mp,auto,
                    retaliation,n_offenders,rel_offenders))

paste(length(which(complete.cases(subset(enve_incvic,
                                         select=-eval(excnames))) == TRUE)), a)

### excluding weapons

excnames <- quote(c(gun,knife,blunt,other,violenceI,violenceII,mp,auto,
                    retaliation,weapon_type))

paste(length(which(complete.cases(subset(enve_incvic,
                                         select=-eval(excnames))) == TRUE)), a)

excnames <- quote(c(gun,knife,blunt,other,violenceI,violenceII,mp,auto,
                    retaliation,weapon_type,had_weapon))

paste(length(which(complete.cases(subset(enve_incvic,
                                         select=-eval(excnames))) == TRUE)), a)


### excluding weapons and offender related

excnames <- quote(c(gun,knife,blunt,other,violenceI,violenceII,mp,auto,
                    retaliation,n_offenders,rel_offenders,weapon_type))

paste(length(which(complete.cases(subset(enve_incvic,
                                         select=-eval(excnames))) == TRUE)), a)

excnames <- quote(c(gun,knife,blunt,other,violenceI,violenceII,mp,auto,
                    retaliation,n_offenders,rel_offenders,weapon_type,had_weapon))

paste(length(which(complete.cases(subset(enve_incvic,
                                         select=-eval(excnames))) == TRUE)), a)


### excluding with violence

excnames <- quote(c(gun,knife,blunt,other,violenceI,violenceII,mp,auto,
                    retaliation,with_violence))

paste(length(which(complete.cases(subset(enve_incvic,
                                         select=-eval(excnames))) == TRUE)), a)


excnames <- quote(c(gun,knife,blunt,other,violenceI,violenceII,mp,auto,
                    retaliation,n_offenders,rel_offenders,weapon_type,
                    with_violence,had_weapon))

paste(length(which(complete.cases(subset(enve_incvic,
                                         select=-eval(excnames))) == TRUE)), a)


### Chisq tests for all {r chisq-incident}

#chisq.test(get(paste("t",2,sep="")))

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



#cv.test = function(df) {
#  CV = sqrt(chisq.test(df)$statistic /
#              (sum(df) * (min(ncol(df),nrow(df)) - 1)))
#  return(as.numeric(CV))
#}


#####################################################
# Now lets describe the variables at the business-level


### trying the groupedData approach

library(nlme)

names(enve_incvic)

enve_grpd <- groupedData(complied_bin ~ month | factor(CVE_UNICA)/NOM_ABR,
                         data=enve_incvic,
                         inner=list(~time+n_offenders+had_weapon+weapon_type+
                                    with_violence+reported+extortion_type+
                                    request+retaliation),
                         outer = list(~denuncias_homs+poblacion+tasahom+loghoms+
                                        logpop+tasahom_cntr+logtasa+NOM_ABR,
                                      ~extortions+extortion_victim+
                           rep_extortion_victim+bribes+bribe_victim+
                           rep_bribe+bribe_cats+size+sector+subsector+
                           hotrestbar+years+yearsquant), order.groups = F)

summary(enve_incvic)
summary(gsummary(enve_grpd))

plot(table(gapply(enve_grpd, "extortions", FUN=max, level=2)))


### BEST to stick to showing summary statistics for the grouped levels, and leave
### cross-level interactions to the actual model.

# make a dataset of the victims, only those in the actual incident-level data
# describe

# same for the state level data

### business-level-eda
business_level_eda <- (subset(enve_test, CVE_UNICA %in% enve_incvic$CVE_UNICA))

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

kable(ext_dist)

#Poissonnes tests
n <- length(business_level_eda$extortions)
mean_ext <- mean(business_level_eda$extortions)
var_ext <- var(business_level_eda$extortions)

mean_ext
var_ext

var_mean_ratio <- var_ext/mean_ext
var_mean_ratio

index_ext <- id.test(enve_test$extortions)
index_ext

vmr_df <- data.frame(Mean=mean_ext, Variance=var_ext, Ratio=var_mean_ratio,
                     Index=unname(index_ext[1]), Pvalue=unname(index_ext[2]),
                     DF=unname(index_ext[3]))

vmr_df

kable(vmr_df)

# plot of extortions
ggplot(business_level_eda, aes(extortions)) +
    geom_histogram(binwidth = 1) +
    coord_trans(y="sqrt") +
    theme_bw() +
    ylab("Frequency") +
    xlab("Extortions") +
    ggtitle("Extortion counts frequency")

ggplot(business_level_eda, aes(extortions)) +
  geom_density() +
  coord_trans(y="sqrt") +
  theme_bw() +
  ggtitle("Extortion counts density")

ggplot(business_level_eda, aes(extortions)) +
  geom_histogram(aes(y=..density..), binwidth = 1) + geom_density() +
  coord_trans(y="sqrt") +
  theme_bw() +
  ggtitle("Extortion counts density")


## plot the capped distrbution of extortion

capped_ext_count <- table(enve_incvic$CVE_UNICA)

capped_ext_count_df <- data.frame(capped_ext_count)

names(capped_ext_count_df) <- c("CVE_UNICA", "Extortions")

table(capped_ext_count)

ggplot(capped_ext_count_df, aes(Extortions)) +
  geom_histogram(binwidth = 1) +
  coord_trans(y="sqrt") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Capped extortion counts") +
  ggtitle("Capped extortion counts (> 0) frequency") +
  expand_limits(y=1)

ggplot(capped_ext_count_df, aes(Extortions)) +
  geom_histogram(binwidth = 1) +
  #coord_trans(y="sqrt") +
  theme_bw() +
  ylab("Frequency") +
  ggtitle("Capped extortion counts (> 0) frequency") +
  expand_limits(y=1)

ggplot(business_level_eda[business_level_eda$extortions>0,], aes(extortions)) +
  geom_histogram(binwidth = 1) +
  coord_trans(y="sqrt") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Extortions") +
  ggtitle("Uncapped extortion counts (> 0) frequency") +
  expand_limits(y=1)

ggplot(business_level_eda[business_level_eda$extortions>0,], aes(extortions)) +
  geom_histogram(binwidth = 1) +
  #coord_trans(y="sqrt") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Extortions") +
  ggtitle("Uncapped extortion counts (> 0) frequency") +
  expand_limits(y=1)


# other extortion variables
table(business_level_eda$extortion_victim)

table(business_level_eda$rep_extortion_victim)

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

kable(summ_table, row.names = F)

### just quickly to assess some potential model performance issues
### check the distribution of the DV with the categorical business vars

# size

t100 <- table(enve_incvic$size, enve_incvic$complied_bin)
t100
kable(t100)

t100/as.integer(margin.table(t100, margin=1))*100
kable(round(t100/as.integer(margin.table(t100, margin=1))*100, 2))

chisq.test(t100)
chisq.test(t100)$expected

chisq.test(t100, simulate.p.value=TRUE, B=9999)

print("Cramer's V'")
cv.test(t100)

# USE NA

t101 <- table(enve_incvic$size, enve_incvic$complied_bin, useNA = "ifany")
t101
kable(t101)

t101/as.integer(margin.table(t101, margin=1))*100
kable(round(t101/as.integer(margin.table(t101, margin=1))*100, 2))

chisq.test(t101)
chisq.test(t101)$expected

chisq.test(t101, simulate.p.value=TRUE, B=9999)

print("Cramer's V'")
cv.test(t101)

# subsector

t200 <- table(enve_incvic$subsector, enve_incvic$complied_bin)
t200
kable(t200)

t200/as.integer(margin.table(t200, margin=1))*100
kable(round(t200/as.integer(margin.table(t200, margin=1))*100, 2))

chisq.test(t200)
chisq.test(t200)$expected

chisq.test(t200, simulate.p.value=TRUE, B=9999)

print("Cramer's V'")
cv.test(t200)

# USE NA

t201 <- table(enve_incvic$subsector, enve_incvic$complied_bin, useNA = "ifany")
t201
kable(t201)

t201/as.integer(margin.table(t201, margin=1))*100
kable(round(t201/as.integer(margin.table(t201, margin=1))*100, 2))

chisq.test(t201)
chisq.test(t201)$expected

chisq.test(t201, simulate.p.value=TRUE, B=9999)

print("Cramer's V'")
cv.test(t201)

## hotrestbar

t300 <- table(enve_incvic$hotrestbar, enve_incvic$complied_bin)
t300
kable(t300)

t300/as.integer(margin.table(t300, margin=1))*100
kable(round(t300/as.integer(margin.table(t300, margin=1))*100, 2))

chisq.test(t300)
chisq.test(t300)$expected

chisq.test(t300, simulate.p.value=TRUE, B=9999)

print("Cramer's V'")
cv.test(t300)

# USE NA

t301 <- table(enve_incvic$hotrestbar, enve_incvic$complied_bin, useNA = "ifany")
t301
kable(t301)

t301/as.integer(margin.table(t301, margin=1))*100
kable(round(t301/as.integer(margin.table(t301, margin=1))*100, 2))

chisq.test(t301)
chisq.test(t301)$expected

chisq.test(t301, simulate.p.value=TRUE, B=9999)

print("Cramer's V'")
cv.test(t301)

## years quant

t400 <- table(enve_incvic$yearsquant, enve_incvic$complied_bin)
t400
kable(t400)

t400/as.integer(margin.table(t400, margin=1))*100
kable(round(t400/as.integer(margin.table(t400, margin=1))*100, 2))

chisq.test(t400)
chisq.test(t400)$expected

chisq.test(t400, simulate.p.value=TRUE, B=9999)

print("Cramer's V'")
cv.test(t400)

# USE NA

t401 <- table(enve_incvic$yearsquant, enve_incvic$complied_bin, useNA = "ifany")
t401
kable(t401)

t401/as.integer(margin.table(t401, margin=1))*100
kable(round(t401/as.integer(margin.table(t401, margin=1))*100, 2))

chisq.test(t401)
chisq.test(t401)$expected

chisq.test(t401, simulate.p.value=TRUE, B=9999)

print("Cramer's V'")
cv.test(t401)

###### Some simple models to test (start with full
###### models and see if can drop levels or use anova)


# not excluding weapons, violence or offender variables.

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

summary(m1)
pR2(m1)

print(xtable(m1), type="html")
htmlreg(m1)
screenreg(m1)

nobs(m1)
confint(m1)

# compare with null
lrtest(m1)
kable(lrtest(m1))

waldtest(m1)
kable(waldtest(m1))

waldtest(m1, test="Chisq")
kable(waldtest(m1, test="Chisq"))

# compare sequentially
anova(m1, test="Rao")
kable(anova(m1, test="Rao"))

anova(m1, test="LRT")
kable(anova(m1, test="LRT"))

# Test for multicollinearity
vif(m1)

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

vif(lm1)

## RMSE for the fitted values

rmses <- data.frame(model=NA)

m1_residuals <- residuals(m1, type="response")

rmses$model[1] <- "m1"
rmses$RMSE <- sqrt(mean(m1_residuals^2))
rmses$NRMSE <- sqrt(mean(m1_residuals^2))/sd(m1_residuals)
rmses$CVRMSE <- sqrt(mean(m1_residuals^2))/(max(m1_residuals)-
                                              min(m1_residuals))

rmses
kable(rmses)

# Plot observed vs fitted
m1_ob_pred <- data.frame(Observed=m1df$complied_bin,
                         Predicted=fitted(m1, type=response))

ggplot(m1_ob_pred, aes(Observed, Predicted)) +
  geom_boxplot() +
  theme_bw() +
  ggtitle("Compliance with extortion demands:\nObserved vs. predicted")

## Stepwise selection of variables

sm1 <- step(m1)

summary(sm1)
print(xtable(sm1), type="html")
htmlreg(sm1)
screenreg(sm1)

htmlreg(list(m1, sm1))
screenreg(list(m1, sm1))

sm1$anova
kable(sm1$anova)

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

summary(m1_nostate)

lrtest(m1_nostate, m1)
kable(lrtest(m1_nostate, m1))

waldtest(m1_nostate, m1)
kable(waldtest(m1_nostate, m1))

waldtest(m1_nostate, m1, test="Chisq")
kable(waldtest(m1_nostate, m1, test="Chisq"))

m1_nostate_nobus <- glm(complied_bin ~
                    extortion_type + request +
                    month + time +
                    n_offenders + rel_offenders +
                    had_weapon +
                    with_violence +
                    reported,
                  data=m1df,
                  family = "binomial")

summary(m1_nostate_nobus)

lrtest(m1_nostate_nobus, m1_nostate)
kable(lrtest(m1_nostate_nobus, m1_nostate))

waldtest(m1_nostate_nobus, m1_nostate)
kable(waldtest(m1_nostate_nobus, m1_nostate))

waldtest(m1_nostate_nobus, m1_nostate, test="Chisq")
kable(waldtest(m1_nostate_nobus, m1_nostate, test="Chisq"))

lrtest(m1_nostate_nobus, m1_nostate, m1)
kable(lrtest(m1_nostate_nobus, m1_nostate, m1))

waldtest(m1_nostate_nobus, m1_nostate, m1)
kable(waldtest(m1_nostate_nobus, m1_nostate, m1))

waldtest(m1_nostate_nobus, m1_nostate, m1, test="Chisq")
kable(waldtest(m1_nostate_nobus, m1_nostate, m1, test="Chisq"))

## without the vars with a lot of missing

m1_no_viol_off_wea <- glm(complied_bin ~
                          extortion_type + request +
                          month + time +
                          reported,
                        data=m1df,
                        family = "binomial")

summary(m1_no_viol_off_wea)

lrtest(m1_no_viol_off_wea, m1_nostate_nobus)
kable(lrtest(m1_no_viol_off_wea, m1_nostate_nobus))

waldtest(m1_no_viol_off_wea, m1_nostate_nobus)
kable(waldtest(m1_no_viol_off_wea, m1_nostate_nobus))

waldtest(m1_no_viol_off_wea, m1_nostate_nobus, test="Chisq")
kable(waldtest(m1_no_viol_off_wea, m1_nostate_nobus, test="Chisq"))

lrtest(m1_no_viol_off_wea, m1_nostate_nobus, m1_nostate, m1)
kable(lrtest(m1_no_viol_off_wea, m1_nostate_nobus, m1_nostate, m1))

waldtest(m1_no_viol_off_wea, m1_nostate_nobus, m1_nostate, m1)
kable(waldtest(m1_no_viol_off_wea, m1_nostate_nobus, m1_nostate, m1))

waldtest(m1_no_viol_off_wea, m1_nostate_nobus, m1_nostate, m1, test="Chisq")
kable(waldtest(m1_no_viol_off_wea, m1_nostate_nobus, m1_nostate, m1, test="Chisq"))

htmlreg(list(m1,sm1, m1_nostate, m1_nostate_nobus,m1_no_viol_off_wea))
screenreg(list(m1,sm1, m1_nostate, m1_nostate_nobus,m1_no_viol_off_wea))

## Using drop1

drop1(m1, test="LRT")
kable(drop1(m1, test="LRT"))


### With weapon type instead of had weapon

excnames <- quote(c(complied_bin,
                    extortion_type, request,
                    month, time,
                    n_offenders, rel_offenders,
                    weapon_type,
                    with_violence,
                    reported,
                    rep_extortion_victim,
                    bribe_victim,
                    size, hotrestbar, yearsquant,
                    loghoms, logpop))

m2df <- enve_incvic[which(complete.cases(subset(enve_incvic, 
                                                select=eval(excnames)))),]

m2 <- glm(complied_bin ~
            extortion_type + request +
            month + time +
            n_offenders + rel_offenders +
            weapon_type +
            with_violence +
            reported +
            rep_extortion_victim +
            bribe_victim +
            size + hotrestbar + yearsquant +
            loghoms + logpop,
          data=m2df,
          family = "binomial")

summary(m2)
pR2(m2)

print(xtable(m2), type="html")
htmlreg(m2)
screenreg(m2)

nobs(m2)
confint(m2)

# compare with null
lrtest(m2)
kable(lrtest(m2))

waldtest(m2)
kable(waldtest(m2))

waldtest(m2, test="Chisq")
kable(waldtest(m2, test="Chisq"))

# compare sequentially
anova(m2, test="Rao")
kable(anova(m2, test="Rao"))

anova(m2, test="LRT")
kable(anova(m2, test="LRT"))

# Test for multicollinearity
vif(m2)

# another test for multicollinearity, but using an lm instead
lm2 <- lm(as.integer(complied_bin) ~
            extortion_type + request +
            month + time +
            n_offenders + rel_offenders +
            weapon_type +
            with_violence +
            reported +
            rep_extortion_victim +
            bribe_victim +
            size + hotrestbar + yearsquant +
            loghoms + logpop,
          data=m2df)

summary(lm2)

vif(lm2)

## RMSE for the fitted values

m2_residuals <- residuals(m2, type="response")

m2_rmses <- c("m2",
              sqrt(mean(m2_residuals^2)),
              sqrt(mean(m2_residuals^2))/sd(m2_residuals),
              sqrt(mean(m2_residuals^2))/(max(m2_residuals)-
                                            min(m2_residuals)))

rmses <- rbind(rmses, m2_rmses)

rmses
kable(rmses)

# Plot observed vs fitted
m2_ob_pred <- data.frame(Observed=m2df$complied_bin,
                         Predicted=fitted(m2, type=response))

ggplot(m2_ob_pred, aes(Observed, Predicted)) +
  geom_boxplot() +
  theme_bw() +
  ggtitle("Compliance with extortion demands:\nObserved vs. predicted")

## Stepwise selection of variables

sm2 <- step(m2)

summary(sm2)
print(xtable(sm2), type="html")
htmlreg(sm2)
screenreg(sm2)

htmlreg(list(m2, sm2))
screenreg(list(m2, sm2))

sm2$anova
kable(sm2$anova)

## Exclude state then business level variables

m2_nostate <- glm(complied_bin ~
                    extortion_type + request +
                    month + time +
                    n_offenders + rel_offenders +
                    weapon_type +
                    with_violence +
                    reported +
                    rep_extortion_victim +
                    bribe_victim +
                    size + hotrestbar + yearsquant,
                  data=m2df,
                  family = "binomial")

summary(m2_nostate)

lrtest(m2_nostate, m2)
kable(lrtest(m2_nostate, m2))

waldtest(m2_nostate, m2)
kable(waldtest(m2_nostate, m2))

waldtest(m2_nostate, m2, test="Chisq")
kable(waldtest(m2_nostate, m2, test="Chisq"))

m2_nostate_nobus <- glm(complied_bin ~
                          extortion_type + request +
                          month + time +
                          n_offenders + rel_offenders +
                          weapon_type +
                          with_violence +
                          reported,
                        data=m2df,
                        family = "binomial")

summary(m2_nostate_nobus)

lrtest(m2_nostate_nobus, m2_nostate)
kable(lrtest(m2_nostate_nobus, m2_nostate))

waldtest(m2_nostate_nobus, m2_nostate)
kable(waldtest(m2_nostate_nobus, m2_nostate))

waldtest(m2_nostate_nobus, m2_nostate, test="Chisq")
kable(waldtest(m2_nostate_nobus, m2_nostate, test="Chisq"))

lrtest(m2_nostate_nobus, m2_nostate, m2)
kable(lrtest(m2_nostate_nobus, m2_nostate, m2))

waldtest(m2_nostate_nobus, m2_nostate, m2)
kable(waldtest(m2_nostate_nobus, m2_nostate, m2))

waldtest(m2_nostate_nobus, m2_nostate, m2, test="Chisq")
kable(waldtest(m2_nostate_nobus, m2_nostate, m2, test="Chisq"))

## without the vars with a lot of missing

m2_no_viol_off_wea <- glm(complied_bin ~
                            extortion_type + request +
                            month + time +
                            reported,
                          data=m2df,
                          family = "binomial")

summary(m2_no_viol_off_wea)

lrtest(m2_no_viol_off_wea, m2_nostate_nobus)
kable(lrtest(m2_no_viol_off_wea, m2_nostate_nobus))

waldtest(m2_no_viol_off_wea, m2_nostate_nobus)
kable(waldtest(m2_no_viol_off_wea, m2_nostate_nobus))

waldtest(m2_no_viol_off_wea, m2_nostate_nobus, test="Chisq")
kable(waldtest(m2_no_viol_off_wea, m2_nostate_nobus, test="Chisq"))

lrtest(m2_no_viol_off_wea, m2_nostate_nobus, m2_nostate, m2)
kable(lrtest(m2_no_viol_off_wea, m2_nostate_nobus, m2_nostate, m2))

waldtest(m2_no_viol_off_wea, m2_nostate_nobus, m2_nostate, m2)
kable(waldtest(m2_no_viol_off_wea, m2_nostate_nobus, m2_nostate, m2))

waldtest(m2_no_viol_off_wea, m2_nostate_nobus, m2_nostate, m2, test="Chisq")
kable(waldtest(m2_no_viol_off_wea, m2_nostate_nobus, m2_nostate, m2, 
               test="Chisq"))

htmlreg(list(m2,sm2, m2_nostate, m2_nostate_nobus,m2_no_viol_off_wea))
screenreg(list(m2,sm2, m2_nostate, m2_nostate_nobus,m2_no_viol_off_wea))

## Using drop

drop1(m2, test="LRT")
kable(drop1(m2, test="LRT"))

# compare m1 vs m2

screenreg(list(m1, m2))


### Try the multilevel spec of these two models, before trying the rest

# first for had weapon (m1) first the two level, then the three

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

summary(m1_l2)
#pR2(m1_l2) # doesn't work on glmer

### ICC

icc(m1_l2)
print(icc(m1_l2), comp="var")

## Describe data structure

## number of observations per individuals

length(unique(m1df$CVE_UNICA))
table(table(m1df$CVE_UNICA))
min(table(m1df$CVE_UNICA))
max(table(m1df$CVE_UNICA))
mean(table(m1df$CVE_UNICA))
sum(table(m1df$CVE_UNICA))

# number of observations per state
length(unique(m1df$CVE_ENT))
table(m1df$CVE_ENT)
sum(table(m1df$CVE_ENT))
min(table(m1df$CVE_ENT))
max(table(m1df$CVE_ENT))
mean(table(m1df$CVE_ENT))

# number of individuals per state

IperG <- with(m1df, tapply(CVE_UNICA, CVE_ENT, 
                           FUN = function(x) length(unique(x))))
sum(IperG)
min(IperG)
max(IperG)
mean(IperG)

## print m1_l2
htmlreg(m1_l2)
screenreg(m1_l2)

nobs(m1_l2)
confint(m1_l2)

## Need to create null models

## HERE need to run null ### Try on ucl machine.. taking too long

m1_l2_null <- glmer(complied_bin ~ 
                      (1|CVE_UNICA),
                    data=m1df,
                    family = "binomial")

summary(m1_l2_null)

m1_l1_null <- glm(complied_bin ~ 1,
                    data=m1df,
                    family = "binomial")

summary(m1_l1_null)

## need to run only area levels.. but only for three level model no?
## If we know its gonna be better dont waste time

# droptest
m1_l2_dropped <- drop1(m1_l2, test="Chisq")
kable(m1_l2_dropped)

# compare with null
lrtest(m1_l2_null, m1_l2)
kable(lrtest(m1_l2_null, m1_l2))

lrtest(m1_l1_null,m1_l2_null, m1_l2)
kable(lrtest(m1_l1_null,m1_l2_null, m1_l2))

# compare sequentially
anova(m1_l2, m1_l2_null, test="LRT")
kable(anova(m1_l2, m1_l2_null, test="LRT"))

anova(m1_l2, m1_l2_null, m1_l1_null, test="LRT")
kable(anova(m1_l2, m1_l2_null, m1_l1_null, test="LRT"))


## RMSE for the fitted values

rmses <- data.frame(model=NA)

m1_l2_residuals <- residuals(m1_l2, type="response")

m1_l2_rmses <- c("m1_l2",
              sqrt(mean(m1_l2_residuals^2)),
              sqrt(mean(m1_l2_residuals^2))/sd(m1_l2_residuals),
              sqrt(mean(m1_l2_residuals^2))/
                          (max(m1_l2_residuals)-min(m1_l2_residuals)))

rmses <- rbind(rmses, m1_l2_rmses)

rmses
kable(rmses)

# Plot observed vs fitted
m1_l2_ob_pred <- data.frame(Observed=m1df$complied_bin,
                         Predicted=fitted(m1_l2, type=response))

ggplot(m1_l2_ob_pred, aes(Observed, Predicted)) +
  geom_boxplot() +
  theme_bw() +
  ggtitle("Compliance with extortion demands:\nObserved vs. predicted")


# FOREST PLOTS?

# Random intercepts
sjp.glmer(m1_l2, show.values = FALSE, sort.est= TRUE)

# Forest plots
sjp.glmer(m1_l2, type="fe", show.values = FALSE)
sjp.glmer(m1_l2, type="fe", show.values = FALSE, sort.est = TRUE)


## Stepwise selection of variables

sm1_l2 <- step(m1_l2)

summary(sm1_l2)
print(xtable(sm1_l2), type="html")
htmlreg(sm1_l2)
screenreg(sm1_l2)

htmlreg(list(m1_l2, sm1_l2))
screenreg(list(m1_l2, sm1_l2))

sm1_l2$anova
kable(sm1_l2$anova)

## Exclude state then business level variables

m1_l2_nostate <- glmer(complied_bin ~
                    extortion_type + request +
                    month + time +
                    n_offenders + rel_offenders +
                    had_weapon +
                    with_violence +
                    reported +
                    rep_extortion_victim +
                    bribe_victim +
                    size + hotrestbar + yearsquant +
                    (1|CVE_UNICA),
                  data=m1df,
                  family = "binomial")

summary(m1_l2_nostate)

lrtest(m1_l2_nostate, m1_l2)
kable(lrtest(m1_l2_nostate, m1_l2))

waldtest(m1_l2_nostate, m1_l2)
kable(waldtest(m1_l2_nostate, m1_l2))

waldtest(m1_l2_nostate, m1_l2, test="Chisq")
kable(waldtest(m1_l2_nostate, m1_l2, test="Chisq"))

m1_l2_nostate_nobus <- glmer(complied_bin ~
                    extortion_type + request +
                    month + time +
                    n_offenders + rel_offenders +
                    had_weapon +
                    with_violence +
                    reported +
                    (1|CVE_UNICA),
                  data=m1df,
                  family = "binomial")

summary(m1_l2_nostate_nobus)

lrtest(m1_l2_nostate_nobus, m1_l2_nostate)
kable(lrtest(m1_l2_nostate_nobus, m1_l2_nostate))

waldtest(m1_l2_nostate_nobus, m1_l2_nostate)
kable(waldtest(m1_l2_nostate_nobus, m1_l2_nostate))

waldtest(m1_l2_nostate_nobus, m1_l2_nostate, test="Chisq")
kable(waldtest(m1_l2_nostate_nobus, m1_l2_nostate, test="Chisq"))

lrtest(m1_l2_nostate_nobus, m1_l2_nostate, m1_l2)
kable(lrtest(m1_l2_nostate_nobus, m1_l2_nostate, m1_l2))

waldtest(m1_l2_nostate_nobus, m1_l2_nostate, m1_l2)
kable(waldtest(m1_l2_nostate_nobus, m1_l2_nostate, m1_l2))

waldtest(m1_l2_nostate_nobus, m1_l2_nostate, m1_l2, test="Chisq")
kable(waldtest(m1_l2_nostate_nobus, m1_l2_nostate, m1_l2, test="Chisq"))

## without the vars with a lot of missing

m1_l2_no_viol_off_wea <- glmer(complied_bin ~
                          extortion_type + request +
                          month + time +
                          reported +
                          (1|CVE_UNICA),
                        data=m1df,
                        family = "binomial")

summary(m1_l2_no_viol_off_wea)

lrtest(m1_l2_no_viol_off_wea, m1_l2_nostate_nobus)
kable(lrtest(m1_l2_no_viol_off_wea, m1_l2_nostate_nobus))

waldtest(m1_l2_no_viol_off_wea, m1_l2_nostate_nobus)
kable(waldtest(m1_l2_no_viol_off_wea, m1_l2_nostate_nobus))

waldtest(m1_l2_no_viol_off_wea, m1_l2_nostate_nobus, test="Chisq")
kable(waldtest(m1_l2_no_viol_off_wea, m1_l2_nostate_nobus, test="Chisq"))

lrtest(m1_l2_no_viol_off_wea, m1_l2_nostate_nobus, m1_l2_nostate, m1_l2)
kable(lrtest(m1_l2_no_viol_off_wea, m1_l2_nostate_nobus, 
             m1_l2_nostate, m1_l2))

waldtest(m1_l2_no_viol_off_wea, m1_l2_nostate_nobus, m1_l2_nostate, m1_l2)
kable(waldtest(m1_l2_no_viol_off_wea, m1_l2_nostate_nobus, 
               m1_l2_nostate, m1_l2))

waldtest(m1_l2_no_viol_off_wea, m1_l2_nostate_nobus, 
         m1_l2_nostate, m1_l2, test="Chisq")
kable(waldtest(m1_l2_no_viol_off_wea, m1_l2_nostate_nobus, 
               m1_l2_nostate, m1_l2, test="Chisq"))

htmlreg(list(m1_l2,sm1_l2, m1_l2_nostate, m1_l2_nostate_nobus,
             m1_l2_no_viol_off_wea))

screenreg(list(m1_l2,sm1_l2, m1_l2_nostate, m1_l2_nostate_nobus,
               m1_l2_no_viol_off_wea))

# Compare with one-level m1

htmlreg(list(m1, m1_l2))

screenreg(list(m1, m1_l2))

lrtest(m1_l2, m1)

### ICC for glmer

library(sjstats)
icc(m1_l2)

print(icc(m1_l2), comp="var")

get_re_var(m1_l2, comp = "sigma_2")

qqmath(ranef(m1_l2))

write.dta(enve_incvic, "enve_incvic.dta")

summary(m1_l2)

## number of observations per individuals

length(unique(m1df$CVE_UNICA))
table(table(m1df$CVE_UNICA))
min(table(m1df$CVE_UNICA))
max(table(m1df$CVE_UNICA))
mean(table(m1df$CVE_UNICA))
sum(table(m1df$CVE_UNICA))

# number of observations per state
length(unique(m1df$CVE_ENT))
table(m1df$CVE_ENT)
sum(table(m1df$CVE_ENT))
min(table(m1df$CVE_ENT))
max(table(m1df$CVE_ENT))
mean(table(m1df$CVE_ENT))

# number of individuals per state

IperG <- with(m1df, tapply(CVE_UNICA, CVE_ENT, 
                  FUN = function(x) length(unique(x))))
sum(IperG)
min(IperG)
max(IperG)
mean(IperG)

summary(m1_l2)

install.packages("sjPlot")
library(sjPlot)
install.packages("arm")

fit <- m1_l2

set_theme(theme = "forest", 
          geom.label.size = 3, 
          axis.textsize = .9, 
          axis.title.size = .9)

sjp.glmer(fit, y.offset = .4, sort.est = "sort.all", facet.grid=FALSE)

sjp.glmer(fit, type = "fe", sort.est = "sort.all", facet.grid=FALSE)

sjp.glmer(fit, type = "eff", show.ci = TRUE)

sjp.glmer(fit, type = "pred", vars=c("extortion_type","request"), 
          show.ci = TRUE)

sjp.glmer(fit, type = "pred", vars=c("extortion_type","hotrestbar"), 
          show.ci = TRUE)

sjp.glmer(fit, type = "pred", vars=c("hotrestbar","extortion_type"), 
          show.ci = TRUE)

sjp.glmer(fit, type = "pred", vars=c("hotrestbar","request"), 
          show.ci = TRUE)

sjp.glmer(fit, type = "pred", vars=c("logpop"), show.ci = TRUE)


sjp.glmer(fit, type = "pred", 
          vars=c("request","extortion_type"), 
          show.ci = TRUE,
          facet.grid = FALSE)

