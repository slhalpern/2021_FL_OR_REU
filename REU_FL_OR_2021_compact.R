# Copied from Mason's HTML code. Removed tons of extra lines
# Attempting to create a more compact script to work with
# Plan to move code chunks to RMarkdown file to allow interpretation
# Need to connect to data files still

#----ALWAYS COMPLETE BEFORE CONTINUING----
#Packages to install prior to running analysis

#1 The "Car" package: Companion to Applied Regression
library(car)

#2 The "RColorBrewer" package: ColorBrewer palettes
library(RColorBrewer)

#3 The "lme4" package: Linear Mixed-Effects Models using 'Eigen' and S4
library(lme4)


#Set Working Directory to allow for better importation of RData file.
#No longer necessary b/c data is in project
#setwd("~/Documents/School/Herbivory Research/R Analysis")

#Import Data file "Herbivory_data.RData"
load("Herbivory_Data.RData")

#1 -----Begin with the Host Variation Experiment analysis-----
#A Plot interacting effects of species variation and movement on the relative growth rate of the Spodoptera
#a Exclude round 1 from the experimental data as well as exclude any NA's from the RGR Data
Excl_Host_RGR_Cond <- subset(Host_RGR_Cond, Host_RGR_Cond$Temporal_Rep != 1 & Host_RGR_Cond$RGR != "NA")

#b Add a column stating the treatment combination.
# M = Mustard in a no movement treatment
# K = Kale in a no movement treatment
# KM = Kale movement treatment with mustard
# KK = Movement treatment with just kale
Excl_Host_RGR_Cond$Treatment_comb <-
  ifelse(Excl_Host_RGR_Cond$Host_Species == "M" & Excl_Host_RGR_Cond$Movement == "N", "M",
         ifelse(Excl_Host_RGR_Cond$Host_Species == "K" & Excl_Host_RGR_Cond$Movement == "N", "K",
                ifelse(Excl_Host_RGR_Cond$Host_Variation == "Y" & Excl_Host_RGR_Cond$Movement == "Y", "KM", "KK")))

#c Use new column "Treatment_comb"
boxplot(as.numeric(Excl_Host_RGR_Cond$RGR) ~
          as.factor(Excl_Host_RGR_Cond$Treatment_comb),
        xlab = "Treatment Combination",
        ylab = "Relative Growth Rate (grams per gram per day)",
        main ="Host Variation and Movement Interactive Effects on 
        Relative Growth Rate", col=brewer.pal(n = 5, name = "Reds"))
legend("bottomright", inset=.02, title="Treatment Combo",
       c("K = Kale w/out Movement", "KK = Kale w/ Movement","KM = Kale w/ Mustard and Movement","M = Mustard w/out Movement"), fill=topo.colors(5), horiz=FALSE, cex=0.75)

#d Plot w/out the title or legend
boxplot(as.numeric(Excl_Host_RGR_Cond$RGR) ~
          as.factor(Excl_Host_RGR_Cond$Treatment_comb), xlab = NULL, ylab = NULL, col=brewer.pal(n = 5, name = "Reds"))

#B Plot interacting effects of species variation and movement on the mean percent leaf damage
#a Rename the Temporal Rep Column
HostData$Temporal_Rep <- HostData$"4.0"

#b Exclude round 1 from the experimental data
HostExperimentExclude1 <- subset(HostData, HostData$Temporal_Rep != 1)

#b Add column stating the treatment combination.
# M = Mustard in a no movement treatment
# K = Kale in a no movement treatment
# KM = Kale Movement treatment with mustard
# MK = Mustard Movement treatment with kale
# KK = Movement treatment with just kale
HostExperimentExclude1$Treatment_comb <-
  ifelse(HostExperimentExclude1$Host_Species == "M" & HostExperimentExclude1$Movement == "N", "M",
         ifelse(HostExperimentExclude1$Host_Species == "K" & HostExperimentExclude1$Movement == "N", "K",
                ifelse(HostExperimentExclude1$Host_Variation == "Y" & HostExperimentExclude1$Host_Species == "K" & HostExperimentExclude1$Movement == "Y", "KM",
                       ifelse(HostExperimentExclude1$Host_Variation == "Y" & HostExperimentExclude1$Host_Species == "M" & HostExperimentExclude1$Movement == "Y", "MK", "KK"))))

#c Exclude treatment "MK" from Data set 
HostExperimentExclude <- subset(HostExperimentExclude1, HostExperimentExclude1$Treatment_comb != "MK")

#d Use new column "Treatment_comb"
boxplot(as.numeric(HostExperimentExclude$Mean_PLD) ~ as.factor(HostExperimentExclude$Treatment_comb),
        xlab = "Treatment Combination",
        ylab = "Mean Percent Leaf Damage (%)",
        main ="Host Variation and Movement Interactive Effects on
Mean Percent Leaf Damage", col=brewer.pal(n = 5, name = "Purples"))
legend("topright", inset=.02, title="Treatment Combo", c("K = Kale w/out Movement", "KK = Kale w/ Movement","KM = Kale w/ Mustard and Movement","M = Mustard w/out Movement"), fill=topo.colors(4), horiz=FALSE, cex=0.5)

#e Plot w/out title or legend
boxplot(as.numeric(HostExperimentExclude$Mean_PLD) ~ as.factor(HostExperimentExclude$Treatment_comb), xlab = NULL, ylab = NULL, col=brewer.pal(n = 4, name = "Purples"))

#C Complete Levene's test to determine variance of relative growth rate of Spodoptera in Host Experiment Including Mustard
#a Repeat steps a-b in section 1A if not done so already
#b Create an Anova to extract the Residuals
m1_RGR_Host <- lm(Excl_Host_RGR_Cond$RGR ~ as.factor(Excl_Host_RGR_Cond$Temporal_Rep)+              as.factor(Excl_Host_RGR_Cond$Host_Species)+as.factor(Excl_Host_RGR_Cond$Treatment_comb))

#c Add abs(m1$residuals) to data sheet as a new column
Excl_Host_RGR_Cond$abs.resid <- abs(m1_RGR_Host$residuals)

#d Run levenne's test (Forced me to use ":" instead of "+" between factors)
m2_RGR_Host <- leveneTest(Excl_Host_RGR_Cond$abs.resid ~ as.factor(Excl_Host_RGR_Cond$Temporal_Rep):as.factor(Excl_Host_RGR_Cond$Host_Species):as.factor(Excl_Host_RGR_Cond$Treatment_comb))

#e show model
m2_RGR_Host

#f Pairwise test Host Variation (K vs. KK)
#fa Subset the data
HN_RGR_Host <- subset(Excl_Host_RGR_Cond, Excl_Host_RGR_Cond$Treatment_comb != "M" & Excl_Host_RGR_Cond$Treatment_comb != "KM")

#fb Run test using new subset
m2aa_RGR_Host <- leveneTest(as.numeric(HN_RGR_Host$abs.resid) ~ as.factor(HN_RGR_Host$Temporal_Rep):as.factor(HN_RGR_Host$Host_Species):as.factor(HN_RGR_Host$Treatment_comb))
m2aa_RGR_Host

#g Pairwise test Host Variation (KK vs. KM)
#ga Subset the data
MN_RGR_Host <- subset(Excl_Host_RGR_Cond, Excl_Host_RGR_Cond$Treatment_comb != "K" & Excl_Host_RGR_Cond$Treatment_comb != "M")

#gb Run test using new subset
m2c_RGR_Host <- leveneTest(as.numeric(MN_RGR_Host$abs.resid) ~ as.factor(MN_RGR_Host$Temporal_Rep):as.factor(MN_RGR_Host$Host_Species):as.factor(MN_RGR_Host$Treatment_comb))
m2c_RGR_Host

#h Pairwise test Host Variation (K vs. M)
#ha Subset the data
MY_RGR_Host <- subset(Excl_Host_RGR_Cond, Excl_Host_RGR_Cond$Treatment_comb != "KK" & Excl_Host_RGR_Cond$Treatment_comb != "KM")

#hb Run test using new subset
m2d_RGR_Host <- leveneTest(as.numeric(MY_RGR_Host$abs.resid) ~ as.factor(MY_RGR_Host$Temporal_Rep):as.factor(MY_RGR_Host$Host_Species):as.factor(MY_RGR_Host$Treatment_comb))
m2d_RGR_Host

#D Complete levene's test to determine variance of mean percent leaf damage in Host Experiment including Mustard
#a Repeat steps a-b in section 1B if not done so already

#b Create an Anova to extract the Residuals
m1_MeanPLD_Host <- lm(HostExperimentExclude$Mean_PLD ~ as.factor(HostExperimentExclude$Temporal_Rep)+as.factor(HostExperimentExclude$Host_Species)+as.factor(HostExperimentExclude$Treatment_comb))

#c Add abs(m1_MeanPLD_Host$residuals) to data sheet as a new column
HostExperimentExclude$abs.resid <- abs(m1_MeanPLD_Host$residuals)

#d Run levenne's test (Forced me to use ":" instead of "+" between factors)
m2_MeanPLD_Host <- leveneTest(HostExperimentExclude$abs.resid ~ as.factor(HostExperimentExclude$Temporal_Rep):as.factor(HostExperimentExclude$Host_Species):as.factor(HostExperimentExclude$Treatment_comb))

#e show model
m2_MeanPLD_Host

#f Pairwise test movement (K vs KK)
#fa Subset the data
HN_MPLD_Host <- subset(HostExperimentExclude, HostExperimentExclude$Treatment_comb != "KM" & HostExperimentExclude$Treatment_comb != "M")

#fb Run test using new subset
m2a_MeanPLD_Host <- leveneTest(HN_MPLD_Host$Mean_PLD ~ as.factor(HN_MPLD_Host$Temporal_Rep):as.factor(HN_MPLD_Host$Host_Species):as.factor(HN_MPLD_Host$Treatment_comb))
m2a_MeanPLD_Host

#h Pairwise test Host Variation (KK vs KM)
#ha Subset the data
MN_MeanPLD_Host <- subset(HostExperimentExclude, HostExperimentExclude$Treatment_comb != "K" & HostExperimentExclude$Treatment_comb != "M" & HostExperimentExclude$Host_Species != "M")

#hb Run test using new subset
m2c_MeanPLD_Host <- leveneTest(MN_MeanPLD_Host$Mean_PLD ~ as.factor(MN_MeanPLD_Host$Temporal_Rep):as.factor(MN_MeanPLD_Host$Host_Species):as.factor(MN_MeanPLD_Host$Treatment_comb))
m2c_MeanPLD_Host

#i Pairwise test (K vs. M)
#ia Subset the data to exclude Non-Movement plants
MY_MeanPLD_Host <- subset(HostExperimentExclude, HostExperimentExclude$Treatment_comb != "KK" & HostExperimentExclude$Treatment_comb != "KM")

#ic Run test using new subset
m2d_MeanPLD_Host <- leveneTest(MY_MeanPLD_Host$Mean_PLD ~ as.factor(MY_MeanPLD_Host$Temporal_Rep):as.factor(MY_MeanPLD_Host$Host_Species):as.factor(MY_MeanPLD_Host$Treatment_comb))
m2d_MeanPLD_Host

#E Complete mean analysis for the relative growth rate of the Spodoptera for the host experiment including mustard
#a Repeat steps a-b in section 1A if not done so already
#b Create boxplot of data
boxplot(as.numeric(Excl_Host_RGR_Cond$RGR) ~ Excl_Host_RGR_Cond$Host_Variation:Excl_Host_RGR_Cond$Movement)

#c Levene test to see if variances are homogeneous among 4 treatment combinations
m5_RGR_Host <- leveneTest(as.numeric(Excl_Host_RGR_Cond$RGR) ~ Excl_Host_RGR_Cond$Treatment_comb)
m5_RGR_Host

#d Create a qqplot to test homogeneity
qqPlot(as.numeric(Excl_Host_RGR_Cond$RGR))

#e Run the linear model to test the significance of the predictors and the predictor interactions
m6_RGR_Host <- lm(as.numeric(Excl_Host_RGR_Cond$RGR) ~ Excl_Host_RGR_Cond$Temporal_Rep + Excl_Host_RGR_Cond$Host_Variation + Excl_Host_RGR_Cond$Movement +
                    Excl_Host_RGR_Cond$Temporal_Rep*Excl_Host_RGR_Cond$Host_Variation +
                    Excl_Host_RGR_Cond$Temporal_Rep*Excl_Host_RGR_Cond$Movement +
                    Excl_Host_RGR_Cond$Host_Variation*Excl_Host_RGR_Cond$Movement + Excl_Host_RGR_Cond$Temporal_Rep*Excl_Host_RGR_Cond$Host_Variation*Excl_Host_RGR_Cond$Movement)
Anova(m6_RGR_Host)

#F Complete mean analysis for the relative growth rate of the Spodoptera for the host experiment excluding Mustard
#a Repeat steps a-b in section 1A if not done so already
#b Exclude mustard from the data set
Excl2_Host_RGR_Cond <- subset(Excl_Host_RGR_Cond, Excl_Host_RGR_Cond$Host_Species != "M")

#d Create boxplot of data
boxplot(as.numeric(Excl2_Host_RGR_Cond$RGR) ~ Excl2_Host_RGR_Cond$Host_Variation:Excl2_Host_RGR_Cond$Movement)

#e Levene test to see if variances are homogeneous among 4 treatment combinations
m5_RGR_Host <- leveneTest(as.numeric(Excl2_Host_RGR_Cond$RGR) ~ Excl2_Host_RGR_Cond$Treatment_comb)
m5_RGR_Host

#f Create a qqplot test homogeneity
qqPlot(as.numeric(Excl2_Host_RGR_Cond$RGR))

#g Run the linear model to test the significance of the predictors and the predictor interactions
m7_RGR_Host <- lm(as.numeric(Excl2_Host_RGR_Cond$RGR) ~ Excl2_Host_RGR_Cond$Temporal_Rep + Excl2_Host_RGR_Cond$Host_Variation + Excl2_Host_RGR_Cond$Movement +
                    Excl2_Host_RGR_Cond$Temporal_Rep*Excl2_Host_RGR_Cond$Host_Variation +
                    Excl2_Host_RGR_Cond$Temporal_Rep*Excl2_Host_RGR_Cond$Movement +
                    Excl2_Host_RGR_Cond$Host_Variation*Excl2_Host_RGR_Cond$Movement +
                    Excl2_Host_RGR_Cond$Temporal_Rep*Excl2_Host_RGR_Cond$Host_Variation*Excl2_Host_RGR_Cond$Movement)
Anova(m7_RGR_Host)

#G Complete mean analysis for the mean percent leaf damage for the host experiment
#a Repeat steps a-b in section 1B if not done so already
#b Levene test to see if variances are homogeneous among 4 treatment combinations
m5_MeanPLD_Host <- leveneTest(HostExperimentExclude$Mean_PLD ~ HostExperimentExclude$Host_Species*HostExperimentExclude$Movement)
m5_MeanPLD_Host

#d Create a qqplot test homogeneity
qqPlot(HostExperimentExclude$Mean_PLD)

#c Run the mixed linear model to test the significance of the predictors and the predictor interactions
m6_MeanPLD_Host <- lmer(HostExperimentExclude$Mean_PLD ~ HostExperimentExclude$Temporal_Rep + HostExperimentExclude$Host_Species*HostExperimentExclude$Movement + (1 | HostExperimentExclude$Cluster_ID)) #Last part means nested within clustered.
Anova(m6_MeanPLD_Host)

#2 -----Begin the Fertilization Experiment analysis-----
#A Plot interacting effects of fertilization and movement on the relative growth rate of the Spodoptera
#a Exclude round 1 from the experimental data and any NA's from the RGR data set
Excl_Fert_RGR_Cond <- subset(Fert_RGR_Cond, Fert_RGR_Cond$Temporal_Rep != 1 & Fert_RGR_Cond$RGR != "NA")

#b Add column stating the treatment combination.
# FYMY = Fertilized with Movement
# FYMN = Fertilized with no movement
# FNMY = Not fertilized with movement
# FNMN = Not fertilized with no movement
Excl_Fert_RGR_Cond$Treatment_comb <-
  ifelse(Excl_Fert_RGR_Cond$Fertilization == "Y" & Excl_Fert_RGR_Cond$Movement == "Y", "FYMY",
         ifelse(Excl_Fert_RGR_Cond$Fertilization == "Y" & Excl_Fert_RGR_Cond$Movement == "N", "FYMN",
                ifelse(Excl_Fert_RGR_Cond$Fertilization == "N" & Excl_Fert_RGR_Cond$Movement == "Y", "FNMY", "FNMN")))

#c Use new column "Treatment_comb"
boxplot(as.numeric(Excl_Fert_RGR_Cond$RGR) ~ Excl_Fert_RGR_Cond$Treatment_comb,
        xlab = "Treatment Combination", ylab = "Relative Growth Rate (grams per gram per day)",
        main ="Fertilization and Movement Interactive Affects on
Relative Growth Rate", col=brewer.pal(n = 4, name = "Blues"))
legend("bottomleft", inset=.02, title="Treatment Combo", c("FNMN = Not fertilized with no movement", "FNMY = Not fertilized with movement","FYMN = Fertilized with no movement", "FYMY = Fertilized with Movement"), fill=topo.colors(4), horiz=FALSE, cex=0.6)

#d Plot w/out title or legend
boxplot(as.numeric(Excl_Fert_RGR_Cond$RGR) ~ Excl_Fert_RGR_Cond$Treatment_comb,
        xlab = NULL, ylab = NULL, col=brewer.pal(n = 4, name = "Blues"))

#B Plot interacting effects of fertilization and movement on the mean percent leaf damage
#a Exclude round 1 from the experimental data
Excl_Fert_Data <- subset(Fert_Data, Fert_Data$Temporal_Rep != 1)

#b Add column stating the treatment combination.
# FYMY = Fertilized with Movement
# FYMN = Fertilized with no movement
# FNMY = Not fertilized with movement
# FNMN = Not fertilized with no movement
Excl_Fert_Data$Treatment_comb <-
  ifelse(Excl_Fert_Data$Fertilization == "Y" & Excl_Fert_Data$Movement == "Y", "FYMY",
         ifelse(Excl_Fert_Data$Fertilization == "Y" & Excl_Fert_Data$Movement == "N", "FYMN",
                ifelse(Excl_Fert_Data$Fertilization == "N" & Excl_Fert_Data$Movement == "Y", "FNMY", "FNMN")))

#c Use new column "Treatment_comb"
boxplot(as.numeric(Excl_Fert_Data$Mean_PLD) ~ Excl_Fert_Data$Treatment_comb,
        xlab = "Treatment Combination",  ylab = "Mean Percent Leaf Damage (%)", main ="Fertilizer and Movement Interactive Affects on
Mean Percent Leaf Damage", col=brewer.pal(n = 4, name = "Greens"))
legend("topright", inset=.02, title="Treatment Combo",
       c("FNMN = Not fertilized with no movement", "FNMY = Not fertilized with movement","FYMN = Fertilized with no movement", "FYMY = Fertilized with Movement"), fill=topo.colors(4), horiz=FALSE, cex=0.7)

#d Plot w/out title or legend
boxplot(as.numeric(Excl_Fert_Data$Mean_PLD) ~ Excl_Fert_Data$Treatment_comb,
        xlab = NULL,  ylab = NULL, col=brewer.pal(n = 4, name = "Greens"))

#C Complete Levene's test to determine variance of relative growth rate of Spodoptera in fertilizer experiment
#a Create an Anova to extract the Residuals
m1_Fert_RGR <- lm(as.numeric(Excl_Fert_RGR_Cond$RGR) ~ Excl_Fert_RGR_Cond$Temporal_Rep + Excl_Fert_RGR_Cond$State +Excl_Fert_RGR_Cond$Treatment_comb)

#b Add abs(m1$residuals) to data sheet as a new column
Excl_Fert_RGR_Cond$abs.resid <- abs(m1_Fert_RGR$residuals)

#c Run levenne's test (Forced me to use ":" instead of "+" between factors)
m2_Fert_RGR <- leveneTest(Excl_Fert_RGR_Cond$abs.resid ~ as.factor(Excl_Fert_RGR_Cond$Temporal_Rep):Excl_Fert_RGR_Cond$State:as.factor(Excl_Fert_RGR_Cond$Treatment_comb))

#d Show Model
m2_Fert_RGR

#e Pairwise test fert (FNMN vs. FYMN)
#ea Subset the data
FN_Fert_RGR <- subset(Excl_Fert_RGR_Cond, Excl_Fert_RGR_Cond$Treatment_comb != "FYMY" & Excl_Fert_RGR_Cond$Treatment_comb != "FNMY")

#eb Run test using new subset
m2a_Fert_RGR <- leveneTest(as.numeric(FN_Fert_RGR$abs.resid) ~ as.factor(FN_Fert_RGR$Temporal_Rep):FN_Fert_RGR$State:as.factor(FN_Fert_RGR$Treatment_comb))
m2a_Fert_RGR

#f Pairwise test fert (FNMY vs FYMY)
#fa Subset the data
FY_Fert_RGR <- subset(Excl_Fert_RGR_Cond, Excl_Fert_RGR_Cond$Treatment_comb != "FYMN" & Excl_Fert_RGR_Cond$Treatment_comb != "FNMN")

#fb Run test using new subset
m2b_Fert_RGR <- leveneTest(as.numeric(FY_Fert_RGR$abs.resid) ~ as.factor(FY_Fert_RGR$Temporal_Rep):FY_Fert_RGR$State:as.factor(FY_Fert_RGR$Treatment_comb))
m2b_Fert_RGR

#g Pairwise test movement (FNMN vs. FNMY)
#ga Subset the data
MN_Fert_RGR <- subset(Excl_Fert_RGR_Cond, Excl_Fert_RGR_Cond$Treatment_comb != "FYMN" & Excl_Fert_RGR_Cond$Treatment_comb != "FYMY")

#gb Run test using new subset
m2c_Fert_RGR <- leveneTest(as.numeric(MN_Fert_RGR$abs.resid) ~ as.factor(MN_Fert_RGR$Temporal_Rep):MN_Fert_RGR$State:as.factor(MN_Fert_RGR$Treatment_comb))
m2c_Fert_RGR

#h Pairwise test movement (FYMN vs. FYMY)
#ha Subset the data
MY_Fert_RGR <- subset(Excl_Fert_RGR_Cond, Excl_Fert_RGR_Cond$Treatment_comb != "FNMN" & Excl_Fert_RGR_Cond$Treatment_comb != "FNMY")

#hb Run test using new subset
m2d_Fert_RGR <- leveneTest(as.numeric(MY_Fert_RGR$abs.resid) ~ as.factor(MY_Fert_RGR$Temporal_Rep):MY_Fert_RGR$State:as.factor(MY_Fert_RGR$Treatment_comb))
m2d_Fert_RGR

#D Complete Levene's test to determine variance of mean percent leaf damage in fertilizer experiment
#a Repeat steps a-c in section 1B if not done so already
#b Create an Anova to extract the Residuals
m1_Fert_MPLD <- lm(Excl_Fert_Data$Mean_PLD ~ Excl_Fert_Data$Temporal_Rep + Excl_Fert_Data$State +Excl_Fert_Data$Treatment_comb)

#c Add abs(m1$residuals) to data sheet as a new column
Excl_Fert_Data$abs.resid <- abs(m1_Fert_MPLD$residuals)

#d Run levenne's test (Forced me to use ":" instead of "+" between factors)
m2_Fert_MPLD <- leveneTest(Excl_Fert_Data$abs.resid ~  as.factor(Excl_Fert_Data$Temporal_Rep):Excl_Fert_Data$State:as.factor(Excl_Fert_Data$Treatment_comb))

#e show model
m2_Fert_MPLD

#f Pairwise test movement (FNMN vs FNMY)
#fa Subset the data to exclude
FN_Fert_MPLD <- subset(Excl_Fert_Data, Excl_Fert_Data$Treatment_comb != "FYMY" & Excl_Fert_Data$Treatment_comb != "FYMN")

#fb Run test using new subset
m2a_Fert_MPLD <- leveneTest(FN_Fert_MPLD$abs.resid ~ as.factor(FN_Fert_MPLD$Temporal_Rep):FN_Fert_MPLD$State:as.factor(FN_Fert_MPLD$Treatment_comb))
m2a_Fert_MPLD

#g Pairwise test movement (FYMN vs. FYMY)
#ga Subset the data
FY_Fert_MPLD <- subset(Excl_Fert_Data, Excl_Fert_Data$Treatment_comb != "FNMY" & Excl_Fert_Data$Treatment_comb != "FNMN")

#gb Run test using new subset
m2b_Fert_MPLD <- leveneTest(FY_Fert_MPLD$abs.resid ~ as.factor(FY_Fert_MPLD$Temporal_Rep):FY_Fert_MPLD$State:as.factor(FY_Fert_MPLD$Treatment_comb))
m2b_Fert_MPLD

#h Pairwise test fert (FNMY vs FYMY)
#ha Subset the data
MN_Fert_MPLD <- subset(Excl_Fert_Data, Excl_Fert_Data$Treatment_comb != "FYMN" & Excl_Fert_Data$Treatment_comb != "FNMN")

#hb Run test using new subset
m2c_Fert_MPLD <- leveneTest(MN_Fert_MPLD$Mean_PLD ~ as.factor(MN_Fert_MPLD$Temporal_Rep):MN_Fert_MPLD$State:as.factor(MN_Fert_MPLD$Treatment_comb))
m2c_Fert_MPLD

#i Pairwise test fert (FNMN vs FYMN)
#ia Subset the data to exclude Non-Movement plants
MY_Fert_MPLD <- subset(Excl_Fert_Data, Excl_Fert_Data$Treatment_comb != "FNMY" & Excl_Fert_Data$Treatment_comb != "FYMY")

#ib Run test using new subset
m2d_Fert_MPLD <- leveneTest(MY_Fert_MPLD$Mean_PLD ~ as.factor(MY_Fert_MPLD$Temporal_Rep):MY_Fert_MPLD$State:as.factor(MY_Fert_MPLD$Treatment_comb))
m2d_Fert_MPLD

#E Complete mean analysis for the relative growth rate of the Spodoptera for the fertilizer experiment
#a Repeat steps a-c in section 2A if not done so already
#b Transform the data using log
Excl_Fert_RGR_Cond$LogRGR <- log(as.numeric(Excl_Fert_RGR_Cond$RGR))
Excl2_Fert_LOGRGR <- subset(Excl_Fert_RGR_Cond, Excl_Fert_RGR_Cond$LogRGR != "NA")
# error message: In log(as.numeric(Excl_Fert_RGR_Cond$RGR)) : NaNs produced
# can't do ca code below

#c Levene test to see if variances are homogeneous among 4 treatment combinations
m3_Fert_RGR <- leveneTest(as.numeric(Excl_Fert_RGR_Cond$RGR) ~ Excl_Fert_RGR_Cond$Fertilization:Excl_Fert_RGR_Cond$Movement)
m3_Fert_RGR

#ca Levene test to see if variances are homogeneous among 4 treatment combinations using transformed data
m3_Fert_RGR <- leveneTest(Excl_Fert_LOGRGR$LogRGR ~ Excl_Fert_LOGRGR$Fertilization:Excl_Fert_LOGRGR$Movement)
m3_Fert_RGR

#d Create a qqplot of transformed data to test homogeneity
qqPlot(as.numeric(Excl_Fert_RGR_Cond$RGR))

#g Run the linear model to test the significance of the predictors and the predictor interactions
m4_fert_RGR <- lm(as.numeric(Excl_Fert_RGR_Cond$RGR) ~ Excl_Fert_RGR_Cond$State + Excl_Fert_RGR_Cond$Temporal_Rep + Excl_Fert_RGR_Cond$Fertilization + Excl_Fert_RGR_Cond$Movement +Excl_Fert_RGR_Cond$State*Excl_Fert_RGR_Cond$Fertilization + Excl_Fert_RGR_Cond$State*Excl_Fert_RGR_Cond$Movement + Excl_Fert_RGR_Cond$Temporal_Rep*Excl_Fert_RGR_Cond$Fertilization + Excl_Fert_RGR_Cond$Temporal_Rep*Excl_Fert_RGR_Cond$Movement +
                    Excl_Fert_RGR_Cond$State*Excl_Fert_RGR_Cond$Fertilization*Excl_Fert_RGR_Cond$Movement +
                    Excl_Fert_RGR_Cond$Temporal_Rep*Excl_Fert_RGR_Cond$Fertilization*Excl_Fert_RGR_Cond$Movement + Excl_Fert_RGR_Cond$Fertilization*Excl_Fert_RGR_Cond$Movement)

m4_fert_RGR <- lm(as.numeric(Excl_Fert_RGR_Cond$RGR) ~ Excl_Fert_RGR_Cond$State + Excl_Fert_RGR_Cond$Temporal_Rep + Excl_Fert_RGR_Cond$Fertilization + Excl_Fert_RGR_Cond$Movement +Excl_Fert_RGR_Cond$Fertilization*Excl_Fert_RGR_Cond$Movement)
Anova(m4_fert_RGR)

#F Complete mean analysis for the mean percent leaf damage for the fertilizer experiment
#a Repeat steps a-c in section 2B if not done so already
#c Levene test to see if variances are homogeneous among 4 treatment combinations
m3_Fert_MeanPLD <- leveneTest(Excl_Fert_Data$Mean_PLD ~ Excl_Fert_Data$Fertilization*Excl_Fert_Data$Movement)
m3_Fert_MeanPLD

#d Create a qqplot test homogeneity
qqPlot(Excl_Fert_Data$Mean_PLD)

#c Run the mixed linear model to test the significance of the predictors and the predictor interactions
m4_Fert_MeanPLD <- lmer(Excl_Fert_Data$Mean_PLD ~ Excl_Fert_Data$Temporal_Rep + Excl_Fert_Data$State + Excl_Fert_Data$Fertilization*Excl_Fert_Data$Movement + (1 | Excl_Fert_Data$Cluster_ID))
Anova(m4_Fert_MeanPLD)

#3 ----Begin Coefficient of Variance Analysis----  (Complete using same analysis as means analysis)
#A Complete Coefficient of Variance Analysis for Host Experiment
#a Exclude round 1 from the experimental data and and #DIV/0! from the CV data set
Excl_HOST_CV <- subset(HOST_CV, HOST_CV$Temporal_Rep != 1 & HOST_CV$CV != "#DIV/0!")

#b Add column stating the treatment combination.
# M = Mustard in a no movement treatment
# K = Kale in a no movement treatment
# KM = Kale Movement treatment with mustard
# MK = Mustard Movement treatment with kale
# KK = Movement treatment with just kale
Excl_HOST_CV$Treatment_comb <- ifelse(Excl_HOST_CV$Host_Species == "M" & Excl_HOST_CV$Movement == "N", "M", ifelse(Excl_HOST_CV$Host_Species == "K" & Excl_HOST_CV$Movement == "N", "K", ifelse(Excl_HOST_CV$Host_Variation == "Y" & Excl_HOST_CV$Host_Species == "K" & Excl_HOST_CV$Movement == "Y", "KM", ifelse(Excl_HOST_CV$Host_Variation == "Y" & Excl_HOST_CV$Host_Species == "M" & Excl_HOST_CV$Movement == "Y", "MK", "KK"))))

#Exclude all mustard from the data sheet
Excl2_HOST_CV <- subset(Excl_HOST_CV, Excl_HOST_CV$Treatment_comb != "M" & Excl_HOST_CV$Treatment_comb != "MK")

#c Levene test to see if variances are homogeneous among 4 treatment combinations
m5_CV_Host <- leveneTest(Excl_HOST_CV$CV ~ Excl_HOST_CV$Host_Variation*Excl_HOST_CV$Movement)
m5_CV_Host

#d Create a qqplot test homogeneity
qqPlot(Excl_HOST_CV$CV)

#c Run the mixed linear model to test the significance of the predictors and the predictor interactions
m6_CV_Host <- lmer(Excl_HOST_CV$CV ~ Excl_HOST_CV$Temporal_Rep + Excl_HOST_CV$Host_Variation*Excl_HOST_CV$Movement + (1 | Excl_HOST_CV$Cluster_ID)) #Last part means nested within clustered.
Anova(m6_CV_Host)

#e Create Plot of Coefficient of Variance for Host experiment
boxplot(Excl2_HOST_CV$CV ~ Excl2_HOST_CV$Treatment_comb, xlab = NULL, ylab = NULL, col=brewer.pal(n = 3, name = "RdPu"))

#B Complete Coefficient of Variance Analysis for Fertilizer Experiment
#a Exclude round 1 from the experimental data and and #DIV/0! from the CV data set
Excl_Fert_CV <- subset(FERT_CV, FERT_CV$Temporal_Rep != 1 & FERT_CV$CV != "#DIV/0!")

#b Add column stating the treatment combination.
# FYMY = Fertilized with Movement
# FYMN = Fertilized with no movement
# FNMY = Not fertilized with movement
# FNMN = Not fertilized with no movement
Excl_Fert_CV$Treatment_comb <- ifelse(Excl_Fert_CV$Fertilization == "Y" & Excl_Fert_CV$Movement == "Y", "FYMY", ifelse(Excl_Fert_CV$Fertilization == "Y" & Excl_Fert_CV$Movement == "N", "FYMN", ifelse(Excl_Fert_CV$Fertilization == "N" & Excl_Fert_CV$Movement == "Y", "FNMY", "FNMN")))

#c Transform the data using log
Excl_Fert_CV$LogCV <- log(as.numeric(Excl_Fert_CV$CV))
Excl2_Fert_LOGCV <- subset(Excl_Fert_CV, Excl_Fert_CV$LogCV != "-Inf")

#c Levene test to see if variances are homogeneous among 4 treatment combinations using transformed data
m3b_Fert_LOGCV <- leveneTest(as.numeric(Excl2_Fert_LOGCV$CV) ~ Excl2_Fert_LOGCV$Fertilization:Excl2_Fert_LOGCV$Movement)
m3b_Fert_LOGCV

#d Create a qqplot test homogeneity
qqPlot(Excl2_Fert_LOGCV$LogCV)

#c Run the mixed linear model to test the significance of the predictors and the predictor interactions
m6_Fert_CV <- lmer(Excl2_Fert_LOGCV$LogCV ~ Excl2_Fert_LOGCV$Temporal_Rep +  Excl2_Fert_LOGCV$State + Excl2_Fert_LOGCV$Fertilization*Excl2_Fert_LOGCV$Movement + (1 | Excl2_Fert_LOGCV$Cluster_ID)) #Last part means nested within clustered.
Anova(m6_Fert_CV)

#e Create Plot of Coefficient of Variance for Host experiment
boxplot(Excl2_Fert_LOGCV$LogCV ~ Excl2_Fert_LOGCV$Treatment_comb, xlab = NULL, ylab = NULL, col=brewer.pal(n = 3, name = "Oranges"))

#4 ----Begin Regression Analysis----
#Remove mustard from data set and subset data to contain only kale
LB = subset(LB, LB$Host_Species != "M" & LB$LLL != "NA")

#Complete linear model
reg1 <- lm(Total_FLA ~ as.numeric(LLL) +as.numeric(ILN), data = LB)
summary(reg1)

#Plot Total Final Leaf Area with Longest Leaf Length
plot(LB$Total_FLA ~ LB$LLL)

#Plot Total Final Leaf Area with Initial Leaf Number
plot(LB$Total_FLA ~ LB$ILN)

#Plot  to show the accuracy of the regression
plot(reg1) #intercept is very negative, going to try combining Initial leaf number and Longest Leaf Length

#create new column that multiplies initial leaf number by longest leaf length
LB$Total_LL <- as.numeric(LB$LLL) * as.numeric(LB$ILN)

#plot total_FLA as Total_LL
plot(LB$Total_FLA ~ LB$Total_LL)

#plot total_FLA as Total_LL w/log transformation
plot(log(LB$Total_FLA) ~ log(LB$Total_LL))

#run a linear model of Total_FLA as Total_LL
reg2 <- lm((LB$Total_FLA) ~ (LB$Total_LL))

#ask for a plot
plot(reg2) #Intercept is still very negative, going to try a log transformation

#ask for summary
summary(reg2)

#transform linear model using log
model1 <- lm(log(LB$Total_FLA) ~ log(LB$Total_LL))

#ask for a plot
plot(model1)

#get summary
summary(model1)    

#use results from the log transformation to get equation
# ln(y)= -0.42007+1.38467ln(x) #can’t use, so we must take both side by e^x
#new equation is y = 0.657001 * x^(1.38467) or Abs_LA = 0.657001 * Total_LL^(1.38467)
#so our regression needs to be raised to that power

#A Plot interacting effects of species variation and movement on the Mean Absolute Damage
#have to add temporal_rep to data set
HostData$Temporal_Rep <- HostData$`4.0`

#a Exclude round 1 from the experimental data
HostExperimentExclude1 <- subset(HostData, HostData$Temporal_Rep != "1")

#add new column for Total Leaf Length
HostExperimentExclude1$Total_LL <- as.numeric(HostExperimentExclude1$LLL) * as.numeric(HostExperimentExclude1$ILN)

#add new column with equation
HostExperimentExclude1$Abs_LA <- (0.657001*HostExperimentExclude1$Total_LL^1.38467)

#add column that multiplies mean percent damage by absolute leaf area  
HostExperimentExclude1$actual_damage <- as.numeric(HostExperimentExclude1$Abs_LA) * (as.numeric(HostExperimentExclude1$Mean_PLD)/100)

#b Add column stating the treatment combination.
# M = Mustard in a no movement treatment
# K = Kale in a no movement treatment
# KM = Kale Movement treatment with mustard
# MK = Mustard Movement treatment with kale
# KK = Movement treatment with just kale
HostExperimentExclude1$Treatment_comb <- ifelse(HostExperimentExclude1$Host_Species == "M" & HostExperimentExclude1$Movement == "N", "M", ifelse(HostExperimentExclude1$Host_Species == "K" & HostExperimentExclude1$Movement == "N", "K", ifelse(HostExperimentExclude1$Host_Variation == "Y" & HostExperimentExclude1$Host_Species == "K" & HostExperimentExclude1$Movement == "Y", "KM", ifelse(HostExperimentExclude1$Host_Variation == "Y" & HostExperimentExclude1$Host_Species == "M" & HostExperimentExclude1$Movement == "Y", "MK", "KK"))))

#c Exclude treatment "MK" and "M" from Data set
HostExperimentExclude <- subset(HostExperimentExclude1, HostExperimentExclude1$Treatment_comb != "MK" & HostExperimentExclude1$Treatment_comb != "M")

#d Use new column "Treatment_comb"
boxplot(as.numeric(HostExperimentExclude$actual_damage) ~ as.factor(HostExperimentExclude$Treatment_comb), xlab = "Treatment Combination", ylab = "Absolute Leaf Damage", main ="Host Variation and Movement Interactive Effects on
Absolute Leaf Damage", col=brewer.pal(n = 5, name = "Purples"))
legend("topright", inset=.02, title="Treatment Combo", c("K = Kale w/out Movement", "KK = Kale w/ Movement","KM = Kale w/ Mustard and Movement","M = Mustard w/out Movement"), fill=topo.colors(4), horiz=FALSE, cex=0.5)

#e Plot w/out title or legend
boxplot(as.numeric(HostExperimentExclude$actual_damage) ~ as.factor(HostExperimentExclude$Treatment_comb), xlab = NULL, ylab = NULL, col=brewer.pal(n = 4, name = "Purples"))

#B Complete Levenes test to determine the variance of the absolute leaf damage in the Host Experiment
#b Create an Anova to extract the Residuals
m1_absolutedamage_Host <- lm(HostExperimentExclude$actual_damage ~ as.factor(HostExperimentExclude$Temporal_Rep)+as.factor(HostExperimentExclude$Host_Variation)+as.factor(HostExperimentExclude$Treatment_comb))

#c Add abs(m1_absolutedamage_Host$residuals) to data sheet as a new column
HostExperimentExclude$abs.resid2 <- abs(m1_absolutedamage_Host$residuals)

#d Run levenne's test (Forced me to use ":" instead of "+" between factors)
m2_absolutedamage_Host <- leveneTest(HostExperimentExclude$abs.resid2 ~ as.factor(HostExperimentExclude$Temporal_Rep):as.factor(HostExperimentExclude$Host_Species):as.factor(HostExperimentExclude$Treatment_comb))

#e show model
m2_absolutedamage_Host

#f Pairwise test movement (K vs KK)
#fa Subset the data
HN_absolutedamage_Host <- subset(HostExperimentExclude, HostExperimentExclude$Treatment_comb != "KM" &
                                   HostExperimentExclude$Treatment_comb != "M")

#fb Run test using new subset
m2a_absolutedamgae_Host <- leveneTest(HN_absolutedamage_Host$actual_damage ~
                                        as.factor(HN_absolutedamage_Host$Temporal_Rep):
                                        as.factor(HN_absolutedamage_Host$Host_Species):
                                        as.factor(HN_absolutedamage_Host$Treatment_comb))
m2a_absolutedamgae_Host

#h Pairwise test Host Variation (KK vs KM)
#ha Subset the data
MN_absolutedamage_Host <- subset(HostExperimentExclude, HostExperimentExclude$Treatment_comb != "K" &
                                   HostExperimentExclude$Treatment_comb != "M" & HostExperimentExclude$Host_Species != "M")

#hb Run test using new subset
m2c_absoultedamage_Host <- leveneTest(MN_absolutedamage_Host$actual_damage ~
                                        as.factor(MN_absolutedamage_Host$Temporal_Rep):
                                        as.factor(MN_absolutedamage_Host$Host_Species):as.factor(MN_absolutedamage_Host$Treatment_comb))
m2c_absoultedamage_Host

#C Mean’s Analysis for Mean Absolute Leaf Damage
#a Repeat steps a-b in section 1B if not done so already
#b Levene test to see if variances are homogeneous among 4 treatment combinations
m5_absolutedamage_Host <- leveneTest(HostExperimentExclude$actual_damage ~
                                       HostExperimentExclude$Host_Variation*HostExperimentExclude$Movement)
m5_absolutedamage_Host

#D Plot interacting effects of fertilizer and movement for absolute leaf damage
#Remove temporal rep 1
Excl_Fert_Data = subset(Fert_Data, Fert_Data$Temporal_Rep != "1")

#use results from the log transformation of Leaf Byte Data to get equation
#y=0.657001*x^1.38467
#so our regression needs to be raised to that power

#add new column for Total Leaf Length
Excl_Fert_Data$Total_LL <- as.numeric(Excl_Fert_Data$LLL) * as.numeric(Excl_Fert_Data$ILN)

#add new column with equation
Excl_Fert_Data$Abs_LA <- (0.657001*Excl_Fert_Data$Total_LL^1.38467)

#add column that multiplies mean percent damage by absolute leaf area  
Excl_Fert_Data$actual_damage <- as.numeric(Excl_Fert_Data$Abs_LA) * (as.numeric(Excl_Fert_Data$Mean_PLD)/100)

#b Add column stating the treatment combination.
# FYMY = Fertilized with Movement
# FYMN = Fertilized with no movement
# FNMY = Not fertilized with movement
# FNMN = Not fertilized with no movement
Excl_Fert_Data$Treatment_comb <- ifelse(Excl_Fert_Data$Fertilization == "Y" & Excl_Fert_Data$Movement == "Y", "FYMY", ifelse(Excl_Fert_Data$Fertilization == "Y" & Excl_Fert_Data$Movement == "N", "FYMN", ifelse(Excl_Fert_Data$Fertilization == "N" & Excl_Fert_Data$Movement == "Y", "FNMY", "FNMN")))

#c Use new column "Treatment_comb"
boxplot(as.numeric(Excl_Fert_Data$actual_damage) ~ Excl_Fert_Data$Treatment_comb,
        xlab = "Treatment Combination",  ylab = "Mean Absolute Damage", main ="Fertilizer and Movement Interactive Affects on
Mean Absolute Leaf Damage", col=brewer.pal(n = 4, name = "Greens"))
legend("topright", inset=.02, title="Treatment Combo",
       c("FNMN = Not fertilized with no movement", "FNMY = Not fertilized with movement","FYMN = Fertilized with no movement", "FYMY = Fertilized with Movement"), fill=topo.colors(4), horiz=FALSE, cex=0.7)

#d Plot w/out title or legend
boxplot(as.numeric(Excl_Fert_Data$actual_damage) ~ Excl_Fert_Data$Treatment_comb,
        xlab = NULL,  ylab = NULL, col=brewer.pal(n = 4, name = "Greens"))

#E Complete Levene's test to determine the variance of the absolute leaf damage in the Fertilizer Experiment
#a Repeat steps a-c in section 1B if not done so already
#b Create an Anova to extract the Residuals
m1_Fert_absolutedamage <- lm(Excl_Fert_Data$actual_damage ~ Excl_Fert_Data$Temporal_Rep + Excl_Fert_Data$State +Excl_Fert_Data$Treatment_comb)

#c Add abs(m1$residuals) to data sheet as a new column
Excl_Fert_Data$abs.resid <- abs(m1_Fert_absolutedamage$residuals)

#d Run levenne's test (Forced me to use ":" instead of "+" between factors)
m2_Fert_absolutedamage <- leveneTest(Excl_Fert_Data$abs.resid ~ as.factor(Excl_Fert_Data$Temporal_Rep):Excl_Fert_Data$State:as.factor(Excl_Fert_Data$Treatment_comb))

#e show model
m2_Fert_absolutedamage

#f Pairwise test movement (FNMN vs FNMY)
#fa Subset the data to exclude
FN_Fert_absolutedamage <- subset(Excl_Fert_Data, Excl_Fert_Data$Treatment_comb != "FYMY" & Excl_Fert_Data$Treatment_comb != "FYMN")

#fb Run test using new subset
m2a_Fert_absolutedamage <- leveneTest(FN_Fert_absolutedamage$abs.resid ~ as.factor(FN_Fert_absolutedamage$Temporal_Rep):
                                        FN_Fert_absolutedamage$State:as.factor(FN_Fert_absolutedamage$Treatment_comb))
m2a_Fert_absolutedamage

#g Pairwise test movement (FYMN vs. FYMY)
#ga Subset the data
FY_Fert_absolutedamage <- subset(Excl_Fert_Data, Excl_Fert_Data$Treatment_comb != "FNMY" & Excl_Fert_Data$Treatment_comb != "FNMN")

#gb Run test using new subset
m2b_Fert_absolutedamage <- leveneTest(FY_Fert_absolutedamage$abs.resid ~ as.factor(FY_Fert_absolutedamage$Temporal_Rep):FY_Fert_absolutedamage$State:as.factor(FY_Fert_absolutedamage$Treatment_comb))
m2b_Fert_absolutedamage

#h Pairwise test fert (FNMY vs FYMY)
#ha Subset the data
MN_Fert_absolutedamage <- subset(Excl_Fert_Data, Excl_Fert_Data$Treatment_comb != "FYMN" & Excl_Fert_Data$Treatment_comb != "FNMN")

#hb Run test using new subset
m2c_Fert_absolutedamage <- leveneTest(MN_Fert_absolutedamage$actual_damage ~ as.factor(MN_Fert_absolutedamage$Temporal_Rep):MN_Fert_absolutedamage$State:as.factor(MN_Fert_absolutedamage$Treatment_comb))
m2c_Fert_absolutedamage

#i Pairwise test fert (FNMN vs FYMN)
#ia Subset the data to exclude Non-Movement plants
MY_Fert_absolutedamage <- subset(Excl_Fert_Data, Excl_Fert_Data$Treatment_comb != "FNMY" & Excl_Fert_Data$Treatment_comb != "FYMY")

#ib Run test using new subset
m2d_Fert_absolutedamage <- leveneTest(MY_Fert_absolutedamage$actual_damage ~ as.factor(MY_Fert_absolutedamage$Temporal_Rep):MY_Fert_absolutedamage$State:as.factor(MY_Fert_absolutedamage$Treatment_comb))
m2d_Fert_absolutedamage