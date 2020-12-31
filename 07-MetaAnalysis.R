## Analysis Script - Meta-Analysis for Gender and Number attraction effects ----
## "Attraction Effects for Verbal Gender and Number Are Similar but Not        #
## Identical: Self-Paced Reading Evidence from Modern Standard Arabic"         #
## M. A. Tucker, A. Idrissi, and D. Almeida                                    #
## Script Author: Diogo Almeida <diogo@nyu.edu,                                #
##                Matt Tucker <matt.tucker@nyu.edu>                            #
## v. 1, current 29 January 2017                                               #
## v. 2, current 26 December 2020                                              #

## Loading Packages ------------------------------------------------------------
library(tidyverse)
library(viridis)
library(ragg)
library(pins)
library(fs)
library(here)
library(stargazer)
library(metafor)

## Loading data for meta analysis ----------------------------------------------
this_exp <- "meta"
figs.dir <- here(this_exp, "figures")
dir_create(figs.dir)

meta.analysis.rdata.files <- c("Experiment1-DataForMetaAnalysis.RData",
                               "Experiment2-DataForMetaAnalysis.RData",
                               "Experiment3-DataForMetaAnalysis.RData",
                               "Experiment4-DataForMetaAnalysis.RData",
                               "Experiment5-DataForMetaAnalysis.RData",
                               "Tucker2015-DataForMetaAnalysis.RData")

## Check if files exist locally, otherwise download them from figshare ---------
if (all(file_exists(here(this_exp, meta.analysis.rdata.files)))) {
  lapply(here(this_exp, meta.analysis.rdata.files), load, envir = .GlobalEnv)
} else {
  meta.datafile.figshare <- "https://ndownloader.figshare.com/files/25894410"
  pin(meta.datafile.figshare, name = "MetaAnalysisDatasets-small.zip")
  pin_get("MetaAnalysisDatasets-small.zip") %>%
    unzip() %>%
    load(envir = .GlobalEnv, verbose = TRUE)
}

## Regions of Interest for Analysis --------------------------------------------
meta.regions <- c("Verb", "Verb+1", "Verb+2")

## Variables for meta analysis - plotting --------------------------------------
chrono.order <- c("Tucker2015.A", "Tucker2015.B", "Exp1.A", "Exp2.A", "Exp3.A",
                  "Exp3.B", "Exp4.A", "Exp5.A", "Exp2.B", "Exp4.B", "Exp5.B")

exp.labels <- c("Exp1.A", "Exp2.A", "Exp2.B", "Exp3.A", "Exp3.B", "Exp4.A", 
                "Exp4.B", "Exp5.A", "Exp5.B", "Tucker2015.A", "Tucker2015.B")

exp.plotlabels <- c("1", "2A", "2B", "3: BrkPlAmb", 
                    "3: BrkPlUnamb", "4A", "4B", "5A", "5B", 
                    "T15: SndPl", "T15: BrkPl")


## Prepare Experiment 1 --------------------------------------------------------
exp1 <- exp1.data.saves %>%
  mutate(Experiment = factor(rep("Exp1", times = length(SubjID))),
         SubExperiment = factor(rep("A", times = length(SubjID))),
         SubjPhi = factor(rep("Masc", times = length(SubjID)))) %>%
  separate(Condition, into = c("Match", "Grammaticality"))

## Prepare Experiment 2 --------------------------------------------------------
exp2 <- exp2.data.saves %>%
  mutate(Experiment = factor(rep("Exp2", times = length(SubjID)))) %>%
  separate(Condition, into = c("SubjPhi", "Match", "Grammaticality"))

## Prepare Experiment 3 --------------------------------------------------------
exp3 <- exp3.data.saves %>%
  mutate(Experiment = factor(rep("Exp3", times = length(SubjID))),
         SubExperiment = factor(if_else(Ambiguity == "Ambiguous", "A", "B")),
         SubjPhi = factor(rep("Sg", times = length(SubjID)))) %>%
  separate(Condition, into = c("Match", "Grammaticality"))

## Prepare Experiment 4 --------------------------------------------------------
exp4 <- exp4.data.saves %>%
  mutate(Experiment = factor(rep("Exp4", times = length(SubjID)))) %>%
  separate(Condition, into = c("SubjPhi", "Match", "Grammaticality"))

## Prepare Experiment 5 --------------------------------------------------------
exp5 <- exp5.data.saves %>%
  mutate(Experiment = factor(rep("Exp5", times = length(SubjID)))) %>%
  separate(Condition, into = c("PhiFeature", "Match", "Grammaticality")) %>%
  mutate(SubjPhi = factor(if_else(PhiFeature == "Num", "Sg", "Masc")))

## Prepare Tucker 2015 ---------------------------------------------------------
tucker2015 <- tucker2015.data.saves %>%
  mutate(Experiment = factor(rep("Tucker2015", times = length(SubjID))),
         SubExperiment = factor(if_else(Gender == "Fem", "A", "B")),
         SubjPhi = factor(rep("Sg", times = length(SubjID)))) %>%
  separate(Condition, into = c("Match", "Grammaticality"))

## combine datasets ------------------------------------------------------------
meta.data <- bind_rows(exp1, exp2, exp3, exp4, exp5, tucker2015) %>%  
  mutate(SubjPhiType = factor(if_else(SubjPhi %in% 
                                        c("Sg", "Pl"), "Num", "Gend")))%>%
  
  unite(., "Condition", Match, Grammaticality) %>%
  unite(., "UniqueSubjID", SubjID, Experiment, SubExperiment, remove = FALSE) %>%
  spread(Condition, meanRT)

## Calculate the Grammaticality Effect in each experiment ----------------------
set.seed(1234)
meta.gram <- meta.data %>% 
  mutate(GrammaticalityEffect = (Match_Ungram + NoMatch_Ungram) - 
           (Match_Gram + NoMatch_Gram)) %>%
  group_by(Experiment, SubExperiment, SubjPhi, Region) %>%
  summarise(MeanEffect = mean(GrammaticalityEffect),
            SDEffect = sd(GrammaticalityEffect),
            N = n(),
            VarEffect = (SDEffect^2)/N,
            StudyWeightEffect = 1 / VarEffect,
            CIEffect = paste(bootES(GrammaticalityEffect)$bounds, 
                                      collapse = "/")) %>%
  mutate(Exp = factor(paste(Experiment, SubExperiment, sep = "."))) %>%
  separate(CIEffect, into = c("CI.min", "CI.max"), sep = "/", convert = TRUE) %>%
  mutate(EffectType = factor(rep("Grammaticality", times = length(Exp))))

## Calculate the Attraction Effect in each experiment within Ungramm sentences -
meta.attr.ungram <- meta.data %>% 
  mutate(IntrusionU = Match_Ungram - NoMatch_Ungram) %>%
  group_by(Experiment, SubExperiment, SubjPhi, Region) %>%
  summarise(MeanEffect = mean(IntrusionU),
            SDEffect = sd(IntrusionU),
            N = n(),
            VarEffect = (SDEffect^2)/N,
            StudyWeightEffect = 1 / VarEffect,
            CIEffect = paste(bootES(IntrusionU)$bounds, collapse = "/")) %>%
  mutate(Exp = factor(paste(Experiment, SubExperiment, sep = "."))) %>%
  separate(CIEffect, into = c("CI.min", "CI.max"), sep = "/", convert = TRUE) %>%
  mutate(EffectType = factor(rep("Attraction.U", times = length(Exp))))

## Calculate the Attraction Effect in each experiment within Gramm sentences ---
meta.attr.gram <- meta.data %>% 
  mutate(IntrusionG = Match_Gram - NoMatch_Gram) %>%
  group_by(Experiment, SubExperiment, SubjPhi, Region) %>%
  summarise(MeanEffect = mean(IntrusionG),
            SDEffect = sd(IntrusionG),
            N = n(),
            VarEffect = (SDEffect^2)/N,
            StudyWeightEffect = 1 / VarEffect,
            CIEffect = paste(bootES(IntrusionG)$bounds, collapse = "/")) %>%
  mutate(Exp = factor(paste(Experiment, SubExperiment, sep = "."))) %>%
  separate(CIEffect, into = c("CI.min", "CI.max"), sep = "/", convert = TRUE) %>%
  mutate(EffectType = factor(rep("Attraction.G", times = length(Exp))))

## Combining Grammatical and Attraction Effects into single dataset ------------
meta.studies <- bind_rows(meta.gram, meta.attr.gram, meta.attr.ungram)

## Calculate the Meta Analyis Estimates ----------------------------------------
## This uses the correct formulas
## same output as in package metafor
meta.estimates <- meta.studies %>%
  group_by(EffectType, SubjPhi, Region) %>%
  summarise(MeanEffect = sum(MeanEffect * StudyWeightEffect) /
              sum(StudyWeightEffect),
            CI.min = MeanEffect - (1.96 * sqrt(1/sum(StudyWeightEffect))),
            CI.max = MeanEffect + (1.96 * sqrt(1/sum(StudyWeightEffect)))) %>%
  mutate(Exp = factor(rep("MetaAnalysis", times = length(SubjPhi))))

## Attraction Effect
meta.estimates %>% 
  filter(Region %in% meta.regions) %>%
  filter(EffectType == "Attraction.U")

## Attraction Effect on Subject
meta.estimates %>% 
  filter(Region %in% meta.regions) %>%
  filter(EffectType == "Attraction.G")

meta.estimates %>% 
  filter(Region %in% meta.regions) %>%
  filter(EffectType == "Grammaticality")

## Meta Analysis Calculation and Plotting using package metafor ----------------
## Easy forest plot functions

### Number Attraction ----------------------------------------------------------

### Ungrammatical Sentences
### Singular and Plural Subjects
### Verb, Verb+1 and Verb+2 regions
meta.attr.sg.V.u <- meta.studies %>%
  filter(Region == "Verb", EffectType == "Attraction.U", SubjPhi %in% c("Sg"))
meta.attr.sg.V1.u <- meta.studies %>%
  filter(Region == "Verb+1", EffectType == "Attraction.U", SubjPhi %in% c("Sg"))
meta.attr.sg.V2.u <- meta.studies %>%
  filter(Region == "Verb+2", EffectType == "Attraction.U", SubjPhi %in% c("Sg"))
meta.attr.pl.V.u <- meta.studies %>%
  filter(Region == "Verb", EffectType == "Attraction.U", SubjPhi %in% c("Pl"))
meta.attr.pl.V1.u <- meta.studies %>%
  filter(Region == "Verb+1", EffectType == "Attraction.U", SubjPhi %in% c("Pl"))
meta.attr.pl.V2.u <- meta.studies %>%
  filter(Region == "Verb+2", EffectType == "Attraction.U", SubjPhi %in% c("Pl"))

### Grammatical Sentences
### Singular and Plural Subjects
### Verb, Verb+1 and Verb+2 regions
meta.attr.sg.V.g <- meta.studies %>%
  filter(Region == "Verb", EffectType == "Attraction.G", SubjPhi %in% c("Sg"))
meta.attr.sg.V1.g <- meta.studies %>%
  filter(Region == "Verb+1", EffectType == "Attraction.G", SubjPhi %in% c("Sg"))
meta.attr.sg.V2.g <- meta.studies %>%
  filter(Region == "Verb+2", EffectType == "Attraction.G", SubjPhi %in% c("Sg"))
meta.attr.pl.V.g <- meta.studies %>%
  filter(Region == "Verb", EffectType == "Attraction.G", SubjPhi %in% c("Pl"))
meta.attr.pl.V1.g <- meta.studies %>%
  filter(Region == "Verb+1", EffectType == "Attraction.G", SubjPhi %in% c("Pl"))
meta.attr.pl.V2.g <- meta.studies %>%
  filter(Region == "Verb+2", EffectType == "Attraction.G", SubjPhi %in% c("Pl"))


## Fitting the Meta Analysis FE Model for Number: Ungramm and Gramm Sentences --
## Ungrammatical Sentences - Singular and Plural Subjects
num.attr.sg.V.u <- rma(yi = MeanEffect, vi = VarEffect, method = "FE", 
                     slab = Exp, data = meta.attr.sg.V.u)
num.attr.sg.V1.u <- rma(yi = MeanEffect, vi = VarEffect, method = "FE", 
                      slab = Exp, data = meta.attr.sg.V1.u)
num.attr.sg.V2.u <- rma(yi = MeanEffect, vi = VarEffect, method = "FE", 
                      slab = Exp, data = meta.attr.sg.V2.u)
num.attr.pl.V.u <- rma(yi = MeanEffect, vi = VarEffect, method = "FE",
                     slab = Exp, data = meta.attr.pl.V.u)
num.attr.pl.V1.u <- rma(yi = MeanEffect, vi = VarEffect, method = "FE",
                      slab = Exp, data = meta.attr.pl.V1.u)
num.attr.pl.V2.u <- rma(yi = MeanEffect, vi = VarEffect, method = "FE",
                      slab = Exp, data = meta.attr.pl.V2.u)

## Grammatical Sentences - Singular and Plural Subjects
num.attr.sg.V.g <- rma(yi = MeanEffect, vi = VarEffect, method = "FE", 
                      slab = Exp, data = meta.attr.sg.V.g)
num.attr.sg.V1.g <- rma(yi = MeanEffect, vi = VarEffect, method = "FE",
                       slab = Exp, data = meta.attr.sg.V1.g)
num.attr.sg.V2.g <- rma(yi = MeanEffect, vi = VarEffect, method = "FE", 
                       slab = Exp, data = meta.attr.sg.V2.g)
num.attr.pl.V.g <- rma(yi = MeanEffect, vi = VarEffect, method = "FE",
                      slab = Exp, data = meta.attr.pl.V.g)
num.attr.pl.V1.g <- rma(yi = MeanEffect, vi = VarEffect, method = "FE",
                       slab = Exp, data = meta.attr.pl.V1.g)
num.attr.pl.V2.g <- rma(yi = MeanEffect, vi = VarEffect, method = "FE",
                       slab = Exp, data = meta.attr.pl.V2.g)

## Forest Plots: Number Attraction Ungrammatical + Grammatical Sentences, Sg ---
### Number Attraction plotting parameters: Singular Subjects Sentences ---------
spr.rt.width  <-  7
spr.rt.height <-  7.5
plotcex <-  1
plotcex.axis <- 1
plotcex.main <- 1.3
plotcex.axis.lab <- 1
plotdigits <- 0L
num.attr.sg.order <- na.omit(match(chrono.order, num.attr.sg.V.u$slab))
num.attr.pl.order <- na.omit(match(chrono.order, num.attr.pl.V.u$slab))
num.exp.labels.sg <- exp.plotlabels[exp.labels %in% num.attr.sg.V.u$slab]
num.exp.labels.pl <- exp.plotlabels[exp.labels %in% num.attr.pl.V.u$slab]

## Attraction Number Singular - Ungrammatical + Grammatical PDF plot -----------
pdf(file = here(figs.dir, "Attraction-Num-Sg.pdf"),
    width = spr.rt.width, height = spr.rt.height)
par(mar = c(4, 0, 0, 1.5))
par(mfrow = c(2, 3))

## First Row: Ungrammatical Sentences
forest(num.attr.sg.V.u, order = num.attr.sg.order,
       slab = num.exp.labels.sg,
       main = "\n\n\n\n\n\nVerb", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(num.attr.sg.V1.u, order = num.attr.sg.order,
       slab = num.exp.labels.sg,
       main = "\n\n\n\nSingular Subjects\nUngrammatical Sentences\nVerb+1", 
       cex = plotcex, cex.main = plotcex.main, cex.axis = plotcex.axis, 
       cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(num.attr.sg.V2.u, order = num.attr.sg.order,
       slab = num.exp.labels.sg,
       main = "\n\n\n\n\n\nVerb+2", cex = plotcex, cex.main = plotcex.main,
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)

## Second Row: Grammatical Sentences
forest(num.attr.sg.V.g, order = num.attr.sg.order, 
       slab = num.exp.labels.sg,
       main = "\n\n\n\n\n\nVerb", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(num.attr.sg.V1.g, order = num.attr.sg.order, 
       slab = num.exp.labels.sg,
       main = "\n\n\n\nSingular Subjects\nGrammatical Sentences\nVerb+1", 
       cex = plotcex, cex.main = plotcex.main, cex.axis = plotcex.axis, 
       cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(num.attr.sg.V2.g, order = num.attr.sg.order, 
       slab = num.exp.labels.sg,
       main = "\n\n\n\n\n\nVerb+2", 
       cex = plotcex, cex.main = plotcex.main, cex.axis = plotcex.axis, 
       cex.lab = plotcex.axis.lab, digits = plotdigits)
dev.off()

## Attraction Number Singular - Ungrammatical + Grammatical EPS plot -----------
setEPS()
postscript(file = path(figs.dir, "Attraction-Num-Sg.eps"),
           width = spr.rt.width, height = spr.rt.height)
par(mar = c(4, 0, 0, 1.5))
par(mfrow = c(2, 3))

## First Row: Ungrammatical Sentences
forest(num.attr.sg.V.u, order = num.attr.sg.order,
       slab = num.exp.labels.sg,
       main = "\n\n\n\n\n\nVerb", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(num.attr.sg.V1.u, order = num.attr.sg.order,
       slab = num.exp.labels.sg,
       main = "\n\n\n\nSingular Subjects\nUngrammatical Sentences\nVerb+1", 
       cex = plotcex, cex.main = plotcex.main, cex.axis = plotcex.axis, 
       cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(num.attr.sg.V2.u, order = num.attr.sg.order,
       slab = num.exp.labels.sg,
       main = "\n\n\n\n\n\nVerb+2", cex = plotcex, cex.main = plotcex.main,
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)

## Second Row: Grammatical Sentences
forest(num.attr.sg.V.g, order = num.attr.sg.order, 
       slab = num.exp.labels.sg,
       main = "\n\n\n\n\n\nVerb", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(num.attr.sg.V1.g, order = num.attr.sg.order, 
       slab = num.exp.labels.sg,
       main = "\n\n\n\nSingular Subjects\nGrammatical Sentences\nVerb+1", 
       cex = plotcex, cex.main = plotcex.main, cex.axis = plotcex.axis, 
       cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(num.attr.sg.V2.g, order = num.attr.sg.order, 
       slab = num.exp.labels.sg,
       main = "\n\n\n\n\n\nVerb+2", 
       cex = plotcex, cex.main = plotcex.main, cex.axis = plotcex.axis, 
       cex.lab = plotcex.axis.lab, digits = plotdigits)
dev.off()

## Forest Plots: Number Attraction Ungrammatical + Grammatical Sentences, Pl ---
## Number Attraction plotting parameters: Plural Subjects Sentences ---------
spr.rt.width  <-  7
spr.rt.height <-  3.75
plotcex <-  1
plotcex.axis <- 1
plotcex.main <- 1
plotcex.axis.lab <- 1
plotdigits <- 0L


## Attraction Number Plural - Ungrammatical + Grammatical PDF plots ------------
pdf(file = here(figs.dir, "Attraction-Num-Pl.pdf"),
    width = spr.rt.width, height = spr.rt.height)
par(mar = c(4, 0, 0, 1.5))
par(mfrow = c(2, 3))

## First Row: Ungrammatical Sentences
forest(num.attr.pl.V.u, order = num.attr.pl.order, 
       slab = num.exp.labels.pl,
       main = "\n\n\n\n\nVerb", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(num.attr.pl.V1.u, order = num.attr.pl.order, 
       slab = num.exp.labels.pl,
       main = "\n\n\nPlural Subjects\nUngrammatical Sentences\nVerb+1", 
       cex = plotcex, cex.main = plotcex.main, cex.axis = plotcex.axis, 
       cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(num.attr.pl.V2.u, order = num.attr.pl.order, 
       slab = num.exp.labels.pl,
       main = "\n\n\n\n\nVerb+2", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
## Second Row: Grammatical Sentences
forest(num.attr.pl.V.g, order = num.attr.pl.order, 
       slab = num.exp.labels.pl,
       main = "\n\n\n\n\nVerb", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(num.attr.pl.V1.g, order = num.attr.pl.order, 
       slab = num.exp.labels.pl,
       main = "\n\n\nPlural Subjects\nGrammatical Sentences\nVerb+1", 
       cex = plotcex, cex.main = plotcex.main, cex.axis = plotcex.axis, 
       cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(num.attr.pl.V2.g, order = num.attr.pl.order, 
       slab = num.exp.labels.pl,
       main = "\n\n\n\n\nVerb+2", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
dev.off()

## Attraction Number Plural - Ungrammatical + Grammatical PDF plots ------------
setEPS()
postscript(file = path(figs.dir, "Attraction-Num-Pl.eps"),
           width = spr.rt.width, height = spr.rt.height)
par(mar = c(4, 0, 0, 1.5))
par(mfrow = c(2, 3))

## First Row: Ungrammatical Sentences
forest(num.attr.pl.V.u, order = num.attr.pl.order, 
       slab = num.exp.labels.pl,
       main = "\n\n\n\n\nVerb", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(num.attr.pl.V1.u, order = num.attr.pl.order, 
       slab = num.exp.labels.pl,
       main = "\n\n\nPlural Subjects\nUngrammatical Sentences\nVerb+1", 
       cex = plotcex, cex.main = plotcex.main, cex.axis = plotcex.axis, 
       cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(num.attr.pl.V2.u, order = num.attr.pl.order, 
       slab = num.exp.labels.pl,
       main = "\n\n\n\n\nVerb+2", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
## Second Row: Grammatical Sentences
forest(num.attr.pl.V.g, order = num.attr.pl.order, 
       slab = num.exp.labels.pl,
       main = "\n\n\n\n\nVerb", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(num.attr.pl.V1.g, order = num.attr.pl.order, 
       slab = num.exp.labels.pl,
       main = "\n\n\nPlural Subjects\nGrammatical Sentences\nVerb+1", 
       cex = plotcex, cex.main = plotcex.main, cex.axis = plotcex.axis, 
       cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(num.attr.pl.V2.g, order = num.attr.pl.order, 
       slab = num.exp.labels.pl,
       main = "\n\n\n\n\nVerb+2", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
dev.off()


### Gender Attraction ----------------------------------------------------------

### Ungrammatical Sentences
### Masculine and Feminine Subjects
### Verb, Verb+1 and Verb+2 regions
meta.attr.masc.V.u <- meta.studies %>%
  filter(Region == "Verb", EffectType == "Attraction.U", 
         SubjPhi %in% c("Masc"))
meta.attr.masc.V1.u <- meta.studies %>%
  filter(Region == "Verb+1", EffectType == "Attraction.U",
         SubjPhi %in% c("Masc"))
meta.attr.masc.V2.u <- meta.studies %>%
  filter(Region == "Verb+2", EffectType == "Attraction.U",
         SubjPhi %in% c("Masc"))
meta.attr.fem.V.u <- meta.studies %>%
  filter(Region == "Verb", EffectType == "Attraction.U",
         SubjPhi %in% c("Fem"))
meta.attr.fem.V1.u <- meta.studies %>%
  filter(Region == "Verb+1", EffectType == "Attraction.U",
         SubjPhi %in% c("Fem"))
meta.attr.fem.V2.u <- meta.studies %>%
  filter(Region == "Verb+2", EffectType == "Attraction.U",
         SubjPhi %in% c("Fem"))

### Grammatical Sentences
### Masculine and Feminine Subjects
### Verb, Verb+1 and Verb+2 regions
meta.attr.masc.V.g <- meta.studies %>%
  filter(Region == "Verb", EffectType == "Attraction.G", 
         SubjPhi %in% c("Masc"))
meta.attr.masc.V1.g <- meta.studies %>%
  filter(Region == "Verb+1", EffectType == "Attraction.G",
         SubjPhi %in% c("Masc"))
meta.attr.masc.V2.g <- meta.studies %>%
  filter(Region == "Verb+2", EffectType == "Attraction.G",
         SubjPhi %in% c("Masc"))
meta.attr.fem.V.g <- meta.studies %>%
  filter(Region == "Verb", EffectType == "Attraction.G",
         SubjPhi %in% c("Fem"))
meta.attr.fem.V1.g <- meta.studies %>%
  filter(Region == "Verb+1", EffectType == "Attraction.G",
         SubjPhi %in% c("Fem"))
meta.attr.fem.V2.g <- meta.studies %>%
  filter(Region == "Verb+2", EffectType == "Attraction.G",
         SubjPhi %in% c("Fem"))

## Fitting the Meta Analysis FE Model for Gender: Ungramm and Gramm Sentences --
## Ungrammatical Sentences - Masculine and Feminine Subjects
gend.attr.masc.V.u <- rma(yi = MeanEffect, vi = VarEffect, method = "FE",
                          slab = Exp,data = meta.attr.masc.V.u)
gend.attr.masc.V1.u <- rma(yi = MeanEffect, vi = VarEffect, method = "FE", 
                           slab = Exp, data = meta.attr.masc.V1.u)
gend.attr.masc.V2.u <- rma(yi = MeanEffect, vi = VarEffect, method = "FE", 
                           slab = Exp, data = meta.attr.masc.V2.u)
gend.attr.fem.V.u <- rma(yi = MeanEffect, vi = VarEffect, method = "FE",
                         slab = Exp, data = meta.attr.fem.V.u)
gend.attr.fem.V1.u <- rma(yi = MeanEffect, vi = VarEffect, method = "FE",
                          slab = Exp, data = meta.attr.fem.V1.u)
gend.attr.fem.V2.u <- rma(yi = MeanEffect, vi = VarEffect, method = "FE",
                          slab = Exp, data = meta.attr.fem.V2.u)

## Grammatical Sentences - Masculine and Feminine Subjects
gend.attr.masc.V.g <- rma(yi = MeanEffect, vi = VarEffect, method = "FE",
                          slab = Exp, data = meta.attr.masc.V.g)
gend.attr.masc.V1.g <- rma(yi = MeanEffect, vi = VarEffect, method = "FE",
                           slab = Exp, data = meta.attr.masc.V1.g)
gend.attr.masc.V2.g <- rma(yi = MeanEffect, vi = VarEffect, method = "FE",
                           slab = Exp, data = meta.attr.masc.V2.g)
gend.attr.fem.V.g <- rma(yi = MeanEffect, vi = VarEffect, method = "FE",
                         slab = Exp, data = meta.attr.fem.V.g)
gend.attr.fem.V1.g <- rma(yi = MeanEffect, vi = VarEffect, method = "FE",
                          slab = Exp, data = meta.attr.fem.V1.g)
gend.attr.fem.V2.g <- rma(yi = MeanEffect, vi = VarEffect, method = "FE",
                          slab = Exp, data = meta.attr.fem.V2.g)

## Forest Plots: Gender Attraction Ungrammatical + Grammatical Sentences, Masc -
## Gender Attraction plotting parameters: Masc Subjects Sentences ---------
spr.rt.width  <-  7
spr.rt.height <-  7.75
plotcex <-  1
plotcex.axis <- 1
plotcex.main <- 1.3
plotcex.axis.lab <- 1
plotdigits <- 0L
gend.attr.masc.order <- na.omit(match(chrono.order, gend.attr.masc.V.u$slab))
gend.attr.fem.order <- na.omit(match(chrono.order, gend.attr.fem.V.u$slab))
gend.exp.labels.masc <- exp.plotlabels[exp.labels %in% gend.attr.masc.V.u$slab]
gend.exp.labels.fem <- exp.plotlabels[exp.labels %in% gend.attr.fem.V.u$slab]

## Attraction Gender Masc - Ungrammatical + Grammatical PDF plot ---------------
pdf(file = here(figs.dir, "Attraction-Gen-Masc.pdf"),
    width = spr.rt.width, height = spr.rt.height)
par(mar = c(4, 0, 0, 1.5))
par(mfrow = c(2, 3))

## First row: Ungrammatical Sentences
forest(gend.attr.masc.V.u, order = gend.attr.masc.order,
       slab = gend.exp.labels.masc,
       main = "\n\n\n\n\n\n\n\nVerb", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(gend.attr.masc.V1.u, order = gend.attr.masc.order, 
       slab = gend.exp.labels.masc,
       main = "\n\n\n\n\nMasculine Subjects\nUngrammatical Sentences\n\nVerb+1",
       cex = plotcex, cex.main = plotcex.main, cex.axis = plotcex.axis, 
       cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(gend.attr.masc.V2.u, order = gend.attr.masc.order, 
       slab = gend.exp.labels.masc,
       main = "\n\n\n\n\n\n\n\nVerb+2", cex = plotcex, cex.main = plotcex.main,
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
## Second Row: Grammatical Sentences
forest(gend.attr.masc.V.g, order = gend.attr.masc.order, 
       slab = gend.exp.labels.masc,
       main = "\n\n\n\n\n\n\n\nVerb", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(gend.attr.masc.V1.g, order = gend.attr.masc.order,
       slab = gend.exp.labels.masc,
       main = "\n\n\n\n\nMasculine Subjects\nGrammatical Sentences\n\nVerb+1", 
       cex = plotcex, cex.main = plotcex.main, cex.axis = plotcex.axis, 
       cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(gend.attr.masc.V2.g, order = gend.attr.masc.order,
       slab = gend.exp.labels.masc,
       main = "\n\n\n\n\n\n\n\nVerb+2", cex = plotcex, cex.main = plotcex.main,
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
dev.off()

## Attraction Gender Masc - Ungrammatical + Grammatical EPS plot ---------------
setEPS()
postscript(file = path(figs.dir, "Attraction-Gen-Masc.eps"),
           width = spr.rt.width, height = spr.rt.height)
par(mar = c(4, 0, 0, 1.5))
par(mfrow = c(2, 3))

## First Row: Ungrammatical Sentences
forest(gend.attr.masc.V.u, order = gend.attr.masc.order,
       slab = gend.exp.labels.masc,
       main = "\n\n\n\n\n\n\n\nVerb", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(gend.attr.masc.V1.u, order = gend.attr.masc.order, 
       slab = gend.exp.labels.masc,
       main = "\n\n\n\n\nMasculine Subjects\nUngrammatical Sentences\n\nVerb+1",
       cex = plotcex, cex.main = plotcex.main, cex.axis = plotcex.axis, 
       cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(gend.attr.masc.V2.u, order = gend.attr.masc.order, 
       slab = gend.exp.labels.masc,
       main = "\n\n\n\n\n\n\n\nVerb+2", cex = plotcex, cex.main = plotcex.main,
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
## Second Row: Grammatical Sentences
forest(gend.attr.masc.V.g, order = gend.attr.masc.order, 
       slab = gend.exp.labels.masc,
       main = "\n\n\n\n\n\n\n\nVerb", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(gend.attr.masc.V1.g, order = gend.attr.masc.order,
       slab = gend.exp.labels.masc,
       main = "\n\n\n\n\nMasculine Subjects\nGrammatical Sentences\n\nVerb+1", 
       cex = plotcex, cex.main = plotcex.main, cex.axis = plotcex.axis, 
       cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(gend.attr.masc.V2.g, order = gend.attr.masc.order,
       slab = gend.exp.labels.masc,
       main = "\n\n\n\n\n\n\n\nVerb+2", cex = plotcex, cex.main = plotcex.main,
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
dev.off()


## Forest Plots: Gender Attraction Ungrammatical + Grammatical Sentences, Fem --
## Gender Attraction plotting parameters: Fem Subjects Sentences ---------------
spr.rt.width  <-  7
spr.rt.height <-  3.75
plotcex <-  1
plotcex.axis <- 1
plotcex.main <- 1
plotcex.axis.lab <- 1
plotdigits <- 0L

# Attraction Gender Feminine - Ungrammatical + Grammatical PDF plot
pdf(file = here(figs.dir, "Attraction-Gen-Fem.pdf"),
    width = spr.rt.width, height = spr.rt.height)
par(mar = c(4, 0, 0, 1.5))
par(mfrow = c(2, 3))

## First Row: Ungrammatical Sentences
forest(gend.attr.fem.V.u, order = gend.attr.fem.order,
       slab = gend.exp.labels.fem,
       main = "\n\n\n\n\nVerb", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(gend.attr.fem.V1.u, order = gend.attr.fem.order, 
       slab = gend.exp.labels.fem,
       main = "\n\n\nFeminine Subjects\nUngrammatical Sentences\nVerb+1", 
       cex = plotcex, cex.main = plotcex.main, cex.axis = plotcex.axis, 
       cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(gend.attr.fem.V2.u, order = gend.attr.fem.order, 
       slab = gend.exp.labels.fem,
       main = "\n\n\n\n\nVerb+2", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)

## Second Row: Grammatical Sentences
forest(gend.attr.fem.V.g, order = gend.attr.fem.order,
       slab = gend.exp.labels.fem,
       main = "\n\n\n\n\nVerb", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(gend.attr.fem.V1.g, order = gend.attr.fem.order, 
       slab = gend.exp.labels.fem,
       main = "\n\n\nFeminine Subjects\nGrammatical Sentences\nVerb+1", 
       cex = plotcex, cex.main = plotcex.main, cex.axis = plotcex.axis, 
       cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(gend.attr.fem.V2.g, order = gend.attr.fem.order, 
       slab = gend.exp.labels.fem,
       main = "\n\n\n\n\nVerb+2", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
dev.off()

# Attraction Gender Feminine - Ungrammatical + Grammatical EPS plot
setEPS()
postscript(file = path(figs.dir, "Attraction-Gen-Fem.eps"),
           width = spr.rt.width, height = spr.rt.height)
par(mar = c(4, 0, 0, 1.5))
par(mfrow = c(2, 3))
## First Row: Ungrammatical Sentences
forest(gend.attr.fem.V.u, order = gend.attr.fem.order,
       slab = gend.exp.labels.fem,
       main = "\n\n\n\n\nVerb", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(gend.attr.fem.V1.u, order = gend.attr.fem.order, 
       slab = gend.exp.labels.fem,
       main = "\n\n\nFeminine Subjects\nUngrammatical Sentences\nVerb+1", 
       cex = plotcex, cex.main = plotcex.main, cex.axis = plotcex.axis, 
       cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(gend.attr.fem.V2.u, order = gend.attr.fem.order, 
       slab = gend.exp.labels.fem,
       main = "\n\n\n\n\nVerb+2", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)

## Second Row: Grammatical Sentences
forest(gend.attr.fem.V.g, order = gend.attr.fem.order,
       slab = gend.exp.labels.fem,
       main = "\n\n\n\n\nVerb", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(gend.attr.fem.V1.g, order = gend.attr.fem.order, 
       slab = gend.exp.labels.fem,
       main = "\n\n\nFeminine Subjects\nGrammatical Sentences\nVerb+1", 
       cex = plotcex, cex.main = plotcex.main, cex.axis = plotcex.axis, 
       cex.lab = plotcex.axis.lab, digits = plotdigits)
forest(gend.attr.fem.V2.g, order = gend.attr.fem.order, 
       slab = gend.exp.labels.fem,
       main = "\n\n\n\n\nVerb+2", cex = plotcex, cex.main = plotcex.main, 
       cex.axis = plotcex.axis, cex.lab = plotcex.axis.lab, digits = plotdigits)
dev.off()

## Clean-up --------------------------------------------------------------------
rm(list = ls())
