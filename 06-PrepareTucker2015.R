################################################################################
# Analysis Script - Processing the Tucker et al., 2015 data                    #
# "Attraction Effects for Verbal Gender and Number Are Similar but Not         #
# Identical: Self-Paced Reading Evidence from Modern Standard Arabic"          #
# M. A. Tucker, A. Idrissi, and D. Almeida                                     #
# Script Author: Diogo Almeida <diogo@nyu.edu,                                 #
#                Matt Tucker <matt.tucker@nyu.edu>                             #
# v. 1, current 29 January 2017                                                #
# v. 2, current 26 December 2020                                               #
# v. 3, current 29 December 2020                                               #
################################################################################
## Loading Packages ------------------------------------------------------------
library(tidyverse)
library(viridis)
library(ragg)
library(pins)
library(fs)
library(here)
library(stargazer)
library(bootES)
library(sessioninfo)
library(pander)

## Functions -------------------------------------------------------------------
lamb.winsorize <- function(x, cut.off = 0.01, cut.off.unit = "percent") {
  if (is.list(x)) {
    x <- unlist(x)
  }
  winsorized.x <- x
  if (cut.off.unit == "sd") {
    sdx <- sd(x)
    mx  <- mean(x)
    lowerx <- mx - (cut.off * sdx)
    upperx <- mx + (cut.off * sdx)
  } else {
    if (cut.off.unit == "percent") {
      len.x <- length(x)
      x.ascending <- sort(x)
      g <- trunc(cut.off * len.x)
      lowerx <- x.ascending[g + 1]
      upperx <- x.ascending[len.x - g]
    } else {
      stop("'cut.off.unit' must be 'sd' (standard deviation) or 'percent'!")
    }
  }
  winsorized.x[x <= lowerx] <- lowerx
  winsorized.x[x >= upperx] <- upperx
  return(winsorized.x)
}
lamb.sem <- function(x) {
  if (any(is.na(x))){
    x <- x[!is.na(x)]
  }
  return(sqrt(var(x)/length(x)))
}

## plotting variables ----------------------------------------------------------
manuscript.spr.plot.theme <- theme_bw() + 
  theme(legend.key = element_blank(), 
        plot.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(color = 'black'),
        axis.text.x = element_text(angle = 45, hjust = 1),        
        legend.position="bottom",
        legend.title = element_blank())

spr.rt.width  <-  6.7
spr.rt.height <-  3.7

## Set variable paths for data, figures and tables produced by analysis --------
this_exp <- "exp6-tucker2015"
data.dir <- here(this_exp, "data")
figs.dir <- here(this_exp, "figures")
tbls.dir <- here(this_exp, "tables")
meta.dir <- here("meta")
dir_create(c(data.dir, figs.dir, tbls.dir, meta.dir))

## Getting data ----------------------------------------------------------------
exp6.datafile.figshare <- "https://ndownloader.figshare.com/files/25917369"
pin(exp6.datafile.figshare, name = "tucker2015_data")
tucker2015.original <- pin_get("tucker2015_data") %>% 
  readr::read_csv()

## Saving a copy of the original dataset on the data folder --------------------
tucker2015.original %>%
  readr::write_csv(path(data.dir, "tucker2015.csv"))

## Set parameters for data analysis --------------------------------------------
wins.cutoff  <- .01   # 1% cutoff
error.cutoff <- .5    # Original Analysis = .5; Alternative = .67 (roughly 2/3)

## Regions of Interest for Data Analysis ---------------------------------------
rois <- c("Verb", "Verb+1", "Verb+2")

tucker2015.regions <- c("NP Subj", "Comp", "RC Verb", "Attr", "Adverb", "Verb", 
                  "Verb+1", "Verb+2")

## Exclude Fillers, Incorrect trials, NAs, and Regions after Verb+2 ------------
tucker2015.data.ok <- tucker2015.original %>%
  filter(Correct == 1 & Condition != "Filler" & Region %in% tucker2015.regions & 
           !is.na(RT)) %>%
  tibble::as_tibble(.)

## Winsorizing the data at 1% --------------------------------------------------
tucker2015.data.ok <- tucker2015.data.ok %>%
  group_by(Condition, Region) %>%
  mutate(RTw01 = lamb.winsorize(RT, cut.off = 0.01, cut.off.unit = "percent")) %>%
  ungroup()

## Subject averages ------------------------------------------------------------
tucker2015.data.saves <- tucker2015.data.ok %>% 
  mutate(Match = factor(
    ifelse(Condition %in% c("Pl/Ungram", "Sg/Gram"), "NoMatch", "Match"), 
    levels = c("Match", "NoMatch"))) %>%
  rename(., ConditionOrig = Condition) %>%
  rename(., GramOrig = Grammaticality) %>%
  mutate(Grammaticality = factor(
    ifelse(GramOrig == "Grammatical", "Gram", "Ungram"))) %>%
  unite(., Condition, Match, Grammaticality, sep = "/", remove = FALSE) %>%
  filter(Region %in% tucker2015.regions) %>%
  group_by(SubjID, Condition, Gender, Region) %>%
  summarise(meanRT = mean(RTw01, na.rm = TRUE)) %>%
  ungroup()

## Find subjects who had empty cells -------------------------------------------
## i.e., who had conditons in which no average data can be computed given our
## exclusion criteria
tucker2015.subjects.empty.cells <- tucker2015.data.saves %>%
  group_by(Region, SubjID) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  filter(count < 4) %>% 
  select(SubjID) %>%
  distinct() %>%
  unlist() %>%
  as.vector()

## Excluding participants that did not meet the error rate threshold -----------
tucker2015.bad.subjects <- tucker2015.original %>%
  filter(Condition != "Filler" & Region %in% tucker2015.regions) %>%
  group_by(SubjID) %>%
  summarise(MeanCorrect = mean(Correct, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(MeanCorrect < .5) %>%
  select(SubjID)

## Excluding subjects with empty cells and who answered less than 50% correct --
tucker2015.data.ok <- tucker2015.data.ok %>%
  filter(!SubjID %in% tucker2015.bad.subjects) %>%
  droplevels()

tucker2015.data.saves <- tucker2015.data.saves %>%
  filter(!SubjID %in% tucker2015.subjects.empty.cells &
           !SubjID %in% tucker2015.bad.subjects) %>%
  droplevels()

## Creating grand averages for plotting ----------------------------------------
tucker2015.data.gdave <- tucker2015.data.saves %>%
  group_by(Condition, Gender, Region) %>%
  summarise(gd.avg.RT = mean(meanRT, na.rm = TRUE),
            gd.avg.RT.SE = lamb.sem(meanRT),
            se.min = gd.avg.RT - gd.avg.RT.SE,
            se.max = gd.avg.RT + gd.avg.RT.SE) %>%
  separate(Condition, into = c("Match", "Grammaticality"), sep = "/", 
           remove = FALSE) %>%
  ungroup() %>%
  mutate(SubjPhi = rep("Num", time = length(Match))) %>%
  mutate(Experiment = rep("tucker2015", times = length(SubjPhi))) %>%
  droplevels()

## Grand Average Plotting ------------------------------------------------------
tucker2015.grand.average.plot <- ggplot(tucker2015.data.gdave, 
                                  aes(x = Region, y = gd.avg.RT, 
                                      group = Condition, 
                                      colour = Condition))

plot.manuscript.all <- tucker2015.grand.average.plot +
  labs(x = "Region", y = "Raw RT (ms)", 
       title = "Tucker 2015: Number Attraction, Singular Subject by Gender") + 
  geom_line(aes(linetype = Condition)) + 
  geom_point(aes(shape = Condition), size = 3) +
  geom_errorbar(aes(ymax = se.max, ymin = se.min), width = 0.1) + 
  facet_grid(Gender ~ .) +
  scale_shape_manual(values = c("Match/Gram" = 15, 
                                "Match/Ungram" = 15,
                                "NoMatch/Gram" = 0, 
                                "NoMatch/Ungram" = 0)) + 
  scale_linetype_manual(values = c("NoMatch/Gram" = 1, 
                                   "Match/Gram" = 1,
                                   "NoMatch/Ungram" = 2, 
                                   "Match/Ungram" = 2)) + 
  scale_colour_manual(values = c("NoMatch/Gram" = viridis_pal()(10)[7], 
                                 "Match/Gram" = viridis_pal()(10)[7], 
                                 "NoMatch/Ungram" = viridis_pal()(10)[2], 
                                 "Match/Ungram" = viridis_pal()(10)[2])) +
  manuscript.spr.plot.theme + guides(shape = guide_legend(nrow = 1, 
                                                          byrow = TRUE)) + 
  annotate("rect", xmin = 5.5, xmax = 8.5, ymin = -Inf, ymax = Inf, alpha = 0.2,
           fill = "grey")

## Save the plots --------------------------------------------------------------
ggsave(plot.manuscript.all,
       file = path(figs.dir, "tucker2015.pdf"),
       width = spr.rt.width, height = spr.rt.height)
ggsave(plot.manuscript.all,
       file = path(figs.dir, "tucker2015.eps"), 
       width = spr.rt.width, height = spr.rt.height, device = cairo_ps)
agg_png(filename = path(figs.dir, "tucker2015.png"),
        width = spr.rt.width, height = spr.rt.height, units = "in",
        res = 216)
print(plot.manuscript.all)
dev.off()

## Mean and bootstrapped CIs for the effects of interest -----------------------
## Effect 1: Attraction in Ungrammatical Sentences
## Effect 2: Attraction in Grammatical Sentences
## Effect 3: Grammaticality Effect
## -----------------------------------------------
## Using a specific seed makes this reproducible
## we did not use a specific seed for the original analysis so the exact CIs
## will not be identical to the ones reported in the paper.
set.seed(1234) 
tucker2015.reformatted.for.cis <- tucker2015.data.saves %>%
  mutate(Experiment = factor(rep("Tucker2015", times = length(SubjID))),
         SubExperiment = Gender,
         SubjPhi = factor(rep("Sg", times = length(SubjID)))) %>%
  separate(Condition, into = c("Match", "Grammaticality")) %>%
  unite(., "Condition", Match, Grammaticality) %>%
  pivot_wider(names_from = Condition, values_from = meanRT)

## Effect 1: Attraction in Ungrammatical Sentences
tucker2015.attr.ungram <-  tucker2015.reformatted.for.cis %>%
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
  mutate(EffectType = rep("Attraction_Ungrammatical", times = length(Exp))) %>%
  ungroup()


## Effect 2: Attraction in Grammatical Sentences
tucker2015.attr.gram <-  tucker2015.reformatted.for.cis %>%
  mutate(IntrusionU = Match_Gram - NoMatch_Gram) %>%
  group_by(Experiment, SubExperiment, SubjPhi, Region) %>%
  summarise(MeanEffect = mean(IntrusionU),
            SDEffect = sd(IntrusionU),
            N = n(),
            VarEffect = (SDEffect^2)/N,
            StudyWeightEffect = 1 / VarEffect,
            CIEffect = paste(bootES(IntrusionU)$bounds, collapse = "/")) %>%
  mutate(Exp = factor(paste(Experiment, SubExperiment, sep = "."))) %>%
  separate(CIEffect, into = c("CI.min", "CI.max"), sep = "/", convert = TRUE) %>%
  mutate(EffectType = rep("Attraction_Grammatical", times = length(Exp))) %>%
  ungroup()

## Effect 3: Grammaticality Effect
tucker2015.grammaticality <-  tucker2015.reformatted.for.cis %>%
  mutate(GrammaticalityEffect = (Match_Ungram + NoMatch_Ungram) - 
           (Match_Gram + NoMatch_Gram)) %>%
  group_by(Experiment, SubExperiment, SubjPhi, Region) %>%
  summarise(MeanEffect = mean(GrammaticalityEffect),
            SDEffect = sd(GrammaticalityEffect),
            N = n(),
            VarEffect = (SDEffect^2)/N,
            StudyWeightEffect = 1 / VarEffect,
            CIEffect = paste(bootES(GrammaticalityEffect)$bounds, collapse = "/")) %>%
  mutate(Exp = factor(paste(Experiment, SubExperiment, sep = "."))) %>%
  separate(CIEffect, into = c("CI.min", "CI.max"), sep = "/", convert = TRUE) %>%
  mutate(EffectType = rep("Grammaticality", times = length(Exp))) %>%
  ungroup()

## Put all effects together
tucker2015.effects <- bind_rows(tucker2015.attr.ungram, tucker2015.attr.gram, 
                                tucker2015.grammaticality)

## LaTeX tables for Effects and their Bootstrapped 95% CIs ---------------------
tucker2015.effects %>%
  filter(Region %in% rois) %>%
  mutate_if(is.factor, as.character) %>%
  group_by(SubExperiment) %>%
  mutate(SE = sqrt(VarEffect),
         Mean = paste0(round(MeanEffect, 0), " (", round(CI.min, 0), ", ", round(CI.max, 0), ")")) %>%
  ungroup() %>%
  select(Region, EffectType, SubExperiment, Mean) %>%
  droplevels() %>%
  pivot_wider(names_from = Region, values_from = Mean) %>%
  stargazer::stargazer(type = "latex", summary = FALSE, rownames = FALSE,
                       out=path(tbls.dir, "tucker2015-cis.tex"))


## Save the relevant data structures for future manipulation -------------------
save(tucker2015.data.ok, tucker2015.data.saves, tucker2015.data.gdave, 
     tucker2015.effects,
     file = path(meta.dir, "Tucker2015-DataForMetaAnalysis.RData"))

## Save the session information to compare with original if needed -------------
session_info_name <- paste0("tucker2020_", lubridate::today(), ".txt")
session_information <- session_info()
sink(file = path(session_info_name))
pander(session_information)
sink()

## Clean environment -----------------------------------------------------------
rm(list = ls())

