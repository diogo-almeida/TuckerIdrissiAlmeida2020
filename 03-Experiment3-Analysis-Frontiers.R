################################################################################
# Analysis Script - Experiment 3                                               #
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

## Functions -------------------------------------------------------------------
lamb.winsorize <- function(x, cut.off = 0.1, cut.off.unit = "percent") {
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
        plot.title = element_text(hjust = 0.5),
        plot.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(color = 'black'),
        legend.position="bottom",
        legend.title = element_blank())

spr.rt.width  <-  6.7
spr.rt.height <-  3.7

## Set variable paths for data, figures and tables produced by analysis --------
this_exp <- "exp3"
data.dir <- here(this_exp, "data")
figs.dir <- here(this_exp, "figures")
tbls.dir <- here(this_exp, "tables")
meta.dir <- here("meta")
dir_create(c(data.dir, figs.dir, tbls.dir, meta.dir))

## Getting data ----------------------------------------------------------------
exp3.datafile.figshare <- "https://ndownloader.figshare.com/files/25918542"
pin(exp3.datafile.figshare, name = "experiment3_data")
exp3.original <- pin_get("experiment3_data") %>% 
  readr::read_csv()

## Saving a copy of the original dataset on the data folder --------------------
exp3.original %>%
  readr::write_csv(path(data.dir, "experiment3.csv"))

### Set parameters for data analysis -------------------------------------------
wins.cutoff  <- .01   # 1% cutoff
error.cutoff <- .5    # Original Analysis = .5; Alternative = .67 (roughly 2/3)

## Regions of Interest for Data Analysis ---------------------------------------
rois <- c("Verb", "Verb+1", "Verb+2")

exp3.regions <- c("NP Subj", "Comp", "RC Verb", "Attr", "Adverb", "Verb", 
                  "Verb+1", "Verb+2")

## Exclude Fillers, Incorrect trials, NAs, and Regions after Verb+2 ------------
exp3.data.ok <- exp3.original %>%
  filter(Correct == 1 & Condition != "Filler" & Region %in% exp3.regions & 
           !is.na(RT))

## Winsorizing the data at 1% --------------------------------------------------
exp3.data.ok <- exp3.data.ok %>%
  group_by(Condition, Region) %>%
  mutate(RTw01 = lamb.winsorize(RT, cut.off = 0.01, 
                                cut.off.unit = "percent")) %>%
  ungroup()

## Subject averages per ambiguity condition ------------------------------------
exp3.data.saves <- exp3.data.ok %>% 
  group_by(SubjID, Condition, Ambiguity, Region) %>%
  summarise(meanRT = mean(RTw01, na.rm = TRUE)) %>%
  ungroup()

## Find subjects who had empty cells -------------------------------------------
## i.e., who had conditions in which no average data can be computed given our
## exclusion criteria
exp3.subjects.empty.cells <- exp3.data.saves %>%
  group_by(Region, SubjID, Ambiguity) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  filter(count < 4) %>% 
  select(SubjID) %>%
  distinct() %>%
  unlist() %>%
  as.vector()

## Find subjects who has less than 50% comprehension questions correct ---------
exp3.bad.subjects <- exp3.original %>%
  filter(Condition != "Filler" & Region %in% exp3.regions) %>%
  group_by(SubjID) %>%
  summarise(MeanCorrect = mean(Correct, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(MeanCorrect < .5) %>%
  select(SubjID)

## Excluding subjects with empty cells and who answered less than 50% correct --
exp3.data.ok <- exp3.data.ok %>%
  filter(!SubjID %in% exp3.bad.subjects) %>%
  droplevels()

exp3.data.saves <- exp3.data.saves %>%
  filter(!SubjID %in% exp3.subjects.empty.cells &
           !SubjID %in% exp3.bad.subjects) %>%
  droplevels()

## Grand Averages --------------------------------------------------------------
exp3.data.gdave <- exp3.data.saves %>%
  group_by(Condition, Region, Ambiguity) %>%
  summarise(gd.avg.RT = mean(meanRT, na.rm = TRUE),
            gd.avg.RT.SE = lamb.sem(meanRT),
            se.min = gd.avg.RT - gd.avg.RT.SE,
            se.max = gd.avg.RT + gd.avg.RT.SE) %>%
  separate(Condition, into = c("Match", "Grammaticality"), sep = "/", 
           remove = FALSE) %>%
  ungroup() %>%
  mutate(SubjPhi = rep("Num", time = length(Match))) %>%
  mutate(Experiment = rep("exp3.A", times = length(SubjPhi))) %>%
  droplevels()

## Means & Variances Tables for LaTeX ------------------------------------------
regions.for.table <- c("Verb", "Verb+1", "Verb+2")
exp3.means.latex <- exp3.data.gdave %>%
  filter(Region %in% regions.for.table) %>% 
  select(Condition, Ambiguity, Region, gd.avg.RT, gd.avg.RT.SE) %>%
  transmute(Cond = paste(Ambiguity, Condition, sep="/"),
            Region = Region,
            Average = round(gd.avg.RT, 0),
            SE = round(gd.avg.RT.SE, 0)) %>%
  arrange(Region)
colnames(exp3.means.latex) <- c("Condition", "Region", "Mean", "SE")

exp3.means.latex %>% select(Condition, Mean, SE) %>%
  stargazer::stargazer(summary = FALSE, rownames = FALSE, keep = c(1, 2, 3), 
                       out = path(tbls.dir, "exp3-means.tex"))

## Grand Average Plotting ------------------------------------------------------
exp3.grand.average.plot <- ggplot(exp3.data.gdave, 
                                  aes(x = Region, y = gd.avg.RT, 
                                      group = Condition, 
                                      colour = Condition))

plot.manuscript.all <- exp3.grand.average.plot +
  labs(x = "Region", y = "Raw RT (ms)", 
       title = "Experiment 3: Number Attraction, Singular Subjects") + 
  geom_line(aes(linetype = Condition)) + 
  geom_point(aes(shape = Condition), size = 3) +
  geom_errorbar(aes(ymax = se.max, ymin = se.min), width=0.1) + 
  facet_grid(Ambiguity ~ ., 
             scales = "free_y") + 
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
       file = path(figs.dir, "exp3.pdf"),
       width = spr.rt.width, height = spr.rt.height)
ggsave(plot.manuscript.all, 
       file = path(figs.dir, "exp3.eps"), 
       width = spr.rt.width, height = spr.rt.height, device = cairo_ps)
agg_png(filename = path(figs.dir, "exp3.png"),
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
exp3.reformatted.for.cis <- exp3.data.saves %>%
  mutate(Experiment = factor(rep("Exp3", times = length(SubjID))),
         SubExperiment = Ambiguity,
         SubjPhi = factor(rep("Sg", times = length(SubjID)))) %>%
  separate(Condition, into = c("Match", "Grammaticality")) %>%
  unite(., "Condition", Match, Grammaticality) %>%
  pivot_wider(names_from = Condition, values_from = meanRT)

## Effect 1: Attraction in Ungrammatical Sentences
exp3.attr.ungram <-  exp3.reformatted.for.cis %>%
  mutate(IntrusionU = Match_Ungram - NoMatch_Ungram) %>%
  group_by(Experiment, SubExperiment, SubjPhi, Region) %>%
  summarise(MeanEffect = mean(IntrusionU),
            SDEffect = sd(IntrusionU),
            N = n(),
            VarEffect = (SDEffect^2)/N,
            StudyWeightEffect = 1 / VarEffect,
            CIEffect = paste(bootES(IntrusionU)$bounds, collapse = "/")) %>%
  mutate(Exp = factor(paste(Experiment, SubExperiment, sep = "."))) %>%
  separate(CIEffect, into = c("CI.min", "CI.max"), sep = "/", convert= TRUE) %>%
  mutate(EffectType = rep("Attraction_Ungrammatical", times = length(Exp))) %>%
  ungroup()


## Effect 2: Attraction in Grammatical Sentences
exp3.attr.gram <-  exp3.reformatted.for.cis %>%
  mutate(IntrusionU = Match_Gram - NoMatch_Gram) %>%
  group_by(Experiment, SubExperiment, SubjPhi, Region) %>%
  summarise(MeanEffect = mean(IntrusionU),
            SDEffect = sd(IntrusionU),
            N = n(),
            VarEffect = (SDEffect^2)/N,
            StudyWeightEffect = 1 / VarEffect,
            CIEffect = paste(bootES(IntrusionU)$bounds, collapse = "/")) %>%
  mutate(Exp = factor(paste(Experiment, SubExperiment, sep = "."))) %>%
  separate(CIEffect, into = c("CI.min", "CI.max"), sep = "/", convert= TRUE) %>%
  mutate(EffectType = rep("Attraction_Grammatical", times = length(Exp))) %>%
  ungroup()

## Effect 3: Grammaticality Effect
exp3.grammaticality <-  exp3.reformatted.for.cis %>%
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
  separate(CIEffect, into = c("CI.min", "CI.max"), sep = "/", convert= TRUE) %>%
  mutate(EffectType = rep("Grammaticality", times = length(Exp))) %>%
  ungroup()

## Put all effects together
exp3.effects <- bind_rows(exp3.attr.ungram, exp3.attr.gram, exp3.grammaticality)

## LaTeX tables for Effects and their Bootstrapped 95% CIs ---------------------
exp3.effects %>%
  filter(Region %in% rois) %>%
  mutate_if(is.factor, as.character) %>%
  group_by(SubExperiment) %>%
  mutate(SE = sqrt(VarEffect),
         Mean = paste0(round(MeanEffect, 0), " (", round(CI.min, 0), ", ", 
                       round(CI.max, 0), ")")) %>%
  ungroup() %>%
  select(Region, SubExperiment, EffectType, Mean) %>%
  droplevels() %>%
  group_by(SubExperiment) %>%
  arrange(SubExperiment, .by_group = TRUE) %>%
  ungroup() %>%
  pivot_wider(names_from = Region, values_from = Mean) %>%
  stargazer::stargazer(type = "latex", summary = FALSE, rownames = FALSE,
                       out=path(tbls.dir, "exp3-cis.tex"))

## Save the relevant data structures for future manipulation -------------------
save(exp3.data.ok, exp3.data.saves, exp3.data.gdave, exp3.effects, 
     file = path(meta.dir, "Experiment3-DataForMetaAnalysis.RData"))

## Clean environment -----------------------------------------------------------
rm(list = ls())
