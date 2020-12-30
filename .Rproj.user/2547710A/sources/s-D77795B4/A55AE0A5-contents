################################################################################
# Analysis Script - Experiment 1                                               #
# "Attraction Effects for Verbal Gender and Number Are Similar but Not         #
# Identical: Self-Paced Reading Evidence from Modern Standard Arabic"          #
# M. A. Tucker, A. Idrissi, and D. Almeida                                     #
# Script Author: Diogo Almeida <diogo@nyu.edu,                                 #
#                Matt Tucker <matt.tucker@nyu.edu>                             #
# v. 1, current 29 January 2017                                                #
# v. 2, current 22 March 2018                                                  #
# v. 3, current 24 December 2020                                               #
# v. 4, current 29 December 2020                                               #
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
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(color = 'black'),
        legend.position="bottom",
        legend.title = element_blank())

spr.rt.width  <-  6.7
spr.rt.height <-  3.7

## Set variable paths for data, figures and tables produced by analysis --------
this_exp <- "exp1"
data.dir <- here(this_exp, "data")
figs.dir <- here(this_exp, "figures")
tbls.dir <- here(this_exp, "tables")
meta.dir <- here("meta")
dir_create(c(data.dir, figs.dir, tbls.dir, meta.dir))

## Getting data ----------------------------------------------------------------
exp1.datafile.figshare <- "https://ndownloader.figshare.com/files/25914762"
pin(exp1.datafile.figshare, name = "experiment1_data")
exp1.original <- pin_get("experiment1_data") %>% 
  readr::read_csv()

## Saving a copy of the original dataset on the data folder --------------------
exp1.original %>%
  readr::write_csv(here(data.dir, "experiment1.csv"))

## Set parameters for data analysis --------------------------------------------
wins.cutoff  <- .01   # 1% cutoff
error.cutoff <- .5    # Original Analysis = .5; Alternative = .67 (roughly 2/3)

## Regions of Interest ---------------------------------------------------------
rois <- c("Verb", "Verb+1", "Verb+2")

exp1.regions <- c("NP Subj", "Comp", "RC Verb", "Attr", "Adverb", "Verb", 
                  "Verb+1", "Verb+2")

## Exclude Fillers, Incorrect trials, NAs, and Regions after Verb+2 ------------
exp1.data.ok <- exp1.original %>%
  filter(Correct == 1 & Condition != "Filler" & Region %in% exp1.regions & 
           !is.na(RT))

## Winsorizing the data at 1% --------------------------------------------------
exp1.data.ok <- exp1.data.ok %>%
  group_by(Condition, Region) %>%
  mutate(RTw01 = lamb.winsorize(RT, cut.off = wins.cutoff, 
                                cut.off.unit = "percent")) %>%
  ungroup()

## Subject averages ------------------------------------------------------------
exp1.data.saves <- exp1.data.ok %>% 
  group_by(SubjID, Condition, Region) %>%
  summarise(meanRT = mean(RTw01, na.rm = TRUE)) %>%
  ungroup()

## Find subjects who had empty cells -------------------------------------------
## i.e., who had conditions in which no average data can be computed given our
## exclusion criteria
exp1.subjects.empty.cells <- exp1.data.saves %>%
  group_by(Region, SubjID) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  filter(count < 4) %>% 
  select(SubjID) %>%
  distinct() %>%
  unlist() %>%
  as.vector()

exp1.bad.subjects <- exp1.original %>%
  filter(Condition != "Filler" & Region %in% exp1.regions) %>%
  group_by(SubjID) %>%
  summarise(MeanCorrect = mean(Correct, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(MeanCorrect < error.cutoff) %>%
  select(SubjID)


## Excluding subjects with empty cells and who answered less than 50% correct --
exp1.data.ok <- exp1.data.ok %>%
  filter(!SubjID %in% exp1.bad.subjects) %>%
  droplevels()

exp1.data.saves <- exp1.data.saves %>%
  filter(!SubjID %in% exp1.subjects.empty.cells &
           !SubjID %in% exp1.bad.subjects) %>%
  droplevels()

## Grand Averages --------------------------------------------------------------
exp1.data.gdave <- exp1.data.saves %>%
  group_by(Condition, Region) %>%
  summarise(gd.avg.RT = mean(meanRT, na.rm = TRUE),
            gd.avg.RT.SE = lamb.sem(meanRT),
            se.min = gd.avg.RT - gd.avg.RT.SE,
            se.max = gd.avg.RT + gd.avg.RT.SE) %>%
  separate(Condition, into = c("Match", "Grammaticality"), sep = "/", 
           remove = FALSE) %>%
  ungroup() %>%
  mutate(SubjPhi = rep("Gend", time = length(Match))) %>%
  mutate(across(where(is.character), factor)) %>%
  mutate(Experiment = rep("exp1.A", times = length(SubjPhi))) %>%
  droplevels()

## Means & Variances Tables for LaTeX ------------------------------------------
regions.for.table <- c("Verb", "Verb+1", "Verb+2")
exp1.means.latex <- exp1.data.gdave %>%
  filter(Region %in% regions.for.table) %>% 
  select(Condition, Region, gd.avg.RT, gd.avg.RT.SE) %>%
  transmute(Cond = as.character(Condition),
         Region = Region,
         Average = round(gd.avg.RT, 0),
         SE = round(gd.avg.RT.SE, 0)) %>%
  arrange(Region)
colnames(exp1.means.latex) <- c("Condition", "Region", "Mean", "SE")

exp1.means.latex %>% select(Condition, Mean, SE) %>%
  stargazer::stargazer(summary=FALSE, rownames=FALSE, keep=c(1, 2, 3), 
                       out=here(tbls.dir, "exp1-means.tex"))

## Grand Average Plotting ------------------------------------------------------
exp1.grand.average.plot <- ggplot(exp1.data.gdave, 
                                  aes(x = Region, y = gd.avg.RT, 
                                      group = Condition, 
                                      colour = Condition))

plot.manuscript.all <- exp1.grand.average.plot +
  labs(x = "Region", y = "Raw RT (ms)", 
       title = "Experiment 1: Gender Attraction, Masculine Subjects") + 
  geom_line(aes(linetype = Condition)) + 
  geom_point(aes(shape = Condition), size = 3) +
  geom_errorbar(aes(ymax = se.max, ymin = se.min), width=0.1) + 
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
       file = here(figs.dir, "exp1.pdf"),
       width = spr.rt.width, height = spr.rt.height)
ggsave(plot.manuscript.all, 
       file = here(figs.dir, "exp1.eps"), 
       width = spr.rt.width, height = spr.rt.height, device = cairo_ps)
agg_png(filename = here(figs.dir, "exp1.png"),
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
exp1.reformatted.for.cis <- exp1.data.saves %>%
  mutate(Experiment = factor(rep("Exp1", times = length(SubjID))),
         SubExperiment = factor(rep("A", times = length(SubjID))),
         SubjPhi = factor(rep("Masc", times = length(SubjID)))) %>%
  separate(Condition, into = c("Match", "Grammaticality")) %>%
  unite(., "Condition", Match, Grammaticality) %>%
  pivot_wider(names_from = Condition, values_from = meanRT)

## Effect 1: Attraction in Ungrammatical Sentences
exp1.attr.ungram <-  exp1.reformatted.for.cis %>%
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
exp1.attr.gram <-  exp1.reformatted.for.cis %>%
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
exp1.grammaticality <-  exp1.reformatted.for.cis %>%
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
exp1.effects <- bind_rows(exp1.attr.ungram, exp1.attr.gram, exp1.grammaticality)

## LaTeX tables for Effects and their Bootstrapped 95% CIs ---------------------
exp1.effects %>%
  filter(Region %in% rois & Exp %in% c("Exp1.A")) %>%
  mutate(SE = sqrt(VarEffect),
         Mean = paste0(round(MeanEffect, 0), " (", round(CI.min, 0), ", ", 
                       round(CI.max, 0), ")")) %>%
  select(Region, EffectType, Mean) %>%
  droplevels() %>%
  pivot_wider(names_from = Region, values_from = Mean) %>%
  stargazer::stargazer(type = "latex", summary = FALSE, rownames = FALSE,
                       out=here(tbls.dir, "exp1-cis.tex"))


## Save the relevant data structures for future manipulation -------------------
save(exp1.data.ok, exp1.data.saves, exp1.data.gdave, exp1.effects,
     file = here(meta.dir, "Experiment1-DataForMetaAnalysis.RData"))

## Clean environment -----------------------------------------------------------
rm(list = ls())
