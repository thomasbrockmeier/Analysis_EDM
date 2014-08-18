---
output: html_document
---
## Data analysis

``` {r include=FALSE}
setwd("/home/david/Dropbox/Escuela/Master Brain and Cognitive Sciences/Internship/Research Project 2/Analysis")
source("clean.data.R")
source("descriptives.R")
source("reliability.check.R")
source("fleiss.EDM.rev.R")
source("select.EDM.R")
library("psych")

timbre <- read.csv("20140714/timbre20140714.csv", header=T)
rhythm <- read.csv("20140714/rhythm20140714.csv", header = T)
timbreWPC <- read.csv("20140714/timbreWPC20140714.csv", header=T)
rhythmWPC <- read.csv("20140714/rhythmWPC20140714.csv", header=T)
general <- read.csv("20140714/general20140714.csv", header = T)

kappa.agreement <- function(kappa){
        if (kappa <= 0) {
                agreement <- "poor"
        } else if (kappa >= 0.01 & kappa <= 0.20) {
                agreement <- "slight"
        } else if (kappa >= 0.21 & kappa <= 0.40) {
                agreement <- "fair"
        } else if (kappa >= 0.41 & kappa <= 0.60) {
                agreement <- "moderate"
        } else if (kappa >= 0.61 & kappa <= 0.80) {
                agreement <- "substantial"
        } else if (kappa >= 0.81 & kappa <= 1) {
                agreement <- "almost perfect"
        }
        return(agreement)
}
```

After receiving all the results from the experiment on timbre, rhythm, and general similarities, we aimed to find a level of agreement between the raters. For this experiment, the statistical analysis required was Fleiss' Kappa due to its non-parametric nature. Landis and Koch (1977) gave an interpretation for the Fleiss' Kappa values, presented in the following table:

Kappa | Agreement
------------- | -------------
 < 0 | Poor agreement
0.01 - 0.20  | Slight agreement
0.21 - 0.40 | Fair agreement
0.41 - 0.60 | Moderate agreement
0.61 - 0.80 | Substantial agreement
0.81 - 1.00 | Almost perfect agreement

### Experiment 1

The first agreement analysis made was across the participants. Because of the experimental design it was not possible to use the entire data set of the timbre and rhythm experiments in a single run of the analysis. Instead, we randomly selected **_n_** ratings of each pair for one thousand iterations and calculated the mean. Additionally, we took into account the ratings in a four-point scale, and also scaled it down to a binary scale. The results are as follows:

For four-point scale data (original):
``` {r echo=FALSE, warning=FALSE, comment=NA, message=FALSE}
fleiss.timbre.4 <- fleiss.EDM(data = timbre, polarize = FALSE, iterations = 1000)
```

For two-point scale data (re-scaled):
``` {r echo=FALSE, warning=FALSE, comment=NA}
fleiss.timbre.2 <- fleiss.EDM(data = timbre, polarize = TRUE, iterations = 1000)
```

These tables show the descriptive statistics of Fleiss' Kappa's analysis. The most important points are:

* Kappa value's __mean__ and __standard deviation__. These show the level of agreement, and the reliability of the randomized iterations of the analysis.
* p-value's __median__. We take the median since the range of the p-values is usually very wide and heavily skewed. The median gives us a better overview of the most representative value without taking outliers into account.

Knowing this, we can see how the mean Kappa value for the two-point scale (mean Kappa = `r fleiss.timbre.2["Kappa", "mean"]`, median p-value = `r fleiss.timbre.2["p.value","median"]`) is slightly larger than for the four-point scale (mean Kappa = `r fleiss.timbre.4["Kappa", "mean"]`, median p-value = `r fleiss.timbre.4["p.value","median"]`), enough to describe the effect as __`r kappa.agreement(fleiss.timbre.2["Kappa", "mean"])` agreement__ according to Landis and Koch (1977).

``` {r, include=FALSE}
threshold <- 25
# For four-point scale data (original):
clean.timbre.4 <- clean.data.pairwise(raw.data = reliability.check(timbre), polarize = FALSE)
desc.timbre.by.sd.4 <- describe(clean.data.subjectwise(raw.data = timbre, polarize = FALSE))
desc.timbre.by.low.sd.4 <- desc.timbre.by.sd.4[order(desc.timbre.by.sd.4$sd)[1:threshold],]
# desc.timbre.by.high.sd.4 <- desc.timbre.by.sd.4[order(desc.timbre.by.sd.4$sd, decreasing = TRUE)[1:threshold],]
fleiss.timbre.low.sd.4 <- fleiss.EDM(t(clean.timbre.4[rownames(desc.timbre.by.low.sd.4),]), is.raw = F)

# For two-point scale data (re-scaled):
clean.timbre.2 <- clean.data.pairwise(raw.data = reliability.check(timbre), polarize = TRUE)
desc.timbre.by.sd.2 <- describe(clean.data.subjectwise(raw.data = timbre, polarize = TRUE))
desc.timbre.by.low.sd.2 <- desc.timbre.by.sd.2[order(desc.timbre.by.sd.2$sd)[1:threshold],]
# desc.timbre.by.high.sd.2 <- desc.timbre.by.sd.2[order(desc.timbre.by.sd.2$sd, decreasing = TRUE)[1:threshold],]
fleiss.timbre.low.sd.2 <- fleiss.EDM(t(clean.timbre.2[rownames(desc.timbre.by.low.sd.2),]), is.raw = F)
```

In the previous analysis we used the full rating data set. This contains some pairs with very concordant ratings and other pairs with very disperse ratings. This is to be expected, since participants have different listening strategies and musical preferences which mould their affinity to certain musical traits. Because of this, we selected the `r threshold` pairs with the lowest standard deviations (four-point scale minimum sd = `r min(desc.timbre.by.low.sd.4$sd)`, maximum sd = `r max(desc.timbre.by.low.sd.4$sd)`; two-point scale minimum sd = `r min(desc.timbre.by.low.sd.2$sd)`, maximum sd = `r max(desc.timbre.by.low.sd.2$sd)`) for a second Fleiss' Kappa test. The results for this reduced data set show higher levels of agreement than those of the full data set, reaching __`r kappa.agreement(fleiss.timbre.low.sd.4["Kappa", "mean"])` agreement__ on the four-point scale (mean Kappa = `r fleiss.timbre.low.sd.4["Kappa", "mean"]`, median p-value = `r fleiss.timbre.low.sd.4["p.value","median"]`), and __`r kappa.agreement(fleiss.timbre.low.sd.2["Kappa", "mean"])` agreement__ on the two-point scale (mean Kappa = `r fleiss.timbre.low.sd.2["Kappa", "mean"]`, median p-value = `r fleiss.timbre.low.sd.2["p.value","median"]`). __(Note for edit: Evidently, the agreement level increases as we decrease the number of pairs with low SD. We need to find a good number that doesn't look like we're cherrypicking our data.)__

For four-point scale data (original):
``` {r echo=FALSE, warning=FALSE, comment=NA}
print(fleiss.timbre.low.sd.4)
```

For two-point scale data (re-scaled):
``` {r echo=FALSE, warning=FALSE, comment=NA}
print(fleiss.timbre.low.sd.2)
```

### Experiment 2

As part of Experiment 2, one participant rated the same set of pairs of segments six times. For this analysis we did not need to get random samples from the ratings since it does not have missing values. A regular Fleiss' Kappa was applied:

For four-point scale data (original):
``` {r echo=FALSE, warning=FALSE, comment=NA}
print(fleiss.timbre.4.WPC <- kappam.fleiss(clean.data.pairwise(raw.data = timbreWPC, tracknames = FALSE, polarize = FALSE)))
```

For two-point scale data (re-scaled):
``` {r echo=FALSE, warning=FALSE, comment=NA}
print(fleiss.timbre.2.WPC <- kappam.fleiss(clean.data.pairwise(raw.data = timbreWPC, tracknames = FALSE, polarize = TRUE)))
```

With this information, we can conclude that this participant has __substantial agreement__ on his ratings based on a two-point scale (Kappa = `r fleiss.timbre.2.WPC$value`, p-value = `r fleiss.timbre.2.WPC$p.value`), and __moderate agreement__ on a four-point scale (Kappa = `r fleiss.timbre.4.WPC$value`, p-value = `r fleiss.timbre.4.WPC$p.value`).

### Experiment 3

As a part of the study, we also tested participants for their agreement on general music similarity. The Fleiss' Kappa analysis shows the following:

For four-point scale data (original):
``` {r echo=FALSE, warning=FALSE, comment=NA}
print(fleiss.general.4 <- kappam.fleiss(clean.data.pairwise(raw.data = general, tracknames = FALSE, polarize = FALSE)))
```

For two-point scale data (re-scaled):
``` {r echo=FALSE, warning=FALSE, comment=NA}
print(fleiss.general.2 <- kappam.fleiss(clean.data.pairwise(raw.data = general, tracknames = FALSE, polarize = TRUE)))
```

We can conclude that even with the re-scaled ratings, the general similarity agreement remains as __slight__ (two-point scale Kappa = `r fleiss.general.2$value`, p-value = `r fleiss.general.2$p.value`; four-point scale Kappa = `r fleiss.general.4$value`, p-value = `r fleiss.general.4$p.value`).

Since the general similarity stimuli were formed from a reduced pool of segments, it is important to analyze the same isolated pairs when looking for timbre similarity. In the following table we can see the Fleiss' Kappa analysis of timbre similarity of pairs 7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, and 190:

For four-point scale data (original):
``` {r echo=FALSE, warning=FALSE, comment=NA}
fleiss.timbre.sel.4 <- fleiss.EDM(data = timbre, polarize = FALSE, iterations = 1000, selection = "short")
```

For two-point scale data (re-scaled):
``` {r echo=FALSE, warning=FALSE, comment=NA}
fleiss.timbre.sel.2 <- fleiss.EDM(data = timbre, polarize = TRUE, iterations = 1000, selection = "short")
```

By taking this isolated group, we can find an increase in Kappa values (two-point scale mean Kappa = `r fleiss.timbre.sel.2["Kappa", "mean"]`, median p-value = `r fleiss.timbre.sel.2["p.value", "median"]`; four-point scale mean Kappa = `r fleiss.timbre.sel.4["Kappa", "mean"]`, median p-value = `r fleiss.timbre.sel.4["p.value", "median"]`), they are within the __`r kappa.agreement(fleiss.timbre.sel.2["Kappa", "mean"])` agreement__ level for the two-point scale, and __`r kappa.agreement(fleiss.timbre.sel.4["Kappa", "mean"])` agreement__ level for the four-point scale. It is worth mentioning that the median p-values for both two- and four-point scale ratings are slightly above and at the accepted significance value of 0.05, which means that these results could be attributed to chance.


#### Wilcoxon rank sum test

``` {r, include=FALSE}
## Timbre vs General---------------------------------------------------------------
# For four-point scale data (original):
clean.timbre.4 <- clean.data.pairwise(raw.data = reliability.check(timbre), polarize = FALSE)
clean.timbre.4 <- clean.timbre.4[c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190),]
clean.general.4 <- clean.data.pairwise(raw.data = general, polarize = FALSE)
desc.timbre.4 <- describe(clean.data.subjectwise(reliability.check(timbre), polarize = FALSE))[c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190),]
desc.general.4 <- describe(clean.data.subjectwise(general, polarize = FALSE))
table.timbre.general.4 <- matrix(nrow = 0, ncol = 4)
colnames(table.timbre.general.4) <- c("W", "p.value", "Timbre mean rating", "General mean rating")
for (i in seq(1,nrow(clean.timbre.4))) {
        wtmp <- wilcox.test(clean.timbre.4[i,], clean.general.4[i,])
        table.timbre.general.4 <- rbind(table.timbre.general.4, c(wtmp$statistic, wtmp$p.value, desc.timbre.4[i,"mean"], desc.general.4[i,"mean"]))
}
rownames(table.timbre.general.4) <- as.character(c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190))
diff.timbre.general.4 <- table.timbre.general.4[table.timbre.general.4[,2] < 0.05,]

# For two-point scale data (re-scaled):
clean.timbre.2 <- clean.data.pairwise(raw.data = reliability.check(timbre), polarize = TRUE)
clean.timbre.2 <- clean.timbre.2[c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190),]
clean.general.2 <- clean.data.pairwise(raw.data = general, polarize = TRUE)
desc.timbre.2 <- describe(clean.data.subjectwise(reliability.check(timbre), polarize = TRUE))[c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190),]
desc.general.2 <- describe(clean.data.subjectwise(general, polarize = TRUE))
table.timbre.general.2 <- matrix(nrow = 0, ncol = 4)
colnames(table.timbre.general.2) <- c("W", "p.value", "Timbre mean rating", "General mean rating")
for (i in seq(1,nrow(clean.timbre.2))) {
        wtmp <- wilcox.test(clean.timbre.2[i,], clean.general.2[i,])
        table.timbre.general.2 <- rbind(table.timbre.general.2, c(wtmp$statistic, wtmp$p.value, desc.timbre.2[i,"mean"], desc.general.2[i,"mean"]))
}
rownames(table.timbre.general.2) <- as.character(c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190))
diff.timbre.general.2 <- table.timbre.general.2[table.timbre.general.2[,2] < 0.05,]

## Rhythm vs. General----------------------------------------------------------------------
# For four-point scale data (original):
clean.rhythm.4 <- clean.data.pairwise(raw.data = reliability.check(rhythm), polarize = FALSE)
clean.rhythm.4 <- clean.rhythm.4[c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190),]
desc.rhythm.4 <- describe(clean.data.subjectwise(reliability.check(rhythm), polarize = FALSE))[c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190),]
desc.general.4 <- describe(clean.data.subjectwise(general, polarize = FALSE))
table.rhythm.general.4 <- matrix(nrow = 0, ncol = 4)
colnames(table.rhythm.general.4) <- c("W", "p.value", "Rhythm mean rating", "General mean rating")
for (i in seq(1,nrow(clean.rhythm.4))) {
        wtmp <- wilcox.test(clean.rhythm.4[i,], clean.general.4[i,])
        table.rhythm.general.4 <- rbind(table.rhythm.general.4, c(wtmp$statistic, wtmp$p.value, desc.rhythm.4[i,"mean"], desc.general.4[i,"mean"]))
}
rownames(table.rhythm.general.4) <- as.character(c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190))
diff.rhythm.general.4 <- table.rhythm.general.4[table.rhythm.general.4[,2] < 0.05,]

# For two-point scale data (re-scaled):
clean.rhythm.2 <- clean.data.pairwise(raw.data = reliability.check(rhythm), polarize = TRUE)
clean.rhythm.2 <- clean.rhythm.2[c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190),]
desc.rhythm.2 <- describe(clean.data.subjectwise(reliability.check(rhythm), polarize = TRUE))[c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190),]
desc.general.2 <- describe(clean.data.subjectwise(general, polarize = TRUE))
table.rhythm.general.2 <- matrix(nrow = 0, ncol = 4)
colnames(table.rhythm.general.2) <- c("W", "p.value", "Rhythm mean rating", "General mean rating")
for (i in seq(1,nrow(clean.rhythm.2))) {
        wtmp <- wilcox.test(clean.rhythm.2[i,], clean.general.2[i,])
        table.rhythm.general.2 <- rbind(table.rhythm.general.2, c(wtmp$statistic, wtmp$p.value, desc.rhythm.2[i,"mean"], desc.general.2[i,"mean"]))
}
rownames(table.rhythm.general.2) <- as.character(c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190))
diff.rhythm.general.2 <- table.rhythm.general.2[table.rhythm.general.2[,2] < 0.05,]

```

We also conducted a Wilcoxon rank sum test with __timbre similarity__ and __general similarity__, which determines the probability of the two data sets belonging to the same group. The Wilcoxon rank sum test shows that `r as.character(nrow(diff.timbre.general.4))` pairs in the four-point scale and `r as.character(nrow(diff.timbre.general.2))` pairs in the two-point scale have a probability below 0.05 of belonging to the same group.

For four-point scale data (original):
``` {r echo=FALSE, warning=FALSE, comment=NA}
diff.timbre.general.4
```

For two-point scale data (re-scaled):
``` {r echo=FALSE, warning=FALSE, comment=NA}
diff.timbre.general.2
```

As a comparison, we also calculated the _W values_ of both __rhythm similarity__ and __general similarity__ to get a better overview of the possible attributes that participants might be listening for. In this test we can observe `r as.character(nrow(diff.rhythm.general.4))` pairs in the four-point scale and `r as.character(nrow(diff.rhythm.general.2))` pairs in the two-point scale have a lower-than-0.05 probability of being part of the same group.

For four-point scale data (original):
``` {r echo=FALSE, warning=FALSE, comment=NA}
diff.rhythm.general.4
```

For two-point scale data (re-scaled):
``` {r echo=FALSE, warning=FALSE, comment=NA}
diff.rhythm.general.2
```

``` {r, include=FALSE}
## Timbre vs Rhythm
# For four-point scale data (original):
clean.timbre.4 <- clean.data.pairwise(raw.data = reliability.check(timbre), polarize = FALSE)
desc.timbre.4 <- describe(t(clean.timbre.4))
clean.rhythm.4 <- clean.data.pairwise(raw.data = reliability.check(rhythm), polarize = FALSE)
desc.rhythm.4 <- describe(t(clean.rhythm.4))
table.timbre.rhythm.4 <- matrix(nrow = 0, ncol = 4)
colnames(table.timbre.rhythm.4) <- c("W", "p.value", "Timbre mean rating", "Rhythm mean rating")
for (i in seq(1,nrow(clean.timbre.4))) {
        wtmp <- wilcox.test(clean.timbre.4[i,], clean.rhythm.4[i,])
        if (is.na(wtmp$p.value)) {
                wtmp$p.value <- "NA"
        }
       table.timbre.rhythm.4 <- rbind(table.timbre.rhythm.4, c(wtmp$statistic, wtmp$p.value, desc.timbre.4[i, "mean"], desc.rhythm.4[i, "mean"]))
}
rownames(table.timbre.rhythm.4) <- as.character(seq(1, nrow(table.timbre.rhythm.4)))
diff.timbre.rhythm.4 <- table.timbre.rhythm.4[table.timbre.rhythm.4[,2] < 0.05,]

# For two-point scale data (re-scaled):
clean.timbre.2 <- clean.data.pairwise(raw.data = reliability.check(timbre), polarize = TRUE)
desc.timbre.2 <- describe(t(clean.timbre.2))
clean.rhythm.2 <- clean.data.pairwise(raw.data = reliability.check(rhythm), polarize = TRUE)
desc.rhythm.2 <- describe(t(clean.rhythm.2))
table.timbre.rhythm.2 <- matrix(nrow = 0, ncol = 4)
colnames(table.timbre.rhythm.2) <- c("W", "p.value", "Timbre mean rating", "Rhythm mean rating")
for (i in seq(1,nrow(clean.timbre.2))) {
        wtmp <- wilcox.test(clean.timbre.2[i,], clean.rhythm.2[i,])
        if (is.na(wtmp$p.value)) {
                wtmp$p.value <- "NA"
        }
       table.timbre.rhythm.2 <- rbind(table.timbre.rhythm.2, c(wtmp$statistic, wtmp$p.value, desc.timbre.2[i, "mean"], desc.rhythm.2[i, "mean"]))
}
rownames(table.timbre.rhythm.2) <- as.character(seq(1, nrow(table.timbre.rhythm.2)))
diff.timbre.rhythm.2 <- table.timbre.rhythm.2[table.timbre.rhythm.2[,2] < 0.05,]
```

Another interesting comparison is the difference in similarity ratings between __timbre__ and __rhythm__. Since both projects had participants assess the same segment pairs in the same way, the two data sets are comparable in number of pairs (_n_ = 190). We found that in the original four-point scale ratings, there are `r as.character(nrow(diff.timbre.rhythm.4))` pairs with low probability of belonging to the same group. In the re-scaled two-point variant, `r as.character(nrow(diff.timbre.rhythm.2))` pairs have a low probability of belonging to the same group. There are `r length(intersect(rownames(diff.timbre.rhythm.4), rownames(diff.timbre.rhythm.2)))` pairs that are present in in both four-and two-point scales (`r intersect(rownames(diff.timbre.rhythm.4), rownames(diff.timbre.rhythm.2))`). This could mean that the participants are indeed listening for different attributes in these `r length(intersect(rownames(diff.timbre.rhythm.4), rownames(diff.timbre.rhythm.2)))` pairs of tracks to determine the similarity rating of the specific trait (timbre or rhythm).  Here are the pairs that fit in this criterion for both four- and two-point scales:

For four-point scale data (original):
``` {r echo=FALSE, warning=FALSE, comment=NA}
print(diff.timbre.rhythm.4)
```

For two-point scale data (re-scaled):
``` {r echo=FALSE, warning=FALSE, comment=NA}
print(diff.timbre.rhythm.2)

```

