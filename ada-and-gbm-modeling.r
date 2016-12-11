# load libraries
library(rattle) # For GUI convenience during exploration and transformation
library(magrittr) # For the %>% and %<>% operators.
building <- TRUE
scoring  <- ! building

############################################################
# data prep
############################################################

# A pre-defined value is used to reset the random seed so that results are repeatable.
crv$seed <- 42

# Load the data.
crs$dataset <- read.csv("file:///F:/1. RS Home Files/SNHU/DAT-690 Capstone Data Analytics/Datasets/NEW/cell.data.calibration.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

# Build the training/test datasets.
set.seed(crv$seed)
crs$nobs <- nrow(crs$dataset) # 563 observations
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.75*crs$nobs) # 422 observations
crs$validate <- NULL
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 141 observations

# Identify the variables and their class
crs$input <- c("REVENUE", "MOU", "RECCHRGE", "DIRECTAS",
               "OVERAGE", "ROAM", "CHANGEM", "CHANGER",
               "DROPVCE", "BLCKVCE", "UNANSVCE", "CUSTCARE",
               "THREEWAY", "MOUREC", "OUTCALLS", "INCALLS",
               "PEAKVCE", "OPEAKVCE", "DROPBLK", "CALLFWDV",
               "CALLWAIT", "CHURN", "MONTHS", "UNIQSUBS",
               "ACTVSUBS", "CSA", "CSA_CITYCODE", "CSA_AREACODE",
               "CSA_CITY", "CSA_STATE", "PHONES", "MODELS",
               "EQPDAYS", "AGE1", "AGE2", "CHILDREN",
               "CREDITA", "CREDITAA", "CREDITB", "CREDITC",
               "CREDITDE", "CREDITGY", "CREDITZ", "CREDIT_RATING",
               "PRIZMRUR", "PRIZMUB", "PRIZMTWN", "PRZM_NUM",
               "REFURB", "WEBCAP", "TRUCK", "RV",
               "OCCPROF", "OCCCLER", "OCCCRFT", "OCCSTUD",
               "OCCRET", "OCCSELF", "OCC", "OCC_LABEL",
               "OWNRENT", "MARRYUN", "MARRYYES", "MARRYNO",
               "MARRY", "MARRY_LABEL", "MAILORD", "MAILRES",
               "MAILFLAG", "TRAVEL", "PCOWN", "CREDITCD",
               "RETCALLS", "RETACCPT", "NEWCELLY", "NEWCELLN",
               "REFER", "INCMISS", "INCOME", "MCYCLE",
               "CREDITAD", "SETPRCM", "SETPRC", "RETCALL")

crs$numeric <- c("REVENUE", "MOU", "RECCHRGE", "DIRECTAS",
                 "OVERAGE", "ROAM", "CHANGEM", "CHANGER",
                 "DROPVCE", "BLCKVCE", "UNANSVCE", "CUSTCARE",
                 "THREEWAY", "MOUREC", "OUTCALLS", "INCALLS",
                 "PEAKVCE", "OPEAKVCE", "DROPBLK", "CALLFWDV",
                 "CALLWAIT", "CHURN", "MONTHS", "UNIQSUBS",
                 "ACTVSUBS", "CSA_AREACODE", "PHONES", "MODELS",
                 "EQPDAYS", "AGE1", "AGE2", "CHILDREN",
                 "CREDITA", "CREDITAA", "CREDITB", "CREDITC",
                 "CREDITDE", "CREDITGY", "CREDITZ", "CREDIT_RATING",
                 "PRIZMRUR", "PRIZMUB", "PRIZMTWN", "PRZM_NUM",
                 "REFURB", "WEBCAP", "TRUCK", "RV",
                 "OCCPROF", "OCCCLER", "OCCCRFT", "OCCSTUD",
                 "OCCRET", "OCCSELF", "OCC", "OWNRENT",
                 "MARRYUN", "MARRYYES", "MARRYNO", "MARRY",
                 "MAILORD", "MAILRES", "MAILFLAG", "TRAVEL",
                 "PCOWN", "CREDITCD", "RETCALLS", "RETACCPT",
                 "NEWCELLY", "NEWCELLN", "REFER", "INCMISS",
                 "INCOME", "MCYCLE", "CREDITAD", "SETPRCM",
                 "SETPRC", "RETCALL")

crs$categoric <- c("CSA", "CSA_CITYCODE", "CSA_CITY", "CSA_STATE",
                   "OCC_LABEL", "MARRY_LABEL")

crs$target  <- "CHURNDEP"
crs$risk    <- NULL
crs$ident   <- "CUSTOMER"
crs$ignore  <- c("OCCHMKR", "CALIBRAT")
crs$weights <- NULL

# Edit the dataset. Change binary variable to factor/character by right clicking on the column header
# Note that edits will overwrite the current dataset.
crs$dataset <- RGtk2Extras::dfedit(crs$dataset,
                                   dataset.name="Rattle Dataset",
                                   size=c(800, 400))


#Observe the structure of the dataset to confirm transformation
str(crs$dataset)


############################################################
# inital/basic data exploration
############################################################

# The 'Hmisc' package provides the 'contents' and 'describe' functions.
library(Hmisc, quietly=TRUE)

# Obtain a summary and description of the dataset for basic exploration.
contents(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])
describe(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])

#============================================================
# Distribution of Values
#============================================================

# The 'basicStats' package provides the 'fBasics' function.
library(fBasics, quietly=TRUE)

# Generate a description of the numeric data.
lapply(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)][,c(1:25, 28, 31:59, 61:65, 67:85)], basicStats)

# Create Cumulative Distribution Plots. This process can be repeated for all applicable variables.
# Generate just the data for an Ecdf plot of the variable 'REVENUE'.
ds <- rbind(data.frame(dat=crs$dataset[crs$sample,][,"REVENUE"], grp="All"))

# The 'Hmisc' package provides the 'Ecdf' function.
library(Hmisc, quietly=TRUE)

# Plot the data.
Ecdf(ds[ds$grp=="All",1], col="#E495A5", xlab="REVENUE", lwd=2, ylab=expression(Proportion <= x), subtitles=FALSE)

# Add a title to the plot.
title(main="Distribution of REVENUE (sample)",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Create Histogram plots for variables. This process can be repeated for all applicable variables.
# Use ggplot2 to generate histogram plot for REVENUE
library(dplyr) # Provides select().
library(ggplot2) # Provides ggplot(), aes(), geom_density(), xlab(), ggtitle(), labs().

# Generate the plot.
p01 <- crs %>%
  with(dataset[sample,]) %>%
  select(REVENUE) %>%
  ggplot(aes(x=REVENUE)) +
  geom_density(lty=3) +
  xlab("REVENUE\n\nRattle 2016-Dec-09 21:18:47 R.Sofranko") +
  ggtitle("Distribution of REVENUE (sample)") +
  labs(y="Density")

# Use ggplot2 to generate histogram plot for MOU
# Generate the plot.
p02 <- crs %>%
  with(dataset[sample,]) %>%
  select(MOU) %>%
  ggplot(aes(x=MOU)) +
  geom_density(lty=3) +
  xlab("MOU\n\nRattle 2016-Dec-09 21:18:47 R.Sofranko") +
  ggtitle("Distribution of MOU (sample)") +
  labs(y="Density")

# Display the plots.
library(gridExtra) # Provides grid.arrange().
grid.arrange(p01, p02)

############################################################
# data transformation
############################################################

# Check data type returned from str() or confirm with is.factor
is.factor(crs$CHURNDEP)

# Change applicable variables to factors
crs$CHILDREN <- as.factor(crs$CHILDREN)
crs$CREDITA <- as.factor(crs$CREDITA)
crs$CREDITAA <- as.factor(crs$CREDITAA)
crs$CREDITB <- as.factor(crs$CREDITB)
crs$CREDITC <- as.factor(crs$CREDITC)
crs$CREDITDE <- as.factor(crs$CREDITDE)
crs$CREDITGY <- as.factor(crs$CREDITGY)
crs$CREDITZ <- as.factor(crs$CREDITZ)
crs$PRIZMRUR <- as.factor(crs$PRIZMRUR)
crs$PRIZMUB <- as.factor(crs$PRIZMUB)
crs$PRIZMTWN <- as.factor(crs$PRIZMTWN)
crs$REFURB <- as.factor(crs$REFURB)
crs$WEBCAP <- as.factor(crs$WEBCAP)
crs$TRUCK <- as.factor(crs$TRUCK)
crs$RV <- as.factor(crs$RV)
crs$OCCPROF <- as.factor(crs$OCCPROF)
crs$OCCCLER <- as.factor(crs$OCCCLER)
crs$OCCCRFT <- as.factor(crs$OCCCRFT)
crs$OCCSTUD <- as.factor(crs$OCCSTUD)
crs$OCCRET <- as.factor(crs$OCCRET)
crs$OCCSELF <- as.factor(crs$OCCSELF)
crs$OWNRENT <- as.factor(crs$OWNRENT)
crs$MARRYUN <- as.factor(crs$MARRYUN)
crs$MARRYYES <- as.factor(crs$MARRYYES)
crs$MARRYNO <- as.factor(crs$MARRYNO)
crs$MAILORD <- as.factor(crs$MAILORD)
crs$MAILRES <- as.factor(crs$MAILRES)
crs$MAILFLAG <- as.factor(crs$MAILFLAG)
crs$TRAVEL <- as.factor(crs$TRAVEL)
crs$PCOWN <- as.factor(crs$PCOWN)
crs$CREDITCD <- as.factor(crs$CREDITCD)
crs$RETCALLS <- as.factor(crs$RETCALLS)
crs$RETACCPT <- as.factor(crs$RETACCPT)
crs$NEWCELLY <- as.factor(crs$NEWCELLY)
crs$NEWCELLN <- as.factor(crs$NEWCELLN)
crs$INCMISS <- as.factor(crs$INCMISS)
crs$MCYCLE <- as.factor(crs$MCYCLE)
crs$SETPRCM <- as.factor(crs$SETPRCM)
crs$RETCALL <- as.factor(crs$RETCALL)
crs$CHURNDEP <- as.factor(crs$CHURNDEP)

# Confirm successful transformation to factor
is.factor(crs$CHILDREN)
is.factor(crs$CREDITA)
is.factor(crs$CREDITAA)
is.factor(crs$CREDITB)
is.factor(crs$CREDITC)
is.factor(crs$CREDITDE)
is.factor(crs$CREDITGY)
is.factor(crs$CREDITZ)
is.factor(crs$PRIZMRUR)
is.factor(crs$PRIZMUB)
is.factor(crs$PRIZMTWN)
is.factor(crs$REFURB)
is.factor(crs$WEBCAP)
is.factor(crs$TRUCK)
is.factor(crs$RV)
is.factor(crs$OCCPROF)
is.factor(crs$OCCCLER)
is.factor(crs$OCCCRFT)
is.factor(crs$OCCSTUD)
is.factor(crs$OCCRET)
is.factor(crs$OCCSELF)
is.factor(crs$OWNRENT)
is.factor(crs$MARRYUN)
is.factor(crs$MARRYYES)
is.factor(crs$MARRYNO)
is.factor(crs$MAILORD)
is.factor(crs$MAILRES)
is.factor(crs$MAILFLAG)
is.factor(crs$TRAVEL)
is.factor(crs$PCOWN)
is.factor(crs$CREDITCD)
is.factor(crs$RETCALLS)
is.factor(crs$RETACCPT)
is.factor(crs$NEWCELLY)
is.factor(crs$NEWCELLN)
is.factor(crs$INCMISS)
is.factor(crs$MCYCLE)
is.factor(crs$SETPRCM)
is.factor(crs$RETCALL)
is.factor(crs$CHURNDEP)

#============================================================
# Rescale the numeric data
#============================================================
# The 'reshape' package provides the 'rescaler' function.
library(reshape, quietly=TRUE)

# Rescale REVENUE.
crs$dataset[["R01_REVENUE"]] <- crs$dataset[["REVENUE"]]

# Rescale to [0,1].
if (building)
{
  crs$dataset[["R01_REVENUE"]] <-  rescaler(crs$dataset[["REVENUE"]], "range")
}
# When scoring transform using the training data parameters.
if (scoring)
{
  crs$dataset[["R01_REVENUE"]] <- (crs$dataset[["REVENUE"]] - 4.840000)/abs(376.390000 - 4.840000)
}

# Rescale MOU.
crs$dataset[["R01_MOU"]] <- crs$dataset[["MOU"]]
if (building)
{
  crs$dataset[["R01_MOU"]] <-  rescaler(crs$dataset[["MOU"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_MOU"]] <- (crs$dataset[["MOU"]] - 0.000000)/abs(2992.250000 - 0.000000)
}

# Rescale RECCHRGE.
crs$dataset[["R01_RECCHRGE"]] <- crs$dataset[["RECCHRGE"]]
if (building)
{
  crs$dataset[["R01_RECCHRGE"]] <-  rescaler(crs$dataset[["RECCHRGE"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_RECCHRGE"]] <- (crs$dataset[["RECCHRGE"]] - 0.000000)/abs(159.990000 - 0.000000)
}

# Rescale DIRECTAS.
crs$dataset[["R01_DIRECTAS"]] <- crs$dataset[["DIRECTAS"]]
if (building)
{
  crs$dataset[["R01_DIRECTAS"]] <-  rescaler(crs$dataset[["DIRECTAS"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_DIRECTAS"]] <- (crs$dataset[["DIRECTAS"]] - 0.000000)/abs(15.590000 - 0.000000)
}

# Rescale OVERAGE.
crs$dataset[["R01_OVERAGE"]] <- crs$dataset[["OVERAGE"]]
if (building)
{
  crs$dataset[["R01_OVERAGE"]] <-  rescaler(crs$dataset[["OVERAGE"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_OVERAGE"]] <- (crs$dataset[["OVERAGE"]] - 0.000000)/abs(760.250000 - 0.000000)
}

# Rescale ROAM.
crs$dataset[["R01_ROAM"]] <- crs$dataset[["ROAM"]]
if (building)
{
  crs$dataset[["R01_ROAM"]] <-  rescaler(crs$dataset[["ROAM"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_ROAM"]] <- (crs$dataset[["ROAM"]] - 0.000000)/abs(92.400000 - 0.000000)
}

# Rescale CHANGEM.
crs$dataset[["R01_CHANGEM"]] <- crs$dataset[["CHANGEM"]]
if (building)
{
  crs$dataset[["R01_CHANGEM"]] <-  rescaler(crs$dataset[["CHANGEM"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_CHANGEM"]] <- (crs$dataset[["CHANGEM"]] - -1345.500000)/abs(1244.750000 - -1345.500000)
}

# Rescale CHANGER.
crs$dataset[["R01_CHANGER"]] <- crs$dataset[["CHANGER"]]
if (building)
{
  crs$dataset[["R01_CHANGER"]] <-  rescaler(crs$dataset[["CHANGER"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_CHANGER"]] <- (crs$dataset[["CHANGER"]] - -224.980000)/abs(895.570000 - -224.980000)
}

# Rescale DROPVCE.
crs$dataset[["R01_DROPVCE"]] <- crs$dataset[["DROPVCE"]]
if (building)
{
  crs$dataset[["R01_DROPVCE"]] <-  rescaler(crs$dataset[["DROPVCE"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_DROPVCE"]] <- (crs$dataset[["DROPVCE"]] - 0.000000)/abs(82.000000 - 0.000000)
}

# Rescale BLCKVCE.
crs$dataset[["R01_BLCKVCE"]] <- crs$dataset[["BLCKVCE"]]
if (building)
{
  crs$dataset[["R01_BLCKVCE"]] <-  rescaler(crs$dataset[["BLCKVCE"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_BLCKVCE"]] <- (crs$dataset[["BLCKVCE"]] - 0.000000)/abs(73.670000 - 0.000000)
}

# Rescale UNANSVCE.
crs$dataset[["R01_UNANSVCE"]] <- crs$dataset[["UNANSVCE"]]
if (building)
{
  crs$dataset[["R01_UNANSVCE"]] <-  rescaler(crs$dataset[["UNANSVCE"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_UNANSVCE"]] <- (crs$dataset[["UNANSVCE"]] - 0.000000)/abs(255.000000 - 0.000000)
}

# Rescale CUSTCARE.
crs$dataset[["R01_CUSTCARE"]] <- crs$dataset[["CUSTCARE"]]
if (building)
{
  crs$dataset[["R01_CUSTCARE"]] <-  rescaler(crs$dataset[["CUSTCARE"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_CUSTCARE"]] <- (crs$dataset[["CUSTCARE"]] - 0.000000)/abs(32.330000 - 0.000000)
}

# Rescale THREEWAY.
crs$dataset[["R01_THREEWAY"]] <- crs$dataset[["THREEWAY"]]
if (building)
{
  crs$dataset[["R01_THREEWAY"]] <-  rescaler(crs$dataset[["THREEWAY"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_THREEWAY"]] <- (crs$dataset[["THREEWAY"]] - 0.000000)/abs(14.670000 - 0.000000)
}

# Rescale MOUREC.
crs$dataset[["R01_MOUREC"]] <- crs$dataset[["MOUREC"]]
if (building)
{
  crs$dataset[["R01_MOUREC"]] <-  rescaler(crs$dataset[["MOUREC"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_MOUREC"]] <- (crs$dataset[["MOUREC"]] - 0.000000)/abs(1122.040000 - 0.000000)
}

# Rescale OUTCALLS.
crs$dataset[["R01_OUTCALLS"]] <- crs$dataset[["OUTCALLS"]]
if (building)
{
  crs$dataset[["R01_OUTCALLS"]] <-  rescaler(crs$dataset[["OUTCALLS"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_OUTCALLS"]] <- (crs$dataset[["OUTCALLS"]] - 0.000000)/abs(396.000000 - 0.000000)
}

# Rescale INCALLS.
crs$dataset[["R01_INCALLS"]] <- crs$dataset[["INCALLS"]]
if (building)
{
  crs$dataset[["R01_INCALLS"]] <-  rescaler(crs$dataset[["INCALLS"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_INCALLS"]] <- (crs$dataset[["INCALLS"]] - 0.000000)/abs(404.000000 - 0.000000)
}

# Rescale PEAKVCE.
crs$dataset[["R01_PEAKVCE"]] <- crs$dataset[["PEAKVCE"]]

if (building)
{
  crs$dataset[["R01_PEAKVCE"]] <-  rescaler(crs$dataset[["PEAKVCE"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_PEAKVCE"]] <- (crs$dataset[["PEAKVCE"]] - 0.000000)/abs(1018.670000 - 0.000000)
}

# Rescale OPEAKVCE.
crs$dataset[["R01_OPEAKVCE"]] <- crs$dataset[["OPEAKVCE"]]
if (building)
{
  crs$dataset[["R01_OPEAKVCE"]] <-  rescaler(crs$dataset[["OPEAKVCE"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_OPEAKVCE"]] <- (crs$dataset[["OPEAKVCE"]] - 0.000000)/abs(1052.330000 - 0.000000)
}

# Rescale DROPBLK.
crs$dataset[["R01_DROPBLK"]] <- crs$dataset[["DROPBLK"]]
if (building)
{
  crs$dataset[["R01_DROPBLK"]] <-  rescaler(crs$dataset[["DROPBLK"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_DROPBLK"]] <- (crs$dataset[["DROPBLK"]] - 0.000000)/abs(87.000000 - 0.000000)
}

# Rescale CALLFWDV.
crs$dataset[["R01_CALLFWDV"]] <- crs$dataset[["CALLFWDV"]]
if (building)
{
  crs$dataset[["R01_CALLFWDV"]] <-  rescaler(crs$dataset[["CALLFWDV"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_CALLFWDV"]] <- (crs$dataset[["CALLFWDV"]] - 0.000000)/abs(1.000000 - 0.000000)
}

# Rescale CALLWAIT.
crs$dataset[["R01_CALLWAIT"]] <- crs$dataset[["CALLWAIT"]]
if (building)
{
  crs$dataset[["R01_CALLWAIT"]] <-  rescaler(crs$dataset[["CALLWAIT"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_CALLWAIT"]] <- (crs$dataset[["CALLWAIT"]] - 0.000000)/abs(101.000000 - 0.000000)
}

# Rescale MONTHS.
crs$dataset[["R01_MONTHS"]] <- crs$dataset[["MONTHS"]]
if (building)
{
  crs$dataset[["R01_MONTHS"]] <-  rescaler(crs$dataset[["MONTHS"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_MONTHS"]] <- (crs$dataset[["MONTHS"]] - 6.000000)/abs(55.000000 - 6.000000)
}

# Rescale UNIQSUBS.
crs$dataset[["R01_UNIQSUBS"]] <- crs$dataset[["UNIQSUBS"]]
if (building)
{
  crs$dataset[["R01_UNIQSUBS"]] <-  rescaler(crs$dataset[["UNIQSUBS"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_UNIQSUBS"]] <- (crs$dataset[["UNIQSUBS"]] - 1.000000)/abs(8.000000 - 1.000000)
}

# Rescale ACTVSUBS.
crs$dataset[["R01_ACTVSUBS"]] <- crs$dataset[["ACTVSUBS"]]
if (building)
{
  crs$dataset[["R01_ACTVSUBS"]] <-  rescaler(crs$dataset[["ACTVSUBS"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_ACTVSUBS"]] <- (crs$dataset[["ACTVSUBS"]] - 1.000000)/abs(4.000000 - 1.000000)
}

# Rescale PHONES.
crs$dataset[["R01_PHONES"]] <- crs$dataset[["PHONES"]]
if (building)
{
  crs$dataset[["R01_PHONES"]] <-  rescaler(crs$dataset[["PHONES"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_PHONES"]] <- (crs$dataset[["PHONES"]] - 1.000000)/abs(13.000000 - 1.000000)
}

# Rescale MODELS.
crs$dataset[["R01_MODELS"]] <- crs$dataset[["MODELS"]]
if (building)
{
  crs$dataset[["R01_MODELS"]] <-  rescaler(crs$dataset[["MODELS"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_MODELS"]] <- (crs$dataset[["MODELS"]] - 1.000000)/abs(9.000000 - 1.000000)
}

# Rescale EQPDAYS.
crs$dataset[["R01_EQPDAYS"]] <- crs$dataset[["EQPDAYS"]]
if (building)
{
  crs$dataset[["R01_EQPDAYS"]] <-  rescaler(crs$dataset[["EQPDAYS"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_EQPDAYS"]] <- (crs$dataset[["EQPDAYS"]] - -1.000000)/abs(1324.000000 - -1.000000)
}


# Rescale CHILDREN.
crs$dataset[["R01_CHILDREN"]] <- crs$dataset[["CHILDREN"]]
if (building)
{
  crs$dataset[["R01_CHILDREN"]] <-  rescaler(crs$dataset[["CHILDREN"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_CHILDREN"]] <- (crs$dataset[["CHILDREN"]] - 0.000000)/abs(1.000000 - 0.000000)
}


# Rescale RETCALLS.
crs$dataset[["R01_RETCALLS"]] <- crs$dataset[["RETCALLS"]]
if (building)
{
  crs$dataset[["R01_RETCALLS"]] <-  rescaler(crs$dataset[["RETCALLS"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_RETCALLS"]] <- (crs$dataset[["RETCALLS"]] - 0.000000)/abs(2.000000 - 0.000000)
}

# Rescale RETACCPT.
crs$dataset[["R01_RETACCPT"]] <- crs$dataset[["RETACCPT"]]
if (building)
{
  crs$dataset[["R01_RETACCPT"]] <-  rescaler(crs$dataset[["RETACCPT"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_RETACCPT"]] <- (crs$dataset[["RETACCPT"]] - 0.000000)/abs(2.000000 - 0.000000)
}



# Rescale REFER.
crs$dataset[["R01_REFER"]] <- crs$dataset[["REFER"]]
if (building)
{
  crs$dataset[["R01_REFER"]] <-  rescaler(crs$dataset[["REFER"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_REFER"]] <- (crs$dataset[["REFER"]] - 0.000000)/abs(6.000000 - 0.000000)
}

# Rescale INCOME.
crs$dataset[["R01_INCOME"]] <- crs$dataset[["INCOME"]]
if (building)
{
  crs$dataset[["R01_INCOME"]] <-  rescaler(crs$dataset[["INCOME"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_INCOME"]] <- (crs$dataset[["INCOME"]] - 0.000000)/abs(9.000000 - 0.000000)
}

# Rescale MCYCLE.
crs$dataset[["R01_MCYCLE"]] <- crs$dataset[["MCYCLE"]]
if (building)
{
  crs$dataset[["R01_MCYCLE"]] <-  rescaler(crs$dataset[["MCYCLE"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_MCYCLE"]] <- (crs$dataset[["MCYCLE"]] - 0.000000)/abs(1.000000 - 0.000000)
}

# Rescale CREDITAD.
crs$dataset[["R01_CREDITAD"]] <- crs$dataset[["CREDITAD"]]
if (building)
{
  crs$dataset[["R01_CREDITAD"]] <-  rescaler(crs$dataset[["CREDITAD"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_CREDITAD"]] <- (crs$dataset[["CREDITAD"]] - 0.000000)/abs(4.000000 - 0.000000)
}

# Rescale SETPRC.
crs$dataset[["R01_SETPRC"]] <- crs$dataset[["SETPRC"]]
if (building)
{
  crs$dataset[["R01_SETPRC"]] <-  rescaler(crs$dataset[["SETPRC"]], "range")
}
if (scoring)
{
  crs$dataset[["R01_SETPRC"]] <- (crs$dataset[["SETPRC"]] - 0.000000)/abs(199.990000 - 0.000000)
}

# Replace input variables with rescaled variables
crs$input <- c("CHURN", "CSA", "CSA_CITYCODE", "CSA_CITY",
               "CSA_STATE", "AGE1", "AGE2", "CREDITA",
               "CREDITAA", "CREDITB", "CREDITC", "CREDITDE",
               "CREDITGY", "CREDITZ", "CREDIT_RATING", "PRIZMRUR",
               "PRIZMUB", "PRIZMTWN", "PRZM_NUM", "REFURB",
               "WEBCAP", "TRUCK", "RV", "OCCPROF",
               "OCCCLER", "OCCCRFT", "OCCSTUD", "OCCRET",
               "OCCSELF", "OCC", "OCC_LABEL", "OWNRENT",
               "MARRYUN", "MARRYYES", "MARRYNO", "MARRY",
               "MARRY_LABEL", "MAILORD", "MAILRES", "MAILFLAG",
               "TRAVEL", "PCOWN", "CREDITCD", "NEWCELLY",
               "NEWCELLN", "INCMISS", "SETPRCM", "RETCALL",
               "R01_REVENUE", "R01_MOU", "R01_RECCHRGE", "R01_DIRECTAS",
               "R01_OVERAGE", "R01_ROAM", "R01_CHANGEM", "R01_CHANGER",
               "R01_DROPVCE", "R01_BLCKVCE", "R01_UNANSVCE", "R01_CUSTCARE",
               "R01_THREEWAY", "R01_MOUREC", "R01_OUTCALLS", "R01_INCALLS",
               "R01_PEAKVCE", "R01_OPEAKVCE", "R01_DROPBLK", "R01_CALLFWDV",
               "R01_CALLWAIT", "R01_MONTHS", "R01_UNIQSUBS", "R01_ACTVSUBS",
               "R01_CSA_AREACODE", "R01_PHONES", "R01_MODELS", "R01_EQPDAYS",
               "R01_CHILDREN", "R01_RETCALLS", "R01_RETACCPT", "R01_REFER",
               "R01_INCOME", "R01_MCYCLE", "R01_CREDITAD", "R01_SETPRC")

crs$numeric <- c("R01_REVENUE", "R01_MOU", "R01_RECCHRGE", "R01_DIRECTAS",
                 "R01_OVERAGE", "R01_ROAM", "R01_CHANGEM", "R01_CHANGER",
                 "R01_DROPVCE", "R01_BLCKVCE", "R01_UNANSVCE", "R01_CUSTCARE",
                 "R01_THREEWAY", "R01_MOUREC", "R01_OUTCALLS", "R01_INCALLS",
                 "R01_PEAKVCE", "R01_OPEAKVCE", "R01_DROPBLK", "R01_CALLFWDV",
                 "R01_CALLWAIT", "R01_MONTHS", "R01_UNIQSUBS", "R01_ACTVSUBS",
                 "R01_CSA_AREACODE", "R01_PHONES", "R01_MODELS", "R01_EQPDAYS",
                 "R01_CHILDREN", "R01_RETCALLS", "R01_RETACCPT", "R01_REFER",
                 "R01_INCOME", "R01_MCYCLE", "R01_CREDITAD", "R01_SETPRC")

crs$categoric <- c("CHURN", "CSA", "CSA_CITYCODE", "CSA_CITY",
                   "CSA_STATE", "AGE1", "AGE2", "CREDITA",
                   "CREDITAA", "CREDITB", "CREDITC", "CREDITDE",
                   "CREDITGY", "CREDITZ", "CREDIT_RATING", "PRIZMRUR",
                   "PRIZMUB", "PRIZMTWN", "PRZM_NUM", "REFURB",
                   "WEBCAP", "TRUCK", "RV", "OCCPROF",
                   "OCCCLER", "OCCCRFT", "OCCSTUD", "OCCRET",
                   "OCCSELF", "OCC", "OCC_LABEL", "OWNRENT",
                   "MARRYUN", "MARRYYES", "MARRYNO", "MARRY",
                   "MARRY_LABEL", "MAILORD", "MAILRES", "MAILFLAG",
                   "TRAVEL", "PCOWN", "CREDITCD", "NEWCELLY",
                   "NEWCELLN", "INCMISS", "SETPRCM", "RETCALL")

crs$target  <- "CHURNDEP"
crs$risk    <- NULL
crs$ident   <- "CUSTOMER"
crs$ignore  <- c("REVENUE", "MOU", "RECCHRGE", "DIRECTAS", "OVERAGE", "ROAM", "CHANGEM", "CHANGER", "DROPVCE", "BLCKVCE", "UNANSVCE", "CUSTCARE", "THREEWAY", "MOUREC", "OUTCALLS", "INCALLS", "PEAKVCE", "OPEAKVCE", "DROPBLK", "CALLFWDV", "CALLWAIT", "MONTHS", "UNIQSUBS", "ACTVSUBS", "CSA_AREACODE", "PHONES", "MODELS", "EQPDAYS", "CHILDREN", "OCCHMKR", "RETCALLS", "RETACCPT", "REFER", "INCOME", "MCYCLE", "CREDITAD", "SETPRC", "CALIBRAT")
crs$weights <- NULL

#============================================================
# Perform missing value imputation.
#============================================================

# Identify which variables contain missing values
summary(crs$dataset)
 
# Transform variables by imputing missing values replacing the missing value with the population mean.

# Impute R01_REVENUE.
crs$dataset[["IMN_R01_REVENUE"]] <- crs$dataset[["R01_REVENUE"]]

# Change all NAs to the mean value.
if (building)
{
  crs$dataset[["IMN_R01_REVENUE"]][is.na(crs$dataset[["R01_REVENUE"]])] <- mean(crs$dataset[["R01_REVENUE"]], na.rm=TRUE)
}
if (scoring)
{
  crs$dataset[["IMN_R01_REVENUE"]][is.na(crs$dataset[["R01_REVENUE"]])] <- 0.14246485371898
}

# Impute R01_MOU.
crs$dataset[["IMN_R01_MOU"]] <- crs$dataset[["R01_MOU"]]
if (building)
{
  crs$dataset[["IMN_R01_MOU"]][is.na(crs$dataset[["R01_MOU"]])] <- mean(crs$dataset[["R01_MOU"]], na.rm=TRUE)
}
if (scoring)
{
  crs$dataset[["IMN_R01_MOU"]][is.na(crs$dataset[["R01_MOU"]])] <- 0.167524375581661
}

# Impute R01_RECCHRGE.
crs$dataset[["IMN_R01_RECCHRGE"]] <- crs$dataset[["R01_RECCHRGE"]]
if (building)
{
  crs$dataset[["IMN_R01_RECCHRGE"]][is.na(crs$dataset[["R01_RECCHRGE"]])] <- mean(crs$dataset[["R01_RECCHRGE"]], na.rm=TRUE)
}
if (scoring)
{
  crs$dataset[["IMN_R01_RECCHRGE"]][is.na(crs$dataset[["R01_RECCHRGE"]])] <- 0.28910864415657
}

# Impute R01_DIRECTAS.
crs$dataset[["IMN_R01_DIRECTAS"]] <- crs$dataset[["R01_DIRECTAS"]]
if (building)
{
  crs$dataset[["IMN_R01_DIRECTAS"]][is.na(crs$dataset[["R01_DIRECTAS"]])] <- mean(crs$dataset[["R01_DIRECTAS"]], na.rm=TRUE)
}
if (scoring)
{
  crs$dataset[["IMN_R01_DIRECTAS"]][is.na(crs$dataset[["R01_DIRECTAS"]])] <- 0.0514590115012709
}

# Impute R01_OVERAGE.
crs$dataset[["IMN_R01_OVERAGE"]] <- crs$dataset[["R01_OVERAGE"]]
if (building)
{
  crs$dataset[["IMN_R01_OVERAGE"]][is.na(crs$dataset[["R01_OVERAGE"]])] <- mean(crs$dataset[["R01_OVERAGE"]], na.rm=TRUE)
}
if (scoring)
{
  crs$dataset[["IMN_R01_OVERAGE"]][is.na(crs$dataset[["R01_OVERAGE"]])] <- 0.0481050831740427
}

# Impute R01_ROAM.
crs$dataset[["IMN_R01_ROAM"]] <- crs$dataset[["R01_ROAM"]]
if (building)
{
  crs$dataset[["IMN_R01_ROAM"]][is.na(crs$dataset[["R01_ROAM"]])] <- mean(crs$dataset[["R01_ROAM"]], na.rm=TRUE)
}
if (scoring)
{
  crs$dataset[["IMN_R01_ROAM"]][is.na(crs$dataset[["R01_ROAM"]])] <- 0.0114535345818768
}

# Impute R01_CHANGEM.
crs$dataset[["IMN_R01_CHANGEM"]] <- crs$dataset[["R01_CHANGEM"]]
if (building)
{
  crs$dataset[["IMN_R01_CHANGEM"]][is.na(crs$dataset[["R01_CHANGEM"]])] <- mean(crs$dataset[["R01_CHANGEM"]], na.rm=TRUE)
}
if (scoring)
{
  crs$dataset[["IMN_R01_CHANGEM"]][is.na(crs$dataset[["R01_CHANGEM"]])] <- 0.513051778723451
}

# Impute R01_CHANGER.
crs$dataset[["IMN_R01_CHANGER"]] <- crs$dataset[["R01_CHANGER"]]
if (building)
{
  crs$dataset[["IMN_R01_CHANGER"]][is.na(crs$dataset[["R01_CHANGER"]])] <- mean(crs$dataset[["R01_CHANGER"]], na.rm=TRUE)
}
if (scoring)
{
  crs$dataset[["IMN_R01_CHANGER"]][is.na(crs$dataset[["R01_CHANGER"]])] <- 0.200412082839225
}

# Replace input variables with imputed variables
crs$input <- c("CHURN", "CSA", "CSA_CITYCODE", "CSA_AREACODE",
               "CSA_CITY", "CSA_STATE", "AGE1", "AGE2",
               "CREDITA", "CREDITAA", "CREDITB", "CREDITC",
               "CREDITDE", "CREDITGY", "CREDITZ", "CREDIT_RATING",
               "PRIZMRUR", "PRIZMUB", "PRIZMTWN", "PRZM_NUM",
               "REFURB", "WEBCAP", "TRUCK", "RV",
               "OCCPROF", "OCCCLER", "OCCCRFT", "OCCSTUD",
               "OCCRET", "OCCSELF", "OCC", "OCC_LABEL",
               "OWNRENT", "MARRYUN", "MARRYYES", "MARRYNO",
               "MARRY", "MARRY_LABEL", "MAILORD", "MAILRES",
               "MAILFLAG", "TRAVEL", "PCOWN", "CREDITCD",
               "NEWCELLY", "NEWCELLN", "INCMISS", "SETPRCM",
               "RETCALL", "R01_DROPVCE", "R01_BLCKVCE", "R01_UNANSVCE",
               "R01_CUSTCARE", "R01_THREEWAY", "R01_MOUREC", "R01_OUTCALLS",
               "R01_INCALLS", "R01_PEAKVCE", "R01_OPEAKVCE", "R01_DROPBLK",
               "R01_CALLFWDV", "R01_CALLWAIT", "R01_MONTHS", "R01_UNIQSUBS",
               "R01_ACTVSUBS", "R01_PHONES", "R01_MODELS", "R01_EQPDAYS",
               "R01_CHILDREN", "R01_RETCALLS", "R01_RETACCPT", "R01_REFER",
               "R01_INCOME", "R01_MCYCLE", "R01_CREDITAD", "R01_SETPRC",
               "IMN_R01_REVENUE", "IMN_R01_MOU", "IMN_R01_RECCHRGE", "IMN_R01_DIRECTAS",
               "IMN_R01_OVERAGE", "IMN_R01_ROAM", "IMN_R01_CHANGEM", "IMN_R01_CHANGER")

crs$numeric <- c("AGE1", "AGE2", "R01_DROPVCE", "R01_BLCKVCE",
                 "R01_UNANSVCE", "R01_CUSTCARE", "R01_THREEWAY", "R01_MOUREC",
                 "R01_OUTCALLS", "R01_INCALLS", "R01_PEAKVCE", "R01_OPEAKVCE",
                 "R01_DROPBLK", "R01_CALLFWDV", "R01_CALLWAIT", "R01_MONTHS",
                 "R01_UNIQSUBS", "R01_ACTVSUBS", "R01_PHONES", "R01_MODELS",
                 "R01_EQPDAYS", "R01_CHILDREN", "R01_RETCALLS", "R01_RETACCPT",
                 "R01_REFER", "R01_INCOME", "R01_MCYCLE", "R01_CREDITAD",
                 "R01_SETPRC", "IMN_R01_REVENUE", "IMN_R01_MOU", "IMN_R01_RECCHRGE",
                 "IMN_R01_DIRECTAS", "IMN_R01_OVERAGE", "IMN_R01_ROAM", "IMN_R01_CHANGEM",
                 "IMN_R01_CHANGER")

crs$categoric <- c("CHURN", "CSA", "CSA_CITYCODE", "CSA_AREACODE",
                   "CSA_CITY", "CSA_STATE", "CREDITA", "CREDITAA",
                   "CREDITB", "CREDITC", "CREDITDE", "CREDITGY",
                   "CREDITZ", "CREDIT_RATING", "PRIZMRUR", "PRIZMUB",
                   "PRIZMTWN", "PRZM_NUM", "REFURB", "WEBCAP",
                   "TRUCK", "RV", "OCCPROF", "OCCCLER",
                   "OCCCRFT", "OCCSTUD", "OCCRET", "OCCSELF",
                   "OCC", "OCC_LABEL", "OWNRENT", "MARRYUN",
                   "MARRYYES", "MARRYNO", "MARRY", "MARRY_LABEL",
                   "MAILORD", "MAILRES", "MAILFLAG", "TRAVEL",
                   "PCOWN", "CREDITCD", "NEWCELLY", "NEWCELLN",
                   "INCMISS", "SETPRCM", "RETCALL")

crs$target  <- "CHURNDEP"
crs$risk    <- NULL
crs$ident   <- "CUSTOMER"
crs$ignore  <- c("REVENUE", "MOU", "RECCHRGE", "DIRECTAS", "OVERAGE", "ROAM", "CHANGEM", "CHANGER", "DROPVCE", "BLCKVCE", "UNANSVCE", "CUSTCARE", "THREEWAY", "MOUREC", "OUTCALLS", "INCALLS", "PEAKVCE", "OPEAKVCE", "DROPBLK", "CALLFWDV", "CALLWAIT", "MONTHS", "UNIQSUBS", "ACTVSUBS", "PHONES", "MODELS", "EQPDAYS", "CHILDREN", "OCCHMKR", "RETCALLS", "RETACCPT", "REFER", "INCOME", "MCYCLE", "CREDITAD", "SETPRC", "CALIBRAT", "R01_REVENUE", "R01_MOU", "R01_RECCHRGE", "R01_DIRECTAS", "R01_OVERAGE", "R01_ROAM", "R01_CHANGEM", "R01_CHANGER")
crs$weights <- NULL

############################################################
# data exploration
############################################################

#============================================================
# Correlation Analysis
#============================================================

# The 'Hmisc' package provides the 'contents' function.
library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.
contents(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])

# Generate a correlation plot for the variables.
# The 'corrplot' package provides the 'corrplot' function.
library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.
crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.
crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.
print(crs$cor)

# Graphically display the correlations.
opar <- par(cex=0.5)
corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation Analysis")
par(opar)

#============================================================
# Principal Components Analysis (on numerics only).
#============================================================
pc <- prcomp(na.omit(crs$dataset[crs$sample, crs$numeric]), scale=TRUE, center=TRUE, tol=0)

# Show the output of the analysis.
pc

# Summarise the importance of the components found.
summary(pc)

# Display a plot showing the relative importance of the components.
plot(pc, main="")
title(main="Principal Components Importance")
axis(1, at=seq(0.7, ncol(pc$rotation)*1.2, 1.2), labels=colnames(pc$rotation), lty=0)

# Display a plot showing the two most principal components.
biplot(pc, main="")
title(main="Principal Components")

# Compute and visualize PCA using FactoMineR and factoextra for more details
install.packages("FactoMineR")

# Install and Load factoextra
install.packages("factoextra")
library(factoextra)

# The importance of PCs can be visualized using a screeplot
fviz_screeplot(pc, ncp=10)

#Plot the variables on the components. Variables are colored according to the values of the squared cosine
fviz_pca_var(pc, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=0.5) + theme_minimal()

# Contributions of variables on PC1
fviz_contrib(pc, choice = "var", axes = 1)

# Top 15 contributiobns of variables on PC1
fviz_contrib(pc, choice = "var", axes = 1, top = 15)

# Contributions of variables on PC2
fviz_contrib(pc, choice = "var", axes = 2)

# Top 15 contributiobns of variables on PC2
fviz_contrib(pc, choice = "var", axes = 2, top = 15)

# Plot variable factors map using color contributions
fviz_pca_var(pc, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=50) + theme_minimal()

#============================================================
# BORUTA ALL-RELEVANT FEATURE SELECTION
#============================================================

# Install and call package
install.packages("Boruta")
library(Boruta)

# Load dataset
traindata <- read.csv("file:///F:/1. RS Home Files/SNHU/DAT-690 Capstone Data Analytics/Datasets/NEW/cell.data.calibration.csv", header = T, stringsAsFactors = F)

# Check for missing values
summary(traindata)

# Replace blanks with NA
traindata[traindata == ""] <- NA
traindata <- traindata[complete.cases(traindata),]

#Run Boruta Package
set.seed(123)
boruta.train <- Boruta(CHURNDEP~., data = traindata, doTrace = 2)
print(boruta.train)

# Plot Boruta Variable Importance Chart
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

#Take decision on tentative attributes by comparing z-scores
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

############################################################
# Modeling
############################################################

#============================================================
# Variable Selection
#============================================================

# Select model variables based off of varaible selection analysis
# Build the training/validate/test datasets.
set.seed(crv$seed)
crs$nobs <- nrow(crs$dataset) # 563 observations
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.75*crs$nobs) # 422 observations
crs$validate <- NULL
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 141 observations

# The following variable selections have been noted.
crs$input <- c("CREDITC", "REFURB", "WEBCAP", "OCCCLER",
               "MAILRES", "MAILFLAG", "RETCALL", "R01_DROPVCE",
               "R01_MOUREC", "R01_EQPDAYS", "IMN_R01_MOU", "IMN_R01_RECCHRGE",
               "IMN_R01_ROAM", "R01_INCALLS")

crs$numeric <- c("R01_DROPVCE", "R01_MOUREC", "R01_EQPDAYS", "IMN_R01_MOU",
                 "IMN_R01_RECCHRGE", "IMN_R01_ROAM", "R01_INCALLS")

crs$categoric <- c("CREDITC", "REFURB", "WEBCAP", "OCCCLER",
                   "MAILRES", "MAILFLAG", "RETCALL")

crs$target  <- "CHURNDEP"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("REVENUE", "MOU", "RECCHRGE", "DIRECTAS", "OVERAGE", "ROAM", "CHANGEM", "CHANGER", "DROPVCE", "BLCKVCE", "UNANSVCE", "CUSTCARE", "THREEWAY", "MOUREC", "OUTCALLS", "INCALLS", "PEAKVCE", "OPEAKVCE", "DROPBLK", "CALLFWDV", "CALLWAIT", "CHURN", "MONTHS", "UNIQSUBS", "ACTVSUBS", "CSA", "PHONES", "MODELS", "EQPDAYS", "CUSTOMER", "AGE1", "AGE2", "CHILDREN", "CREDITA", "CREDITAA", "CREDITB", "CREDITDE", "CREDITGY", "CREDITZ", "CREDIT_RATING", "PRIZMRUR", "PRIZMUB", "PRIZMTWN", "PRZM_NUM", "TRUCK", "RV", "OCCPROF", "OCCCRFT", "OCCSTUD", "OCCHMKR", "OCCRET", "OCCSELF", "OCC", "OCC_LABEL", "OWNRENT", "MARRYUN", "MARRYYES", "MARRYNO", "MARRY", "MARRY_LABEL", "MAILORD", "TRAVEL", "PCOWN", "CREDITCD", "RETCALLS", "RETACCPT", "NEWCELLY", "NEWCELLN", "REFER", "INCMISS", "INCOME", "MCYCLE", "CREDITAD", "SETPRCM", "SETPRC", "CALIBRAT", "R01_MOU", "R01_RECCHRGE", "R01_ROAM")
crs$weights <- NULL

#============================================================
# Ada Boost Model
#============================================================
# The `ada' package implements the boost algorithm.
# Build the Ada Boost model.
set.seed(crv$seed)
crs$ada <- ada::ada(CHURNDEP ~ .,
                    data=crs$dataset[crs$train,c(crs$input, crs$target)],
                    control=rpart::rpart.control(maxdepth=30,
                                                 cp=0.010000,
                                                 minsplit=20,
                                                 xval=10),
                    iter=50)

# Print the results of the modelling.
print(crs$ada)
round(crs$ada$model$errs[crs$ada$iter,], 2)
cat('Variables actually used in tree construction:\n')
print(sort(names(listAdaVarsUsed(crs$ada))))
cat('\nFrequency of variables actually used:\n')
print(listAdaVarsUsed(crs$ada))

# Evaluate model performance.
# Generate an Error Matrix for the Ada Boost model.
# Obtain the response from the Ada Boost model.
crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Generate the confusion matrix showing counts.
table(crs$dataset[crs$test, c(crs$input, crs$target)]$CHURNDEP, crs$pr,
      useNA="ifany",
      dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.
pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                            function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(crs$dataset[crs$test, c(crs$input, crs$target)]$CHURNDEP, crs$pr)
round(per, 2)

# Calculate the overall error percentage.
cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.
cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

# Evaluate model performance.
# ROC Curve: requires the ROCR package.
library(ROCR)

# ROC Curve: requires the ggplot2 package.
library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ada model on cell [test].
crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.
no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$CHURNDEP)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL
if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Ada Boost cell [test] CHURNDEP")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.
# Remove observations with missing target.
no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$CHURNDEP)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL
if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

# Evaluate model performance.
# Lift Chart: requires the ROCR package.
library(ROCR)

# Obtain predictions for the ada model on cell [test].
crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.
no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$CHURNDEP)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL
if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

# Convert rate of positive predictions to percentage.
per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#CC0000FF", lty=1, xlab="Caseload (%)", add=FALSE)

# Generate a Lift Chart for the ada model on cell [train].
crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Also convert rate of positive predictions to percentage
per <- performance(prediction(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$CHURNDEP),"lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Now plot the lift.
# Bug in ROCR 1.0-3 plot does not obey the add command.# Calling the function directly (.plot.performance) does work.
.plot.performance(per, col="#00CCCCFF", lty=2, add=TRUE)

# Add a legend to the plot.
legend("topright", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="Ada Boost", inset=c(0.05, 0.05))

# Add decorations to the plot.
title(main="Lift Chart  cell ",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

# Plot the relative importance of the variables.
ada::varplot(crs$ada)

# Generate an Error Matrix for the Ada Boost model.
# Obtain the response from the Ada Boost model.
crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Generate the confusion matrix showing counts.
table(crs$dataset[crs$test, c(crs$input, crs$target)]$CHURNDEP, crs$pr,
      useNA="ifany",
      dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.
pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                            function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(crs$dataset[crs$test, c(crs$input, crs$target)]$CHURNDEP, crs$pr)
round(per, 2)

# Calculate the overall error percentage.
cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.
cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

#============================================================
# Advanced: Stochastic Gradient Boosting
#============================================================
#Install and call gbm and caret packages
install.packages("gbm")
install.packages("caret")
library(caret)
library(gbm)

# Create dataset for gbm model
churn.data <- crs$dataset

# Assign input variables to dataframe with dependent variable last
churnDF <- churn.data[c("CREDITC", "REFURB", "WEBCAP", "OCCCLER","MAILRES", "MAILFLAG", "RETCALL", "R01_DROPVCE","R01_MOUREC", "R01_EQPDAYS", "IMN_R01_MOU", "IMN_R01_RECCHRGE","IMN_R01_ROAM", "R01_INCALLS", "CHURNDEP")]

# Observe the structure of the dataset
print(str(churnDF))
print(names(churnDF))

# Generalize variables so the code can be recycled
outcomeName <- 'CHURNDEP'
predictorsNames <- names(churnDF)[names(churnDF) != outcomeName]

# Force gbm into classification mode by changing outcome variable to a factor
churnDF$CHURNDEP2 <- ifelse(churnDF$CHURNDEP==1,'yes','nope')
churnDF$CHURNDEP2 <- as.factor(churnDF$CHURNDEP2)
outcomeName <- 'CHURNDEP2'

# Partition dataset
set.seed(42)
splitIndex <- createDataPartition(churnDF[,outcomeName], p = .75, list = FALSE, times = 1)
trainDF <- churnDF[ splitIndex,]
testDF  <- churnDF[-splitIndex,]

# Tune the model with 10-fold cross validation
objControl <- trainControl(method='cv', number=10, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)

# Build the gbm model using ROC
objModel <- train(trainDF[,predictorsNames], trainDF[,outcomeName], 
                                       method='gbm', 
                                      trControl=objControl,  
                                       metric = "ROC",
                                       preProc = c("center", "scale"))

# Call summary() to find variable importance
summary(objModel)

# Find out what tuning parameters were most important to the model
print(objModel)

# Evaluate gbm model with class predictions and accuracy scores
predictions <- predict(object=objModel, testDF[,predictorsNames], type='raw')
head(predictions)
print(postResample(pred=predictions, obs=as.factor(testDF[,outcomeName])))

# Evaluate using probabilites
library(pROC)
predictions <- predict(object=objModel, testDF[,predictorsNames], type='prob')
head(predictions)

# Obtain AUC score for test set
auc <- roc(ifelse(testDF[,outcomeName]=="yes",1,0), predictions[[2]])
print(auc$auc)

# Variable Importance Plot 
plot(varImp(objModel,scale=F))




