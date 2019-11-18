# load data
opioids.df <- read.csv("Narrowed Project Data.csv", header = TRUE) 

# find the dimension of data frame
dim(opioids.df)

# show the first six rows
head(opioids.df)

# show variables in string
setNames(opioids.df, gsub ("[ ,.]","", tolower(names(opioids.df))))
str(opioids.df)

# find summary statistics for each column
summary(opioids.df)

# use dummies package
library(dummies)
opioids.df <- dummy.data.frame(opioids.df, sep = ".")
names(opioids.df)

# use set.seed() to get the same partitions when re-running the R code.
set.seed(1)

## partitioning into training (60%) and validation (40%)
# randomly sample 60% of the row IDs for training; the remaining 40% serve as
# validation
train.rows <- sample(rownames(opioids.df), dim(opioids.df)[1]*0.6)
# collect all the columns with training row ID into training set:
train.data <- opioids.df[train.rows, ]
# assign row IDs that are not already in the training set, into validation 
valid.rows <- setdiff(rownames(opioids.df), train.rows) 
valid.data <- opioids.df[valid.rows, ]

# compute mean TREATED4OPIODS per pnrnmflag = (0, 1)
data.for.plot <- aggregate(opioids.df$TREATED4OPIODS, by = list(opioids.df$pnrnmflag), FUN = mean)
names(data.for.plot) <- c("pnrnmflag", "MeanTREATED4OPIODS")
barplot(data.for.plot$MeanTREATED4OPIODS,  names.arg = data.for.plot$pnrnmflag, 
        xlab = "pnrnmflag", ylab = "Avg. TREATED4OPIODS", col=c("darkblue","red"))

# alternative plot with ggplot
library(ggplot2)
ggplot(data.for.plot) + geom_bar(aes(x = pnrnmflag, y = MeanTREATED4OPIODS), stat = "identity")

## boxplot of TREATED4OPIODS for different values of herage
boxplot(opioids.df$TREATED4OPIODS ~ opioids.df$herage, xlab = "TREATED4OPIODS", ylab = "herage")

# alternative plot with ggplot
ggplot(opioids.df) + geom_boxplot(aes(x = as.factor(TREATED4OPIODS), y = herage)) + xlab("TREATED4OPIODS")

# alternative plot with ggplot
library(ggplot2)
ggplot(opioids.df, aes(x= TREATED4OPIODS,  group=irsex)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="TREATED4OPIODS") +
  facet_grid(~irsex) +
  scale_y_continuous(labels = scales::percent)

## histogram of TREATED4OPIODS
hist(opioids.df$TREATED4OPIODS, xlab = "TREATED4OPIODS")

# We use opioids data as a demo data set, standardize the data to make variables comparable
df <- scale(opioids.df)

# R base heatmap
heatmap(df, scale = "none")

# Enhanced heat maps
library("gplots")
heatmap(df, scale = "none", col = bluered(100), 
          trace = "none", density.info = "none")

library(ggplot2)
ggplot(opioids.df, aes(x= TREATED4OPIODS,  group=irsex)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="TREATED4OPIODS") +
  facet_grid(~irsex) +
  scale_y_continuous(labels = scales::percent)
