### Normalise total activity data
# Script to test for normality and normalise data
# This is exemplified by total_distance but
# can be done for any parameter

# Load packages
library("ggpubr")
library("dplyr")
library("data.table")

## PREPARE

# Calculate a data.table with average activity per minute 
# throughout experiment for each fly - convert distance to m
# data is DART_transform_batch output
setDT(data)
total_distance <- data[, .(total_distance = sum(mm) / 1000), by = fly]

# Add third column to total_distance, defining genotype for groups of flies -
# important for plotting by genotype below
total_distance[, genotype := substr(fly, 1, (nchar(fly) - 2))]
# If more than 9 flies were used, need to correct for genotype names
geno_length <- min(nchar(total_distance$genotype))
total_distance[, genotype := substr(fly, 1, geno_length)]

## VISUALISE AND PLOT

# Visual inspection of data with density plot and QQ-plot
subset <- total_distance[genotype == "l,dy", ]
ggdensity(subset$total_distance)

# Visual inspection with QQ-plot
ggqqplot(subset$total_distance)

# Test for normality
shapiro.test(subset$total_distance)

## PLOT

# If tests are passed, normalise data
means <- total_distance[, .(mean(total_distance)), by = genotype]
colnames(means)[2] <- "mean"
gepd_mean <- means[means$genotype == "gepd", ]$mean
loxp_mean <- means[means$genotype == "loxp", ]$mean
total_distance[genotype == "gepd", total_distance := (total_distance / gepd_mean)]
total_distance[genotype == "g+dy", total_distance := (total_distance / gepd_mean)]
total_distance[genotype == "g,dy", total_distance := (total_distance / gepd_mean)]
total_distance[genotype == "loxp", total_distance := (total_distance / loxp_mean)]
total_distance[genotype == "l+dy", total_distance := (total_distance / loxp_mean)]
total_distance[genotype == "l,dy", total_distance := (total_distance / loxp_mean)]

ggplot(data = total_distance, aes(x = genotype, y = total_distance, colour = genotype)) + 
  geom_boxplot() +
  geom_jitter(height = 0, width = 0.1) + 
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 8, 0.5)) +
  ylab("Total Distance / m") +
  theme(axis.title.x = element_blank(), 
        axis.line = element_line(colour="black"),
        panel.grid.major = element_line(colour="#f0f0f0"), 
        panel.grid.minor = element_blank())

