###############################################################################
############################# DART Analysis ###################################
###############################################################################
################# Workflow to analyse DART data for GEPD flies ################
################ Analyse total distance travelled in 12 h light ###############
###############################################################################

############################### 20/03/2019 ####################################

###############################################################################
###################### Run DART_transform_batch.R #############################
###############################################################################

## Analyse DART experimental output with DART_transform_batch.R
# Analyse all three replicates individually
source("DART_transform_batch.R")

dir_path1 <- "../../2018/2018-02/2018-02-14/pk_3replicates_slo_1st/modified/"
sample_file_names1 <- "../../2018/2018-02/2018-02-14/pk_3replicates_slo_1st/modified/analysis/pk_3replicates_slo_1st_meta.csv"
slo1 <- DART_transform_batch(dir_path1, sample_file_names1, dead = 3600, bin = 1, threshold = 0, start = 120, end = 3720)

dir_path2 <- "../../2018/2018-02/2018-02-25/pk_3replicates_slo_2nd/modified/"
sample_file_names2 <- "../../2018/2018-02/2018-02-25/pk_3replicates_slo_2nd/modified/analysis/pk_3replicates_slo_2nd_meta.csv"
slo2 <- DART_transform_batch(dir_path2, sample_file_names2, dead = 3600, bin = 1, threshold = 0, start = 120, end = 3720)

dir_path3 <- "../../2018/2018-02/2018-02-28/pk_3replicates_slo_3rd/modified/"
sample_file_names3 <- "../../2018/2018-02/2018-02-28/pk_3replicates_slo_3rd/modified/analysis/pk_3replicates_slo_3rd_meta.csv"
slo3 <- DART_transform_batch(dir_path3, sample_file_names3, dead = 3600, bin = 1, threshold = 0, start = 120, end = 3720)

## 2. Process data to prepare for plotting
library("ggplot2")
library("ggthemes")
library("data.table")
library("FSA")


###############################################################################
########################## Total distance analysis ############################
###############################################################################

# Bind all data together
slo123 <- rbind(slo1, slo2, slo3)

# Calculate a data.table with average total distance travelled 
# throughout 12 h light for each fly - convert distance to m
setDT(slo123)
total_distance <- slo123[, .(total_distance = sum(mm) / 1000), by = fly]

# Add third column to total_distance, defining genotype/ID for groups of flies
total_distance[, genotype := substr(fly, 1, (nchar(fly) - 4))]

# If more than 9 flies were used, need to correct for genotype names
geno_length <- min(nchar(total_distance$genotype))
total_distance[, genotype := substr(fly, 1, geno_length)]

# Add extra column that defines mutant vs. ctrl generally
total_distance_ctrl_index_m <- grep("ctrl[1-9]_m", total_distance$genotype, perl = TRUE)
total_distance_ctrl_index_f <- grep("ctrl[1-9]_f", total_distance$genotype, perl = TRUE)
total_distance_gepd_index_m <- grep("gepd[1-9]_m", total_distance$genotype, perl = TRUE)
total_distance_gepd_index_f <- grep("gepd[1-9]_f", total_distance$genotype, perl = TRUE)

total_distance[, Genotype := "place_holder"]
total_distance[total_distance_ctrl_index_m, Genotype := "loxP_m"]
total_distance[total_distance_ctrl_index_f, Genotype := "loxP_f"]
total_distance[total_distance_gepd_index_m, Genotype := "GEPD_m"]
total_distance[total_distance_gepd_index_f, Genotype := "GEPD_f"]

# Grep males and females into separate data.tables
total_distance_m_index <- grep("m$", total_distance$Genotype, perl = TRUE)
total_distance_f_index <- grep("f$", total_distance$Genotype, perl = TRUE)

total_distance_m <- total_distance[total_distance_m_index]
total_distance_f <- total_distance[total_distance_f_index]

## 3. Plot the data
# Plot males and females individually

## 3a. males
setDT(total_distance_m)

total_distance_m_ctrl_index <- grep("^ctrl", total_distance_m$genotype, perl = TRUE)
total_distance_m_ctrl <- total_distance_m[total_distance_m_ctrl_index]
max_total_distance_m_ctrl <- max(total_distance_m_ctrl$total_distance) + 2

total_distance_m_gepd_index <- grep("^gepd", total_distance_m$genotype, perl = TRUE)
total_distance_m_gepd <- total_distance_m[total_distance_m_gepd_index]
max_total_distance_m_gepd <- max(total_distance_m_gepd$total_distance) + 2

sign_loxp <- data.frame(a = c(1, 3), b = rep(max_total_distance_m_ctrl, 2))
sign_loxp2 <- data.frame(a = c(2, 2), b = c(max_total_distance_m_ctrl, max_total_distance_m_ctrl + 1))
sign_combined <- data.frame(a = c(2, 5), b = rep(max_total_distance_m_ctrl + 1, 2))
sign_gepd <- data.frame(a = c(5, 5), b = c(max_total_distance_m_ctrl + 1, max_total_distance_m_gepd))
sign_gepd2 <- data.frame(a = c(4, 6), b = rep(max_total_distance_m_gepd, 2))

# Change Genotype for plotting
total_distance_m$Genotype <- factor(ifelse(total_distance_m$Genotype == "loxP_m", "loxP", "GEPD"),
                                    levels = c("loxP", "GEPD"))

total_distance_m$genotype <- factor(total_distance_m$genotype, levels = c("ctrl2_m", "ctrl3_m", "ctrl1_m", "gepd2_m", "gepd1_m", "gepd3_m"))

# Save the ggplot output to a variable
total_distance_m_gg <- ggplot(data = total_distance_m, aes(x = genotype, y = total_distance)) + 
                        geom_boxplot(outlier.shape = NA, na.rm = TRUE, aes(fill = Genotype)) +
                        scale_fill_manual(values = c("grey64", "firebrick"), labels = c(expression('slo'^{loxP}*'/+'), expression('slo'^{E366G}*'/+'))) +
                        geom_jitter(position = position_jitter(0.2, 0), shape = 4, na.rm = TRUE, size = 4) + 
                        scale_shape_discrete(solid = FALSE) +
                        theme_bw() +
                        scale_y_continuous(name = "Total Distance / m", limits = c(0, 50)) +
                        scale_x_discrete(name = NULL, labels = c(expression('132.1.1/+'), expression('111.1.1/+'), expression('7.1.1/+'), expression('25.1.1/+'), expression('137.1.3/+'), expression('72.1.1/+'))) +
                        theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
                          text = element_text(size = 50), 
                          legend.text = element_text(size = 30),
                          legend.justification = c(1, 1), legend.position = c(1, 1),
                          legend.box.margin = margin(c(10, 10, 10, 10)),
                          legend.title = element_blank(),
                          axis.text.x = element_text(face = "italic", angle = 45, hjust = 1, vjust = 1))

# Run the Kruskal-Wallis test
kruskal.test(total_distance_m$total_distance ~ total_distance_m$genotype, data = total_distance_m)

## 4b. Dunn's post-hoc test
# Perform a post-hoc analysis to see which levels are different from one another.
# One option of a post-hoc test is the Dunn test.
dunnTest(total_distance_m$total_distance ~ total_distance_m$genotype, data = total_distance_m)

total_distance_m_gg2 <- total_distance_m_gg + geom_line(data = sign_loxp, aes(x = a, y = b), size = 1) +
  geom_line(data = sign_loxp2, aes(x = a, y = b), size = 1) +
  geom_line(data = sign_loxp2, aes(x = a, y = b), size = 1) +
  geom_line(data = sign_combined, aes(x = a, y = b), size = 1) +
  geom_line(data = sign_gepd, aes(x = a, y = b), size = 1) +
  geom_line(data = sign_gepd2, aes(x = a, y = b), size = 1) +
  annotate("text", x = 3.5, y = max_total_distance_m_ctrl + 2.5, label = "****", size = 12)

# Plot the ggplot output together with significance lines
tiff("../slo_triplicates/total_distance_m.tiff", units = "in", width = 12.5, height = 10, res = 300)
total_distance_m_gg2
dev.off()

## 3b. females
# Repeat above pipeline for females
setDT(total_distance_f)

total_distance_f_ctrl_index <- grep("^ctrl", total_distance_f$genotype, perl = TRUE)
total_distance_f_ctrl <- total_distance_f[total_distance_f_ctrl_index]
max_total_distance_f_ctrl <- max(total_distance_f_ctrl$total_distance) + 2

total_distance_f_gepd_index <- grep("^gepd", total_distance_f$genotype, perl = TRUE)
total_distance_f_gepd <- total_distance_f[total_distance_f_gepd_index]
max_total_distance_f_gepd <- max(total_distance_f_gepd$total_distance) + 2

sign_loxp <- data.frame(a = c(1, 3), b = rep(max_total_distance_f_ctrl, 2))
sign_loxp2 <- data.frame(a = c(2, 2), b = c(max_total_distance_f_ctrl, max_total_distance_f_ctrl + 1))
sign_combined <- data.frame(a = c(2, 5), b = rep(max_total_distance_f_ctrl + 1, 2))
sign_gepd <- data.frame(a = c(5, 5), b = c(max_total_distance_f_ctrl + 1, max_total_distance_f_gepd))
sign_gepd2 <- data.frame(a = c(4, 6), b = rep(max_total_distance_f_gepd, 2))

# Change Genotype for plotting
total_distance_f$Genotype <- factor(ifelse(total_distance_f$Genotype == "loxP_f", "loxP", "GEPD"),
                                    levels = c("loxP", "GEPD"))

total_distance_f$genotype <- factor(total_distance_f$genotype, levels = c("ctrl2_f", "ctrl3_f", "ctrl1_f", "gepd2_f", "gepd1_f", "gepd3_f"))

# Save the ggplot output to a variable
total_distance_f_gg <- ggplot(data = total_distance_f, aes(x = genotype, y = total_distance)) + 
  geom_boxplot(outlier.shape = NA, na.rm = TRUE, aes(fill = Genotype)) +
  scale_fill_manual(values = c("grey64", "firebrick"), labels = c(expression('slo'^{loxP}*'/+'), expression('slo'^{E366G}*'/+'))) +
  geom_jitter(position = position_jitter(0.2, 0), shape = 4, na.rm = TRUE, size = 4) + 
  scale_shape_discrete(solid = FALSE) +
  theme_bw() +
  scale_y_continuous(name = "Total Distance / m", limits = c(0, 50)) +
  scale_x_discrete(name = NULL, labels = c(expression('132.1.1/+'), expression('111.1.1/+'), expression('7.1.1/+'), expression('25.1.1/+'), expression('137.1.3/+'), expression('72.1.1/+'))) +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50), 
        legend.text = element_text(size = 30),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.box.margin = margin(c(10, 10, 10, 10)),
        legend.title = element_blank(),
        axis.text.x = element_text(face = "italic", angle = 45, hjust = 1, vjust = 1))

# Run the Kruskal-Wallis test
kruskal.test(total_distance_f$total_distance ~ total_distance_f$genotype, data = total_distance_f)

## 4b. Dunn's post-hoc test
# Perform a post-hoc analysis to see which levels are different from one another.
# One option of a post-hoc test is the Dunn test.
dunnTest(total_distance_f$total_distance ~ total_distance_f$genotype, data = total_distance_f)

total_distance_f_gg2 <- total_distance_f_gg + geom_line(data = sign_loxp, aes(x = a, y = b), size = 1) +
  geom_line(data = sign_loxp2, aes(x = a, y = b), size = 1) +
  geom_line(data = sign_loxp2, aes(x = a, y = b), size = 1) +
  geom_line(data = sign_combined, aes(x = a, y = b), size = 1) +
  geom_line(data = sign_gepd, aes(x = a, y = b), size = 1) +
  geom_line(data = sign_gepd2, aes(x = a, y = b), size = 1) +
  annotate("text", x = 3.5, y = max_total_distance_f_ctrl + 2.5, label = "****", size = 12)

# Plot the ggplot output together with significance lines
tiff("../slo_triplicates/total_distance_f.tiff", units = "in", width = 12.5, height = 10, res = 300)
total_distance_f_gg2
dev.off()

###############################################################################
########################### Movement analysis #################################
###############################################################################

###############################################################################
################## Analyse speed distribution in 12 h light ###################
###############################################################################

## 1. Process slo123 for plotting
library(dplyr)
library(plyr)

# Have a look at slo123 again, which you will work with for this analysis
head(slo123)

# Add fourth column, defining genotype/ID for groups of flies
setDT(slo123)
slo123[, genotype := substr(fly, 1, (nchar(fly) - 4))]

# If more than 9 flies were used, need to correct for genotype names - 
geno_length <- min(nchar(slo123$genotype))
slo123[, genotype := substr(fly, 1, geno_length)]

# Add a column defining the general Genotype (e.g., all gepd_m_X's together)
slo123_ctrl_index_m <- grep("ctrl[1-9]_m", slo123$genotype, perl = TRUE)
slo123_ctrl_index_f <- grep("ctrl[1-9]_f", slo123$genotype, perl = TRUE)
slo123_gepd_index_m <- grep("gepd[1-9]_m", slo123$genotype, perl = TRUE)
slo123_gepd_index_f <- grep("gepd[1-9]_f", slo123$genotype, perl = TRUE)

slo123[, Genotype := "place_holder"]
slo123[slo123_ctrl_index_m, Genotype := "loxP_m"]
slo123[slo123_ctrl_index_f, Genotype := "loxP_f"]
slo123[slo123_gepd_index_m, Genotype := "GEPD_m"]
slo123[slo123_gepd_index_f, Genotype := "GEPD_f"]

# Plot and analyse males and females separately
slo123_m <- slo123[Genotype == "GEPD_m" | Genotype == "loxP_m"]
slo123_f <- slo123[Genotype == "GEPD_f" | Genotype == "loxP_f"]

# Males
tiff("../slo_triplicates/speed_m.tiff", units = "in", width = 15, height = 10, res = 300)
ggplot(slo123_m, aes(x = mm, colour = Genotype)) +
  stat_ecdf(geom = "step", size = 1) +
  theme_bw() +
  scale_colour_manual(values = c("firebrick", "grey64"), labels = c(expression('slo'^{E366G}*'/+'), expression('slo'^{loxP}*'/+'))) +
  xlab("Speed (mm / s)") +
  ylab("Percentage") +
  scale_x_continuous(limits = c(0, 10)) +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        axis.title.x = element_text(size = 50, margin = margin(t = 30, r = 0, b = 0, l = 0)),
        text = element_text(size = 50), 
        legend.text = element_text(size = 30),
        legend.justification = c(1, 0), legend.position = c(1, 0),
        legend.box.margin = margin(c(10, 10, 10, 10)),
        legend.title = element_blank(),
        legend.key.size = unit(2.0, 'cm')) +
  guides(colour = guide_legend(reverse = T))
dev.off()

# Females
tiff("../slo_triplicates/speed_f.tiff", units = "in", width = 15, height = 10, res = 300)
ggplot(slo123_f, aes(x = mm, colour = Genotype)) +
  stat_ecdf(geom = "step", size = 1) +
  theme_bw() +
  scale_colour_manual(values = c("firebrick", "grey64"), labels = c(expression('slo'^{E366G}*'/+'), expression('slo'^{loxP}*'/+'))) +
  xlab("Speed (mm / s)") +
  ylab("Percentage") +
  scale_x_continuous(limits = c(0, 10)) +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        axis.title.x = element_text(size = 50, margin = margin(t = 30, r = 0, b = 0, l = 0)),
        text = element_text(size = 50), 
        legend.text = element_text(size = 30),
        legend.justification = c(1, 0), legend.position = c(1, 0),
        legend.box.margin = margin(c(10, 10, 10, 10)),
        legend.title = element_blank(),
        legend.key.size = unit(2.0, 'cm')) +
    guides(colour = guide_legend(reverse=T))
dev.off()

## Stats - Kolmogorov-Smirnov test
# Males
slo123_m_gepd <- slo123_m[Genotype == "GEPD_m"]
slo123_m_loxp <- slo123_m[Genotype == "loxP_m"]
ks.test(slo123_m_gepd$mm, slo123_m_loxp$mm)

# Females
slo123_f_gepd <- slo123_f[Genotype == "GEPD_f"]
slo123_f_loxp <- slo123_f[Genotype == "loxP_f"]
ks.test(slo123_f_gepd$mm, slo123_f_loxp$mm)


###############################################################################
#################### Analyse the time moving in 12 h light ####################
###############################################################################

# Perform this analysis on pooled data by genotype (Genotype)

## 1. Process data for plotting
# Get the fraction of time active per fly
fraction_active <- slo123[, .(sum(mm != 0) / .SD[, .N]), by = fly]
colnames(fraction_active)[2] <- "active"

# Add genotype column to fraction_active
fraction_active[, genotype := substr(fly, 1, (nchar(fly) - 4))]

# If more than 9 flies were used, need to correct for genotype names
geno_length <- min(nchar(fraction_active$genotype))
fraction_active[, genotype := substr(fly, 1, geno_length)]

# Add a column defining the general Genotype (e.g., all gepd_m_X's together)
fraction_active_ctrl_index_m <- grep("ctrl[1-9]_m", fraction_active$genotype, perl = TRUE)
fraction_active_ctrl_index_f <- grep("ctrl[1-9]_f", fraction_active$genotype, perl = TRUE)
fraction_active_gepd_index_m <- grep("gepd[1-9]_m", fraction_active$genotype, perl = TRUE)
fraction_active_gepd_index_f <- grep("gepd[1-9]_f", fraction_active$genotype, perl = TRUE)

fraction_active[, Genotype := "place_holder"]
fraction_active[fraction_active_ctrl_index_m, Genotype := "loxP_m"]
fraction_active[fraction_active_ctrl_index_f, Genotype := "loxP_f"]
fraction_active[fraction_active_gepd_index_m, Genotype := "GEPD_m"]
fraction_active[fraction_active_gepd_index_f, Genotype := "GEPD_f"]

# Grep males and females into separate data.tables
frac_m_index <- grep("m$", fraction_active$Genotype, perl = TRUE)
frac_f_index <- grep("f$", fraction_active$Genotype, perl = TRUE)

fraction_active_m <- fraction_active[frac_m_index]
fraction_active_f <- fraction_active[frac_f_index]

## 2a. Plot active fractions of males
# After running the significance tests, you can add the significance bars.
# I am following the same process as described above for total_distance.
setDT(fraction_active_m)
fraction_active_m_gepd_index <- grep("^gepd", fraction_active_m$genotype, perl = TRUE)
fraction_active_m_gepd <- fraction_active_m[fraction_active_m_gepd_index]
max_fraction_active_m_gepd <- max(fraction_active_m_gepd$active)

fraction_active_m_ctrl_index <- grep("^ctrl", fraction_active_m$genotype, perl = TRUE)
fraction_active_m_ctrl <- fraction_active_m[fraction_active_m_ctrl_index]
max_fraction_active_m_ctrl <- max(fraction_active_m_ctrl$active)

fraction_active_m$Genotype <- factor(fraction_active_m$Genotype, levels = c("loxP_m", "GEPD_m"))

fraction_active_m_gg <- ggplot(data = fraction_active_m, aes(x = Genotype, y = active)) +
  geom_boxplot(outlier.shape = NA, na.rm = TRUE, aes(fill = Genotype)) +
  scale_fill_manual(values = c("grey64", "firebrick"), labels = c(expression('slo'^{loxP}*'/+'), expression('slo'^{E366G}*'/+'))) +
  geom_jitter(position = position_jitter(0.2, 0), shape = 4, na.rm = TRUE, size = 4) + 
  theme_bw() +
  scale_y_continuous(name = "Active Ratio", limits = c(0, 1.2), breaks = c(0, 0.5, 1)) +
  scale_x_discrete(name = NULL, labels = c(expression('slo'^{loxP}*'/+'), expression('slo'^{E366G}*'/+'))) +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50), 
        legend.position = "none",
        axis.text.x = element_text(face = "italic", angle = 45, hjust = 1, vjust = 1))

fraction_active_m_gg 

# Stats
wilcox.test(active ~ Genotype, data = fraction_active_m)

# Error bars
activity_bar_m <- max_fraction_active_m_gepd + 0.1
activity_bar_m_gg <- data.frame(Genotype = c(1, 2), active = rep(activity_bar_m, 2))

tiff("../slo_triplicates/active_ratio_m.tiff", units = "in", width = 12.5, height = 10, res = 300)
fraction_active_m_gg + geom_line(data = activity_bar_m_gg, aes(x = Genotype, y = active), size = 1) +
  annotate("text", x = 1.5, y = activity_bar_m + 0.05, label = "**", size = 12)
dev.off()

## 2b. Plot active fractions of females
setDT(fraction_active_f)
fraction_active_f_gepd_index <- grep("^gepd", fraction_active_f$genotype, perl = TRUE)
fraction_active_f_gepd <- fraction_active_f[fraction_active_f_gepd_index]
max_fraction_active_f_gepd <- max(fraction_active_f_gepd$active)

fraction_active_f_ctrl_index <- grep("^ctrl", fraction_active_f$genotype, perl = TRUE)
fraction_active_f_ctrl <- fraction_active_f[fraction_active_f_ctrl_index]
max_fraction_active_f_ctrl <- max(fraction_active_f_ctrl$active)

fraction_active_f$Genotype <- factor(fraction_active_f$Genotype, levels = c("loxP_f", "GEPD_f"))

fraction_active_f_gg <- ggplot(data = fraction_active_f, aes(x = Genotype, y = active)) +
  geom_boxplot(outlier.shape = NA, na.rm = TRUE, aes(fill = Genotype)) +
  scale_fill_manual(values = c("grey64", "firebrick"), labels = c(expression('slo'^{loxP}*'/+'), expression('slo'^{E366G}*'/+'))) +
  geom_jitter(position = position_jitter(0.2, 0), shape = 4, na.rm = TRUE, size = 4) + 
  theme_bw() +
  scale_y_continuous(name = "Active Ratio", limits = c(0, 1.2), breaks = c(0, 0.5, 1)) +
  scale_x_discrete(name = NULL, labels = c(expression('slo'^{loxP}*'/+'), expression('slo'^{E366G}*'/+'))) +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50), 
        legend.position = "none",
        axis.text.x = element_text(face = "italic", angle = 45, hjust = 1, vjust = 1))

fraction_active_f_gg 

# Stats
wilcox.test(active ~ Genotype, data = fraction_active_f)

# Error bars
activity_bar_f <- max_fraction_active_f_gepd + 0.1
activity_bar_f_gg <- data.frame(Genotype = c(1, 2), active = rep(activity_bar_f, 2))

tiff("../slo_triplicates/active_ratio_f.tiff", units = "in", width = 12.5, height = 10, res = 300)
fraction_active_f_gg + geom_line(data = activity_bar_f_gg, aes(x = Genotype, y = active), size = 1) +
  annotate("text", x = 1.5, y = activity_bar_f + 0.05, label = "****", size = 12)
dev.off()

###############################################################################
################## Analyse movement initiation in 12 h light ##################
###############################################################################

## 1. Process data before plotting

# Add fifth column of movement initiation - pad with FALSE (fill argument)
slo123[, initiation := shift(mm, n = 1, type = "lag", fill = -1) == 0 & mm != 0, by = fly]

# Get the movement initiaton number per fly as a new data.table
initiation <- slo123[, .(sum(initiation)), by = fly]
colnames(initiation)[2] <- "num_initiations"

# Add genotype column to initiation
initiation[, genotype := substr(fly, 1, (nchar(fly) - 4))]
# If more than 9 flies were used, need to correct for genotype names
geno_length <- min(nchar(initiation$genotype))
initiation[, genotype := substr(fly, 1, geno_length)]

# Add a column defining the general Genotype (e.g., all gepd_m_X's together)
initiation_ctrl_index_m <- grep("ctrl[1-9]_m", initiation$genotype, perl = TRUE)
initiation_ctrl_index_f <- grep("ctrl[1-9]_f", initiation$genotype, perl = TRUE)
initiation_gepd_index_m <- grep("gepd[1-9]_m", initiation$genotype, perl = TRUE)
initiation_gepd_index_f <- grep("gepd[1-9]_f", initiation$genotype, perl = TRUE)

initiation[, Genotype := "place_holder"]
initiation[initiation_ctrl_index_m, Genotype := "loxP_m"]
initiation[initiation_ctrl_index_f, Genotype := "loxP_f"]
initiation[initiation_gepd_index_m, Genotype := "GEPD_m"]
initiation[initiation_gepd_index_f, Genotype := "GEPD_f"]

# Grep males and females into separate data.tables
initiation_m_index <- grep("m$", initiation$Genotype, perl = TRUE)
initiation_f_index <- grep("f$", initiation$Genotype, perl = TRUE)

initiation_m <- initiation[initiation_m_index]
initiation_f <- initiation[initiation_f_index]

## 2. Plot the data
# Once the statistical tests have been run, you can add the significance bars,
# as described above for total_distance.

# 2a. Plot males
setDT(initiation_m)
initiation_m_gepd_index <- grep("^gepd", initiation_m$genotype, perl = TRUE)
initiation_m_gepd <- initiation_m[initiation_m_gepd_index]
max_initiation_m_gepd <- max(initiation_m_gepd$num_initiations)

initiation_m_ctrl_index <- grep("^ctrl", initiation_m$genotype, perl = TRUE)
initiation_m_ctrl <- initiation_m[initiation_m_ctrl_index]
max_initiation_m_ctrl <- max(initiation_m_ctrl$num_initiations)

# Define the singnificance bar position
ctrl_bar_up_m_init <- max_initiation_m_gepd + 50

initiation_m$Genotype <- factor(initiation_m$Genotype, levels = c("loxP_m", "GEPD_m"))

initiation_m_gg <- ggplot(data = initiation_m, aes(x = Genotype, y = num_initiations)) + 
  geom_boxplot(outlier.shape = NA, na.rm = TRUE, aes(fill = Genotype)) +
  scale_fill_manual(values = c("grey64", "firebrick"), labels = c(expression('slo'^{loxP}*'/+'), expression('slo'^{E366G}*'/+'))) +
  geom_jitter(position = position_jitter(0.2, 0), shape = 4, na.rm = TRUE, size = 4) + 
  theme_bw() +
  scale_y_continuous(name = "Movement Initiations", limits = c(0, 500)) +
  scale_x_discrete(name = NULL, labels = c(expression('slo'^{loxP}*'/+'), expression('slo'^{E366G}*'/+'))) +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50), 
        legend.position = "none",
        axis.text.x = element_text(face = "italic", angle = 45, hjust = 1, vjust = 1))

initiation_m_gg

# Stats
wilcox.test(num_initiations ~ Genotype, data = initiation_m)

# Error bars
initiation_bar_m_gg <- data.frame(Genotype = c(1, 2), active = rep(ctrl_bar_up_m_init, 2))

initiation_m_gg + geom_line(data = initiation_bar_m_gg, aes(x = Genotype, y = active), size = 1) +
  annotate("text", x = 1.5, y = ctrl_bar_up_m_init + 20, label = "****", size = 12)

tiff("../slo_triplicates/initiations_m.tiff", units = "in", width = 12.5, height = 10, res = 300)
initiation_m_gg + geom_line(data = initiation_bar_m_gg, aes(x = Genotype, y = active), size = 1) +
  annotate("text", x = 1.5, y = ctrl_bar_up_m_init + 20, label = "****", size = 12)
dev.off()

# 2b. Plot females
setDT(initiation_f)
initiation_f_gepd_index <- grep("^gepd", initiation_f$genotype, perl = TRUE)
initiation_f_gepd <- initiation_f[initiation_f_gepd_index]
max_initiation_f_gepd <- max(initiation_f_gepd$num_initiations)

initiation_f_ctrl_index <- grep("^ctrl", initiation_f$genotype, perl = TRUE)
initiation_f_ctrl <- initiation_f[initiation_f_ctrl_index]
max_initiation_f_ctrl <- max(initiation_f_ctrl$num_initiations)

# Define the singnificance bar position
gepd_bar_up_f_init <- max_initiation_f_gepd + 50

initiation_f$Genotype <- factor(initiation_f$Genotype, levels = c("loxP_f", "GEPD_f"))

initiation_f_gg <- ggplot(data = initiation_f, aes(x = Genotype, y = num_initiations)) + 
  geom_boxplot(outlier.shape = NA, na.rm = TRUE, aes(fill = Genotype)) +
  scale_fill_manual(values = c("grey64", "firebrick"), labels = c(expression('slo'^{loxP}*'/+'), expression('slo'^{E366G}*'/+'))) +
  geom_jitter(position = position_jitter(0.2, 0), shape = 4, na.rm = TRUE, size = 4) + 
  theme_bw() +
  scale_y_continuous(name = "Movement Initiations", limits = c(0, 500)) +
  scale_x_discrete(name = NULL, labels = c(expression('slo'^{loxP}*'/+'), expression('slo'^{E366G}*'/+'))) +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50), 
        legend.position = "none",
        axis.text.x = element_text(face = "italic", angle = 45, hjust = 1, vjust = 1))

initiation_f_gg

# Stats
wilcox.test(num_initiations ~ Genotype, data = initiation_f)

# Error bars
initiation_bar_f_gg <- data.frame(Genotype = c(1, 2), active = rep(gepd_bar_up_f_init, 2))

initiation_f_gg + geom_line(data = initiation_bar_f_gg, aes(x = Genotype, y = active), size = 1) +
  annotate("text", x = 1.5, y = gepd_bar_up_f_init + 20, label = "****", size = 12)

tiff("../slo_triplicates/initiations_f.tiff", units = "in", width = 12.5, height = 10, res = 300)
initiation_f_gg + geom_line(data = initiation_bar_f_gg, aes(x = Genotype, y = active), size = 1) +
  annotate("text", x = 1.5, y = gepd_bar_up_f_init + 20, label = "****", size = 12)
dev.off()

###############################################################################
############### Analyse bout length distribution in 12 h light ################
###############################################################################

# Add binary movement colum
slo123[, moving := as.numeric(mm != 0)]

# Use rle function to get lengths of movement bouts
slo123_rle <- slo123[, rle(moving), by = fly]
slo123_rle <- slo123_rle[values == 1][, 1:2]
# Get mean bout length per fly
slo123_mean_bout <- slo123_rle[, mean(lengths), by = fly]
colnames(slo123_mean_bout) <- c("fly", "mean_BL")

# Add columns for names and plotting
# Add genotype column to mean_bouts
slo123_mean_bout[, genotype := substr(fly, 1, (nchar(fly) - 4))]
# If more than 9 flies were used, need to correct for genotype names
geno_length <- min(nchar(slo123_mean_bout$genotype))
slo123_mean_bout[, genotype := substr(fly, 1, geno_length)]

# Add a column defining the general Genotype (e.g., all gepd_m_X's together)
mean_bouts_ctrl_index_m <- grep("ctrl[1-9]_m", slo123_mean_bout$genotype, perl = TRUE)
mean_bouts_ctrl_index_f <- grep("ctrl[1-9]_f", slo123_mean_bout$genotype, perl = TRUE)
mean_bouts_gepd_index_m <- grep("gepd[1-9]_m", slo123_mean_bout$genotype, perl = TRUE)
mean_bouts_gepd_index_f <- grep("gepd[1-9]_f", slo123_mean_bout$genotype, perl = TRUE)

slo123_mean_bout[, Genotype := "place_holder"]
slo123_mean_bout[mean_bouts_ctrl_index_m, Genotype := "loxP_m"]
slo123_mean_bout[mean_bouts_ctrl_index_f, Genotype := "loxP_f"]
slo123_mean_bout[mean_bouts_gepd_index_m, Genotype := "GEPD_m"]
slo123_mean_bout[mean_bouts_gepd_index_f, Genotype := "GEPD_f"]

# Grep males and females into separate data.tables
mean_bouts_m_index <- grep("m$", slo123_mean_bout$Genotype, perl = TRUE)
mean_bouts_f_index <- grep("f$", slo123_mean_bout$Genotype, perl = TRUE)

mean_bouts_m <- slo123_mean_bout[mean_bouts_m_index]
mean_bouts_f <- slo123_mean_bout[mean_bouts_f_index]

mean_bouts_m$Genotype <- factor(mean_bouts_m$Genotype, levels = c("loxP_m", "GEPD_m"))
mean_bouts_f$Genotype <- factor(mean_bouts_f$Genotype, levels = c("loxP_f", "GEPD_f"))

# Plot males
setDT(mean_bouts_m)
mean_bouts_m_gepd_index <- grep("^gepd", mean_bouts_m$genotype, perl = TRUE)
mean_bouts_m_gepd <- mean_bouts_m[mean_bouts_m_gepd_index]
max_mean_bouts_m_gepd <- max(mean_bouts_m_gepd$mean_BL)

mean_bouts_m_ctrl_index <- grep("^ctrl", mean_bouts_m$genotype, perl = TRUE)
mean_bouts_m_ctrl <- mean_bouts_m[mean_bouts_m_ctrl_index]
max_mean_bouts_m_ctrl <- max(mean_bouts_m_ctrl$mean_BL)

# Define the singnificance bar position
ctrl_bar_up_m_BL <- max_mean_bouts_m_ctrl + 100

mean_bouts_m_gg <- ggplot(data = mean_bouts_m, aes(x = Genotype, y = mean_BL)) + 
  geom_boxplot(outlier.shape = NA, na.rm = TRUE, aes(fill = Genotype)) +
  scale_fill_manual(values = c("grey64", "firebrick")) +
  geom_jitter(position = position_jitter(0.2, 0), shape = 4, na.rm = TRUE, size = 4) + 
  scale_shape_discrete(solid = FALSE) +
  theme_bw() +
  scale_y_continuous(name = "Mean Bout Length / s", limits = c(0, 1500)) +
  scale_x_discrete(name = NULL, labels = c(expression('slo'^{loxP}*'/+'), expression('slo'^{E366G}*'/+'))) +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50), 
        legend.position = "none",
        axis.text.x = element_text(face = "italic", angle = 45, hjust = 1, vjust = 1))

mean_bouts_m_gg 

# Stats
wilcox.test(mean_BL ~ Genotype, data = mean_bouts_m)

# Error bars
mean_bouts_bar_m_gg <- data.frame(Genotype = c(1, 2), mean_BL = rep(gepd_bar_up_m_BL, 2))

mean_bouts_m_gg + geom_line(data = mean_bouts_bar_m_gg, aes(x = Genotype, y = mean_BL), size = 1) +
  annotate("text", x = 1.5, y = gepd_bar_up_m_BL + 50, label = "****", size = 12)

tiff("../slo_triplicates/BL_m.tiff", units = "in", width = 12.5, height = 10, res = 300)
mean_bouts_m_gg + geom_line(data = mean_bouts_bar_m_gg, aes(x = Genotype, y = mean_BL), size = 1) +
  annotate("text", x = 1.5, y = gepd_bar_up_m_BL + 50, label = "****", size = 12)
dev.off()

# Plot females
setDT(mean_bouts_f)
mean_bouts_f_gepd_index <- grep("^gepd", mean_bouts_f$genotype, perl = TRUE)
mean_bouts_f_gepd <- mean_bouts_f[mean_bouts_f_gepd_index]
max_mean_bouts_f_gepd <- max(mean_bouts_f_gepd$mean_BL)

mean_bouts_f_ctrl_index <- grep("^ctrl", mean_bouts_f$genotype, perl = TRUE)
mean_bouts_f_ctrl <- mean_bouts_f[mean_bouts_f_ctrl_index]
max_mean_bouts_f_ctrl <- max(mean_bouts_f_ctrl$mean_BL)

# Define the singnificance bar position
ctrl_bar_up_f_BL <- max_mean_bouts_f_ctrl + 100

mean_bouts_f_gg <- ggplot(data = mean_bouts_f, aes(x = Genotype, y = mean_BL)) + 
  geom_boxplot(outlier.shape = NA, na.rm = TRUE, aes(fill = Genotype)) +
  scale_fill_manual(values = c("grey64", "firebrick")) +
  geom_jitter(position = position_jitter(0.2, 0), shape = 4, na.rm = TRUE, size = 4) + 
  scale_shape_discrete(solid = FALSE) +
  theme_bw() +
  scale_y_continuous(name = "Mean Bout Length / s", limits = c(0, 1500)) +
  scale_x_discrete(name = NULL, labels = c(expression('slo'^{loxP}*'/+'), expression('slo'^{E366G}*'/+'))) +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50), 
        legend.position = "none",
        axis.text.x = element_text(face = "italic", angle = 45, hjust = 1, vjust = 1))

mean_bouts_f_gg 

# Stats
wilcox.test(mean_BL ~ Genotype, data = mean_bouts_f)

# Error bars
mean_bouts_bar_f_gg <- data.frame(Genotype = c(1, 2), mean_BL = rep(ctrl_bar_up_f_BL, 2))

mean_bouts_f_gg + geom_line(data = mean_bouts_bar_f_gg, aes(x = Genotype, y = mean_BL), size = 1) +
  annotate("text", x = 1.5, y = ctrl_bar_up_f_BL + 50, label = "****", size = 12)

tiff("../slo_triplicates/BL_f.tiff", units = "in", width = 12.5, height = 10, res = 300)
mean_bouts_f_gg + geom_line(data = mean_bouts_bar_f_gg, aes(x = Genotype, y = mean_BL), size = 1) +
  annotate("text", x = 1.5, y = ctrl_bar_up_f_BL + 50, label = "****", size = 12)
dev.off()

###############################################################################
###############################################################################
###############################################################################

# Include session info
sessionInfo()
