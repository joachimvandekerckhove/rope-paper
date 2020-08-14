# Load libraries
library(tidyverse)
library(cowplot)

# Load posterior distributions for plotting
posteriorsPlot <- read_csv("../Datasets/SooRottmanDFE_posteriors.csv") %>% as_tibble()

# Get HDI
hdi <- posteriorsPlot %>% filter(hdi95==1) %>% group_by(experiment, analysis) %>%
    summarise(Q2.5=min(logit), Q97.5=max(logit))
# Set levels
post.df <- merge(posteriorsPlot, hdi)
post.df$dodge <- -20
post.df$analysis <- as.factor(ifelse(post.df$analysis=="underweighting", "Analysis of underweighting", "Analysis of decision policies"))
post.df$experiment <- as.factor(ifelse(post.df$experiment=="HH", "Hills & Hertwig (2010)", paste("Study ", as.character(post.df$experiment), sep="")))
post.df$analysis <- factor(post.df$analysis, levels = c("Analysis of underweighting", "Analysis of decision policies"))
post.df$experiment <- factor(post.df$experiment, levels = c("Hills & Hertwig (2010)", "Study 1", "Study 2", "Study 3", "Study 4"))

# Plot (Figure B1)
ggplot(post.df, aes(x=logit)) +
    theme_minimal() +
    geom_histogram(binwidth=.05, alpha=1, position=position_identity(), color="grey40") +
    facet_grid(experiment~analysis) +
    geom_segment(aes(x=Q2.5, xend=Q97.5, y=dodge, yend=dodge), size=.5, color="black") +
    scale_y_continuous(breaks=c()) +
    scale_x_continuous(minor_breaks = c(), breaks=c(-1.694595721, -1.238078417, -0.810930216, -0.401341391, 0, 0.401341391, 0.810930216, 1.238078417, 1.694595721, 2.197224577, 2.772588722), limits = c(-1.5, 3)) +
    geom_vline(xintercept = 0, color="black") +
    ylab("Density") +
    xlab("") +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank())
