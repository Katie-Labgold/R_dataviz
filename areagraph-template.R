#--------------------------------#
# Area Graph
#
# K. Labgold
# 11/07/2022
#--------------------------------#

# Sample Data Preparation ----

library(ggplot2)
library(dplyr)

sample.dat3 <- as.data.frame(matrix(ncol = 3, nrow = 15))
colnames(sample.dat3) <- c("Group", "timepoint", "value")

sample.dat3$Group <- rep(c("A", "B", "C"), each = 5)
sample.dat3$timepoint <- rep(1:5, 3)
sample.dat3$value <- round(rnorm(15, mean = 25, sd = 10), 0) # random number generator for count @ timepoint


sample.dat3.overall <- sample.dat3 %>%
                        group_by(timepoint) %>%
                        summarize(value.sum = sum(value)) %>%
                        arrange(timepoint) %>%
                        mutate(cumcount.overall = cumsum(value.sum)) %>%
                        ungroup() %>%
                        mutate(denom = 1000, # add fake denominator
                               prop.overall = round((cumcount.overall/denom)*100, 0)) # calculate proportion

sample.dat3.group <- sample.dat3 %>%
                     # cumulative count by group
                        group_by(Group) %>%
                        arrange(timepoint) %>%
                        mutate(cumcount.group = cumsum(value)) %>%
                        ungroup() %>%
                        mutate(denom = 1000,
                               denom.group = case_when(
                                 Group == "A" ~ 250,
                                 Group == "B" ~ 250,
                                 Group == "C" ~ 500
                               ),
                               prop.group = round((cumcount.group/denom.group)*100, 0),
                               proportional.prop.group = prop.group*(denom.group/denom))

# Plots for totals ----

## Plot for overall totals ----

cols.overall <- rep(c("darkgreen"), 5)

ggplot(sample.dat3.overall, aes(x = timepoint, y = cumcount.overall)) +
  geom_area(col = "black", fill = cols.overall, alpha = 0.75, size = 1.3) +
  theme_bw() +
  ggtitle("Total values (timepoints 1-5)") +
  theme(
    panel.grid.major.y = element_line(size = 1),
    panel.grid.minor = element_blank(),
    panel.border=element_blank(),
    axis.ticks=element_blank(),
    plot.title=element_text(size = 16, face="bold"),
    plot.title.position = "plot",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)),
    plot.caption=element_text(size=8, margin=margin(t=12), color="#7a7d7e")
  )


## Plot by group totals ----

cols.group <- rep(c("#0c457d", "#6bd2db", "#e8702a"), each = 5)

ggplot(sample.dat3.group, aes(x = timepoint, y = cumcount.group, group=Group)) +
  geom_area(col = "black", size = 1.3, alpha = 0.75, fill = cols.group) +
  theme_bw() +
  ggtitle("Total values by group (timepoints 1-5)") +
  theme(
    panel.grid.major.y = element_line(size = 1),
    panel.grid.minor = element_blank(),
    panel.border=element_blank(),
    axis.ticks=element_blank(),
    plot.title=element_text(size = 16, face="bold"),
    plot.title.position = "plot",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)),
    plot.caption=element_text(size=8, margin=margin(t=12), color="#7a7d7e")
  ) +
  # Add text labels for each group
  geom_text(aes(x = 4.6, y = 315, label = "Group A"),
            color = "Black", size = 4, vjust = -1.5, 
            fontface = "bold", family = "sans") +
  geom_text(aes(x = 4.6, y = 175, label = "Group B"),
            color = "Black", size = 4, vjust = -1.5, 
            fontface = "bold", family = "sans") +
  geom_text(aes(x = 4.6, y = 40, label = "Group C"),
            color = "Black", size = 4, vjust = -1.5, 
            fontface = "bold", family = "sans")

# Plots for proportions ----

## Overall proportion ----
ggplot(sample.dat3.overall, aes(x = timepoint, y = prop.overall)) +
  geom_area(col = "black", fill = cols.overall, alpha = 0.75, size = 1.3) +
  theme_bw() +
  ggtitle("Proportion value (timepoints 1-5)") +
  ylim(0, 100) +
  theme(
    panel.grid.major.y = element_line(size = 1),
    panel.grid.minor = element_blank(),
    panel.border=element_blank(),
    axis.ticks=element_blank(),
    plot.title=element_text(size = 16, face="bold"),
    plot.title.position = "plot",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)),
    plot.caption=element_text(size=8, margin=margin(t=12), color="#7a7d7e")
  )

## Group proportions ----
# visually groups are proportional to the total proportion completed
# but note, it doesn't communicate the max of each group
# e.g., group B is more than 50% for that group but we can't see that here

ggplot(sample.dat3.group, aes(x = timepoint, y = proportional.prop.group, group=Group)) +
  geom_area(col = "black", size = 1.3, alpha = 0.75, fill = cols.group) +
  theme_bw() +
  ggtitle("Visually proportional % values by group (timepoints 1-5)") +
  theme(
    panel.grid.major.y = element_line(size = 1),
    panel.grid.minor = element_blank(),
    panel.border=element_blank(),
    axis.ticks=element_blank(),
    plot.title=element_text(size = 16, face="bold"),
    plot.title.position = "plot",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)),
    plot.caption=element_text(size=8, margin=margin(t=12), color="#7a7d7e")
  ) +
  ylim(0, 100) +
  # Add text labels for each group
  geom_text(aes(x = 4.6, y = 28, label = "Group A"),
            color = "Black", size = 4, vjust = -1.5, 
            fontface = "bold", family = "sans") +
  geom_text(aes(x = 4.6, y = 14, label = "Group B"),
            color = "Black", size = 4, vjust = -1.5, 
            fontface = "bold", family = "sans") +
  geom_text(aes(x = 4.6, y = 1, label = "Group C"),
            color = "Black", size = 4, vjust = -1.5, 
            fontface = "bold", family = "sans")

### Group proportions: overlay ----
# to get around the limitation, overlay by group
# [KL Placeholder]


### Group proportions: small multiples ----
# alternative to overlay
textA <- data.frame(timepoint = 4.6, prop.group = 10, lab = "Group A", Group = "A")
textB <- data.frame(timepoint = 4.6, prop.group = 10, lab = "Group B", Group = "B")
textC <- data.frame(timepoint = 4.6, prop.group = 10, lab = "Group C", Group = "C")

ggplot(sample.dat3.group, aes(x = timepoint, y = prop.group, group=Group)) +
  geom_area(col = "black", size = 1.3, alpha = 0.75, fill = cols.group) +
  theme_bw() +
  ggtitle("Visually proportional % values by group (timepoints 1-5)") +
  theme(
    panel.spacing = unit(1.5, "lines"), # change spacing between plots
    strip.text.y = element_blank(), # remove squares
    panel.grid.major.y = element_line(size = 1),
    panel.grid.minor = element_blank(),
    panel.border=element_blank(),
    axis.ticks=element_blank(),
    plot.title=element_text(size = 16, face="bold"),
    plot.title.position = "plot",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)),
    plot.caption=element_text(size=8, margin=margin(t=12), color="#7a7d7e")
  ) +
  ylim(0, 100) +
  facet_grid(vars(Group)) +
  geom_text(data = textA, label = "Group A", fontface = "bold") +
  geom_text(data = textB, label = "Group B", fontface = "bold") +
  geom_text(data = textC, label = "Group C", fontface = "bold")
 

