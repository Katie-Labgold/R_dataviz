#--------------------------------------#
# Slopegraph template
# K. Labgold
# 11/01/2022
#--------------------------------------#


# Creating sample data ----
sample.dat <- as.data.frame(matrix(ncol = 3, nrow = 6))
colnames(sample.dat) <- c("Year", "Group", "Value")

sample.dat$Year <- c("2010", "2010", "2010", "2020", "2020", "2020")
sample.dat$Group <- c("A", "B", "C", "A", "B", "C")
sample.dat$Value <- c(10, 25, 50, 20, 15, 35)

# Slope graph: version #1 w/ numbers 
library(CGPfunctions)
library(ggplot2)
library(dplyr)
library(extrafont)
library(extrafont)
yloadfonts(device = "win")

color <- c("A" = "gray", "B" = "gray", "C" = "darkorange")
label.color <- c("darkgray", "darkgray", "darkorange", 
                 "darkgray", "darkgray", "darkorange")

newggslopegraph(dataframe = sample.dat, # dataset components
                Times = Year, 
                Measurement = Value, 
                Grouping = Group,
                
                # Primary figure attributes
                Title = "Figure Title",
                SubTitle = "Figure Caption",
                Caption = "KL template",
                LineColor = color,
                LineThickness = 1.5,
                
                # Sizing / placement of attributes
                XTextSize = 15,    # Size of the times
                YTextSize = 5,     # Size of the groups
                TitleTextSize = 14,
                SubTitleTextSize = 12,
                CaptionTextSize = 10,
                TitleJustify = "left",
                SubTitleJustify = "left",
                CaptionJustify = "left",
                DataTextColor = label.color,
                DataTextSize = 4.5) + # Size of the data)  
                
                # Adding Y axis gridlines
                ##theme(panel.grid.major.y = element_line(color = "lightgray", size = 0.25)) +
  
                # Adding X axis gridlines
                theme(panel.grid.major.x = element_line(color = "lightgray", size = 1))

# Slope graph: version #2 w/ dots and formattable group labels

color <- c("A" = "gray", "B" = "gray", "C" = "darkorange")
label.color2 <- c("darkgray", "darkgray", "darkorange")

ggplot(data = sample.dat, aes(x = Year, y = Value, group = Group)) +
  geom_line(aes(color = Group), size = 2) + # add alpha in aes if want it to be transparent
  geom_point(aes(color = Group), size = 4) + # add alpha in aes if want it to be transparent
  scale_color_manual(values = c("darkgray",
                                "darkgray",
                                "darkorange")) +
  # Label at top
  scale_x_discrete(position = "top") +
  theme_bw() +
  # Add label text for 2010
  geom_text(data = sample.dat %>% filter(Year == "2010"), 
            #aes(label = Group, # if just want group
            aes(label = paste0(Group, " - ", Value, "%"), # if want group w/ value
                  hjust = 1.25, 
                  fontface = "bold", 
                  size = 4),
            color = label.color2) +
  # Add label text for 2020
  geom_text(data = sample.dat %>% filter(Year == "2020"), 
            aes(label = Group,
                hjust = -1, 
                fontface = "bold", 
                size = 4),
            color = label.color2) +

  # Remove the legend
  theme(legend.position = "none") +
  # Remove the panel border
  theme(panel.border = element_blank()) +
  #  Labelling as desired
  labs(
    title = "Title",
    subtitle = "Subtitle indicating that these are the values",
    caption = "KL template"
  ) +
  # Remove axis ticks
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "lightgray", size = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 16, family = "sans")) # Arial
