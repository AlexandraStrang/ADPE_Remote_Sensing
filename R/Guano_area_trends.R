
# Plots exploring the trends in guano area of four Adélie penguin colonies in the Ross Sea extracted from VHR imagery over 2009-2021
# Creator: Alexandra Strang

sessionInfo() # for citing package versions
citation() # for citing packages

#####################################################################################################################
# Read in data
#####################################################################################################################

Guanoarea <- read.csv("ADPE_guano_area_trends.csv")

# Add colour to sites 
colours<-c("blue","red","green","orange")

#####################################################################################################################
# Plot guano area change for each colony over time
#####################################################################################################################

library(ggplot2)

Four_trends_plot <- ggplot(Guanoarea, aes(x = Season, y = Guano_area, col = Colony)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~factor(Colony, levels = c('Cape Crozier', 'Cape Bird', 'Cape Royds', 'Inexpressible Island')),
  scales ="free_y") +
  xlab("Season") +
  ylab("Guano area (m2)") +
  theme_minimal() +
  theme(legend.position ="none") +
  theme(axis.line = element_line(color='black'),
       plot.background = element_blank(),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.border = element_blank()) +
  scale_colour_manual(values=colours) +
  scale_x_continuous(breaks=seq(2009,2021,by=2))

Four_trends_plot # Plot not presented in manuscript

# Try all together on the same graph

Guanoarea$logGuano_area <- log(Guanoarea$Guano_area)

All_trends_plot <- ggplot(Guanoarea, aes(x = Season, y = logGuano_area, group=Colony)) + 
  geom_point(aes(colour = Colony)) + 
  geom_line(aes(colour = Colony)) +
  xlab("Season") +
  ylab("Log Guano area (m2)") +
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  scale_color_manual(values = colours) +
  scale_x_continuous(breaks=seq(2009,2021,by=2))

All_trends_plot # Plot not presented in manuscript

#####################################################################################################################
# Plot individually and then combine
#####################################################################################################################

# Subset data by colony

CROZdf <- subset(Guanoarea, Guanoarea$Colony_code=="CROZ")
BIRDdf <- subset(Guanoarea, Guanoarea$Colony_code=="BIRD")
ROYDdf <- subset(Guanoarea, Guanoarea$Colony_code=="ROYD")
INEXdf <- subset(Guanoarea, Guanoarea$Colony_code=="INEX")

# Crozier
Crozier_trend_plot <- ggplot(CROZdf, aes(x = Season, y = Guano_area)) + 
  geom_point(colour = "#Ff0000") + 
  geom_line(colour = "#Ff0000") +
  xlab(element_blank()) +
  ylab(element_blank()) +
  theme_minimal() +
  theme(legend.position = c(1,1)) + 
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  scale_x_continuous(limits=c(2009,2021), breaks = seq(2009,2021, by=2)) +
  scale_y_continuous(limits = c(330000,540000), breaks = seq(350000,540000, by=50000))

Crozier_trend_plot

# Bird
Bird_trend_plot <- ggplot(BIRDdf, aes(x = Season, y = Guano_area)) + 
  geom_point(colour= "#0000FF") + 
  geom_line(colour= "#0000FF") +
  xlab(element_blank()) +
  ylab(element_blank()) +
  theme_minimal() +
  theme(legend.position = c(1,1)) + 
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  scale_x_continuous(limits = c(2009,2021), breaks = seq(2009,2021, by=2)) +
  scale_y_continuous(limits = c(115000,159900), breaks = seq(120000,159000, by=10000))

Bird_trend_plot

# Royds
Royds_trend_plot <- ggplot(ROYDdf, aes(x = Season, y = Guano_area)) + 
  geom_point(colour = "#00ff00") + 
  geom_line(colour = "#00ff00") +
 xlab(element_blank()) +
 ylab(element_blank()) +
  theme_minimal() +
  theme(legend.position = c(1,1)) + 
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  scale_x_continuous(limits = c(2009,2021), breaks = seq(2009,2021, by=2)) +
  scale_y_continuous(limits = c(4000,14900), breaks = seq(5000,14900, by=2500))

Royds_trend_plot

# Inexpressible
Inexpressible_trend_plot <- ggplot(INEXdf, aes(x = Season, y = Guano_area)) + 
  geom_point(colour = "#FFA500") + 
  xlab(element_blank()) +
  ylab(element_blank()) +
  theme_minimal() +
  theme(legend.position = c(1,1)) + 
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  scale_x_continuous(limits = c(2009,2021), breaks = seq(2009,2021, by=2)) +
  scale_y_continuous(limits = c(65000,84900), breaks = seq(65000,84900, by=5000))

Inexpressible_trend_plot

# Plot together

library(ggpubr)

# Combine and add labels

Together <- plot(ggarrange(Crozier_trend_plot, 
          Bird_trend_plot, 
          Royds_trend_plot,
          Inexpressible_trend_plot,
          ncol = 2, nrow = 2, labels=c("a","b","c","d")))
annotate_figure(Together, left = "Guano area (m2)", bottom = "Season")

# Figure S2
