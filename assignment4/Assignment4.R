library("readr")
library("dplyr")
library("data.table")
library(ggplot2)

#Set working directory
working_directory <- setwd("C:/Users/latera/Desktop/HMS520_Class_Automn2022/assignment4")
# specify which package to use
csv_loader <- fread

# load data
counts <- csv_loader("counts.csv")
head(counts)

counts <- rename(counts,
                 "mortality rate" = mr,
                 "infection fatality rate" = ifr)
head(counts)

#Plot everything together
ggplot(counts, aes(x = date, y = `mortality rate`, col=state)) +
  geom_point() + 
  xlab("date") + ylab("Mortality Rate")+
  ggtitle("Mortality rate")+
  theme(legend.position = "none")

#Plot faceted over states
ggplot(counts, aes(x = date, y = `mortality rate`, col=state)) +
  geom_point() + 
  facet_wrap(~state)+
  xlab("date") + ylab("Mortality Rate")+
  ggtitle("Mortality rate")+
  theme(text = element_text(size = 11),legend.position = "none")

#Plot faceted over states, number of rows 10,
ggplot(counts, aes(x = date, y = `mortality rate`, col=state)) +
  geom_point() + 
  facet_wrap(~state,nrow=10)+
  xlab("date") + ylab("Mortality Rate")+
  ggtitle("Mortality rate for all the states")+
  theme(text = element_text(size = 11),legend.position = "none")

#Plot log transformed
plot_ob<-ggplot(counts, aes(x = date, y = `mortality rate`, col=state)) +
  geom_point() + 
  facet_wrap(~state,nrow=10)+
  scale_y_log10()+
  xlab("date") + ylab("Mortality Rate")+
  ggtitle("Mortality rate")+
  theme(text = element_text(size = 11),legend.position = "none")
plot_ob
#Save the image
ggsave("mr_by_state_date.png", 
       plot = plot_ob,
       width = 10, height = 20)


#Plot everything together - boxplot
ggplot(counts, aes(x = state, y = `mortality rate`, col=state)) +
  geom_boxplot() + 
  xlab("State") + ylab("Mortality Rate")+
  ggtitle("Mortality rate")+
  theme(axis.text.x = element_text(size = 8, angle = 90),legend.position = "none")


#Plot everything together - boxplot - reorder
ggplot(counts, aes(x=reorder(state, -`mortality rate`), y = `mortality rate`, col=state)) +
  geom_boxplot() + 
  xlab("State") + ylab("Mortality Rate")+
  ggtitle("Mortality rate")+
  theme(axis.text.x = element_text(size = 8, angle = 90),legend.position = "none")

#Plot everything together - boxplot - log-plot
plot_ob<-ggplot(counts, aes(x=reorder(state, `mortality rate`), y = `mortality rate`, col=state)) +
  geom_boxplot() + 
  scale_y_log10()+
  xlab("State") + ylab("Mortality Rate")+
  ggtitle("Mortality rate")+
  theme(axis.text.x = element_text(size = 8, angle = 90),legend.position = "none")
plot_ob
ggsave("mr_by_state_state.png", 
       plot = plot_ob,
       width = 8, height = 15)


#Reload the data
csv_loader <- fread
counts <- csv_loader("counts.csv")
head(counts)
counts <- rename(counts,
                 "mortality rate" = mr,
                 "infection fatality rate" = ifr)
#Aggregating the mean
mean_counts <- counts[, lapply(.SD, mean), by = state, .SDcols = c("infection fatality rate","mortality rate")]
mean_counts
mean_counts_long <- melt(
  mean_counts,
  id.vars = "state",
  measure.vars = c("infection fatality rate","mortality rate"),
  variable.name = "Type",
  value.name = "Value",
  variable.factor = FALSE
)
mean_counts_long
#Plotting using two plates
ggplot(mean_counts_long, aes(x=Value, fill=state)) +
  geom_histogram() + 
  facet_wrap(~Type)+
  xlab("Rates") +
  ggtitle("Both plot")+
  theme(axis.text.x = element_text(size = 8),legend.position = "none")

#Plotting using two plates - x-axis log transformed
plot_ob<-ggplot(mean_counts_long, aes(x=Value, fill=state)) +
  geom_histogram() + 
  facet_wrap(~Type)+
  scale_x_log10()+
  xlab("Rates") +
  ggtitle("Both plot")+
  theme(axis.text.x = element_text(size = 8),legend.position = "none")
plot_ob
#Save to image
ggsave("mr_and_ifr.png", 
       plot = plot_ob,
       width = 8, height = 8)
