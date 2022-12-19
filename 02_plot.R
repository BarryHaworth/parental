#  First plots of Blood data

library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape2)

PROJECT_DIR <- '/home/ruca78/work/blood/'
DATA_DIR    <- paste0(PROJECT_DIR,'data/')
PLOT_DIR    <- paste0(PROJECT_DIR,'plots/')

load(paste0(DATA_DIR,"total.RData"))
load(paste0(DATA_DIR,"state.RData"))
load(paste0(DATA_DIR,"per_person.RData"))
load(paste0(DATA_DIR,"division.RData"))

total_melt <- melt(total %>% select(-c(year,month)),id.vars = "date") %>% rename(type=variable,donations=value)

png(paste0(PLOT_DIR,"total_type.png"),width=800,height=800)
ggplot(total_melt, aes(x=date, y=donations, color=type)) + 
  geom_line(size=1) +
  ggtitle("ATO Donations by type") +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Date")+ylab("Donations") + labs(color="Type")+ ylim(0,NA)+
  theme(legend.position = "bottom")
dev.off()

png(paste0(PLOT_DIR,"state_donations.png"),width=800,height=800)
ggplot(state, aes(x=date, y=total,color=state)) + 
  geom_line(size=1) +
  ggtitle("ATO Total Donations by State") +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Date")+ylab("Donations") + labs(color="State")+  ylim(0,NA)+
  theme(legend.position = "bottom")
dev.off()

png(paste0(PLOT_DIR,"per_person_donations_all.png"),width=800,height=800)
ggplot(per_person, aes(x=date, y=total,color=state)) + 
  geom_line() +
  ggtitle("ATO Donations per 1000 by State") +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Date")+ylab("Donations") + labs(color="State")+  ylim(0,NA)+
  theme(legend.position = "bottom")
dev.off()

png(paste0(PLOT_DIR,"per_person_donations.png"),width=800,height=800)
ggplot(per_person %>% filter(state !="NT"), aes(x=date, y=total,color=state)) + 
  geom_line(size=1) +
  ggtitle("ATO Donations per 1000 by State (excl NT)") +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Date")+ylab("Donations") + labs(color="State")+  ylim(0,NA)+
  theme(legend.position = "bottom")
dev.off()

# Plots by Division
png(paste0(PLOT_DIR,"nsw_division.png"),width=800,height=800)
ggplot(division %>% filter(state=='NSW'),  aes(x=date, y=total, color=factor(division))) +
  facet_wrap(state~.,scales="free")+
  geom_line(size=1) +
  ggtitle("Total Donations by Division, NSW") +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("date")+ylab("Donations") + labs(color="Year")+  ylim(0,NA)+
  theme(legend.position = "bottom")
dev.off()

png(paste0(PLOT_DIR,"vic_division.png"),width=800,height=800)
ggplot(division %>% filter(state=='VIC'),  aes(x=date, y=total, color=factor(division))) +
  facet_wrap(state~.,scales="free")+
  geom_line(size=1) +
  ggtitle("Total Donations by Division, VIC") +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("date")+ylab("Donations") + labs(color="Year")+ ylim(0,NA)+
  theme(legend.position = "bottom")
dev.off()

png(paste0(PLOT_DIR,"qld_division.png"),width=800,height=800)
ggplot(division %>% filter(state=='QLD'),  aes(x=date, y=total, color=factor(division))) +
  facet_wrap(state~.,scales="free")+
  geom_line(size=1) +
  ggtitle("Total Donations by Division, QLD") +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("date")+ylab("Donations") + labs(color="Year")+ ylim(0,NA)+
  theme(legend.position = "bottom")
dev.off()

png(paste0(PLOT_DIR,"sa_division.png"),width=800,height=800)
ggplot(division %>% filter(state=='SA'),  aes(x=date, y=total, color=factor(division))) +
  facet_wrap(state~.,scales="free")+
  geom_line(size=1) +
  ggtitle("Total Donations by Division, SA") +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("date")+ylab("Donations") + labs(color="Year")+ ylim(0,NA)+
  theme(legend.position = "bottom")
dev.off()

png(paste0(PLOT_DIR,"wa_division.png"),width=800,height=800)
ggplot(division %>% filter(state=='WA'),  aes(x=date, y=total, color=factor(division))) +
  facet_wrap(state~.,scales="free")+
  geom_line(size=1) +
  ggtitle("Total Donations by Division, WA") +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("date")+ylab("Donations") + labs(color="Year")+
  theme(legend.position = "bottom")
dev.off()

png(paste0(PLOT_DIR,"tas_division.png"),width=800,height=800)
ggplot(division %>% filter(state=='TAS'),  aes(x=date, y=total, color=factor(division))) +
  facet_wrap(state~.,scales="free")+
  geom_line(size=1) +
  ggtitle("Total Donations by Division, TAS") +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("date")+ylab("Donations") + labs(color="Year")+ ylim(0,NA)+
  theme(legend.position = "bottom")
dev.off()


# Year on Year plots
png(paste0(PLOT_DIR,"total_yoy_donations.png"),width=800,height=800)
ggplot(total,  aes(x=month, y=total, color=factor(year), group=year)) +
  geom_line(size=1) +
  ggtitle("ATO Year Total Donations Year on Year") +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Month")+ylab("Donations") + labs(color="Year")+ ylim(0,NA)+
  theme(legend.position = "bottom")
dev.off()

png(paste0(PLOT_DIR,"state_total_yoy_donations.png"),width=1600,height=1600)
ggplot(state,  aes(x=month, y=total, color=factor(year), group=year)) +
  facet_wrap(state~.,scales="free")+
  geom_line(size=1) +
  ggtitle("State Total Donations Year on Year") +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Month")+ylab("Donations") + labs(color="Year")+ ylim(0,NA)+
  theme(legend.position = "bottom")
dev.off()

# Plots by State - YOY, Type
state_names =c("ACT","NSW","NT","QLD","SA","TAS","VIC","WA")

for (name in state_names){
  single_state <- state %>% 
    filter(state==name) %>%        # Filter on state
    filter(year >= year(now())-3)  # Keep last three years only
  
  print(paste(name,"YOY donations plot"))
  single_state <- single_state %>%
    filter(year >= year(now())-3)  # Keep last three years only
  
  png(paste0(PLOT_DIR,name,"_yoy.png"),width=800,height=800)
  print(ggplot(single_state,  aes(x=month, y=total, color=factor(year), group=year)) +
    geom_line(size=1) +
      geom_point() +
      ggtitle(paste(name,"Donations Year on Year")) +
    theme(plot.title=element_text(hjust=0.5))+
    xlab("Month")+ylab("Donations") + labs(color="Year")+ ylim(0,NA)+
    theme(legend.position = "bottom"))
  dev.off()
  
  print(paste(name,"donations by type plot"))
  state_melt <- melt(state %>% filter(state==name) %>% select(-c(state,year,month)),id.vars = "date") %>% rename(type=variable,donations=value)
  
  png(paste0(PLOT_DIR,name,"_type.png"),width=800,height=800)
  print(ggplot(state_melt, aes(x=date, y=donations, color=type)) + 
    geom_line(size=1) +
      geom_point() +
      ggtitle(paste(name,"Donations by type")) +
    theme(plot.title=element_text(hjust=0.5))+
    xlab("Date")+ylab("Donations") + labs(color="Type")+ ylim(0,NA)+
    theme(legend.position = "bottom"))
  dev.off()
  
  }

