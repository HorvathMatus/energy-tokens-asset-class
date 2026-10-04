rm(list=ls())
library(ggplot2)
library(lubridate)
library(gridExtra)
library(grid)

### Select period of data
lim_dates <- c(ymd("2020-12-08"), ymd("2026-01-31"))

################################################################################
### Read data
C <- t(read.csv("julia/Output/C.csv", header = F, sep=",", dec="."))[,-1]
CI1 <- t(read.csv("julia/Output/CI1.csv", header = F, sep=",", dec="."))[,-1]
CI2 <- t(read.csv("julia/Output/CI2.csv", header = F, sep=",", dec="."))[,-1]


# Read returns 
load("Data/return.RData")


################################################################################
###Graph Total Dynamic Network Connectedness
plot_data <- data.frame(ymd(ret[((nrow(ret)-nrow(C)+1):nrow(ret)),1]), as.numeric(C[,1]), as.numeric(CI1[,1]), as.numeric(CI2[,1]))
colnames(plot_data) <- c("Time", "DymNet", "CI1", "CI2")

p_tot <- ggplot(data =plot_data, aes(x = Time, y=DymNet, group = 1))+
  theme_classic()+
  geom_ribbon(aes(ymin = CI1, ymax = CI2), fill = "grey70")+
  geom_line(aes(y=DymNet))+
  ggtitle("Total Dynamic Network Connectedness")+
  ylim(0,100)+
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y")+
  labs(y="Dynamic NET", x="Date")+ 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
p_tot


ggsave("Output/Total.jpg", p_tot, width = 5000, height = 3000, 
        dpi = 500, units = "px", limitsize = F)

################################################################################
###Graphs TO/FROM/NET

# Lists for saving ggplots
p_to   <- list()
p_from <- list()
p_net  <- list()

# Cyclus for generating plots
for(i in 1:(ncol(ret)-1)){
  plot_data_to <- data.frame(ymd(ret[((nrow(ret)-nrow(C)+1):nrow(ret)),1]), as.numeric(C[,i+1]), as.numeric(CI1[,i+1]), as.numeric(CI2[,i+1]))
  plot_data_fr <- data.frame(ymd(ret[((nrow(ret)-nrow(C)+1):nrow(ret)),1]), as.numeric(C[,2+1*(ncol(ret)-1)+i-1]), as.numeric(CI1[,2+1*(ncol(ret)-1)+i-1]), as.numeric(CI2[,2+1*(ncol(ret)-1)+i-1]))
  plot_data_net <- data.frame(ymd(ret[((nrow(ret)-nrow(C)+1):nrow(ret)),1]), as.numeric(C[,2+4*(ncol(ret)-1)+i-1]), as.numeric(CI1[,2+4*(ncol(ret)-1)+i-1]), as.numeric(CI2[,2+4*(ncol(ret)-1)+i-1]))
  
  colnames(plot_data_to) <- c("Time", "DymNet", "CI1", "CI2")
  colnames(plot_data_fr) <- c("Time", "DymNet", "CI1", "CI2")
  colnames(plot_data_net) <- c("Time", "DymNet", "CI1", "CI2")
  
  p_to[[i]] <- ggplot(data=plot_data_to, aes(x = Time, y=DymNet, group = 1))+
    theme_classic()+
    geom_ribbon(aes(ymin = CI1, ymax = CI2), fill = "grey70")+
    geom_line(aes(y=DymNet))+
    #ggtitle(paste0("Directional TO for ", colnames(ret)[i+1]))+
    ylim(0,15)+
    scale_x_date(date_breaks = "3 month", date_labels = "%b %Y")+
    labs(y="Dynamic NET", x="Date")+ 
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
  
  p_from[[i]] <- ggplot(data=plot_data_fr, aes(x = Time, y=DymNet, group = 1))+
    theme_classic()+
    geom_ribbon(aes(ymin = CI1, ymax = CI2), fill = "grey70")+
    geom_line(aes(y=DymNet))+
    #ggtitle(paste0("Directional FROM for ", colnames(ret)[i+1]))+
    ylim(0,15)+
    scale_x_date(date_breaks = "3 month", date_labels = "%b %Y")+
    labs(y="Dynamic NET", x="Date")+ 
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
  
  p_net[[i]] <- ggplot(data=plot_data_net, aes(x = Time, y=DymNet, group = 1))+
    theme_classic()+
    geom_ribbon(aes(ymin = CI1, ymax = CI2), fill = "grey70")+
    geom_line(aes(y=DymNet))+
    #ggtitle(paste0("Directional NET for ", colnames(ret)[i+1]))+
    ylim(-5,5)+
    geom_hline(yintercept = 0, col = "red")+
    scale_x_date(date_breaks = "3 month", date_labels = "%b %Y")+
    labs(y="Dynamic NET", x="Date")+ 
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
}


### Layout for plots
layout <- c(NA, rep(1,3), rep(2,3), rep(3,3))
for (i in 2:(ncol(ret))){
  layout <- rbind(layout, 
                  c( (i-1)*4, rep((i-1)*4+1,3), rep((i-1)*4+2,3), rep((i-1)*4+3,3)),
                  c( (i-1)*4, rep((i-1)*4+1,3), rep((i-1)*4+2,3), rep((i-1)*4+3,3)),
                  c( (i-1)*4, rep((i-1)*4+1,3), rep((i-1)*4+2,3), rep((i-1)*4+3,3)))
}

plot_1 <-  grid.arrange(textGrob("Directional TO"),     #1
                        textGrob("Directional FROM"),   #2
                        textGrob("Directional NET"),    #3
                        textGrob("POWR", rot = 90), #BTC
                        p_to[[1]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_from[[1]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[1]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("WPR", rot = 90), #BZ
                        p_to[[2]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[2]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[2]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("EWT", rot = 90), #CL
                        p_to[[3]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[3]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[3]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("WOZX", rot = 90), #ES
                        p_to[[4]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[4]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[4]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("SNC", rot = 90), #EURUSD
                        p_to[[5]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[5]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[5]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("SLR", rot = 90), #GC
                        p_to[[6]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[6]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[6]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        layout_matrix=as.matrix(layout))

### Save plots
ggsave("Output/TO_FROM_NET.jpg", plot_1, width = 9000, height = 12000, 
         dpi = 500, units = "px", limitsize = F)

