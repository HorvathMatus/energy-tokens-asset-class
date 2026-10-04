rm(list=ls())
library(ggplot2)
library(lubridate)
library(gridExtra)
library(grid)

e_token <- "SNC"
### Select period of data
lim_dates <- c(ymd("2020-12-08"), ymd("2026-01-31"))

################################################################################
### Read data
C <- t(read.csv(paste0("julia/Output/Chorizon_", e_token, ".csv"), header = F, sep=",", dec="."))[,-1]
CI1 <- t(read.csv(paste0("julia/Output/CIhorizon1_", e_token, ".csv"), header = F, sep=",", dec="."))[,-1]
CI2 <- t(read.csv(paste0("julia/Output/CIhorizon2_", e_token, ".csv"), header = F, sep=",", dec="."))[,-1]


# Read returns 
load(paste0("Data/return_", e_token, ".RData"))

################################################################################
###Graph Total Dynamic Network Connectedness
plot_data <- data.frame(ymd(ret[((nrow(ret)-nrow(C)+1):nrow(ret)),1]), 
                        as.numeric(C[,1]), as.numeric(CI1[,1]), as.numeric(CI2[,1]),
                        as.numeric(C[,2]), as.numeric(CI1[,2]), as.numeric(CI2[,2]),
                        as.numeric(C[,3]), as.numeric(CI1[,3]), as.numeric(CI2[,3]),
                        as.numeric(C[,4]), as.numeric(CI1[,4]), as.numeric(CI2[,4]))
colnames(plot_data) <- c("Time", 
                         "DymNet", "CI1", "CI2",
                         "Long", "CI1_long", "CI2_long",
                         "Med", "CI1_Med", "CI2_Med",
                         "Short", "CI1_Short", "CI2_Short")

events <- data.frame(
  xmin  = as.Date(c("2021-01-01", "2022-04-01")),
  xmax  = as.Date(c("2021-04-01", "2022-10-01")),
  event = c("Event 1", "Event 2")
)


# Reshape data for proper legend
plot_data_long <- plot_data %>%
  pivot_longer(
    cols = c(DymNet, Long, Med, Short),
    names_to = "Series",
    values_to = "Value"
  ) %>%
  mutate(Series = case_when(
    Series == "DymNet" ~ "Total connectedness",
    Series == "Short" ~ "1 week (1–5 days)",
    Series == "Med" ~ "1 month (5–20 days)",
    Series == "Long" ~ "Long term (>20 days)",
    TRUE ~ Series
  ))

p_tot <- ggplot(data = plot_data, aes(x = Time)) +
  theme_classic() +
  
  geom_rect(
    data = events,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = event),
    inherit.aes = FALSE,
    alpha = 0.12
  ) +
  scale_fill_manual(values = c("Event 1" = "lightgreen", "Event 2" = "orange")) +
  guides(fill = "none") +
  
  geom_ribbon(aes(ymin = CI1, ymax = CI2), fill = scales::alpha("dimgrey", .2), show.legend = FALSE) +
  geom_ribbon(aes(ymin = CI1_long, ymax = CI2_long), fill = scales::alpha("red", .2), show.legend = FALSE) +
  geom_ribbon(aes(ymin = CI1_Med, ymax = CI2_Med), fill = scales::alpha("blue", .2), show.legend = FALSE) +
  geom_ribbon(aes(ymin = CI1_Short, ymax = CI2_Short), fill = scales::alpha("darkgreen", .2), show.legend = FALSE) +
  
  # lines using the reshaped data
  geom_line(
    data = plot_data_long,
    aes(x = Time, y = Value, color = Series),
    linewidth = 0.8
  ) +
  
  scale_color_manual(
    name = NULL,
    breaks = c("Total connectedness", "1 week (1–5 days)", "1 month (5–20 days)", "Long term (>20 days)"),
    values = c(
      "Total connectedness" = "black",
      "1 week (1–5 days)" = "darkgreen",
      "1 month (5–20 days)" = "blue",
      "Long term (>20 days)" = "red"
    )
  ) +
  ylim(0, 100) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(y = "Dynamic NET", x = "Date") + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.position = "inside",
    legend.position.inside = c(0.98, 1.03),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = scales::alpha("white", 0), color = NA),
    legend.key = element_rect(fill = "transparent", color = NA)
  )

p_tot


ggsave(paste0("Output/Frequencies/Total_", e_token, ".png"), p_tot, bg = "transparent", width = 5000, height = 3000, 
       dpi = 500, units = "px", limitsize = F)




################################################################################
################################# LONG #########################################
###Graphs TO/FROM/NET

# Lists for saving ggplots
p_to   <- list()
p_from <- list()
p_net  <- list()

# Cyclus for generating plots
for(i in 1:(ncol(ret)-1)){
  plot_data_to <- data.frame(ymd(ret[((nrow(ret)-nrow(C)+1):nrow(ret)),1]), 
                             as.numeric(C[,8+0*(ncol(ret)-1)+i-1]), 
                             as.numeric(CI1[,8+0*(ncol(ret)-1)+i-1]), 
                             as.numeric(CI2[,8+0*(ncol(ret)-1)+i-1]))
  plot_data_fr <- data.frame(ymd(ret[((nrow(ret)-nrow(C)+1):nrow(ret)),1]), 
                             as.numeric(C[,8+4*(ncol(ret)-1)+i-1]), 
                             as.numeric(CI1[,8+4*(ncol(ret)-1)+i-1]), 
                             as.numeric(CI2[,8+4*(ncol(ret)-1)+i-1]))
  plot_data_net <- data.frame(ymd(ret[((nrow(ret)-nrow(C)+1):nrow(ret)),1]), 
                              as.numeric(C[,8+16*(ncol(ret)-1)+i-1]), 
                              as.numeric(CI1[,8+16*(ncol(ret)-1)+i-1]), 
                              as.numeric(CI2[,8+16*(ncol(ret)-1)+i-1]))
  
  colnames(plot_data_to) <- c("Time", "DymNet", "CI1", "CI2")
  colnames(plot_data_fr) <- c("Time", "DymNet", "CI1", "CI2")
  colnames(plot_data_net) <- c("Time", "DymNet", "CI1", "CI2")
  
  p_to[[i]] <- ggplot(data=plot_data_to, aes(x = Time, y=DymNet, group = 1))+
    theme_classic()+
    geom_ribbon(aes(ymin = CI1, ymax = CI2), fill = "grey70")+
    geom_line(aes(y=DymNet))+
    #ggtitle(paste0("Directional TO for ", colnames(ret)[i+1]))+
    ylim(0,0.5)+
    scale_x_date(date_breaks = "3 month", date_labels = "%b %Y")+
    labs(y="Dynamic NET", x="Date")+ 
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
  
  p_from[[i]] <- ggplot(data=plot_data_fr, aes(x = Time, y=DymNet, group = 1))+
    theme_classic()+
    geom_ribbon(aes(ymin = CI1, ymax = CI2), fill = "grey70")+
    geom_line(aes(y=DymNet))+
    #ggtitle(paste0("Directional FROM for ", colnames(ret)[i+1]))+
    ylim(0,0.5)+
    scale_x_date(date_breaks = "3 month", date_labels = "%b %Y")+
    labs(y="Dynamic NET", x="Date")+ 
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(angle =45, vjust = 1, hjust = 1, size = 10))
  
  p_net[[i]] <- ggplot(data=plot_data_net, aes(x = Time, y=DymNet, group = 1))+
    theme_classic()+
    geom_ribbon(aes(ymin = CI1, ymax = CI2), fill = "grey70")+
    geom_line(aes(y=DymNet))+
    #ggtitle(paste0("Directional NET for ", colnames(ret)[i+1]))+
    ylim(-0.5,0.5)+
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
                        textGrob(e_token, rot = 90), #BTC
                        p_to[[1]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_from[[1]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[1]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("BTC", rot = 90), #BZ
                        p_to[[2]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[2]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[2]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("ETH", rot = 90), #CL
                        p_to[[3]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[3]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[3]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("SOL", rot = 90), #ES
                        p_to[[4]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[4]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[4]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("AVAX", rot = 90), #EURUSD
                        p_to[[5]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[5]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[5]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("DPI", rot = 90), #GC
                        p_to[[6]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[6]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[6]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("IDEX", rot = 90), #GC
                        p_to[[7]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[7]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[7]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("SPTOP20", rot = 90), #GC
                        p_to[[8]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[8]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[8]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("EUA", rot = 90), #GC
                        p_to[[9]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[9]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[9]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("NEE", rot = 90), #GC
                        p_to[[10]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[10]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[10]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("ENPH", rot = 90), #GC
                        p_to[[11]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[11]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[11]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("PBW", rot = 90), #GC
                        p_to[[12]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[12]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[12]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("PBD", rot = 90), #GC
                        p_to[[13]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[13]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[13]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("FENY", rot = 90), #GC
                        p_to[[14]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[14]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[14]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("QCLN", rot = 90), #GC
                        p_to[[15]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[15]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[15]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("ICLN", rot = 90), #GC
                        p_to[[16]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[16]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[16]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("B", rot = 90), #GC
                        p_to[[17]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[17]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[17]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("CL", rot = 90), #GC
                        p_to[[18]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[18]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[18]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("HH", rot = 90), #GC
                        p_to[[19]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[19]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[19]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("GC", rot = 90), #GC
                        p_to[[20]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[20]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[20]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("HG", rot = 90), #GC
                        p_to[[21]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[21]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[21]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("LIT", rot = 90), #GC
                        p_to[[22]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[22]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[22]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("ESGD", rot = 90), #GC
                        p_to[[23]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[23]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[23]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("ESGU", rot = 90), #GC
                        p_to[[24]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[24]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[24]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("BGRN", rot = 90), #GC
                        p_to[[25]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[25]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[25]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        layout_matrix=as.matrix(layout))

### Save plots
ggsave(paste0("Output/Frequencies/TO_FROM_NET_long_", e_token, ".jpg"), plot_1, width = 9000, height = 36000, 
         dpi = 500, units = "px", limitsize = F)

p_et <- p_net[[1]] + 
  theme_classic() +
  
  # Add event shading
  geom_rect(
    data = events,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = event),
    inherit.aes = FALSE,
    alpha = 0.12
  ) +
  scale_fill_manual(values = c("Event 1" = "lightgreen", "Event 2" = "orange")) +
  guides(fill = "none") +
  
  # Confidence interval and line
  geom_ribbon(aes(ymin = CI1, ymax = CI2), fill = "grey70") +
  geom_line(aes(y = DymNet)) +
  geom_hline(yintercept = 0, col = "red") +
  
  labs(y = "Dynamic NET", x = "Date") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )
ggsave(paste0("Output/Frequencies/TO_FROM_NET_long_", e_token, ".png"), p_et, width = 5000, height = 3000, 
       dpi = 500, units = "px", limitsize = F, bg = "transparent")

################################################################################
################################## MED #########################################
###Graphs TO/FROM/NET

# Lists for saving ggplots
p_to   <- list()
p_from <- list()
p_net  <- list()

# Cyclus for generating plots
for(i in 1:(ncol(ret)-1)){
  plot_data_to <- data.frame(ymd(ret[((nrow(ret)-nrow(C)+1):nrow(ret)),1]), 
                             as.numeric(C[,8+1*(ncol(ret)-1)+i-1]), 
                             as.numeric(CI1[,8+1*(ncol(ret)-1)+i-1]), 
                             as.numeric(CI2[,8+1*(ncol(ret)-1)+i-1]))
  plot_data_fr <- data.frame(ymd(ret[((nrow(ret)-nrow(C)+1):nrow(ret)),1]), 
                             as.numeric(C[,8+5*(ncol(ret)-1)+i-1]), 
                             as.numeric(CI1[,8+5*(ncol(ret)-1)+i-1]), 
                             as.numeric(CI2[,8+5*(ncol(ret)-1)+i-1]))
  plot_data_net <- data.frame(ymd(ret[((nrow(ret)-nrow(C)+1):nrow(ret)),1]), 
                              as.numeric(C[,8+18*(ncol(ret)-1)+i-1]), 
                              as.numeric(CI1[,8+18*(ncol(ret)-1)+i-1]), 
                              as.numeric(CI2[,8+18*(ncol(ret)-1)+i-1]))
  
  colnames(plot_data_to) <- c("Time", "DymNet", "CI1", "CI2")
  colnames(plot_data_fr) <- c("Time", "DymNet", "CI1", "CI2")
  colnames(plot_data_net) <- c("Time", "DymNet", "CI1", "CI2")
  
  p_to[[i]] <- ggplot(data=plot_data_to, aes(x = Time, y=DymNet, group = 1))+
    theme_classic()+
    geom_ribbon(aes(ymin = CI1, ymax = CI2), fill = "grey70")+
    geom_line(aes(y=DymNet))+
    #ggtitle(paste0("Directional TO for ", colnames(ret)[i+1]))+
    ylim(0,0.5)+
    scale_x_date(date_breaks = "3 month", date_labels = "%b %Y")+
    labs(y="Dynamic NET", x="Date")+ 
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
  
  p_from[[i]] <- ggplot(data=plot_data_fr, aes(x = Time, y=DymNet, group = 1))+
    theme_classic()+
    geom_ribbon(aes(ymin = CI1, ymax = CI2), fill = "grey70")+
    geom_line(aes(y=DymNet))+
    #ggtitle(paste0("Directional FROM for ", colnames(ret)[i+1]))+
    ylim(0,0.5)+
    scale_x_date(date_breaks = "3 month", date_labels = "%b %Y")+
    labs(y="Dynamic NET", x="Date")+ 
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
  
  p_net[[i]] <- ggplot(data=plot_data_net, aes(x = Time, y=DymNet, group = 1))+
    theme_classic()+
    geom_ribbon(aes(ymin = CI1, ymax = CI2), fill = "grey70")+
    geom_line(aes(y=DymNet))+
    #ggtitle(paste0("Directional NET for ", colnames(ret)[i+1]))+
    ylim(-0.5,0.5)+
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
                        textGrob(e_token, rot = 90), #BTC
                        p_to[[1]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_from[[1]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[1]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("BTC", rot = 90), #BZ
                        p_to[[2]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[2]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[2]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("ETH", rot = 90), #CL
                        p_to[[3]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[3]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[3]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("SOL", rot = 90), #ES
                        p_to[[4]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[4]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[4]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("AVAX", rot = 90), #EURUSD
                        p_to[[5]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[5]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[5]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("DPI", rot = 90), #GC
                        p_to[[6]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[6]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[6]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("IDEX", rot = 90), #GC
                        p_to[[7]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[7]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[7]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("SPTOP20", rot = 90), #GC
                        p_to[[8]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[8]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[8]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("EUA", rot = 90), #GC
                        p_to[[9]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[9]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[9]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("NEE", rot = 90), #GC
                        p_to[[10]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[10]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[10]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("ENPH", rot = 90), #GC
                        p_to[[11]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[11]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[11]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("PBW", rot = 90), #GC
                        p_to[[12]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[12]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[12]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("PBD", rot = 90), #GC
                        p_to[[13]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[13]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[13]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("FENY", rot = 90), #GC
                        p_to[[14]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[14]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[14]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("QCLN", rot = 90), #GC
                        p_to[[15]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[15]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[15]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("ICLN", rot = 90), #GC
                        p_to[[16]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[16]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[16]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("B", rot = 90), #GC
                        p_to[[17]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[17]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[17]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("CL", rot = 90), #GC
                        p_to[[18]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[18]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[18]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("HH", rot = 90), #GC
                        p_to[[19]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[19]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[19]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("GC", rot = 90), #GC
                        p_to[[20]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[20]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[20]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("HG", rot = 90), #GC
                        p_to[[21]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[21]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[21]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("LIT", rot = 90), #GC
                        p_to[[22]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[22]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[22]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("ESGD", rot = 90), #GC
                        p_to[[23]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[23]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[23]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("ESGU", rot = 90), #GC
                        p_to[[24]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[24]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[24]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("BGRN", rot = 90), #GC
                        p_to[[25]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[25]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[25]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        layout_matrix=as.matrix(layout))

### Save plots
ggsave(paste0("Output/Frequencies/TO_FROM_NET_medium_", e_token, ".jpg"), plot_1, width = 9000, height = 36000, 
       dpi = 500, units = "px", limitsize = F)

p_et <- p_net[[1]] + 
  theme_classic() +
  
  # Add event shading
  geom_rect(
    data = events,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = event),
    inherit.aes = FALSE,
    alpha = 0.12
  ) +
  scale_fill_manual(values = c("Event 1" = "lightgreen", "Event 2" = "orange")) +
  guides(fill = "none") +
  
  # Confidence interval and line
  geom_ribbon(aes(ymin = CI1, ymax = CI2), fill = "grey70") +
  geom_line(aes(y = DymNet)) +
  geom_hline(yintercept = 0, col = "red") +
  
  labs(y = "Dynamic NET", x = "Date") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )
ggsave(paste0("Output/Frequencies/TO_FROM_NET_medium_", e_token, ".png"), p_et, width = 5000, height = 3000, 
       dpi = 500, units = "px", limitsize = F, bg = "transparent")

################################################################################
################################## SHORT #######################################
###Graphs TO/FROM/NET

# Lists for saving ggplots
p_to   <- list()
p_from <- list()
p_net  <- list()

# Cyclus for generating plots
for(i in 1:(ncol(ret)-1)){
  plot_data_to <- data.frame(ymd(ret[((nrow(ret)-nrow(C)+1):nrow(ret)),1]), 
                             as.numeric(C[,8+2*(ncol(ret)-1)+i-1]), 
                             as.numeric(CI1[,8+2*(ncol(ret)-1)+i-1]), 
                             as.numeric(CI2[,8+2*(ncol(ret)-1)+i-1]))
  plot_data_fr <- data.frame(ymd(ret[((nrow(ret)-nrow(C)+1):nrow(ret)),1]), 
                             as.numeric(C[,8+6*(ncol(ret)-1)+i-1]), 
                             as.numeric(CI1[,8+6*(ncol(ret)-1)+i-1]), 
                             as.numeric(CI2[,8+6*(ncol(ret)-1)+i-1]))
  plot_data_net <- data.frame(ymd(ret[((nrow(ret)-nrow(C)+1):nrow(ret)),1]), 
                              as.numeric(C[,8+19*(ncol(ret)-1)+i-1]), 
                              as.numeric(CI1[,8+19*(ncol(ret)-1)+i-1]), 
                              as.numeric(CI2[,8+19*(ncol(ret)-1)+i-1]))
  
  colnames(plot_data_to) <- c("Time", "DymNet", "CI1", "CI2")
  colnames(plot_data_fr) <- c("Time", "DymNet", "CI1", "CI2")
  colnames(plot_data_net) <- c("Time", "DymNet", "CI1", "CI2")
  
  p_to[[i]] <- ggplot(data=plot_data_to, aes(x = Time, y=DymNet, group = 1))+
    theme_classic()+
    geom_ribbon(aes(ymin = CI1, ymax = CI2), fill = "grey70")+
    geom_line(aes(y=DymNet))+
    #ggtitle(paste0("Directional TO for ", colnames(ret)[i+1]))+
    ylim(0,1)+
    scale_x_date(date_breaks = "3 month", date_labels = "%b %Y")+
    labs(y="Dynamic NET", x="Date")+ 
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
  
  p_from[[i]] <- ggplot(data=plot_data_fr, aes(x = Time, y=DymNet, group = 1))+
    theme_classic()+
    geom_ribbon(aes(ymin = CI1, ymax = CI2), fill = "grey70")+
    geom_line(aes(y=DymNet))+
    #ggtitle(paste0("Directional FROM for ", colnames(ret)[i+1]))+
    ylim(0,1)+
    scale_x_date(date_breaks = "3 month", date_labels = "%b %Y")+
    labs(y="Dynamic NET", x="Date")+ 
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
  
  p_net[[i]] <- ggplot(data=plot_data_net, aes(x = Time, y=DymNet, group = 1))+
    theme_classic()+
    geom_ribbon(aes(ymin = CI1, ymax = CI2), fill = "grey70")+
    geom_line(aes(y=DymNet))+
    #ggtitle(paste0("Directional NET for ", colnames(ret)[i+1]))+
    ylim(-0.5,0.5)+
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
                        textGrob(e_token, rot = 90), #BTC
                        p_to[[1]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_from[[1]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[1]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("BTC", rot = 90), #BZ
                        p_to[[2]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[2]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[2]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("ETH", rot = 90), #CL
                        p_to[[3]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[3]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[3]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("SOL", rot = 90), #ES
                        p_to[[4]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[4]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[4]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("AVAX", rot = 90), #EURUSD
                        p_to[[5]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[5]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[5]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("DPI", rot = 90), #GC
                        p_to[[6]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[6]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[6]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("IDEX", rot = 90), #GC
                        p_to[[7]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[7]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[7]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("SPTOP20", rot = 90), #GC
                        p_to[[8]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[8]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[8]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("EUA", rot = 90), #GC
                        p_to[[9]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[9]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[9]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("NEE", rot = 90), #GC
                        p_to[[10]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[10]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[10]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("ENPH", rot = 90), #GC
                        p_to[[11]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[11]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[11]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("PBW", rot = 90), #GC
                        p_to[[12]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[12]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[12]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("PBD", rot = 90), #GC
                        p_to[[13]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[13]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[13]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("FENY", rot = 90), #GC
                        p_to[[14]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[14]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[14]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("QCLN", rot = 90), #GC
                        p_to[[15]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[15]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[15]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("ICLN", rot = 90), #GC
                        p_to[[16]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[16]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[16]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("B", rot = 90), #GC
                        p_to[[17]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[17]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[17]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("CL", rot = 90), #GC
                        p_to[[18]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[18]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[18]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("HH", rot = 90), #GC
                        p_to[[19]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[19]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[19]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("GC", rot = 90), #GC
                        p_to[[20]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[20]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[20]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("HG", rot = 90), #GC
                        p_to[[21]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[21]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[21]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("LIT", rot = 90), #GC
                        p_to[[22]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[22]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[22]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("ESGD", rot = 90), #GC
                        p_to[[23]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[23]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[23]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("ESGU", rot = 90), #GC
                        p_to[[24]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[24]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[24]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        textGrob("BGRN", rot = 90), #GC
                        p_to[[25]] + theme(plot.background = element_rect(color="black", linewidth=1)), 
                        p_from[[25]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        p_net[[25]] + theme(plot.background = element_rect(color="black", linewidth=1)),
                        layout_matrix=as.matrix(layout))

### Save plots
ggsave(paste0("Output/Frequencies/TO_FROM_NET_short_", e_token, ".jpg"), plot_1, width = 9000, height = 36000, 
       dpi = 500, units = "px", limitsize = F)

p_et <- p_net[[1]] + 
  theme_classic() +
  
  # Add event shading
  geom_rect(
    data = events,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = event),
    inherit.aes = FALSE,
    alpha = 0.12
  ) +
  scale_fill_manual(values = c("Event 1" = "lightgreen", "Event 2" = "orange")) +
  guides(fill = "none") +
  
  # Confidence interval and line
  geom_ribbon(aes(ymin = CI1, ymax = CI2), fill = "grey70") +
  geom_line(aes(y = DymNet)) +
  geom_hline(yintercept = 0, col = "red") +
  
  labs(y = "Dynamic NET", x = "Date") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )
ggsave(paste0("Output/Frequencies/TO_FROM_NET_short_", e_token, ".png"), p_et, width = 5000, height = 3000, 
       dpi = 500, units = "px", limitsize = F, bg = "transparent")


