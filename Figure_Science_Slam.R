data_2.1.1.3 <- data_2 %>%
  filter(!is.na(Q46_1N))%>%
  group_by(Q46_1N, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  group_by(Country)%>%
  mutate(share_sum = cumsum(share))%>%
  ungroup()%>%
  # mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany", "Romania")))%>%
  mutate(Q46_1N_label = case_when(Q46_1N == 1 ~ "Strongly\n oppose",
                                  Q46_1N == 2 ~ "Rather\n oppose",
                                  Q46_1N == 3 ~ "Neutral",
                                  Q46_1N == 4 ~ "Rather\n support",
                                  Q46_1N == 5 ~ "Strongly\n support"))%>%
  mutate(Q46_1N_label = factor(Q46_1N_label, levels = c("Neutral", "Rather\n oppose", "Strongly\n oppose", "Rather\n support", "Strongly\n support")))%>%
  mutate(Period = "t=0")%>%
  mutate(share = ifelse(Q46_1N < 3, -share, share))%>%
  mutate(share = ifelse(Q46_1N == 3, share/2, share))%>%
  mutate(side = ifelse(Q46_1N == 3, "right", NA))

data_2.1.1.4 <- data_2.1.1.3 %>%
  bind_rows(mutate(mutate(filter(data_2.1.1.3, Q46_1N == 3), share = -share), side = "left"))%>%
  arrange(Country, Q46_1N)%>%
  filter(Country == "Germany")

P_1 <- ggplot(data_2.1.1.4, aes(x = share, y = fct_rev(Country), fill = fct_rev(Q46_1N_label)))+
  geom_vline(aes(xintercept = 0), linewidth = 0.3)+
  geom_col(position = "stack", colour = "black", width = 0.75, linewidth = 0.3)+
  theme_bw()+
  coord_cartesian(xlim = c(-0.76,0.76))+
  scale_fill_manual(guide = guide_legend(title.position = "top"),
                    values = c("#DC0000FF", "#E64B35FF", "#B09C85FF", "#91D1C2FF", "#00A087FF"),
                    breaks = c("Strongly\n oppose", "Rather\n oppose", "Neutral", "Rather\n support", "Strongly\n support"))+
  labs(fill = "Do you support or oppose the EU ETS2?")+
  xlab("Share of respondents")+
  guides(fill = "none")+
  # ggtitle("Overall policy support (Q46_1 and Q46_2)")+
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x  = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5, size = 8),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))

P_1.1 <- P_1 + coord_cartesian(xlim = c(-10,10))+theme(panel.grid.major.x = element_blank(), axis.ticks.x = element_blank())
P_1.2 <- P_1 + coord_cartesian(xlim = c(-9,9))+theme(panel.grid.major.x = element_blank(), axis.ticks.x = element_blank())
P_1.3 <- P_1 + coord_cartesian(xlim = c(-8,8))+theme(panel.grid.major.x = element_blank(), axis.ticks.x = element_blank())
P_1.4 <- P_1 + coord_cartesian(xlim = c(-7,7))+theme(panel.grid.major.x = element_blank(), axis.ticks.x = element_blank())
P_1.5 <- P_1 + coord_cartesian(xlim = c(-6,6))+theme(panel.grid.major.x = element_blank(), axis.ticks.x = element_blank())
P_1.6 <- P_1 + coord_cartesian(xlim = c(-5,5))+theme(panel.grid.major.x = element_blank(), axis.ticks.x = element_blank())
P_1.7 <- P_1 + coord_cartesian(xlim = c(-4,4))+theme(panel.grid.major.x = element_blank(), axis.ticks.x = element_blank())
P_1.8 <- P_1 + coord_cartesian(xlim = c(-3,3))+theme(panel.grid.major.x = element_blank(), axis.ticks.x = element_blank())
P_1.9a <- P_1 + coord_cartesian(xlim = c(-2.5,2.5))+theme(panel.grid.major.x = element_blank(), axis.ticks.x = element_blank())
P_1.9b <- P_1 + coord_cartesian(xlim = c(-2,2))+theme(panel.grid.major.x = element_blank(), axis.ticks.x = element_blank())
P_1.9c <- P_1 + coord_cartesian(xlim = c(-1.5,1.5))+theme(panel.grid.major.x = element_blank(), axis.ticks.x = element_blank())
P_1.10 <- P_1 + coord_cartesian(xlim = c(-0.76,0.76))+theme(panel.grid.major.x = element_blank(), axis.ticks.x = element_blank())
P_1.11 <- P_1 + coord_cartesian(xlim = c(-0.76,0.76))+
  scale_x_continuous(labels = \(x) scales::percent(abs(x)),
                     breaks = c(-0.75,-0.5,-0.25,0,0.25,0.5,0.75))+theme(axis.text.x = element_text(size = 12))

P_2.1.4 <- ggplot(data_2.1.1.4, aes(x = share, y = fct_rev(Country), fill = fct_rev(Q46_1N_label)))+
  geom_vline(aes(xintercept = 0), linewidth = 0.3)+
  geom_col(position = "stack", colour = "black", width = 0.75, linewidth = 0.3)+
  theme_bw()+
  coord_cartesian(xlim = c(-0.76,0.76))+
  scale_fill_manual(guide = guide_legend(title.position = "top"),
                    values = c("#DC0000FF", "#E64B35FF", "#B09C85FF", "#91D1C2FF", "#00A087FF"),
                    breaks = c("Strongly\n oppose", "Rather\n oppose", "Neutral", "Rather\n support", "Strongly\n support"))+
  labs(fill = "Do you support or oppose the EU ETS2?")+
  scale_x_continuous(labels = \(x) scales::percent(abs(x)),
                     breaks = c(-0.75,-0.5,-0.25,0,0.25,0.5,0.75))+
  xlab("Share of respondents")+
  ylab("Country")+
  # ggtitle("Overall policy support (Q46_1 and Q46_2)")+
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 9),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5, size = 8))

jpeg("C:/Users/leonardm/Desktop/Science Slam/Plots/A_Figure_1_%d.jpeg", width = 190 / 25.4, height = 40 / 25.4, units = "in", res = 300, quality = 100)
print(P_1.1)
print(P_1.2)
print(P_1.3)
print(P_1.4)
print(P_1.5)
print(P_1.6)
print(P_1.7)
print(P_1.8)
print(P_1.9a)
print(P_1.9b)
print(P_1.9c)
print(P_1.10)
print(P_1.11)
dev.off()

jpeg("C:/Users/leonardm/Desktop/Science Slam/Plots/A_Figure_2_%d.jpeg", width = 190 / 25.4, height = 45 / 25.4, units = "in", res = 300, quality = 100)
print(P_1.11)
dev.off()

# Distinction high and low costs ####

data_2.1.1.5 <- data_2 %>%
  filter(Country == "Germany")%>%
  mutate(Group_Relative = binning(Q42_1_relative, bins = 2, method = "quantile", labels = c("Low", "High")))%>%
  filter(!is.na(Group_Relative))%>%
  filter(!is.na(Q46_1N))%>%
  group_by(Q46_1N, Group_Relative, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country, Group_Relative)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  group_by(Country)%>%
  mutate(share_sum = cumsum(share))%>%
  ungroup()%>%
  # mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany", "Romania")))%>%
  mutate(Q46_1N_label = case_when(Q46_1N == 1 ~ "Strongly\n oppose",
                                  Q46_1N == 2 ~ "Rather\n oppose",
                                  Q46_1N == 3 ~ "Neutral",
                                  Q46_1N == 4 ~ "Rather\n support",
                                  Q46_1N == 5 ~ "Strongly\n support"))%>%
  mutate(Q46_1N_label = factor(Q46_1N_label, levels = c("Neutral", "Rather\n oppose", "Strongly\n oppose", "Rather\n support", "Strongly\n support")))%>%
  mutate(Period = "t=0")%>%
  mutate(share = ifelse(Q46_1N < 3, -share, share))%>%
  mutate(share = ifelse(Q46_1N == 3, share/2, share))%>%
  mutate(side = ifelse(Q46_1N == 3, "right", NA))

data_2.1.1.6 <- data_2.1.1.5 %>%
  bind_rows(mutate(mutate(filter(data_2.1.1.5, Q46_1N == 3), share = -share), side = "left"))%>%
  arrange(Country, Q46_1N)%>%
  filter(Country == "Germany")

P_2.1.6 <- ggplot(data_2.1.1.6, aes(x = share, y = as.character(Group_Relative), fill = fct_rev(Q46_1N_label)))+
  geom_vline(aes(xintercept = 0), linewidth = 0.3)+
  geom_col(position = "stack", colour = "black", width = 0.75, linewidth = 0.3)+
  theme_bw()+
  coord_cartesian(xlim = c(-0.76,0.76))+
  scale_fill_manual(guide = guide_legend(title.position = "top"),
                    values = c("#DC0000FF", "#E64B35FF", "#B09C85FF", "#91D1C2FF", "#00A087FF"),
                    breaks = c("Strongly\n oppose", "Rather\n oppose", "Neutral", "Rather\n support", "Strongly\n support"))+
  labs(fill = "Do you support or oppose the EU ETS2?")+
  scale_x_continuous(labels = \(x) scales::percent(abs(x)),
                     breaks = c(-0.75,-0.5,-0.25,0,0.25,0.5,0.75))+
  scale_y_discrete(labels = c("Expects low costs", "Expects high costs"))+
  xlab("Share of respondents")+
  guides(fill = "none")+
  # ggtitle("Overall policy support (Q46_1 and Q46_2)")+
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x  = element_text(size = 14),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5, size = 8),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))

ggsave("C:/Users/leonardm/Desktop/Science Slam/Plots/A_Figure_6.jpeg.png", P_2.1.6, width = 19, height = 6, units = "cm", dpi = 600, bg = "transparent")


# Percentile Figure ####

i <- 66 

  data_plot_1 <- expand_grid(x = seq(1,10,1), y = seq(1,10,1))%>%
    mutate(Position = (y-1)*10+x)%>%
    arrange(Position)%>%
    mutate(Percentile = c(100:1))%>%
    mutate(Interest = ifelse(Percentile == i,1,0),
           Less     = ifelse(Percentile  < i,1,0),
           More     = ifelse(Percentile  > i,1,0))%>%
    mutate(Status = ifelse(Interest == 1, "Sie",
                           # ifelse(Less == 1, "Weniger stark betroffen", "Stärker betroffen")))%>%
                           ifelse(Less == 1, "Haushalte mit niedrigeren Kosten", "Haushalte mit höheren Kosten")))%>%
    mutate(Status_ESP = ifelse(Interest == 1, "Usted",
                               ifelse(Less == 1, "Hogares menos afectados", "Hogares más afectados")))%>%
    mutate(Status_FRA = ifelse(Interest == 1, "Vous",
                               ifelse(Less == 1, "Ménages moins impactés", "Ménages plus impactés")))%>%
    mutate(Status_ROM = ifelse(Interest == 1, "Dumneavoastră", 
                               ifelse(Less == 1, "Familii cu costuri mai mici", "Familii cu costuri mai mari")))%>%
    arrange(y)%>%
    mutate(x = rep(c(10:1),10))%>%
    mutate(Status = factor(Status, levels = c("Sie", "Haushalte mit niedrigeren Kosten", "Haushalte mit höheren Kosten")))%>%
    mutate(Status_ESP = factor(Status_ESP, levels = c("Usted", "Hogares menos afectados", "Hogares más afectados")))%>%
    mutate(Status_FRA = factor(Status_FRA, levels = c("Vous", "Ménages moins impactés", "Ménages plus impactés")))%>%
    mutate(Status_ROM = factor(Status_ROM, levels = c("Dumneavoastră", "Familii cu costuri mai mici", "Familii cu costuri mai mari")))
  
  # With icons
  
  data_plot_3 <- data_plot_1

        P_4 <- ggplot(data_plot_3)+
          annotate("rect", xmin = -1, xmax = 9, ymin = 0.8, ymax = 0.9, fill = "#FFDC91FF", alpha = 0.7, colour = NA)+
          annotate("rect", xmin = 89, xmax = 99, ymin = 0.8, ymax = 0.9, fill = "#BC3C29FF", alpha = 0.7, colour = NA)+
          geom_text(aes(x = 100-Position, y = 1, colour = Status), label = "p", family = "wmpeople1", size = 3)+
          # geom_bracket(xmin = 0, xmax = 59, label = "Weniger stark betroffen", y.position = 1.5)+
          # geom_bracket(xmin = 61, xmax = 100, label = "Stärker betroffen", y.position = 1.5)+
          theme_void()+
          scale_colour_manual(values = c("#0072B5FF","#FFDC91FF","#BC3C29FF"))+
          labs(colour = "")+
          guides(colour = "none")+
          # Left bracket
          annotate("segment", x = -0.75,  xend = i-2.75, y = 1.1, yend = 1.1, linewidth = 0.2) + # top horizontal
          annotate("segment", x = -0.75,  xend = -0.75,  y = 1.1, yend = 1.05, linewidth = 0.2) + # left vertical
          annotate("segment", x = i-2.75, xend = i-2.75, y = 1.1, yend = 1.05, linewidth = 0.2)  + # right vertical
          annotate("text",    x = min(max((i-2.5)/2,13),29),           y = 1.2,  label = "Households with lower costs", size = 2.5)+
          # Right bracket
          annotate("segment", x = i-0.75,  xend = 98.5, y = 1.1, yend = 1.1,  linewidth = 0.2) + # top horizontal
          annotate("segment", x = i-0.75,  xend = i-0.75,  y = 1.1, yend = 1.05, linewidth = 0.2) + # left vertical
          annotate("segment", x = 98.5, xend = 98.5, y = 1.1, yend = 1.05, linewidth = 0.2)  + # right vertical
          annotate("text",    x = min(max((i-2.5)/2+50,63),86),           y = 1.2,  label = "Households with higher costs", size = 2.5)+
          # Central annotation
          annotate("segment", x = i-1.75, xend = i-1.75, y = 0.8, yend = 0.9, linewidth = 0.2)  + # vertical
          annotate("text",    x = i-1.75, y = 0.75,  label = "You", size = 2.5)+
          coord_cartesian(ylim = c(0.7,1.4))+
          # coord_cartesian(ylim = c(0,2))+
          #guides(colour = "none")+
          theme(legend.position = "bottom",
                plot.margin = margin(0,0,0,0),
                legend.text = element_text(size = 9),
                panel.background = element_rect(fill = "transparent", color = NA),
                plot.background = element_rect(fill = "transparent", color = NA),
                legend.background = element_rect(fill = "transparent", color = NA),
                legend.box.background = element_rect(fill = "transparent", color = NA))

        library(showtext)
        showtext_opts(dpi = 600)
        showtext_auto()
        
      ggsave("C:/Users/leonardm/Desktop/Science Slam/Plots/A_Figure_3.jpeg.png", P_4, width = 15, height = 2, units = "cm", dpi = 600, bg = "transparent")
  
      showtext_auto(FALSE)

      a <- "absolute"
      b <- "GER"
      c <- "45"

            
            print(paste0("Price level: ", c))
            if(b == "GER"){data_3.4 <- data_GER_3.4}

            
            if(c == "45"){
              data_3.4 <- data_3.4 %>%
                rename(abs_interest = abs_interest_45,
                       rel_interest = rel_interest_45)
            }
            

            if(a == "absolute"){data_3.4 <- data_3.4 %>%
              mutate(value_0      = abs_interest,
                     Percentile_0 = Percentiles_abs)}

            
            least_0 <- data_3.4 %>%
              filter(Percentile_0 < 11)%>%
              summarise(value_0 = wtd.mean(value_0))%>%
              pull(value_0)
            
            most_0 <- data_3.4 %>%
              filter(Percentile_0 > 89)%>%
              summarise(value_0 = wtd.mean(value_0))%>%
              pull(value_0)
            
            i <- 66
              
            print(i)
              
              individual_0 <- data_3.4 %>%
                filter(Percentile_0 == i)%>%
                summarise(value_0 = wtd.mean(value_0))%>%
                pull(value_0)
              
              
              
              if(b == "GER"){
                data_out <- data.frame("Variable" = c("Hoseholds with lowest costs (10%)", "You", "Households with highest costs (10%)"),
                                       "Measure"  = c(least_0, individual_0, most_0))%>%
                  mutate(Variable = fct_reorder(Variable, Measure))%>%
                  mutate(Colour = case_when(Variable == "Hoseholds with lowest costs (10%)" ~ "#FFDC91FF",
                                            Variable == "Households with highest costs (10%)" ~ "#BC3C29FF",
                                            Variable == "You" ~ "#0072B5FF"))%>%
                  arrange(Variable)
                
                levels(data_out$Variable) <- str_wrap(levels(data_out$Variable), width = 20)
                
                if(a == "absolute") {ylab_0 <- "Additional costs in €"}
                
              }
              

              if(a == "absolute"){
                P_3.4 <- ggplot(data_out, aes(x = Variable, y = Measure, fill = Variable))+
                  geom_col(colour = "black", alpha = 0.7, width = 0.65)+
                  scale_fill_manual(values = data_out$Colour)+
                  #coord_flip()+
                  scale_x_discrete()+
                  guides(fill = "none")+
                  scale_y_continuous(expand = c(0,0), labels = scales::dollar_format(prefix = "€ "))+
                  coord_cartesian(ylim = c(0,max(data_out$Measure + 10)))+
                  theme_bw()+
                  xlab("")+
                  ylab(ylab_0)+
                  theme(axis.text = element_text(size = 7),
                        axis.title.x = element_blank(),
                        axis.title.y = element_text(size = 8),
                        panel.border = element_blank(),
                        panel.background = element_rect(fill = "transparent", color = NA),
                        plot.background = element_rect(fill = "transparent", color = NA),
                        legend.background = element_rect(fill = "transparent", color = NA),
                        legend.box.background = element_rect(fill = "transparent", color = NA))
                
                library(showtext)
                showtext_opts(dpi = 600)
                showtext_auto()
                
                ggsave("C:/Users/leonardm/Desktop/Science Slam/Plots/A_Figure_4.jpeg.png", P_3.4, width = 12, height = 7, units = "cm", dpi = 600, bg = "transparent")
                
                showtext_auto(FALSE)

              }

              # Figure Effects Treatment Interventions ####
              
              
tidy_slam <- tidy_3.8 %>%
                filter(Country == "Germany")%>%
                filter(term == "Post_B_ONLY" | term == "Post_C_ONLY")%>%
                filter(term == "Post_B_ONLY" | VAR == "Policy support" | VAR == "Perception of costs")%>%
                mutate(order = as.character(1:n()))%>%
                mutate(VAR = factor(VAR, levels = c("Perception of costs", "Perception of fairness", "Perception of effectiveness", "Policy support")))%>%
                bind_rows(data.frame(VAR = "Support 2", estimate = 2, ci_low = 2, ci_high = 2, term = "Post_C_ONLY"))%>%
                bind_rows(data.frame(VAR = "Ppercetion of veffectieness", estimate = 3, ci_low = 2, ci_high = 2, term = "Post_C_ONLY"))
              
              tidy_slam_1 <- tidy_slam %>%
                filter(term == "Post_C_ONLY")%>%
                mutate(VAR = factor(VAR, levels = c("Support 2", "Ppercetion of veffectieness", "Policy support", "Perception of costs")))
              
P_3.8.1 <- ggplot(tidy_slam_1, aes(x = estimate, y = VAR))+
  geom_vline(aes(xintercept = 0), linewidth = 0.25)+
    geom_errorbar(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.15, width = 0.2)+
    geom_point(shape = 22, stroke = 0.3, size = 3, fill = "#3C5488FF")+
    expand_limits(x = c(-0.05,0.25))+
  coord_cartesian(xlim = c(-0.05,0.55))+
    theme_bw()+
  scale_y_discrete()+
    xlab("Treatment effect")+
    theme(panel.grid.minor = element_blank(),
          strip.placement = "outside",
          strip.text = element_text(size = 7),
          panel.border = element_rect(color = "black", fill = NA),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linewidth = 0.2),
          axis.ticks = element_line(linewidth = 0.2),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          axis.title.x  = element_text(size = 10),
          axis.title.y = element_blank(),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.box.background = element_rect(fill = "transparent", color = NA))
  
tidy_slam_2 <- tidy_slam %>%
  filter(VAR != "Policy support" & VAR != "Ppercetion of veffectieness")%>%
  mutate(VAR = factor(VAR, levels = c("Support 2", "Perception of effectiveness", "Perception of fairness", "Perception of costs")))


P_3.8.2 <- ggplot(tidy_slam_2, aes(x = estimate, y = VAR))+
  geom_vline(aes(xintercept = 0), linewidth = 0.25)+
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.15, width = 0.2)+
  geom_point(shape = 22, stroke = 0.3, size = 3, fill = "#3C5488FF")+
  expand_limits(x = c(-0.05,0.25))+
  coord_cartesian(xlim = c(-0.05,0.55))+
  theme_bw()+
  scale_y_discrete()+
  xlab("Treatment effect")+
  theme(panel.grid.minor = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 7),
        panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.2),
        axis.ticks = element_line(linewidth = 0.2),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x  = element_text(size = 10),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))
  

tidy_slam_3 <- tidy_slam %>%
  filter(VAR != "Policy support" | term != "Post_C_ONLY")%>%
  filter(VAR != "Support 2" & VAR != "Ppercetion of veffectieness")%>%
  mutate(VAR = factor(VAR, levels = c("Policy support", "Perception of effectiveness", "Perception of fairness", "Perception of costs")))

P_3.8.3 <- ggplot(filter(tidy_slam_3, VAR != "Policy support" | term != "Post_C_ONLY"), aes(x = estimate, y = VAR))+
  geom_vline(aes(xintercept = 0), linewidth = 0.25)+
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.15, width = 0.2)+
  geom_point(shape = 22, stroke = 0.3, size = 3, fill = "#3C5488FF")+
  expand_limits(x = c(-0.05,0.25))+
  coord_cartesian(xlim = c(-0.05,0.55))+
  theme_bw()+
  scale_y_discrete()+
  xlab("Treatment effect")+
  theme(panel.grid.minor = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 7),
        panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.2),
        axis.ticks = element_line(linewidth = 0.2),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x  = element_text(size = 10),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))

ggsave("C:/Users/leonardm/Desktop/Science Slam/Plots/A_Figure_5_1.jpeg.png", P_3.8.1, width = 12, height = 7, units = "cm", dpi = 600, bg = "transparent")
ggsave("C:/Users/leonardm/Desktop/Science Slam/Plots/A_Figure_5_2.jpeg.png", P_3.8.2, width = 12, height = 7, units = "cm", dpi = 600, bg = "transparent")
ggsave("C:/Users/leonardm/Desktop/Science Slam/Plots/A_Figure_5_3.jpeg.png", P_3.8.3, width = 12, height = 7, units = "cm", dpi = 600, bg = "transparent")

