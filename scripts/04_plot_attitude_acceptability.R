library(tidyverse)
library(ggplot2)
library(cowplot) #ggdraw, draw_plot, draw_label functions

# reading in data and preparing dataframe for model
dat<-read_csv("data/processed/gbdat_CompositeDummyVars4.csv")
dat2<-dat[(dat$resident==1),]%>% #removes part time residents
    dplyr::select(belief_mt, satis,emotions, hunting_norm_beliefs, trust, proc_justice,
                  imposed, natural, knowl_gb, edu, age, tenure_new, attitude_likert,tenure_new3,
                  exp1_afar, exp3_closehome, exp4_damage, exp5_knowppldamage, exp6_fearsafety,
                  ag_profit, outdoor_ind, hunter, male, urban,
                  years_current_address, acres,
                  weight) #only include variables of interest
dat3<-na.omit(dat2) #removing non-response rows
dat3$weight_norm<-dat3$weight/(mean(dat3$weight)) #calculates the normalized weight, mean of weight_norm should be 1 and sum should be number of cases
dat3<-dat3%>%dplyr::select(-weight) #removing the non-normalized weight
dat3$log_years_current_address<-log(dat3$years_current_address) #transforming years at current address -- order of magnitude difference causing heteroscedasticity
dat3$log_acres<-log(dat3$acres+1) #transforming acres -- order of magnitude difference causing heteroscedasticity

# creating new columns for figure symbols/colours
dat3<-dat3%>%
    mutate(color_satis1=case_when(
        satis==1~"Very unsatisfied",
        satis==2~"Unsatisfied",
        satis==3~"Neither",
        satis==4~"Satisfied",
        satis==5~"Very satisfied"))%>%
    mutate(color_satis2 = factor(dat3$color_satis1,
                                 levels=c("Very unsatisfied", "Unsatisfied", "Neither", "Satisfied", "Very satisfied"), #need to create levels for vector to plot in the right order
                                 labels=c("Very unsatisfied", "Unsatisfied", "Neither", "Satisfied", "Very satisfied")))
print(dat3%>%dplyr::select(satis, color_satis1, color_satis2), n=30) #checking to make sure new columns are correct

# attitude, acceptance (belief_mt), satisfaction figure
RColorBrewer::display.brewer.pal(11, "RdBu") #finding some good colors
RColorBrewer::brewer.pal(11, "RdBu")[c(1,2,4,9,11)] #choosing the ones I like and getting the code for them

## creates main plot
p1<-ggplot(dat3,aes(x=belief_mt, y=attitude_likert, weight=weight_norm, 
                    color=color_satis2, fill=color_satis2))+
    geom_point(alpha = 0.7, position = position_jitter(width=0.3, height=0, seed=1), size=1.5)+
    scale_color_manual(values=c("#67001F", "#B2182B" ,"#F4A582" ,"#4393C3" ,"#053061"))+
    scale_fill_manual(values=c("#67001F50", "#B2182B50" ,"#F4A58250" ,"#4393C350" ,"#05306150"))+
    xlab("Normative population size belief")+
    scale_y_continuous(breaks=c(1:5), name = "Attitude towards grizzlies")+
    scale_x_reverse()+
    theme_classic(base_size=12)+
    theme(axis.text=element_text(color="black"))+
    labs(color="Satisfaction with\ngrizzly management")+
    guides(color = guide_legend(order = 1),
           fill="none")+
    coord_cartesian(ylim = c(0.5, 5.5), xlim = c(5.5, 0.5))+
    geom_vline(xintercept = 3, linetype="dotted", colour="grey20")+
    geom_hline(yintercept = 3, linetype="dotted", colour="grey20")+
    annotate("text", x = .5, y = .5, label="Tolerant", hjust=1, size=3, fontface=2)+
    annotate("text", x = 5.5, y = .5, label="Intolerant", hjust=0, size=3, fontface=2)+
    annotate("text", x = .5, y = 5.5, label="Enthusiastic", hjust=1, size=3, fontface=2)+
    annotate("text", x = 5.5, y = 5.5, label="Pragmatic", hjust=0, size=3, fontface=2)+
    annotate("text", x = 3, y = 3, label="Indifferent", size=3, fontface=2)

## creates horizontal arrows
arrow_p <- 
    ggplot(dat3) +
    geom_segment(aes(x = 1.2, y = 0.5, xend = 4, yend = 0.5),
                 col = "black",
                 arrow = arrow(length=unit(.3, "cm"))) +
    coord_cartesian(ylim = c(0.5,5.5), xlim = c(0.5, 5.5)) +
    theme_void()

arrow_p2 <- 
    ggplot(dat3) +
    geom_segment(aes(x = 4, y = 0.5, xend = 1.2, yend = 0.5),
                 col = "black",
                 arrow = arrow(length=unit(.3, "cm"))) +
    coord_cartesian(ylim = c(0.5,5.5), xlim = c(0.5, 5.5)) +
    theme_void()

## creates vertical arrow
arrow_p3 <- 
    ggplot(dat3) +
    geom_segment(aes(x = .5, y = .8, xend = .5, yend = 4),
                 col = "black",
                 arrow = arrow(length=unit(.3, "cm"))) +
    coord_cartesian(ylim = c(0.5,5.5), xlim = c(0.5, 5.5)) +
    theme_void()

## generates the complete plot
ggdraw() +
    draw_plot(p1, x=.1, y=.25, width=.9, height = .75) +
    draw_plot(arrow_p2, x = 0, y = .2)+
    draw_plot(arrow_p, x = 0, y = .05)+
    draw_plot(arrow_p3, x = .05, y = .25)+
    draw_label("Acceptability of grizzlies", x=.45,y=.13, size=12)+
    draw_label("Too high", x=.23, y=.22, size=9)+
    draw_label("Just right", x=.43, y=.22, size=9)+
    draw_label("Too low", x=.63, y=.22, size=9)+
    draw_label("Low", x=.23, y=.05, size=9)+
    draw_label("High", x=.63, y=.05, size=9)+
    draw_label("Positive", x=.06, y=0.9, size=9, angle=90)+
    draw_label("Negative", x=.06, y=0.4, size=9, angle=90)
#ggsave("figs/AttitudeAcceptability.png", width = 6, height=4, units="in", dpi=300, bg="white")