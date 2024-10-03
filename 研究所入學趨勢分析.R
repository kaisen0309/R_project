library(ggplot2)
library(flexdashboard)
library(dplyr)
library(tidyr)
library(leaflet)
library(fmsb)
library(ggridges)
library(kableExtra)
library(plotly)
library(gapminder)
library(gganimate)
library(gifski)
library(tidyverse)
library(reshape2)
library(readxl)
library(htmlwidgets)
library(maps)


####資料集####

apply <- read_excel("E:/高大/三上/DAV/DAV_期末報告_A1103355_張凱森/Rdata_apply.xlsx")
test <- read_excel("E:/高大/三上/DAV/DAV_期末報告_A1103355_張凱森/Rdata_test.xlsx")
####maps####
TaiwanMap <- map_data('world', region="Taiwan")

labelpoints <- data.frame(
  long=c(121.53978195544607, 120.99753933087563,  120.99660552169593, 121.57735557757647, 120.21689193090907),
  lat=c(25.0176070055163, 24.787664300933457, 24.796316471831467,24.987976459501716, 22.997012339233304 ),
  names=c(" ", " ", "NTHU&NYCU", "NTU&NCCU", "NCKU"),
  strnigAsFactors=FALSE
)
school_map <- ggplot()+
  coord_map()+
  geom_polygon(data=TaiwanMap, 
               mapping = aes(x=long, 
                             y=lat, 
                             group=group), 
               fill=rgb(0,1,1,0.3), 
               color=rgb(0,1,1,0.8), 
               size=.2)+
  geom_point(data = labelpoints, 
             aes(x=long, y=lat), 
             color="blue", fill="yellow", shape=21, size=3)+
  geom_text(data = labelpoints, 
            mapping = aes(x=long, y=lat-0.1, label=names), size=3)
school_map


####每年總推甄人數 bar####
apply_year <- apply %>% 
  select(school, year, total_apply) %>% 
  group_by(year) %>% 
  arrange(year, -total_apply) %>%
  mutate(rank=1:n()) 

#各年份的圖
apply_total_plot <- ggplot(data=apply_year, 
                           aes(fill=school))+
  aes(xmin=0, 
      xmax=total_apply)+
  aes(ymin=rank-0.4,  
      ymax=rank+0.4, 
      y=rank)+
  facet_wrap(~year, ncol=3)+
  geom_rect(alpha=0.7)+
  scale_x_continuous(
    limits=c(-4000, 12000), 
    breaks = c(0, 4000, 8000, 12000))+
  geom_text(col="black", 
            hjust="right", 
            aes(label=school), 
            x=-350)+
  geom_text(size=3, 
            col="black", 
            hjust="right", 
            aes(label=total_apply), 
            x=apply_year$total_apply+300)+
  scale_y_reverse()+
  labs(fill=NULL)+
  labs(x="各校推甄總人數")+
  labs(y="")+
  theme_bw()+
  theme(legend.position = "none")

apply_total_plot

####每年總筆試人數 bar####
test_year <- test %>% 
  select(school, year, total_apply) %>% 
  group_by(year) %>% 
  arrange(year, -total_apply) %>%  
  mutate(rank=1:n()) 

#各年份的圖
test_total_plot <- ggplot(data=test_year, 
                          aes(fill=school))+
  aes(xmin=0, 
      xmax=total_apply)+ 
  aes(ymin=rank-0.4, 
      ymax=rank+0.4, 
      y=rank)+
  facet_wrap(~year, ncol=3)+
  geom_rect(alpha=0.7)+
  scale_x_continuous(
    limits=c(-4300, 18000), 
    breaks = c(0, 4000, 10000, 16000))+
  geom_text(col="black", 
            hjust="right", 
            aes(label=school), 
            x=-500)+
  geom_text(size=3,
            col="black", 
            hjust="right", 
            aes(label=total_apply), 
            x=test_year$total_apply-100)+
  scale_y_reverse()+
  labs(fill=NULL)+
  labs(x="各校筆試總人數")+
  labs(y="")+
  theme_bw()+
  theme(legend.position = "none")

test_total_plot

####每年推甄資管報名人數 bar+動####
applyIM_year <- apply %>% 
  select(school, year, im_apply) %>% 
  group_by(year) %>% 
  arrange(year, -im_apply) %>%   
  mutate(rank=1:n()) 

#各年份的圖
apply_IM_plot <- ggplot(data=applyIM_year, 
                        aes(fill=school))+
  aes(xmin=0, 
      xmax=im_apply)+
  aes(ymin=rank-0.4,  
      ymax=rank+0.4, 
      y=rank)+
  facet_wrap(~year, ncol=3)+
  geom_rect(alpha=0.7)+
  scale_x_continuous(
    limits=c(-200, 400), 
    breaks = c(0, 200, 400))+
  geom_text(col="black", 
            hjust="right", 
            aes(label=school), 
            x=-50)+
  scale_y_reverse()+
  labs(fill=NULL)+
  labs(x='各校資管所推甄報名人數')+
  labs(y="")+
  theme_bw()+
  theme(legend.position = "none")

apply_IM_plot

apply_IM_plot2 <- apply_IM_plot+geom_text(size=3,
                                          col="black", 
          hjust="right", 
          aes(label=round(im_apply)), 
          x=applyIM_year$im_apply-15)
apply_IM_plot2

#動圖
apply_IM_total <- apply_IM_plot+
  facet_null()+
  scale_x_continuous(
    limits = c(-80, 400), 
    breaks = c(0, 200, 400))+
  geom_text(x=1000, y=-10, 
            family = "Times", 
            aes(label = as.character(year)), 
            size=30, col="black")+
  aes(group = school)+
  gganimate::transition_time(year)+
  labs(title = "Year: {round(frame_time)}")

apply_IM_total
anim_save("apply_IM_total.fig")

####每年推甄資管錄取人數 bar####
applyIMEnroll <- apply %>% 
  select(school, year, im_enroll) %>% 
  group_by(year) %>% 
  arrange(year, -im_enroll) %>%   
  mutate(rank=1:n()) 

apply_IM_Enroll <- ggplot(data=applyIMEnroll, 
                          aes(fill=school))+
  aes(xmin=0, 
      xmax=im_enroll)+
  aes(ymin=rank-0.4,  
      ymax=rank+0.4, 
      y=rank)+
  facet_wrap(~year, ncol=3)+
  geom_rect(alpha=0.7)+
  scale_x_continuous(
    limits=c(-30, 50), 
    breaks = c(0, 20, 40))+
  geom_text(col="black", 
            hjust="right", 
            aes(label=school), 
            x=-10)+
  geom_text(size=3,
            col="black", 
            hjust="right", 
            aes(label=im_enroll), 
            x=applyIMEnroll$im_enroll-1.5)+
  scale_y_reverse()+
  labs(fill=NULL)+
  labs(x='各校資管所推甄錄取人數')+
  labs(y="")+
  theme_bw()+
  theme(legend.position = "none")

apply_IM_Enroll


####每年推甄資管錄取率 折線圖####
apply_im_rate <- apply %>% 
  ggplot(mapping=aes(x=year, y=im_rate*100, fill=factor(year), color=factor(school)))+
  scale_y_continuous(breaks = c(5,7.5,  10, 12.5, 15, 17.5, 20,22.5))+
  geom_point(size=2, show.legend = FALSE)+
  geom_text( size=3.5,
             vjust = c(rep(-0.5, 5), c(1.5, rep(-0.5, 4)), c(2, -0.5, 0,0,0), c(-0.5, 2,2,2,-0.5), c(rep(-0.5, each=4), 2)), 
            aes(label=round(im_rate*100, 2)),
            x=apply$year,
            y=apply$im_rate*100+0.1)+
  geom_line(aes(group=school) )+
  labs(x="年份", 
       y="錄取率(%)")+
  theme_bw()+
  ggtitle("資管所推甄錄取率")

apply_im_rate

####每年考試資管報名人數 bar+動####
testIM_year <- test %>% 
  select(school, year, im_apply) %>% 
  group_by(year) %>% 
  arrange(year, -im_apply) %>%   
  mutate(rank=1:n()) 


test_IM_plot <- ggplot(data=testIM_year, 
                       aes(fill=school))+
  aes(xmin=0, 
      xmax=im_apply)+ 
  aes(ymin=rank-0.4, 
      ymax=rank+0.4, 
      y=rank)+
  facet_wrap(~year, ncol=3)+
  geom_rect(alpha=0.7)+
  scale_x_continuous(
    limits=c(-300, 750), 
    breaks = c(0, 200, 400, 600, 800))+
  geom_text(col="black", 
            hjust="right", 
            aes(label=school), 
            x=-50)+
  scale_y_reverse()+
  labs(fill=NULL)+
  labs(x='各校資管所考試報名人數')+
  labs(y="")+
  theme_bw()+
  theme(legend.position = "none")

test_IM_plot

test_IM_plot2 <- test_IM_plot+geom_text(size=3,
                                        col="black", 
          hjust="right", 
          aes(label=im_apply), 
          x=testIM_year$im_apply-10)
test_IM_plot2

#動圖
test_IM_total <- test_IM_plot+
  facet_null()+
  scale_x_continuous(
    limits = c(-80, 750), 
    breaks = c(0, 200, 400, 600, 800))+
  geom_text(x=1000, y=-10, 
            family = "Times", 
            aes(label = as.character(year)), 
            size=30, col="black")+
  aes(group = school)+
  transition_time(year)+
  labs(title = "Year: {round(frame_time)}")

test_IM_total
anim_save("test_IM_total.fig")
####每年考試資管錄取人數 bar####
testIMEnroll <- test %>% 
  select(school, year, im_enroll) %>% 
  group_by(year) %>% 
  arrange(year, -im_enroll) %>%   
  mutate(rank=1:n()) 

test_IM_Enroll <- ggplot(data=testIMEnroll, 
                         aes(fill=school))+
  aes(xmin=0, 
      xmax=im_enroll)+
  aes(ymin=rank-0.4, 
      ymax=rank+0.4, 
      y=rank)+
  facet_wrap(~year, ncol=3)+
  geom_rect(alpha=0.7)+
  scale_x_continuous(
    limits=c(-15, 30), 
    breaks = c(0, 15, 30, 45))+
  geom_text(col="black", 
            hjust="right", 
            aes(label=school), 
            x=-3)+
  geom_text(size=3,
            col="black", 
            hjust="right", 
            aes(label=im_enroll), 
            x=testIMEnroll$im_enroll-1)+
  scale_y_reverse()+
  labs(fill=NULL)+
  labs(x='各校資管所考試錄取人數')+
  labs(y="")+
  theme_bw()+
  theme(legend.position = "none")

test_IM_Enroll

####每年考試資管錄取率 折線圖####
test_im_rate <- test %>% 
  ggplot(mapping=aes(x=year, y=im_rate*100, fill=factor(year), color=factor(school)))+
  scale_y_continuous(breaks = c(2.5,3,  3.5, 4, 4.5, 5,5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9))+
  geom_point(size=2, show.legend = FALSE)+
  geom_text( size=3.5,
             vjust = c(rep(c(-0.2,-0.3, -1, -0.5), each=5), -0.5, 2.5, -0.5, 3, -0.5), 
             aes(label=round(im_rate*100, 2)),
             x=test$year,
             y=test$im_rate*100+0.1)+
  geom_line(aes(group=school) )+
  labs(x="年份", 
       y="錄取率(%)")+
  theme_bw()+
  ggtitle("資管所考試錄取率")
test_im_rate

####每年推甄資工報名人數 bar+動####
applyCS_year <- apply %>% 
  select(school, year, cs_apply) %>% 
  group_by(year) %>% 
  arrange(year, -cs_apply) %>%  
  mutate(rank=1:n()) 

apply_CS_plot <- ggplot(data=applyCS_year, 
                        aes(fill=school))+
  aes(xmin=0, 
      xmax=cs_apply)+
  aes(ymin=rank-0.4, 
      ymax=rank+0.4, 
      y=rank)+
  facet_wrap(~year, ncol=3)+
  geom_rect(alpha=0.7)+
  scale_x_continuous(
    limits=c(-400, 1400), 
    breaks = c(0, 400, 800, 1200))+
  geom_text(col="black", 
            hjust="right", 
            aes(label=school), 
            x=-50)+
  scale_y_reverse()+
  labs(fill=NULL)+
  labs(x='各校資工所推甄報名人數')+
  labs(y="")+
  theme_bw()+
  theme(legend.position = "none")

apply_CS_plot

apply_CS_plot2 <- apply_CS_plot+geom_text(size=3,
                                          col="black", 
                                          hjust="right", 
                                          aes(label=cs_apply), 
                                          x=applyCS_year$cs_apply+300)
apply_CS_plot2

#動圖
apply_CS_total <- apply_CS_plot+
  facet_null()+
  scale_x_continuous(
    limits = c(-200, 1400), 
    breaks = c(0, 400, 800, 1200))+
  geom_text(x=1000, y=-10, 
            family = "Times", 
            aes(label = as.character(year)), 
            size=30, col="black")+
  aes(group = school)+
  transition_time(year)+
  labs(title = "Year: {round(frame_time)}")

apply_CS_total
anim_save("apply_CS_total.fig")

####每年推甄資工錄取人數 bar####
applyCSEnroll <- apply %>% 
  select(school, year, cs_enroll) %>% 
  group_by(year) %>% 
  arrange(year, -cs_enroll) %>%   
  mutate(rank=1:n()) 

apply_CS_Enroll <- ggplot(data=applyCSEnroll, 
                          aes(fill=school))+
  aes(xmin=0, 
      xmax=cs_enroll)+
  aes(ymin=rank-0.4, 
      ymax=rank+0.4, 
      y=rank)+
  facet_wrap(~year, ncol=3)+
  geom_rect(alpha=0.7)+
  scale_x_continuous(
    limits=c(-70, 200), 
    breaks = c(0,50, 100, 150, 200))+
  geom_text(col="black", 
            hjust="right", 
            aes(label=school), 
            x=-10)+
  geom_text(size=3,
            col="black", 
            hjust="right", 
            aes(label=cs_enroll), 
            x=applyCSEnroll$cs_enroll)+
  scale_y_reverse()+
  labs(fill=NULL)+
  labs(x='各校資工所推甄錄取人數')+
  labs(y="")+
  theme_bw()+
  theme(legend.position = "none")

apply_CS_Enroll


####每年推甄資工錄取率 折線圖####
apply_cs_rate <- apply %>% 
  ggplot(mapping=aes(x=year, y=cs_rate*100, fill=factor(year), color=factor(school)))+
  scale_y_continuous(breaks = c(10, 15, 20, 25, 30, 35))+
  geom_point(size=2, show.legend = FALSE)+
  geom_text( size=3.5,
             vjust = c(c(rep(-0.5,10)), c(-0.5, rep(2, 4)), c(rep(1.5, 5)), rep(-0.5, 5)), 
             aes(label=round(cs_rate*100, 2)),
             x=apply$year,
             y=apply$cs_rate*100+0.1)+
  geom_line(aes(group=school) )+
  labs(x="年份", 
       y="錄取率(%)")+
  theme_bw()+
  ggtitle("資工所推甄錄取率")

apply_cs_rate

####每年考試資工報名人數 bar+動####
testCS_year <- test %>% 
  select(school, year, cs_apply) %>% 
  group_by(year) %>% 
  arrange(year, -cs_apply) %>%   
  mutate(rank=1:n()) 


test_CS_plot <- ggplot(data=testCS_year, 
                       aes(fill=school))+
  aes(xmin=0, 
      xmax=cs_apply)+ 
  aes(ymin=rank-0.4, 
      ymax=rank+0.4, 
      y=rank)+
  facet_wrap(~year, ncol=3)+
  geom_rect(alpha=0.7)+
  scale_x_continuous(
    limits=c(-500, 2300), 
    breaks = c(0,  800,  1600, 2400))+
  geom_text(col="black", 
            hjust="right", 
            aes(label=school), 
            x=-50)+
  scale_y_reverse()+
  labs(fill=NULL)+
  labs(x='各校資工所考試報名人數')+
  labs(y="")+
  theme_bw()+
  theme(legend.position = "none")

test_CS_plot

test_CS_plot2 <- test_CS_plot+geom_text(size=3, 
                                        col="black", 
                                        hjust="right", 
                                        aes(label=cs_apply), 
                                        x=testCS_year$cs_apply+350)

test_CS_plot2
#動圖
test_CS_total <- test_CS_plot+
  facet_null()+
  scale_x_continuous(
    limits = c(-300, 2400), 
    breaks = c(0,  800,  1600, 2400))+
  geom_text(x=1000, y=-10, 
            family = "Times", 
            aes(label = as.character(year)), 
            size=30, col="black")+
  aes(group = school)+
  transition_time(year)+
  labs(title = "Year: {round(frame_time)}")

test_CS_total
anim_save("test_CS_total.fig")

####每年考試資工錄取人數 bar####
testCSEnroll <- test %>% 
  select(school, year, cs_enroll) %>% 
  group_by(year) %>% 
  arrange(year, -cs_enroll) %>%   
  mutate(rank=1:n()) 

test_CS_Enroll <- ggplot(data=testCSEnroll, 
                         aes(fill=school))+
  aes(xmin=0, 
      xmax=cs_enroll)+
  aes(ymin=rank-0.4,  
      ymax=rank+0.4,
      y=rank)+
  facet_wrap(~year, ncol=3)+
  geom_rect(alpha=0.7)+
  scale_x_continuous(
    limits=c(-35,150), 
    breaks = c(0, 50, 100,150))+
  geom_text(col="black", 
            hjust="right", 
            aes(label=school), 
            x=-5)+
  geom_text(size=3,
            col="black", 
            hjust="right", 
            aes(label=cs_enroll), 
            x=testCSEnroll$cs_enroll-2)+
  scale_y_reverse()+
  labs(fill=NULL)+
  labs(x='各校資工所考試錄取人數')+
  labs(y="")+
  theme_bw()+
  theme(legend.position = "none")

test_CS_Enroll

####每年考試資工錄取率 折線圖####
test_cs_rate <- test %>% 
  ggplot(mapping=aes(x=year, y=cs_rate*100, fill=factor(year), color=factor(school)))+
  scale_y_continuous(breaks = c(2, 3,  4,  5, 6, 7,8,9,10, 15, 20))+
  geom_point(size=2, show.legend = FALSE)+
  geom_text( size=3.5,
             vjust = c(rep(-0.5,5), c(-0.5, 2, 2, -0.2, 2), rep(-0.5,5), c(rep(-0.5,4), 2), c(1.5, -0.5, -0.5, 2, -0.5)), 
             aes(label=round(cs_rate*100, 2)),
             x=test$year,
             y=test$cs_rate*100+0.1)+
  geom_line(aes(group=school) )+
  labs(x="年份", 
       y="錄取率(%)")+
  theme_bw()+
  ggtitle("資工所考試錄取率")
test_cs_rate
