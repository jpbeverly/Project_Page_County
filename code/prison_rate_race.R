library(gapminder)
#install.packages("gapminder")
library(tidyverse)
library(readxl)
library(gganimate)
#install.packages("gifski")
#install.packages("png")
library(png)
library(hrbrthemes)
library(animate)
install.packages("ffmepeg")

data <- read_excel("VA_Incarceration_Rates.xlsx")
data2 <- data %>% select(yfips, year,county_name,total_prison_pop_rate)
med <- data2 %>% group_by(year) %>% summarise(total_prison_pop_rate=median(total_prison_pop_rate, na.rm = TRUE))
med$county_name <- "VA Median"
med$yfips <- 51
data3 <- rbind(data2,med)
page_compare <- subset(data3, year>=1983 & year <= 2013 & county_name %in% c("Greene County", "Rappahanock County", "Warren County", "VA Median", "Page County", "Rockingham County", "Madison County", "Shenandoah County"))

page_compare %>% group_by(year) %>% 
  arrange(year, -total_prison_pop_rate) %>% 
  mutate(rank = 1:n()) -> 
  ranked_by_year
ranked_by_year<- ranked_by_year[,-1]

my_theme <- theme_classic(base_family = "Times") +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line.y = element_blank()) +
  theme(legend.background = element_rect(fill = "gainsboro")) +
  theme(plot.background = element_rect(fill = "gainsboro")) +
  theme(panel.background = element_rect(fill = "gainsboro"))


ranked_by_year %>%  
  ggplot() +
  aes(xmin = 0 ,  
      xmax = total_prison_pop_rate) +  
  aes(ymin = rank - .45,  
      ymax = rank + .45,  
      y = rank) +  
  facet_wrap(~ year) +  
  geom_rect(alpha = 0.7) +  
  aes(height = total_prison_pop_rate,fill = county_name) +  
  scale_fill_viridis_d(option = "magma",  
                       direction = -1) +  
  scale_x_continuous(limits = c(-500, 1300),
                     breaks = c(0, 400, 800, 1200)) +  
  geom_text(col = "gray13",  
            hjust = "right",  
            aes(label = county_name),  
            x = -50) +  
  scale_y_reverse() +  
  labs(fill = NULL) +  
  labs(x = 'Prison Population Rate') +  
  labs(y = "") +
  ggtitle("Prison Population Rates")+
  theme_bw() + theme(plot.title = element_text(size = 30, face = "bold"), axis.text=element_text(size = 20), axis.title.x = element_text(size = 25), legend.text = element_text(size = 15))->
  my_plot
my_plot

windowsFonts("Times" = windowsFont("Times"))
options(gganimate.nframes=300)

anim<- my_plot +  
  facet_null() +  
  scale_x_continuous(  
    limits = c(-500, 1300),  
    breaks = c(0, 400, 800, 1200)) +  
  geom_text(aes(x = 10, y = 10, label = as.character(year)), 
            size = 20, vjust = 0, hjust = -0.5) +  
  aes(group = county_name) +  
  gganimate::transition_time(year)

anim
animate(anim, 300, fps = 5, bg = 'transparent',  width = 800, height = 600, 
        renderer = gifski_renderer()) -> for_gif
anim_save("race3.gif", animation = for_gif, path = 'C:/Users/JoshBeverly/Desktop/VT CLASSES')



