# Powerball
Code for some calculations and charts 



#install needed packages

library(tidyverse)
library(ggtext)
library(ggrepel)
library(showtext)
library(rvest)
library(modelr)

#create list of dates over which I want to get Powerball results

big_dates5 <- seq.Date( from  = as.Date("19-04-2018", format = "%d-%m-%Y"), to = as.Date("17-02-2022", format = "%d-%m-%Y"),by= 7)%>% 
  format(format = "%d-%m-%Y")

#create list of webpages to scrape
links5 <- paste0("https://australia.national-lottery.com/powerball/results/",
         big_dates5)

#Set up to do the scraping by creating vectors and data frames for the information to go in

final5 <- data.frame()
table5 <- vector(mode = "list", length = length(links5))
dates5 <- vector()

#run for loops to scrape the webpages to get the data on number of winners, prizes, etc. (this takes ~10min)

for (i in seq_along(links5)) {
  table5[[i]] <- 
    links5[i] %>%
    read_html() %>%
    html_element("table") %>%
    html_table()
  
  dates5[[i]] <- read_html(links5[i]) %>% 
    html_element("h1") %>% 
    html_text()
  table5[[i]]  <- table5[[i]] %>% 
    mutate( datecol = dates5[[i]])
}

#then bind them and turn into a tibble

final5 <-  bind_rows(final5, table5)

BiggerScrape5 <- as_tibble(final5)

#Change the format and layout of the data to make it easy to work with, rename things, etc. (aka "data cleaning")

draws1822 <- BiggerScrape5 %>% 
  filter(Division %in% c("1", "2","3", "4", "5","6","7","8","9")) %>% 
  mutate(Division = as.numeric(Division)) %>% 
  mutate(Winners = str_remove_all(Winners, "[,]")) %>% 
  mutate(Winners = as.numeric(Winners)) %>% 
  mutate(`Division Prize` = str_remove_all(`Division Prize`, "[$,]")) %>% 
  mutate(`Division Prize` =as.numeric(`Division Prize`)) %>% 
  mutate(columnfordiv1 = if_else(Winners<2, `Division Prize`, `Division Prize`*Winners)) %>% 
  mutate(div1advertised = if_else(Division == 1,columnfordiv1, 0 )) %>% 
  group_by(datecol) %>% 
  mutate(div1advertised = sum(div1advertised)) %>% 
  ungroup() %>% 
  mutate(`Division Prize Pool` = str_sub(`Division Prize Pool`,1,13)) %>% 
  mutate(`Division Prize Pool` = str_remove_all(`Division Prize Pool`,"[$,]")) %>% 
  mutate(`Division Prize Pool` = str_replace(`Division Prize Pool`,"\r\n\t\t\t\t\t\t",""))  %>% 
  mutate(`Division Prize Pool` = as.numeric(`Division Prize Pool`)) %>% 
  mutate(datecol = str_sub(datecol, 37,48)) %>% 
  mutate(datecol = as.Date(datecol, format = "%d %B %Y")) %>% 
  rename(division = Division,
         prize = `Division Prize`,
         prize_pool = `Division Prize Pool`,
         winners = Winners,
         date = datecol) 

#create data for Graph of odds of winning [odds taken from powerball website]

odds_of_winning <- tibble( "division" =c(1,2,3,4,5,6,7,8,9), 
                           "Odds" =c(1/134490400, 1/7078443, 1/686176, 1/36115, 1/16943,1/1173, 1/892, 1/188, 1/66),
                           "words" = c("1 in 134 million", "1 in 7 million", "1 in 686,000", "1 in 36,000", "1 in 6940", "1 in 1173", "1 in 892", "1 in 188", "1 in 66"  ))

#graph odds of winning

odds_of_winning %>% 
  ggplot()+
  aes(x=division, y= Odds, colour = Odds)+
  geom_point(show.legend = FALSE, size =7)+
  scale_y_log10(labels =c("0.01", "0.001", "0.0001", "0.00001", "0.000001","0.0000001", "0.00000001"), 
                breaks = c(.01, .001, .0001, .00001, .000001,.0000001, .00000001))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9), limits = c(1,9.5))+
  geom_text(aes(label =words), hjust =.2, vjust = 1.7, size =6, show.legend = FALSE)+
  scale_colour_gradientn(colours = c("red", "yellow", "white"))+
  labs(title = "Odds of winning each division in powerball, log axis")+ 
  theme(panel.background = element_rect(fill="blue4"),
        panel.grid.minor.x =  element_line(colour = "grey", size =.1),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =  element_line(colour = "grey", size =.1),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(colour = "white",  size =22),
        axis.text = element_text(colour = "white", size =14),
        axis.title = element_text(colour = "white", size =14),
        plot.background = element_rect(fill = "blue4"))


ggsave(filename = paste("odds of winning each division in powerball, log axis",
                        format(Sys.time(), "%Y-%m-%d"), ".jpeg"),
       width = 2740, height =1840, units ="px" )

#Chart to show number  Division One prizes over time

draws1822%>% 
  filter(division ==1) %>% 
  mutate(fillcol  = if_else(winners>0, 0,1)) %>% 
  mutate(label = if_else(winners>0, paste0(scales::dollar(round(div1advertised/1000000)), "m"), NA_character_)) %>% 
  ggplot()+
  aes(x=date, y = div1advertised, fill = fillcol)+
  geom_col(show.legend = FALSE)+
  geom_text(aes(label = label), vjust =-.2, size = 3, colour = "white",show.legend = FALSE)+
  labs(title = "Division 1 prizes aren't won each week.")+
  ylab("Division 1 Advertised Prize")+
  xlab("Date of Powerball Draw")+
  scale_y_continuous(labels = scales::dollar)+
  scale_fill_gradientn(colours = c("red", "grey")) +
  theme(panel.background = element_rect(fill="blue4"),
        panel.grid.major.x =  element_line(colour = "grey", size =.1),
        panel.grid.minor.x =  element_blank(),
        panel.grid.major.y = element_line(colour = "grey", size =.1),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(colour = "white",  size =22),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "light grey", size =14),
        axis.title = element_text(colour = "white", size =14),
        plot.background = element_rect(fill = "blue4"))

ggsave(filename = paste("Total prizes awarded each week",format(Sys.time(), "%Y-%m-%d"), ".jpeg"), 
       width = 2740, height =1840, units ="px")
       
       
#Chart of prizes in each Division

draws1822%>% 
  ggplot()+
  aes(x=division, y= prize)+
  geom_point(alpha =.2,colour = "grey", size =3, show.legend = FALSE, position = "jitter")+
  scale_y_log10(labels = scales::dollar, limits =c(2,100000000), breaks = c(0,10, 100,1000, 10000,100000, 1000000, 10000000,100000000))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9))+
  labs(title="Powerball Prizes per division (log axis)")+
  scale_colour_gradientn(colours = c( "grey", "purple"))+
  theme(panel.background = element_rect(fill="blue4"),
        panel.grid.minor.x =  element_line(colour = "grey", size =.1),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =  element_line(colour = "grey", size =.1),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(colour = "white", family = "Helvetica", size =22),
        axis.text.y = element_text(colour = "white", size =13),
        axis.text.x = element_text(colour = "white", size =15),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "blue4"))

ggsave(filename = paste("Prizes in each division",format(Sys.time(), "%Y-%m-%d"), ".jpeg"), 
       width = 2740, height =1840, units ="px")

#Creating a model that predicts the number of tickets bought depending on Division 1 prize.

pb4model <- draws1822 %>% 
  filter(division == "9") %>% 
  mutate(entries =  winners*66) 

pbmod <-  lm(entries ~ div1advertised, data = pb4model)

coef(pbmod)

#following the process in r4ds to create a tibble that has all known div 1 prizes in a column, 
# and a column of predicted tickets sold for each

grid <- pb4model %>% 
  data_grid(div1advertised)

grid <- grid %>% 
  add_predictions(pbmod)

#Create the inputs we will need to estimate expected value

div1odds <-   odds_of_winning %>% 
  filter(division == 1) %>% 
  select (Odds) 

oddsetc <- grid %>% 
  mutate(odds = div1odds) 

draws1822 %>% 
  filter(str_starts(prize_pool,"Jackpotted"))

draws1822clean <- draws1822 %>% 
  select(division,prize, winners, prize_pool, date, div1advertised) %>% 
  mutate(prize_pool = if_else(is.na(prize_pool), 0, as.numeric(prize_pool))) %>% 
  mutate(entries = if_else(division == 9, winners, 0)) %>% 
  group_by(date) %>% 
  mutate(entries = sum(entries)*66) %>% 
  ungroup() %>% 
  left_join(odds_of_winning, by = "division") %>% 
  left_join(oddsetc, by = "div1advertised") %>% 
  select(-multi, -inverse, -odds2winners,-words, -odds3w ) %>% 
right_join(draws1822withdiv2, by = "date")
