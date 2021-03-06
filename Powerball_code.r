Done in RStudio.

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

#Chart to show number of Division One prizes over time

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
  
   #model to predict div 2prize Pool based on div1 prize
  
  div2model_numbers <- draws1822 %>% 
  group_by(date) %>% 
  mutate(entries = if_else(division == 9, winners*66, 0)) %>% 
  mutate(entries = sum(entries)) %>% 
  ungroup() %>% 
  filter(division == "2") 
  
  
  
div2_Prizeentries_model <- lm(prize ~ entries, data = div2model_numbers)  

coef(div2_Prizeentries_model)

div2grid_Prizeentries <- div2model_numbers %>% 
  data_grid(entries)

div2grid2_Prizeentries <- div2grid_Prizeentries %>% 
  add_predictions(div2_Prizeentries_model)

div2model_numbers %>% 
  ggplot(aes(entries))+
  geom_point(aes(y=prize))+
  geom_line(aes(y=pred), data = div2grid2_Prizeentries, colour="red", size = 1)
  
  draws1822_d2_p <- draws1822 %>% 
  group_by(date) %>% 
  mutate(entries = if_else(division == 9, winners*66, 0)) %>% 
  mutate(entries = sum(entries)) %>% 
  ungroup() %>% 
  left_join(div2grid2_Prizeentries, by = "entries") %>% 
  rename("div2prizepred" = "pred") %>% 
  select(-columnfordiv1, -`Numbers Matched`)
  
  draws1822withdiv2 <- draws1822_d2_p %>% 
  mutate(d2c = if_else(division == 2, 1, 0)) %>% 
  mutate(d2c2 = if_else(winners == 0, 1, 0)) %>% 
  mutate(d2c3 = d2c*d2c2) %>% 
  mutate(prize = if_else(d2c3 == 1, div2prizepred, prize )) %>% 
  select(-d2c, -d2c2, -d2c3)


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

  #Make basic stacked column chart showing EVs for a single powerball draw.

draws1822clean %>% 
  filter(date == "2022-01-13") %>% 
  mutate(check1 = if_else(division == 2, 1,0)) %>% 
mutate(check2 = if_else(winners == 0, 1,0)) %>% 
mutate(check3 = check1*check2) %>% 
  mutate(prize = if_else(check3 == 1, div2prizepred, prize)) %>% 
  mutate(EV = prize*Odds ) %>% 
  mutate(roundEV = round(EV, 3)) %>% 
  group_by(date) %>% 
  mutate(label = paste0("Division", division, ": $",roundEV)) %>% 
  mutate(totalEV = sum(EV)) %>% 
  mutate(division2 = as.factor(division)) %>% 
  mutate(order = fct_reorder(division2, division)) %>% 
  ungroup() %>% 
  ggplot()+
  aes(y = EV, x=date, fill = division2)+
  geom_col(show.legend = FALSE)+
  scale_fill_brewer()+
  scale_y_continuous(limits = c(0, .5), labels = scales::dollar)+
geom_text(aes(label = label, group = order), position = position_stack(vjust = .5), show.legend = FALSE)+
  labs(title = "On the 13th of January this year, a $3 million Division 1 prize was offered. \nA basic calculation of the expected payoff of your Powerball ticket was 42.5 cents.\nMost of which - around 15 cents - came from Division 9 ")+
  
  theme(panel.background = element_rect(fill="blue4"),
        panel.grid.major.x =  element_line(colour = "grey", size =.1),
        panel.grid.minor.x =  element_blank(),
        panel.grid.major.y = element_line(colour = "grey", size =.1),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(colour = "white",  size =16),
        axis.text = element_text(colour = "light grey", size =14),
        axis.title.y = element_text(colour = "white", size =14),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "blue4"))


ggsave(filename = paste("naive EV, 13 Jan", 
                        format(Sys.time(), "%Y-%m-%d"), ".jpeg"), 
       width = 2740, height =1840, units ="px")

#repeat for multiple dates


draws1822clean %>% 
  filter(date > "2022-01-01") %>% 
  mutate(check1 = if_else(division == 2, 1,0)) %>% 
  mutate(check2 = if_else(winners == 0, 1,0)) %>% 
  mutate(check3 = check1*check2) %>% 
  mutate(prize = if_else(check3 == 1, div2prizepred, prize)) %>% 
  mutate(EV = prize*Odds ) %>% 
  mutate(roundEV = round(EV, 3)) %>% 
  group_by(date) %>% 
  mutate(label = paste0("Div", division, ": $",roundEV)) %>% 
  mutate(totalEV = sum(EV)) %>% 
  mutate(division2 = as.factor(division)) %>% 
  mutate(order = fct_reorder(division2, division)) %>% 
  ungroup() %>% 
  ggplot()+
  aes(y = EV, x=date, fill = division2)+
  geom_col(show.legend = FALSE)+
  geom_abline(slope = 0, intercept = 1.21, colour = "red")+
  scale_fill_brewer()+
  scale_x_date(labels = c("6 Jan, $60m", "13 Jan, $3m", "20 Jan, $8m", "27 Jan, $20m", "3 Feb, $40m", "10 Feb, $60m", "17 Feb, $80m"), breaks = as.Date(c("2022-01-06", "2022-01-13", "2022-01-20", "2022-01-27", "2022-02-03", "2022-02-10", "2022-02-17")))+
  scale_y_continuous(limits = c(0, 1.25), labels = scales::dollar)+
  geom_text(aes(label = label, group = order), size  = 2.3, position = position_stack(vjust = .5), show.legend = FALSE)+
  labs(title = "No draws this year have had an expected payoff equal to \nthe cost of the ticket (red line), using the basic calculation")+
  
  theme(panel.background = element_rect(fill="blue4"),
        panel.grid.major.x =  element_line(colour = "grey", size =.1),
        panel.grid.minor.x =  element_blank(),
        panel.grid.major.y = element_line(colour = "grey", size =.1),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(colour = "white",  size =16),
        axis.text.x = element_text(colour = "light grey", size =14, angle = 45, hjust = 1.1, vjust = 1.4),
        axis.text.y = element_text(colour = "light grey", size =14),
        axis.title.y = element_text(colour = "white", size =14),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "blue4"))

  ggsave(filename = paste("naive EV, 2022", 
                        format(Sys.time(), "%Y-%m-%d"), ".jpeg"), 
       width = 2740, height =2740, units ="px")
       
       #Chart to show number of winners and Division One prizes over time

draws1822%>% 
  filter(division ==1) %>% 
  filter(date>"2019-04-01") %>% 
  mutate(label = if_else(winners>0, as.character(winners), NA_character_)) %>% 
  ggplot()+
  aes(x=date, y = div1advertised, fill = winners)+
  geom_col(show.legend = FALSE)+
  geom_text(aes(label = label), vjust =-.2, size = 6, colour = "white",show.legend = FALSE)+
  labs(title = "Division 1 prizes and numbers of winners.")+
  ylab("Division 1 Advertised Prize")+
  xlab("Date of Powerball Draw")+
  scale_fill_gradientn(colours = c("grey40", "orange", "red","dark red"))+
  scale_y_continuous(labels = scales::dollar)+
  theme(panel.background = element_rect(fill="blue4"),
        panel.grid.major.x =  element_line(colour = "grey", size =.1),
        panel.grid.minor.x =  element_blank(),
        panel.grid.major.y = element_line(colour = "grey", size =.1),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(colour = "white",  size =22),
        axis.text = element_text(colour = "light grey", size =14),
        axis.title = element_text(colour = "white", size =14),
        plot.background = element_rect(fill = "blue4"))

ggsave(filename = paste("Total prizes awarded each week, and number of winners",format(Sys.time(), "%Y-%m-%d"), ".jpeg"), 
       width = 2740, height =1840, units ="px")


#Graph showing actual EV vs naive EV in geom_point

draws1822clean %>%
  group_by(date) %>% 
  mutate(entries2 = if_else(division == 9, winners*66, 0) ) %>% #these two mutate lines copy the number of entries calculated for div9 to the whole date.
  mutate(entries3 = sum(entries2)) %>% 
  ungroup() %>% 
  mutate(check1 = if_else(division == 2, 1,0)) %>% 
  mutate(check2 = if_else(winners == 0, 1,0)) %>% 
  mutate(check3 = check1*check2) %>% 
  mutate(prize = if_else(check3 == 1, div2prizepred, prize)) %>% 
  mutate(oddsnowinner = (1-Odds)^entries3) %>% 
  mutate (odds1winner = ((entries3*Odds)^1 * e^-(entries3*Odds)) / factorial(1)) %>%   
  mutate (odds2winner = ((entries3*Odds)^2 * e^-(entries3*Odds)) / factorial(2)) %>% 
  mutate (odds3winner = ((entries3*Odds)^3 * e^-(entries3*Odds)) / factorial(3)) %>% 
  mutate (odds4winner = ((entries3*Odds)^4 * e^-(entries3*Odds)) / factorial(4)) %>% 
  mutate (odds5winner = ((entries3*Odds)^5 * e^-(entries3*Odds)) / factorial(5)) %>%   
  mutate (odds6winner = ((entries3*Odds)^6 * e^-(entries3*Odds)) / factorial(6)) %>% 
  mutate (odds7winner = ((entries3*Odds)^7 * e^-(entries3*Odds)) / factorial(7)) %>% 
  mutate (odds8winner = ((entries3*Odds)^8 * e^-(entries3*Odds)) / factorial(8)) %>% 
  mutate(expected_value =  if_else(division == 1, (
    Odds*(
      div1advertised -
        ((div1advertised/2)*odds2winner +
           (div1advertised*2/3)*odds3winner +
           (div1advertised*3/4)*odds4winner +
           (div1advertised*4/5)*odds5winner +
           (div1advertised*5/6)*odds6winner +
           (div1advertised*6/7)*odds7winner +
           (div1advertised*7/8)*odds8winner) 
    )), prize*Odds)) %>% 
  
 mutate(check1 = if_else(winners>1, 1,0)) %>% 
  mutate(check2 = if_else(division == 1, 1,0)) %>% 
mutate(check3 = check1*check2) %>% 
  mutate(EV = if_else(check3==1, div1advertised*Odds, prize*Odds )) %>% 
  select (EV,check3, division, prize,prize_pool, winners, date, div1advertised, expected_value) %>% 
group_by(date) %>% 
  mutate(total_exp = sum(expected_value)) %>% 
  mutate(total_EV = sum(EV)) %>% 
  ungroup() %>% 
  ggplot()+
  aes(x=total_EV, y= total_exp, colour = div1advertised, size = div1advertised)+
  scale_size(guide = 'none')+
  scale_y_continuous(limits = c(.5, 1.55),breaks = c(.6, .9, 1.2, 1.5))+
  scale_colour_gradientn(colours = c("white", "yellow", "gold", "orange"),labels = scales::dollar, name = "Div 1 Prize")+
  geom_point()+geom_smooth(se = FALSE, show.legend = FALSE)+
  labs(title = "When the jackpot is high, splitting the pot kills your upside")+
  xlab("Naive EV")+
  ylab("Expected Value accounting for multiple winners")+
  theme(panel.background = element_rect(fill="blue4"),
        panel.grid.major.x =  element_line(colour = "grey", size =.1),
        panel.grid.minor.x =  element_blank(),
        panel.grid.major.y = element_line(colour = "grey", size =.1),
        panel.grid.minor.y = element_blank(),
        legend.background = element_rect(fill="blue4"),
        legend.text = element_text(colour = "white", size =10),
        legend.title = element_text(colour = "white", size =16),
        plot.title = element_text(colour = "white",  size =18),
        plot.subtitle = element_text(colour = "white",  size =14),
        axis.text = element_text(colour = "light grey", size =14),
        axis.title = element_text(colour = "white",  size =12),
        plot.background = element_rect(fill = "blue4"))


ggsave(filename = paste("True EV vs naive EV",format(Sys.time(), "%Y-%m-%d"), ".jpeg"), 
       width = 2740, height =1840, units ="px")
