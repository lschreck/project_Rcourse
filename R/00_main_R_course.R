#R course
 
#Clear existing data and graphics
rm(list=ls())
graphics.off()
  
#load libraries
library(stringi)
library(tidyverse)
library(readr)
library(here)
library(cowplot)
library(lubridate)
library(ggplot2)
library(devtools)

remotes::install_github("CTU-Bern/unibeCols")

#Unibe functions
unibeCols::unibeApricot()
unibeCols::unibeSaphireS()

#insurance
#read file
insurance <- read_csv(here("data", "raw", "insurance_with_date.csv"))
str(insurance)

#base R
#insurance <- read.csv(here("data", "raw", "insurance_with_date.csv"))
#str(insurance)

###Monday morning
##exercise Monday morning

#make factors out of the sex and region
insurance$sex <- factor(insurance$sex)
insurance$region <- factor(insurance$region)
class(insurance$sex)

#make a logical indicator for "has more than 2 children"
#and "smokes"
insurance$children2 <- insurance$children>2
insurance$smokes <- insurance$smoker=="yes"

#add 6 months to the date variable
insurance$date <- dmy(insurance$date)
class(insurance$date)
insurance$date6m <- insurance$date+months(6)

#solution
reformatted <- insurance |> 
  mutate(
    across(c(sex, region), factor),
    # sex = factor(sex),
    # region = factor(region),
    gt2_children = children > 2,
    smokes = smoker == "yes",
    date_6m = date + months(6)
    # date_6m = date + 30.4 * 6
  )

#Monday afternoon
ebola <- read_csv(here("data", "raw", "ebola.csv"))
ebola$Date <- dmy(ebola$Date)

#cumulative confirmed ebola cases in three different countries up to 31.5.15
ebola_plot <- ebola |> filter(Date <= "2015-05-31", Country == c("Guinea", "Liberia",
                                                                "Mali")) |>
  select(-c(Cum_susp_cases, Cum_conf_death))

#play around with ggplot
ggplot(ebola_plot, aes(x=Date, y= Cum_conf_cases, color=Country))+
  geom_point()

ggplot(ebola_plot, aes(x=Date, y= Cum_conf_cases))+
  geom_line(mapping=aes(group=Country))

ggplot(ebola_plot, aes(x=Date, y= Cum_conf_cases))+
  geom_col(position="stack")

ggplot(ebola_plot, aes(x=Date, y= Cum_conf_cases, color=Country, fill=Country))+
  geom_point(alpha=0.7, shape=21, size=4.5)+
  geom_line(aes(group=Country),color="black")

ggplot(ebola_plot, aes(x=Date, y= Cum_conf_cases, color=Country, fill=Country))+
  geom_point(alpha=0.7, shape=21, size=4.5)+
  geom_line()

ggplot(ebola_plot, aes(x=Date, y= Cum_conf_cases, color=Country, fill=Country))+
  geom_point(alpha=0.7, shape=21, size=4.5, color="#EE7402", fill="#b0c7d9")+
  geom_line(color="#b0c7d9")

ggplot(ebola_plot, aes(x=Date, y= Cum_conf_cases, color=Country, fill=Country))+
  geom_col(position="stack",alpha=0.7, size=4.5, width=0.5)

#define that everything between pdf and dev.off needs to be saved as PDF
#there you can also define width
#pdf()

b <- ggplot(ebola_plot, aes(x=Date, y=Cum_conf_cases, fill=Country))+
  geom_point(shape=21, size=3, color="blue")+
  xlab(label = "Time")+
  ylab(label = "# of confirmed cases")+
  ggtitle(label = "Confirmed covid cases in 3 cantons")+
  scale_fill_manual(values = c(unibeCols::unibeMustardS()[1],
                               unibeCols::unibeVioletS()[1],
                               unibeCols::unibeBrownS()[1]))+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_y_continuous(breaks=seq(from=0, to=3500, by=500))+ #limits defines the range
  facet_grid(cols=vars(Country))

#dev.off()

a <- ggplot(ebola_plot, aes(x=Date, y=Cum_conf_cases, fill=Country))+
  geom_point()+
  scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", 
                                  "2015-02-01","2015-04-01")),
               labels = c("29 August", "1 October", "1 December", "1 February", "1 April"),
               limits = as.Date(c("2014-05-30", "2016-05-02")))+
  theme_bw()

library(cowplot)

#combine different graphs in one grid
plot_covid_point_grid <- plot_grid(plotlist = 
        list(a,b),
        labels = c("V1", "V2"), label_size = 12, nrow = 2)
plot_covid_point_grid


#Clear existing data and graphics
rm(list=ls())
graphics.off()

#reproduce graphs
insurance <- read_csv(here("data","raw", "insurance_with_date.csv"))
insurance <- insurance |>mutate(children = as.factor(children))

#first graph
ggplot(insurance, aes(x=bmi, color=sex, fill=sex))+
geom_density(alpha=0.4)+
  theme(legend.position = "bottom")+
  xlab(expression(paste(label= "BMI (kg/",m^2,")")))+
  theme(legend.title = element_blank())+
  scale_fill_manual(labels=c("Female", "Male"),values=c(unibeCols::unibeApricot()[1],
                                                        unibeCols::unibeApricot()[3]))+
  scale_color_manual(labels=c("Female", "Male"),values=c(unibeCols::unibeApricot()[1],
                                                        unibeCols::unibeApricot()[3]))

#second graph: not done yet
ggplot(insurance, aes(x=charges, color=sex, fill=sex))+
  geom_histogram()+
  geom_density()+
  theme(legend.position = "top")+
  geom_vline(aes(xintercept=median(charges), color=unibeCols::unibeRedS()[1]))
  
#third graph
ggplot(insurance, aes(x=age, y=bmi, fill=smoker, color=smoker))+
  geom_quantile()+
  geom_point()+
  ylab(expression(paste(label= "BMI (kg/",m^2,")")))+
  xlab(label = "Age")

#forth graph
ggplot(insurance, aes(x=smoker, y=charges))+
  geom_violin()

#fifth graph
ggplot(insurance, aes(x=charges, y=smoker))+
  geom_boxplot()

