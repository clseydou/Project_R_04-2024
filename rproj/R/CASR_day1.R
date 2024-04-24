summary(ins)
fcts<-factor(ins$sex, levels=c("male", "female"))
table(ins$sex, fcts)

table(ins$region)
fctr <- factor(ins$region, levels=c("northeast", "northwest", "southeast", "southwest"))
table(ins$region, fctr)

child<- ifelse(ins$children<2, 0, 1)
table(child, ins$children)

sm<- ifelse(ins$smoker=="yes", 1,0)
table(sm, ins$smoker)

library(lubridate)
d<-as_date(ins$date)
d1<- d+months(6)

"----------------"

dateb <- ebola %>% arrange(Date)


library(lubridate)
library(dplyr)
library(ggplot2)

date<-as.Date(ebola$Date, format="%d/%m/%Y")
eb1 <- ebola %>% select(date = Date, country = Country, cum_conf_cases = Cum_conf_cases) %>% 
  filter(date <= ymd("2015-03-31") & 
           (country == "Guinea" | country ==  "Liberia" | country == "Sierra Leone"))
p1<- ggplot(data = eb1, 
                                  mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_point()
p1

p2 <- ggplot(data = eb1, 
                             mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_line(aes(group = country))
p2

p3 <- ggplot(data = eb1, 
                            mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_col(position = "stack")
p3

ggsave("plot_covid.png", plot = p3, width = 8, height = 6, units = "in", dpi = 300)

p4 <- ggplot(data = eb1, 
                              mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_point(alpha = 0.7, colour = "blue", fill = "green", 
             shape = 22, size = 1.5, stroke = 1.5) 
p4

p5 <- ggplot(data = eb1, 
                             mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_line(mapping = aes(group = country), 
            alpha = 0.7, colour = "blue", linetype = "dashed", linewidth = 1.5)
p5

p6 <- ggplot(data = eb1, 
                            mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_col(alpha = 0.7, colour = "blue", fill = "green", 
           linetype = "solid", linewidth = 0.1, position = "stack", width = 0.7)
p6

p7 <- ggplot(data = eb1, 
                              mapping = aes(x = date, y = cum_conf_cases, fill = country, colour = country)) + 
  geom_point(alpha = 0.7, shape = 22, size = 1.5, stroke = 1.5) 
p7

p8 <- ggplot(data = eb1, 
                             mapping = aes(x = date, y = cum_conf_cases, colour = country)) + 
  geom_line(mapping = aes(group = country), 
            alpha = 0.7, linetype = "dashed", linewidth = 1.5)
p8

p9 <- ggplot(data = eb1, 
                            mapping = aes(x = date, y = cum_conf_cases, fill = country, colour = country)) + 
  geom_col(alpha = 0.7, linetype = "solid", 
           linewidth = 0.1, position = "stack", width = 0.7)
p9

p10 <- ggplot(data = eb1, 
                              mapping = aes(x = date, y = cum_conf_cases, fill = country, colour = country)) + 
  geom_point(alpha = 0.7, shape = 22, size = 1.5, stroke = 1.5) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")
p10

p11 <- ggplot(data = eb1, 
                             mapping = aes(x = date, y = cum_conf_cases, colour = country)) + 
  geom_line(mapping = aes(group = country), 
            alpha = 0.7, linetype = "dashed", linewidth = 1.5) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")
p11

p12 <- ggplot(data = eb1, 
                            mapping = aes(x = date, y = cum_conf_cases, fill = country, colour = country)) + 
  geom_col(alpha = 0.7, linetype = "solid", 
           linewidth = 0.1, position = "stack", width = 0.7) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")
p12

p13 <- ggplot(data = eb1, 
                             mapping = aes(x = date, y = cum_conf_cases, colour = country)) + 
  geom_line(mapping = aes(group = country), 
            alpha = 0.7, linetype = "dashed", linewidth = 1.5) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                      labels = c("GIN", "LBR", "SLE")) +
  scale_x_date(breaks = ymd(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
               labels = c("29 August", "1 October", "1 December", "1 February", "1 April"),
               limits = ymd(c("2014-08-28", "2015-04-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 10000, by = 2500),
                     limits = c(0, 10000)) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")
p13

install.packages("unibeCols")
library(unibeCols)

library(ggplot2)
p14 <- ggplot( ins , aes(x = smoker, y = charges ) ) + 
  geom_boxplot(  ) + 
  ylab( "Charges ($)" ) + 
  coord_flip()



