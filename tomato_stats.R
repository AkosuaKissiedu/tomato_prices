
library(readxl)
library(dplyr) 
library(ggplot2)
library(lubridate)
library(gridExtra)


setwd("C:/Users/afari/Desktop/self learn")

################################################################################################################################################################

#dataset is too large to upload, find data at https://data.humdata.org/dataset/wfp-food-prices
#import dataset
food_data <- read_excel("wfpvam_foodprices.xlsx")

# extract the food items that come from Ghana 
datasample <- food_data %>% filter(adm0_name == "Ghana")

fufu_lightsoup_mixed_produce <- datasample %>% filter(cm_name == "Tomatoes (local) - Retail" | cm_name ==  "Tomatoes (navrongo) - Retail" | 
                                                        cm_name == "Onions - Retail" |
                                                        cm_name ==  "Eggplants - Retail" | cm_name ==  "Meat (chicken) - Retail" | 
                                                        cm_name ==  "Peppers (fresh) - Retail" |
                                                        cm_name ==  "Cassava - Retail" | cm_name ==  "Plantains (apem) - Retail" |
                                                        
                                                        cm_name == "Tomatoes (local) - Wholesale" | cm_name ==  "Tomatoes (navrongo) - Wholesale" | 
                                                        cm_name == "Onions - Wholesale" |
                                                        cm_name ==  "Eggplants - Wholesale" | cm_name ==  "Meat (chicken) - Wholesale" | 
                                                        cm_name ==  "Peppers (fresh) - Wholesale" |
                                                        cm_name ==  "Cassava - Wholesale" | cm_name ==  "Plantains (apem) - Wholesale")


###############################################################################################################################################################
###Preparing data for retail observations 

#filter out the food items sold on a retail basis 
Ghana_commod_retail_prices <- datasample %>% filter(pt_name == "Retail")



#filter out the ingredients (on retail basis) used in the preparation of the dish 
fufu_lightsoup <- Ghana_commod_retail_prices %>% filter(cm_name == "Tomatoes (local) - Retail" | cm_name ==  "Tomatoes (navrongo) - Retail" | 
                                                          cm_name == "Onions - Retail" |
                                                          cm_name ==  "Eggplants - Retail" | cm_name ==  "Meat (chicken) - Retail" | 
                                                          cm_name ==  "Peppers (fresh) - Retail" |
                                                          cm_name ==  "Cassava - Retail" | cm_name ==  "Plantains (apem) - Retail")



#assessing this in the greater accra region based on the narrative about how expensive food in this particular region is
fufu_lightsoup_accra <- fufu_lightsoup %>% filter(adm1_name == "Greater Accra")

#create new date variable to track the prices at each given time in the data set 
fufu_lightsoup_accra <- fufu_lightsoup_accra %>%
  mutate(month_year = paste( fufu_lightsoup_accra$mp_month,"/",fufu_lightsoup_accra$mp_year))

fufu_lightsoup_accra$month_year <- as.Date(parse_date_time(fufu_lightsoup_accra$month_year, c('ym','my')))


###############################################################################################################################################################
### Preparing data for Wholesale observations 

#filter out the food items sold on a wholesale basis 
Ghana_commod_wholesale_prices <- datasample %>% filter(pt_name == "Wholesale")

#filter out the ingredients (on wholesale basis) used in the preparation of the dish 
fufu_lightsoup_wholesale <- Ghana_commod_wholesale_prices %>% filter(cm_name == "Tomatoes (local) - Wholesale" | cm_name ==  "Tomatoes (navrongo) - Wholesale" | 
                                                                       cm_name == "Onions - Wholesale" |
                                                                       cm_name ==  "Eggplants - Wholesale" | cm_name ==  "Meat (chicken) - Wholesale" | 
                                                                       cm_name ==  "Peppers (fresh) - Wholesale" |
                                                                       cm_name ==  "Cassava - Wholesale" | cm_name ==  "Plantains (apem) - Wholesale")

#assessing this in the greater accra region based on the narrative about how expensive food in this particular region is
fufu_lightsoup_accra_wholesale <- fufu_lightsoup_wholesale %>% filter(adm1_name == "Greater Accra")


#create new date variable to track the prices at each given time in the data set 
fufu_lightsoup_accra_wholesale <- fufu_lightsoup_accra_wholesale %>%
  mutate(month_year = paste( fufu_lightsoup_accra_wholesale$mp_month,"/",fufu_lightsoup_accra_wholesale$mp_year))

fufu_lightsoup_accra_wholesale$month_year <- as.Date(parse_date_time(fufu_lightsoup_accra_wholesale$month_year, c('ym','my')))

##############################################################################################################################################################
##retail - tomato
#Separating the ingredient (tomatoes)
fufu_lightsoup_accra <- fufu_lightsoup_accra %>%
  mutate(tomato_gen = ifelse(cm_name == "Tomatoes (local) - Retail" | cm_name == "Tomatoes (navrongo) - Retail", "tomato" , cm_name))

tomato_only <- fufu_lightsoup_accra %>%
  filter(tomato_gen == "tomato")


#ordering tomato observations by date  
tomato_only <- tomato_only[order(tomato_only$month_year),]

#calculating median for tomato observations as dates have multiple prices allocated
tomato_only_medians <- tomato_only %>%
  group_by(month_year) %>%
  summarise_at(vars(mp_price), list(price_medians = median))


#drawing line plot to visualize price changes by date for tomatoes sold on retail basis 
tomato_price_growth <- ggplot(data = tomato_only_medians, mapping= aes(as.Date(month_year), price_medians)) +
  geom_line(color = "#0072B2",size = 2) + geom_point(color = "#0072B2", size = 4 ) + 
  geom_smooth( color = "#ff4162",method = "loess", se = FALSE, size =1) +
  labs(title = "Retail Pricing of Tomatoes from 2019 to 2021",
       subtitle = "Tracking the changes in the retail pricing of tomatoes in the Greater Accra region from 2019 to 2021",
       x = "Dates", y = "Toamato prices in GHS") + 
  theme(plot.title = element_text(color = "#0072B2", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1), 
        panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1)) 

################################################################################

##wholesale - tomato
#Separating the ingredient (tomatoes)
fufu_lightsoup_accra_wholesale <- fufu_lightsoup_accra_wholesale %>%
  mutate(tomato_gen_wholesale = ifelse(cm_name == "Tomatoes (local) - Wholesale" | 
                                         cm_name == "Tomatoes (navrongo) - Wholesale", "tomato" , cm_name))

tomato_only_wholesale <- fufu_lightsoup_accra_wholesale %>%
  filter(tomato_gen_wholesale == "tomato")

#ordering tomato observations by date  
tomato_only_wholesale <- tomato_only_wholesale[order(tomato_only_wholesale$month_year),]

#calculating median for tomato observations as dates have multiple prices allocated
tomato_only_medians_wholesale <- tomato_only_wholesale %>%
  group_by(month_year) %>%
  summarise_at(vars(mp_price), list(price_medians = median))

#drawing line plot to visualize price changes by date for tomatoes sold on retail basis 
tomato_price_growth_wholesale <- ggplot(data = tomato_only_medians_wholesale, mapping= aes(as.Date(month_year), price_medians)) +
  geom_line(color = "#0072B2",size = 2) + geom_point(color = "#0072B2", size = 4 ) + 
  geom_smooth( color = "#ff4162",method = "loess", se = FALSE) +
  labs(title = "Wholesale Pricing of Tomatoes",
       subtitle = "Tracking the changes in the Wholesale pricing of tomatoes",
       x = "Dates", y = "Toamato prices in GHS") + 
  theme(plot.title = element_text(color = "#0072B2", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1), 
        panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1)) 

#################################################################################
##vendor count, retail vs wholesale - for all regions 

tomato_wholesale_retail <- fufu_lightsoup_mixed_produce %>%
  filter(cm_name == "Tomatoes (local) - Retail" | cm_name == "Tomatoes (navrongo) - Retail" | cm_name == "Tomatoes (local) - Wholesale" | 
           cm_name == "Tomatoes (navrongo) - Wholesale")

tomato_wholesale_retail <- tomato_wholesale_retail %>%
  mutate(cm_name = ifelse(cm_name == "Tomatoes (local) - Wholesale" | cm_name == "Tomatoes (navrongo) - Wholesale", "Tomato - Wholesale" ,
                          ifelse(cm_name == "Tomatoes (local) - Retail" | cm_name == "Tomatoes (navrongo) - Retail", "Tomato _Retail", cm_name)))


##Bargraph
p9 <- ggplot(data = tomato_wholesale_retail, aes( x = mp_year,  fill = cm_name)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  scale_fill_manual(values=c("#31356e", "#0072B2")) +
  labs(title = "Wholesale and Retail Tomato Vendors from 2019 to 2021",
       subtitle = "Tracking the changes in the total number of wholesale and retail tomato vendors from 2019 to 2021",
       x = "Years", y = "Vendor Count", fill = "Categories") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),
        panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))

################################################################################
##market share by region 

p10 <- ggplot(data=tomato_wholesale_retail, aes(y=adm1_name , fill=cm_name)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values=c("#d64946", "#f49678")) + 
  labs(title = "Wholesale and Retail Tomato Vendors from 2019 to 2021",
       subtitle = "Comparing  wholesale and retail tomato vendors across regions.",
       x = "Vendor Count", y = "Region", fill = "Categories") + 
  theme(plot.title = element_text(color = "#923b39", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),
        panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))

################################################################################
#understanding the spread of produce prices in the greated accra region for wholesale and retail tomatoes

#drawing line plot to visualize price changes by year for tomatoes sold on retail basis in Accra
tomato_only_price_growth <- ggplot(data = tomato_only, mapping= aes(x = as.factor(mp_year), y = mp_price, fill = as.factor(mp_year))) +
  geom_boxplot(outlier.colour= "#f4cb78", outlier.shape=8,outlier.size=4) + 
  guides(fill = guide_legend(title = "Years")) +
  scale_fill_manual(values=c("#d64946", "#fad8d5", "#f49678")) + 
  labs(title = "Retail Pricing of Tomatoes from 2019 to 2021",
       subtitle = "Assesing retail price changes for tomatoes in the greater Accra region",
       x = "Years", y = "Tomato prices in GHS") + 
  theme(plot.title = element_text(color = "#d64946", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1), 
        panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1) ) 



#drawing line plot to visualize price changes by date for tomatoes sold on retail basis 
tomato_price_growth_wholesale_accra <- ggplot(data = tomato_only_wholesale, 
                                              mapping= aes(x = as.factor(mp_year), y = mp_price, fill = as.factor(mp_year))) +
  geom_boxplot(outlier.colour= "#f4cb78", outlier.shape=8,outlier.size=4) + 
  guides(fill = guide_legend(title = "Years")) + 
  scale_fill_manual(values=c("#d64946", "#fad8d5", "#f49678")) + 
  labs(title = "Wholesale Pricing of Tomatoes from 2019 to 2021",
       subtitle = "Assesing wholesale price changes for tomatoes in the greater Accra region",
       x = "Dates", y = "Tomato prices in GHS") +
  theme(plot.title = element_text(color = "#d64946", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1), 
        panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1) ) 



###############################################################################
p1 <- grid.arrange (tomato_price_growth,tomato_price_growth_wholesale, nrow = 2 )
p2 <-  grid.arrange (tomato_only_price_growth,tomato_price_growth_wholesale_accra, nrow = 2 )
p3 <-  grid.arrange (p9)
p4 <-  grid.arrange (p10)




