library (data.table)
library(naniar)
library(WDI)
library(dplyr)
library(ggplot2)
library(geomtextpath)
library(caTools)
library(gbm)
library(rpart)
library(ipred) 
library(stats)
library(caret)
library(forecast)

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Reading the data
# soccer = fread(paste0(getwd(), "/soccer.csv"))
# zomato = fread(paste0(getwd(), "/zomato.csv"))
# houses = fread(paste0(getwd(), "/Indian_housing_Pune_data.csv"))
cotwo = fread(paste0(getwd(), "/Agrofood_co2_emission.csv")) # https://www.kaggle.com/datasets/alessandrolobello/agri-food-co2-emission-dataset-forecasting-ml/data
addit = WDI(indicator = "NY.GDP.MKTP.CD", start = 1990, end = 2020, extra = TRUE)
colnames(addit)[colnames(addit) == "year"] = "Year"
colnames(addit)[colnames(addit) == "country"] = "Country"
colnames(addit)[colnames(addit) == "NY.GDP.MKTP.CD"] = "GDP"
cotwo = cotwo %>% rename(Country = Area,
                         temp_change = `Average Temperature °C`,
                         savanna_fires = `Savanna fires`,
                         forest_fires = `Forest fires`,
                         crop_residues = `Crop Residues`,
                         rice_cultiv = `Rice Cultivation`,
                         drained_organ_soil = `Drained organic soils (CO2)`,
                         pesticides_man = `Pesticides Manufacturing`,
                         food_transport = `Food Transport`,
                         net_forest_conversion = `Net Forest conversion`,
                         food_household_cons = `Food Household Consumption`,
                         food_retail = `Food Retail`,
                         onfarm_el_use = `On-farm Electricity Use`,
                         food_packaging = `Food Packaging`,
                         agrifood_sys_waste_disp = `Agrifood Systems Waste Disposal`,
                         food_processing = `Food Processing`,
                         fertil_manuf = `Fertilizers Manufacturing`,
                         manure_applied_soils = `Manure applied to Soils`,
                         manure_left_pasture = `Manure left on Pasture`,
                         manure_management = `Manure Management`,
                         fires_org_soils = `Fires in organic soils`,
                         fires_hum_trop = `Fires in humid tropical forests`,
                         of_en_use = `On-farm energy use`,
                         rural_population = `Rural population`,
                         urban_population = `Urban population`,
                         total_pop_M = `Total Population - Male`,
                         total_pop_F = `Total Population - Female`)
cotwo$Country = replace(cotwo$Country, cotwo$Country == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")
cotwo$Country = replace(cotwo$Country, cotwo$Country == "United Republic of Tanzania", "Tanzania")
cotwo$Country = replace(cotwo$Country, cotwo$Country == "United States of America", "United States")
cotwo$Country = replace(cotwo$Country, cotwo$Country == "United States Virgin Islands", "Virgin Islands (U.S.)")
cotwo$Country = replace(cotwo$Country, cotwo$Country == "Bolivia (Plurinational State of)", "Bolivia")
cotwo$Country = replace(cotwo$Country, cotwo$Country == "Venezuela (Bolivarian Republic of)", "Venezuela")
cotwo$Country = replace(cotwo$Country, cotwo$Country == "Iran (Islamic Republic of)", "Iran")
cotwo$Country = replace(cotwo$Country, cotwo$Country == "Republic of Korea", "South Korea")
cotwo$Country = replace(cotwo$Country, cotwo$Country == "Democratic People's Republic of Korea", "North Korea")
cotwo$Country = replace(cotwo$Country, cotwo$Country == "United Kingdom", "UK")
addit$Country = replace(addit$Country, addit$Country == "Venezuela, RB", "Venezuela")
addit$Country = replace(addit$Country, addit$Country == "Congo, Dem. Rep.", "Congo")
addit$Country = replace(addit$Country, addit$Country == "Egypt, Arab Rep.", "Egypt")
addit$Country = replace(addit$Country, addit$Country == "Iran, Islamic Rep.", "Iran")
addit$Country = replace(addit$Country, addit$Country == "Korea, Rep.", "South Korea")
addit$Country = replace(addit$Country, addit$Country == "United Kingdom", "UK")



cotwo = merge(cotwo, addit, by = c("Country", "Year"))

# Overview about the data
head(cotwo)
summary(cotwo)
cotwo = cotwo[, -c("iso2c", "status", "capital", "lending", "lastupdated")]
cotwo$longitude = as.numeric(cotwo$longitude)
cotwo$latitude = as.numeric(cotwo$latitude)

# Check
rowSums(cotwo[, 3:25], na.rm = TRUE) == cotwo$total_emission
check = data.table("sum" = rowSums(cotwo[, 3:25], na.rm = TRUE),
                   "actual" = cotwo$total_emission)
sum(round(rowSums(cotwo[, 3:25], na.rm = TRUE), 1) == round(cotwo$total_emission, 1)) / length(round(rowSums(cotwo[, 3:25], na.rm = TRUE), 1) == round(cotwo$total_emission, 1))
rm(check)

# Missing values
gg_miss_var(cotwo)

# Feature engineering
cotwo = merge(x = cotwo, y = cotwo[, mean(temp_change), by = Year], by = "Year")
setorder(cotwo, Country, Year)
colnames(cotwo)[length(cotwo)] = "world_temp_change"
cotwo$gdp_capita = cotwo$GDP / (cotwo$total_pop_M + cotwo$total_pop_F)
cotwo$total_pop = cotwo$total_pop_M + cotwo$total_pop_F

# Main drivers of emissions
# 1. Direct
driver_direct = data.table("name" = colnames(cotwo)[3:25],
                           "sum" = 0)
for (i in 3:25){
  driver_direct[i-2,2] = sum(cotwo[,..i], na.rm = TRUE)
}

driver_direct = driver_direct[sum > 0]
driver_direct = driver_direct[order(-rank(sum), name)]
driver_direct$name = factor(driver_direct$name, levels = rev(as.character(driver_direct$name)))
non_grey_names = levels(driver_direct$name)[15:length(levels(driver_direct$name))]

# Update the plot
ggplot(driver_direct, aes(x="", y=sum, fill=name)) +
  geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
  coord_polar(theta = "y", start=0) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        legend.position = "right") +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Contributors") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("cyan2", "cadetblue", "dodgerblue", "darkorchid", "blueviolet", "blue", "#254290", "black", rep("grey", 14)), breaks = non_grey_names) +
  geom_text(aes(label = c(scales::percent(round(sum / sum(driver_direct$sum),2))[1:8], rep("",14)), x = 1.65),
            position = position_stack(vjust = 0.5)
            )

# 2. Indirect
ml_drivers = cotwo[, -c("of_en_use")]
ml_drivers$income = as.factor(ml_drivers$income)
ml_drivers$region = as.factor(ml_drivers$region)
ml_drivers$GDP = ml_drivers$GDP / 1000000000
ml_drivers$total_pop = ml_drivers$total_pop_F + ml_drivers$total_pop_M
ml_drivers$rural_population = ml_drivers$rural_population / (ml_drivers$urban_population + ml_drivers$rural_population)
ml_drivers$total_pop_M = ml_drivers$total_pop_M / (ml_drivers$total_pop_M + ml_drivers$total_pop_F)
ml_drivers = ml_drivers[complete.cases(ml_drivers)]
ml_drivers = cbind(Id = seq(from = 1, to = nrow(ml_drivers)), ml_drivers)
split = sample.split(ml_drivers$Id, SplitRatio = 0.7)
train = ml_drivers[split, ]
test = ml_drivers[!split, ]


ml_summary = data.table("true" = test$total_emission,
                        "lm" = 0,
                        "gbm" = 0)
# LM
lm_mod = lm(total_emission ~ Year + rural_population + total_pop_M + GDP + region + income + longitude + latitude + gdp_capita,
         data = train)
summary(lm_mod)
ml_summary$lm = predict(lm_mod, test)

rel_imp = calc.relimp(lm_mod, type = "lmg", importance = TRUE)
lm_summary = data.table("Feature" = rownames(as.data.frame(rel_imp@lmg)),
                        "Value" = as.numeric(rel_imp@lmg))

ggplot(lm_summary, aes(x = reorder(Feature, -Value), y = Value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.background = element_rect(fill = "white", colour="white"),
        axis.line = element_line(colour = "black"))  +
  labs(title = "Feature importance for lm",
       x = "Features",
       y = "Importance")


# Boosting
gb_mod = gbm(total_emission ~ Year + rural_population + total_pop_M + GDP + region + income + longitude + latitude + gdp_capita,
          data = train,
          distribution = "gaussian",
          n.trees = 1000, shrinkage = 0.01,
          interaction.depth = 4,
          bag.fraction = 0.7,
          n.minobsinnode = 5)
gb_summary = data.table("Feature" = summary(gb_mod)$var,
                        "Value" = summary(gb_mod)$rel.inf)

ggplot(gb_summary, aes(x = reorder(Feature, -Value), y = Value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.background = element_rect(fill = "white", colour="white"),
        axis.line = element_line(colour = "black"))  +
  labs(title = "Feature importance for bagging",
       x = "Features",
       y = "Importance")


ml_summary$gbm = predict(gb_mod, test)

# Bagging
ba_mod = bagging( 
  formula = total_emission ~ Year + rural_population + total_pop_M + GDP + region + income + longitude + latitude + gdp_capita, 
  data = train, 
  nbagg = 50,    
  coob = TRUE, 
  control = rpart.control(minsplit = 2, cp = 0,
                          min_depth=2) 
)
summary(ba_mod)
ml_summary$ba = predict(ba_mod, test)
ba_summary = varImp(ba_mod)

ggplot(ba_summary, aes(x = reorder(rownames(ba_summary), -Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.background = element_rect(fill = "white", colour="white"),
        axis.line = element_line(colour = "black")) +
  labs(title = "Feature importance for bagging",
       x = "Features",
       y = "Importance")

# Which countries contributed most to emissions?
em_overall = cotwo[, sum(total_emission), by = Year]
colnames(em_overall)[2] = "World"
em_regions = cotwo[, sum(total_emission), by = c("Year","region")]
em_regions = em_regions[complete.cases(em_regions),]

em_regions = em_regions %>%
  mutate(region = as.factor(region)) %>%
  spread(key = region, value = V1)

em_regions = merge(x = em_overall, em_regions, by = "Year")

em_regions = em_regions %>%
  gather(key = "variable", value = "value", -Year)

em_regions = em_regions[seq(0, nrow(em_regions), 2),] # delete every second row
colnames(em_regions)[2] = "Region"

ggplot(em_regions, aes(x = Year, y = value)) +
  geom_point(aes(color = Region)) +
  geom_line(aes(color = Region)) +
  scale_color_manual(values = c("South Asia" = "blue",
                                "Europe & Central Asia" = "red",
                                "Middle East & North Africa" = "green",
                                "East Asia & Pacific" = "yellow",
                                "Sub-Saharan Africa" = "purple",
                                "Latin America & Caribbean" = "brown",
                                "North America" = "orange")) +
  labs(title = "Emission development over time",
       x = "Year",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.line = element_line(colour = "black"))

# Treemap

treemap_data = cotwo[, sum(total_emission), by = Country]
colnames(treemap_data)[2] = "Sum"
treemap_data = merge(x = treemap_data, y = cotwo[,c("Country","region","iso3c","total_pop_M","total_pop_F")], by = "Country", all.x = TRUE)
treemap_data = as.data.frame(treemap_data[!duplicated(Country)])
treemap_data = treemap_data[complete.cases(treemap_data),]
treemap_data$emission_per_capita = treemap_data$Sum / (treemap_data$total_pop_M + treemap_data$total_pop_F)

ggplot(treemap_data["Sum" > 0, ], aes(area = Sum, fill = region, label = iso3c, subgroup = region)) + 
  geom_treemap(layout = "squarified") + 
  geom_treemap_text(place = "centre", size = 12) +
  scale_fill_manual(values = c("South Asia" = "blue",
                               "Europe & Central Asia" = "red",
                               "Middle East & North Africa" = "green",
                               "East Asia & Pacific" = "yellow",
                               "Sub-Saharan Africa" = "purple",
                               "Latin America & Caribbean" = "brown",
                               "North America" = "orange")) +
  labs(title = "Absolute contribution to CO2 emissions")

ggplot(treemap_data["Sum" > 0, ], aes(area = emission_per_capita, fill = region, label = iso3c, subgroup = region)) + 
  geom_treemap(layout = "squarified") + 
  geom_treemap_text(place = "centre", size = 12) +
  scale_fill_manual(values = c("South Asia" = "blue",
                               "Europe & Central Asia" = "red",
                               "Middle East & North Africa" = "green",
                               "East Asia & Pacific" = "yellow",
                               "Sub-Saharan Africa" = "purple",
                               "Latin America & Caribbean" = "brown",
                               "North America" = "orange")) +
  labs(title = "Relative (per capita) contribution to CO2 emissions")

# Development
emission_dev = data.table("Country" = cotwo$Country,
                          "Year" = cotwo$Year,
                          "emission" = cotwo$total_emission)
emission_dev = emission_dev[Year < 1993 | Year > 2017]
emission_dev$Year = ifelse(emission_dev$Year < 2000, "Beginning", "Ending")
emission_dev = emission_dev[,mean(emission), by = c("Country","Year")]
colnames(emission_dev)[3] = "Mean"
emission_dev2 = emission_dev[, diff(Mean), by = Country]
colnames(emission_dev2)[2] = "Abs"
emission_dev2 = merge(x = emission_dev2, y = emission_dev[, min(Mean), by = Country], by = "Country")
colnames(emission_dev2)[3] = "Min"
emission_dev2$Rel = emission_dev2$Abs / abs(emission_dev2$Min)

# Plotting
options(scipen = 999)

# Absolute
abs_plot = emission_dev2[order(emission_dev2$Abs),]
abs_plot = abs_plot[c(1:5, ((nrow(abs_plot)-4):nrow(abs_plot)))]
abs_plot$label = as.factor(abs_plot$Abs > 0)

ggplot(abs_plot, aes(x = reorder(Country, -Abs), y = Abs)) +
  geom_bar(stat = "identity", aes(fill = label)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.line = element_line(colour = "black"),
        legend.position="none") +
  labs(title = "Abs. change in emission for top and bottom countries",
       x = "Countries",
       y = "Change") +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "darkgreen"))

# Relative
rel_plot = emission_dev2[order(emission_dev2$Rel),]
rel_plot = rel_plot[c(1:5, ((nrow(rel_plot)-4):nrow(rel_plot)))]
rel_plot$label = as.factor(rel_plot$Rel > 0)

ggplot(rel_plot, aes(x = reorder(Country, -Rel), y = Rel)) +
  geom_bar(stat = "identity", aes(fill = label)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.line = element_line(colour = "black"),
        legend.position="none") +
  labs(title = "Rel. change in emission for top and bottom countries",
       x = "Countries",
       y = "Change") +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "darkgreen"))

# Which countries experience the biggest increase in temperature?

temp_regions = cotwo[, mean(temp_change), by = c("Year","region")]
temp_regions = temp_regions[complete.cases(temp_regions),]

temp_regions = temp_regions %>%
  mutate(region = as.factor(region)) %>%
  spread(key = region, value = V1)

temp_regions = data.table(merge(x = temp_regions, y = cotwo[,c("Year", "world_temp_change")], by = "Year", all.y = FALSE))
temp_regions = temp_regions[!duplicated(Year),]

temp_regions = temp_regions %>%
  gather(key = "variable", value = "value", -Year)

temp_regions = temp_regions[seq(0, nrow(temp_regions), 3),] # delete every second row
colnames(temp_regions)[2] = "Region"

ggplot(temp_regions, aes(x = Year, y = value)) +
  geom_point(aes(color = Region)) +
  geom_line(aes(color = Region, size = ifelse(Region == "world_temp_change", 0.5, 0.1))) +
  scale_color_manual(values = c("South Asia" = "blue",
                                "Europe & Central Asia" = "red",
                                "Middle East & North Africa" = "green",
                                "East Asia & Pacific" = "yellow",
                                "Sub-Saharan Africa" = "purple",
                                "Latin America & Caribbean" = "brown",
                                "North America" = "orange",
                                "world_temp_change" = "black")) +
  labs(title = "Emission development over time",
       x = "Year",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.line = element_line(colour = "black")) +
  guides(size = "none")

temp_regions2 = cotwo[, mean(temp_change), by = "region"]
colnames(temp_regions2)[2] = "Average"
temp_regions2 = temp_regions2[complete.cases(temp_regions2),]
de = data.frame("World",mean(cotwo$world_temp_change))
names(de) = c("region","Average")
temp_regions2 = rbind(temp_regions2, de)
temp_regions2$class = ifelse(temp_regions2$Average > as.numeric(temp_regions2[region == "World",2]), "above", "below")
temp_regions2$class = ifelse(temp_regions2$Average == as.numeric(temp_regions2[region == "World",2]), "World", temp_regions2$class)

ggplot(temp_regions2, aes(x = reorder(region, -Average), y = Average, fill = class)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("above" = "red",
                                "World" = "grey",
                                "below" = "green")) +
  labs(title = "Temperature increase in regions",
       x = "Region",
       y = "Temperature increase (K)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.line = element_line(colour = "black")) +
  guides(fill = "none")

# Which countries see the biggest increase in fires?
fire_data = cotwo[, c("Year", "Country", "region", "savanna_fires", "forest_fires", "fires_org_soils", "fires_hum_trop", "temp_change", "world_temp_change")]
fire_data$sum_fire = rowSums(fire_data[, 4:7], na.rm = TRUE)
fire_regions = fire_data[, mean(sum_fire), by = c("Year","region")]
fire_regions = fire_regions[complete.cases(fire_regions),]

ggplot(fire_regions, aes(x = Year, y = V1)) +
  geom_point(aes(color = region)) +
  geom_line(aes(color = region)) +
  scale_color_manual(values = c("South Asia" = "blue",
                                "Europe & Central Asia" = "red",
                                "Middle East & North Africa" = "green",
                                "East Asia & Pacific" = "yellow",
                                "Sub-Saharan Africa" = "purple",
                                "Latin America & Caribbean" = "brown",
                                "North America" = "orange",
                                "world_temp_change" = "black")) +
  labs(title = "Emission development over time",
       x = "Year",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.line = element_line(colour = "black")) +
  guides(size = "none")

fire_data2 = data.table(fire_data[, -c("temp_change", "world_temp_change", "sum_fire", "Year", "Country")] %>% gather(key = "variable", value = "value", -region))
fire_data2 = fire_data2[, sum(value, na.rm = TRUE), by = c("variable","region")]
fire_data2 = fire_data2[complete.cases(fire_data2), ]
colnames(fire_data2)[3] = "Value"

ggplot(fire_data2, aes(area = Value, fill = region, label = variable, subgroup = region)) + 
  geom_treemap(layout = "squarified") + 
  geom_treemap_text(place = "centre", size = 12) +
  scale_fill_manual(values = c("South Asia" = "blue",
                               "Europe & Central Asia" = "red",
                               "Middle East & North Africa" = "green",
                               "East Asia & Pacific" = "yellow",
                               "Sub-Saharan Africa" = "purple",
                               "Latin America & Caribbean" = "brown",
                               "North America" = "orange")) +
  labs(title = "Absolute contribution to CO2 emissions")

# Forecasting
forecasting_data = ts(data.table("Year" = unique(cotwo$Year),
                                 "temp" = unique(cotwo$world_temp_change),
                                 "emission" = cotwo[, sum(total_emission), by = Year]$V1))

ses(forecasting_data[,2], h = 12)
plot(forecasting_data[,2:3])

arima_temp = auto.arima(forecasting_data[,2])
fc_temp = forecast(arima_temp, h = 10)
plot_data = data.table("Year" = c(forecasting_data[,1], seq(2021,2030,1)),
                       "old" = c(forecasting_data[,2], fc_temp$mean),
                       "Lower 80%" = c(rep(NA, nrow(forecasting_data)), fc_temp$lower[1:10]),
                       "Lower 95%" = c(rep(NA, nrow(forecasting_data)), fc_temp$lower[11:20]),
                       "Upper 80%" = c(rep(NA, nrow(forecasting_data)), fc_temp$upper[1:10]),
                       "Upper 95%" = c(rep(NA, nrow(forecasting_data)), fc_temp$upper[11:20]))

plot_data = plot_data %>% gather(key = "variable", value = "value", -Year)

ggplot(plot_data, aes(x = Year, y = value)) +
  geom_line(aes(color = variable)) +
  scale_color_manual(values = c("black" = "grey",
                                "Lower 80%" = "lightblue",
                                "Upper 80%" = "lightblue",
                                "Lower 95%" = "blue",
                                "Upper 95%" = "blue")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.line = element_line(colour = "black")) +
  labs(title = "Temperature increase forecast",
       x = "Year",
       y = "temperature increase [K]") +
  guides(color = guide_legend(title = "Confidence level"))

# Emission
arima_em = auto.arima(forecasting_data[,3])
fc_em = forecast(arima_em, h = 10)

plot_data = data.table("Year" = c(forecasting_data[,1], seq(2021,2030,1)),
                       "old" = c(forecasting_data[,3], fc_em$mean),
                       "Lower 80%" = c(rep(NA, nrow(forecasting_data)), fc_em$lower[1:10]),
                       "Lower 95%" = c(rep(NA, nrow(forecasting_data)), fc_em$lower[11:20]),
                       "Upper 80%" = c(rep(NA, nrow(forecasting_data)), fc_em$upper[1:10]),
                       "Upper 95%" = c(rep(NA, nrow(forecasting_data)), fc_em$upper[11:20]))

plot_data = plot_data %>% gather(key = "variable", value = "value", -Year)

ggplot(plot_data, aes(x = Year, y = value)) +
  geom_line(aes(color = variable)) +
  scale_color_manual(values = c("black" = "grey",
                                "Lower 80%" = "lightblue",
                                "Upper 80%" = "lightblue",
                                "Lower 95%" = "blue",
                                "Upper 95%" = "blue")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.line = element_line(colour = "black")) +
  labs(title = "Emission forecast",
       x = "Year",
       y = "Emission forecast") +
  guides(color = guide_legend(title = "Confidence level"))




# Worldmap
world = map_data("world")

world_data = cotwo[, sum(total_emission), by = Country]
colnames(world_data)[length(world_data)] = "Emission"
world_data = merge(x = world_data, y = cotwo[, mean(total_pop), by = Country])
colnames(world_data)[length(world_data)] = "Mean_pop"

world_data$Country = replace(world_data$Country, world_data$Country == "United States", "USA")
world$region = replace(world$region, world$region == "Democratic Republic of the Congo", "Congo")
world$region = replace(world$region, world$region == "Russia", "Russian Federation")

world_data = merge(x = world_data, y = world, by.x = "Country", by.y = "region")
world_data$Em_per_capita = world_data$Emission / world_data$Mean_pop
world_data = world_data[!duplicated(Country)]

ggplot(data = world_data[Mean_pop > 1000000], mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Em_per_capita)) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
  ggtitle("Global Human Development Index (HDI)") +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5)
  )

# Temperature
world_data = cotwo[, mean(temp_change), by = Country]
colnames(world_data)[length(world_data)] = "Temp"

world_data$Country = replace(world_data$Country, world_data$Country == "United States", "USA")
world$region = replace(world$region, world$region == "Democratic Republic of the Congo", "Congo")
world$region = replace(world$region, world$region == "Russia", "Russian Federation")

world_data = merge(x = world_data, y = world, by.x = "Country", by.y = "region")
world_data = world_data[!duplicated(Country)]

ggplot(data = world_data, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Temp)) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
  ggtitle("Global Human Development Index (HDI)") +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5)
  )



















