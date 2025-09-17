library(tidyverse)
library(rio)
library(rcartocolor)
library(htmlwidgets)
library(priceR)

# loading df
df<-import("2014_2024_price_data.csv")


# adjusting prices for inflation using priceR

# price: the original dollar values
# from_date: the year those dollar values are from
# to_date: the reference year we want to adjust prices to (in this code 2024)
# country: "US" tells priceR to use U.S. inflation data

df$Cost_adj <- adjust_for_inflation(
  price     = df$Cost,
  from_date = df$Year,
  to_date   = 2024,
  country   = "US"
)


#viz

# Creating a ggplot theme for use in several figures throughout the code.
plot_theme = theme(text = element_text(size=11),
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.background = element_blank(),
                   plot.background = element_rect(fill = "transparent",colour = NA),
                   axis.text = element_text(color="black"),
                   axis.line = element_line(colour = "black"),
                   strip.text.y = element_text(angle = 0), axis.title.y = element_text(angle=90, hjust=.5, vjust=.5),
                   axis.title = element_text(lineheight=1.1), 
                   legend.position = "bottom", 
                   legend.key = element_rect(colour = "transparent",  fill = "transparent") 
) 


# Convert year to a numeric and removing
df$Year <- as.numeric(df$Year)

#Releveling sector
df<-df %>% 
  mutate(Sector = factor(Sector, levels=c("Residential","Commercial","Industrial"))) 
  
# creating color pallet for the states

safe_pal <- carto_pal(12, "Safe")


# PLOTTING 

# electricity plot adjusted for inflation
plot2_electric_inf <- ggplot(df[df$Type == "electricity", ], 
                         aes(x = Year, y = Cost_adj, color = State, shape = State)) +  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 2014:2024) +
  labs(
    title = "Electricity Price (cents/kwHr) from 2014 to 2024 by State/Sector (2024 $)",
    x = "Year",
    y = "Cost (2024 USD, inflation-adjusted)",
    color = "State",
    caption="Data Source: EIA"
  ) +
  facet_grid(.~Sector)+
  plot_theme+
  scale_fill_manual(values = safe_pal)+
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11)) +
  guides(shape = "none") 


# Making interactable
plotly::ggplotly(plot2_electric_inf)

# Saving
saveWidget(plotly::ggplotly(plot2_electric_inf), "plot1_electric_inflation_adjust.html", selfcontained = TRUE)


# Difference (and percent change) between 2014-2024 by state and sector
price_diff_df <- df %>%
  filter(Year %in% c(2014, 2024)&Type=="electricity") %>%
  group_by(State, Sector) %>%
  # Calculate difference: 2024 - 2014
  summarise(
    Cost_2014 = Cost_adj[Year == 2014],
    Cost_2024 = Cost_adj[Year == 2024],
    Diff = Cost_adj[Year == 2024] - Cost_adj[Year == 2014],
    `%Change` = ((Cost_adj[Year == 2024]/Cost_adj[Year == 2014])-1)*100
  )

# pivoting
wide_price_df<-price_diff_df %>%
  select(-c("Cost_2014","Cost_2024","Diff")) %>% 
  pivot_wider(
    names_from = Sector,
    values_from = `%Change` 
  )

# saving csv of inflation adjusted prices
write.csv(wide_price_df,"2014_2014_price_data_infl_adjust.csv", row.names = FALSE)


# not adjusted for inflation
# electricity plot
plot2_electric <- ggplot(df[df$Type == "electricity", ], 
                         aes(x = Year, y = Cost, color = State, shape = State)) +  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 2014:2024) +
  labs(
    title = "Electricity Price (cents/kwHr) from 2014 to 2024 by State/Sector",
    x = "Year",
    y = "Cost",
    color = "State",
    caption="Data Source: EIA"
  ) +
  facet_grid(.~Sector)+
  plot_theme+
  scale_fill_manual(values = safe_pal)+
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11)) +
  guides(shape = "none") 

# Making interactable
plotly::ggplotly(plot2_electric)

# Saving
saveWidget(plotly::ggplotly(plot2_electric), "plot1_electric.html", selfcontained = TRUE)


# nat gas plot
plot1_nat_gas <- ggplot(df[df$Type == "natural_gas", ], 
                        aes(x = Year, y = Cost, color = State, shape = State)) +  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 2014:2024) +
  labs(
    title = "Natural Gas Price ($/Mcf) from 2014 to 2024 by State/Sector",
    x = "Year",
    y = "Cost",
    color = "State",
    caption="Data Source: EIA"
  ) +
  facet_grid(.~Sector)+
  plot_theme+
  scale_fill_manual(values = safe_pal)+
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)) +
  guides(shape = "none") 
  
# Making interactable
plotly::ggplotly(plot1_nat_gas)

saveWidget(plotly::ggplotly(plot1_nat_gas), "natural_gas_plot.html", selfcontained = TRUE)

