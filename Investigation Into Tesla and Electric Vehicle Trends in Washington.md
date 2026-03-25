# **Investigation Into Tesla and Electric Vehicle Trends in Washington**
### **Sebastian Ortuno Barrero**

Please note this is not a brand-biased project. This project was conducted to further develop my data analysis and visualization skills using real-world data.

The growth of electric vehicles (EVs) has become a major topic in recent years as governments, companies, and consumers shift toward more sustainable transportation options. In this R Markdown project, I aimed to focus on the data itself rather than opinions, using data analysis and visualization techniques to explore Tesla’s position in the EV market and compare it with other electric vehicle manufacturers. The goal was to better understand market share, sales trends, pricing, vehicle performance, and adoption patterns across Washington State.

The dataset used in this project contains information on electric vehicles registered in Washington, including details such as vehicle type, model year, electric range, MSRP, and utility providers. It is important to note that this dataset may not include all electric vehicles and contains some missing or incomplete values, particularly for MSRP, which may affect certain analyses.

```{r setup, include=FALSE}
library(dplyr)#Loading dplyr library
library(tidyr)#Loading tidyr libary
library(tidyverse) #Loading tidyverse library
library(ggplot2) #Loading tidyverse library 
library(reticulate) #Loading reticulate library
library(scales)
```

```{r}
#Loading the Electric_Vehicle_Population_Data.csv

df<- read.csv("C:/Users/sebas/OneDrive/Documents/NEC MASTERS/Projects Portfolio/Projects Portfolio/R Projects/Tesla Project/Electric_Vehicle_Population_Data.csv") #Loading the data

df_tesla_prices<-read.csv("C:/Users/sebas/OneDrive/Documents/NEC MASTERS/Projects Portfolio/Projects Portfolio/R Projects/Tesla Project/Tesla_Current_Base_Prices.csv")
```

## **1. Cleaning data**

In this step, I cleaned the dataset by standardizing column names, simplifying key variables, and preparing the data for analysis. I replaced periods in column names with underscores to make them easier to work with in R. I then created a new variable called Tesla to classify each vehicle as either TESLA or OTHER, allowing me to focus the analysis on Tesla compared to all other brands. Finally, I converted Base_MSRP into a numeric format so it could be properly used in calculations, summaries, and visualizations.

```{r}
cleaned_df <- df %>%                                    # Start with original dataset (df)
  rename_all(~ gsub("\\.", "_", .)) %>%         # Replace dots (.) in column names with (_)
  mutate(                                                # Modify variables
    Base.MSRP = as.numeric(Base_MSRP),                  # Convert Base_MSRP to numeric format
    Tesla = ifelse(Make == 'TESLA', 'TESLA', 'OTHER')   # Label rows as TESLA or OTHER based on Make
  )
```

## **2. What is Tesla’s market share compared to other electric vehicle manufacturers in Washington?**

```{r}
total_vehicles <- nrow(cleaned_df)  # Count total number of vehicles in dataset

tesla_vehicles <- cleaned_df %>% 
  filter(Tesla == 'TESLA') %>%    # Keep only Tesla vehicles
  nrow()        # Count number of Tesla vehicles

tesla_market_share <- round(tesla_vehicles / total_vehicles * 100, 1) # Calculate Tesla market share (%), rounded to 1 decimal

# Create data for visualization
market_share <- data.frame(
  category = c("Tesla", "Other"),                     # Define categories
  value = c(tesla_market_share, 100 - tesla_market_share)     # Tesla vs remaining market share
)

# Plot donut chart
ggplot(market_share, aes(x = 2, y = value, fill = category)) +  # Initialize plot with data and aesthetics
  geom_col(width = 1) +                                # Create bar chart (base for donut)
  coord_polar(theta = "y") +                            # Convert bar chart into donut
  geom_text(aes(label = paste0(value, "%")),            # Add percentage labels
            position = position_stack(vjust = 0.5),    # Center labels within each segment
            size = 5) +                                 # Set label size
  labs(
    title = "Tesla Market Share in Washington (%)"      # Add chart title (fixed typo)
  ) +
  theme_void() +                                        # Remove background, axes, and gridlines
  theme(legend.title = element_blank())                 # Remove legend title
```
<img width="959" height="590" alt="image" src="https://github.com/user-attachments/assets/32c4eec8-768b-4a5f-b965-873d451cd1ec" />


Based on the results, I found that Tesla represents 42.9% of the vehicles in the dataset, while all other brands together account for 57.1%. This shows that Tesla has a strong presence in Washington’s EV market, but the majority is still made up of other manufacturers combined.



## **3. Which Tesla models have sold the most, how much revenue might they have generated, and how have Tesla vehicle sales changed over time?**

```{r}
make_model_sold <- cleaned_df %>% 
  filter(Tesla == "TESLA") %>%  # Keep only Tesla vehicles
  group_by(Make, Model) %>%     # Group by brand and model
  summarise(count_sold = n()) %>% # Count vehicles per group
  arrange(desc(count_sold)) # Sort from highest to lowest sales

#There weren't good base msrp data in this dataset, supplementing with online data so I will take the missing data from df_tesla_prices dataset

msrp_by_year <- cleaned_df %>% 
  filter(Base_MSRP > 0) %>% # Keep only valid MSRP values
  filter(Tesla == 'TESLA') %>% # Keep only Tesla vehicles
  group_by(Model_Year, Make, Model) %>% # Group by year, make, and model
  summarise(min(Base_MSRP)) %>% # Get minimum MSRP per group
  arrange(Model_Year, Make, Model) # Sort results by year, make, and model

msrp_by_year # Display MSRP summary table

# Clean Tesla price data before joining
df_tesla_prices <- df_tesla_prices %>% 
  mutate(Model = toupper(Model)) # Convert model names to uppercase

# Join data to estimate missing prices
top_tesla_models_priced <- make_model_sold %>% 
  left_join(df_tesla_prices, by = "Model") %>% # Join sales data with price data
  mutate(
    estimated_revenue = as.numeric(count_sold) * as.numeric(Base_Price_USD) # Estimate revenue per model
  )

# Final estimates for lifetime sales
top_tesla_models_priced <- top_tesla_models_priced %>% 
  filter(!is.na(estimated_revenue)) %>% # Remove rows with missing revenue
  group_by(Model) %>% # Group by model
  slice_min(Base_Price_USD) %>% # Keep lowest price per model
  ungroup() # Remove grouping

ggplot(make_model_sold, aes(x = reorder(Model, count_sold), y = count_sold)) + 
  geom_col(fill = 'dodgerblue') + # Create bar chart
  coord_flip() + # Flip axes for readability
  labs(title = 'Tesla Models Sold - All Time') + # Add chart title
  theme_minimal() # Apply clean theme

<img width="958" height="589" alt="image" src="https://github.com/user-attachments/assets/41992f3f-4cfe-4e27-8219-15f8e6f2db9c" />


# Next visualization
ggplot(top_tesla_models_priced, aes(x = reorder(Model, estimated_revenue), y = estimated_revenue)) + 
  geom_col(fill = 'forestgreen') + # Create revenue bar chart
  coord_flip() + # Flip axes for readability
  labs(title = 'Estimated revenue per model', x = '', y = 'Estimated Revenue') + # Add labels
  scale_y_continuous(labels = label_comma()) + # Format y-axis with commas
  theme_minimal() # Apply clean theme



<img width="963" height="591" alt="image" src="https://github.com/user-attachments/assets/5b7a52ea-0ec1-408c-b639-99935638cc20" />



# Create yearly Tesla sales summary
tesla_sold_by_year <- cleaned_df %>% 
  filter(Tesla == 'TESLA') %>% # Keep only Tesla vehicles
  group_by(Model_Year) %>% # Group by model year
  summarise(count = n()) %>% # Count vehicles per year
  arrange(Model_Year) # Sort by year
    
# Tesla sold by year chart
ggplot(tesla_sold_by_year, aes(x = Model_Year, y = count)) +
  geom_line(color = 'dodgerblue', size = 1.1) + # Add trend line
  geom_point(color = 'red', size = 2) + # Add data points
  labs(title = 'Tesla Vehicles Sold by Year') + # Add chart title
  theme_minimal() # Apply clean theme


<img width="962" height="593" alt="image" src="https://github.com/user-attachments/assets/5a6f3e8b-3016-40b2-9f45-612a0ea764d0" />

```

From the results, I found that the Model Y has the highest number of sales in the dataset, followed by the Model 3, while the Model S and Model X are much lower and the Cybertruck and Roadster appear in very small numbers. Using supplemental price data, the estimated revenue chart also shows that the Model Y and Model 3 generate the largest share of revenue because of their much higher sales volume. Finally, the yearly sales trend shows that Tesla sales increased significantly over time, especially after 2017, peaked around 2023, and then declined afterward in the dataset, which may reflect either a recent slowdown or incomplete data for the latest year.



## **4. Tesla vs Other BEVs: Electric Range and Price Comparison**


```{r}
# Average Range and MSRP
Avg_Range_for_EVs <- cleaned_df %>% 
  filter(Electric_Vehicle_Type == "Battery Electric Vehicle (BEV)", # Keep only BEVs
         Electric_Range != 0, # Remove zero range values
         Base_MSRP != 0) %>% # Remove zero MSRP values
  group_by(Tesla) %>% # Group by Tesla vs Other
  summarise(
    avg_range = mean(Electric_Range), # Average electric range
    avg_MSRP = median(Base_MSRP) # Median MSRP (more robust than mean)
  )

# Median Range and MSRP
Median_Range_for_EVs <- cleaned_df %>% 
  filter(Electric_Vehicle_Type == "Battery Electric Vehicle (BEV)", # Keep only BEVs
         Electric_Range != 0, # Remove zero range values
         Base_MSRP != 0) %>% # Remove zero MSRP values
  group_by(Tesla) %>% # Group by Tesla vs Other
  summarise(
    avg_range = mean(Electric_Range), # Average range (still included)
    median_range = median(Electric_Range), # Median electric range
    median_MSRP = median(Base_MSRP) # Median MSRP
  )

# Note: MSRP is not fully populated and may skew results

# Visualization: Average Electric Range
ggplot(Avg_Range_for_EVs, aes(x = Tesla, y = avg_range, fill = Tesla)) +
  geom_col() + # Bar chart
  labs(title = 'AVG Electric Range (BEV)',
       x='Vehicle',
       y='Average Range') + # Title
  theme_minimal() # Clean theme

<img width="964" height="593" alt="image" src="https://github.com/user-attachments/assets/b562d461-4e74-4415-807a-dcbf1b235e82" />



# Visualization: Average MSRP
ggplot(Avg_Range_for_EVs, aes(x = Tesla, y = avg_MSRP, fill = Tesla)) +
  geom_col() + # Bar chart
  labs(title = 'AVG MSRP',
       x='Vehicle',
       y='Average MSRP') + # Title
  theme_minimal() # Clean theme

<img width="959" height="593" alt="image" src="https://github.com/user-attachments/assets/213aad74-3359-42d5-9c8a-95b9fc937dfb" />


```


The charts show that Tesla battery electric vehicles (BEVs) have a much higher average electric range than other BEVs in the dataset, but they also have a substantially higher MSRP. This suggests that Tesla vehicles are positioned as a more premium option in the electric vehicle market, offering stronger driving range in exchange for a higher price. In other words, the results reflect a clear trade-off between performance and cost. However, because MSRP data is not fully available for all vehicles, the price comparison should be interpreted with some caution.

## **5. Growth Trends of BEVs vs PHEVs Over Time**

```{r}
bhev_vs_bev <- cleaned_df %>%
  group_by(Model_Year, Electric_Vehicle_Type) %>% # Group by year and EV type (BEV vs PHEV)
  summarise(count = n(), .groups = "drop") # Count vehicles and remove grouping

# Visualization
ggplot(bhev_vs_bev, aes(x = Model_Year, y = count, color = Electric_Vehicle_Type)) +
  geom_line(size = 1.1) + # Add trend lines
  geom_point(size = 2) + # Add data points
  labs(title = 'PHEV vs BEV Trends Over Time') + # Chart title
  theme_minimal() # Clean theme

<img width="961" height="589" alt="image" src="https://github.com/user-attachments/assets/d1eb6b5c-72b0-4671-a53a-af30d6532173" />

```


The chart shows that both battery electric vehicles (BEVs) and plug-in hybrid electric vehicles (PHEVs) have increased over time, but BEVs have experienced significantly faster growth, especially after 2018. BEV counts rise sharply and peak around 2023, far exceeding PHEVs, which grow more gradually and remain at lower levels throughout the period. This suggests a clear shift in the market toward fully electric vehicles rather than hybrid options. The slight decline observed in the most recent years may reflect incomplete data or reporting lags rather than an actual drop in adoption.


## **6. Top Utility Providers Supporting Tesla Adoption in Washington**

```{r}
utility_count_tesla <- cleaned_df %>%
  filter(Tesla == 'TESLA') %>% # Keep only Tesla vehicles
  group_by(Electric_Utility) %>% # Group by utility provider
  summarise(count = n(), .groups = "drop") %>% # Count vehicles per utility
  arrange(desc(count)) %>% # Sort from highest to lowest
  head(5) # Keep top 5 utilities

# Visualization
ggplot(utility_count_tesla, aes(x = reorder(Electric_Utility, count), y = count)) +
  geom_col(fill = 'steelblue') + # Bar chart
  coord_flip() + # Flip axes for readability
  labs(title = 'Top Electric Utilities for Tesla in Washington', 
       x = 'Utility', # Fix typo in label
       y = 'Tesla Count') +
  theme_minimal() + # Clean theme
  theme(axis.text.y = element_text(size = 4)) # Adjust y-axis text size

<img width="960" height="593" alt="image" src="https://github.com/user-attachments/assets/50773da0-c881-4113-9998-4131af3489ac" />

```


The chart highlights that Tesla adoption in Washington is heavily concentrated among a few key electric utility providers, with Puget Sound Energy and its related service areas accounting for the largest share by a significant margin. This suggests that regions served by these utilities have stronger infrastructure, incentives, or demand for electric vehicles, particularly Tesla. Other utilities, such as Seattle City Light and Bonneville Power Administration, show lower but still notable counts, indicating more moderate adoption levels. Overall, the results imply that local utility coverage and regional energy support systems may play an important role in influencing where Tesla vehicles are most prevalent.

## **7.Conclusion**

From these visualizations, we can conclude the following findings;

Market Share Tesla holds a strong position in Washington’s electric vehicle market. The data showed that Tesla represents a large share of the EV market, although other manufacturers combined still make up the majority.

Top-Selling Models Tesla sales are mainly driven by the Model Y and Model 3. The data showed that these two models had the highest sales counts and generated the largest estimated revenue compared to other Tesla models.

Range and Price Tesla battery electric vehicles tend to offer higher electric range, but at a higher price. The data showed that Tesla BEVs had a much greater average electric range than other BEVs, while also having a substantially higher MSRP.

BEV vs PHEV Trends Fully electric vehicles are growing faster than plug-in hybrid vehicles. The data showed that BEVs increased much more sharply over time than PHEVs, especially after 2018, suggesting stronger market preference for fully electric vehicles.

Utility Providers Tesla adoption is concentrated in a few major utility service areas. The data showed that Puget Sound Energy and related utility regions accounted for the highest Tesla counts in Washington.

Overall, we can conclude that Tesla is a leading force in Washington’s electric vehicle market, driven by strong market share, high sales of the Model Y and Model 3, superior electric range, and strong adoption in key utility regions, while the broader market continues shifting toward fully electric vehicles over plug-in hybrids.
