In this report, I detail the creation and application of a linear regression model to forecast housing prices in Brooklyn, leveraging data spanning from January 2016 to December 2020. My aim was to predict prices for the latter half of 2020, specifically for Q3 and Q4, using actual Q3 data and synthetic Q4 data derived by adjusting Q3's dataset to reflect Q4 conditions while maintaining all other parameters constant. A paired t-test conducted to examine the price variation between these two quarters showed a notable price increase from Q3 to Q4.

The dataset, after meticulous preprocessing and cleaning, was narrowed down to 13,052 entries focusing exclusively on residential single-unit apartments and condos, falling under the "A" and "R" building class categories. To correct for the positive skewness of the price distribution, I applied a square root transformation to the price variable, post outlier treatment which involved excluding inherited properties valued at $0, properties priced over $7 million, and those under $200k.

The final model utilized a combination of numerical values and feature-engineered categorical variables, including neighborhood, zip code categories, building class, land and gross square feet (both square root transformed), and block & lot categories. Impressively, this model explained 63.25% of the variance in sale prices, with a Root Mean Square Error (RMSE) of $438,476, indicating the predictions are reliably close to actual sale prices. It considered 40 features in total, offering a comprehensive analysis with a high degree of accuracy.






