# Console Market Time Series Analysis: a focus on Xbox and GamePass service

## Project Overview
This project aims to analyze the gaming console market, focusing on Xbox sales and the effect of Microsoft's Game Pass subscription service. We investigate time-series data for Game Pass subscriptions and Xbox console sales, comparing them to competitors Sony and Nintendo. Key insights into market growth, competition, and the impact of Game Pass are explored using various statistical models.

## Data Sources
The data for this project were gathered from various sources:
- **Game Pass Subscriptions**: Data from Microsoft’s Game Pass service starting from its launch in July 2017 to December 2019.
- **Console Sales Data**: Sales data for Xbox, PlayStation, and Nintendo consoles, covering a time span from November 2005 to October 2023.
  - Sources: [Statista](https://www.statista.com/), [VGChartz](https://www.vgchartz.com/)

While some data was available for download, others required web scraping when they were not immediately available.

## Models and Analysis
We applied several forecasting and regression techniques to model the impact of Game Pass subscriptions on Xbox sales, as well as comparisons with Sony and Nintendo sales. Key models include:
- **ARIMA** and **Bass Model** for Game Pass subscriptions.
- **Holt-Winters additive model** for forecasting Xbox console sales.
- **Unbalanced Competition and Regime Change Diachronic Model (UCRCD)** for comparing sales between Xbox, PlayStation, and Nintendo.

## Results
- Game Pass shows a consistent upward trend in subscription numbers, with its introduction having a weak but noticeable negative effect on Xbox console sales.
- The competitive analysis suggests distinct dynamics between Xbox, PlayStation, and Nintendo, with unique insights into each company’s market behavior over time.
