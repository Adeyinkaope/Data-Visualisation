# IJC445 Composite Visualisation: Urban Air Pollution (2019–2023)

Data Visualisation


Overview 🔧

This repository presents a composite visualisation and analysis of urban air pollution patterns at two UK urban background monitoring sites—Glasgow Townhead and Sheffield Devonshire Green—using validated data from the UK Automated Urban and Rural Network (AURN). The analysis focuses on the period 2019–2023 and explores how air pollutant concentrations vary seasonally and in relation to meteorological conditions.

The composite visualisation consists of four coordinated charts designed to explain temporal trends, pollutant co-variation, temperature dependence, and wind-driven transport mechanisms influencing air quality.

Question: 

How do PM2.5, PM10, NO₂ and O₃ vary across seasons and years, and how are high-pollution events linked to wind and temperature conditions across the two sites?

Key messages:
- Primary pollutants (NO₂, PM2.5, PM10) peak in winter, while O₃ peaks in summer.
- PM2.5 and NO₂ co-vary strongly under low wind speeds and specific wind sectors (directional transport + reduced dispersion).
- Temperature patterns separate photochemical ozone behaviour from primary pollutant accumulation.
- Polar wind-direction × wind-speed summaries reveal stable directional hotspots for elevated PM2.5.

Key Features 🔍
Data Preparation

Cleaning and validation of hourly AURN air-quality and meteorological data

Aggregation of hourly measurements into daily means for long-term trend analysis

Classification of wind direction into sectors and scaling of wind speed for visual encoding


install.packages(c(
  "tidyverse",
  "ggplot2",
  "lubridate",
  "viridis",
  "patchwork",
  "scales"
))





Visualisations


Insights into air pollution dynamics using:

Time-series small multiples 📉 – Seasonal and multi-year trends in PM2.5, PM10, NO₂ and O₃

Scatter plots 🔸 – Co-variation between PM2.5 and NO₂ with wind speed and direction encoding

Temperature relationship plots 🌡️ – Seasonal pollutant behaviour across temperature regimes

Polar heatmaps 🧭 – Wind direction × wind speed influence on PM2.5 concentrations

Analytical Frameworks

ASSERT framework to structure the analytical workflow from question formulation to interpretation

Grammar of Graphics principles to justify geometry, aesthetics, scales, and coordinate systems

Accessibility & Ethics

Colour-blind-safe palettes and transparency to reduce overplotting

Small multiples to reduce cognitive load

Critical reflection on smoothing, aggregation, and ethical risks of misinterpretation

Git Clone https://github.com/Adeyinkaope/Data-Visualisation.git





