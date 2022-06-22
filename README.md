# Adaptive Management App for Storm Managers

The ‘Adaptive Management App for Storm Managers’ is designed to allow users to review sampling trends and evaluate methodologies through the use of an interactive data dashboard. Users may upload data that is stored in CEDEN format or directly downloaded from the CEDEN website, or by using a generalized data template made available through the application. The data may then be subset using the application provided filters to view location and parameters of concern.  

The initial analysis ‘View Data Results’ provides insight into data trends over time with the options of viewing log or linear scale with or without regression lines. In the ‘How is my sampling frequency?’ section, the user may compare the current effort with a variable number of observations. This allows the user to engage with the data dashboard and select a sampling plan which optimizes the number of samples over time ensuring the sampling frequency will effectively and efficiently detect future trends.

https://sccwrp.shinyapps.io/monitoring_program/


## File Listing
* app.R - Loads library dependencies, imports sources, sets server options and starts the shiny app.
* app_server.R - Core server logic
* app_ui.R - UI logic for assembling pages
* helpers.R - Contains functions for common tasks such as unique lists for dropdown selectors and parameter specific filter functions.
* mod_analysis.R - Analysis module functions for driving Analysis tab. Includes, effect size chart, gauge charts, slider selection.
* mod_inventory.R - Inventory module functions for driving Inventory elements.
* mod_results.R - Results module functions for driving Results tab. Includes summary table.
* config/parameters.yml - General template and example data configuration file.
* config/ceden_download.yml - Configuration file for CEDEN downloads
* config/ceden_sub_template.yml - Configuration file for modified CEDEN downloads

## Power Analysis
The power analysis driving effect size charts under the Analysis section leverage R's pwr.f2.test function for calculating effect size and number of observation values. This function implements Cohen's f^2 calculation, given 4 of 5 arguments (u, v, f2, sig.level, and power) it will compute a value for the omitted variable.  All visualizations assume significance level (sig.level) = 0.05 and power = 0.8. We set u = 1 as we assume a linear model and u = number of coefficients - 1. We leverage v as a proxy for N since the formula goes as v = N - u -1.

For both gauge dials and the effect size chart we generate f2 values for various N (by way of v) and calculate the inflection point in which we calculate the slope of f2 values over N values (slope-y), the slope of N values over f2 values (slope-x), and identify the point in which slope-y >= slope-x. This point then sets where green needles on gauge charts are and where the green line appears on the effect size chart. This logic can be found in mod_analysis.R: get_inflection_point.

Code for these visualizations can be found in mod_analysis.R. The primary functions containing logic described above can be found in the following functions:
* mod_analysis_gauge_effect_size_server - logic for gauge charts
* mod_pwr_f2_analysis_chart_server - logic for the effect size chart
* get_inflection_point - logic for calculating the optimal effort point.

## Trend Analysis
Our monitoring trend analysis algorithm allows managers to evaluate current sampling efforts and compare to different sampling methodologies. The computation follows the following steps:

* Filter data to consist of one stations and parameter option
* Obtain residuals for a linear model
* Create grid search spanning 0.1% to 2% effort and 0.1% to 1% change, for each effort/change pair:
  * Leverage residuals to simulate a decade's worth of data sampling at the given effort while looking for the associated change
  * Compute power values for simulated data, we then have an associated power for each effort/change pair
  * Identify optimal point by computing contour lines using effort, change, and power data then calculate inflection point at which the rate of change for y/x surpasses x/y.
* We then leverage power analysis data to populate gauge dials and the effort chart.

### Simulation Technique
The simulation technique source code can be found in the simvals_opt function (https://github.com/sfei/sccwrp-monitoring-analysis-app/blob/main/helpers.R#L151). This step consists of the following:

* Calculate N<sub>0</sub> as the median of log(1+Result)
* Calculate rate of change per step (-1 * change * N<sub>0</sub>)/effort
* Calculate N as N<sub>0</sub> + simulated_effort * change_step - change_step
* Calculate simulated residuals using a normal distribution factoring in simulated effort and residual variation
* Calculate simulated values as N + simulated_residuals 

### Power Calculation
Once we have simulation data we iterate over simulations and:

* Fit a linear model
* Gather F-statistic information
* Calculate the area under the curve for the f-distribution using R's pf function
* Calculate power as sum(pvals < alpht (=0.05)) / num_rows

Code for the above can be found in the powfun function (https://github.com/sfei/sccwrp-monitoring-analysis-app/blob/main/helpers.R#L313)

### Optimal Calculation
We calculate the optimal point using R's geom_contour function which helps us create contour lines fitting the underlying power data at a pre-determined power level (0.8). We then identify the inflection point in which slope y/x > x/y and use that as our optimal point. More information can be found in the getopt function (https://github.com/sfei/sccwrp-monitoring-analysis-app/blob/main/helpers.R#L343)