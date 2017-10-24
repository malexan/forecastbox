## Intro

The forecastbox web-service allows user to make predictions based on historical data. Forecastbox works with univariate time series: it uses one row of historical data (sales in dollars, visitors per night, orders per month) to predict future values.

## Amount of data

The longer your time series the better: an forecasting algorithm will be able to extract more information from your data and to build reliable forecast.

For montly data it is recommended to have not less than 48 historical observations (four years).

## Workflow

You should insert your time series data in Data Input panel. Your data must be stored as a column. You can copy your data from any spreadsheet application (Excel, LibreOffice, Google Spreadsheets).

1. Data Input panel.
    1. Paste a column with time series data.
    1. Choose year and month for the first observation.
    1. Check that the time series is imported correctly.
1. Get predictions for your time series in Forecast panel.
      
## Feedback

We would like to hear your opinion, questions and suggestions you get during the use of our web-service. Please send email message to [mailto:forecastbox@matrunich.com](forecastbox@matrunich.com) or fill an issue at [forecastbox GitLab repository](https://gitlab.com/rstatcons/forecastbox/issues/).
