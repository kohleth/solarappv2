---
title: "Freqeuently Asked Questions"
output: 
    html_document:
        toc: true
---

If this page doesn\'t answer your question, or if you have any suggestion about CheckMySolar, please [email](mailto:kohleth@gmail.com) me.

### Why isn't data available before 2015-09-16?
We only started collecting data from that date.

### Why isn't data vailable between 2015-09-22 and 2015-09-27?
We just don't have data from that period. But we are working hard to retrieve those data. Meanwhile, you can use the Forecast facility to see what might have happened on those dates.

### How often is the data updated?
Data for the current date will be available after 2100 AEST.

### Is 10km really 10km?
Not exactly. In fact, it will be a bit more than 10km. But I don\'t imagine this will cause any trouble.

### \"Sorry, but there isn't enough data from your area.\" What do I do?
You can try to look at a nearby suburb (where you suspect will have more data). We also try to recommend suitable suburbs in the message area at the bottom of the page, but our recommendation don\'t always work! Alternatively, you can use the Forecast facility to see what might have happened at your area.

### \"Warning: the amount of data we have for your area is not great. Any statistical model (including the regression line) will not be reliable.\" What does this mean?
This means you shouldn\'t trust the statistical output, including the regression line. You can choose to disable the regression line by unchecking the relevant box. What you can still trust is the plot of the data itself (without other augmentation). 

### What is regression with intercept 0?
A.k.a. regression through the origin. It forces the regression line to pass through the origin, i.e. (0,0). This means when the system size is 0kW, we should have generated 0kWh. It is conceptually a nice thing to have. In practice, it is not that different to the standard regression line.

### What is quantile regression?
In our case, it estimates the top and bottom 10\% line. If you lie above the '0.9' line, your system is estimated to be performing better than 90\% of systems of the same size. Likewise, if you fall below the '0.1' line, your system is estimated to be the worst performing 10\%. See also [this wiki](https://en.wikipedia.org/wiki/Quantile_regression).

### How can you 'forecast' the past?
In CheckMySolar, forecast is really a fancy word for 'prediction'. Therefore, while forecasting the past does not make much sense in english, it is really just a prediction of the past.

### What is the 95\% prediction interval?
It is very unlikely that your system's actual output is exactly what we predicted it to be. Therefore we also provide the 95\% prediction interval, which is the interval in which we are 95\% sure your actual output will lie. It is quite wide, and we are working to improve our statistical models to make our forecast (prediction) more precise. See also [this wiki](https://en.wikipedia.org/wiki/Prediction_interval).

### What should I do if my system is under-performing?
Your first point of contact should be your retailer, that is, the company of the salesperson who signed you up. Call them, and call them hard (they might try to dodge). You will need to show them evidence of under-performance, which you can get from this website. If they take your case, they will send an electrician to come and have an inspection. If for whatever reason you can't find your retailer, your next point of contact will be the hardware distributor (the company from which your retailer purchases their hardware). For example, if you suspect your inverter is failing, try find out who the distributor is and call them. If none works, chances are you are xxxx, but try contact the Clean Energy Council or ICCC.

### Can you help? (not a likely FAQ)
Yes. i) Tell your friends about CheckMySolar. ii) [Send me](mailto:kohleth@gmail.com) suggestions to improve CheckMySolar. iii) [Chip in](#chipin) a bit.

