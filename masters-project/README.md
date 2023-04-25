# **Masters Project**

## **The What**
This was work completed for my dissertation in 2021-2022 as part of the Part-Time Masters in IoT I completed at UU.
The goal was to explore effects of production on tools and ideally be able to forecast Tool Wear for a Mill-Turn CNC machine.
The outcome was a 2-phase approach where I used R for initial data wrangling, exploration and analysis and then Python for application of Machine Learning. 

## **The How**
This was completed using fully open source tools and techniques. 
I was able to, safely, connect the CNC machine to a private network and establish a connection through python's OPCUA tools. I then wrote a script which extracted the machine variables I was interested in and published them to a Mosquitto broker. A TIG stack (Telegraf - Influx - Grafana) was used to ingest the data to Influx, a time series db, and then visualise them, in (near-to) real time, in Grafana. Using Influx I could then pull whatever datasets I required within whichever time periods were of interest and analyse them. 

It must be noted, security is always of upmost importance and as such, each module in this architecture was password protected and the data was secured using TLS certification; furthermore, all modules were containerised in Docker, including the custom script. Initially this ran on an rPi 4 for development but was then migrated to a server for long-term and stable use.

There are no raw datasets available in this repo as the data did not belong to me; it belonged to the commercial partner. The purpose of this repo is for my own future reference and because I believe in open source I am happy to share my methodologies with others, so long as there is no NDA conflict. 

### *Phase 1 - R*
The data was being collected at less than 1Hz and with ~140 variables of interest that's 140 data points every second. The machine was in production for 20 hours, producing 74 parts but the data extracted was for the full 24 hours. This resulted in a very large dataset of ~ 12 million data points and with 11 columns per data point, the final dataset contained ~138 million data points. One of the main reasons for using R is its swiftness at handling large datasets that more popular tools such as Excel simply cannot handle. Moreso, I enjoy using R, as much as Python, as I like the dplyr and ggplot2 packages. Having used R so much for data cleaning, wrangling and analysis, I find it more intuitive than anything else. 

In this project, I used R for the cleaning, wrangling and analysis where I explored the data specific to certain tools of interest. I narrowed the data down to one tool of particular interest and then used R to explore the production variables to find correlation to theoretical tool wear. Once the tool, variable and spindle of focus was chosen, I used R to apply a Feature Importance and a K-Means clustering algorithm. The feature importance confirmed what was determined through analysis: the variable to focus on wrt tool wear. The K-Means was able to identify 4 clusters which correlated to particular tool wear events such as fracture and chipping. 

As I was developing the R scripts, I also used a Markdown file for some commenting and notes. This was not meant for dissemination but by the end, due to the length and detail, I decided to submit it as part of the supplementary material in the dissertation. The markdown file is not as well presented as I would like since it was never meant to for dissemination and was more of a brain dump for future reference. The Markdown file which produced the html is too large to view on github (obviously it can still be uploaded for version control using git lfs) and can't be run without the raw data so instead I have uploaded the html output to an AWS S3 bucket and is able to be viewed at this url: 

**<u>HTML notes:</u>** https://s3.eu-west-2.amazonaws.com/cdn.lintol.io/supporting-material-1-R-notebook.html


### *Phase 2 - Python*
In this phase the final dataset from Phase 1 then was used for forecasting to potentially identify end of life of the tool rather than predicted or assumed. Arima and three flavours of
Recurrent Neural Networks were used: LSTM, BiLSTM and GRU. I won't go into detail here as this was a long process but the outcome was that the GRU model behaved the best for the data it was applied to. The full html report can be found here: 

**<u>Report:</u>** https://s3.eu-west-2.amazonaws.com/cdn.lintol.io/supporting-material-2-python-jupyter-notebook.html
