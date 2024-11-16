**Question 1** he code reads the WHO Public Health Roundup text file,
tokenizes it, and performs NLP to extract sentiment and country
mentions. Sentiment analysis is conducted using NRC, AFINN, and Bing
lexicons. The outputs include summary statistics of sentiment and the
number of times countries are mentioned.Additionally, two visualizations
are generated where one showing the sentiment distribution and one
showing country mentions.

**Question 2** The scraped news releases are saved as individual .txt
files within the output\_dir directory. Each file contains the cleaned
textual content of a news release, stripped of extraneous elements such
as images, links, and contact information. Examples of these files
include article\_1.txt, article\_2.txt, and so on that are saved in a
separate folder. These documents provide the raw material for further
sentiment analysis. Sentiment analysis was performed using the Bing
Lexicon to classify words into positive or negative categories. The
results of this analysis are saved in two csv files,
news\_articles\_metadata.csv, which includes metadata for all scraped
articles. And ghana\_sentiment\_results.csv, which specifically
summarizes sentiment analysis for articles mentioning Ghana. To
visualize the sentiment analysis results, two bar charts were created
using ggplot2. The first plot, titled “Overall Sentiment in WHO Africa
News Releases”, depicts the distribution of positive and negative
sentiment across all scraped articles, showing a predominance of
positive sentiment. The second plot, titled “Sentiment About Ghana in
WHO Africa News Releases”, highlights the sentiment distribution for
news articles mentioning Ghana. This plot also shows a higher proportion
of positive sentiment compared to negative sentiment
