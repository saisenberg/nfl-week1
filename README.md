
## Which NFL Fanbase Had the Worst Week 1?

This project scrapes every NFL team's subreddit - beginning five minutes before each game's end time and ending twenty-four hours after the game's start - and uses sentiment analysis to determine a "positivity" and "negativity" score for every individual comment. We then aggregate sentiment results on a per-team, per-post, per-comment, and per-word level, and rank each NFL team by its subreddit's *net positivity*. We also identify the specific posts which elicited the most positive and negative comments, as well as the most positive and negative comments themselves. Finally, we identify the words most commonly appearing in each NFL subreddit's positive and negative comments.

A full description of the project can be found at [**saisenberg.com**](https://saisenberg.com/projects/nfl-week1.html). To use the Shiny app of weekly fanbase sentiment, visit my [shinyapps.io](https://saisenberg.shinyapps.io/weekly-nfl-sentiment/) page.

### Getting started

#### Prerequisite software

* Python (suggested install through [Anaconda](https://www.anaconda.com/download/))

#### Prerequisite libraries

* Python:
    - praw (```!pip install praw```)
    - matplotlib, nltk, numpy, pandas, re, sys, unicodedata (```all installed with Anaconda```)
    
### Instructions for use

#### 1. Run the code contained in */python/nfl-week1.ipynb*

This code comprises the entirety of the project summary above.

A few important notes:

* Change the *week* variable in the appropriate kernel to change the week to scrape. 

* Change the *mins_scrape* variable to change the number of minutes before the end of each game to scrape. 

* Change the *hours_scrape* variable to change the number of hours after the start of each game to scrape.

* Keep in mind that PRAW can only scrape the **one thousand** most recent posts in a given subreddit.

Additionally:

* If this is your first time scraping Reddit, make sure to set up an [app](https://www.reddit.com/prefs/apps).

* Be sure to update your Reddit app client ID, client secret, and user agent, as well as your personal Reddit username and password in the appropriate kernel.

### Author

* **Sam Isenberg** - [saisenberg.com](https://saisenberg.com) | [github.com/saisenberg](https://github.com/saisenberg)


### License

This project is licensed under the MIT License - see the *LICENSE.md* file for details.

### Acknowledgements

StackOverflow user [Yann Dubois](https://stackoverflow.com/questions/43018030/replace-appostrophe-short-words-in-python/47091370#47091370)
