# redditComments
Repo for reddit comment database manipulation and lexical analysis

This set of scripts is used to access reddit comment data in a PostgreSQL database accessed through RPostgresQL, for the purpose of mapping lexical connection between subreddits. These scripts run in R with required packages:
RPostgresQL;
plyr;
igraph;
rgexf;
colorspace.

Note the initial parsing scripts (redditComments/parse/*.*) are used to take the monthly files containing the reddit comment data and sensibly put it into a PostgreSQL DB. Each comment is treated as a single line (valid for the JSON format), non-letter characters are converted into spaces and the resulting "words" are added to a word count for that time interval. The resulatant DB contains the start-date of the time interval, word, subreddit and count, and is is assumed all scripts here can access that DB. More details can be found in /parse/readMe.

The scripts in this directory handle the generation of word frequency tables and top subreddit listings for a time interval. getPass is used to return a text password for the DB - this can be altered to provde a password in whatever way you see fit. The remaining three central scripts handle everything else.

All scripts use ~/R/data/rcomments to store and recall data - by ensuring this folder exists, the changes necessary to run the scripts can be minimised.

# topSubsCount.R
This script gets the top N subreddits (defined by subLim, line 8, default 300) by word usage for each distinct date in the database. The "con" variable contains all the necessary DB connection information, and should be changed where necessary.
The first DB call pulls all the distinct dates from the DB. The second DB call set is located in the loop, which goes through each date and takes the top N subreddits out.
The resultant table has headers of the date, and each row is the top N subreddits, and is written to the location defined by topSubTableFile (line 9). The location of this file should be noted for use in the next script.

# pc_cproc_subWords_remote.R
This script does the bulk of the data processing. Using the same database and the list of top subreddits, it queries the DB for the top 3000 words from each subreddit in the top N returned in the previous script. It requires the same DB access credentials as before, as well as the locations of the top subreddit table, and an optional list of English stop words to run. 
If this list is empty, the script will run but retain words like I and The. Additionally, this stop words file should contain fragments like "http", words that aren't lexical contributors to a subreddit.
With these as input, the script will run through all the dates in the header of the top subreddits and get any words that match any of the subreddits in the column for that date. The last result set returned is saved to file for result checking.
The results then need to be combined into a table with all words used that week in the first column and the counts for each subreddit - this is handled by successive "merge" operations (essentially full outer joins) in the loop at line 85. The resultant word frequency table for the top 300 subreddits contains the subreddit name and counts of word used. This section also creates a full lexicon for all subreddits for that week.
The word frequency lists can then be directly compared with the metric in line 123 to get the separation between the subreddits ("weight"), which is put into a two-way table (subRelTable) and saved with data rounded to 3 significant figures for later use.
The final step in this script generates a thresholded list (default threshold set by thresh in ilne 154 as top 5%) of all the connectioned between subreddits - if the connection is over threshold, the subreddit pair is recorded along with the difference from the threshold (to give a lower weight for worse connection, in the next script). This is repeated for each week, and successively added to the end of the table, to get at time-resolved picture of the subreddits and their edges, which can be used in the next step.

# igraphAnim_timeResGexf_communityWordTable.R
This unecessarily-lengthily-named script handles the production of the files to be displayed (gexfs) as well as determining communities, and the words which define them.
It requires reading in of the thresholded table of edges, a list of subreddit sizes by word, the word frequency table for that week, and the reddit total lexicon for each week, all of which will have been previously generated. 
The full thresholded list of all subreddits is loaded into an igraph object, and subreddits which have zero size at that time are displayed "invisibly" in the final graph (to make animation easier). The igraph object g is then used for all subsequent manipulation.
The colour palette used is for light backgrounds - this can be altered to contain whatever colours you would like, but as there are 15+ communities, this can be difficult.
Communities are determined with a fastgreedy community finder, and assigned colours based on their communnity index. The word use in community subreddits is the aggregated and compared to the main reddit lexicon in lines 149 and 150, to isolate the top 10 words for the subreddit. After determination of the representative words, the graph at that time is output, along with the representative word set and the list of subreddits above threshold for that time, for display.











