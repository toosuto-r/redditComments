# redditComments
Repo for reddit comment database manipulation and lexical analysis

This set of scripts is used to access reddit comment data in a PostgreSQL database accessed through RPostgresQL, for the purpose of mapping lexical connection between subreddits. There are two DB structures used

  A.  Native comments - all fields (most importantly, the subreddit, body and UTC stamp of post) in different fields, and one document per post.         This system has an additional field with the body text stemmed into postgres' tsvector format, which is searchable for word frequency.
  B.  A DB of words created by directly processing each line of the input JSON data, adding a field for subreddit, word, and UTC stamp. This is         vastly more rapid in the initial data input phase, but lacks ability to look for phrases, and contains no information on the frequency          of each word within a document.
  
Either of these sets can be used to generate word frequency tables for each subreddit for any time interval. We use a test set from January 2015, and generate the top 3000 word frequency table from the top 300 subreddits. To map lexical similarity between a subreddit pair, we compare the percentage use of each word in each subreddit (perA, perB) through the metric:
    
    weight <- (abs(perA-perB)+1)*(perA+perB)
    
The first term in brackets takes into account the word frequency difference, and the second term weights differences between frequently-used words more highly, to reduce noise from very low-ranked words. The addition of 1 to the first bracketed term is to ensure the weight size increases when multiplied. There are some considerations to be made with which words to include - for example "deleted" appears frequently, but we manually remove it from each subreddit (effectively looking only at the top 2999 words) since this chiefly will come from deleted comments, which don't reflect the lexical use patterns of the subreddit's userbase.

This simple metric provides a good match between certain subreddits (e.g. "motorcycles" and "bicycling") and a very poor match between other subreddits (e.g. subreddits for giveaways, meetups, foreign subreddits etc. and everything else). We can do this for each subreddit pair in turn (n^2-n pairings). A colour-coded table provides a good visualisation, but some of the remaining scripts select connections over a certain threshold (top 4%) of connection strength, and prepare a link-and-wieght network diagram in gephi, which shows tight knots of subreddits, effectively mapping the lexical connections. The thresholding simply allows meaningful node distribution.

The weights between these subreddits depend on the full word frequency tables, however to reduce this to at-a-glance similarity, we can devise a metric based on the difference between word use in the currently queried subreddit pair (currUse) and the main reddit corpus (fullUse). The metric used for this is:
    
    linkValue <- (abs(currUse-fullUse)+1)*(currUse)/weight
    
Similar to above, this prioritises words with low weight (i.e. high similarity) and a large difference from the main corpus. The second currUse term again values highly-used words.

##############################

List of scripts, and description:
  
  
  pg_proc_subWords.R
    This script accesses the database, and pulls the word tables for each subreddit using a ts_stat call and SELECT on the ts_vector column to      limit to each of the current subreddits (a list of which was previously pulled directly from the DB with a SELECT COUNT(*) call).
    
  getPass.R
    Quick function to grab the password for the DB.
  
  subSizes.R
    This function goes through each of the subs and pulls the total number of word entries, if this step was neglected in the inital call
    (proTip: it was)
  
  fullWordTable.R
    this pulls the full usage of all words in the 300 subreddits analysed, using the first subreddit as a base and subsequently adding the
    counts of words in the new subreddit which match the current base, and appending those that didn't (and their counts) on the end, to be used     in the next iteration. 
  
  subTableProc.R
  subTableProc_strongLinks.R
    These two functions open a subreddit pair, use match() to find the links between them, take into account missing links and create a
    temporary table with the percentage use of words in sub A as the 3000 words, and the remaining added words as words which exist in B but not
    A. the second column is the use of the same words in the same order, but for sub B. We use the above weight metric to calculate the
    difference in word frequency, and sum this table. At the same time, we can look for the words responsible for the lowest weight, and analyse
    each word's difference from the main corpus, and use this in the linkValue metric above. The relational table is then written to file.
  
  subRelgexfPrep_threshold.R
    This script goes through each link in the table created above, and writes it into a suitable format to be accepted by gephi (node 1 name,
    node 2 name, link weight). This only happens for link weights above a certain value (top 4%). The links are also subtracted from 1, to 
    create a larger weight for tighter links.
  
  gexf_subRel.R
    This script is largely lifted directly from https://gist.github.com/Vessy with a couple of minor tweaks: the subreddit sizes are used for 
    node size, but as log of their actual size; the links are given no colour, to allow colouration by the source node after modularity; and of 
    course a differenct dataset. The script uses Fruchterman-Reingold layout, which can be reasonably well recreated in gephi itself with the 
    same layout, gravity <- 0.5 and size <- 3000.