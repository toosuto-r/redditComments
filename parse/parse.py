import re
import json
import psycopg2
import csv
import datetime
from collections import Counter

# TODO all this should be borken down into nice separable functions
# TODO in particular the "parse" stage should be testable on it's own
# TODO to allow for tweeking how it separates into words or adding new functionality etc.



# connect to the psql database
db = psycopg2.connect(database="reddit")
cur = db.cursor()



# util function to convert a date to utc int
def toUTC(date):
    utc = date - datetime.datetime(1970,1,1)
    utc = int(utc.total_seconds())
    return utc

# util to print with a timestamp (because I like that)
def print_withtime (text):
    print "%s %s " % (str(datetime.datetime.now()),text)



# for a given startdate this will parse a weeks worth of comments and write to psql
def parseWeek(startdate):

    # startdate is a date type
    seven_days = datetime.timedelta(7)
    # add 7 days to get the end of the week
    enddate = startdate + seven_days

    # what are the file names for the months we're looking at?
    file1 = 'RC_' + str(startdate)[:7]
    file2 = 'RC_' + str(enddate)[:7]

    # init dict of word counts by subreddit
    word_counts = {}

    # run parseComments (for two files if necessary)
    print_withtime ("parsing the comments for the week starting %s" % (str(startdate)[:10]))
    word_counts = parseComments(file1, word_counts, toUTC(startdate), toUTC(enddate))
    if file1 != file2 and enddate.day != 1:
        word_counts = parseComments(file2, word_counts, toUTC(startdate), toUTC(enddate))

    print_withtime ("writing word counts for the week starting %s to database" % (str(startdate)[:10]))
    dumpPostgres(str(startdate)[:10], word_counts)



def parseComments(file, word_counts, starttime, endtime):
    
    # open given file and build the word count dict 
    with open(file) as comments:
        for idx,comment in enumerate(comments):
            comment = json.loads(comment)
            text, subreddit, time = [comment[x] for x in ['body', 'subreddit', 'created_utc']]
            time = int(time)
            if idx % 1000000 == 0:
                print_withtime ("Parsed %s comments from %s" % (idx,file))
            if time < starttime or time > endtime:
                continue
            # get rid of punctuation, force everything to lower case and split into a list of words
            # TODO keep numbers too?
            words = re.sub("[^a-zA-Z0-9]", " ", text).lower().split()
            # init entry for this subreddit if required
            if not subreddit in word_counts:
                word_counts[subreddit] = Counter()
            # increment the word counts`
            for word in words:
                word_counts[subreddit][word] +=1

    return word_counts
    

# write a given dict of word counts to psql
def dumpPostgres(date,word_counts):

    # remove value 1 elements from dict (probably noise, saves space)
    for subreddit in word_counts:
        word_counts[subreddit] = { k:v for (k,v) in word_counts[subreddit].iteritems() if v != 1}
    
    # dump to a temp csv first
    print_withtime ("dumping to temp csv...")
    with open('/tmp/reddit/test.csv', 'wb') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['date','subreddit','word','count'])
        for subreddit, words in word_counts.iteritems():
            for word, count in words.iteritems():
                writer.writerow([date,subreddit,word,word_counts[subreddit][word]])

    # then copy into postgres (much faster than inserts)
    print_withtime ("...and inserting to postgres")
    cur.execute('COPY word_counts FROM \'/tmp/reddit/test.csv\' CSV HEADER;')
    db.commit()




# test it out with a month or two of data
startdate = datetime.datetime(2015,01,05)
seven_days = datetime.timedelta(7)

parseWeek(startdate)
startdate += seven_days
parseWeek(startdate)
startdate += seven_days
parseWeek(startdate)
startdate += seven_days
parseWeek(startdate)
startdate += seven_days
parseWeek(startdate)
startdate += seven_days
parseWeek(startdate)
startdate += seven_days
parseWeek(startdate)
