"""
Module for parsing word counts by subreddit from the raw reddit comments archive
Could be easily expanded in future to do other sorts of analysis (not just word counting)
"""

import re
import json
import csv
import datetime
import os
from collections import Counter
import psycopg2


class Parser():
    """
    Main parsing class for the module
    """

    def __init__(self, data_dir, unzip_dir, database="reddit", port=12360, tablename="word_counts"):
        """
        Initialize the parser class

        Keyword arguments:
        data_dir -- the location the reddit comment archive, organised and named as in the torrent
        unzip_dir -- the location where individual files should be decompressed.
                     They will only be unzipped as required, and will be cleaned up after parsing
        database -- the name of the psql database (default reddit)
        port -- the port of the psql database (default 12360)
        tablename -- the psql table to insert results to (default word_counts)
        """

        # init class variables
        self.data_dir = data_dir
        self.unzip_dir = unzip_dir
        self.tablename = tablename

        # connect to the psql database
        self.db = psycopg2.connect(database=database, port=port)
        self.cur = self.db.cursor()


    # util function to convert a date to utc int
    @staticmethod
    def to_utc(date):
        """Takes a datetime and returns a UTC timestamp (int)"""
        utc = date - datetime.datetime(1970, 1, 1)
        utc = int(utc.total_seconds())
        return utc


    # util to print with a timestamp (because I like that)
    @staticmethod
    def print_withtime(text):
        """Prints output with a timestamp"""
        print "%s %s " % (str(datetime.datetime.now()), text)


    # util to unzip the required files
    def unzip_files(self, rfiles):
        """Makes sure the list of files provided are all currently unzipped"""

        # which files are currently unzipped?
        unzipped = os.listdir(self.unzip_dir)
        # unzip any that aren't already there
        for rfile in rfiles:
            if rfile not in unzipped:
                self.print_withtime("unzipping the archive %s.bz2" % rfile)
                input_file = self.data_dir + '/' + rfile[3:7] + '/' + rfile + '.bz2'
                output_file = self.unzip_dir + '/' + rfile
                command = "bunzip2 -dk %s && mv %s %s" % (input_file, input_file[:-4], output_file)
                self.print_withtime("running command \'%s\'" % (command))
                os.system(command)


    # util to tidy up any files no longer required
    def delete_files(self, rfiles):
        """Removes any files from the unzip folder that are not in the list provided i.e. no longer needed"""

        # which files are already unzipped?
        unzipped = os.listdir(self.unzip_dir)
        unzipped = [x for x in unzipped if 'RC_' in x]
        # are any no longer required?
        for rfile in unzipped:
            if rfile not in rfiles:
                # get rid of them
                self.print_withtime("deleting the file %s" % rfile)
                os.remove(self.unzip_dir + '/' + rfile)


    # for a given startdate, enddate and time window this will parse that many days of comments and write to psql
    def parse(self, startdate, enddate, timedelta=datetime.timedelta(7)):
        """
        The main method for the Parser class.  Parses the reddit comments archive, returning word counts by subreddit and time window and saving these in psql.

        Keyword arguments:
        startdate -- date to start parsing comments for
        enddate -- date to stop parsing comments for
        timedelta -- time window (default 7 days/a week)
        """

        # build list of start dates
        startdates = [startdate]
        while (startdates[-1] + timedelta) < enddate:
            startdates.append(startdates[-1] + timedelta)

        # run a parse for each startdate
        for startdate in startdates:

            self.print_withtime("parsing the comments for the period starting " + str(startdate)[:10])

            # what is the end of the time window, and which month/years does it cover?
            enddate = startdate + timedelta
            # build a list of files we need, based on months included in the data
            year_months = range((12 * startdate.year) + (startdate.month - 1), 1 + (12 * enddate.year) + (enddate.month - 1))
            rfiles = []
            for year_month in year_months:
                filename = 'RC_%(year)s-%(month)s' % {'year':(year_month / 12), 'month':str(1 + year_month % 12).zfill(2)}
                rfiles.append(filename)

            # prepared the comment files
            self.unzip_files(rfiles)
            self.delete_files(rfiles)

            # init dict of word counts by subreddit
            word_counts = {}

            # run parse_comments (for all files)
            for rfile in rfiles:
                self.print_withtime("parsing comments from the file %s" % rfile)
                word_counts = self.parse_comments(self.unzip_dir + '/' + rfile, word_counts, self.to_utc(startdate), self.to_utc(enddate))

            self.print_withtime("writing word counts for the week starting %s to database" % (str(startdate)[:10]))
            self.dump_postgres(str(startdate)[:10], word_counts)

        self.delete_files([])


    def parse_comments(self, rfile, word_counts, starttime, endtime):
        """
        Parses comments from a given file for a given time window, returning a dictionary of word counts by subreddit
        """

        # open given file and build the word count dict
        with open(rfile) as comments:
            for idx, comment in enumerate(comments):
                comment = json.loads(comment)
                text, subreddit, time = [comment[x] for x in ['body', 'subreddit', 'created_utc']]
                time = int(time)
                if idx % 1000000 == 0:
                    self.print_withtime("Parsed %s comments" % (idx))
                if time < starttime or time > endtime:
                    continue
                # get rid of punctuation, force everything to lower case and split into a list of words
                words = re.sub("[^a-zA-Z0-9]", " ", text).lower().split()
                # init entry for this subreddit if required
                if not subreddit in word_counts:
                    word_counts[subreddit] = Counter()
                # increment the word counts`
                for word in words:
                    word_counts[subreddit][word] += 1

        return word_counts


    # write a given dict of word counts to psql table
    def dump_postgres(self, date, word_counts):
        """Takes a dictionary of word counts for a given time window and inserts into PostgreSQL"""

        # remove value 1 elements from dict (probably noise, saves space)
        for subreddit in word_counts:
            word_counts[subreddit] = {k:v for (k, v) in word_counts[subreddit].iteritems() if v != 1}

        # dump to a temp csv first
        self.print_withtime("dumping to temp csv /tmp/reddit_comments.csv ...")
        with open('/tmp/reddit_comments.csv', 'wb') as csvfile:
            writer = csv.writer(csvfile)
            writer.writerow(['date', 'subreddit', 'word', 'count'])
            for subreddit, words in word_counts.iteritems():
                for word, count in words.iteritems():
                    writer.writerow([date, subreddit, word, count])

        # then copy into postgres (much faster than inserts)
        self.print_withtime("...and inserting to postgres %s table" % (self.tablename))
        self.cur.execute('COPY %s FROM \'/tmp/reddit_comments.csv\' CSV HEADER;' % (self.tablename))
        self.db.commit()
