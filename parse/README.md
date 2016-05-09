# Parsing reddit comment archive

This subfolder has the necessary code for parsing the reddit comment archive (raw .bz2 files) into a PostgreSQL database for further analysis.  The full history represents a very large amount of data - too large to dump into any standard database and be able to query effectively without some serious hardware.  For this reason we effectively *pre-aggregate* the data.  We parse each week of comments, count the instances of each word, and generate a word count table keyed by week and sub-reddit.  This is then saved into a standard PostgreSQL table, where it can be queried effeciently.

## Using these scripts

First the PostgreSQL table must be setup.  Run the script:

```
psql reddit_database -f db_schema.sql
```

where *reddit_database* is the name of the PostgreSQL database you want to use.  

The command line script *parse_comments* can then be used to populate this table.  This script takes 5 arguments:

```
-s --startdate The date to start parsing comments for, in the format YYYY-MM-DD
-e --enddate The date to stop parsing comments for, in the format YYYY-MM-DD
-d --data_dir the directory holding the zipped up reddit comment archive
-u --unzip_dir the directory to decompress files into
-t --tablename the postgres table to insert into (word_counts by default)
```

The data directory is where the torrented reddit comment archive is stored (as bz2 files).  The tree of this directory should look something like:

```
├── 2014
│   ├── RC_2014-01.bz2
│   ├── RC_2014-02.bz2
│   ├── RC_2014-03.bz2
│   ├── RC_2014-04.bz2
│   ├── RC_2014-05.bz2
│   ├── RC_2014-06.bz2
│   ├── RC_2014-07.bz2
│   ├── RC_2014-08.bz2
│   ├── RC_2014-09.bz2
│   ├── RC_2014-10.bz2
│   ├── RC_2014-11.bz2
│   └── RC_2014-12.bz2
├── 2015
│   ├── RC_2015-01.bz2
│   ├── RC_2015-02.bz2
│   ├── RC_2015-03.bz2
│   ├── RC_2015-04.bz2
│   ├── RC_2015-05.bz2
│   ├── RC_2015-06.bz2
│   ├── RC_2015-07.bz2
│   ├── RC_2015-08.bz2
│   ├── RC_2015-09.bz2
│   ├── RC_2015-10.bz2
│   ├── RC_2015-11.bz2
│   └── RC_2015-12.bz2
├── 2016
│   ├── RC_2016-01.bz2
│   └── RC_2016-02.bz2
```

And the unzip directory is simply a temp directory where these files can be exracted to while the parse is running (they will be cleaned up afterwards).  So for example, you could run:

```
parse_comments -s 2010-01-01 -e 2010-12-31 -d /data/reddit -u /tmp/reddit
```

This will populate the word_counts table (although it may take a long time...).  Once the table is populated add an index by running:

```
CREATE INDEX word_counts_idx ON word_counts (date,subreddit)
```
