-- very complex schema for the word counts
CREATE TABLE word_counts (
    date      DATE,
    subreddit TEXT,
    word      TEXT,
    count     BIGINT
)
