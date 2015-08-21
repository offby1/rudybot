Random notes about sqlite's full-text stuff.

I backed up the live db by doing

    sqlite3 /mnt/rudybot/corpus.db .dump > /tmp/eyow

That took a surprisingly long time -- five or ten minutes, maybe
longer (see Note below).  That file compressed nicely with gzip.  Then I copied it to
the laptop, uncompressed, and loaded it into a fresh db via something
like

    $ sqlite3 corpus.db
    sqlite> .read giant-sql-dump.sql

... many minutes passed ...

    sqlite> CREATE VIRTUAL TABLE f_log USING FTS4(text TEXT);
    sqlite> insert into f_log(text) select text from log;

... many minutes passed ...

    sqlite> select count(text) from f_log where text match 'pip';
    286 -- super fast
    sqlite> select * from word_popularity where word = 'pip';
    pip|236 -- similarly fast
    sqlite> select count(text) from log where text like '%pip%';
    3646 -- about two seconds
    sqlite> select count(text) from log where text like '% pip %';
    124 -- also about two seconds

So ... I might be able to nix the `log_word_map` and `word_popularity` tables!

Note: I probably should have done this instead:

    :) 21:00:17 [ec2-user@ip-10-0-0-142 rudybot]$ time sqlite3 corpus.db '.backup backup.db'

    real    1m49.144s
    user    0m2.028s
    sys     0m5.452s

