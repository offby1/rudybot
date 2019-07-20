In order for the witticism code to work, there needs to be an elasticsearch instance set up,
and various config describing it.

I've done it once before but can't remember how I did it; watch as I try to recreate it.

btw the client I'm using isn't part of boto, oddly; instead it's
https://elasticsearch-py.readthedocs.io/en/master/

- Went to https://us-west-1.console.aws.amazon.com/es/home?region=us-west-1#create-domain, started clicking buttons
  I chose that region because rudybot runs on my one ec2 instance that is in that same region.

- guessed I'll need 30GB storage (actually I assume I need way less, but I rounded up)
- guess I'll need one shard
- t2.small.elasticsearch is the cheapest instance type available; it's $0.048 per Hour, about $32/month
- totally guessed on VPC, security groups, &c; I didn't understand any of that stuff
- after updating the semi-random endpoint, and trying "python3 upload-json-log-to-elasticsearch.py", it dies with
  elasticsearch.exceptions.AuthorizationException: AuthorizationException(403, '{"Message":"User: anonymous is not authorized to perform: es:ESHttpGet"}')

  No idea how that used to work :-|

- the access policy at https://us-west-1.console.aws.amazon.com/es/home?region=us-west-1#domain:resource=rudybot-witticisms;action=access-policy is

      {
        "Version": "2012-10-17",
        "Statement": [
          {
            "Effect": "Allow",
            "Principal": {
              "AWS": "arn:aws:iam::661326993281:root"
            },
            "Action": "es:*",
            "Resource": "arn:aws:es:us-west-1:661326993281:domain/rudybot-witticisms/*"
          }
        ]
      }

- Created arn:aws:iam::661326993281:policy/ElasticSearchFullAccess, which looks like this

      {
          "Version": "2012-10-17",
          "Statement": [
              {
                  "Sid": "VisualEditor0",
                  "Effect": "Allow",
                  "Action": "es:*",
                  "Resource": "*"
              }
          ]
      }

  ... attached that to my EC2 instance's IAM role arn:aws:iam::661326993281:role/teensy-server ... let's see if that does anything

After enabling logging, it's clear my requests aren't being signed _at all_, so no wonder I'm getting a 403.
Maybe I just need to pass some more environment-variable mojo.

[Docs for the
client](https://elasticsearch-py.readthedocs.io/en/master/#compatibility)
say I should be using `elasticsearch>=6.0.0,<7.0.0`, since the version
of Elasticsearch is 6.7

    :-) 2019-07-20T22:21:32+0000 [ip-10-0-0-79 ~]$ aws es  describe-elasticsearch-domain --domain-name rudybot-witticisms | fgrep ElasticsearchVersion
            "ElasticsearchVersion": "6.7",

Downgrading didn't help.

https://elasticsearch-py.readthedocs.io/en/master/#running-on-aws-with-iam seems to describe how I'd expect it to work.
