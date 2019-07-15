In order for the witticism code to work, there needs to be an elasticsearch instance set up,
and various config describing it.

I've done it once before but can't remember how I did it; watch as I try to recreate it.

- Went to https://us-west-1.console.aws.amazon.com/es/home?region=us-west-1#create-domain, started clicking buttons
  I chose that region because rudybot runs on my one ec2 instance that is in that same region.

- guessed I'll need 30GB storage (actually I assume I need way less, but I rounded up)
- guess I'll need one shard
- t2.small.elasticsearch is the cheapest instance type available; it's $0.048 per Hour, about $32/month
- totally guessed on VPC, security groups, &c; I didn't understand any of that stuff
- after updating the semi-random endpoint, and trying "python3 upload-json-log-to-elasticsearch.py", it dies with
  elasticsearch.exceptions.AuthorizationException: AuthorizationException(403, '{"Message":"User: anonymous is not authorized to perform: es:ESHttpGet"}')

No idea how that used to work :-|
