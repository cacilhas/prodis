%	host to bind
setting(prodis:host, '0.0.0.0').

%	port to bind
setting(prodis:port, 6379).

%	cache file
setting(prodis:cache, '/var/lib/prodis/cache.db').

%	log file
setting(prodis:logfile, '/var/log/prodis.log').

%	log level
setting(prodis:loglevel, info).
