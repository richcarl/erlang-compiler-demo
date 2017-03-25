%% conditional compilation
-ifdef(TEST).
-define(DOMAIN, "local").
-else.
-define(DOMAIN, "foo.com").
-endif.
