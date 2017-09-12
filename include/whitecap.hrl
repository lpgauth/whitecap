%% macros
-define(APP, whitecap).
-define(CHILD(Name, Mod), {Name, {Mod, start_link, [Name]}, permanent, 5000, worker, [Mod]}).

%% records
-record(whitecap_req, {
    state          :: body | done,
    verb           :: verb(),
    path           :: binary(),
    headers        :: [binary()],
    content_length :: undefined | non_neg_integer(),
    body           :: undefined | binary()
}).

-type verb()         :: get | post | put.
-type whitecap_req() :: #whitecap_req {}.
