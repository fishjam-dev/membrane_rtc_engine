-define(PRV_ERROR(Reason),
        {error, {?MODULE, Reason}}).
-define(RAISE(Reason), erlang:error(?PRV_ERROR(Reason))).
-define(DEFAULT_HEX_REPO, <<"hexpm">>).
