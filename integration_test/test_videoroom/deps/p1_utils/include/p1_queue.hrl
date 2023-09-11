-record(file_q,
	{tail = 0       :: non_neg_integer(),
	 head = 0       :: non_neg_integer(),
	 limit          :: non_neg_integer() | unlimited,
	 fd             :: file:fd(),
	 path           :: binary(),
	 owner = self() :: pid(),
	 start = 0      :: non_neg_integer(),
	 stop = 0       :: non_neg_integer()}).

-define(qlen(Q), element(2, Q)).
