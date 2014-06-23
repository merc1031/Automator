-include("partial.hrl").

-record(request_body, {
        content_type :: string(),
        data :: list() | {converted, string()}
}).

-record(http_queue_element, {
          token :: term(),
          type :: atom(),
          url :: string(),
          headers :: list(),
          body = #request_body{} :: #request_body{},
          callback :: #partial{},
          start_time :: erlang:timestamp()
}).
