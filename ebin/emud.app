{application, emud,
	[
		{description, "EMud Erlang mud server."},
		{vsn, "0.1a"},
		{modules, [emud, core, input_parser]},
		{registered, [emud_listener, emud_client_manager]},
		{applications, [kernel, stdlib]},
		{mod, {emud, {port, 3333}}}
	]
}.
