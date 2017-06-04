termize_file(Name) ->
  {ok,F} = file:open(Name, [read,binary]),
  case file:read(F, 1024*1024) of
    {ok,Bin} -> ok
  end,
  binary_to_term(Bin).
