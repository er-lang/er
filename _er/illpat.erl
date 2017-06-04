
tr_erlref ({section, [], [{title,[],["DATA TYPES"]}|Child]}, Acc) ->
    {taglist, _, Tags} = lists:keyfind(taglist, 1, Child),
    io:format("Tags ~p\n", [Tags]),
    DTypes = [ begin
                   case tr__type_name(TName, "0", Acc) of
                       {h3, [{id,"type-"++TName}], [TName++"/0"]} ->
                           %% Did not find type, will use taglist's definition
                           Defs = [X || {tag,_,[{c,_,[X]}]} <- Tags],
                           DType = hd(Defs); % Issue if length(Defs) ≥ 2 !!
                       _Ok ->
                           DType = _Ok
                   end,
                   io:format("\tDType ~p ~p\n", [TName,DType]),
                   [ "\n    "
                   , {'div', [{class,"type"}], [DType]} ]
               end || {item,_,[{marker,[{id,"type-"++TName}|_],_}|_]} <- Tags ],
    tr__category("Types", "types", DTypes);
tr_erlref ({section, [], Child}, _Acc) ->
    {'div', [{class,"section"}], Child};
