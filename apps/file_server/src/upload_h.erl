%% Feel free to use, reuse and abuse the code in this file.

%% @doc Upload handler.
-module(upload_h).

-export([init/2]).

init(Req, Opts) ->
    {NewReq, _Res} = handle_req(Req, []),
    {ok, NewReq, Opts}.

handle_req(Req, Res) ->
    case cowboy_req:read_part(Req) of
        {ok, Headers, Req2} ->
            {ok, Data, Req3} = cowboy_req:read_part_body(Req2),
            {file, <<"inputfile">>, Filename, ContentType} = cow_multipart:form_data(Headers),
            lager:debug("Received file ~p of content-type ~p~n", [Filename, ContentType]),
            {ok, Info} = lib_excel:open_content(Data),
            handle_req(Req3, [{Filename, Info}|Res]);
        {done, Req1} ->
            {Req1, Res}
    end.
