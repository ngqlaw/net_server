%% Feel free to use, reuse and abuse the code in this file.

%% @doc Upload handler.
-module(upload_h).

-export([init/2]).

-include_lib("xmerl/include/xmerl.hrl").

%% Excel对象
-record(excel, {
    sheets = []
}).

%% 页
-record(excel_sheet, {
    id = 0,
    name,
    rows
}).

%%　行
-record(excel_row, {r, cells = []}).

%% 单元格
-record(excel_cell, {c, v}).

init(Req, Opts) ->
    {NewReq, _Res} = handle_req(Req, []),
    {ok, NewReq, Opts}.

handle_req(Req, Res) ->
    case cowboy_req:read_part(Req) of
        {ok, Headers, Req2} ->
            {ok, Data, Req3} = cowboy_req:read_part_body(Req2),
            {file, <<"inputfile">>, Filename, ContentType} = cow_multipart:form_data(Headers),
            lager:debug("Received file ~p of content-type ~p~n", [Filename, ContentType]),
            {ok, Info} = do_open(Data),
            handle_req(Req3, [{Filename, Info}|Res]);
        {done, Req1} ->
            {Req1, Res}
    end.

do_open(File) ->
    {ok, ExcelData} = zip:unzip(File, [memory]),

    % prase string table info
    SharedStringsBinary = proplists:get_value("xl/sharedStrings.xml", ExcelData),
    {SharedStringsDoc, _} = xmerl_scan:string(erlang:binary_to_list(SharedStringsBinary)),
    SharedStringXML = xmerl_xpath:string("/sst/si/t", SharedStringsDoc),
    {ok, StringTable} = new_excel_string_table(SharedStringXML),

    % parse sheets info
    WorkbookBinary = proplists:get_value("xl/workbook.xml", ExcelData),
    {WorkbookDoc, _} = xmerl_scan:string(erlang:binary_to_list(WorkbookBinary)),
    [#xmlElement{content = SheetsXML}] = xmerl_xpath:string("/workbook/sheets", WorkbookDoc),
    SheetInfos = [new_excel_sheet(SheetXML)||SheetXML <- SheetsXML],

    % load sheets data
    {ok, #excel{sheets = lists:foldr(
    fun(SheetInfo = #excel_sheet{id = SheetId}, AccIn) ->
        SheetDataFile = lists:concat(["xl/worksheets/sheet", SheetId, ".xml"]),
        case proplists:get_value(SheetDataFile, ExcelData) of
            undefined -> AccIn;
            SheetDataBinary ->
                {SheetDataDoc, _} = xmerl_scan:string(erlang:binary_to_list(SheetDataBinary)),
                [#xmlElement{content = RowsXML}] = xmerl_xpath:string("/worksheet/sheetData", SheetDataDoc),
                Rows = [new_excel_row(RowXML, StringTable) || RowXML<-RowsXML],
                [SheetInfo#excel_sheet{rows = Rows}|AccIn]
        end
    end, [], SheetInfos)}}.

new_excel_string_table(SharedStringXML) ->
    new_excel_string_table(SharedStringXML, dict:new(), 0).

new_excel_string_table([], StringTable, _Index) -> {ok, StringTable};
new_excel_string_table([#xmlElement{content = [#xmlText{value = Value}|_]}|T], StringTable, Index) ->
    NewStringTable = dict:store(Index, Value, StringTable),
    new_excel_string_table(T, NewStringTable, Index + 1).

new_excel_sheet(#xmlElement{attributes = Attrs}) ->
    {value, #xmlAttribute{value = SheetName}} = lists:keysearch(name, #xmlAttribute.name, Attrs),
    {value, #xmlAttribute{value = SheetIdStr}} = lists:keysearch('r:id', #xmlAttribute.name, Attrs),
    #excel_sheet{id = SheetIdStr -- "rId", name = SheetName}.

new_excel_row(#xmlElement{attributes = Attrs, content = CellsXML}, StringTable) ->
    {value, #xmlAttribute{value = R}} = lists:keysearch(r, #xmlAttribute.name, Attrs),
    Cells = [new_excel_cell(CellXML, StringTable)|| CellXML <- CellsXML],
    #excel_row{r = list_to_integer(R), cells = Cells}.

new_excel_cell(#xmlElement{
        attributes = Attrs,
        content = [#xmlElement{content = [#xmlText{value = V}]}]
    }, StringTable) ->
    {value, #xmlAttribute{value = C}} = lists:keysearch(r, #xmlAttribute.name, Attrs),
    case lists:keysearch(t, #xmlAttribute.name, Attrs) of
        false ->
            #excel_cell{c = C, v = V};
        {value, #xmlAttribute{value = _}} ->
            case catch dict:fetch(list_to_integer(V), StringTable) of
                {'EXIT', _Error} -> V = <<"undefined">>;
                V -> ok
            end,
            #excel_cell{c = C, v = V}
    end;
new_excel_cell(#xmlElement{
        attributes = Attrs
    }, _StringTable) ->
    {value, #xmlAttribute{value = C}} = lists:keysearch(r, #xmlAttribute.name, Attrs),
    #excel_cell{c = C, v = <<"undefined">>}.
