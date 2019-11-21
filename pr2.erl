% Trent Julich, Nov. 19th 2019, TCSS380
-module(pr2).

-export([mymain/0]).
-export([directoryAsString/1]).
-export([blocksAsString/1]).
-export([removeFileDirTable/2]).
-export([createBlockTable/2]).

-export([addFileDirTable/5]).
-export([printDirTable/1]).
-export([readUserSelection/0]).

mymain() ->
    io:format("Enter total size of memory model: "),
    TotalSize = readInInteger(),
    io:format("Enter size of each block: "),
    BlockSize = readInInteger(),
    loop().

loop() ->
    Continue = readUserSelection(),
    case Continue of
        quit -> ok;
        _ -> loop()
    end.

printMenu() -> 
    io:format("1 - Add File~n"),
    io:format("2 - Remove File~n"),
    io:format("3 - Print~n"),
    io:format("4 - Quit~n").

%% Input Operations %%

readUserSelection() ->
    printMenu(), 
    S = io:get_line("Please enter selection: "),
    {Int,_} = string:to_integer(S),
    case Int of 
        1 -> 1;
        2 -> 2;
        3 -> 3;
        4 -> quit
    end.

readInInteger() ->
    I = io:get_line(" "),
    {Int,_} = string:to_integer(I),
    Int.

%% Directory Table Operations %%

addFileDirTable( ListOfFileTuples, Name, Size, Start, Len ) ->
    NewEntry = {Name, Size, Start, Len},
    ListOfFileTuples++[NewEntry].

removeFileDirTable([], _) -> io:format("Cannot remove from empty list.~n");
removeFileDirTable([H|T], Name) ->
    removeFileDirTableHelper([], H, T, Name).  

removeFileDirTableHelper(Searched, {Name, X, Y, Z}, Unsearched, NameToRemove) -> 
    if
        Name =:= NameToRemove ->
            Searched++Unsearched;
        true -> 
            removeFileDirTableHelper(Searched++[{Name, X, Y, Z}], hd(Unsearched), tl(Unsearched), NameToRemove)
    end.

printDirTable( [] ) -> ok;
printDirTable([{FileName, Size, Start, Length}|T]) ->
    io:fwrite("~s ~w ~w ~w ~n", [FileName, Size, Start, Length]),
    printDirTable(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Block Table Operations %%

createBlockTable(TotalSize, BlockSize) ->
    Temp = TotalSize div BlockSize,
    if
        TotalSize rem BlockSize /= 0 ->
            NumBlocks = Temp + 1;
        true ->
            NumBlocks = Temp
    end,
    createBlockTableHelper([], NumBlocks).

createBlockTableHelper(List, 0) -> List;
createBlockTableHelper(List, NumBlocksRem) ->
    createBlockTableHelper(List++[{0,0}], NumBlocksRem-1).

removeFromBlockTable(ListOfBlockTuples, Start, Length) ->
    

printBlockTable([]) -> ok;
printBlockTable([{Free, Fragmented}|T]) ->
    io:format("~w ~w ~n", [Free, Fragmented]),
    printBlockTable(T).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%



blocksAsString( ListOfBlockTuples ) -> "TODO".

directoryAsString( ListOfFileTuples ) -> directoryAsStringHelper( ListOfFileTuples, "").

directoryAsStringHelper( [], StringSoFar ) -> StringSoFar;
directoryAsStringHelper( [{FileName, Size, Start, Len} | T], StringSoFar) ->
    Temp = "["++FileName++", "++Size++", "++Start++", "++Len++"]",
    directoryAsStringHelper(T, StringSoFar++Temp).






