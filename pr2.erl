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
    {TotalSize, BlockSize} = initBlockTable(),
    BlockTable = createBlockTable(TotalSize, BlockSize),
    DirectoryTable = [],
    loop(DirectoryTable, BlockTable, BlockSize).

loop(DirectoryTable, BlockTable, BlockSize) ->
    
    Choice = readUserSelection(),
    case Choice of
        add -> 
            {FileName, FileSize} = addFilePrompt(), 
            ReqBlocks = calculateReqBlocks(FileSize, BlockSize),
            StartIndex = findFirstFreeBlock(BlockTable, ReqBlocks, 1),
            case StartIndex of
                -1 -> 
                    io:format("Unable to add file~n"),
                    loop(DirectoryTable, BlockTable, BlockSize);
                _ -> 
                    NewDirectoryTable = addFileDirTable(DirectoryTable, FileName, FileSize, StartIndex, ReqBlocks),
                    NewBlockTable = updateBlockTable(BlockTable, StartIndex, FileSize, BlockSize),
                    loop(NewDirectoryTable, NewBlockTable, BlockSize)
            end;
        remove -> 
            RemoveItems = removeFileDirTable(DirectoryTable, removeFilePrompt()),
            {NewTable, FileSize, Start} = RemoveItems,
            NewBlockTable = freeBlocks(BlockTable, Start, FileSize, BlockSize),
            loop(NewTable, NewBlockTable, BlockSize);
        print -> 
            printDirTableHeader(),
            printDirTable(DirectoryTable),
            printBlockTableHeader(), 
            printBlockTable(BlockTable), 
            loop(DirectoryTable, BlockTable, BlockSize);
        quit -> 
            ok;
        _ -> 
            loop(DirectoryTable, BlockTable, BlockSize)
    end.

addFilePrompt() ->
    io:format("Please enter the file name: "),
    FileName = readInput(1),
    io:format("Please enter the size of the file: "),
    FileSize = readInput(0),
    {FileName, FileSize}.

removeFilePrompt() ->
    io:format("Enter name of file to remove: "),
    readInput(1).

printMenu() -> 
    io:format("1 - Add File~n"),
    io:format("2 - Remove File~n"),
    io:format("3 - Print~n"),
    io:format("4 - Quit~n").

%%%%%%%%%%%%%%%%%%%%%%
%% Input Operations %%
%%%%%%%%%%%%%%%%%%%%%%

readUserSelection() ->
    printMenu(),
    io:format("Please enter selection: "), 
    Choice = readInput(0),
    case Choice of 
        1 -> add;
        2 -> remove;
        3 -> print;
        4 -> quit;
        _ -> bad_input
    end.

readInput(Option) ->
    case Option of 
        0 -> 
            I = io:get_line(" "),
            {Int,_} = string:to_integer(I),
            Int;
        1 -> 
            S = io:get_line(" "),
            {Strip, _} = lists:split(length(S)-1, S),
            Strip
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Directory Table Operations %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addFileDirTable( ListOfFileTuples, Name, Size, Start, Len ) ->
    NewEntry = {Name, Size, Start, Len},
    ListOfFileTuples++[NewEntry].

removeFileDirTable([], _) -> io:format("Cannot remove from empty list.~n");
removeFileDirTable([H|T], Name) ->
    removeFileDirTableHelper([], H, T, Name).  

removeFileDirTableHelper(Searched, {Name, X, Y, Z}, Unsearched, NameToRemove) -> 
    if
        Name =:= NameToRemove ->
            {Searched++Unsearched, X, Y};
        true -> 
            removeFileDirTableHelper(Searched++[{Name, X, Y, Z}], hd(Unsearched), tl(Unsearched), NameToRemove)
    end.

printDirTableHeader() ->
    io:format("-------------------------~n"),
    io:format("Directory Table~n"),
    io:format("-------------------------~n"),
    io:format("FileName       FileSize~n").

printDirTable( [] ) -> ok;
printDirTable([{FileName, Size, _, _}|T]) ->
    io:fwrite("~s              ~w ~n", [FileName, Size]),
    printDirTable(T).

directoryAsString( ListOfFileTuples ) -> directoryAsStringHelper( ListOfFileTuples, "").

directoryAsStringHelper( [], StringSoFar ) -> StringSoFar;
directoryAsStringHelper( [{FileName, Size, Start, Len} | T], StringSoFar) ->
    Temp = "["++FileName++", "++Size++", "++Start++", "++Len++"]",
    directoryAsStringHelper(T, StringSoFar++Temp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Block Table Operations %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculateReqBlocks(FileSize, BlockSize) ->
    WholeBlocks = FileSize div BlockSize,
    if
        FileSize rem BlockSize /= 0 ->
            WholeBlocks + 1;
        true -> 
            WholeBlocks
    end.

freeBlocks([Head|Tail], 1, FileSize, BlockSize) ->
    if 
        FileSize =< 0 ->
            [Head]++Tail;
        true -> 
            [{0,0}]++freeBlocks(Tail, 1, FileSize-BlockSize, BlockSize)
    end;
freeBlocks(BlockTable, StartBlock, FileSize, BlockSize) ->
    freeBlocks(BlockTable, StartBlock - 1, FileSize, BlockSize).    

updateBlockTable([], _, _, _) ->
    []; 
updateBlockTable([{Used, Fragmented} | T], 1, FileSize, BlockSize) ->
    if 
        FileSize =< 0 ->
            [{Used, Fragmented}]++T;
        true ->
            if 
                FileSize >= BlockSize ->
                    [{BlockSize, 0}]++updateBlockTable(T, 1, FileSize-BlockSize, BlockSize);
                true ->
                    [{FileSize, BlockSize-FileSize}]++updateBlockTable(T, 1, FileSize-BlockSize, BlockSize)
            end
    end;
updateBlockTable([H|T], StartIndex, FileSize, BlockSize) ->
    [H]++updateBlockTable(T, StartIndex - 1, FileSize, BlockSize).

findFirstFreeBlock([], _, _) -> -1;
findFirstFreeBlock([{Used, Fragmented}|T], ReqBlocks, Index) ->
    case Used of
        0 -> 
            Success = findConsecutive(T, ReqBlocks - 1),
            case Success of 
                success -> Index;
                failure -> findFirstFreeBlock(T, ReqBlocks, Index + 1)
            end;
        _ -> findFirstFreeBlock(T, ReqBlocks, Index + 1)
    end.

findConsecutive(_, 0) -> 
    success;
findConsecutive([], _) ->
    failure;
findConsecutive([{Used, _}|T], ReqBlocks) ->
    case Used of 
        0 -> findConsecutive(T, ReqBlocks - 1);
        _ -> failure
    end.

initBlockTable() ->
    io:format("Enter total size of memory model: "),
    TotalSize = readInput(0),
    io:format("Enter size of each block: "),
    BlockSize = readInput(0),
    {TotalSize, BlockSize}.

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
    
printBlockTable([]) -> ok;
printBlockTable([{Free, Fragmented}|T]) ->
    io:format("~w       ~w ~n", [Free, Fragmented]),
    printBlockTable(T).

printBlockTableHeader() ->
    io:format("------------------~n"),
    io:format("Block Table ~n"),
    io:format("------------------~n"),
    io:format("Free    Fragmented ~n").

blocksAsString( ListOfBlockTuples ) -> "TODO".