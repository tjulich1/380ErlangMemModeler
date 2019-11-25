% Trent Julich, Nov. 19th 2019, TCSS380
-module(pr2).

-export([mymain/0]).
-export([directoryAsString/1]).
-export([blocksAsString/1]).
-export([printMenu/0]).

% Driver for the program. Used to initialize the directory and block
% tables and begin interacting with the user.
mymain() ->
    {TotalSize, BlockSize} = initBlockTable(),
    BlockTable = createBlockTable(TotalSize, BlockSize),
    DirectoryTable = [],
    loop(DirectoryTable, BlockTable, BlockSize).

% Main loop of the program, prompts user for selection, and handles selection.
% Continues looping until user force quits, or selects menu item 4.
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

% Prompts the user for the name and size of a file to add to the model.
addFilePrompt() ->
    io:format("Please enter the file name: "),
    FileName = readInput(1),
    io:format("Please enter the size of the file: "),
    FileSize = readInput(0),
    {FileName, FileSize}.

% Prompts the user for the name of a file to remove from model.
removeFilePrompt() ->
    io:format("Enter name of file to remove: "),
    readInput(1).

% Prints the possible menu selections to the shell.
printMenu() -> 
    io:format("1 - Add File~n"),
    io:format("2 - Remove File~n"),
    io:format("3 - Print~n"),
    io:format("4 - Quit~n").

%%%%%%%%%%%%%%%%%%%%%%
%% Input Operations %%
%%%%%%%%%%%%%%%%%%%%%%

% Function used to handle a users selection of one of the menu items
% printed to the shell.
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

% Function used to read input from the shell. If the option passed = 0,
% the function will read in an integer. If the option passed = 1, the 
% function will read in a string.
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

% Function used to add a file with the given properties to the 
% given directory table.
addFileDirTable( ListOfFileTuples, Name, Size, Start, Len ) ->
    NewEntry = {Name, Size, Start, Len},
    ListOfFileTuples++[NewEntry].

% Function used to remove a file with the given name from the 
% given directory table.
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

% Function used to print the given directory table to the shell.
printDirTableHeader() ->
    io:format("-------------------------~n"),
    io:format("Directory Table~n"),
    io:format("-------------------------~n"),
    io:format("FileName       FileSize~n").
printDirTable( [] ) -> ok;
printDirTable([{FileName, Size, _, _}|T]) ->
    io:fwrite("~s              ~w ~n", [FileName, Size]),
    printDirTable(T).

% Function used to return the given list of file tuples as a string. 
% returns a string of the format "{FileName1, FileSize1}...{FileNameN, FileSizeN}"
directoryAsString( ListOfFileTuples ) -> 
    directoryAsStringHelper(ListOfFileTuples).
directoryAsStringHelper([]) -> "";
directoryAsStringHelper([{FileName, FileSize, _, _} | T]) ->
    "{"++FileName++", "++lists:flatten(io_lib:format("~p",[FileSize]))++"}"++directoryAsStringHelper(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Block Table Operations %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Calculates and returns the number of blocks required to store
% the given file with blocks of the given size.
calculateReqBlocks(FileSize, BlockSize) ->
    WholeBlocks = FileSize div BlockSize,
    if
        FileSize rem BlockSize /= 0 ->
            WholeBlocks + 1;
        true -> 
            WholeBlocks
    end.

% Takes the given block table and sets the "used" and "fragmented" values
% to zero where 
freeBlocks([], _, _, _) ->
    [];
freeBlocks([Head|Tail], 1, FileSize, BlockSize) ->
    if 
        FileSize =< 0 ->
            [Head]++Tail;
        true -> 
            [{0,0}]++freeBlocks(Tail, 1, FileSize-BlockSize, BlockSize)
    end;
freeBlocks(BlockTable, StartBlock, FileSize, BlockSize) ->
    freeBlocks(BlockTable, StartBlock - 1, FileSize, BlockSize).    

% Function used to update the block table, filling in the used and fragmented
% values. Takes in the total file size, as well as the size of each blocks,
% along with the start index where the updating should start.
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

% Function used to search for free blocks to store a file needing "ReqBlocks" 
% blocks. Will return -1 if there is not enough room for it in the table, 
% or else will return the index of the first block it was saved in.
findFirstFreeBlock([], _, _) -> -1;
findFirstFreeBlock([{Used, _}|T], ReqBlocks, Index) ->
    case Used of
        0 -> 
            Success = findConsecutive(T, ReqBlocks - 1),
            case Success of 
                success -> Index;
                failure -> findFirstFreeBlock(T, ReqBlocks, Index + 1)
            end;
        _ -> findFirstFreeBlock(T, ReqBlocks, Index + 1)
    end.

% Function to check consecutive blocks. Returns 'success' if consecutive blocks
% are open, 'failure' otherwise.
findConsecutive(_, 0) -> 
    success;
findConsecutive([], _) ->
    failure;
findConsecutive([{Used, _}|T], ReqBlocks) ->
    case Used of 
        0 -> findConsecutive(T, ReqBlocks - 1);
        _ -> failure
    end.

% Prompts user for total memory size, and size of each block.
initBlockTable() ->
    io:format("Enter total size of memory model: "),
    TotalSize = readInput(0),
    io:format("Enter size of each block: "),
    BlockSize = readInput(0),
    {TotalSize, BlockSize}.

% Creates and returns a list of tuples with ceil(TotalSize/BlockSize) 
% number of tuples representing blocks.
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
    
% Prints the given block table to the shell.
printBlockTable([]) -> ok;
printBlockTable([{Free, Fragmented}|T]) ->
    io:format("~w       ~w ~n", [Free, Fragmented]),
    printBlockTable(T).

% Prints a header for a block table to the shell.
printBlockTableHeader() ->
    io:format("------------------~n"),
    io:format("Block Table ~n"),
    io:format("------------------~n"),
    io:format("Free    Fragmented ~n").

blocksAsString( ListOfBlockTuples ) -> 
    io:format("Used    Fragmented~n"),
    blocksAsStringHelper(ListOfBlockTuples).

blocksAsStringHelper([]) -> ok;
blocksAsStringHelper([{Used, Fragmented}|T]) ->
    io:format("~w       ~w~n", [Used, Fragmented]),
    blocksAsStringHelper(T).