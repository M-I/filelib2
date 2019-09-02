-module(filelib2_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([fold_files/1, foreach_file/1, dir_tree/1, is_link/1]).

-import(lists, [foreach/2]).

all() -> [fold_files,dir_tree,is_link, foreach_file].


%% Largely transposed from erlang/otp's test/filelib_SUITE.erl
%% https://github.com/erlang/otp/blob/master/lib/stdlib/test/filelib_SUITE.erl
fold_files(Config) when is_list(Config) ->
    Dir = filename:join(proplists:get_value(priv_dir, Config), "fold_files"),
    ok = file:make_dir(Dir),
    Dirs = [filename:join(Dir, D) || D <- ["blurf","blurf/blarf"]], 
    foreach(fun(D) -> ok = file:make_dir(D) end, Dirs),
    All = all_files(),
    AllDir = all_dir(),
    AllRegular = all_regular_files(),
    Files = mkfiles(lists:reverse(All), Dir),

    %% Test.
    Files1 = filelib2:fold_files(fun(H, T) -> [H|T] end, [], Dir),
    same_lists(All, Files1, Dir),

    Files2 = filelib2:fold_files(fun(H, T) ->
					 case filename:extension(H) of
					     ".text" -> [H|T];
					     _ -> T
					 end
				 end, [], Dir),
    same_lists(["blurf/nisse.text"], Files2, Dir),


    Files3 = filelib2:fold_files(fun(H, T) ->
					 case filename:extension(H) of
					     ".txt" -> [H|T];
					     _ -> T
					 end
				 end, [], Dir),
    same_lists(["fb.txt","ko.txt", 
     "blurf/blarf/aaa.txt","blurf/blarf/urfa.txt"], Files3, Dir),

    Files4 = filelib2:fold_files(fun(H, T) ->
					 case filename:basename(H, ".txt") of
					     "ko" -> [H|T];
					     _ -> T
					 end
				 end, [], Dir),
    same_lists(["ko.txt"], Files4, Dir),

    Files5 = filelib2:fold_files(fun(H, T) ->
					 case filelib:is_dir(H) of
					     true -> [H|T];
					     _ -> T
					 end
				 end, [], Dir),
    same_lists(AllDir, Files5, Dir),

    Files6 = filelib2:fold_files(fun(H, T) ->
					 case filelib:is_regular(H) of
					     true -> [H|T];
					     _ -> T
					 end
				 end, [], Dir),
    same_lists(AllRegular, Files6, Dir),

    %% Cleanup
    del(Files),
    foreach(fun(D) -> ok = file:del_dir(D) end, lists:reverse(Dirs)),
    ok = file:del_dir(Dir).

foreach_file(Config) when is_list(Config) ->
    Dir = filename:join(proplists:get_value(priv_dir, Config), "foreach_file"),
    ok = file:make_dir(Dir),
    Dirs = [filename:join(Dir, D) || D <- ["blurf","blurf/blarf"]], 
    foreach(fun(D) -> ok = file:make_dir(D) end, Dirs),
    All = all_files(),
    Files = mkfiles(lists:reverse(All), Dir),

    %% Test.
    %% careful, erlang CT also makes use of the process dictionary
    ok = filelib2:foreach_file(fun(F) -> put(F,foreach_file_test), ok end, Dir),
    Files1 = get_keys(foreach_file_test),
    same_lists(All, Files1, Dir),

    %% Cleanup
    del(Files),
    foreach(fun(D) -> ok = file:del_dir(D) end, lists:reverse(Dirs)),
    ok = file:del_dir(Dir).
 
dir_tree(Config) when is_list(Config) ->
    Dir = filename:join(proplists:get_value(priv_dir, Config), "dir_tree"),
    ok = file:make_dir(Dir),
    Dirs = [filename:join(Dir, D) || D <- ["blurf","blurf/blarf"]], 
    foreach(fun(D) -> ok = file:make_dir(D) end, Dirs),
    All = all_files(),
    Files = mkfiles(All, Dir),
    

    %% Test.
    Tree = filelib2:dir_tree(Dir),
    same_lists(All, Tree, Dir),

    %% Cleanup
    del(Files),
    foreach(fun(D) -> ok = file:del_dir(D) end, lists:reverse(Dirs)),
    ok = file:del_dir(Dir).

is_link(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = filename:join(PrivDir, ?MODULE_STRING++"_is_file_symlink"),
    SubDir = filename:join(Dir, "sub"),
    AFile = filename:join(SubDir, "a_file"),
    DirAlias = filename:join(Dir, "dir_symlink"),
    FileAlias = filename:join(Dir, "file_symlink"),
    BrokenAlias = filename:join(Dir, "broken_symlink"),
    BrokenPath = "./the/prince/is/in/another/castle",
    ok = file:make_dir(Dir),
    ok = file:make_dir(SubDir),
    ok = file:write_file(AFile, "not that big\n"),
    true = filelib2:is_dir(Dir),
    true = filelib2:is_regular(AFile),
    true = filelib2:is_file(AFile),
    case file:make_symlink(SubDir, DirAlias) of
	{error, enotsup} ->
	    {skip, "Links not supported on this platform"};
	{error, eperm} ->
	    {win32,_} = os:type(),
	    {skip, "Windows user not privileged to create symlinks"};
	ok ->
	    true = filelib:is_dir(DirAlias),
	    false = filelib2:is_dir(DirAlias),
	    true = filelib:is_file(DirAlias),
	    true = filelib2:is_file(DirAlias),
	    true = filelib2:is_symlink(DirAlias),
	    ok = file:make_symlink(AFile,FileAlias),
	    ok = file:make_symlink(BrokenPath,BrokenAlias),
	    true = filelib:is_file(FileAlias),
	    true = filelib2:is_file(FileAlias),
	    true = filelib:is_regular(FileAlias),
	    false = filelib2:is_regular(FileAlias),
	    true = filelib2:is_symlink(FileAlias),
	    ok
    end.

all_files() ->
    ["blurf", "fb.txt","ko.txt", "blurf/blarf",
	   "blurf/nisse.text","blurf/blarf/aaa.txt","blurf/blarf/urfa.txt"].

all_regular_files() ->
    ["fb.txt","ko.txt", 
     "blurf/nisse.text","blurf/blarf/aaa.txt","blurf/blarf/urfa.txt"].

all_dir() ->
    ["blurf", "blurf/blarf"].

same_lists(Expected0, Actual0, BaseDir) ->
    Expected = [filename:absname(N, BaseDir) || N <- lists:sort(Expected0)],
    Actual = lists:sort(Actual0),
    Actual = Expected.

mkfiles([H|T], Dir) ->
    Name = filename:join(Dir, H),
    Garbage = [31+rand:uniform(95) || _ <- lists:seq(1, rand:uniform(1024))],
    file:write_file(Name, Garbage),
    [Name|mkfiles(T, Dir)];
mkfiles([], _) -> [].

del([H|T]) ->
    ok = case filelib:is_regular(H) of
	     true -> file:delete(H);
	     _ -> ok
	 end,
    del(T);
del([]) -> ok.
