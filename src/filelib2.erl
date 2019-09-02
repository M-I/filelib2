%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @copyright 2019 M. Igeleke
%% @doc Extra File Utilities
%% 
%% Helper functions for files and folders

-module(filelib2).

%% API

%% File utilities.
-export([foreach_file/2,
         dir_tree/1,
         fold_files/3,
	 list_dir_all/1,
	 is_file/1,
	 is_regular/1,
	 is_dir/1,
	 is_symlink/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("kernel/include/file.hrl").

-type filename_all() :: string() | binary() | atom().

-type posix()     :: 'eacces'  | 'eagain'  | 'ebadf'   | 'ebusy'  | 'edquot'
		   | 'eexist'  | 'efault'  | 'efbig'   | 'eintr'  | 'einval'
		   | 'eio'     | 'eisdir'  | 'eloop'   | 'emfile' | 'emlink'
		   | 'enametoolong'
		   | 'enfile'  | 'enodev'  | 'enoent'  | 'enomem' | 'enospc'
		   | 'enotblk' | 'enotdir' | 'enotsup' | 'enxio'  | 'eperm'
		   | 'epipe'   | 'erofs'   | 'espipe'  | 'esrch'  | 'estale'
		   | 'exdev'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Like file:list_dir_all(Dir). But returns a list of path.
-spec list_dir_all(filename_all()) -> [filename_all()].
list_dir_all(Dir) ->
    case file:list_dir_all(Dir) of
        {ok, Basenames} -> 
            [filename:join(Dir, Name) || Name <- Basenames];
        _ ->
            []
    end.

%% @doc Similar to lists:foreach(Fun, List). But for a directory tree.
-spec foreach_file(Fun, Dir) -> ok when
      Fun :: fun((File :: filename_all()) -> ok),
      Dir :: filename_all().
foreach_file(Fun, Dir) ->
    Files = list_dir_all(Dir),
    do_foreach_file(Fun, Files).

%% @doc Similar to filelib:fold_files(Dir, RegExp, Recursive, Fun, AccIn). 
%% Will fold Fun over regular files, but will also fold it over directory and symbolic link. 
-spec fold_files(Fun, AccIn, Dir) -> AccOut when
      Fun :: fun((F :: filename_all(), AccIn) -> AccOut),
      AccIn :: term(),
      AccOut :: term(),
      Dir :: filename_all().
fold_files(Fun, Acc, Dir) ->
    Files = list_dir_all(Dir),
    do_fold_files(Fun, Acc, Files).

%% @doc 
-spec dir_tree(Dir) -> Files when
      Dir :: filename_all(),
      Files :: [filename_all()].
dir_tree(Dir) ->
    Files = list_dir_all(Dir),
    Acc = Files,
    do_dir_tree(Files, Acc).

-spec is_file(Filename) -> boolean() | {error, Reason} when
      Filename :: filename_all(),
      Reason ::  posix() | badarg.
is_file(Filename) ->
    case  file:read_link_info(Filename) of
	{ok, _Info} -> true;
	{error,enoent} -> false
    end.

-spec is_regular(Filename) -> boolean() | {error, Reason} when
      Filename :: filename_all(),
      Reason ::  posix() | badarg.
is_regular(Filename) ->
    {ok, Info} = file:read_link_info(Filename),
    regular == Info#file_info.type.

-spec is_dir(Filename) -> boolean() | {error, Reason} when
      Filename :: filename_all(),
      Reason ::  posix() | badarg.
is_dir(Filename) ->
    {ok, Info} = file:read_link_info(Filename),
    directory == Info#file_info.type.

-spec is_symlink(Filename) -> boolean() | {error, Reason} when
      Filename :: filename_all(),
      Reason ::  posix() | badarg.
is_symlink(Filename) ->
    {ok, Info} = file:read_link_info(Filename),
    symlink == Info#file_info.type.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_foreach_file(Fun, [File|Files]) ->
    ok = Fun(File),
    ok = do_foreach_file(Fun, list_dir_all(File)),
    do_foreach_file(Fun, Files);
do_foreach_file(_Fun, []) ->
    ok.

do_fold_files(Fun, Acc, [File|Files]) ->
    NewAcc = Fun(File, Acc),
    NewFiles =  list_dir_all(File) ++ Files,
    do_fold_files(Fun, NewAcc, NewFiles);
do_fold_files(_Fun, Acc, []) ->
    Acc.

do_dir_tree([H|T], Files) ->
    More = list_dir_all(H),
    do_dir_tree(More ++ T, More ++ Files);
do_dir_tree([], Files) ->
    Files.
