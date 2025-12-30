%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Nov 2025 12:21 AM
%%%-------------------------------------------------------------------
-module(basic_math).
-export([add/2, sub/2, safe_div/2]).

%% add(A, B) -> A + B
add(A, B) when is_number(A), is_number(B) ->
  A + B.

%% sub(A, B) -> A - B
sub(A, B) when is_number(A), is_number(B) ->
  A - B.

%% safe_div(A, B) -> either A/B or {error, division_by_zero}
safe_div(_, 0) ->
  {error, division_by_zero};
safe_div(A, B) when is_number(A), is_number(B) ->
  A / B.
