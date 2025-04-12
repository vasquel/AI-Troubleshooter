:- module(util, [tokenize/2, match_keywords/3]).

:- use_module(library(lists)).

% Tokenize input
tokenize(Input, Tokens) :-
    split_string(Input, " ", ".,!?-", Raw),
    maplist(string_lower, Raw, Tokens).

% Simple keyword matcher
match_keywords(Tokens, Keywords, Score) :-
    include({Keywords}/[Word]>>memberchk(Word, Keywords), Tokens, Matches),
    length(Matches, Score).
