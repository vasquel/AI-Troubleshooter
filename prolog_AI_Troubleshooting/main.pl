:- use_module(rules).
:- use_module(util).
:- dynamic rules:rule/3.

% Auto-start the program on launch
:- initialization(start).

% Entry point
start :-
    nl,
    write('=== AI Troubleshooter ==='), nl,
    menu.

% Main menu loop
menu :-
    nl,
    write('1 - Describe your issue'), nl,
    write('2 - Add new rule'), nl,
    write('3 - Exit'), nl,
    write('Choose an option: '),
    read(Choice),
    handle_choice(Choice).

% Option 1: Describe issue and try to match
handle_choice(1) :-
    write('Describe your issue:'), nl,
    read_line_to_string(user_input, Input),
    util:tokenize(Input, Tokens),
    findall([Score, Category, Action],
            (rules:rule(Category, Keywords, Action),
             util:match_keywords(Tokens, Keywords, Score),
             Score > 0),
            Matches),
    sort(0, @>=, Matches, Sorted),
    (
        Sorted = [[TopScore, Cat, Act]|_],
        TopScore >= 2
    ->
        format('✅ Matched!\nCategory: ~w\nAction: ~w\n', [Cat, Act])
    ;
        write('❌ No strong match found.'), nl
    ),
    menu.

% Option 2: Add a new rule dynamically
handle_choice(2) :-
    write('Enter category: '), read_line_to_string(user_input, Cat),
    write('Enter keywords (comma-separated): '), read_line_to_string(user_input, Keys),
    write('Enter solution/action: '), read_line_to_string(user_input, Act),
    split_string(Keys, ",", " ", Raw),
    maplist(string_lower, Raw, Keywords),
    assertz(rules:rule(Cat, Keywords, Act)),
    write('✅ Rule added!'), nl,
    menu.

% Option 3: Exit
handle_choice(3) :-
    write('👋 Exiting. Stay cool!'), nl.

% Catch-all for invalid inputs
handle_choice(_) :-
    write('❌ Invalid option.'), nl,
    menu.
