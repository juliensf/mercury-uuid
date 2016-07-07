%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

:- module test_uuid.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uuid.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    uuid.generate(UUID, !IO),
    io.write_string(to_string(UUID), !IO),
    io.nl(!IO),
    list.foldl(test_from_string, test_strings, !IO).

:- pred test_from_string(string::in, io::di, io::uo) is det.

test_from_string(S, !IO) :-
    io.format("from_string(\"%s\") ==> ", [s(S)], !IO),
    ( if uuid.from_string(S, _)
    then io.write_string("TRUE\n", !IO)
    else io.write_string("FALSE\n", !IO)
    ).

:- func test_strings = list(string).

test_strings = [
    "1ec503ab-1038-4123-bfe0-2619e881d172",
    "7752e86b-55cd-46c2-bbf5-c71af5128c43",
    "  7752e86b-55cd-46c2-bbf5-c71af5128c43",   % Leading whitespace.
    "7752e86b-55cd-46c2-bbf5-c71af5128c43  ",   % Trailing whitespace.
    "7752e86b-55cd-   46c2-bbf5-c71af5128c43",

    "",
    "abcdef",

    "00000000000000000000000000000000",        % C# N format.
    "{00000000-0000-0000-0000-000000000000}",  % C# D format.
    "(00000000-0000-0000-0000-000000000000)",  % C# P format.
    "{0x00000000,0x0000,0x0000,{0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00}}"  % C# X format.
].

%---------------------------------------------------------------------------%
:- end_module test_uuid.
%---------------------------------------------------------------------------%
