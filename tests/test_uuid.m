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
    %uuid.generate(UUID, !IO),
    %io.write_string(to_string(UUID), !IO),
    %io.nl(!IO),
    io.write_string("=== Testing conversion to string ===\n", !IO),
    list.foldl(test_from_string, test_strings, !IO),
    io.nl(!IO),
    io.write_string("=== Testing Equality ===\n", !IO),
    UUIDs = test_uuids,
    list.foldl(test_equality(UUIDs), UUIDs, !IO),
    io.nl(!IO),
    io.write_string("=== Testing Comparison ===\n", !IO),
    list.foldl(test_comparison(UUIDs), UUIDs, !IO),
    io.nl(!IO),
    io.write_string("=== Test conversion to byte list ===\n", !IO),
    list.foldl(test_to_bytes, UUIDs, !IO),
    io.nl(!IO),
    io.write_string("=== Test conversion from byte list ===\n", !IO),
    list.foldl(test_from_bytes, byte_lists, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

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
    "1EC503AB-1038-4123-BFE0-2619E881D172",
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

:- pred test_equality(list(uuid)::in, uuid::in, io::di, io::uo) is det.

test_equality(Bs, A, !IO) :-
    list.foldl(do_test_equality(A), Bs, !IO).

:- pred do_test_equality(uuid::in, uuid::in, io::di, io::uo) is det.

do_test_equality(A, B, !IO) :-
    io.format("%s = %s ==> ", [s(to_string(A)), s(to_string(B))], !IO),
    ( if A = B
    then io.write_string("TRUE\n", !IO)
    else io.write_string("FALSE\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_comparison(list(uuid)::in, uuid::in, io::di, io::uo) is det.

test_comparison(Bs, A, !IO) :-
    list.foldl(do_test_comparison(A), Bs, !IO).

:- pred do_test_comparison(uuid::in, uuid::in, io::di, io::uo) is det.

do_test_comparison(A, B, !IO) :-
    io.format("compare(%s, %s) ==> ", [s(to_string(A)), s(to_string(B))],
        !IO),
    compare(Res, A, B),
    io.print_line(Res, !IO).

%---------------------------------------------------------------------------%

:- pred test_to_bytes(uuid::in, io::di, io::uo) is det.

test_to_bytes(U, !IO) :-
    io.format("to_bytes(%s) ==> ", [s(to_string(U))], !IO),
    Bytes = uuid.to_bytes(U),
    io.print_line(Bytes, !IO).

%---------------------------------------------------------------------------%

:- pred test_from_bytes(list(int)::in, io::di, io::uo) is det.

test_from_bytes(Bytes, !IO) :-
    io.format("from_bytes(%s) ==> ", [s(string(Bytes))], !IO),
    UUID = uuid.from_bytes(Bytes),
    io.write_string(uuid.to_string(UUID), !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- func test_uuids = list(uuid).

test_uuids = [
    det_from_string("b3fddc1a-1d17-4b8b-b48e-35a0d4c69820"),
    det_from_string("f39ce8ac-14dd-4578-a920-39582dd2598f"),
    det_from_string("fc58eb87-6627-48ec-97b5-05dd85a42d86"),
    det_from_string("16061a91-9304-41dd-8e95-8136373f5d4a"),
    det_from_string("bab2a57f-397a-47e0-9d01-ebc3820e896b"),
    det_from_string("ffffffff-ffff-ffff-ffff-ffff11111111"),
    det_from_string("ffffffff-ffff-ffff-ffff-ffff22222222")
].

%---------------------------------------------------------------------------%

:- func byte_lists = list(list(int)).

byte_lists = [
    [179, 253, 220, 26, 29, 23, 75, 139, 180, 142, 53, 160, 212, 198, 152, 32],
    [243, 156, 232, 172, 20, 221, 69, 120, 169, 32, 57, 88, 45, 210, 89, 143],
    [252, 88, 235, 135, 102, 39, 72, 236, 151, 181, 5, 221, 133, 164, 45, 134],
    [22, 6, 26, 145, 147, 4, 65, 221, 142, 149, 129, 54, 55, 63, 93, 74],
    [186, 178, 165, 127, 57, 122, 71, 224, 157, 1, 235, 195, 130, 14, 137, 107]
].

%---------------------------------------------------------------------------%
:- end_module test_uuid.
%---------------------------------------------------------------------------%
