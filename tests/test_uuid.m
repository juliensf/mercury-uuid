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

:- import_module int.
:- import_module list.
:- import_module random.
:- import_module random.sfc32.

:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("=== Testing random generation ===\n", !IO),
    test_random_generation(16, !IO),
    io.nl(!IO),
    io.write_string("=== Testing version ==\n", !IO),
    list.foldl(test_version, version_uuids, !IO),
    io.nl(!IO),
    io.write_string("=== Testing conversion from string ===\n", !IO),
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

:- pred test_random_generation(int::in, io::di, io::uo) is det.

test_random_generation(I, !IO):-
    sfc32.init(RNG, State0),
    do_test_random_generation(RNG, I, State0, _State, !IO).

:- pred do_test_random_generation(RNG::in, int::in, State::di, State::uo,
    io::di, io::uo) is det <= urandom(RNG, State).

do_test_random_generation(RNG, I, !State, !IO) :-
    ( if I > 0 then
        random_uuid(RNG, UUID, !State),
        io.format("%s (version: %d, variant: %d)\n",
            [s(to_string(UUID)), i(version(UUID)), i(variant(UUID))], !IO),
        do_test_random_generation(RNG, I - 1, !State, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred test_from_string(string::in, io::di, io::uo) is det.

test_from_string(S, !IO) :-
    io.format("from_string(\"%s\") ==> ", [s(S)], !IO),
    ( if uuid.from_string(S, _) then
        io.write_string("TRUE\n", !IO)
    else
        io.write_string("FALSE\n", !IO)
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
    ( if A = B then
        io.write_string("TRUE\n", !IO)
    else
        io.write_string("FALSE\n", !IO)
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

:- pred test_from_bytes(list(uint8)::in, io::di, io::uo) is det.

test_from_bytes(Bytes, !IO) :-
    io.format("from_bytes(%s) ==> ", [s(string(Bytes))], !IO),
    UUID = uuid.from_bytes(Bytes),
    io.write_string(uuid.to_string(UUID), !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred test_version(uuid::in, io::di, io::uo) is det.

test_version(U, !IO) :-
    io.format("%s (version: %d)\n", [s(to_string(U)), i(version(U))], !IO).

%---------------------------------------------------------------------------%

:- func test_uuids = list(uuid).

test_uuids = [
    uuid("b3fddc1a-1d17-4b8b-b48e-35a0d4c69820"),
    uuid("f39ce8ac-14dd-4578-a920-39582dd2598f"),
    uuid("fc58eb87-6627-48ec-97b5-05dd85a42d86"),
    uuid("16061a91-9304-41dd-8e95-8136373f5d4a"),
    uuid("bab2a57f-397a-47e0-9d01-ebc3820e896b"),
    uuid("ffffffff-ffff-ffff-ffff-ffff11111111"),
    uuid("ffffffff-ffff-ffff-ffff-ffff22222222")
].

%---------------------------------------------------------------------------%

:- func version_uuids = list(uuid).

version_uuids = [
    uuid("b9789c58-1ac2-11eb-b1ac-0231668a1f2a"),  % Time-based       (v1)
    uuid("000001f5-5e9a-21ea-9e00-0242ac130003"),  % DEC security     (v2)
    uuid("cea59c91-c0cd-33af-bee3-a799ddaba88c"),  % Name-based MD5   (v3)
    uuid("2b397df2-0809-412c-907d-c5bd26df775f"),  % Random           (v4)
    uuid("7688ece8-8e50-5bfa-ac80-26bb3da00d61")   % Name-based SHA-1 (v5)
].

%---------------------------------------------------------------------------%

:- func byte_lists = list(list(uint8)).

byte_lists = [
    [179u8, 253u8, 220u8, 26u8, 29u8, 23u8, 75u8, 139u8, 180u8, 142u8, 53u8, 160u8, 212u8, 198u8, 152u8, 32u8],
    [243u8, 156u8, 232u8, 172u8, 20u8, 221u8, 69u8, 120u8, 169u8, 32u8, 57u8, 88u8, 45u8, 210u8, 89u8, 143u8],
    [252u8, 88u8, 235u8, 135u8, 102u8, 39u8, 72u8, 236u8, 151u8, 181u8, 5u8, 221u8, 133u8, 164u8, 45u8, 134u8],
    [22u8, 6u8, 26u8, 145u8, 147u8, 4u8, 65u8, 221u8, 142u8, 149u8, 129u8, 54u8, 55u8, 63u8, 93u8, 74u8],
    [186u8, 178u8, 165u8, 127u8, 57u8, 122u8, 71u8, 224u8, 157u8, 1u8, 235u8, 195u8, 130u8, 14u8, 137u8, 107u8]
].

%---------------------------------------------------------------------------%
:- end_module test_uuid.
%---------------------------------------------------------------------------%
