%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2016, 2020 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% This module provides universally unique identifiers (UUIDs).
%
%---------------------------------------------------------------------------%

:- module uuid.
:- interface.

:- import_module list.
:- import_module random.

%---------------------------------------------------------------------------%

    % A universally unique identifier (UUID).
    %
    % For the Java backend the representation is a "java.util.UUID" object.
    % For the C# backend the representation is a "System.Guid" object.
    % For other backends UUIDs are represented using a Mercury type.
    %
    % Ordering on uuid/0 values is defined lexicographically on the
    % components of the UUID, arranged as follows:
    %
    %     Octet 0 - 3   : an unsigned 32-bit integer.
    %     Octet 4 - 5   : an unsigned 16-bit integer.
    %     Octet 6 - 7   : an unsigned 16-bit integer.
    %     Octet 8       : an unsigned 8-bit integer.
    %     Octet 9       : an unsigned 8-bit integer.
    %     Octet 10 - 15 : an unsigned 48-bit integer.
    %
    % Note that for the Java backend the above ordering differs from that
    % provided by the Java's compareTo() method.
    %
:- type uuid.

    % Return the nil (empty) UUID.
    % This is a UUID where all 128 bits are set to zero.
    %
:- func nil_uuid = uuid.

    % Return the version number associated with the UUID.
    % The version number describes how the UUID was generated.
    %
    %     1 - Time-based UUID.
    %     2 - DCE Security based UUID.
    %     3 - Named-based based UUID generated using MD5 hash.
    %     4 - Randomly generated UUID.
    %     5 - Name-based UUID generated using SHA1 hash.
    %
:- func version(uuid) = int.

    % Return the variant number associated with the UUID.
    % The variant number describes the layout of the UUID.
    %
    %    0 - Reserved, NCS backwards compatibility.
    %    2 - IETF RFC 4122.
    %    6 - Reserved, Microsoft Corporation backwards compatibility.
    %    7 - Reserved for future definition.
    %
:- func variant(uuid) = int.

    % to_string(UUID) = S:
    % S is the string representation of UUID.
    % S will have the form:
    %
    %    xxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
    %
    % where each 'x' is a hexadecimal digit.  Alphabetic hexadecimal
    % digits will be lowercase.
    %
:- func to_string(uuid) = string.

    % from_string(S, UUID):
    % Convert a string of the form:
    %
    %    xxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
    %
    % where 'x' is a hexadecimal digit, into a UUID.
    % Leading and trailing whitespace is _not_ allowed.
    % Fails if S is not of the above form.
    %
:- pred from_string(string::in, uuid::out) is semidet.

    % det_from_string(S) = UUID:
    % As above, but throws a software_error/1 exception instead of failing.
    %
:- func det_from_string(string) = uuid.

    % A synonym for the above.
    %
:- func uuid(string) = uuid.

    % to_bytes(UUID) = Bytes:
    %
    % Bytes is a list of unsigned bytes in the UUID ordered from most
    % significant to least significant byte.
    %
:- func to_bytes(uuid) = list(uint8).

    % from_bytes(Bytes) = UUID:
    %
    % Construct a UUID from the list of bytes in Bytes.
    % Bytes must be ordered from most significant to least significant byte.
    %
    % Throws a software_error/1 exception if length(Bytes, 16) is false.
    %
:- func from_bytes(list(uint8)) = uuid.

    % generate(RNG, UUID, !State):
    %
    % Generate a type 4 UUID using bytes supplied by the given random number
    % generator.
    %
    % NOTE: you _must_ use a cryptographically strong (pseudo) random number
    % generator with this predicate in order to minimise the possibility
    % collisions.
    %
:- pred random_uuid(R::in, uuid::out, State::di, State::uo) is det
    <= urandom(R, State).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module require.
:- import_module string.
:- import_module uint.
:- import_module uint8.
:- import_module uint16.
:- import_module uint32.

:- interface.

:- pred uuid_equal(uuid::in, uuid::in) is semidet.

:- pred uuid_compare(comparison_result::uo, uuid::in, uuid::in) is det.

:- implementation.

%---------------------------------------------------------------------------%

:- type uuid
    --->    uuid(
                time_low :: uint32,
                time_mid :: uint16,
                time_hi_and_version :: uint16,
                clock_seq_hi_and_reserved :: uint8,
                clock_seq_low :: uint8,
                node0 :: uint8,     % Octet 10
                node1 :: uint8,     % Octet 11
                node2 :: uint8,     % Octet 12
                node3 :: uint8,     % Octet 13
                node4 :: uint8,     % Octet 14
                node5 :: uint8      % Octet 15
            ).

:- pragma foreign_type("Java", uuid, "java.util.UUID")
    where equality is uuid_equal,
          comparison is uuid_compare.

:- pragma foreign_type("C#", uuid, "System.Guid")
    where equality is uuid_equal,
          comparison is uuid_compare.

%---------------------------------------------------------------------------%
%
% Equality.
%

:- pragma no_determinism_warning(uuid_equal/2).

uuid_equal(_, _) :-
    unexpected($pred, "uuid_equal for Mercury UUIDs").

:- pragma foreign_proc("Java",
    uuid_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = A.equals(B);
").

:- pragma foreign_proc("C#",
    uuid_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = A.Equals(B);
").

%---------------------------------------------------------------------------%
%
% Comparison.
%

:- pragma no_determinism_warning(uuid_compare/3).

uuid_compare(_, _, _) :-
    unexpected($pred, "uuid_compare for Mercury UUIDs").

:- pragma foreign_proc("Java",
    uuid_compare(R::uo, A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    R = do_uuid_compare(A, B);
").

:- pragma foreign_code("Java", "

// Java's UUID.compareTo method works by comparing the long components of the
// UUID.  This is not consistent with how libbuid etc implement comparison of
// UUIDs.  The following implements the libuuid style comparison for Java.
//
public static builtin.Comparison_result_0 do_uuid_compare(
     java.util.UUID A, java.util.UUID B)
{
    long msbA = A.getMostSignificantBits();
    long msbB = B.getMostSignificantBits();

    int time_lowA = (int) (msbA >>> 32);
    int time_lowB = (int) (msbB >>> 32);
    if (time_lowA != time_lowB) {
        // Unsigned 32-bit comparison.
        if ((time_lowA & 0xffffffffL) > (time_lowB & 0xffffffffL)) {
            return builtin.COMPARE_GREATER;
        } else {
            return builtin.COMPARE_LESS;
        }
    }

    short time_midA = (short) ((msbA & 0xffff0000) >>> 16);
    short time_midB = (short) ((msbB & 0xffff0000) >>> 16);
    if (time_midA != time_midB) {
        if ((time_midA & 0xffff) > (time_midB & 0xffff)) {
            return builtin.COMPARE_GREATER;
        } else {
            return builtin.COMPARE_LESS;
        }
    }

    short time_hi_and_versionA = (short) ((msbA & 0xffff));
    short time_hi_and_versionB = (short) ((msbB & 0xffff));
    if (time_hi_and_versionA != time_hi_and_versionB) {
        // Unsigned 16-bit comparison.
        if (
            (time_hi_and_versionA & 0xffff) >
            (time_hi_and_versionB & 0xffff)
        ) {
            return builtin.COMPARE_GREATER;
        } else {
            return builtin.COMPARE_LESS;
        }
    }

    long lsbA = A.getLeastSignificantBits();
    long lsbB = B.getLeastSignificantBits();

    short clock_seqA = (short) (lsbA >>> 48);
    short clock_seqB = (short) (lsbB >>> 48);
    if (clock_seqA != clock_seqB) {
        if ((clock_seqA & 0xffff) > (clock_seqB & 0xffff)) {
            return builtin.COMPARE_GREATER;
        } else {
            return builtin.COMPARE_LESS;
        }
    }

    for (int i = 40; i >= 0; i-=8) {
        byte nodeA = (byte) ((lsbA >>> i) & 0xff);
        byte nodeB = (byte) ((lsbB >>> i) & 0xff);
        if (nodeA != nodeB) {
            if ((nodeA & 0xff) > (nodeB & 0xff)) {
                return builtin.COMPARE_GREATER;
            } else {
                return builtin.COMPARE_LESS;
            }
        }
    }

    return builtin.COMPARE_EQUAL;
}

").

:- pragma foreign_proc("C#",
    uuid_compare(R::uo, A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int r = A.CompareTo(B);
    if (r < 0) {
        R = builtin.COMPARE_LESS;
    } else if (r > 0) {
        R = builtin.COMPARE_GREATER;
    } else {
        R = builtin.COMPARE_EQUAL;
    }
").

%---------------------------------------------------------------------------%

nil_uuid = uuid(0u32, 0u16, 0u16, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8).

:- pragma foreign_proc("C#",
    nil_uuid = (U::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    U = System.Guid.Empty;
").

:- pragma foreign_code("Java", "
    public static final java.util.UUID ML_NIL_UUID =
        new java.util.UUID(0L, 0L);
").

:- pragma foreign_proc("Java",
    nil_uuid = (U::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    U = ML_NIL_UUID;
").

%---------------------------------------------------------------------------%

version(U) =
    cast_to_int((U ^ time_hi_and_version >> 12) /\ 0xf_u16).

:- pragma foreign_proc("C#",
    version(U::in) = (Version::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    byte[] bytes = U.ToByteArray();
    // The version is in the most significant 4 bits of the
    // the timestamp.
    Version = (bytes[7] >> 4) & 0xf;
").

:- pragma foreign_proc("Java",
    version(U::in) = (Version::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Version = U.version();
").

%---------------------------------------------------------------------------%

variant(U) = Variant :-
    ClockSeq = U ^ clock_seq_hi_and_reserved,
    ( if ClockSeq /\ 0x80_u8 = 0u8 then
        Variant = 0
    else if ClockSeq /\ 0x40_u8 = 0u8 then
        Variant = 2
    else if ClockSeq /\ 0x20_u8 = 0u8 then
        Variant = 6
    else
        Variant = 7
    ).

:- pragma foreign_proc("C#",
    variant(U::in) = (Variant::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    byte[] bytes = U.ToByteArray();
    byte clock_seq_hi = bytes[8];
    if ((clock_seq_hi & 0x80) == 0) {
        Variant = 0;
    } else if ((clock_seq_hi & 0x40) == 0) {
        Variant = 2;
    } else if ((clock_seq_hi & 0x20) == 0) {
        Variant = 6;
    } else {
        Variant = 7;
    }
").

:- pragma foreign_proc("Java",
    variant(U::in) = (Variant::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Variant = U.variant();
").

%---------------------------------------------------------------------------%
%
% Conversion to a string.
%

to_string(U) = S :-
    string.format(
        "%8.8x-%4.4x-%4.4x-%2.2x%2.2x-%2.2x%2.2x%2.2x%2.2x%2.2x%2.2x", [
        u(cast_to_uint(U ^ time_low)),
        u(cast_to_uint(U ^ time_mid)),
        u(cast_to_uint(U ^ time_hi_and_version)),
        u(cast_to_uint(U ^ clock_seq_hi_and_reserved)),
        u(cast_to_uint(U ^ clock_seq_low)),
        u(cast_to_uint(U ^ node0)),
        u(cast_to_uint(U ^ node1)),
        u(cast_to_uint(U ^ node2)),
        u(cast_to_uint(U ^ node3)),
        u(cast_to_uint(U ^ node4)),
        u(cast_to_uint(U ^ node5))],
        S).

:- pragma foreign_proc("Java",
    to_string(U::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = U.toString();
").

:- pragma foreign_proc("C#",
    to_string(U::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = U.ToString();
").

%---------------------------------------------------------------------------%
%
% Conversion from a string.
%

from_string(S, U) :-
    Components = string.split_at_char('-', S),
    Components = [
        TimeLowStr,
        TimeMidStr,
        TimeHiAndVersionStr,
        ClockSeqStr,
        NodeStr
    ],

    string.length(TimeLowStr, 8),
    string.length(TimeMidStr, 4),
    string.length(TimeHiAndVersionStr, 4),
    string.length(ClockSeqStr, 4),
    string.length(NodeStr, 12),

    hex_string_to_uint(TimeLowStr, TimeLow),
    hex_string_to_uint(TimeMidStr, TimeMid),
    hex_string_to_uint(TimeHiAndVersionStr, TimeHiAndVersion),
    octet_from_string(ClockSeqStr, 0, ClockSeqHiAndReserved),
    octet_from_string(ClockSeqStr, 2, ClockSeqLo),
    octet_from_string(NodeStr, 0, Node0),
    octet_from_string(NodeStr, 2, Node1),
    octet_from_string(NodeStr, 4, Node2),
    octet_from_string(NodeStr, 6, Node3),
    octet_from_string(NodeStr, 8, Node4),
    octet_from_string(NodeStr, 10, Node5),

    U = uuid(
        uint32.cast_from_uint(TimeLow),
        uint16.cast_from_int(uint.cast_to_int(TimeMid)),
        uint16.cast_from_int(uint.cast_to_int(TimeHiAndVersion)),
        ClockSeqHiAndReserved,
        ClockSeqLo,
        Node0,
        Node1,
        Node2,
        Node3,
        Node4,
        Node5).

:- pred hex_string_to_uint(string::in, uint::out) is semidet.

hex_string_to_uint(S, U) :-
    string.foldl(acc_hex_digits, S, 0u, U).

:- pred acc_hex_digits(char::in, uint::in, uint::out) is semidet.

acc_hex_digits(C, !Acc) :-
    hex_digit_to_int(C, I),
    !:Acc = !.Acc * 16u + cast_from_int(I).

:- pred octet_from_string(string::in, int::in, uint8::out) is semidet.

octet_from_string(S, I, U8) :-
    % These lookups are safe since our caller has checked the
    % length of S already.
    HiDigit = S ^ unsafe_elem(I),
    LoDigit = S ^ unsafe_elem(I + 1),
    hex_digit_to_int(HiDigit, Hi),
    hex_digit_to_int(LoDigit, Lo),
    U8 = cast_from_int(Hi * 16 + Lo).

:- pragma foreign_proc("Java",
    from_string(S::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        U = java.util.UUID.fromString(S);
        SUCCESS_INDICATOR = true;
    } catch (java.lang.IllegalArgumentException e) {
        U = ML_NIL_UUID;
        SUCCESS_INDICATOR = false;
    }
").

:- pragma foreign_proc("C#",
    from_string(S::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // TryParse allows UUIDs with leading and trailing whitespace.
    // For consistency with the other backends we do not want to allow
    // this.
    if (
        S.Length > 0 &&
        !System.Char.IsWhiteSpace(S[0]) &&
        !System.Char.IsWhiteSpace(S[S.Length - 1]) &&
        System.Guid.TryParseExact(S, \"D\", out U)
    ) {
        SUCCESS_INDICATOR = true;
    } else {
        U = System.Guid.Empty;
        SUCCESS_INDICATOR = false;
    }

").

det_from_string(S) =
    ( if from_string(S, U)
    then
        U
    else
        func_error("uuid.det_from_string: string is not a UUID")
    ).

uuid(S) = det_from_string(S).

%---------------------------------------------------------------------------%
%
% Conversion to byte list.
%

to_bytes(U) = Bytes :-
    U = uuid(
        TimeLow,
        TimeMid,
        TimeHiAndVersion,
        ClockSeqHiAndReserved,
        ClockSeqLow,
        Node0,
        Node1,
        Node2,
        Node3,
        Node4,
        Node5
    ),
    Bytes = [
        uint32_byte(TimeLow, 3),
        uint32_byte(TimeLow, 2),
        uint32_byte(TimeLow, 1),
        uint32_byte(TimeLow, 0),
        uint16_byte(TimeMid, 1),
        uint16_byte(TimeMid, 0),
        uint16_byte(TimeHiAndVersion, 1),
        uint16_byte(TimeHiAndVersion, 0),
        ClockSeqHiAndReserved,
        ClockSeqLow,
        Node0,
        Node1,
        Node2,
        Node3,
        Node4,
        Node5
    ].

:- func uint16_byte(uint16, int) = uint8.

uint16_byte(U16, N) =
    cast_from_int(cast_to_int((U16 >> (N * 8)) /\ 0xff_u16)).

:- func uint32_byte(uint32, int) = uint8.

uint32_byte(U32, N) =
    cast_from_int(cast_to_int((U32 >> (N * 8)) /\ 0xff_u32)).

:- pragma foreign_proc("Java",
    to_bytes(U::in) = (Bs::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Bs = list.empty_list();

    long lsb = U.getLeastSignificantBits();
    for (int s = 0; s <= 56; s += 8) {
        Bs = list.cons((byte) ((lsb >>> s) & 0xff), Bs);
    }

    long msb = U.getMostSignificantBits();
    for (int s = 0; s <= 56; s += 8) {
        Bs = list.cons((byte) ((msb >>> s) & 0xff), Bs);
    }
").

:- pragma foreign_proc("C#",
    to_bytes(U::in) = (Bs::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    byte[] bytes = U.ToByteArray();
    Bs = list.empty_list();
    Bs = list.cons(bytes[15], Bs);
    Bs = list.cons(bytes[14], Bs);
    Bs = list.cons(bytes[13], Bs);
    Bs = list.cons(bytes[12], Bs);
    Bs = list.cons(bytes[11], Bs);
    Bs = list.cons(bytes[10], Bs);

    Bs = list.cons(bytes[9], Bs);
    Bs = list.cons(bytes[8], Bs);

    Bs = list.cons(bytes[6], Bs);
    Bs = list.cons(bytes[7], Bs);

    Bs = list.cons(bytes[4], Bs);
    Bs = list.cons(bytes[5], Bs);

    Bs = list.cons(bytes[0], Bs);
    Bs = list.cons(bytes[1], Bs);
    Bs = list.cons(bytes[2], Bs);
    Bs = list.cons(bytes[3], Bs);
").

%---------------------------------------------------------------------------%

from_bytes(Bytes) = UUID :-
    list.length(Bytes, NumBytes),
    ( if list.length(Bytes, 16) then
        UUID = do_from_bytes(Bytes)
    else
        Msg = "from_bytes: expected 16 bytes; have " ++
            int_to_string(NumBytes),
        error(Msg)
    ).

:- func do_from_bytes(list(uint8)) = uuid.

do_from_bytes(Bytes) = UUID :-
    ( if
        Bytes = [B0, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13,
            B14, B15]
    then
        UUID = uuid(
            uint32.from_bytes_be(B0, B1, B2, B3),
            uint16.from_bytes_be(B4, B5),
            uint16.from_bytes_be(B6, B7),
            B8,
            B9,
            B10,
            B11,
            B12,
            B13,
            B14,
            B15
        )
    else
        unexpected($pred, "expected 16 bytes")
    ).

:- pragma foreign_proc("Java",
    do_from_bytes(Bs::in) = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    long msb = 0;
    for (int i = 0; i < 8; i++) {
        msb = (msb << 8) + (list.det_head(Bs).byteValue() & 0xff);
        Bs = list.det_tail(Bs);
    }

    long lsb = 0;
    for (int i = 0; i < 8; i++) {
        lsb = (lsb << 8) + (list.det_head(Bs).byteValue() & 0xff);
        Bs = list.det_tail(Bs);
    }

    U = new java.util.UUID(msb, lsb);
").

:- pragma foreign_code("C#", "

public static readonly byte[] bytes_to_set =
    {3, 2, 1, 0, 5, 4, 7, 6, 8, 9, 10, 11, 12, 13, 14, 15};
").

:- pragma foreign_proc("C#",
    do_from_bytes(Bs::in) = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    byte[] bytes = new byte[16];
    for (int i = 0; i < 16; i++) {
        bytes[bytes_to_set[i]] = (byte) list.det_head(Bs);
        Bs = list.det_tail(Bs);
    }
    U = new System.Guid(bytes);
").

%---------------------------------------------------------------------------%
%
% Random generation.
%

random_uuid(RNG, UUID, !State) :-
    some [!ByteArray] (
        array.generate_foldl(16, generate_byte(RNG), !:ByteArray, !State),
        some [!B6] (
            array.unsafe_lookup(!.ByteArray, 6, !:B6),
            !:B6 = !.B6 /\ 0x0f_u8,  % Clear version.
            !:B6 = !.B6 \/ 0x40_u8,  % Set to version 4 (randomly generated).
            array.unsafe_set(6, !.B6, !ByteArray)
        ),
        some [!B8] (
            array.unsafe_lookup(!.ByteArray, 8, !:B8),
            !:B8 = !.B8 /\ 0x3f_u8,  % Clear variant.
            !:B8 = !.B8 \/ 0x80_u8,  % Set to IETF variant.
            array.unsafe_set(8, !.B8, !ByteArray)
        ),
        Bytes = array.to_list(!.ByteArray),
        UUID = from_bytes(Bytes)
    ).

:- pred generate_byte(RNG::in, int::in, uint8::out, State::di, State::uo)
    is det <= urandom(RNG, State).

generate_byte(RNG, _, Byte, !State) :-
    random.generate_uint8(RNG, Byte, !State).

%---------------------------------------------------------------------------%
:- end_module uuid.
%---------------------------------------------------------------------------%
