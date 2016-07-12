%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2016, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% A Mercury library providing universally unique identifiers (UUIDs).
% Note that this is a wrapper around whatever UUID functionality the underlying
% platform provides and not a Mercury implementation of them.
%
%---------------------------------------------------------------------------%

:- module uuid.
:- interface.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % A universally unique identifier (UUID).
    %
    % In C grades the representation is as follows:
    %   * For platforms that use libuuid it is a pointer to "uuid_t".
    %   * On Windows it is a pointer to "UUID".
    %
    % In the Java grade the representation is a "java.util.UUID" object.
    % In the C# grade the representation is a "System.Guid" object.
    %
    % XXX TODO grade independent comparison (currently backend dependent).
    %
:- type uuid.

    % generate(UUID, !IO):
    % Randomly generate a UUID.
    % Throws a software_error/1 exception if a UUID cannot be randomly
    % generated.
    %
:- pred generate(uuid::out, io::di, io::uo) is det.

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

    % to_bytes(UUID) = Bytes:
    %
    % Bytes is a list of unsigned bytes in the UUID ordered from most
    % significant to least significant byte.
    % The list of bytes is represented by list of ints, with each byte
    % occupying the lower 8 bits of an int.
    %
    % Note that the above ordering will be returned even on platforms where the
    % underlying representation is the Microsoft one.
    %
:- func to_bytes(uuid) = list(int).

    % from_bytes(Bytes) = UUID:
    %
    % Construct UUID from the list of unsigned bytes in Bytes.
    % Each byte is represented using the lower 8 bits of each int with
    % the remaining bits being ignored.
    % Bytes must be ordered from most significant to least significant byte.
    %
    % Note that the above ordering is used even on platforms where the
    % underlying representation is the Microsoft one.
    %
    % Throws a software_error/1 exception if length(Bytes, 16) is false.
    %
:- func from_bytes(list(int)) = uuid.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module require.
:- import_module string.

:- interface.

:- pred uuid_equal(uuid::in, uuid::in) is semidet.

:- pred uuid_compare(comparison_result::uo, uuid::in, uuid::in) is det.

:- implementation.

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
#if defined(MR_WIN32) && !defined(MR_CYGWIN)
    #include ""mercury_windows.h""
    #include <Rpc.h>
    typedef UUID MER_uuid;
#else
    #include <uuid/uuid.h>
    typedef uuid_t MER_uuid;
#endif

").

:- pragma foreign_type("C", uuid, "MER_uuid *", [can_pass_as_mercury_type])
     where equality is uuid_equal,
           comparison is uuid_compare.

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

:- pragma foreign_proc("C",
    uuid_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
#if defined(MR_WIN32) && !defined(MR_CYGWIN)
    RPC_STATUS status;
    SUCCESS_INDICATOR = (UuidEqual(A, B, &status)) ? MR_TRUE : MR_FALSE;
#else
    SUCCESS_INDICATOR = (uuid_compare(*A, *B) == 0) ? MR_TRUE : MR_FALSE;
#endif
").

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

:- pragma foreign_proc("C",
    uuid_compare(R::uo, A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    int r;
#if defined(MR_WIN32) && !defined(MR_CYGWIN)
    RPC_STATUS status;
    r = UuidCompare(A, B, &status);
#else
    r = uuid_compare(*A, *B);
#endif
    if (r < 0) {
        R = MR_COMPARE_LESS;
    } else if (r > 0) {
        R = MR_COMPARE_GREATER;
    } else {
        R = MR_COMPARE_EQUAL;
    }
").

:- pragma foreign_proc("Java",
    uuid_compare(R::uo, A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int r = A.compareTo(B);
    if (r < 0) {
        R = builtin.COMPARE_LESS;
    } else if (r > 0) {
        R = builtin.COMPARE_GREATER;
    } else {
        R = builtin.COMPARE_EQUAL;
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
%
% Random generation.
%

% XXX are these actually thread safe?


generate(U, !IO) :-
    do_generate(U, Ok, !IO),
    (
        Ok = yes
    ;
        Ok = no,
        error("uuid.generate: cannot generate random UUID")
    ).

:- pred do_generate(uuid::out, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_generate(U::out, Res::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe, tabled_for_io,
        will_not_modify_trail],
"
#if defined(MR_WIN32) && !defined(MR_CYGWIN)
    U = MR_GC_malloc_atomic(sizeof(UUID));
    if (UuidCreate(U) == RPC_S_OK) {
        Res = MR_YES;
    } else {
        Res = MR_NO;
    }
#else
    U = MR_GC_malloc_atomic(sizeof(uuid_t));
    uuid_generate(*U);
    Res = MR_YES;
#endif
").

:- pragma foreign_proc("Java",
    do_generate(U::out, Res::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe],
"
    U = java.util.UUID.randomUUID();
    Res = bool.YES;
").

:- pragma foreign_proc("C#",
    do_generate(U::out, Res::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe],
"
    U = System.Guid.NewGuid();
    Res = mr_bool.YES;
").

%---------------------------------------------------------------------------%
%
% Conversion to a string.
%

:- pragma foreign_proc("C",
    to_string(U::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
#if defined(MR_WIN32) && !defined(MR_CYGWIN)
    RPC_STATUS res;
    RPC_CSTR c_uuid = NULL;
    S = MR_GC_malloc(sizeof(char) * 37);
    res = UuidToString(U, &c_uuid);
    if (res == RPC_S_OK) {
        MR_make_aligned_string_copy(S, (const char *)c_uuid);
        RpcStringFree(&c_uuid);
    } else {
        /* res == RCP_S_OUT_OF_MEMORY */
        MR_external_fatal_error(""mercury_uuid"",
            ""cannot allocate memory for UUID to string conversion"");
    }
#else
    S = MR_GC_malloc(sizeof(char) * 37);
    uuid_unparse_lower(*U, S);
#endif
").

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

:- pragma foreign_proc("C",
    from_string(S::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
#if defined(MR_WIN32) && !defined(MR_CYGWIN)
    UUID u;
    if (UuidFromString((unsigned char *)S, &u) == RPC_S_OK) {
        U = MR_GC_malloc_atomic(sizeof(UUID));
        MR_memcpy(U, &u, sizeof(UUID));
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
#else
    uuid_t u;
    if (uuid_parse(S, u) == 0) {
        U = MR_GC_malloc_atomic(sizeof(uuid_t));
        MR_memcpy(U, u, sizeof(uuid_t));
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
#endif
").

:- pragma foreign_proc("Java",
    from_string(S::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        U = java.util.UUID.fromString(S);
        SUCCESS_INDICATOR = true;
    } catch (java.lang.IllegalArgumentException e) {
        U = new java.util.UUID(0, 0);  // Dummy value - XXX make static member.
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
    then U
    else func_error("uuid.det_from_string: string is not a UUID")
    ).

%---------------------------------------------------------------------------%
%
% Conversion to byte list.
%

:- pragma foreign_proc("C",
    to_bytes(U::in) = (Bs::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    Bs = MR_list_empty();
#if defined(MR_WIN32) && !defined(MR_CYGWIN)

    unsigned long data1 = U->Data1;
    unsigned short data2 = U->Data2;
    unsigned short data3 = U->Data3;

    Bs = MR_list_cons(U->Data4[7], Bs);
    Bs = MR_list_cons(U->Data4[6], Bs);
    Bs = MR_list_cons(U->Data4[5], Bs);
    Bs = MR_list_cons(U->Data4[4], Bs);
    Bs = MR_list_cons(U->Data4[3], Bs);
    Bs = MR_list_cons(U->Data4[2], Bs);

    Bs = MR_list_cons(U->Data4[1], Bs);
    Bs = MR_list_cons(U->Data4[0], Bs);

    Bs = MR_list_cons(((unsigned char *)(&data3))[0], Bs);
    Bs = MR_list_cons(((unsigned char *)(&data3))[1], Bs);

    Bs = MR_list_cons(((unsigned char *)(&data2))[0], Bs);
    Bs = MR_list_cons(((unsigned char *)(&data2))[1], Bs);

    Bs = MR_list_cons(((unsigned char *)(&data1))[0], Bs);
    Bs = MR_list_cons(((unsigned char *)(&data1))[1], Bs);
    Bs = MR_list_cons(((unsigned char *)(&data1))[2], Bs);
    Bs = MR_list_cons(((unsigned char *)(&data1))[3], Bs);

#else

    for (int i = 15; i >= 0; i--) {
        Bs = MR_list_cons((*U)[i], Bs);
    }

#endif
").

:- pragma foreign_proc("Java",
    to_bytes(U::in) = (Bs::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Bs = list.empty_list();

    long lsb = U.getLeastSignificantBits();
    for (int s = 0; s <= 56; s+=8) {
        Bs = list.cons((int)((byte)(lsb >>> s) & 0xff), Bs);
    }

    long msb = U.getMostSignificantBits();
    for (int s = 0; s <= 56; s+=8) {
        Bs = list.cons((int)((byte)(msb >>> s) & 0xff), Bs);
    }
").

:- pragma foreign_proc("C#",
    to_bytes(U::in) = (Bs::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    byte[] bytes = U.ToByteArray();
    Bs = list.empty_list();
    Bs = list.cons((int)bytes[15], Bs);
    Bs = list.cons((int)bytes[14], Bs);
    Bs = list.cons((int)bytes[13], Bs);
    Bs = list.cons((int)bytes[12], Bs);
    Bs = list.cons((int)bytes[11], Bs);
    Bs = list.cons((int)bytes[10], Bs);

    Bs = list.cons((int)bytes[9], Bs);
    Bs = list.cons((int)bytes[8], Bs);

    Bs = list.cons((int)bytes[6], Bs);
    Bs = list.cons((int)bytes[7], Bs);

    Bs = list.cons((int)bytes[4], Bs);
    Bs = list.cons((int)bytes[5], Bs);

    Bs = list.cons((int)bytes[0], Bs);
    Bs = list.cons((int)bytes[1], Bs);
    Bs = list.cons((int)bytes[2], Bs);
    Bs = list.cons((int)bytes[3], Bs);
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

:- func do_from_bytes(list(int)) = uuid.

:- pragma foreign_proc("C",
    do_from_bytes(Bs::in) = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
#if defined(MR_WIN32) && !defined(MR_CYGWIN)

    unsigned long data1 = 0;
    unsigned short data2 = 0;
    unsigned short data3 = 0;

    U = MR_GC_malloc_atomic(sizeof(UUID));

    for (int i = 3; i >= 0; i--) {
        ((unsigned char *)(&data1))[i] = (unsigned char) MR_list_head(Bs);
        Bs = MR_list_tail(Bs);
    }
    U->Data1 = data1;

    ((unsigned char *)(&data2))[1] = (unsigned char) MR_list_head(Bs);
    Bs = MR_list_tail(Bs);
    ((unsigned char *)(&data2))[0] = (unsigned char) MR_list_head(Bs);
    Bs = MR_list_tail(Bs);
    U->Data2 = data2;

    ((unsigned char *)(&data3))[1] = (unsigned char) MR_list_head(Bs);
    Bs = MR_list_tail(Bs);
    ((unsigned char *)(&data3))[0] = (unsigned char) MR_list_head(Bs);
    Bs = MR_list_tail(Bs);
    U->Data3 = data3;

    for (int i = 0; i < 8; i++) {
        U->Data4[i] = (unsigned char) MR_list_head(Bs);
        Bs = MR_list_tail(Bs);
    }

#else

    U = MR_GC_malloc_atomic(sizeof(uuid_t));
    for (int i = 0; i < 16; i++) {
        (*U)[i] = MR_list_head(Bs);
        Bs = MR_list_tail(Bs);
    }

#endif
").

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
        bytes[bytes_to_set[i]] = (byte)((int)list.det_head(Bs));
        Bs = list.det_tail(Bs);
    }
    U = new System.Guid(bytes);
").

%---------------------------------------------------------------------------%
:- end_module uuid.
%---------------------------------------------------------------------------%
