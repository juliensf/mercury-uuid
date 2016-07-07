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

%---------------------------------------------------------------------------%

:- type uuid.

    % generate(U, !IO):
    % U is a randomly generate UUID.
    %
:- pred generate(uuid::out, io::di, io::uo) is det.

    % to_string(U) = S:
    % S is the string representation of U.
    %
:- func to_string(uuid) = string.

    % from_string(S, U):
    % Convert a string of the form:
    %
    %    xxxxxxx-xxxx-xxxx-xxxxxxxxxxxx
    %
    % where 'x' is a hexadecimal digit into a UUID.
    % Leading and trailing whitespace is _not_ allowed.
    % Fails if S is not of the above form.
    %
:- pred from_string(string::in, uuid::out) is semidet.

:- func det_from_string(string) = uuid.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

:- interface.

:- pred uuid_equal(uuid::in, uuid::in) is semidet.

:- pred uuid_compare(comparison_result::uo, uuid::in, uuid::in) is det.

:- implementation.

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "#include <uuid/uuid.h>").

:- pragma foreign_type("C", uuid, "uuid_t *", [can_pass_as_mercury_type])
     where equality is uuid_equal,
           comparison is uuid_compare.

:- pragma foreign_type("Java", uuid, "java.util.UUID")
    where equality is uuid_equal,
          comparison is uuid_compare.

:- pragma foreign_type("C#", uuid, "System.Guid").

%---------------------------------------------------------------------------%
%
% Equality.
%

:- pragma foreign_proc("C",
    uuid_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (uuid_compare(*A, *B) == 0) ? MR_TRUE : MR_FALSE;
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
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int r;
    r = uuid_compare(*A, *B);
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

:- pragma foreign_proc("C",
    generate(U::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe],
"
    U = MR_GC_NEW(uuid_t);
    uuid_generate(*U);
").

:- pragma foreign_proc("Java",
    generate(U::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe],
"
    U = java.util.UUID.randomUUID();
").

:- pragma foreign_proc("C#",
    generate(U::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe],
"
    U = System.Guid.NewGuid();
").

%---------------------------------------------------------------------------%
%
% Conversion to a string.
%

:- pragma foreign_proc("C",
    to_string(U::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = MR_GC_malloc(sizeof(char) * 37);
    uuid_unparse_lower(*U, S);
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
    [will_not_call_mercury, promise_pure, thread_safe],
"
    uuid_t u;
    if (uuid_parse(S, u) == 0) {
        U = MR_GC_NEW(uuid_t);
        MR_memcpy(U, u, 16);
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
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
:- end_module uuid.
%---------------------------------------------------------------------------%
