target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-darwin15.2.0"

%myint = type { i64 }
%mytype = type { %myint, i32, i8 }

@.global_thing = private global i32 42;

define i32* @addf(%mytype* %a) nounwind alwaysinline {
    %1 = alloca %mytype;
    store %mytype zeroinitializer, %mytype* %1;
    %a2ptr = getelementptr %mytype, %mytype* %a, i32 0, i32 1;
    ret i32* %a2ptr;
}
