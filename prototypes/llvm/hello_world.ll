; ModuleID = 'Hello.ll'
%CSTR = type [32 x i8]
@.str_1 = internal constant %CSTR c"Hello world with %d arguments!\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main(i32 %argc, i8** %argv) {
	%tmp.0 = call i32 (i8*, ...)* @printf(
         i8* getelementptr (%CSTR* @.str_1, i64 0, i64 0), 
         i32 %argc )
	ret i32 0
}
