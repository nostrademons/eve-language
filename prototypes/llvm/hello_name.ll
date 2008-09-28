; ModuleID = 'Hello Name.ll'
@.str_1 = internal constant [12 x i8] c"Hello, %s!\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main(i32 %argc, i8** %argv) {
    %tmp.1 = getelementptr i8** %argv, i64 1
    %name = load i8** %tmp.1
	%tmp.0 = call i32 (i8*, ...)* @printf(
         i8* getelementptr ([12 x i8]* @.str_1, i64 0, i64 0), 
         i8* %name)
	ret i32 0
}
