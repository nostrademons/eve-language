; ModuleID = 'Add2.ll'
@.str_1 = internal constant [19 x i8] c"The result is %d!\0A\00"
@.str_2 = internal constant [25 x i8] c"Usage: add2 <num1> <num2>"

declare i32 @printf(i8*, ...)
declare i32 @puts(i8*)
declare i32 @atoi(i8*)

define i32 @main(i32 %argc, i8** %argv) {

    %has.args = icmp eq i32 %argc, 3
    br i1 %has.args, label %DoIt, label %Usage

  DoIt:
    %tmp.1 = getelementptr i8** %argv, i64 1
    %str.1 = load i8** %tmp.1
    %num.1 = call i32 (i8*)* @atoi(i8* %str.1)

    %tmp.2 = getelementptr i8** %argv, i64 2
    %str.2 = load i8** %tmp.2
    %num.2 = call i32 (i8*)* @atoi(i8* %str.2)

    %sum = add i32 %num.1, %num.2

	call i32 (i8*, ...)* @printf(
         i8* getelementptr ([19 x i8]* @.str_1, i32 0, i32 0), 
         i32 %sum )
	ret i32 0

  Usage:
    call i32 (i8*)* @puts(
         i8* getelementptr ([25 x i8]* @.str_2, i32 0, i32 0))
    ret i32 1
}
