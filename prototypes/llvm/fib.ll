; ModuleID = 'Fibonacci'
@.str_1 = internal constant [19 x i8] c"The result is %d!\0A\00"
@.str_2 = internal constant [19 x i8] c"Usage: fib <num1>\0A\00"

declare i32 @printf(i8*, ...)
declare i32 @atoi(i8*)

define i32 @main(i32 %argc, i8** %argv) {

    %has.args = icmp eq i32 %argc, 2
    br i1 %has.args, label %DoIt, label %Usage

  DoIt:
    %tmp.0 = getelementptr i8** %argv, i64 1
    %str = load i8** %tmp.0
    %num = call i32 @atoi (i8* %str)
    call i32 (i8*, ...)* @printf(
         i8* getelementptr ([19 x i8]* @.str_1, i32 0, i32 0),
         i32 %num)
    ret i32 0
    
  Usage:
    call i32 (i8*, ...)* @printf(
         i8* getelementptr ([19 x i8]* @.str_2, i32 0, i32 0))
    ret i32 1
}
