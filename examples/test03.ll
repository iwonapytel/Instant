@dnl = internal constant [4 x i8] c"%d\0A\00"
declare i32 @printf(i8*, ...)
define void @printInt(i32 %x) {
%t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
ret void
}
define i32 @main() {
%1 = add nsw i32 1, 1
%2 = add nsw i32 1, %1
%3 = add nsw i32 1, %2
%4 = add nsw i32 1, %3
%5 = add nsw i32 1, %4
%6 = add nsw i32 1, %5
%7 = add nsw i32 1, %6
%8 = add nsw i32 1, %7
%9 = add nsw i32 1, %8
%10 = add nsw i32 1, %9
%11 = add nsw i32 1, %10
%12 = add nsw i32 1, %11
%13 = add nsw i32 1, %12
%14 = add nsw i32 1, %13
%15 = add nsw i32 1, %14
%16 = add nsw i32 1, %15
%17 = add nsw i32 1, %16
%18 = add nsw i32 1, %17
%19 = add nsw i32 1, %18
%20 = add nsw i32 1, %19
%21 = add nsw i32 1, %20
%22 = add nsw i32 1, %21
%23 = add nsw i32 1, %22
%24 = add nsw i32 1, %23
%25 = add nsw i32 1, %24
%26 = add nsw i32 1, %25
%27 = add nsw i32 1, %26
%28 = add nsw i32 1, %27
%29 = add nsw i32 1, %28
%30 = add nsw i32 1, %29
%31 = add nsw i32 1, %30
%32 = add nsw i32 1, %31
%33 = add nsw i32 1, %32
%34 = add nsw i32 1, %33
%35 = add nsw i32 1, %34
%36 = add nsw i32 1, %35
%37 = add nsw i32 1, %36
%38 = add nsw i32 1, %37
%39 = add nsw i32 1, %38
%40 = add nsw i32 1, %39
%41 = add nsw i32 1, %40
call void @printInt(i32 %41)
ret i32 0
}