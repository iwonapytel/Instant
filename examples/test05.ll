@dnl = internal constant [4 x i8] c"%d\0A\00"

declare i32 @printf(i8*, ...)

define void @printInt(i32 %x) {
%t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
ret void
}

define i32 @main() {
%1 = sub nsw i32 1, 1
%2 = sub nsw i32 1, 1
%3 = sub nsw i32 1, 1
%4 = sub nsw i32 1, 1
%5 = sub nsw i32 1, 1
%6 = sub nsw i32 1, 1
%7 = sub nsw i32 1, 1
%8 = sub nsw i32 1, 1
%9 = sub nsw i32 1, 1
%10 = sub nsw i32 1, 1
%11 = sub nsw i32 1, 1
%12 = sub nsw i32 1, 1
%13 = sub nsw i32 1, 1
%14 = sub nsw i32 1, 1
%15 = sub nsw i32 1, 1
%16 = sub nsw i32 1, 1
%17 = sub nsw i32 1, 1
%18 = sub nsw i32 1, 1
%19 = sub nsw i32 1, 1
%20 = add nsw i32 %18, %19
%21 = add nsw i32 %17, %20
%22 = add nsw i32 %16, %21
%23 = add nsw i32 %15, %22
%24 = add nsw i32 %14, %23
%25 = add nsw i32 %13, %24
%26 = add nsw i32 %12, %25
%27 = add nsw i32 %11, %26
%28 = add nsw i32 %10, %27
%29 = add nsw i32 %9, %28
%30 = add nsw i32 %8, %29
%31 = add nsw i32 %7, %30
%32 = add nsw i32 %6, %31
%33 = add nsw i32 %5, %32
%34 = add nsw i32 %4, %33
%35 = add nsw i32 %3, %34
%36 = add nsw i32 %2, %35
%37 = add nsw i32 %1, %36
%38 = add nsw i32 1, %37
call void @printInt(i32 %38)
ret i32 0
}