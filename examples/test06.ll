@dnl = internal constant [4 x i8] c"%d\0A\00"

declare i32 @printf(i8*, ...)

define void @printInt(i32 %x) {
%t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
ret void
}

define i32 @main() {
%a = alloca i32
store i32 0, i32* %a 
%b = alloca i32
store i32 1, i32* %b 
%c = alloca i32
store i32 0, i32* %c 
%d = alloca i32
store i32 1, i32* %d 
%e = alloca i32
store i32 0, i32* %e 
%f = alloca i32
store i32 1, i32* %f 
%g = alloca i32
store i32 0, i32* %g 
%h = alloca i32
store i32 1, i32* %h 
%1 = load i32, i32* %a
%2 = load i32, i32* %b
%3 = mul nsw i32 %1, %2
%4 = load i32, i32* %c
%5 = load i32, i32* %d
%6 = mul nsw i32 %4, %5
%7 = load i32, i32* %e
%8 = load i32, i32* %f
%9 = load i32, i32* %g
%10 = load i32, i32* %h
%11 = add nsw i32 %9, %10
%12 = add nsw i32 %8, %11
%13 = add nsw i32 %7, %12
%14 = add nsw i32 %6, %13
%15 = add nsw i32 %3, %14
call void @printInt(i32 %15)
store i32 1, i32* %a 
store i32 2, i32* %b 
store i32 1, i32* %c 
store i32 2, i32* %d 
store i32 1, i32* %e 
store i32 2, i32* %f 
store i32 1, i32* %g 
store i32 2, i32* %h 
%i = alloca i32
store i32 1, i32* %i 
%j = alloca i32
store i32 2, i32* %j 
%k = alloca i32
store i32 1, i32* %k 
%l = alloca i32
store i32 2, i32* %l 
%m = alloca i32
store i32 1, i32* %m 
%n = alloca i32
store i32 2, i32* %n 
%16 = load i32, i32* %a
%17 = mul nsw i32 2, %16
%18 = load i32, i32* %b
%19 = sdiv i32 %18, 2
%20 = load i32, i32* %c
%21 = load i32, i32* %d
%22 = load i32, i32* %e
%23 = load i32, i32* %f
%24 = load i32, i32* %g
%25 = load i32, i32* %h
%26 = load i32, i32* %i
%27 = load i32, i32* %j
%28 = sdiv i32 %27, 2
%29 = load i32, i32* %k
%30 = load i32, i32* %l
%31 = load i32, i32* %m
%32 = load i32, i32* %n
%33 = add nsw i32 %31, %32
%34 = add nsw i32 %30, %33
%35 = add nsw i32 %29, %34
%36 = add nsw i32 %28, %35
%37 = add nsw i32 %26, %36
%38 = add nsw i32 %25, %37
%39 = add nsw i32 %24, %38
%40 = add nsw i32 %23, %39
%41 = add nsw i32 %22, %40
%42 = add nsw i32 %21, %41
%43 = add nsw i32 %20, %42
%44 = add nsw i32 %19, %43
%45 = add nsw i32 %17, %44
%46 = sdiv i32 %45, 10
call void @printInt(i32 %46)
ret i32 0
}