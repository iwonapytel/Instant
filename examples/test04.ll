@dnl = internal constant [4 x i8] c"%d\0A\00"

declare i32 @printf(i8*, ...)

define void @printInt(i32 %x) {
%t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
ret void
}

define i32 @main() {
%a = alloca i32
store i32 1, i32* %a 
%b = alloca i32
store i32 2, i32* %b 
%1 = load i32, i32* %b
%2 = load i32, i32* %a
%3 = load i32, i32* %a
%4 = load i32, i32* %a
%5 = load i32, i32* %a
%6 = load i32, i32* %a
%7 = load i32, i32* %a
%8 = load i32, i32* %a
%9 = load i32, i32* %a
%10 = load i32, i32* %a
%11 = load i32, i32* %a
%12 = load i32, i32* %b
%13 = load i32, i32* %a
%14 = load i32, i32* %a
%15 = load i32, i32* %a
%16 = load i32, i32* %a
%17 = load i32, i32* %a
%18 = load i32, i32* %a
%19 = load i32, i32* %a
%20 = load i32, i32* %a
%21 = load i32, i32* %a
%22 = load i32, i32* %a
%23 = load i32, i32* %a
%24 = load i32, i32* %a
%25 = load i32, i32* %a
%26 = load i32, i32* %a
%27 = load i32, i32* %a
%28 = load i32, i32* %a
%29 = load i32, i32* %a
%30 = load i32, i32* %a
%31 = load i32, i32* %a
%32 = load i32, i32* %b
%33 = add nsw i32 %31, %32
%34 = add nsw i32 1, %33
%35 = add nsw i32 %30, %34
%36 = add nsw i32 %29, %35
%37 = add nsw i32 1, %36
%38 = add nsw i32 %28, %37
%39 = add nsw i32 %27, %38
%40 = add nsw i32 1, %39
%41 = add nsw i32 %26, %40
%42 = add nsw i32 %25, %41
%43 = add nsw i32 %24, %42
%44 = add nsw i32 %23, %43
%45 = add nsw i32 1, %44
%46 = add nsw i32 %22, %45
%47 = add nsw i32 %21, %46
%48 = add nsw i32 %20, %47
%49 = add nsw i32 %19, %48
%50 = add nsw i32 %18, %49
%51 = add nsw i32 %17, %50
%52 = add nsw i32 %16, %51
%53 = add nsw i32 %15, %52
%54 = add nsw i32 %14, %53
%55 = add nsw i32 %13, %54
%56 = add nsw i32 1, %55
%57 = add nsw i32 %12, %56
%58 = add nsw i32 %11, %57
%59 = add nsw i32 %10, %58
%60 = add nsw i32 %9, %59
%61 = add nsw i32 1, %60
%62 = add nsw i32 %8, %61
%63 = add nsw i32 %7, %62
%64 = add nsw i32 %6, %63
%65 = add nsw i32 %5, %64
%66 = add nsw i32 %4, %65
%67 = add nsw i32 1, %66
%68 = add nsw i32 %3, %67
%69 = add nsw i32 %2, %68
%70 = add nsw i32 %1, %69
call void @printInt(i32 %70)
ret i32 0
}