.class public test06
.super java/lang/Object
.method public <init>()V
.limit stack 1
.limit locals 1
.line 1
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method

.method public static main([Ljava/lang/String;)V
.limit stack 4
.limit locals 15
iconst_0
istore 1
iconst_1
istore 2
iconst_0
istore 3
iconst_1
istore 4
iconst_0
istore 5
iconst_1
istore 6
iconst_0
istore 7
iconst_1
istore 8
getstatic java/lang/System/out Ljava/io/PrintStream;
iload 7
iload 8
iadd
iload 6
swap
iadd
iload 5
swap
iadd
iload 3
iload 4
imul
swap
iadd
iload 1
iload 2
imul
swap
iadd
invokevirtual java/io/PrintStream/println(I)V
iconst_1
istore 1
iconst_2
istore 2
iconst_1
istore 3
iconst_2
istore 4
iconst_1
istore 5
iconst_2
istore 6
iconst_1
istore 7
iconst_2
istore 8
iconst_1
istore 9
iconst_2
istore 10
iconst_1
istore 11
iconst_2
istore 12
iconst_1
istore 13
iconst_2
istore 14
getstatic java/lang/System/out Ljava/io/PrintStream;
iload 13
iload 14
iadd
iload 12
swap
iadd
iload 11
swap
iadd
iload 10
iconst_2
idiv
swap
iadd
iload 9
swap
iadd
iload 8
swap
iadd
iload 7
swap
iadd
iload 6
swap
iadd
iload 5
swap
iadd
iload 4
swap
iadd
iload 3
swap
iadd
iload 2
iconst_2
idiv
swap
iadd
iconst_2
iload 1
imul
swap
iadd
ldc 10
idiv
invokevirtual java/io/PrintStream/println(I)V
return
.end method