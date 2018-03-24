@int = internal constant [4 x i8] c"%d\0A\00"
@ints = internal constant [3 x i8] c"%d\00"
@str = internal constant [4 x i8] c"%s\0A\00"
@mstr = internal constant [5 x i8] c"%ms\0A\00"
@rerr = internal constant [15 x i8] c"runtime error\0A\00"

%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i8*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type { %struct._IO_marker*, %struct._IO_FILE*, i32 }

@stdin = external global %struct._IO_FILE*, align 8

declare i32 @scanf(i8*, ...)
declare i32 @printf(i8*, ...)
declare void @exit(i32)
declare i64 @getline(i8**, i64*, %struct._IO_FILE*)
declare i64 @strlen(i8*)
declare i8* @malloc(i64)
declare i8* @strcpy(i8*, i8*)
declare i8* @strcat(i8*, i8*)

define void @printInt(i32 %v) {
  %form = getelementptr [4 x i8], [4 x i8]* @int, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %form, i32 %v)
  ret void
}

define void @printString(i8* %v) {
  %form = getelementptr [4 x i8], [4 x i8]* @str, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %form, i8* %v)
  ret void
}

define void @error() {
  %s = getelementptr [15 x i8], [15 x i8]* @rerr, i32 0, i32 0
  %form = getelementptr [4 x i8], [4 x i8]* @str, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %s)
  call void @exit(i32 1)
  ret void
}

define i32 @readInt() {
  %v = alloca i32
  %form = getelementptr [3 x i8], [3 x i8]* @ints, i32 0, i32 0
  call i32 (i8*, ...) @scanf(i8* %form, i32* %v)
  %res = load i32, i32* %v
  ret i32 %res
}

define i8* @readString() {
  %v = alloca i8*
  store i8* null, i8** %v
  %n = alloca i64
  store i64 0, i64* %n
  
  %v1 = load %struct._IO_FILE*, %struct._IO_FILE** @stdin, align 8
  %v2 = call i64 @getline(i8** %v, i64* %n, %struct._IO_FILE* %v1)
  %res = load i8*, i8** %v
  ret i8* %res
}

define i8* @concatString(i8*, i8*) {
  %3 = alloca i8*, align 8
  %4 = alloca i8*, align 8
  %5 = alloca i32, align 4
  %6 = alloca i8*, align 8
  store i8* %0, i8** %3, align 8
  store i8* %1, i8** %4, align 8
  %7 = load i8*, i8** %3, align 8
  %8 = call i64 @strlen(i8* %7) #4
  %9 = load i8*, i8** %4, align 8
  %10 = call i64 @strlen(i8* %9) #4
  %11 = add i64 %8, %10
  %12 = add i64 %11, 1
  %13 = trunc i64 %12 to i32
  store i32 %13, i32* %5, align 4
  %14 = load i32, i32* %5, align 4
  %15 = sext i32 %14 to i64
  %16 = call i8* @malloc(i64 %15)
  store i8* %16, i8** %6, align 8
  %17 = load i8*, i8** %6, align 8
  %18 = load i8*, i8** %3, align 8
  %19 = call i8* @strcpy(i8* %17, i8* %18)
  %20 = load i8*, i8** %6, align 8
  %21 = load i8*, i8** %4, align 8
  %22 = call i8* @strcat(i8* %20, i8* %21)
  %23 = load i8*, i8** %6, align 8
  ret i8* %23
}

; Tablice
; Layout tablic w pamięci:
; 4 bajty - rozmiar tablicy
; x bajtów - elementy tablicy

define i32 @arrlen(i8*) {
  %2 = getelementptr inbounds i8, i8* %0, i64 -4
  %3 = bitcast i8* %2 to i32*
  %4 = load i32, i32* %3, align 4
  ret i32 %4
}

; Tworzy tablicę
; Argumenty:
; rozmiar tablicy
; rozmiar pojedyńczego elementu
define i8* @arrmk(i32, i32) {
  %3 = mul nsw i32 %1, %0
  %4 = add nsw i32 %3, 4
  %5 = sext i32 %4 to i64
  %6 = tail call i8* @malloc(i64 %5)
  %7 = bitcast i8* %6 to i32*
  store i32 %0, i32* %7, align 4
  %8 = getelementptr inbounds i8, i8* %6, i64 4
  ret i8* %8
}

