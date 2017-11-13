; ModuleID = 'Dummy'
source_filename = "<string>"

@result = private global i32 0

define i32 @main() {
entry:
  %L0 = alloca i32
  %L1 = alloca i32
  %L2 = alloca i32
  %L3 = alloca i32
  store i32 4096, i32* %L0
  store i32 8192, i32* %L1
  store i32 12288, i32* %L2
  %0 = load i32, i32* %L0
  %1 = load i32, i32* %L1
  %2 = add i32 %1, %0
  %3 = load i32, i32* %L2
  %4 = add i32 %3, %2
  store i32 %4, i32* %L3
  %5 = load i32, i32* %L3
  %6 = load i32, i32* %L0
  br label %BB8

BB8:                                              ; preds = %entry
  %7 = icmp sle i32 %5, %6
  br i1 %7, label %BB1, label %BB0

BB0:                                              ; preds = %BB8
  %8 = load i32, i32* %L3
  store i32 %8, i32* @result
  br label %BB2

BB1:                                              ; preds = %BB8
  %9 = load i32, i32* %L1
  %10 = load i32, i32* %L3
  br label %BB9

BB9:                                              ; preds = %BB1
  %11 = icmp sle i32 %9, %10
  br i1 %11, label %BB4, label %BB3

BB3:                                              ; preds = %BB9
  %12 = load i32, i32* %L1
  store i32 %12, i32* @result
  br label %BB2

BB4:                                              ; preds = %BB9
  %13 = load i32, i32* %L2
  %14 = load i32, i32* %L3
  br label %BB10

BB10:                                             ; preds = %BB4
  %15 = icmp sle i32 %13, %14
  br i1 %15, label %BB6, label %BB5

BB5:                                              ; preds = %BB10
  %16 = load i32, i32* %L2
  store i32 %16, i32* @result
  br label %BB2

BB6:                                              ; preds = %BB10
  %17 = load i32, i32* %L0
  store i32 %17, i32* @result
  br label %BB2

BB2:                                              ; preds = %BB6, %BB5, %BB3, %BB0
  %18 = load i32, i32* @result
  ret i32 %18

BB7:                                              ; No predecessors!
  ret i32 0
}
