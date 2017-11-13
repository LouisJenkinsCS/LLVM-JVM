; ModuleID = 'Dummy'
source_filename = "<string>"

@result = private global i32 0

define i32 @main() {
BB8:
  %0 = add i32 8192, 4096
  %1 = add i32 12288, %0
  %2 = icmp sle i32 %1, 4096
  br i1 %2, label %BB9, label %BB0

BB0:                                              ; preds = %BB8
  store i32 %1, i32* @result
  br label %BB2

BB9:                                              ; preds = %BB8
  %3 = icmp sle i32 8192, %1
  br i1 %3, label %BB10, label %BB3

BB3:                                              ; preds = %BB9
  store i32 8192, i32* @result
  br label %BB2

BB10:                                             ; preds = %BB9
  %4 = icmp sle i32 12288, %1
  br i1 %4, label %BB6, label %BB5

BB5:                                              ; preds = %BB10
  store i32 12288, i32* @result
  br label %BB2

BB6:                                              ; preds = %BB10
  store i32 4096, i32* @result
  br label %BB2

BB2:                                              ; preds = %BB6, %BB5, %BB3, %BB0
  %5 = load i32, i32* @result
  ret i32 %5
}
