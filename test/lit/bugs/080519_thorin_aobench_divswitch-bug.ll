; RUN: rvTool -wfv -i %s -k lambda_106958 -t lambda_106958_vectorize -s C_U_U_U_U 

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%0 = type { %1, %1 }
%1 = type { float, float, float }

define void @ao_impala(i32 %w_106943, i32 %h_106944, i32 %nsubsamples_106945, [0 x float]* %image_106946) {
ao_impala_start:
  br label %ao_impala

ao_impala:                                        ; preds = %ao_impala_start
  call void @lambda_106958_vectorize(i32 0, i32 %nsubsamples_106945, i32 %h_106944, i32 %w_106943, [0 x float]* %image_106946)
  br label %return

return:                                           ; preds = %ao_impala
  ret void
}

declare void @lambda_106958_vectorize(i32, i32, i32, i32, [0 x float]*)

define internal void @lambda_106958(i32 %i_106960, i32 %_106962, i32 %_106963, i32 %_106964, [0 x float]* %_106965) {
lambda_106958_start:
  %0 = ashr i32 %i_106960, 2
  %1 = sitofp i32 %_106964 to float
  %2 = sitofp i32 %_106963 to float
  %3 = fdiv float %2, 2.000000e+00
  %4 = fdiv float %1, 2.000000e+00
  %5 = and i32 3, %i_106960
  %6 = lshr i32 %i_106960, 16
  %7 = sitofp i32 %_106962 to float
  %8 = fdiv float %1, %2
  %9 = and i32 65535, %i_106960
  %10 = and i32 -16777216, %i_106960
  %11 = and i32 65280, %i_106960
  %12 = and i32 16711680, %i_106960
  %13 = and i32 255, %i_106960
  %14 = xor i32 -1091571699, %i_106960
  %invSamples = fdiv float 1.000000e+00, %7
  %15 = shl i32 %9, 16
  %16 = lshr i32 %10, 24
  %17 = shl i32 %11, 8
  %18 = lshr i32 %12, 8
  %19 = shl i32 %13, 24
  %20 = fmul float %invSamples, %invSamples
  %21 = or i32 %15, %6
  %22 = or i32 %19, %17
  %23 = or i32 %22, %18
  %24 = or i32 %23, %16
  br label %unroll_step

unroll_step:                                      ; preds = %expr_false3, %lambda_106958_start
  %isect_107051.sroa.9.0 = phi float [ undef, %lambda_106958_start ], [ %isect_107051.sroa.9.1.lcssa, %expr_false3 ]
  %isect_107051.sroa.14.0 = phi float [ undef, %lambda_106958_start ], [ %isect_107051.sroa.14.1.lcssa, %expr_false3 ]
  %isect_107051.sroa.19.0 = phi float [ undef, %lambda_106958_start ], [ %isect_107051.sroa.19.1.lcssa, %expr_false3 ]
  %isect_107051.sroa.24.0 = phi float [ undef, %lambda_106958_start ], [ %isect_107051.sroa.24.1.lcssa, %expr_false3 ]
  %rngstate_107438.sroa.27.0 = phi i32 [ %24, %lambda_106958_start ], [ %rngstate_107438.sroa.27.1.lcssa, %expr_false3 ]
  %rngstate_107438.sroa.18.0 = phi i32 [ %21, %lambda_106958_start ], [ %rngstate_107438.sroa.18.1.lcssa, %expr_false3 ]
  %rngstate_107438.sroa.9.0 = phi i32 [ %14, %lambda_106958_start ], [ %rngstate_107438.sroa.9.1.lcssa, %expr_false3 ]
  %rngstate_107438.sroa.0.0 = phi i32 [ %i_106960, %lambda_106958_start ], [ %rngstate_107438.sroa.0.1.lcssa, %expr_false3 ]
  %isect_107051.sroa.30.0 = phi float [ undef, %lambda_106958_start ], [ %isect_107051.sroa.30.1.lcssa, %expr_false3 ]
  %isect_107051.sroa.36.0 = phi float [ undef, %lambda_106958_start ], [ %isect_107051.sroa.36.1.lcssa, %expr_false3 ]
  %lower = phi i32 [ %30, %expr_false3 ], [ 0, %lambda_106958_start ]
  %25 = icmp slt i32 %lower, %_106963
  br i1 %25, label %expr_true, label %expr_false

expr_false:                                       ; preds = %unroll_step
  ret void

expr_true:                                        ; preds = %unroll_step
  %26 = add nsw i32 %lower, %0
  %27 = mul nsw i32 %26, %_106964
  %28 = sitofp i32 %26 to float
  br label %unroll_step1

unroll_step1:                                     ; preds = %expr_false7, %expr_true
  %isect_107051.sroa.9.1 = phi float [ %isect_107051.sroa.9.0, %expr_true ], [ %isect_107051.sroa.9.2.lcssa, %expr_false7 ]
  %isect_107051.sroa.14.1 = phi float [ %isect_107051.sroa.14.0, %expr_true ], [ %isect_107051.sroa.14.2.lcssa, %expr_false7 ]
  %isect_107051.sroa.19.1 = phi float [ %isect_107051.sroa.19.0, %expr_true ], [ %isect_107051.sroa.19.2.lcssa, %expr_false7 ]
  %isect_107051.sroa.24.1 = phi float [ %isect_107051.sroa.24.0, %expr_true ], [ %isect_107051.sroa.24.2.lcssa, %expr_false7 ]
  %rngstate_107438.sroa.27.1 = phi i32 [ %rngstate_107438.sroa.27.0, %expr_true ], [ %rngstate_107438.sroa.27.2.lcssa, %expr_false7 ]
  %rngstate_107438.sroa.18.1 = phi i32 [ %rngstate_107438.sroa.18.0, %expr_true ], [ %rngstate_107438.sroa.18.2.lcssa, %expr_false7 ]
  %rngstate_107438.sroa.9.1 = phi i32 [ %rngstate_107438.sroa.9.0, %expr_true ], [ %rngstate_107438.sroa.9.2.lcssa, %expr_false7 ]
  %rngstate_107438.sroa.0.1 = phi i32 [ %rngstate_107438.sroa.0.0, %expr_true ], [ %rngstate_107438.sroa.0.2.lcssa, %expr_false7 ]
  %isect_107051.sroa.30.1 = phi float [ %isect_107051.sroa.30.0, %expr_true ], [ %isect_107051.sroa.30.2.lcssa, %expr_false7 ]
  %isect_107051.sroa.36.1 = phi float [ %isect_107051.sroa.36.0, %expr_true ], [ %isect_107051.sroa.36.2.lcssa, %expr_false7 ]
  %lower2 = phi i32 [ %40, %expr_false7 ], [ 0, %expr_true ]
  %29 = icmp slt i32 %lower2, %_106964
  br i1 %29, label %expr_true4, label %expr_false3

expr_false3:                                      ; preds = %unroll_step1
  %isect_107051.sroa.9.1.lcssa = phi float [ %isect_107051.sroa.9.1, %unroll_step1 ]
  %isect_107051.sroa.14.1.lcssa = phi float [ %isect_107051.sroa.14.1, %unroll_step1 ]
  %isect_107051.sroa.19.1.lcssa = phi float [ %isect_107051.sroa.19.1, %unroll_step1 ]
  %isect_107051.sroa.24.1.lcssa = phi float [ %isect_107051.sroa.24.1, %unroll_step1 ]
  %rngstate_107438.sroa.27.1.lcssa = phi i32 [ %rngstate_107438.sroa.27.1, %unroll_step1 ]
  %rngstate_107438.sroa.18.1.lcssa = phi i32 [ %rngstate_107438.sroa.18.1, %unroll_step1 ]
  %rngstate_107438.sroa.9.1.lcssa = phi i32 [ %rngstate_107438.sroa.9.1, %unroll_step1 ]
  %rngstate_107438.sroa.0.1.lcssa = phi i32 [ %rngstate_107438.sroa.0.1, %unroll_step1 ]
  %isect_107051.sroa.30.1.lcssa = phi float [ %isect_107051.sroa.30.1, %unroll_step1 ]
  %isect_107051.sroa.36.1.lcssa = phi float [ %isect_107051.sroa.36.1, %unroll_step1 ]
  %30 = add nsw i32 2, %lower
  br label %unroll_step

expr_true4:                                       ; preds = %unroll_step1
  %31 = add nsw i32 %lower2, %5
  %32 = add nsw i32 %27, %31
  %33 = sitofp i32 %31 to float
  %offset = mul nsw i32 3, %32
  %34 = add nsw i32 2, %offset
  %35 = add nsw i32 1, %offset
  %36 = getelementptr inbounds [0 x float], [0 x float]* %_106965, i64 0, i32 %offset
  %37 = getelementptr inbounds [0 x float], [0 x float]* %_106965, i64 0, i32 %34
  %38 = getelementptr inbounds [0 x float], [0 x float]* %_106965, i64 0, i32 %35
  br label %unroll_step5

unroll_step5:                                     ; preds = %expr_false11, %expr_true4
  %isect_107051.sroa.9.2 = phi float [ %isect_107051.sroa.9.1, %expr_true4 ], [ %isect_107051.sroa.9.3.lcssa, %expr_false11 ]
  %isect_107051.sroa.14.2 = phi float [ %isect_107051.sroa.14.1, %expr_true4 ], [ %isect_107051.sroa.14.3.lcssa, %expr_false11 ]
  %isect_107051.sroa.19.2 = phi float [ %isect_107051.sroa.19.1, %expr_true4 ], [ %isect_107051.sroa.19.3.lcssa, %expr_false11 ]
  %isect_107051.sroa.24.2 = phi float [ %isect_107051.sroa.24.1, %expr_true4 ], [ %isect_107051.sroa.24.3.lcssa, %expr_false11 ]
  %rngstate_107438.sroa.27.2 = phi i32 [ %rngstate_107438.sroa.27.1, %expr_true4 ], [ %rngstate_107438.sroa.27.3.lcssa, %expr_false11 ]
  %rngstate_107438.sroa.18.2 = phi i32 [ %rngstate_107438.sroa.18.1, %expr_true4 ], [ %rngstate_107438.sroa.18.3.lcssa, %expr_false11 ]
  %rngstate_107438.sroa.9.2 = phi i32 [ %rngstate_107438.sroa.9.1, %expr_true4 ], [ %rngstate_107438.sroa.9.3.lcssa, %expr_false11 ]
  %rngstate_107438.sroa.0.2 = phi i32 [ %rngstate_107438.sroa.0.1, %expr_true4 ], [ %rngstate_107438.sroa.0.3.lcssa, %expr_false11 ]
  %isect_107051.sroa.30.2 = phi float [ %isect_107051.sroa.30.1, %expr_true4 ], [ %isect_107051.sroa.30.3.lcssa, %expr_false11 ]
  %isect_107051.sroa.36.2 = phi float [ %isect_107051.sroa.36.1, %expr_true4 ], [ %isect_107051.sroa.36.3.lcssa, %expr_false11 ]
  %lower6 = phi i32 [ %47, %expr_false11 ], [ 0, %expr_true4 ]
  %39 = icmp slt i32 %lower6, %_106962
  br i1 %39, label %expr_true8, label %expr_false7

expr_false7:                                      ; preds = %unroll_step5
  %isect_107051.sroa.9.2.lcssa = phi float [ %isect_107051.sroa.9.2, %unroll_step5 ]
  %isect_107051.sroa.14.2.lcssa = phi float [ %isect_107051.sroa.14.2, %unroll_step5 ]
  %isect_107051.sroa.19.2.lcssa = phi float [ %isect_107051.sroa.19.2, %unroll_step5 ]
  %isect_107051.sroa.24.2.lcssa = phi float [ %isect_107051.sroa.24.2, %unroll_step5 ]
  %rngstate_107438.sroa.27.2.lcssa = phi i32 [ %rngstate_107438.sroa.27.2, %unroll_step5 ]
  %rngstate_107438.sroa.18.2.lcssa = phi i32 [ %rngstate_107438.sroa.18.2, %unroll_step5 ]
  %rngstate_107438.sroa.9.2.lcssa = phi i32 [ %rngstate_107438.sroa.9.2, %unroll_step5 ]
  %rngstate_107438.sroa.0.2.lcssa = phi i32 [ %rngstate_107438.sroa.0.2, %unroll_step5 ]
  %isect_107051.sroa.30.2.lcssa = phi float [ %isect_107051.sroa.30.2, %unroll_step5 ]
  %isect_107051.sroa.36.2.lcssa = phi float [ %isect_107051.sroa.36.2, %unroll_step5 ]
  %40 = add nsw i32 4, %lower2
  br label %unroll_step1

expr_true8:                                       ; preds = %unroll_step5
  %41 = sitofp i32 %lower6 to float
  %du = fmul float %41, %invSamples
  %42 = fadd float %33, %du
  %43 = fsub float %42, %4
  %px = fdiv float %43, %4
  %44 = fmul float %px, %8
  %45 = fmul float %44, %44
  br label %unroll_step9

unroll_step9:                                     ; preds = %_cont76, %expr_true8
  %isect_107051.sroa.9.3 = phi float [ %isect_107051.sroa.9.2, %expr_true8 ], [ %isect_107051.sroa.9.7, %_cont76 ]
  %isect_107051.sroa.14.3 = phi float [ %isect_107051.sroa.14.2, %expr_true8 ], [ %isect_107051.sroa.14.7, %_cont76 ]
  %isect_107051.sroa.19.3 = phi float [ %isect_107051.sroa.19.2, %expr_true8 ], [ %isect_107051.sroa.19.7, %_cont76 ]
  %isect_107051.sroa.24.3 = phi float [ %isect_107051.sroa.24.2, %expr_true8 ], [ %isect_107051.sroa.24.7, %_cont76 ]
  %rngstate_107438.sroa.27.3 = phi i32 [ %rngstate_107438.sroa.27.2, %expr_true8 ], [ %rngstate_107438.sroa.27.5, %_cont76 ]
  %rngstate_107438.sroa.18.3 = phi i32 [ %rngstate_107438.sroa.18.2, %expr_true8 ], [ %rngstate_107438.sroa.18.5, %_cont76 ]
  %rngstate_107438.sroa.9.3 = phi i32 [ %rngstate_107438.sroa.9.2, %expr_true8 ], [ %rngstate_107438.sroa.9.5, %_cont76 ]
  %rngstate_107438.sroa.0.3 = phi i32 [ %rngstate_107438.sroa.0.2, %expr_true8 ], [ %rngstate_107438.sroa.0.5, %_cont76 ]
  %isect_107051.sroa.30.3 = phi float [ %isect_107051.sroa.30.2, %expr_true8 ], [ %isect_107051.sroa.30.7, %_cont76 ]
  %isect_107051.sroa.36.3 = phi float [ %isect_107051.sroa.36.2, %expr_true8 ], [ %isect_107051.sroa.36.7, %_cont76 ]
  %lower10 = phi i32 [ %261, %_cont76 ], [ 0, %expr_true8 ]
  %46 = icmp slt i32 %lower10, %_106962
  br i1 %46, label %expr_true12, label %expr_false11

expr_false11:                                     ; preds = %unroll_step9
  %isect_107051.sroa.9.3.lcssa = phi float [ %isect_107051.sroa.9.3, %unroll_step9 ]
  %isect_107051.sroa.14.3.lcssa = phi float [ %isect_107051.sroa.14.3, %unroll_step9 ]
  %isect_107051.sroa.19.3.lcssa = phi float [ %isect_107051.sroa.19.3, %unroll_step9 ]
  %isect_107051.sroa.24.3.lcssa = phi float [ %isect_107051.sroa.24.3, %unroll_step9 ]
  %rngstate_107438.sroa.27.3.lcssa = phi i32 [ %rngstate_107438.sroa.27.3, %unroll_step9 ]
  %rngstate_107438.sroa.18.3.lcssa = phi i32 [ %rngstate_107438.sroa.18.3, %unroll_step9 ]
  %rngstate_107438.sroa.9.3.lcssa = phi i32 [ %rngstate_107438.sroa.9.3, %unroll_step9 ]
  %rngstate_107438.sroa.0.3.lcssa = phi i32 [ %rngstate_107438.sroa.0.3, %unroll_step9 ]
  %isect_107051.sroa.30.3.lcssa = phi float [ %isect_107051.sroa.30.3, %unroll_step9 ]
  %isect_107051.sroa.36.3.lcssa = phi float [ %isect_107051.sroa.36.3, %unroll_step9 ]
  %47 = add nsw i32 1, %lower6
  br label %unroll_step5

expr_true12:                                      ; preds = %unroll_step9
  %48 = sitofp i32 %lower10 to float
  %dv = fmul float %48, %invSamples
  %49 = fadd float %28, %dv
  %50 = fsub float %49, %3
  %51 = fsub float -0.000000e+00, %50
  %py = fdiv float %51, %3
  %52 = fmul float %py, %py
  %53 = fadd float %45, %52
  %54 = fadd float 1.000000e+00, %53
  %55 = call float @llvm.sqrt.f32(float %54)
  %56 = fdiv float 1.000000e+00, %55
  %57 = fmul float %44, %56
  %58 = fmul float %py, %56
  %59 = fmul float -1.000000e+00, %56
  %.fca.1.0.insert320 = insertvalue %0 { %1 zeroinitializer, %1 undef }, float %57, 1, 0
  %.fca.1.1.insert321 = insertvalue %0 %.fca.1.0.insert320, float %58, 1, 1
  %.fca.1.2.insert322 = insertvalue %0 %.fca.1.1.insert321, float %59, 1, 2
  %60 = extractvalue %0 %.fca.1.2.insert322, 1
  %61 = extractvalue %0 %.fca.1.2.insert322, 0
  %62 = extractvalue %1 %60, 2
  %63 = extractvalue %1 %60, 0
  %64 = extractvalue %1 %60, 1
  %65 = extractvalue %1 %61, 2
  %66 = extractvalue %1 %61, 0
  %67 = extractvalue %1 %61, 1
  %68 = fsub float %65, -3.500000e+00
  %69 = fsub float %66, -2.000000e+00
  %70 = fmul float %68, %62
  %71 = fmul float %68, %68
  %72 = fmul float %69, %63
  %73 = fmul float %69, %69
  %74 = fmul float %67, %64
  %75 = fmul float %67, %67
  %76 = fadd float %72, %74
  %77 = fadd float %73, %75
  %78 = fadd float %76, %70
  %79 = fadd float %77, %71
  %80 = fmul float %78, %78
  %C = fsub float %79, 2.500000e-01
  %D = fsub float %80, %C
  %81 = fcmp olt float 0.000000e+00, %D
  br i1 %81, label %expr_true14, label %_cont

expr_true14:                                      ; preds = %expr_true12
  %82 = call float @llvm.sqrt.f32(float %D)
  %83 = fsub float -0.000000e+00, %78
  %t = fsub float %83, %82
  %84 = fcmp olt float 0.000000e+00, %t
  %85 = fcmp olt float %t, 0x4376345780000000
  %or.cond = and i1 %84, %85
  br i1 %or.cond, label %expr_true18, label %_cont

expr_true18:                                      ; preds = %expr_true14
  %86 = fmul float %62, %t
  %87 = fmul float %63, %t
  %88 = fmul float %64, %t
  %89 = fadd float %66, %87
  %90 = fadd float %65, %86
  %91 = fadd float %67, %88
  %92 = fsub float %90, -3.500000e+00
  %93 = fsub float %89, -2.000000e+00
  %94 = fmul float %92, %92
  %95 = fmul float %93, %93
  %96 = fmul float %91, %91
  %97 = fadd float %95, %96
  %98 = fadd float %97, %94
  %99 = call float @llvm.sqrt.f32(float %98)
  %100 = fdiv float 1.000000e+00, %99
  %101 = fmul float %93, %100
  %102 = fmul float %91, %100
  %103 = fmul float %92, %100
  br label %_cont

_cont:                                            ; preds = %expr_true14, %expr_true12, %expr_true18
  %isect_107051.sroa.0.0 = phi float [ %t, %expr_true18 ], [ 0x4376345780000000, %expr_true14 ], [ 0x4376345780000000, %expr_true12 ]
  %isect_107051.sroa.9.4 = phi float [ %89, %expr_true18 ], [ %isect_107051.sroa.9.3, %expr_true14 ], [ %isect_107051.sroa.9.3, %expr_true12 ]
  %isect_107051.sroa.14.4 = phi float [ %91, %expr_true18 ], [ %isect_107051.sroa.14.3, %expr_true14 ], [ %isect_107051.sroa.14.3, %expr_true12 ]
  %isect_107051.sroa.19.4 = phi float [ %90, %expr_true18 ], [ %isect_107051.sroa.19.3, %expr_true14 ], [ %isect_107051.sroa.19.3, %expr_true12 ]
  %isect_107051.sroa.24.4 = phi float [ %101, %expr_true18 ], [ %isect_107051.sroa.24.3, %expr_true14 ], [ %isect_107051.sroa.24.3, %expr_true12 ]
  %isect_107051.sroa.30.4 = phi float [ %102, %expr_true18 ], [ %isect_107051.sroa.30.3, %expr_true14 ], [ %isect_107051.sroa.30.3, %expr_true12 ]
  %isect_107051.sroa.36.4 = phi float [ %103, %expr_true18 ], [ %isect_107051.sroa.36.3, %expr_true14 ], [ %isect_107051.sroa.36.3, %expr_true12 ]
  %isect_107051.sroa.42.0 = phi i32 [ 1, %expr_true18 ], [ 0, %expr_true14 ], [ 0, %expr_true12 ]
  %104 = fsub float %65, -3.000000e+00
  %105 = fsub float %66, -5.000000e-01
  %106 = fmul float %104, %62
  %107 = fmul float %105, %63
  %108 = fmul float %104, %104
  %109 = fmul float %105, %105
  %110 = fadd float %107, %74
  %111 = fadd float %109, %75
  %112 = fadd float %110, %106
  %113 = fadd float %111, %108
  %114 = fmul float %112, %112
  %C149 = fsub float %113, 2.500000e-01
  %D150 = fsub float %114, %C149
  %115 = fcmp olt float 0.000000e+00, %D150
  br i1 %115, label %expr_true22, label %_cont31

expr_true22:                                      ; preds = %_cont
  %116 = call float @llvm.sqrt.f32(float %D150)
  %117 = fsub float -0.000000e+00, %112
  %t151 = fsub float %117, %116
  %118 = fcmp olt float 0.000000e+00, %t151
  %119 = fcmp olt float %t151, %isect_107051.sroa.0.0
  %or.cond175 = and i1 %118, %119
  br i1 %or.cond175, label %expr_true28, label %_cont31

expr_true28:                                      ; preds = %expr_true22
  %120 = fmul float %62, %t151
  %121 = fmul float %63, %t151
  %122 = fadd float %66, %121
  %123 = fadd float %65, %120
  %124 = fmul float %64, %t151
  %125 = fadd float %67, %124
  %126 = fsub float %123, -3.000000e+00
  %127 = fsub float %122, -5.000000e-01
  %128 = fmul float %126, %126
  %129 = fmul float %127, %127
  %130 = fmul float %125, %125
  %131 = fadd float %129, %130
  %132 = fadd float %131, %128
  %133 = call float @llvm.sqrt.f32(float %132)
  %134 = fdiv float 1.000000e+00, %133
  %135 = fmul float %127, %134
  %136 = fmul float %125, %134
  %137 = fmul float %126, %134
  br label %_cont31

_cont31:                                          ; preds = %expr_true22, %_cont, %expr_true28
  %isect_107051.sroa.0.1 = phi float [ %t151, %expr_true28 ], [ %isect_107051.sroa.0.0, %expr_true22 ], [ %isect_107051.sroa.0.0, %_cont ]
  %isect_107051.sroa.9.5 = phi float [ %122, %expr_true28 ], [ %isect_107051.sroa.9.4, %expr_true22 ], [ %isect_107051.sroa.9.4, %_cont ]
  %isect_107051.sroa.14.5 = phi float [ %125, %expr_true28 ], [ %isect_107051.sroa.14.4, %expr_true22 ], [ %isect_107051.sroa.14.4, %_cont ]
  %isect_107051.sroa.19.5 = phi float [ %123, %expr_true28 ], [ %isect_107051.sroa.19.4, %expr_true22 ], [ %isect_107051.sroa.19.4, %_cont ]
  %isect_107051.sroa.24.5 = phi float [ %135, %expr_true28 ], [ %isect_107051.sroa.24.4, %expr_true22 ], [ %isect_107051.sroa.24.4, %_cont ]
  %isect_107051.sroa.30.5 = phi float [ %136, %expr_true28 ], [ %isect_107051.sroa.30.4, %expr_true22 ], [ %isect_107051.sroa.30.4, %_cont ]
  %isect_107051.sroa.36.5 = phi float [ %137, %expr_true28 ], [ %isect_107051.sroa.36.4, %expr_true22 ], [ %isect_107051.sroa.36.4, %_cont ]
  %isect_107051.sroa.42.1 = phi i32 [ 1, %expr_true28 ], [ %isect_107051.sroa.42.0, %expr_true22 ], [ %isect_107051.sroa.42.0, %_cont ]
  %138 = fsub float %65, 0xC0019999A0000000
  %139 = fsub float %66, 1.000000e+00
  %140 = fmul float %138, %62
  %141 = fmul float %138, %138
  %142 = fmul float %139, %63
  %143 = fmul float %139, %139
  %144 = fadd float %142, %74
  %145 = fadd float %143, %75
  %146 = fadd float %144, %140
  %147 = fadd float %145, %141
  %148 = fmul float %146, %146
  %C152 = fsub float %147, 2.500000e-01
  %D153 = fsub float %148, %C152
  %149 = fcmp olt float 0.000000e+00, %D153
  br i1 %149, label %expr_true33, label %_cont42

expr_true33:                                      ; preds = %_cont31
  %150 = call float @llvm.sqrt.f32(float %D153)
  %151 = fsub float -0.000000e+00, %146
  %t154 = fsub float %151, %150
  %152 = fcmp olt float 0.000000e+00, %t154
  %153 = fcmp olt float %t154, %isect_107051.sroa.0.1
  %or.cond177 = and i1 %152, %153
  br i1 %or.cond177, label %expr_true39, label %_cont42

expr_true39:                                      ; preds = %expr_true33
  %154 = fmul float %64, %t154
  %155 = fmul float %63, %t154
  %156 = fadd float %67, %154
  %157 = fadd float %66, %155
  %158 = fmul float %62, %t154
  %159 = fadd float %65, %158
  %160 = fsub float %159, 0xC0019999A0000000
  %161 = fsub float %157, 1.000000e+00
  %162 = fmul float %160, %160
  %163 = fmul float %161, %161
  %164 = fmul float %156, %156
  %165 = fadd float %163, %164
  %166 = fadd float %165, %162
  %167 = call float @llvm.sqrt.f32(float %166)
  %168 = fdiv float 1.000000e+00, %167
  %169 = fmul float %160, %168
  %170 = fmul float %156, %168
  %171 = fmul float %161, %168
  br label %_cont42

_cont42:                                          ; preds = %expr_true33, %_cont31, %expr_true39
  %isect_107051.sroa.0.2 = phi float [ %t154, %expr_true39 ], [ %isect_107051.sroa.0.1, %expr_true33 ], [ %isect_107051.sroa.0.1, %_cont31 ]
  %isect_107051.sroa.9.6 = phi float [ %157, %expr_true39 ], [ %isect_107051.sroa.9.5, %expr_true33 ], [ %isect_107051.sroa.9.5, %_cont31 ]
  %isect_107051.sroa.14.6 = phi float [ %156, %expr_true39 ], [ %isect_107051.sroa.14.5, %expr_true33 ], [ %isect_107051.sroa.14.5, %_cont31 ]
  %isect_107051.sroa.19.6 = phi float [ %159, %expr_true39 ], [ %isect_107051.sroa.19.5, %expr_true33 ], [ %isect_107051.sroa.19.5, %_cont31 ]
  %isect_107051.sroa.24.6 = phi float [ %171, %expr_true39 ], [ %isect_107051.sroa.24.5, %expr_true33 ], [ %isect_107051.sroa.24.5, %_cont31 ]
  %isect_107051.sroa.30.6 = phi float [ %170, %expr_true39 ], [ %isect_107051.sroa.30.5, %expr_true33 ], [ %isect_107051.sroa.30.5, %_cont31 ]
  %isect_107051.sroa.36.6 = phi float [ %169, %expr_true39 ], [ %isect_107051.sroa.36.5, %expr_true33 ], [ %isect_107051.sroa.36.5, %_cont31 ]
  %isect_107051.sroa.42.2 = phi i32 [ 1, %expr_true39 ], [ %isect_107051.sroa.42.1, %expr_true33 ], [ %isect_107051.sroa.42.1, %_cont31 ]
  %172 = fmul float 0.000000e+00, %59
  %173 = fmul float 0.000000e+00, %57
  %174 = fadd float %173, %58
  %175 = fadd float %174, %172
  %176 = call float @llvm.fabs.f32(float %175)
  %177 = fcmp ole float 0x3C670EF540000000, %176
  br i1 %177, label %expr_true44, label %ray_plane_intersect_cont

expr_true44:                                      ; preds = %_cont42
  %t155 = fdiv float -5.000000e-01, %175
  %178 = fcmp olt float 0.000000e+00, %t155
  %179 = fcmp olt float %t155, %isect_107051.sroa.0.2
  %or.cond179 = and i1 %178, %179
  br i1 %or.cond179, label %expr_true48, label %ray_plane_intersect_cont

expr_true48:                                      ; preds = %expr_true44
  %180 = fmul float %57, %t155
  %181 = fmul float %58, %t155
  %182 = fmul float %59, %t155
  %183 = fadd float 0.000000e+00, %180
  %184 = fadd float 0.000000e+00, %181
  %185 = fadd float 0.000000e+00, %182
  br label %ray_plane_intersect_cont

ray_plane_intersect_cont:                         ; preds = %expr_true44, %_cont42, %expr_true48
  %isect_107051.sroa.9.7 = phi float [ %183, %expr_true48 ], [ %isect_107051.sroa.9.6, %expr_true44 ], [ %isect_107051.sroa.9.6, %_cont42 ]
  %isect_107051.sroa.14.7 = phi float [ %184, %expr_true48 ], [ %isect_107051.sroa.14.6, %expr_true44 ], [ %isect_107051.sroa.14.6, %_cont42 ]
  %isect_107051.sroa.19.7 = phi float [ %185, %expr_true48 ], [ %isect_107051.sroa.19.6, %expr_true44 ], [ %isect_107051.sroa.19.6, %_cont42 ]
  %isect_107051.sroa.24.7 = phi float [ 0.000000e+00, %expr_true48 ], [ %isect_107051.sroa.24.6, %expr_true44 ], [ %isect_107051.sroa.24.6, %_cont42 ]
  %isect_107051.sroa.30.7 = phi float [ 1.000000e+00, %expr_true48 ], [ %isect_107051.sroa.30.6, %expr_true44 ], [ %isect_107051.sroa.30.6, %_cont42 ]
  %isect_107051.sroa.36.7 = phi float [ 0.000000e+00, %expr_true48 ], [ %isect_107051.sroa.36.6, %expr_true44 ], [ %isect_107051.sroa.36.6, %_cont42 ]
  %isect_107051.sroa.42.3 = phi i32 [ 1, %expr_true48 ], [ %isect_107051.sroa.42.2, %expr_true44 ], [ %isect_107051.sroa.42.2, %_cont42 ]
  %186 = icmp ne i32 %isect_107051.sroa.42.3, 0
  %187 = call i1 @rv_any(i1 %186)
  %.not = xor i1 %187, true
  %.not180 = xor i1 %186, true
  %brmerge = or i1 %.not, %.not180
  br i1 %brmerge, label %_cont76, label %expr_true52

expr_true52:                                      ; preds = %ray_plane_intersect_cont
  %188 = fcmp olt float %isect_107051.sroa.24.7, 0x3FE3333340000000
  %189 = fcmp olt float 0xBFE3333340000000, %isect_107051.sroa.24.7
  %or.cond181 = and i1 %188, %189
  br i1 %or.cond181, label %expr_true67, label %if_else

if_else:                                          ; preds = %expr_true52
  %190 = fcmp olt float %isect_107051.sroa.30.7, 0x3FE3333340000000
  %191 = fcmp olt float 0xBFE3333340000000, %isect_107051.sroa.30.7
  %or.cond182 = and i1 %190, %191
  br i1 %or.cond182, label %expr_true65, label %if_else59

if_else59:                                        ; preds = %if_else
  %192 = fcmp olt float %isect_107051.sroa.36.7, 0x3FE3333340000000
  %193 = fcmp olt float 0xBFE3333340000000, %isect_107051.sroa.36.7
  %or.cond183 = and i1 %192, %193
  br i1 %or.cond183, label %expr_true64, label %if_else63

if_else63:                                        ; preds = %if_else59
  br label %if_join68

expr_true64:                                      ; preds = %if_else59
  br label %if_join68

expr_true65:                                      ; preds = %if_else
  br label %if_join68

expr_true67:                                      ; preds = %expr_true52
  br label %if_join68

if_join68:                                        ; preds = %expr_true65, %expr_true64, %if_else63, %expr_true67
  %basis_107367.sroa.0.0 = phi float [ 1.000000e+00, %expr_true67 ], [ 0.000000e+00, %expr_true65 ], [ 0.000000e+00, %expr_true64 ], [ 1.000000e+00, %if_else63 ]
  %basis_107367.sroa.6.0 = phi float [ 0.000000e+00, %expr_true67 ], [ 1.000000e+00, %expr_true65 ], [ 0.000000e+00, %expr_true64 ], [ 0.000000e+00, %if_else63 ]
  %basis_107367.sroa.11.0 = phi float [ 0.000000e+00, %expr_true67 ], [ 0.000000e+00, %expr_true65 ], [ 1.000000e+00, %expr_true64 ], [ 0.000000e+00, %if_else63 ]
  %194 = fmul float %basis_107367.sroa.0.0, %isect_107051.sroa.30.7
  %195 = fmul float %basis_107367.sroa.11.0, %isect_107051.sroa.30.7
  %196 = fmul float %basis_107367.sroa.6.0, %isect_107051.sroa.24.7
  %197 = fmul float %basis_107367.sroa.11.0, %isect_107051.sroa.24.7
  %198 = fmul float %basis_107367.sroa.6.0, %isect_107051.sroa.36.7
  %199 = fmul float %basis_107367.sroa.0.0, %isect_107051.sroa.36.7
  %200 = fsub float %194, %196
  %201 = fsub float %198, %195
  %202 = fsub float %197, %199
  %203 = fmul float %200, %200
  %204 = fmul float %201, %201
  %205 = fmul float %202, %202
  %206 = fadd float %204, %205
  %207 = fadd float %206, %203
  %208 = call float @llvm.sqrt.f32(float %207)
  %209 = fdiv float 1.000000e+00, %208
  %210 = fmul float %201, %209
  %211 = fmul float %202, %209
  %212 = fmul float %200, %209
  %213 = fmul float %isect_107051.sroa.24.7, %211
  %214 = fmul float %isect_107051.sroa.24.7, %212
  %215 = fmul float %isect_107051.sroa.30.7, %210
  %216 = fmul float %isect_107051.sroa.30.7, %212
  %217 = fmul float %isect_107051.sroa.36.7, %211
  %218 = fmul float %isect_107051.sroa.36.7, %210
  %219 = fsub float %213, %215
  %220 = fsub float %218, %214
  %221 = fsub float %216, %217
  %222 = fmul float %219, %219
  %223 = fmul float %220, %220
  %224 = fmul float %221, %221
  %225 = fadd float %224, %223
  %226 = fadd float %225, %222
  %227 = call float @llvm.sqrt.f32(float %226)
  %228 = fdiv float 1.000000e+00, %227
  %229 = fmul float %221, %228
  %230 = fmul float %220, %228
  %231 = fmul float %219, %228
  %232 = fmul float 0x3F1A36E2E0000000, %isect_107051.sroa.24.7
  %233 = fmul float 0x3F1A36E2E0000000, %isect_107051.sroa.30.7
  %234 = fmul float 0x3F1A36E2E0000000, %isect_107051.sroa.36.7
  %235 = fadd float %isect_107051.sroa.9.7, %232
  %236 = fadd float %isect_107051.sroa.14.7, %233
  %237 = fadd float %isect_107051.sroa.19.7, %234
  %238 = fsub float %235, -2.000000e+00
  %239 = fsub float %237, -3.500000e+00
  %240 = fmul float %238, %238
  %241 = fmul float %236, %236
  %242 = fmul float %239, %239
  %243 = fadd float %240, %241
  %244 = fadd float %243, %242
  %C156 = fsub float %244, 2.500000e-01
  %.fca.0.0.insert = insertvalue %0 undef, float %235, 0, 0
  %.fca.0.1.insert = insertvalue %0 %.fca.0.0.insert, float %236, 0, 1
  %.fca.0.2.insert = insertvalue %0 %.fca.0.1.insert, float %237, 0, 2
  %245 = fmul float 0.000000e+00, %237
  %246 = fmul float 0.000000e+00, %235
  %247 = fadd float %246, %236
  %248 = fadd float %247, %245
  %249 = fadd float 5.000000e-01, %248
  %250 = fsub float -0.000000e+00, %249
  br label %unroll_step73

unroll_step73:                                    ; preds = %expr_false80, %if_join68
  %occlusion_107349.0 = phi float [ 0.000000e+00, %if_join68 ], [ %occlusion_107349.1.lcssa, %expr_false80 ]
  %rngstate_107438.sroa.27.4 = phi i32 [ %rngstate_107438.sroa.27.3, %if_join68 ], [ %rngstate_107438.sroa.27.6.lcssa, %expr_false80 ]
  %rngstate_107438.sroa.18.4 = phi i32 [ %rngstate_107438.sroa.18.3, %if_join68 ], [ %rngstate_107438.sroa.18.6.lcssa, %expr_false80 ]
  %rngstate_107438.sroa.9.4 = phi i32 [ %rngstate_107438.sroa.9.3, %if_join68 ], [ %rngstate_107438.sroa.9.6.lcssa, %expr_false80 ]
  %rngstate_107438.sroa.0.4 = phi i32 [ %rngstate_107438.sroa.0.3, %if_join68 ], [ %rngstate_107438.sroa.0.6.lcssa, %expr_false80 ]
  %lower74 = phi i32 [ %263, %expr_false80 ], [ 0, %if_join68 ]
  %251 = icmp slt i32 %lower74, 8
  br i1 %251, label %unroll_step78.preheader, label %expr_false75

unroll_step78.preheader:                          ; preds = %unroll_step73
  br label %unroll_step78

expr_false75:                                     ; preds = %unroll_step73
  %occlusion_107349.0.lcssa = phi float [ %occlusion_107349.0, %unroll_step73 ]
  %rngstate_107438.sroa.27.4.lcssa = phi i32 [ %rngstate_107438.sroa.27.4, %unroll_step73 ]
  %rngstate_107438.sroa.18.4.lcssa = phi i32 [ %rngstate_107438.sroa.18.4, %unroll_step73 ]
  %rngstate_107438.sroa.9.4.lcssa = phi i32 [ %rngstate_107438.sroa.9.4, %unroll_step73 ]
  %rngstate_107438.sroa.0.4.lcssa = phi i32 [ %rngstate_107438.sroa.0.4, %unroll_step73 ]
  %252 = fsub float 6.400000e+01, %occlusion_107349.0.lcssa
  %253 = fdiv float %252, 6.400000e+01
  %254 = fmul float %253, %20
  %255 = load float, float* %36
  %256 = fadd float %255, %254
  store float %256, float* %36
  %257 = load float, float* %38
  %258 = fadd float %257, %254
  store float %258, float* %38
  %259 = load float, float* %37
  %260 = fadd float %259, %254
  store float %260, float* %37
  br label %_cont76

_cont76:                                          ; preds = %ray_plane_intersect_cont, %expr_false75
  %rngstate_107438.sroa.27.5 = phi i32 [ %rngstate_107438.sroa.27.3, %ray_plane_intersect_cont ], [ %rngstate_107438.sroa.27.4.lcssa, %expr_false75 ]
  %rngstate_107438.sroa.18.5 = phi i32 [ %rngstate_107438.sroa.18.3, %ray_plane_intersect_cont ], [ %rngstate_107438.sroa.18.4.lcssa, %expr_false75 ]
  %rngstate_107438.sroa.9.5 = phi i32 [ %rngstate_107438.sroa.9.3, %ray_plane_intersect_cont ], [ %rngstate_107438.sroa.9.4.lcssa, %expr_false75 ]
  %rngstate_107438.sroa.0.5 = phi i32 [ %rngstate_107438.sroa.0.3, %ray_plane_intersect_cont ], [ %rngstate_107438.sroa.0.4.lcssa, %expr_false75 ]
  %261 = add nsw i32 1, %lower10
  br label %unroll_step9

unroll_step78:                                    ; preds = %unroll_step78.preheader, %_cont148
  %occlusion_107349.1 = phi float [ %occlusion_107349.2, %_cont148 ], [ %occlusion_107349.0, %unroll_step78.preheader ]
  %rngstate_107438.sroa.27.6 = phi i32 [ %319, %_cont148 ], [ %rngstate_107438.sroa.27.4, %unroll_step78.preheader ]
  %rngstate_107438.sroa.18.6 = phi i32 [ %312, %_cont148 ], [ %rngstate_107438.sroa.18.4, %unroll_step78.preheader ]
  %rngstate_107438.sroa.9.6 = phi i32 [ %305, %_cont148 ], [ %rngstate_107438.sroa.9.4, %unroll_step78.preheader ]
  %rngstate_107438.sroa.0.6 = phi i32 [ %299, %_cont148 ], [ %rngstate_107438.sroa.0.4, %unroll_step78.preheader ]
  %lower79 = phi i32 [ %440, %_cont148 ], [ 0, %unroll_step78.preheader ]
  %262 = icmp slt i32 %lower79, 8
  br i1 %262, label %expr_true81, label %expr_false80

expr_false80:                                     ; preds = %unroll_step78
  %occlusion_107349.1.lcssa = phi float [ %occlusion_107349.1, %unroll_step78 ]
  %rngstate_107438.sroa.27.6.lcssa = phi i32 [ %rngstate_107438.sroa.27.6, %unroll_step78 ]
  %rngstate_107438.sroa.18.6.lcssa = phi i32 [ %rngstate_107438.sroa.18.6, %unroll_step78 ]
  %rngstate_107438.sroa.9.6.lcssa = phi i32 [ %rngstate_107438.sroa.9.6, %unroll_step78 ]
  %rngstate_107438.sroa.0.6.lcssa = phi i32 [ %rngstate_107438.sroa.0.6, %unroll_step78 ]
  %263 = add nsw i32 1, %lower74
  br label %unroll_step73

expr_true81:                                      ; preds = %unroll_step78
  %264 = shl i32 %rngstate_107438.sroa.0.6, 6
  %265 = xor i32 %264, %rngstate_107438.sroa.0.6
  %b = lshr i32 %265, 13
  %266 = and i32 -2, %rngstate_107438.sroa.0.6
  %267 = shl i32 %266, 18
  %268 = xor i32 %267, %b
  %269 = shl i32 %rngstate_107438.sroa.9.6, 2
  %270 = xor i32 %269, %rngstate_107438.sroa.9.6
  %271 = lshr i32 %270, 27
  %272 = and i32 -8, %rngstate_107438.sroa.9.6
  %273 = shl i32 %272, 2
  %274 = xor i32 %273, %271
  %275 = xor i32 %268, %274
  %276 = shl i32 %rngstate_107438.sroa.18.6, 13
  %277 = xor i32 %276, %rngstate_107438.sroa.18.6
  %278 = lshr i32 %277, 21
  %279 = and i32 -16, %rngstate_107438.sroa.18.6
  %280 = shl i32 %279, 7
  %281 = xor i32 %280, %278
  %282 = xor i32 %275, %281
  %283 = shl i32 %rngstate_107438.sroa.27.6, 3
  %284 = xor i32 %283, %rngstate_107438.sroa.27.6
  %285 = lshr i32 %284, 12
  %286 = and i32 -128, %rngstate_107438.sroa.27.6
  %287 = shl i32 %286, 13
  %288 = xor i32 %287, %285
  %289 = xor i32 %282, %288
  %290 = and i32 8388607, %289
  %291 = or i32 1065353216, %290
  %292 = bitcast i32 %291 to float
  %293 = fsub float %292, 1.000000e+00
  %294 = call float @llvm.sqrt.f32(float %293)
  %295 = shl i32 %268, 6
  %296 = xor i32 %295, %268
  %b157 = lshr i32 %296, 13
  %297 = and i32 -2, %268
  %298 = shl i32 %297, 18
  %299 = xor i32 %298, %b157
  %300 = shl i32 %274, 2
  %301 = xor i32 %300, %274
  %302 = lshr i32 %301, 27
  %303 = and i32 -8, %274
  %304 = shl i32 %303, 2
  %305 = xor i32 %304, %302
  %306 = xor i32 %299, %305
  %307 = shl i32 %281, 13
  %308 = xor i32 %307, %281
  %309 = lshr i32 %308, 21
  %310 = and i32 -16, %281
  %311 = shl i32 %310, 7
  %312 = xor i32 %311, %309
  %313 = xor i32 %306, %312
  %314 = shl i32 %288, 3
  %315 = xor i32 %314, %288
  %316 = lshr i32 %315, 12
  %317 = and i32 -128, %288
  %318 = shl i32 %317, 13
  %319 = xor i32 %318, %316
  %320 = xor i32 %313, %319
  %321 = and i32 8388607, %320
  %322 = or i32 1065353216, %321
  %323 = bitcast i32 %322 to float
  %324 = fsub float %323, 1.000000e+00
  %phi = fmul float 0x401921FB60000000, %324
  %scaled = fmul float 0x3FE45F3060000000, %phi
  %325 = call float @llvm.floor.f32(float %scaled)
  %326 = fmul float 0x3FF921FB60000000, %325
  %x = fsub float %phi, %326
  %327 = fptosi float %325 to i32
  %k_mod4 = and i32 3, %327
  %328 = icmp eq i32 %k_mod4, 1
  %329 = icmp eq i32 %k_mod4, 3
  %sin_usecos = or i1 %328, %329
  %outside = select i1 %sin_usecos, float 1.000000e+00, float %x
  switch i32 %k_mod4, label %if_join90 [
    i32 3, label %expr_true89
    i32 1, label %expr_true89
  ]

expr_true89:                                      ; preds = %expr_true81, %expr_true81
  br label %if_join90

if_join90:                                        ; preds = %expr_true81, %expr_true89
  %c2 = phi float [ -5.000000e-01, %expr_true89 ], [ 0xBFC5555560000000, %expr_true81 ]
  switch i32 %k_mod4, label %if_join93 [
    i32 3, label %expr_true92
    i32 1, label %expr_true92
  ]

expr_true92:                                      ; preds = %if_join90, %if_join90
  br label %if_join93

if_join93:                                        ; preds = %if_join90, %expr_true92
  %c4 = phi float [ 0x3FA5555480000000, %expr_true92 ], [ 0x3F81111300000000, %if_join90 ]
  switch i32 %k_mod4, label %if_join96 [
    i32 3, label %expr_true95
    i32 1, label %expr_true95
  ]

expr_true95:                                      ; preds = %if_join93, %if_join93
  br label %if_join96

if_join96:                                        ; preds = %if_join93, %expr_true95
  %c6 = phi float [ 0xBF56C13020000000, %expr_true95 ], [ 0xBF2A0212C0000000, %if_join93 ]
  switch i32 %k_mod4, label %if_join99 [
    i32 3, label %expr_true98
    i32 1, label %expr_true98
  ]

expr_true98:                                      ; preds = %if_join96, %if_join96
  br label %if_join99

if_join99:                                        ; preds = %if_join96, %expr_true98
  %c8 = phi float [ 0x3EF9F57380000000, %expr_true98 ], [ 0x3EC7271500000000, %if_join96 ]
  switch i32 %k_mod4, label %if_join102 [
    i32 3, label %expr_true101
    i32 1, label %expr_true101
  ]

expr_true101:                                     ; preds = %if_join99, %if_join99
  br label %if_join102

if_join102:                                       ; preds = %if_join99, %expr_true101
  %c10 = phi float [ 0xBE916C69A0000000, %expr_true101 ], [ 0xBE5AE00260000000, %if_join99 ]
  %330 = fmul float %294, %294
  %331 = fsub float 1.000000e+00, %330
  %332 = call float @llvm.sqrt.f32(float %331)
  %x2 = fmul float %x, %x
  %flip_sign = icmp slt i32 1, %k_mod4
  %333 = fmul float %x2, %c10
  %334 = icmp eq i32 %k_mod4, 2
  %335 = icmp eq i32 %k_mod4, 0
  %formula = fadd float %333, %c8
  %flip_sign161 = or i1 %328, %334
  %cos_usecos = or i1 %335, %334
  %336 = fmul float %x2, %formula
  %337 = select i1 %cos_usecos, float 0xBE916C69A0000000, float 0xBE5AE00260000000
  %338 = select i1 %cos_usecos, float -5.000000e-01, float 0xBFC5555560000000
  %339 = select i1 %cos_usecos, float 0x3EF9F57380000000, float 0x3EC7271500000000
  %340 = select i1 %cos_usecos, float 0x3FA5555480000000, float 0x3F81111300000000
  %341 = select i1 %cos_usecos, float 0xBF56C13020000000, float 0xBF2A0212C0000000
  %342 = select i1 %cos_usecos, float 1.000000e+00, float %x
  %343 = fadd float %336, %c6
  %344 = fmul float %x2, %337
  %formula162 = fadd float %344, %339
  %345 = fmul float %x2, %343
  %346 = fmul float %332, %isect_107051.sroa.24.7
  %347 = fmul float %x2, %formula162
  %348 = fadd float %345, %c4
  %349 = fadd float %347, %341
  %350 = fmul float %x2, %348
  %351 = fmul float %x2, %349
  %352 = fadd float %350, %c2
  %353 = fadd float %351, %340
  %354 = fmul float %x2, %352
  %355 = fmul float %x2, %353
  %356 = fadd float 1.000000e+00, %354
  %357 = fadd float %355, %338
  %358 = fmul float %356, %outside
  %359 = fmul float %332, %isect_107051.sroa.30.7
  %360 = fmul float %x2, %357
  %361 = fsub float -0.000000e+00, %358
  %362 = fadd float 1.000000e+00, %360
  %363 = select i1 %flip_sign, float %361, float %358
  %364 = fmul float %362, %342
  %y = fmul float %363, %294
  %365 = fsub float -0.000000e+00, %364
  %366 = fmul float %y, %231
  %367 = fmul float %y, %230
  %368 = fmul float %y, %229
  %369 = select i1 %flip_sign161, float %365, float %364
  %x163 = fmul float %369, %294
  %370 = fmul float %332, %isect_107051.sroa.36.7
  %371 = fmul float %x163, %212
  %372 = fmul float %x163, %211
  %373 = fmul float %x163, %210
  %374 = fadd float %371, %366
  %375 = fadd float %372, %367
  %376 = fadd float %373, %368
  %rz = fadd float %374, %370
  %ry = fadd float %375, %359
  %rx = fadd float %376, %346
  %377 = fmul float %239, %rz
  %378 = fmul float %236, %ry
  %379 = fmul float %238, %rx
  %380 = fadd float %379, %378
  %381 = fadd float %380, %377
  %382 = fmul float %381, %381
  %D164 = fsub float %382, %C156
  %383 = fcmp olt float 0.000000e+00, %D164
  br i1 %383, label %expr_true105, label %_cont114

expr_true105:                                     ; preds = %if_join102
  %384 = call float @llvm.sqrt.f32(float %D164)
  %385 = fsub float -0.000000e+00, %381
  %t165 = fsub float %385, %384
  %386 = fcmp olt float 0.000000e+00, %t165
  %387 = fcmp olt float %t165, 0x4376345780000000
  %or.cond185 = and i1 %386, %387
  br i1 %or.cond185, label %expr_true111, label %_cont114

expr_true111:                                     ; preds = %expr_true105
  br label %_cont114

_cont114:                                         ; preds = %expr_true105, %if_join102, %expr_true111
  %occIsect_107804.sroa.0.0 = phi float [ %t165, %expr_true111 ], [ 0x4376345780000000, %expr_true105 ], [ 0x4376345780000000, %if_join102 ]
  %occIsect_107804.sroa.33.0 = phi i32 [ 1, %expr_true111 ], [ 0, %expr_true105 ], [ 0, %if_join102 ]
  %.fca.1.0.insert = insertvalue %0 %.fca.0.2.insert, float %rx, 1, 0
  %.fca.1.1.insert = insertvalue %0 %.fca.1.0.insert, float %ry, 1, 1
  %.fca.1.2.insert = insertvalue %0 %.fca.1.1.insert, float %rz, 1, 2
  %388 = extractvalue %0 %.fca.1.2.insert, 0
  %389 = extractvalue %0 %.fca.1.2.insert, 1
  %390 = extractvalue %1 %388, 0
  %391 = extractvalue %1 %388, 1
  %392 = extractvalue %1 %388, 2
  %393 = extractvalue %1 %389, 2
  %394 = extractvalue %1 %389, 0
  %395 = extractvalue %1 %389, 1
  %396 = fsub float %390, -5.000000e-01
  %397 = fsub float %392, -3.000000e+00
  %398 = fmul float %397, %393
  %399 = fmul float %396, %394
  %400 = fmul float %391, %395
  %401 = fmul float %396, %396
  %402 = fmul float %391, %391
  %403 = fmul float %397, %397
  %404 = fadd float %399, %400
  %405 = fadd float %401, %402
  %406 = fadd float %405, %403
  %407 = fadd float %404, %398
  %C166 = fsub float %406, 2.500000e-01
  %408 = fmul float %407, %407
  %D167 = fsub float %408, %C166
  %409 = fcmp olt float 0.000000e+00, %D167
  br i1 %409, label %expr_true116, label %_cont125

expr_true116:                                     ; preds = %_cont114
  %410 = call float @llvm.sqrt.f32(float %D167)
  %411 = fsub float -0.000000e+00, %407
  %t168 = fsub float %411, %410
  %412 = fcmp olt float 0.000000e+00, %t168
  %413 = fcmp olt float %t168, %occIsect_107804.sroa.0.0
  %or.cond187 = and i1 %412, %413
  br i1 %or.cond187, label %expr_true122, label %_cont125

expr_true122:                                     ; preds = %expr_true116
  br label %_cont125

_cont125:                                         ; preds = %expr_true116, %_cont114, %expr_true122
  %occIsect_107804.sroa.0.1 = phi float [ %t168, %expr_true122 ], [ %occIsect_107804.sroa.0.0, %expr_true116 ], [ %occIsect_107804.sroa.0.0, %_cont114 ]
  %occIsect_107804.sroa.33.1 = phi i32 [ 1, %expr_true122 ], [ %occIsect_107804.sroa.33.0, %expr_true116 ], [ %occIsect_107804.sroa.33.0, %_cont114 ]
  %414 = fsub float %390, 1.000000e+00
  %415 = fsub float %392, 0xC0019999A0000000
  %416 = fmul float %415, %393
  %417 = fmul float %414, %394
  %418 = fmul float %414, %414
  %419 = fmul float %415, %415
  %420 = fadd float %417, %400
  %421 = fadd float %418, %402
  %422 = fadd float %421, %419
  %423 = fadd float %420, %416
  %C169 = fsub float %422, 2.500000e-01
  %424 = fmul float %423, %423
  %D170 = fsub float %424, %C169
  %425 = fcmp olt float 0.000000e+00, %D170
  br i1 %425, label %expr_true127, label %_cont136

expr_true127:                                     ; preds = %_cont125
  %426 = call float @llvm.sqrt.f32(float %D170)
  %427 = fsub float -0.000000e+00, %423
  %t171 = fsub float %427, %426
  %428 = fcmp olt float 0.000000e+00, %t171
  %429 = fcmp olt float %t171, %occIsect_107804.sroa.0.1
  %or.cond189 = and i1 %428, %429
  br i1 %or.cond189, label %expr_true133, label %_cont136

expr_true133:                                     ; preds = %expr_true127
  br label %_cont136

_cont136:                                         ; preds = %expr_true127, %_cont125, %expr_true133
  %occIsect_107804.sroa.0.2 = phi float [ %t171, %expr_true133 ], [ %occIsect_107804.sroa.0.1, %expr_true127 ], [ %occIsect_107804.sroa.0.1, %_cont125 ]
  %occIsect_107804.sroa.33.2 = phi i32 [ 1, %expr_true133 ], [ %occIsect_107804.sroa.33.1, %expr_true127 ], [ %occIsect_107804.sroa.33.1, %_cont125 ]
  %430 = fmul float 0.000000e+00, %rz
  %431 = fmul float 0.000000e+00, %rx
  %432 = fadd float %431, %ry
  %433 = fadd float %432, %430
  %434 = call float @llvm.fabs.f32(float %433)
  %435 = fcmp ole float 0x3C670EF540000000, %434
  br i1 %435, label %expr_true140, label %ray_plane_intersect_cont145

expr_true140:                                     ; preds = %_cont136
  %t172 = fdiv float %250, %433
  %436 = fcmp olt float 0.000000e+00, %t172
  %437 = fcmp olt float %t172, %occIsect_107804.sroa.0.2
  %or.cond191 = and i1 %436, %437
  br i1 %or.cond191, label %expr_true144, label %ray_plane_intersect_cont145

expr_true144:                                     ; preds = %expr_true140
  br label %ray_plane_intersect_cont145

ray_plane_intersect_cont145:                      ; preds = %expr_true140, %_cont136, %expr_true144
  %occIsect_107804.sroa.33.3 = phi i32 [ 1, %expr_true144 ], [ %occIsect_107804.sroa.33.2, %expr_true140 ], [ %occIsect_107804.sroa.33.2, %_cont136 ]
  %438 = icmp ne i32 %occIsect_107804.sroa.33.3, 0
  br i1 %438, label %expr_true147, label %_cont148

expr_true147:                                     ; preds = %ray_plane_intersect_cont145
  %439 = fadd float 1.000000e+00, %occlusion_107349.1
  br label %_cont148

_cont148:                                         ; preds = %ray_plane_intersect_cont145, %expr_true147
  %occlusion_107349.2 = phi float [ %439, %expr_true147 ], [ %occlusion_107349.1, %ray_plane_intersect_cont145 ]
  %440 = add nsw i32 1, %lower79
  br label %unroll_step78
}

; Function Attrs: nounwind readnone speculatable
declare float @llvm.sqrt.f32(float) #0

; Function Attrs: nounwind readnone speculatable
declare float @llvm.fabs.f32(float) #0

declare i1 @rv_any(i1)

; Function Attrs: nounwind readnone speculatable
declare float @llvm.floor.f32(float) #0

attributes #0 = { nounwind readnone speculatable }
