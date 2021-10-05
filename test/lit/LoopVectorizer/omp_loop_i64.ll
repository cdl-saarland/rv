; RUN: opt %s -O3 -S -o /dev/stdout

; CHECK: omp.inner.for.body{{.*}}.rv:

; ModuleID = 'loop.c'
source_filename = "loop.c"
target datalayout = "e-m:e-i64:64-n32:64-S128-v64:64:64-v128:64:64-v256:64:64-v512:64:64-v1024:64:64-v2048:64:64-v4096:64:64-v8192:64:64-v16384:64:64"
target triple = "ve-unknown-linux-gnu"

; Function Attrs: nofree norecurse nounwind
define dso_local void @vector_add(double* nocapture readonly %A, double* nocapture readonly %B, double* nocapture %C, i64 %size) local_unnamed_addr #0 {
entry:
  %cmp = icmp sgt i64 %size, 0
  br i1 %cmp, label %omp.inner.for.body, label %simd.if.end

omp.inner.for.body:                               ; preds = %entry, %omp.inner.for.body
  %.omp.iv.028 = phi i64 [ %add10, %omp.inner.for.body ], [ 0, %entry ]
  %arrayidx = getelementptr inbounds double, double* %A, i64 %.omp.iv.028
  %0 = load double, double* %arrayidx, align 8, !tbaa !2
  %arrayidx7 = getelementptr inbounds double, double* %B, i64 %.omp.iv.028
  %1 = load double, double* %arrayidx7, align 8, !tbaa !2
  %add8 = fadd double %0, %1
  %arrayidx9 = getelementptr inbounds double, double* %C, i64 %.omp.iv.028
  store double %add8, double* %arrayidx9, align 8, !tbaa !2
  %add10 = add nuw nsw i64 %.omp.iv.028, 1
  %exitcond.not = icmp eq i64 %add10, %size
  br i1 %exitcond.not, label %simd.if.end, label %omp.inner.for.body, !llvm.loop !6

simd.if.end:                                      ; preds = %omp.inner.for.body, %entry
  ret void
}

attributes #0 = { nofree norecurse nounwind "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 13.0.0 (gh:sx-aurora-dev/llvm-project.git 3a7117bab79c4b5c1838cfec81bb5683cdc38ad8)"}
!2 = !{!3, !3, i64 0}
!3 = !{!"double", !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C/C++ TBAA"}
!6 = distinct !{!6, !7, !8}
!7 = !{!"llvm.loop.vectorize.width", i32 256}
!8 = !{!"llvm.loop.vectorize.enable", i1 true}
