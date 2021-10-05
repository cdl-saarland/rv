; RUN: opt %s -O3 -S -o /dev/stdout | FileCheck %s

; void stream_triad(double scalar, double *a, double *b, double *c, long n)
; {
;   //int j;
;   long j;
; 
; #pragma omp simd
;   for (j = 0; j < n; j++) {
;     a[j] = b[j] + scalar * c[j];
;   }
; }

; CHECK: omp.inner.for.body{{.*}}.rv:

; ModuleID = 'loop.c'
source_filename = "loop.c"
target datalayout = "e-m:e-i64:64-n32:64-S128-v64:64:64-v128:64:64-v256:64:64-v512:64:64-v1024:64:64-v2048:64:64-v4096:64:64-v8192:64:64-v16384:64:64"
target triple = "ve-unknown-linux-gnu"

; Function Attrs: nofree norecurse nounwind
define dso_local void @stream_triad(double %scalar, double* nocapture %a, double* nocapture readonly %b, double* nocapture readonly %c, i64 %n) local_unnamed_addr #0 {
entry:
  %cmp = icmp sgt i64 %n, 0
  br i1 %cmp, label %omp.inner.for.body, label %simd.if.end

omp.inner.for.body:                               ; preds = %entry, %omp.inner.for.body
  %.omp.iv.029 = phi i64 [ %add11, %omp.inner.for.body ], [ 0, %entry ]
  %arrayidx = getelementptr inbounds double, double* %b, i64 %.omp.iv.029
  %0 = load double, double* %arrayidx, align 8, !tbaa !2, !llvm.access.group !6
  %arrayidx7 = getelementptr inbounds double, double* %c, i64 %.omp.iv.029
  %1 = load double, double* %arrayidx7, align 8, !tbaa !2, !llvm.access.group !6
  %mul8 = fmul double %1, %scalar
  %add9 = fadd double %0, %mul8
  %arrayidx10 = getelementptr inbounds double, double* %a, i64 %.omp.iv.029
  store double %add9, double* %arrayidx10, align 8, !tbaa !2, !llvm.access.group !6
  %add11 = add nuw nsw i64 %.omp.iv.029, 1
  %exitcond.not = icmp eq i64 %add11, %n
  br i1 %exitcond.not, label %simd.if.end, label %omp.inner.for.body, !llvm.loop !7

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
!6 = distinct !{}
!7 = distinct !{!7, !8, !9}
!8 = !{!"llvm.loop.parallel_accesses", !6}
!9 = !{!"llvm.loop.vectorize.enable", i1 true}
