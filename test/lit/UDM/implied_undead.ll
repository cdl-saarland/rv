; RUN: rvTool -wfv -lower -i %s -k implied_undead -s T_U_U -w 8  | FileCheck %s
; RUN: rvTool -wfv -lower -i %s -k implied_undead_neg -s T_U_U -w 8  | FileCheck %s
; RUN: rvTool -wfv -lower -i %s -k implied_undead_and -s T_T_U_U -w 8  | FileCheck %s

; CHECK-NOT: mem_block:

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

declare dso_local zeroext i1 @rv_any(i1 zeroext) local_unnamed_addr #2

define void @implied_undead(i1 %mask, i32* %uniPtr, i32 %uniVal) {
entry:
  %any = call i1 @rv_any(i1 %mask)
  br i1 %any, label %onAnyTrue, label %exit

onAnyTrue:
  br i1 %mask, label %onTrue, label %exit

onTrue:
  store i32 %uniVal, i32* %uniPtr
  br label %exit

exit:
  ret void
}

define void @implied_undead_neg(i1 %mask, i32* %uniPtr, i32 %uniVal) {
entry:
  %negMask = xor i1 %mask, true
  %any = call i1 @rv_any(i1 %negMask)
  br i1 %any, label %onAnyFalse, label %exit

onAnyFalse:
  br i1 %mask, label %exit, label %onFalse

onFalse:
  store i32 %uniVal, i32* %uniPtr
  br label %exit

exit:
  ret void
}

define void @implied_undead_and(i1 %mask, i1 %distraction, i32* %uniPtr, i32 %uniVal) {
entry:
  %andMask = and i1 %mask, %distraction
  %any = call i1 @rv_any(i1 %andMask)
  br i1 %any, label %onAnyBoth, label %exit

onAnyBoth:
  br i1 %mask, label %onTrue, label %exit

onTrue:
  store i32 %uniVal, i32* %uniPtr
  br label %exit

exit:
  ret void
}

attributes #0 = { uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="haswell" "target-features"="+aes,+avx,+avx2,+bmi,+bmi2,+cmov,+cx16,+f16c,+fma,+fsgsbase,+fxsr,+lzcnt,+mmx,+movbe,+pclmul,+popcnt,+rdrnd,+sahf,+sse,+sse2,+sse3,+sse4.1,+sse4.2,+ssse3,+x87,+xsave,+xsaveopt,-adx,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vpopcntdq,-cldemote,-clflushopt,-clwb,-clzero,-fma4,-gfni,-ibt,-lwp,-movdir64b,-movdiri,-mwaitx,-pconfig,-pku,-prefetchwt1,-prfchw,-ptwrite,-rdpid,-rdseed,-rtm,-sgx,-sha,-shstk,-sse4a,-tbm,-vaes,-vpclmulqdq,-waitpkg,-wbnoinvd,-xop,-xsavec,-xsaves" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { argmemonly nounwind }
attributes #2 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="haswell" "target-features"="+aes,+avx,+avx2,+bmi,+bmi2,+cmov,+cx16,+f16c,+fma,+fsgsbase,+fxsr,+lzcnt,+mmx,+movbe,+pclmul,+popcnt,+rdrnd,+sahf,+sse,+sse2,+sse3,+sse4.1,+sse4.2,+ssse3,+x87,+xsave,+xsaveopt,-adx,-avx512bitalg,-avx512bw,-avx512cd,-avx512dq,-avx512er,-avx512f,-avx512ifma,-avx512pf,-avx512vbmi,-avx512vbmi2,-avx512vl,-avx512vnni,-avx512vpopcntdq,-cldemote,-clflushopt,-clwb,-clzero,-fma4,-gfni,-ibt,-lwp,-movdir64b,-movdiri,-mwaitx,-pconfig,-pku,-prefetchwt1,-prfchw,-ptwrite,-rdpid,-rdseed,-rtm,-sgx,-sha,-shstk,-sse4a,-tbm,-vaes,-vpclmulqdq,-waitpkg,-wbnoinvd,-xop,-xsavec,-xsaves" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 7.0.0 (http://llvm.org/git/clang.git 861f2f20135cc54ce4a3f5b5ad16c835eef4c87f) (llvm/trunk 332580)"}
!2 = !{!3, !3, i64 0}
!3 = !{!"int", !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C++ TBAA"}
!6 = !{!7, !8, i64 0}
!7 = !{!"_ZTS4Node", !8, i64 0, !3, i64 4, !3, i64 8}
!8 = !{!"float", !4, i64 0}
!9 = !{!7, !3, i64 4}
!10 = !{!7, !3, i64 8}
