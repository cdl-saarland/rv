define <4 x float> @log2_ps(<4 x float> %x) nounwind readnone {
entry:
  %x6 = bitcast <4 x float> %x to <4 x i32>
  %0 = lshr <4 x i32> %x6, <i32 23, i32 23, i32 23, i32 23>
  %1 = and <4 x i32> %0, <i32 255, i32 255, i32 255, i32 255>
  %2 = add <4 x i32> %1, <i32 -127, i32 -127, i32 -127, i32 -127>
  %3 = sitofp <4 x i32> %2 to <4 x float>
  %4 = or <4 x i32> %x6, <i32 1065353216, i32 1065353216, i32 1065353216, i32 1065353216>
  %5 = and <4 x i32> %4, <i32 1073741823, i32 1073741823, i32 1073741823, i32 1073741823>
  %6 = bitcast <4 x i32> %5 to <4 x float>
  %7 = fmul <4 x float> %6, <float 0x3FAE8AA5E0000000, float 0x3FAE8AA5E0000000, float 0x3FAE8AA5E0000000, float 0x3FAE8AA5E0000000>
  %8 = fsub <4 x float> %7, <float 0x3FDDCE72E0000000, float 0x3FDDCE72E0000000, float 0x3FDDCE72E0000000, float 0x3FDDCE72E0000000>
  %9 = fmul <4 x float> %8, %6
  %10 = fadd <4 x float> %9, <float 0x3FF7B2DBA0000000, float 0x3FF7B2DBA0000000, float 0x3FF7B2DBA0000000, float 0x3FF7B2DBA0000000>
  %11 = fmul <4 x float> %10, %6
  %12 = fsub <4 x float> %11, <float 0x40042A7EC0000000, float 0x40042A7EC0000000, float 0x40042A7EC0000000, float 0x40042A7EC0000000>
  %13 = fmul <4 x float> %12, %6
  %14 = fadd <4 x float> %13, <float 0x40071B2D80000000, float 0x40071B2D80000000, float 0x40071B2D80000000, float 0x40071B2D80000000>
  %15 = fsub <4 x float> %6, <float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00>
  %16 = fmul <4 x float> %15, %14
  %17 = fadd <4 x float> %16, %3
  ret <4 x float> %17
}

define <4 x float> @exp2_ps(<4 x float> %x) nounwind readnone {
entry:
  %0 = fsub <4 x float> %x, <float 5.000000e-01, float 5.000000e-01, float 5.000000e-01, float 5.000000e-01>
  %1 = fptosi <4 x float> %0 to <4 x i32>
  %2 = sitofp <4 x i32> %1 to <4 x float>
  %3 = fsub <4 x float> %x, %2
  %4 = shl <4 x i32> %1, <i32 23, i32 23, i32 23, i32 23>
  %5 = add <4 x i32> %4, <i32 1065353216, i32 1065353216, i32 1065353216, i32 1065353216>
  %6 = bitcast <4 x i32> %5 to <4 x float>
  %7 = fmul <4 x float> %3, <float 0x3F5EC320A0000000, float 0x3F5EC320A0000000, float 0x3F5EC320A0000000, float 0x3F5EC320A0000000>
  %8 = fadd <4 x float> %7, <float 0x3F826900C0000000, float 0x3F826900C0000000, float 0x3F826900C0000000, float 0x3F826900C0000000>
  %9 = fmul <4 x float> %8, %3
  %10 = fadd <4 x float> %9, <float 0x3FAC954460000000, float 0x3FAC954460000000, float 0x3FAC954460000000, float 0x3FAC954460000000>
  %11 = fmul <4 x float> %10, %3
  %12 = fadd <4 x float> %11, <float 0x3FCEBD5A80000000, float 0x3FCEBD5A80000000, float 0x3FCEBD5A80000000, float 0x3FCEBD5A80000000>
  %13 = fmul <4 x float> %12, %3
  %14 = fadd <4 x float> %13, <float 0x3FE62E4F60000000, float 0x3FE62E4F60000000, float 0x3FE62E4F60000000, float 0x3FE62E4F60000000>
  %15 = fmul <4 x float> %14, %3
  %16 = fadd <4 x float> %15, <float 0x3FEFFFFFE0000000, float 0x3FEFFFFFE0000000, float 0x3FEFFFFFE0000000, float 0x3FEFFFFFE0000000>
  %17 = fmul <4 x float> %6, %16
  ret <4 x float> %17
}

define <8 x float> @log2256_ps(<8 x float> %x) nounwind readnone {
entry:
  %x6 = bitcast <8 x float> %x to <8 x i32>
  %0 = lshr <8 x i32> %x6, <i32 23, i32 23, i32 23, i32 23, i32 23, i32 23, i32 23, i32 23>
  %1 = and <8 x i32> %0, <i32 255, i32 255, i32 255, i32 255, i32 255, i32 255, i32 255, i32 255>
  %2 = add <8 x i32> %1, <i32 -127, i32 -127, i32 -127, i32 -127, i32 -127, i32 -127, i32 -127, i32 -127>
  %3 = sitofp <8 x i32> %2 to <8 x float>
  %4 = or <8 x i32> %x6, <i32 1065353216, i32 1065353216, i32 1065353216, i32 1065353216, i32 1065353216, i32 1065353216, i32 1065353216, i32 1065353216>
  %5 = and <8 x i32> %4, <i32 1073741823, i32 1073741823, i32 1073741823, i32 1073741823, i32 1073741823, i32 1073741823, i32 1073741823, i32 1073741823>
  %6 = bitcast <8 x i32> %5 to <8 x float>
  %7 = fmul <8 x float> %6, <float 0x3FAE8AA5E0000000, float 0x3FAE8AA5E0000000, float 0x3FAE8AA5E0000000, float 0x3FAE8AA5E0000000, float 0x3FAE8AA5E0000000, float 0x3FAE8AA5E0000000, float 0x3FAE8AA5E0000000, float 0x3FAE8AA5E0000000>
  %8 = fsub <8 x float> %7, <float 0x3FDDCE72E0000000, float 0x3FDDCE72E0000000, float 0x3FDDCE72E0000000, float 0x3FDDCE72E0000000, float 0x3FDDCE72E0000000, float 0x3FDDCE72E0000000, float 0x3FDDCE72E0000000, float 0x3FDDCE72E0000000>
  %9 = fmul <8 x float> %8, %6
  %10 = fadd <8 x float> %9, <float 0x3FF7B2DBA0000000, float 0x3FF7B2DBA0000000, float 0x3FF7B2DBA0000000, float 0x3FF7B2DBA0000000, float 0x3FF7B2DBA0000000, float 0x3FF7B2DBA0000000, float 0x3FF7B2DBA0000000, float 0x3FF7B2DBA0000000>
  %11 = fmul <8 x float> %10, %6
  %12 = fsub <8 x float> %11, <float 0x40042A7EC0000000, float 0x40042A7EC0000000, float 0x40042A7EC0000000, float 0x40042A7EC0000000, float 0x40042A7EC0000000, float 0x40042A7EC0000000, float 0x40042A7EC0000000, float 0x40042A7EC0000000>
  %13 = fmul <8 x float> %12, %6
  %14 = fadd <8 x float> %13, <float 0x40071B2D80000000, float 0x40071B2D80000000, float 0x40071B2D80000000, float 0x40071B2D80000000, float 0x40071B2D80000000, float 0x40071B2D80000000, float 0x40071B2D80000000, float 0x40071B2D80000000>
  %15 = fsub <8 x float> %6, <float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00, float 1.000000e+00>
  %16 = fmul <8 x float> %15, %14
  %17 = fadd <8 x float> %16, %3
  ret <8 x float> %17
}

define <8 x float> @exp2256_ps(<8 x float> %x) nounwind readnone {
entry:
  %0 = fsub <8 x float> %x, <float 5.000000e-01, float 5.000000e-01, float 5.000000e-01, float 5.000000e-01, float 5.000000e-01, float 5.000000e-01, float 5.000000e-01, float 5.000000e-01>
  %1 = fptosi <8 x float> %0 to <8 x i32>
  %2 = sitofp <8 x i32> %1 to <8 x float>
  %3 = fsub <8 x float> %x, %2
  %4 = shl <8 x i32> %1, <i32 23, i32 23, i32 23, i32 23, i32 23, i32 23, i32 23, i32 23>
  %5 = add <8 x i32> %4, <i32 1065353216, i32 1065353216, i32 1065353216, i32 1065353216, i32 1065353216, i32 1065353216, i32 1065353216, i32 1065353216>
  %6 = bitcast <8 x i32> %5 to <8 x float>
  %7 = fmul <8 x float> %3, <float 0x3F5EC320A0000000, float 0x3F5EC320A0000000, float 0x3F5EC320A0000000, float 0x3F5EC320A0000000, float 0x3F5EC320A0000000, float 0x3F5EC320A0000000, float 0x3F5EC320A0000000, float 0x3F5EC320A0000000>
  %8 = fadd <8 x float> %7, <float 0x3F826900C0000000, float 0x3F826900C0000000, float 0x3F826900C0000000, float 0x3F826900C0000000, float 0x3F826900C0000000, float 0x3F826900C0000000, float 0x3F826900C0000000, float 0x3F826900C0000000>
  %9 = fmul <8 x float> %8, %3
  %10 = fadd <8 x float> %9, <float 0x3FAC954460000000, float 0x3FAC954460000000, float 0x3FAC954460000000, float 0x3FAC954460000000, float 0x3FAC954460000000, float 0x3FAC954460000000, float 0x3FAC954460000000, float 0x3FAC954460000000>
  %11 = fmul <8 x float> %10, %3
  %12 = fadd <8 x float> %11, <float 0x3FCEBD5A80000000, float 0x3FCEBD5A80000000, float 0x3FCEBD5A80000000, float 0x3FCEBD5A80000000, float 0x3FCEBD5A80000000, float 0x3FCEBD5A80000000, float 0x3FCEBD5A80000000, float 0x3FCEBD5A80000000>
  %13 = fmul <8 x float> %12, %3
  %14 = fadd <8 x float> %13, <float 0x3FE62E4F60000000, float 0x3FE62E4F60000000, float 0x3FE62E4F60000000, float 0x3FE62E4F60000000, float 0x3FE62E4F60000000, float 0x3FE62E4F60000000, float 0x3FE62E4F60000000, float 0x3FE62E4F60000000>
  %15 = fmul <8 x float> %14, %3
  %16 = fadd <8 x float> %15, <float 0x3FEFFFFFE0000000, float 0x3FEFFFFFE0000000, float 0x3FEFFFFFE0000000, float 0x3FEFFFFFE0000000, float 0x3FEFFFFFE0000000, float 0x3FEFFFFFE0000000, float 0x3FEFFFFFE0000000, float 0x3FEFFFFFE0000000>
  %17 = fmul <8 x float> %6, %16
  ret <8 x float> %17
}
