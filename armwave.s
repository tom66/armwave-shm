	.arch armv6
	.eabi_attribute 28, 1	@ Tag_ABI_VFP_args
	.eabi_attribute 20, 1	@ Tag_ABI_FP_denormal
	.eabi_attribute 21, 1	@ Tag_ABI_FP_exceptions
	.eabi_attribute 23, 3	@ Tag_ABI_FP_number_model
	.eabi_attribute 24, 1	@ Tag_ABI_align8_needed
	.eabi_attribute 25, 1	@ Tag_ABI_align8_preserved
	.eabi_attribute 26, 2	@ Tag_ABI_enum_size
	.eabi_attribute 30, 2	@ Tag_ABI_optimization_goals
	.eabi_attribute 34, 1	@ Tag_CPU_unaligned_access
	.eabi_attribute 18, 4	@ Tag_ABI_PCS_wchar_t
	.file	"armwave.c"
@ GNU C17 (Raspbian 8.3.0-6+rpi1) version 8.3.0 (arm-linux-gnueabihf)
@	compiled by GNU C version 8.3.0, GMP version 6.1.2, MPFR version 4.0.2, MPC version 1.1.0, isl version isl-0.20-GMP

@ GGC heuristics: --param ggc-min-expand=81 --param ggc-min-heapsize=95638
@ options passed:  -I /usr/local/include/python3.8 -imultilib .
@ -imultiarch arm-linux-gnueabihf armwave.c -mfloat-abi=hard -mfpu=vfp
@ -mtls-dialect=gnu -marm -march=armv6+fp -auxbase-strip armwave.s -g -O3
@ -fverbose-asm -fwrapv
@ options enabled:  -faggressive-loop-optimizations -falign-jumps
@ -falign-labels -falign-loops -fauto-inc-dec -fbranch-count-reg
@ -fcaller-saves -fchkp-check-incomplete-type -fchkp-check-read
@ -fchkp-check-write -fchkp-instrument-calls -fchkp-narrow-bounds
@ -fchkp-optimize -fchkp-store-bounds -fchkp-use-static-bounds
@ -fchkp-use-static-const-bounds -fchkp-use-wrappers -fcode-hoisting
@ -fcombine-stack-adjustments -fcommon -fcompare-elim -fcprop-registers
@ -fcrossjumping -fcse-follow-jumps -fdefer-pop
@ -fdelete-null-pointer-checks -fdevirtualize -fdevirtualize-speculatively
@ -fdwarf2-cfi-asm -fearly-inlining -feliminate-unused-debug-types
@ -fexpensive-optimizations -fforward-propagate -ffp-int-builtin-inexact
@ -ffunction-cse -fgcse -fgcse-after-reload -fgcse-lm -fgnu-runtime
@ -fgnu-unique -fguess-branch-probability -fhoist-adjacent-loads -fident
@ -fif-conversion -fif-conversion2 -findirect-inlining -finline
@ -finline-atomics -finline-functions -finline-functions-called-once
@ -finline-small-functions -fipa-bit-cp -fipa-cp -fipa-cp-clone -fipa-icf
@ -fipa-icf-functions -fipa-icf-variables -fipa-profile -fipa-pure-const
@ -fipa-ra -fipa-reference -fipa-sra -fipa-vrp -fira-hoist-pressure
@ -fira-share-save-slots -fira-share-spill-slots
@ -fisolate-erroneous-paths-dereference -fivopts -fkeep-static-consts
@ -fleading-underscore -flifetime-dse -floop-interchange
@ -floop-unroll-and-jam -flra-remat -flto-odr-type-merging -fmath-errno
@ -fmerge-constants -fmerge-debug-strings -fmove-loop-invariants
@ -fomit-frame-pointer -foptimize-sibling-calls -foptimize-strlen
@ -fpartial-inlining -fpeel-loops -fpeephole -fpeephole2 -fplt
@ -fpredictive-commoning -fprefetch-loop-arrays -freg-struct-return
@ -freorder-blocks -freorder-functions -frerun-cse-after-loop
@ -fsched-critical-path-heuristic -fsched-dep-count-heuristic
@ -fsched-group-heuristic -fsched-interblock -fsched-last-insn-heuristic
@ -fsched-pressure -fsched-rank-heuristic -fsched-spec
@ -fsched-spec-insn-heuristic -fsched-stalled-insns-dep -fschedule-insns
@ -fschedule-insns2 -fsection-anchors -fsemantic-interposition
@ -fshow-column -fshrink-wrap -fshrink-wrap-separate -fsigned-zeros
@ -fsplit-ivs-in-unroller -fsplit-loops -fsplit-paths -fsplit-wide-types
@ -fssa-backprop -fssa-phiopt -fstdarg-opt -fstore-merging
@ -fstrict-aliasing -fstrict-volatile-bitfields -fsync-libcalls
@ -fthread-jumps -ftoplevel-reorder -ftrapping-math -ftree-bit-ccp
@ -ftree-builtin-call-dce -ftree-ccp -ftree-ch -ftree-coalesce-vars
@ -ftree-copy-prop -ftree-cselim -ftree-dce -ftree-dominator-opts
@ -ftree-dse -ftree-forwprop -ftree-fre -ftree-loop-distribute-patterns
@ -ftree-loop-distribution -ftree-loop-if-convert -ftree-loop-im
@ -ftree-loop-ivcanon -ftree-loop-optimize -ftree-loop-vectorize
@ -ftree-parallelize-loops= -ftree-partial-pre -ftree-phiprop -ftree-pre
@ -ftree-pta -ftree-reassoc -ftree-scev-cprop -ftree-sink
@ -ftree-slp-vectorize -ftree-slsr -ftree-sra -ftree-switch-conversion
@ -ftree-tail-merge -ftree-ter -ftree-vrp -funit-at-a-time -funswitch-loops
@ -fvar-tracking -fvar-tracking-assignments -fverbose-asm -fwrapv
@ -fzero-initialized-in-bss -marm -mbe32 -mglibc -mlittle-endian
@ -mpic-data-is-text-relative -msched-prolog -munaligned-access
@ -mvectorize-with-neon-quad

	.text
.Ltext0:
	.cfi_sections	.debug_frame
	.align	2
	.global	test_create_gamma
	.arch armv6
	.syntax unified
	.arm
	.fpu vfp
	.type	test_create_gamma, %function
test_create_gamma:
.LFB57:
	.file 1 "armwave.c"
	.loc 1 35 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 36 5 view .LVU1
	.loc 1 37 5 view .LVU2
.LVL0:
	.loc 1 39 5 view .LVU3
@ armwave.c:35: {
	.loc 1 35 1 is_stmt 0 view .LVU4
	push	{r4, r5, r6, lr}	@
	.cfi_def_cfa_offset 16
	.cfi_offset 4, -16
	.cfi_offset 5, -12
	.cfi_offset 6, -8
	.cfi_offset 14, -4
	vpush.64	{d8, d9, d10}	@
	.cfi_def_cfa_offset 40
	.cfi_offset 80, -40
	.cfi_offset 81, -36
	.cfi_offset 82, -32
	.cfi_offset 83, -28
	.cfi_offset 84, -24
	.cfi_offset 85, -20
	ldr	r4, .L6+20	@ ivtmp.32,
@ armwave.c:40:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 40 32 view .LVU5
	vldr.32	s20, .L6+16	@ tmp128,
@ armwave.c:40:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 40 26 view .LVU6
	vldr.64	d9, .L6	@ tmp135,
@ armwave.c:40:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 40 49 view .LVU7
	vldr.64	d8, .L6+8	@ tmp131,
	add	r5, r4, #1	@ _25, ivtmp.32,
	add	r6, r4, #256	@ _26, ivtmp.32,
	rsb	r5, r5, #1	@ tmp134, _25,
.LVL1:
.L2:
	.loc 1 40 9 is_stmt 1 discriminator 3 view .LVU8
@ armwave.c:40:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 40 32 is_stmt 0 discriminator 3 view .LVU9
	add	r3, r5, r4	@ tmp125, tmp134, ivtmp.32
@ armwave.c:40:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 40 26 discriminator 3 view .LVU10
	vmov.f64	d1, d9	@, tmp135
@ armwave.c:40:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 40 32 discriminator 3 view .LVU11
	vmov	s15, r3	@ int	@ tmp125, tmp125
	vcvt.f32.s32	s15, s15	@ tmp126, tmp125
	vdiv.f32	s0, s15, s20	@ tmp127, tmp126, tmp128
@ armwave.c:40:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 40 26 discriminator 3 view .LVU12
	vcvt.f64.f32	d0, s0	@, tmp127
	bl	pow		@
.LVL2:
@ armwave.c:40:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 40 49 discriminator 3 view .LVU13
	vmul.f64	d0, d0, d8	@ tmp130,, tmp131
@ armwave.c:40:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 40 24 discriminator 3 view .LVU14
	vcvt.u32.f64	s15, d0	@ tmp132, tmp130
	vmov	r3, s15	@ int	@ tmp132, tmp132
	strb	r3, [r4, #1]!	@ tmp132, MEM[base: _24, offset: 0B]
.LVL3:
@ armwave.c:39:     for(i = 0; i < 256; i++) {
	.loc 1 39 5 discriminator 3 view .LVU15
	cmp	r4, r6	@ ivtmp.32, _26
	bne	.L2		@,
@ armwave.c:42: }
	.loc 1 42 1 view .LVU16
	vldm	sp!, {d8-d10}	@,
	.cfi_restore 84
	.cfi_restore 85
	.cfi_restore 82
	.cfi_restore 83
	.cfi_restore 80
	.cfi_restore 81
	.cfi_def_cfa_offset 16
	pop	{r4, r5, r6, pc}	@
.LVL4:
.L7:
	.loc 1 42 1 view .LVU17
	.align	3
.L6:
	.word	-1073741824
	.word	1072483532
	.word	0
	.word	1081073664
	.word	1132396544
	.word	gamma_table-1
	.cfi_endproc
.LFE57:
	.size	test_create_gamma, .-test_create_gamma
	.align	2
	.global	armwave_init
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_init, %function
armwave_init:
.LFB58:
	.loc 1 48 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	.loc 1 49 5 view .LVU19
@ armwave.c:49:     g_armwave_state.flags = 0;
	.loc 1 49 27 is_stmt 0 view .LVU20
	ldr	r3, .L9	@ tmp110,
	mov	r2, #0	@ tmp111,
@ armwave.c:51:     printf("armwave version: %s\n", ARMWAVE_VER);
	.loc 1 51 5 view .LVU21
	ldr	r1, .L9+4	@,
	ldr	r0, .L9+8	@,
@ armwave.c:49:     g_armwave_state.flags = 0;
	.loc 1 49 27 view .LVU22
	str	r2, [r3]	@ tmp111, g_armwave_state.flags
	.loc 1 51 5 is_stmt 1 view .LVU23
	b	printf		@
.LVL5:
.L10:
	.align	2
.L9:
	.word	g_armwave_state
	.word	.LC0
	.word	.LC1
	.cfi_endproc
.LFE58:
	.size	armwave_init, .-armwave_init
	.align	2
	.global	render_nonaa_to_buffer_1ch_slice
	.syntax unified
	.arm
	.fpu vfp
	.type	render_nonaa_to_buffer_1ch_slice, %function
render_nonaa_to_buffer_1ch_slice:
.LVL6:
.LFB59:
	.loc 1 63 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 48
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 64 5 view .LVU25
	.loc 1 65 5 view .LVU26
	.loc 1 66 5 view .LVU27
	.loc 1 67 5 view .LVU28
	.loc 1 68 5 view .LVU29
	.loc 1 71 5 view .LVU30
@ armwave.c:71:     write_buffer_base = g_armwave_state.ch1_buffer + (((slice_y * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 71 40 is_stmt 0 view .LVU31
	ldr	r3, .L27	@ tmp205,
@ armwave.c:63: {
	.loc 1 63 1 view .LVU32
	push	{r4, r5, r6, r7, r8, r9, r10, fp, lr}	@
	.cfi_def_cfa_offset 36
	.cfi_offset 4, -36
	.cfi_offset 5, -32
	.cfi_offset 6, -28
	.cfi_offset 7, -24
	.cfi_offset 8, -20
	.cfi_offset 9, -16
	.cfi_offset 10, -12
	.cfi_offset 11, -8
	.cfi_offset 14, -4
	sub	sp, sp, #52	@,,
	.cfi_def_cfa_offset 88
@ armwave.c:71:     write_buffer_base = g_armwave_state.ch1_buffer + (((slice_y * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 71 82 view .LVU33
	ldr	r2, [r3, #36]	@ _2, g_armwave_state.cmp_x_bitdepth_scale
@ armwave.c:63: {
	.loc 1 63 1 view .LVU34
	mov	r4, r1	@ height, height
	str	r1, [sp, #16]	@ height, %sfp
@ armwave.c:71:     write_buffer_base = g_armwave_state.ch1_buffer + (((slice_y * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 71 65 view .LVU35
	mul	r1, r0, r2	@ tmp208, slice_y, _2
.LVL7:
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 35 view .LVU36
	ldr	ip, [r3, #56]	@ _93, g_armwave_state.waves
@ armwave.c:71:     write_buffer_base = g_armwave_state.ch1_buffer + (((slice_y * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 71 147 view .LVU37
	ldr	fp, [r3, #64]	@ _6, g_armwave_state.bitdepth_height
@ armwave.c:71:     write_buffer_base = g_armwave_state.ch1_buffer + (((slice_y * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 71 105 view .LVU38
	lsr	r1, r1, #16	@ tmp209, tmp208,
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 5 view .LVU39
	cmp	ip, #0	@ _93,
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 35 view .LVU40
	str	ip, [sp, #32]	@ _93, %sfp
@ armwave.c:71:     write_buffer_base = g_armwave_state.ch1_buffer + (((slice_y * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 71 40 view .LVU41
	ldr	lr, [r3, #4]	@ _1, g_armwave_state.ch1_buffer
@ armwave.c:71:     write_buffer_base = g_armwave_state.ch1_buffer + (((slice_y * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 71 130 view .LVU42
	mul	r10, fp, r1	@ _7, _6, tmp209
.LVL8:
	.loc 1 79 5 is_stmt 1 view .LVU43
	.loc 1 79 5 is_stmt 0 view .LVU44
	ble	.L11		@,
@ armwave.c:80:         wave_base = g_armwave_state.wave_buffer + slice_y + (w * g_armwave_state.wave_stride);
	.loc 1 80 36 view .LVU45
	ldr	r1, [r3, #20]	@ _8, g_armwave_state.wave_buffer
@ armwave.c:80:         wave_base = g_armwave_state.wave_buffer + slice_y + (w * g_armwave_state.wave_stride);
	.loc 1 80 81 view .LVU46
	ldr	r3, [r3, #52]	@ _9, g_armwave_state.wave_stride
	cmp	r4, #0	@ height,
	str	r3, [sp, #36]	@ _9, %sfp
	beq	.L11		@,
	rsb	ip, r2, r2, lsl #31	@ tmp219, _2, _2,
	add	r3, r1, r0	@ tmp213, _8, slice_y
	rsb	r0, r3, #0	@ ivtmp.47, tmp213
.LVL9:
	.loc 1 80 81 view .LVU47
	lsl	r1, r2, #1	@ tmp215, _2,
	str	r3, [sp, #24]	@ tmp213, %sfp
	lsl	r3, ip, #1	@ tmp220, tmp219,
	str	r3, [sp, #20]	@ tmp220, %sfp
	add	r3, r1, r2	@ ivtmp.41, tmp215, _2
	str	r3, [sp, #40]	@ ivtmp.41, %sfp
	lsl	r3, r2, #2	@ _186, _2,
	str	r3, [sp]	@ _186, %sfp
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 11 view .LVU48
	mov	r3, #0	@ w,
	str	r1, [sp, #44]	@ tmp215, %sfp
	str	r0, [sp, #12]	@ ivtmp.47, %sfp
	str	r3, [sp, #28]	@ w, %sfp
.LVL10:
.L19:
	.loc 1 80 9 is_stmt 1 view .LVU49
	.loc 1 84 9 view .LVU50
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 11 is_stmt 0 view .LVU51
	ldr	r6, [sp, #44]	@ ivtmp.42, %sfp
	ldr	r9, [sp, #40]	@ ivtmp.41, %sfp
	ldr	r8, [sp, #24]	@ ivtmp.48, %sfp
.LVL11:
.L14:
	.loc 1 87 13 is_stmt 1 view .LVU52
	.loc 1 101 17 view .LVU53
	.loc 1 106 17 view .LVU54
	.loc 1 101 17 view .LVU55
	.loc 1 106 17 view .LVU56
	.loc 1 101 17 view .LVU57
	.loc 1 106 17 view .LVU58
	.loc 1 101 17 view .LVU59
	.loc 1 106 17 view .LVU60
@ armwave.c:102:                     ((((yy + ys) * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 102 34 is_stmt 0 view .LVU61
	ldr	r2, [sp, #20]	@ tmp220, %sfp
@ armwave.c:102:                     ((((yy + ys) * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 102 74 view .LVU62
	lsr	r0, r9, #16	@ tmp277, ivtmp.41,
@ armwave.c:102:                     ((((yy + ys) * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 102 34 view .LVU63
	add	r3, r6, r2	@ tmp228, ivtmp.42, tmp220
@ armwave.c:102:                     ((((yy + ys) * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 102 74 view .LVU64
	str	r0, [sp, #4]	@ tmp277, %sfp
	lsr	r3, r3, #16	@ tmp229, tmp228,
@ armwave.c:87:             word = *(uint32_t*)(wave_base + yy);
	.loc 1 87 18 view .LVU65
	ldr	r0, [r8], #4	@ word, MEM[base: _182, offset: 0B]
.LVL12:
	.loc 1 89 13 is_stmt 1 view .LVU66
	.loc 1 91 17 view .LVU67
	.loc 1 94 17 view .LVU68
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 is_stmt 0 view .LVU69
	mla	r3, fp, r3, r10	@ tmp231, _6, tmp229, _7
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 20 view .LVU70
	ands	r7, r0, #255	@ scale_value, word,
@ armwave.c:102:                     ((((yy + ys) * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 102 34 view .LVU71
	add	r1, r9, r2	@ tmp245, ivtmp.41, tmp220
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 20 view .LVU72
	moveq	r5, #1	@ tmp222,
	movne	r5, #0	@ tmp222,
	ldr	ip, [sp]	@ _186, %sfp
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU73
	add	r3, r3, r7	@ tmp232, tmp231, scale_value
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 19 view .LVU74
	cmp	r7, #255	@ scale_value,
	orreq	r5, r5, #1	@,, tmp227, tmp222
@ armwave.c:102:                     ((((yy + ys) * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 102 74 view .LVU75
	lsr	r2, r6, #16	@ tmp262, ivtmp.42,
	lsr	r1, r1, #16	@ tmp246, tmp245,
@ armwave.c:84:         for(yy = 0; yy < height; yy += 4) {
	.loc 1 84 9 view .LVU76
	ldr	r7, [sp, #12]	@ ivtmp.47, %sfp
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 19 view .LVU77
	cmp	r5, #0	@ tmp227,
	add	r9, r9, ip	@ ivtmp.41, ivtmp.41, _186
.LVL13:
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU78
	mla	r1, fp, r1, r10	@ tmp248, _6, tmp246, _7
	mla	r2, fp, r2, r10	@ tmp264, _6, tmp262, _7
@ armwave.c:107:                 word >>= 8;
	.loc 1 107 22 view .LVU79
	lsr	r4, r0, #8	@ word, word,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU80
	lsl	r3, r3, #1	@ tmp233, tmp232,
@ armwave.c:107:                 word >>= 8;
	.loc 1 107 22 view .LVU81
	lsr	ip, r0, #16	@ word, word,
@ armwave.c:84:         for(yy = 0; yy < height; yy += 4) {
	.loc 1 84 9 view .LVU82
	add	r7, r8, r7	@ tmp285, ivtmp.40, ivtmp.47
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 19 view .LVU83
	bne	.L20		@,
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 20 view .LVU84
	ands	r4, r4, #255	@ scale_value, word,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU85
	mov	r5, r4	@ scale_value, scale_value
	add	r1, r1, r4	@ tmp249, tmp248, scale_value
	ldrh	r4, [lr, r3]	@ *_75, *_75
	lsl	r1, r1, #1	@ tmp250, tmp249,
	str	r4, [sp, #8]	@ *_75, %sfp
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 20 view .LVU86
	moveq	r4, #1	@ tmp239,
	movne	r4, #0	@ tmp239,
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 19 view .LVU87
	cmp	r5, #255	@ scale_value,
	orreq	r4, r4, #1	@,, tmp239, tmp239
	cmp	r4, #0	@ tmp244,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU88
	ldr	r4, [sp, #8]	@ *_75, %sfp
	add	r5, r4, #1	@ tmp236, *_75,
	strh	r5, [lr, r3]	@ movhi	@ tmp236, *_75
	.loc 1 107 17 is_stmt 1 view .LVU89
.LVL14:
	.loc 1 91 17 view .LVU90
	.loc 1 94 17 view .LVU91
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 19 is_stmt 0 view .LVU92
	bne	.L20		@,
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 20 view .LVU93
	ands	ip, ip, #255	@ scale_value, word,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU94
	ldrh	r3, [lr, r1]	@ *_104, *_104
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 20 view .LVU95
	moveq	r4, #1	@ tmp256,
	movne	r4, #0	@ tmp256,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU96
	add	r2, r2, ip	@ tmp265, tmp264, scale_value
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 19 view .LVU97
	cmp	ip, #255	@ scale_value,
	movne	ip, r4	@, tmp261, tmp256
	orreq	ip, r4, #1	@,, tmp261, tmp256
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU98
	add	r3, r3, #1	@ tmp253, *_104,
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 19 view .LVU99
	cmp	ip, #0	@ tmp261,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU100
	lsl	r2, r2, #1	@ tmp266, tmp265,
	strh	r3, [lr, r1]	@ movhi	@ tmp253, *_104
	.loc 1 107 17 is_stmt 1 view .LVU101
.LVL15:
	.loc 1 91 17 view .LVU102
	.loc 1 94 17 view .LVU103
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 19 is_stmt 0 view .LVU104
	bne	.L20		@,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU105
	ldr	r3, [sp, #4]	@ tmp277, %sfp
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 20 view .LVU106
	lsrs	r0, r0, #24	@ word, word,
.LVL16:
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU107
	ldrh	r1, [lr, r2]	@ *_133, *_133
	mla	r3, fp, r3, r0	@ tmp279, _6, tmp277, word
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 20 view .LVU108
	moveq	ip, #1	@ tmp271,
	movne	ip, #0	@ tmp271,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU109
	add	r3, r3, r10	@ tmp280, tmp279, _7
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 19 view .LVU110
	cmp	r0, #255	@ word,
	movne	r0, ip	@, tmp276, tmp271
	orreq	r0, ip, #1	@,, tmp276, tmp271
	cmp	r0, #0	@ tmp276,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU111
	lsl	r3, r3, #1	@ tmp281, tmp280,
	add	r1, r1, #1	@ tmp269, *_133,
	strh	r1, [lr, r2]	@ movhi	@ tmp269, *_133
	.loc 1 107 17 is_stmt 1 view .LVU112
.LVL17:
	.loc 1 91 17 view .LVU113
	.loc 1 94 17 view .LVU114
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 is_stmt 0 view .LVU115
	ldrheq	r2, [lr, r3]	@ *_162, *_162
	addeq	r2, r2, #1	@ tmp284, *_162,
	strheq	r2, [lr, r3]	@ movhi	@ tmp284, *_162
	.loc 1 107 17 is_stmt 1 view .LVU116
.LVL18:
.L20:
@ armwave.c:84:         for(yy = 0; yy < height; yy += 4) {
	.loc 1 84 9 is_stmt 0 view .LVU117
	ldr	r3, [sp, #16]	@ height, %sfp
	cmp	r3, r7	@ height, tmp285
	ldr	r3, [sp]	@ _186, %sfp
	add	r6, r6, r3	@ ivtmp.42, ivtmp.42, _186
	bhi	.L14		@,
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 44 view .LVU118
	ldr	r3, [sp, #28]	@ w, %sfp
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 5 view .LVU119
	ldr	r2, [sp, #32]	@ _93, %sfp
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 44 view .LVU120
	add	r3, r3, #1	@ w, w,
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 5 view .LVU121
	cmp	r3, r2	@ w, _93
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 44 view .LVU122
	str	r3, [sp, #28]	@ w, %sfp
.LVL19:
	.loc 1 79 44 view .LVU123
	ldr	r2, [sp, #12]	@ ivtmp.47, %sfp
	ldr	r3, [sp, #36]	@ _9, %sfp
.LVL20:
	.loc 1 79 44 view .LVU124
	sub	r2, r2, r3	@ ivtmp.47, ivtmp.47, _9
	str	r2, [sp, #12]	@ ivtmp.47, %sfp
	ldr	r2, [sp, #24]	@ ivtmp.48, %sfp
	add	r3, r2, r3	@ ivtmp.48, ivtmp.48, _9
	str	r3, [sp, #24]	@ ivtmp.48, %sfp
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 5 view .LVU125
	bne	.L19		@,
.LVL21:
.L11:
@ armwave.c:111: }
	.loc 1 111 1 view .LVU126
	add	sp, sp, #52	@,,
	.cfi_def_cfa_offset 36
	@ sp needed	@
	pop	{r4, r5, r6, r7, r8, r9, r10, fp, pc}	@
.LVL22:
.L28:
	.loc 1 111 1 view .LVU127
	.align	2
.L27:
	.word	g_armwave_state
	.cfi_endproc
.LFE59:
	.size	render_nonaa_to_buffer_1ch_slice, .-render_nonaa_to_buffer_1ch_slice
	.align	2
	.global	armwave_fill_pixbuf_scaled
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_fill_pixbuf_scaled, %function
armwave_fill_pixbuf_scaled:
.LVL23:
.LFB60:
	.loc 1 118 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 119 5 view .LVU129
	.loc 1 121 5 view .LVU130
	.loc 1 122 5 view .LVU131
	.loc 1 124 5 view .LVU132
	.loc 1 125 5 view .LVU133
	.loc 1 126 5 view .LVU134
	.loc 1 128 5 view .LVU135
@ armwave.c:118: {
	.loc 1 118 1 is_stmt 0 view .LVU136
	push	{r4, r5, r6, r7, r8, r9, r10, lr}	@
	.cfi_def_cfa_offset 32
	.cfi_offset 4, -32
	.cfi_offset 5, -28
	.cfi_offset 6, -24
	.cfi_offset 7, -20
	.cfi_offset 8, -16
	.cfi_offset 9, -12
	.cfi_offset 10, -8
	.cfi_offset 14, -4
@ armwave.c:128:     assert(out_buffer != NULL);
	.loc 1 128 5 view .LVU137
	subs	r5, r0, #0	@ out_buffer, out_buffer
	beq	.L39		@,
	.loc 1 131 5 is_stmt 1 view .LVU138
@ armwave.c:131:     memset(out_buffer, 0x00, g_armwave_state.target_width * g_armwave_state.target_height * 4);
	.loc 1 131 45 is_stmt 0 view .LVU139
	ldr	r4, .L40	@ tmp220,
@ armwave.c:131:     memset(out_buffer, 0x00, g_armwave_state.target_width * g_armwave_state.target_height * 4);
	.loc 1 131 5 view .LVU140
	mov	r1, #0	@,
@ armwave.c:131:     memset(out_buffer, 0x00, g_armwave_state.target_width * g_armwave_state.target_height * 4);
	.loc 1 131 59 view .LVU141
	ldr	r3, [r4, #88]	@ g_armwave_state.target_width, g_armwave_state.target_width
	ldr	r2, [r4, #92]	@ g_armwave_state.target_height, g_armwave_state.target_height
	mul	r2, r2, r3	@ tmp175, g_armwave_state.target_height, g_armwave_state.target_width
@ armwave.c:131:     memset(out_buffer, 0x00, g_armwave_state.target_width * g_armwave_state.target_height * 4);
	.loc 1 131 5 view .LVU142
	lsl	r2, r2, #2	@, tmp175,
	bl	memset		@
.LVL24:
	.loc 1 133 5 is_stmt 1 view .LVU143
@ armwave.c:133:     npix = g_armwave_state.target_width * g_armwave_state.bitdepth_height; 
	.loc 1 133 10 is_stmt 0 view .LVU144
	ldr	r3, [r4, #88]	@ g_armwave_state.target_width, g_armwave_state.target_width
	ldr	ip, [r4, #64]	@ g_armwave_state.bitdepth_height, g_armwave_state.bitdepth_height
	mul	ip, ip, r3	@ npix, g_armwave_state.bitdepth_height, g_armwave_state.target_width
.LVL25:
	.loc 1 176 5 is_stmt 1 view .LVU145
	.loc 1 176 5 is_stmt 0 view .LVU146
	cmp	ip, #0	@ npix,
	pople	{r4, r5, r6, r7, r8, r9, r10, pc}	@
@ armwave.c:177:         wave_word = g_armwave_state.ch1_buffer[n];
	.loc 1 177 36 view .LVU147
	ldr	r1, [r4, #4]	@ _8, g_armwave_state.ch1_buffer
@ armwave.c:181:             rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 181 44 view .LVU148
	ldrsh	r8, [r4, #108]	@ _13, g_armwave_state.ch1_color.r
@ armwave.c:182:             gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 182 44 view .LVU149
	ldrsh	r7, [r4, #110]	@ _17, g_armwave_state.ch1_color.g
@ armwave.c:183:             bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 183 44 view .LVU150
	ldrsh	r6, [r4, #112]	@ _20, g_armwave_state.ch1_color.b
@ armwave.c:194:             yy = (nsub & 0xff) * g_armwave_state.vscale_frac;
	.loc 1 194 49 view .LVU151
	vldr.32	s14, [r4, #40]	@ _35, g_armwave_state.vscale_frac
	sub	r1, r1, #2	@ ivtmp.52, _8,
	add	ip, r1, ip, lsl #1	@ _96, ivtmp.52, npix,
.LVL26:
@ armwave.c:176:     for(n = 0; n < npix; n += 1) {
	.loc 1 176 11 view .LVU152
	mov	r0, #0	@ n,
.LVL27:
.L33:
	.loc 1 177 9 is_stmt 1 view .LVU153
	.loc 1 178 9 view .LVU154
@ armwave.c:178:         value = wave_word & 0xffff;
	.loc 1 178 15 is_stmt 0 view .LVU155
	ldrh	r3, [r1, #2]!	@ MEM[base: _101, offset: 0B], MEM[base: _101, offset: 0B]
.LVL28:
	.loc 1 180 9 is_stmt 1 view .LVU156
@ armwave.c:180:         if(value != 0) {
	.loc 1 180 11 is_stmt 0 view .LVU157
	ands	r3, r3, #255	@ value, MEM[base: _101, offset: 0B],
.LVL29:
	.loc 1 180 11 view .LVU158
	beq	.L32		@,
	.loc 1 181 13 is_stmt 1 view .LVU159
.LVL30:
	.loc 1 182 13 view .LVU160
	.loc 1 183 13 view .LVU161
	.loc 1 185 13 view .LVU162
	.loc 1 186 13 view .LVU163
	.loc 1 187 13 view .LVU164
	.loc 1 190 13 view .LVU165
	.loc 1 193 13 view .LVU166
	.loc 1 194 13 view .LVU167
	.loc 1 195 13 view .LVU168
	.loc 1 196 13 view .LVU169
	.loc 1 199 13 view .LVU170
	.loc 1 201 17 view .LVU171
	.loc 1 202 17 view .LVU172
@ armwave.c:194:             yy = (nsub & 0xff) * g_armwave_state.vscale_frac;
	.loc 1 194 24 is_stmt 0 view .LVU173
	uxtb	lr, r0	@ n, n
@ armwave.c:183:             bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 183 47 view .LVU174
	smulbb	r2, r3, r6	@ tmp206, value, _20
@ armwave.c:194:             yy = (nsub & 0xff) * g_armwave_state.vscale_frac;
	.loc 1 194 32 view .LVU175
	vmov	s15, lr	@ int	@ n, n
@ armwave.c:182:             gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 182 47 view .LVU176
	smulbb	r9, r3, r7	@ tmp210, value, _17
@ armwave.c:183:             bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 183 16 view .LVU177
	asr	r2, r2, #8	@ bb, tmp206,
@ armwave.c:187:             b = MIN(bb, 255);
	.loc 1 187 17 view .LVU178
	cmp	r2, #255	@ bb,
@ armwave.c:194:             yy = (nsub & 0xff) * g_armwave_state.vscale_frac;
	.loc 1 194 32 view .LVU179
	vcvt.f32.s32	s15, s15	@ tmp197, n
@ armwave.c:182:             gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 182 16 view .LVU180
	asr	r9, r9, #8	@ gg, tmp210,
@ armwave.c:181:             rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 181 47 view .LVU181
	smulbb	r3, r3, r8	@ tmp216, value, _13
.LVL31:
@ armwave.c:187:             b = MIN(bb, 255);
	.loc 1 187 17 view .LVU182
	movge	r2, #255	@ bb,
@ armwave.c:186:             g = MIN(gg, 255);
	.loc 1 186 17 view .LVU183
	cmp	r9, #255	@ gg,
	movge	r9, #255	@ gg,
@ armwave.c:181:             rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 181 16 view .LVU184
	asr	r3, r3, #8	@ rr, tmp216,
@ armwave.c:185:             r = MIN(rr, 255);
	.loc 1 185 17 view .LVU185
	cmp	r3, #255	@ rr,
@ armwave.c:194:             yy = (nsub & 0xff) * g_armwave_state.vscale_frac;
	.loc 1 194 32 view .LVU186
	vmul.f32	s15, s15, s14	@ tmp198, tmp197, _35
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 48 view .LVU187
	lsl	r9, r9, #8	@ tmp211, gg,
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 36 view .LVU188
	lsl	r2, r2, #16	@ tmp207, bb,
@ armwave.c:185:             r = MIN(rr, 255);
	.loc 1 185 17 view .LVU189
	movge	r3, #255	@ rr,
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 48 view .LVU190
	uxth	r9, r9	@ tmp212, tmp211
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 36 view .LVU191
	and	r2, r2, #16711680	@ tmp208, tmp207,
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 43 view .LVU192
	orr	r2, r2, r9	@ tmp214, tmp208, tmp212
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 54 view .LVU193
	uxtb	r3, r3	@ rr, rr
@ armwave.c:194:             yy = (nsub & 0xff) * g_armwave_state.vscale_frac;
	.loc 1 194 16 view .LVU194
	vcvt.u32.f32	s15, s15	@ yy, tmp198
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 54 view .LVU195
	orr	r3, r2, r3	@ tmp218, tmp214, rr
@ armwave.c:201:                 offset = (xx + (y * g_armwave_state.target_width)); 
	.loc 1 201 35 view .LVU196
	ldr	lr, [r4, #88]	@ g_armwave_state.target_width, g_armwave_state.target_width
@ armwave.c:196:             xx = (nsub >> 8) / 2;
	.loc 1 196 30 view .LVU197
	asr	r9, r0, #9	@ xx, n,
.LVL32:
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 18 view .LVU198
	orr	r3, r3, #-16777216	@ word, tmp218,
@ armwave.c:202:                 *(out_buffer_base + offset) = word;
	.loc 1 202 35 view .LVU199
	vmov	r2, s15	@ int	@ yy, yy
	mla	lr, lr, r2, r9	@ tmp204, g_armwave_state.target_width, yy, xx
@ armwave.c:202:                 *(out_buffer_base + offset) = word;
	.loc 1 202 45 view .LVU200
	str	r3, [r5, lr, lsl #2]	@ word, *_43
.LVL33:
.L32:
@ armwave.c:176:     for(n = 0; n < npix; n += 1) {
	.loc 1 176 5 discriminator 2 view .LVU201
	cmp	ip, r1	@ _96, ivtmp.52
@ armwave.c:176:     for(n = 0; n < npix; n += 1) {
	.loc 1 176 28 discriminator 2 view .LVU202
	add	r0, r0, #1	@ n, n,
.LVL34:
@ armwave.c:176:     for(n = 0; n < npix; n += 1) {
	.loc 1 176 5 discriminator 2 view .LVU203
	bne	.L33		@,
	pop	{r4, r5, r6, r7, r8, r9, r10, pc}	@
.LVL35:
.L39:
	.loc 1 128 5 is_stmt 1 discriminator 1 view .LVU204
	ldr	r3, .L40+4	@,
	mov	r2, #128	@,
	ldr	r1, .L40+8	@,
	ldr	r0, .L40+12	@,
.LVL36:
	.loc 1 128 5 is_stmt 0 discriminator 1 view .LVU205
	bl	__assert_fail		@
.LVL37:
.L41:
	.loc 1 128 5 discriminator 1 view .LVU206
	.align	2
.L40:
	.word	g_armwave_state
	.word	.LANCHOR0
	.word	.LC2
	.word	.LC3
	.cfi_endproc
.LFE60:
	.size	armwave_fill_pixbuf_scaled, .-armwave_fill_pixbuf_scaled
	.global	__aeabi_idiv
	.align	2
	.global	armwave_generate
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_generate, %function
armwave_generate:
.LFB61:
	.loc 1 212 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 72
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 213 5 view .LVU208
	.loc 1 214 5 view .LVU209
.LVL38:
	.loc 1 216 5 view .LVU210
@ armwave.c:212: {
	.loc 1 212 1 is_stmt 0 view .LVU211
	push	{r4, r5, r6, r7, r8, r9, r10, fp, lr}	@
	.cfi_def_cfa_offset 36
	.cfi_offset 4, -36
	.cfi_offset 5, -32
	.cfi_offset 6, -28
	.cfi_offset 7, -24
	.cfi_offset 8, -20
	.cfi_offset 9, -16
	.cfi_offset 10, -12
	.cfi_offset 11, -8
	.cfi_offset 14, -4
@ armwave.c:216:     memset(g_armwave_state.ch1_buffer, 0, g_armwave_state.ch_buff_size);
	.loc 1 216 5 view .LVU212
	mov	r1, #0	@,
@ armwave.c:216:     memset(g_armwave_state.ch1_buffer, 0, g_armwave_state.ch_buff_size);
	.loc 1 216 58 view .LVU213
	ldr	r4, .L62	@ tmp212,
@ armwave.c:212: {
	.loc 1 212 1 view .LVU214
	sub	sp, sp, #76	@,,
	.cfi_def_cfa_offset 112
@ armwave.c:216:     memset(g_armwave_state.ch1_buffer, 0, g_armwave_state.ch_buff_size);
	.loc 1 216 5 view .LVU215
	ldr	r2, [r4, #80]	@, g_armwave_state.ch_buff_size
	ldr	r0, [r4, #4]	@, g_armwave_state.ch1_buffer
	bl	memset		@
.LVL39:
	.loc 1 218 5 is_stmt 1 view .LVU216
@ armwave.c:218:     for(yy = 0; yy < (g_armwave_state.wave_length / g_armwave_state.slice_height); yy++) {
	.loc 1 218 68 is_stmt 0 view .LVU217
	ldr	r5, [r4, #68]	@ _133, g_armwave_state.slice_height
@ armwave.c:218:     for(yy = 0; yy < (g_armwave_state.wave_length / g_armwave_state.slice_height); yy++) {
	.loc 1 218 51 view .LVU218
	ldr	r0, [r4, #76]	@, g_armwave_state.wave_length
	mov	r1, r5	@, _133
@ armwave.c:218:     for(yy = 0; yy < (g_armwave_state.wave_length / g_armwave_state.slice_height); yy++) {
	.loc 1 218 68 view .LVU219
	str	r5, [sp, #36]	@ _133, %sfp
@ armwave.c:218:     for(yy = 0; yy < (g_armwave_state.wave_length / g_armwave_state.slice_height); yy++) {
	.loc 1 218 51 view .LVU220
	bl	__aeabi_idiv		@
.LVL40:
@ armwave.c:218:     for(yy = 0; yy < (g_armwave_state.wave_length / g_armwave_state.slice_height); yy++) {
	.loc 1 218 5 view .LVU221
	subs	r3, r0, #0	@ _179,
	str	r3, [sp, #64]	@ _179, %sfp
	beq	.L42		@,
.LBB10:
.LBB11:
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 35 view .LVU222
	ldr	r3, [r4, #56]	@ _38, g_armwave_state.waves
@ armwave.c:80:         wave_base = g_armwave_state.wave_buffer + slice_y + (w * g_armwave_state.wave_stride);
	.loc 1 80 81 view .LVU223
	ldr	r1, [r4, #52]	@ _24, g_armwave_state.wave_stride
	cmp	r3, #0	@ _38,
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 35 view .LVU224
	str	r3, [sp, #48]	@ _38, %sfp
@ armwave.c:71:     write_buffer_base = g_armwave_state.ch1_buffer + (((slice_y * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 71 40 view .LVU225
	ldr	r8, [r4, #4]	@ _16, g_armwave_state.ch1_buffer
@ armwave.c:71:     write_buffer_base = g_armwave_state.ch1_buffer + (((slice_y * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 71 82 view .LVU226
	ldr	r3, [r4, #36]	@ _17, g_armwave_state.cmp_x_bitdepth_scale
@ armwave.c:71:     write_buffer_base = g_armwave_state.ch1_buffer + (((slice_y * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 71 147 view .LVU227
	ldr	r9, [r4, #64]	@ _21, g_armwave_state.bitdepth_height
@ armwave.c:80:         wave_base = g_armwave_state.wave_buffer + slice_y + (w * g_armwave_state.wave_stride);
	.loc 1 80 36 view .LVU228
	ldr	r2, [r4, #20]	@ _23, g_armwave_state.wave_buffer
@ armwave.c:80:         wave_base = g_armwave_state.wave_buffer + slice_y + (w * g_armwave_state.wave_stride);
	.loc 1 80 81 view .LVU229
	str	r1, [sp, #44]	@ _24, %sfp
	ble	.L42		@,
	cmp	r5, #0	@ _133,
	beq	.L42		@,
	mul	r0, r3, r5	@ _185, _17, _133
	rsb	r1, r3, r3, lsl #31	@ tmp235, _17, _17,
	str	r2, [sp, #16]	@ ivtmp.72, %sfp
	rsb	r2, r2, #0	@ ivtmp.71, ivtmp.72
	str	r0, [sp, #68]	@ _185, %sfp
	str	r2, [sp, #20]	@ ivtmp.71, %sfp
	lsl	r0, r3, #1	@ tmp231, _17,
	lsl	r2, r1, #1	@ tmp236, tmp235,
	mov	fp, r9	@ _21, _21
	mov	ip, #0	@ ivtmp.70,
	str	r2, [sp, #60]	@ tmp236, %sfp
	add	r2, r0, r3	@ ivtmp.60, tmp231, _17
	lsl	r3, r3, #2	@ _208, _17,
	str	r0, [sp, #56]	@ tmp231, %sfp
	str	ip, [sp, #24]	@ ivtmp.70, %sfp
	str	r2, [sp, #52]	@ ivtmp.60, %sfp
.LBE11:
.LBE10:
@ armwave.c:218:     for(yy = 0; yy < (g_armwave_state.wave_length / g_armwave_state.slice_height); yy++) {
	.loc 1 218 12 view .LVU230
	str	ip, [sp, #40]	@ ivtmp.70, %sfp
	str	r3, [sp, #28]	@ _208, %sfp
.LVL41:
.L48:
	.loc 1 222 9 is_stmt 1 discriminator 3 view .LVU231
.LBB13:
.LBI10:
	.loc 1 62 6 discriminator 3 view .LVU232
.LBB12:
	.loc 1 64 5 discriminator 3 view .LVU233
	.loc 1 65 5 discriminator 3 view .LVU234
	.loc 1 66 5 discriminator 3 view .LVU235
	.loc 1 67 5 discriminator 3 view .LVU236
	.loc 1 68 5 discriminator 3 view .LVU237
	.loc 1 71 5 discriminator 3 view .LVU238
@ armwave.c:71:     write_buffer_base = g_armwave_state.ch1_buffer + (((slice_y * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 71 105 is_stmt 0 discriminator 3 view .LVU239
	ldr	r3, [sp, #24]	@ ivtmp.70, %sfp
@ armwave.c:71:     write_buffer_base = g_armwave_state.ch1_buffer + (((slice_y * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 71 130 discriminator 3 view .LVU240
	ldr	r2, [sp, #16]	@ ivtmp.72, %sfp
@ armwave.c:71:     write_buffer_base = g_armwave_state.ch1_buffer + (((slice_y * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 71 105 discriminator 3 view .LVU241
	lsr	r3, r3, #16	@ tmp237, ivtmp.70,
@ armwave.c:71:     write_buffer_base = g_armwave_state.ch1_buffer + (((slice_y * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 71 130 discriminator 3 view .LVU242
	str	r2, [sp]	@ ivtmp.72, %sfp
	mul	r7, fp, r3	@ _22, _21, tmp237
.LVL42:
	.loc 1 79 5 is_stmt 1 discriminator 3 view .LVU243
@ armwave.c:71:     write_buffer_base = g_armwave_state.ch1_buffer + (((slice_y * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 71 130 is_stmt 0 discriminator 3 view .LVU244
	ldr	r3, [sp, #20]	@ ivtmp.71, %sfp
	str	r3, [sp, #4]	@ ivtmp.71, %sfp
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 11 discriminator 3 view .LVU245
	mov	r3, #0	@ w,
	str	r3, [sp, #12]	@ w, %sfp
.LVL43:
.L45:
	.loc 1 80 9 is_stmt 1 view .LVU246
	.loc 1 84 9 view .LVU247
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 11 is_stmt 0 view .LVU248
	ldr	r5, [sp, #56]	@ ivtmp.61, %sfp
	ldr	r10, [sp, #52]	@ ivtmp.60, %sfp
	ldr	r3, [sp]	@ ivtmp.67, %sfp
	str	r3, [sp, #8]	@ ivtmp.67, %sfp
.LVL44:
.L47:
	.loc 1 87 13 is_stmt 1 view .LVU249
	.loc 1 101 17 view .LVU250
	.loc 1 106 17 view .LVU251
	.loc 1 101 17 view .LVU252
	.loc 1 106 17 view .LVU253
	.loc 1 101 17 view .LVU254
	.loc 1 106 17 view .LVU255
	.loc 1 101 17 view .LVU256
	.loc 1 106 17 view .LVU257
@ armwave.c:102:                     ((((yy + ys) * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 102 34 is_stmt 0 view .LVU258
	ldr	r2, [sp, #60]	@ tmp236, %sfp
@ armwave.c:87:             word = *(uint32_t*)(wave_base + yy);
	.loc 1 87 18 view .LVU259
	ldr	lr, [sp, #8]	@ ivtmp.59, %sfp
@ armwave.c:102:                     ((((yy + ys) * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 102 34 view .LVU260
	add	r3, r2, r5	@ tmp245, tmp236, ivtmp.61
	add	r1, r2, r10	@ tmp262, tmp236, ivtmp.60
@ armwave.c:102:                     ((((yy + ys) * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 102 74 view .LVU261
	lsr	r3, r3, #16	@ tmp246, tmp245,
@ armwave.c:87:             word = *(uint32_t*)(wave_base + yy);
	.loc 1 87 18 view .LVU262
	ldr	r0, [lr], #4	@ word, MEM[base: _204, offset: 0B]
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU263
	mla	r3, fp, r3, r7	@ tmp248, _21, tmp246, _22
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 20 view .LVU264
	ands	r9, r0, #255	@ scale_value, word,
@ armwave.c:107:                 word >>= 8;
	.loc 1 107 22 view .LVU265
	lsr	r6, r0, #16	@ word, word,
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 20 view .LVU266
	moveq	r6, #1	@ tmp239,
	movne	r6, #0	@ tmp239,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU267
	add	r3, r3, r9	@ tmp249, tmp248, scale_value
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 19 view .LVU268
	cmp	r9, #255	@ scale_value,
	orreq	r6, r6, #1	@,, tmp244, tmp239
@ armwave.c:87:             word = *(uint32_t*)(wave_base + yy);
	.loc 1 87 18 view .LVU269
	str	lr, [sp, #8]	@ ivtmp.59, %sfp
.LVL45:
	.loc 1 89 13 is_stmt 1 view .LVU270
	.loc 1 91 17 view .LVU271
	.loc 1 94 17 view .LVU272
@ armwave.c:84:         for(yy = 0; yy < height; yy += 4) {
	.loc 1 84 9 is_stmt 0 view .LVU273
	mov	r9, lr	@ ivtmp.59, ivtmp.59
	ldr	lr, [sp, #4]	@ ivtmp.66, %sfp
	.loc 1 84 9 view .LVU274
	ldr	r4, [sp, #28]	@ _208, %sfp
@ armwave.c:102:                     ((((yy + ys) * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 102 74 view .LVU275
	lsr	r2, r5, #16	@ tmp279, ivtmp.61,
	lsr	r1, r1, #16	@ tmp263, tmp262,
@ armwave.c:84:         for(yy = 0; yy < height; yy += 4) {
	.loc 1 84 9 view .LVU276
	add	r9, r9, lr	@ tmp302, ivtmp.59, ivtmp.66
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 19 view .LVU277
	cmp	r6, #0	@ tmp244,
@ armwave.c:102:                     ((((yy + ys) * g_armwave_state.cmp_x_bitdepth_scale) >> AM_XCOORD_MULT_SHIFT) * g_armwave_state.bitdepth_height);
	.loc 1 102 74 view .LVU278
	lsr	ip, r10, #16	@ tmp294, ivtmp.60,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU279
	mla	r1, fp, r1, r7	@ tmp265, _21, tmp263, _22
	add	r10, r10, r4	@ ivtmp.60, ivtmp.60, _208
.LVL46:
	.loc 1 106 47 view .LVU280
	mla	r2, fp, r2, r7	@ tmp281, _21, tmp279, _22
@ armwave.c:107:                 word >>= 8;
	.loc 1 107 22 view .LVU281
	lsr	r4, r0, #8	@ word, word,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU282
	lsl	r3, r3, #1	@ tmp250, tmp249,
@ armwave.c:84:         for(yy = 0; yy < height; yy += 4) {
	.loc 1 84 9 view .LVU283
	str	r9, [sp, #32]	@ tmp302, %sfp
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 19 view .LVU284
	bne	.L53		@,
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 20 view .LVU285
	ands	r4, r4, #255	@ scale_value, word,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU286
	ldrh	r6, [r8, r3]	@ *_86, *_86
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 20 view .LVU287
	moveq	r9, #1	@ tmp256,
	movne	r9, #0	@ tmp256,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU288
	add	r1, r1, r4	@ tmp266, tmp265, scale_value
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 19 view .LVU289
	cmp	r4, #255	@ scale_value,
	movne	r4, r9	@, tmp261, tmp256
	orreq	r4, r9, #1	@,, tmp261, tmp256
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU290
	add	r6, r6, #1	@ tmp253, *_86,
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 19 view .LVU291
	cmp	r4, #0	@ tmp261,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU292
	lsl	r1, r1, #1	@ tmp267, tmp266,
	strh	r6, [r8, r3]	@ movhi	@ tmp253, *_86
	.loc 1 107 17 is_stmt 1 view .LVU293
.LVL47:
	.loc 1 91 17 view .LVU294
	.loc 1 94 17 view .LVU295
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 19 is_stmt 0 view .LVU296
	bne	.L53		@,
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 20 view .LVU297
	lsr	r3, r0, #16	@ word, word,
	ands	lr, r3, #255	@ scale_value, word,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU298
	ldrh	r3, [r8, r1]	@ *_115, *_115
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 20 view .LVU299
	moveq	r4, #1	@ tmp273,
	movne	r4, #0	@ tmp273,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU300
	add	r2, r2, lr	@ tmp282, tmp281, scale_value
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 19 view .LVU301
	cmp	lr, #255	@ scale_value,
	movne	lr, r4	@, tmp278, tmp273
	orreq	lr, r4, #1	@,, tmp278, tmp273
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU302
	add	r3, r3, #1	@ tmp270, *_115,
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 19 view .LVU303
	cmp	lr, #0	@ tmp278,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU304
	lsl	r2, r2, #1	@ tmp283, tmp282,
	strh	r3, [r8, r1]	@ movhi	@ tmp270, *_115
	.loc 1 107 17 is_stmt 1 view .LVU305
.LVL48:
	.loc 1 91 17 view .LVU306
	.loc 1 94 17 view .LVU307
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 19 is_stmt 0 view .LVU308
	bne	.L53		@,
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 20 view .LVU309
	lsrs	r0, r0, #24	@ word, word,
.LVL49:
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU310
	mla	ip, fp, ip, r0	@ tmp296, _21, tmp294, word
	ldrh	r3, [r8, r2]	@ *_144, *_144
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 20 view .LVU311
	moveq	r1, #1	@ tmp288,
	movne	r1, #0	@ tmp288,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU312
	add	ip, ip, r7	@ tmp297, tmp296, _22
@ armwave.c:94:                 if(COND_UNLIKELY(scale_value == 0x00 || scale_value == 0xff))
	.loc 1 94 19 view .LVU313
	cmp	r0, #255	@ word,
	movne	r0, r1	@, tmp293, tmp288
	orreq	r0, r1, #1	@,, tmp293, tmp288
	cmp	r0, #0	@ tmp293,
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 view .LVU314
	lsl	ip, ip, #1	@ tmp298, tmp297,
	add	r3, r3, #1	@ tmp286, *_144,
	strh	r3, [r8, r2]	@ movhi	@ tmp286, *_144
	.loc 1 107 17 is_stmt 1 view .LVU315
.LVL50:
	.loc 1 91 17 view .LVU316
	.loc 1 94 17 view .LVU317
@ armwave.c:106:                 *(write_buffer + scale_value) += 1;
	.loc 1 106 47 is_stmt 0 view .LVU318
	ldrheq	r3, [r8, ip]	@ *_173, *_173
	addeq	r3, r3, #1	@ tmp301, *_173,
	strheq	r3, [r8, ip]	@ movhi	@ tmp301, *_173
	.loc 1 107 17 is_stmt 1 view .LVU319
.LVL51:
.L53:
@ armwave.c:84:         for(yy = 0; yy < height; yy += 4) {
	.loc 1 84 9 is_stmt 0 view .LVU320
	ldrd	r2, [sp, #32]	@,,
	cmp	r3, r2	@ _133, tmp302
	ldr	r3, [sp, #28]	@ _208, %sfp
	add	r5, r5, r3	@ ivtmp.61, ivtmp.61, _208
	bhi	.L47		@,
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 44 view .LVU321
	ldr	r3, [sp, #12]	@ w, %sfp
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 5 view .LVU322
	ldr	r2, [sp, #48]	@ _38, %sfp
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 44 view .LVU323
	add	r3, r3, #1	@ w, w,
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 5 view .LVU324
	cmp	r2, r3	@ _38, w
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 44 view .LVU325
	str	r3, [sp, #12]	@ w, %sfp
.LVL52:
	.loc 1 79 44 view .LVU326
	ldr	r2, [sp, #4]	@ ivtmp.66, %sfp
	ldr	r3, [sp, #44]	@ _24, %sfp
.LVL53:
	.loc 1 79 44 view .LVU327
	sub	r2, r2, r3	@ ivtmp.66, ivtmp.66, _24
	str	r2, [sp, #4]	@ ivtmp.66, %sfp
	ldr	r2, [sp]	@ ivtmp.67, %sfp
	add	r3, r2, r3	@ ivtmp.67, ivtmp.67, _24
	str	r3, [sp]	@ ivtmp.67, %sfp
@ armwave.c:79:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 79 5 view .LVU328
	bne	.L45		@,
.LVL54:
	.loc 1 79 5 view .LVU329
.LBE12:
.LBE13:
	.loc 1 223 9 is_stmt 1 view .LVU330
	.loc 1 224 9 view .LVU331
@ armwave.c:218:     for(yy = 0; yy < (g_armwave_state.wave_length / g_armwave_state.slice_height); yy++) {
	.loc 1 218 86 is_stmt 0 view .LVU332
	ldr	r3, [sp, #40]	@ yy, %sfp
@ armwave.c:218:     for(yy = 0; yy < (g_armwave_state.wave_length / g_armwave_state.slice_height); yy++) {
	.loc 1 218 5 view .LVU333
	ldr	r2, [sp, #64]	@ _179, %sfp
@ armwave.c:218:     for(yy = 0; yy < (g_armwave_state.wave_length / g_armwave_state.slice_height); yy++) {
	.loc 1 218 86 view .LVU334
	add	r3, r3, #1	@ yy, yy,
@ armwave.c:218:     for(yy = 0; yy < (g_armwave_state.wave_length / g_armwave_state.slice_height); yy++) {
	.loc 1 218 5 view .LVU335
	cmp	r3, r2	@ yy, _179
@ armwave.c:218:     for(yy = 0; yy < (g_armwave_state.wave_length / g_armwave_state.slice_height); yy++) {
	.loc 1 218 86 view .LVU336
	str	r3, [sp, #40]	@ yy, %sfp
.LVL55:
	.loc 1 218 86 view .LVU337
	ldr	r2, [sp, #68]	@ _185, %sfp
	ldr	r3, [sp, #24]	@ ivtmp.70, %sfp
.LVL56:
	.loc 1 218 86 view .LVU338
	add	r3, r3, r2	@ ivtmp.70, ivtmp.70, _185
	str	r3, [sp, #24]	@ ivtmp.70, %sfp
	ldr	r2, [sp, #20]	@ ivtmp.71, %sfp
	ldr	r3, [sp, #36]	@ _133, %sfp
	sub	r2, r2, r3	@ ivtmp.71, ivtmp.71, _133
	str	r2, [sp, #20]	@ ivtmp.71, %sfp
	ldr	r2, [sp, #16]	@ ivtmp.72, %sfp
	add	r3, r2, r3	@ ivtmp.72, ivtmp.72, _133
	str	r3, [sp, #16]	@ ivtmp.72, %sfp
@ armwave.c:218:     for(yy = 0; yy < (g_armwave_state.wave_length / g_armwave_state.slice_height); yy++) {
	.loc 1 218 5 view .LVU339
	bne	.L48		@,
.LVL57:
.L42:
@ armwave.c:228: }
	.loc 1 228 1 view .LVU340
	add	sp, sp, #76	@,,
	.cfi_def_cfa_offset 36
	@ sp needed	@
	pop	{r4, r5, r6, r7, r8, r9, r10, fp, pc}	@
.L63:
	.align	2
.L62:
	.word	g_armwave_state
	.cfi_endproc
.LFE61:
	.size	armwave_generate, .-armwave_generate
	.align	2
	.global	armwave_setup_render
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_setup_render, %function
armwave_setup_render:
.LVL58:
.LFB62:
	.loc 1 234 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 12, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 235 5 view .LVU342
	.loc 1 236 5 view .LVU343
	.loc 1 238 5 view .LVU344
@ armwave.c:234: {
	.loc 1 234 1 is_stmt 0 view .LVU345
	push	{r4, r5, r6, r7, r8, r9, lr}	@
	.cfi_def_cfa_offset 28
	.cfi_offset 4, -28
	.cfi_offset 5, -24
	.cfi_offset 6, -20
	.cfi_offset 7, -16
	.cfi_offset 8, -12
	.cfi_offset 9, -8
	.cfi_offset 14, -4
	vpush.64	{d8}	@
	.cfi_def_cfa_offset 36
	.cfi_offset 80, -36
	.cfi_offset 81, -32
	mov	r4, r0	@ start_point, start_point
	mov	r5, r1	@ end_point, end_point
	mov	r8, r2	@ waves_max, waves_max
	sub	sp, sp, #20	@,,
	.cfi_def_cfa_offset 56
@ armwave.c:234: {
	.loc 1 234 1 view .LVU346
	mov	r9, r3	@ wave_stride, wave_stride
	ldrd	r6, [sp, #56]	@,,
	ldr	ip, [sp, #64]	@ render_flags, render_flags
@ armwave.c:238:     printf("s=%d e=%d w=%d ws=%d tw=%d th=%d rf=0x%08x\n", start_point, end_point, waves_max, wave_stride, target_width, target_height, render_flags);
	.loc 1 238 5 view .LVU347
	str	r3, [sp]	@ wave_stride,
	strd	r6, [sp, #4]	@,,
	mov	r3, r2	@, waves_max
.LVL59:
	.loc 1 238 5 view .LVU348
	str	ip, [sp, #12]	@ render_flags,
	mov	r2, r1	@, end_point
.LVL60:
	.loc 1 238 5 view .LVU349
	mov	r1, r0	@, start_point
.LVL61:
	.loc 1 238 5 view .LVU350
	ldr	r0, .L78+12	@,
.LVL62:
	.loc 1 238 5 view .LVU351
	bl	printf		@
.LVL63:
	.loc 1 241 5 is_stmt 1 view .LVU352
	cmp	r4, r5	@ start_point, end_point
	bcs	.L75		@,
	.loc 1 262 5 view .LVU353
@ armwave.c:273:     g_armwave_state.wave_length = end_point - start_point;
	.loc 1 273 45 is_stmt 0 view .LVU354
	sub	r5, r5, r4	@ _13, end_point, start_point
.LVL64:
@ armwave.c:263:     g_armwave_state.vscale_frac = target_height / 255.0f;
	.loc 1 263 49 view .LVU355
	vldr.32	s12, .L78	@ tmp174,
@ armwave.c:277:         ((float)(g_armwave_state.target_width) / g_armwave_state.wave_length) * (1 << AM_XCOORD_MULT_SHIFT);
	.loc 1 277 48 view .LVU356
	vmov	s15, r5	@ int	@ _13, _13
@ armwave.c:277:         ((float)(g_armwave_state.target_width) / g_armwave_state.wave_length) * (1 << AM_XCOORD_MULT_SHIFT);
	.loc 1 277 79 view .LVU357
	vldr.32	s13, .L78+4	@ tmp198,
@ armwave.c:270:     g_armwave_state.ch_buff_size = (g_armwave_state.bitdepth_height + 4) * (target_width + 4) * sizeof(bufftyp_t);  // Add word padding too
	.loc 1 270 90 view .LVU358
	add	r3, r6, #4	@ tmp185, target_width,
@ armwave.c:262:     g_armwave_state.xstride = target_height;
	.loc 1 262 29 view .LVU359
	ldr	r4, .L78+16	@ tmp242,
.LVL65:
@ armwave.c:277:         ((float)(g_armwave_state.target_width) / g_armwave_state.wave_length) * (1 << AM_XCOORD_MULT_SHIFT);
	.loc 1 277 48 view .LVU360
	vcvt.f32.s32	s10, s15	@ tmp195, _13
@ armwave.c:263:     g_armwave_state.vscale_frac = target_height / 255.0f;
	.loc 1 263 49 view .LVU361
	vmov	s15, r7	@ int	@ target_height, target_height
@ armwave.c:268:     g_armwave_state.size = target_height * target_width;
	.loc 1 268 42 view .LVU362
	mul	r1, r7, r6	@ tmp182, target_height, target_width
@ armwave.c:270:     g_armwave_state.ch_buff_size = (g_armwave_state.bitdepth_height + 4) * (target_width + 4) * sizeof(bufftyp_t);  // Add word padding too
	.loc 1 270 95 view .LVU363
	add	r3, r3, r3, lsl #7	@ tmp188, tmp185, tmp185,
@ armwave.c:263:     g_armwave_state.vscale_frac = target_height / 255.0f;
	.loc 1 263 49 view .LVU364
	vcvt.f32.u32	s11, s15	@ tmp173, target_height
@ armwave.c:277:         ((float)(g_armwave_state.target_width) / g_armwave_state.wave_length) * (1 << AM_XCOORD_MULT_SHIFT);
	.loc 1 277 48 view .LVU365
	vmov	s15, r6	@ int	@ target_width, target_width
@ armwave.c:279:     printf("ch_buff_size=%d, cmp_x_bitdepth_scale=%d (0x%08x), targ_width=%d, wave_length=%d, scaler=%d\n", \
	.loc 1 279 5 view .LVU366
	mov	r2, #65536	@ tmp201,
	str	r5, [sp, #4]	@ _13,
	str	r6, [sp]	@ target_width,
@ armwave.c:277:         ((float)(g_armwave_state.target_width) / g_armwave_state.wave_length) * (1 << AM_XCOORD_MULT_SHIFT);
	.loc 1 277 48 view .LVU367
	vcvt.f32.s32	s14, s15	@ tmp194, target_width
@ armwave.c:270:     g_armwave_state.ch_buff_size = (g_armwave_state.bitdepth_height + 4) * (target_width + 4) * sizeof(bufftyp_t);  // Add word padding too
	.loc 1 270 95 view .LVU368
	lsl	r3, r3, #3	@ tmp189, tmp188,
@ armwave.c:262:     g_armwave_state.xstride = target_height;
	.loc 1 262 29 view .LVU369
	str	r7, [r4, #44]	@ target_height, g_armwave_state.xstride
	.loc 1 263 5 is_stmt 1 view .LVU370
@ armwave.c:265:     g_armwave_state.wave_stride = wave_stride;
	.loc 1 265 33 is_stmt 0 view .LVU371
	str	r9, [r4, #52]	@ wave_stride, g_armwave_state.wave_stride
@ armwave.c:266:     g_armwave_state.waves_max = waves_max;
	.loc 1 266 31 view .LVU372
	str	r8, [r4, #60]	@ waves_max, g_armwave_state.waves_max
@ armwave.c:267:     g_armwave_state.waves = waves_max;  // Need a function to be able to change this on the fly
	.loc 1 267 27 view .LVU373
	str	r8, [r4, #56]	@ waves_max, g_armwave_state.waves
@ armwave.c:272:     g_armwave_state.target_height = target_height;
	.loc 1 272 35 view .LVU374
	strd	r6, [r4, #88]	@, tmp242,
@ armwave.c:279:     printf("ch_buff_size=%d, cmp_x_bitdepth_scale=%d (0x%08x), targ_width=%d, wave_length=%d, scaler=%d\n", \
	.loc 1 279 5 view .LVU375
	str	r2, [sp, #8]	@ tmp201,
@ armwave.c:277:         ((float)(g_armwave_state.target_width) / g_armwave_state.wave_length) * (1 << AM_XCOORD_MULT_SHIFT);
	.loc 1 277 48 view .LVU376
	vdiv.f32	s15, s14, s10	@ tmp196, tmp194, tmp195
@ armwave.c:269:     g_armwave_state.bitdepth_height = 256 * sizeof(bufftyp_t);  // Always 256 possible levels in 8-bit mode
	.loc 1 269 37 view .LVU377
	mov	r2, #512	@ tmp184,
@ armwave.c:268:     g_armwave_state.size = target_height * target_width;
	.loc 1 268 26 view .LVU378
	str	r1, [r4, #84]	@ tmp182, g_armwave_state.size
@ armwave.c:269:     g_armwave_state.bitdepth_height = 256 * sizeof(bufftyp_t);  // Always 256 possible levels in 8-bit mode
	.loc 1 269 37 view .LVU379
	str	r2, [r4, #64]	@ tmp184, g_armwave_state.bitdepth_height
@ armwave.c:279:     printf("ch_buff_size=%d, cmp_x_bitdepth_scale=%d (0x%08x), targ_width=%d, wave_length=%d, scaler=%d\n", \
	.loc 1 279 5 view .LVU380
	mov	r1, r3	@, tmp189
@ armwave.c:270:     g_armwave_state.ch_buff_size = (g_armwave_state.bitdepth_height + 4) * (target_width + 4) * sizeof(bufftyp_t);  // Add word padding too
	.loc 1 270 34 view .LVU381
	str	r3, [r4, #80]	@ tmp189, g_armwave_state.ch_buff_size
@ armwave.c:273:     g_armwave_state.wave_length = end_point - start_point;
	.loc 1 273 33 view .LVU382
	str	r5, [r4, #76]	@ _13, g_armwave_state.wave_length
@ armwave.c:279:     printf("ch_buff_size=%d, cmp_x_bitdepth_scale=%d (0x%08x), targ_width=%d, wave_length=%d, scaler=%d\n", \
	.loc 1 279 5 view .LVU383
	ldr	r0, .L78+20	@,
@ armwave.c:263:     g_armwave_state.vscale_frac = target_height / 255.0f;
	.loc 1 263 49 view .LVU384
	vdiv.f32	s14, s11, s12	@ _3, tmp173, tmp174
@ armwave.c:277:         ((float)(g_armwave_state.target_width) / g_armwave_state.wave_length) * (1 << AM_XCOORD_MULT_SHIFT);
	.loc 1 277 79 view .LVU385
	vmul.f32	s15, s15, s13	@ tmp197, tmp196, tmp198
@ armwave.c:276:     g_armwave_state.cmp_x_bitdepth_scale = \
	.loc 1 276 42 view .LVU386
	vcvt.u32.f32	s15, s15	@ _19, tmp197
@ armwave.c:263:     g_armwave_state.vscale_frac = target_height / 255.0f;
	.loc 1 263 33 view .LVU387
	vstr.32	s14, [r4, #40]	@ _3, g_armwave_state.vscale_frac
	.loc 1 264 5 is_stmt 1 view .LVU388
@ armwave.c:276:     g_armwave_state.cmp_x_bitdepth_scale = \
	.loc 1 276 42 is_stmt 0 view .LVU389
	vmov	ip, s15	@ int	@ _19, _19
@ armwave.c:264:     g_armwave_state.vscale = (int)g_armwave_state.vscale_frac;
	.loc 1 264 30 view .LVU390
	vcvt.s32.f32	s15, s14	@ tmp177, _3
@ armwave.c:279:     printf("ch_buff_size=%d, cmp_x_bitdepth_scale=%d (0x%08x), targ_width=%d, wave_length=%d, scaler=%d\n", \
	.loc 1 279 5 view .LVU391
	mov	r3, ip	@, _19
	mov	r2, ip	@, tmp3
@ armwave.c:276:     g_armwave_state.cmp_x_bitdepth_scale = \
	.loc 1 276 42 view .LVU392
	str	ip, [r4, #36]	@ _19, g_armwave_state.cmp_x_bitdepth_scale
@ armwave.c:264:     g_armwave_state.vscale = (int)g_armwave_state.vscale_frac;
	.loc 1 264 30 view .LVU393
	vstr.32	s15, [r4, #48]	@ int	@ tmp177, g_armwave_state.vscale
	.loc 1 265 5 is_stmt 1 view .LVU394
	.loc 1 266 5 view .LVU395
	.loc 1 267 5 view .LVU396
	.loc 1 268 5 view .LVU397
	.loc 1 269 5 view .LVU398
	.loc 1 270 5 view .LVU399
	.loc 1 271 5 view .LVU400
	.loc 1 272 5 view .LVU401
	.loc 1 273 5 view .LVU402
	.loc 1 276 5 view .LVU403
	.loc 1 279 5 view .LVU404
	bl	printf		@
.LVL66:
	.loc 1 287 5 view .LVU405
@ armwave.c:289:     if(g_armwave_state.ch1_buffer != NULL)
	.loc 1 289 23 is_stmt 0 view .LVU406
	ldr	r0, [r4, #4]	@ _20, g_armwave_state.ch1_buffer
@ armwave.c:287:     g_armwave_state.slice_height = 256; // 64;  
	.loc 1 287 34 view .LVU407
	mov	r3, #256	@ tmp203,
@ armwave.c:289:     if(g_armwave_state.ch1_buffer != NULL)
	.loc 1 289 7 view .LVU408
	cmp	r0, #0	@ _20,
@ armwave.c:287:     g_armwave_state.slice_height = 256; // 64;  
	.loc 1 287 34 view .LVU409
	str	r3, [r4, #68]	@ tmp203, g_armwave_state.slice_height
	.loc 1 289 5 is_stmt 1 view .LVU410
@ armwave.c:289:     if(g_armwave_state.ch1_buffer != NULL)
	.loc 1 289 7 is_stmt 0 view .LVU411
	beq	.L66		@,
	.loc 1 290 9 is_stmt 1 view .LVU412
	bl	free		@
.LVL67:
.L66:
	.loc 1 292 5 view .LVU413
@ armwave.c:292:     g_armwave_state.ch1_buffer = calloc(g_armwave_state.ch_buff_size, 1);
	.loc 1 292 34 is_stmt 0 view .LVU414
	mov	r1, #1	@,
	ldr	r0, [r4, #80]	@, g_armwave_state.ch_buff_size
	bl	calloc		@
.LVL68:
@ armwave.c:294:     assert(g_armwave_state.ch1_buffer != NULL);
	.loc 1 294 5 view .LVU415
	cmp	r0, #0	@ _23,
@ armwave.c:292:     g_armwave_state.ch1_buffer = calloc(g_armwave_state.ch_buff_size, 1);
	.loc 1 292 34 view .LVU416
	mov	r7, r0	@ _23,
@ armwave.c:292:     g_armwave_state.ch1_buffer = calloc(g_armwave_state.ch_buff_size, 1);
	.loc 1 292 32 view .LVU417
	str	r0, [r4, #4]	@ _23, g_armwave_state.ch1_buffer
	.loc 1 294 5 is_stmt 1 view .LVU418
	beq	.L76		@,
	.loc 1 297 5 view .LVU419
.LVL69:
	.loc 1 298 5 view .LVU420
@ armwave.c:298:     points_per_pixel = length / ((float)(target_width));
	.loc 1 298 34 is_stmt 0 view .LVU421
	vmov	s15, r6	@ int	@ target_width, target_width
@ armwave.c:298:     points_per_pixel = length / ((float)(target_width));
	.loc 1 298 22 view .LVU422
	vmov	s13, r5	@ int	@ _13, _13
@ armwave.c:299:     g_armwave_state.slice_record_height = points_per_pixel * g_armwave_state.slice_height;
	.loc 1 299 60 view .LVU423
	vldr.32	s14, [r4, #68]	@ int	@ tmp253, g_armwave_state.slice_height
@ armwave.c:300:     g_armwave_state.xcoord_to_xpixel = malloc(length * sizeof(uint16_t));
	.loc 1 300 40 view .LVU424
	lsl	r0, r5, #1	@, _13,
@ armwave.c:298:     points_per_pixel = length / ((float)(target_width));
	.loc 1 298 34 view .LVU425
	vcvt.f32.u32	s15, s15	@ tmp215, target_width
@ armwave.c:298:     points_per_pixel = length / ((float)(target_width));
	.loc 1 298 22 view .LVU426
	vcvt.f32.u32	s13, s13	@ tmp214, _13
@ armwave.c:299:     g_armwave_state.slice_record_height = points_per_pixel * g_armwave_state.slice_height;
	.loc 1 299 60 view .LVU427
	vcvt.f32.s32	s14, s14	@ tmp218, tmp253
@ armwave.c:298:     points_per_pixel = length / ((float)(target_width));
	.loc 1 298 22 view .LVU428
	vdiv.f32	s16, s13, s15	@ points_per_pixel, tmp214, tmp215
.LVL70:
	.loc 1 299 5 is_stmt 1 view .LVU429
@ armwave.c:299:     g_armwave_state.slice_record_height = points_per_pixel * g_armwave_state.slice_height;
	.loc 1 299 60 is_stmt 0 view .LVU430
	vmul.f32	s15, s14, s16	@ tmp220, tmp218, points_per_pixel
@ armwave.c:299:     g_armwave_state.slice_record_height = points_per_pixel * g_armwave_state.slice_height;
	.loc 1 299 41 view .LVU431
	vcvt.s32.f32	s15, s15	@ tmp221, tmp220
	vstr.32	s15, [r4, #72]	@ int	@ tmp221, g_armwave_state.slice_record_height
	.loc 1 300 5 is_stmt 1 view .LVU432
@ armwave.c:300:     g_armwave_state.xcoord_to_xpixel = malloc(length * sizeof(uint16_t));
	.loc 1 300 40 is_stmt 0 view .LVU433
	bl	malloc		@
.LVL71:
@ armwave.c:302:     assert(g_armwave_state.xcoord_to_xpixel != NULL);
	.loc 1 302 5 view .LVU434
	cmp	r0, #0	@ _31,
@ armwave.c:300:     g_armwave_state.xcoord_to_xpixel = malloc(length * sizeof(uint16_t));
	.loc 1 300 40 view .LVU435
	mov	r6, r0	@ _31,
@ armwave.c:300:     g_armwave_state.xcoord_to_xpixel = malloc(length * sizeof(uint16_t));
	.loc 1 300 38 view .LVU436
	str	r0, [r4, #132]	@ _31, g_armwave_state.xcoord_to_xpixel
	.loc 1 302 5 is_stmt 1 view .LVU437
	beq	.L77		@,
@ armwave.c:305:         g_armwave_state.xcoord_to_xpixel[xx] = (1.0f / points_per_pixel) * xx;
	.loc 1 305 54 is_stmt 0 view .LVU438
	vldr.32	s15, .L78+8	@ tmp230,
	sub	r0, r0, #2	@ ivtmp.76, _31,
@ armwave.c:304:     for(xx = 0; xx < length; xx++) {
	.loc 1 304 12 view .LVU439
	mov	r3, #0	@ xx,
@ armwave.c:305:         g_armwave_state.xcoord_to_xpixel[xx] = (1.0f / points_per_pixel) * xx;
	.loc 1 305 54 view .LVU440
	vdiv.f32	s14, s15, s16	@ _32, tmp230, points_per_pixel
.L69:
.LVL72:
	.loc 1 305 9 is_stmt 1 discriminator 3 view .LVU441
@ armwave.c:305:         g_armwave_state.xcoord_to_xpixel[xx] = (1.0f / points_per_pixel) * xx;
	.loc 1 305 74 is_stmt 0 discriminator 3 view .LVU442
	vmov	s15, r3	@ int	@ xx, xx
@ armwave.c:304:     for(xx = 0; xx < length; xx++) {
	.loc 1 304 32 discriminator 3 view .LVU443
	add	r3, r3, #1	@ xx, xx,
.LVL73:
@ armwave.c:304:     for(xx = 0; xx < length; xx++) {
	.loc 1 304 5 discriminator 3 view .LVU444
	cmp	r5, r3	@ _13, xx
@ armwave.c:305:         g_armwave_state.xcoord_to_xpixel[xx] = (1.0f / points_per_pixel) * xx;
	.loc 1 305 74 discriminator 3 view .LVU445
	vcvt.f32.s32	s15, s15	@ tmp231, xx
	vmul.f32	s15, s15, s14	@ tmp232, tmp231, _32
@ armwave.c:305:         g_armwave_state.xcoord_to_xpixel[xx] = (1.0f / points_per_pixel) * xx;
	.loc 1 305 46 discriminator 3 view .LVU446
	vcvt.u32.f32	s15, s15	@ tmp233, tmp232
	vmov	r2, s15	@ int	@ tmp233, tmp233
	strh	r2, [r0, #2]!	@ movhi	@ tmp233, MEM[base: _101, offset: 0B]
@ armwave.c:304:     for(xx = 0; xx < length; xx++) {
	.loc 1 304 5 discriminator 3 view .LVU447
	bne	.L69		@,
	.loc 1 310 5 is_stmt 1 view .LVU448
@ armwave.c:310:     g_armwave_state.out_pixbuf = malloc(sizeof(uint32_t) * g_armwave_state.size);
	.loc 1 310 34 is_stmt 0 view .LVU449
	ldr	r0, [r4, #84]	@ g_armwave_state.size, g_armwave_state.size
	lsl	r0, r0, #2	@, g_armwave_state.size,
	bl	malloc		@
.LVL74:
@ armwave.c:312:     printf("Ptrs: 0x%08x 0x%08x 0x%08x 0x%08x \n", \
	.loc 1 312 5 view .LVU450
	ldr	r3, [r4, #24]	@ g_armwave_state.test_wave_buffer, g_armwave_state.test_wave_buffer
	mov	r2, r6	@, _31
	str	r3, [sp]	@ g_armwave_state.test_wave_buffer,
	mov	r1, r7	@, _23
@ armwave.c:310:     g_armwave_state.out_pixbuf = malloc(sizeof(uint32_t) * g_armwave_state.size);
	.loc 1 310 34 view .LVU451
	mov	ip, r0	@ tmp237,
@ armwave.c:312:     printf("Ptrs: 0x%08x 0x%08x 0x%08x 0x%08x \n", \
	.loc 1 312 5 view .LVU452
	mov	r3, r0	@, tmp237
@ armwave.c:310:     g_armwave_state.out_pixbuf = malloc(sizeof(uint32_t) * g_armwave_state.size);
	.loc 1 310 32 view .LVU453
	str	ip, [r4, #104]	@ tmp237, g_armwave_state.out_pixbuf
	.loc 1 312 5 is_stmt 1 view .LVU454
	ldr	r0, .L78+24	@,
	bl	printf		@
.LVL75:
	.loc 1 318 5 view .LVU455
@ armwave.c:319: }
	.loc 1 319 1 is_stmt 0 view .LVU456
	add	sp, sp, #20	@,,
	.cfi_remember_state
	.cfi_def_cfa_offset 36
	@ sp needed	@
	vldm	sp!, {d8}	@
	.cfi_restore 80
	.cfi_restore 81
	.cfi_def_cfa_offset 28
.LVL76:
	.loc 1 319 1 view .LVU457
	pop	{r4, r5, r6, r7, r8, r9, lr}	@
	.cfi_restore 14
	.cfi_restore 9
	.cfi_restore 8
	.cfi_restore 7
	.cfi_restore 6
	.cfi_restore 5
	.cfi_restore 4
	.cfi_def_cfa_offset 0
.LVL77:
@ armwave.c:318:     malloc_stats();
	.loc 1 318 5 view .LVU458
	b	malloc_stats		@
.LVL78:
.L75:
	.cfi_restore_state
	.loc 1 241 5 is_stmt 1 discriminator 1 view .LVU459
	ldr	r3, .L78+28	@,
	mov	r2, #241	@,
	ldr	r1, .L78+32	@,
	ldr	r0, .L78+36	@,
	bl	__assert_fail		@
.LVL79:
.L77:
	.loc 1 302 5 discriminator 1 view .LVU460
	ldr	r3, .L78+28	@,
	ldr	r2, .L78+40	@,
	ldr	r1, .L78+32	@,
	ldr	r0, .L78+44	@,
	bl	__assert_fail		@
.LVL80:
.L76:
	.loc 1 294 5 discriminator 1 view .LVU461
	ldr	r3, .L78+28	@,
	ldr	r2, .L78+48	@,
	ldr	r1, .L78+32	@,
	ldr	r0, .L78+52	@,
	bl	__assert_fail		@
.LVL81:
.L79:
	.align	2
.L78:
	.word	1132396544
	.word	1199570944
	.word	1065353216
	.word	.LC4
	.word	g_armwave_state
	.word	.LC6
	.word	.LC9
	.word	.LANCHOR0+28
	.word	.LC2
	.word	.LC5
	.word	302
	.word	.LC8
	.word	294
	.word	.LC7
	.cfi_endproc
.LFE62:
	.size	armwave_setup_render, .-armwave_setup_render
	.align	2
	.global	armwave_set_wave_pointer
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_set_wave_pointer, %function
armwave_set_wave_pointer:
.LVL82:
.LFB63:
	.loc 1 326 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 327 5 view .LVU463
	cmp	r0, #0	@ wave_buffer
	.loc 1 327 5 is_stmt 0 view .LVU464
	beq	.L85		@,
	.loc 1 328 5 is_stmt 1 view .LVU465
@ armwave.c:328:     g_armwave_state.wave_buffer = wave_buffer;
	.loc 1 328 33 is_stmt 0 view .LVU466
	ldr	r3, .L86	@ tmp116,
	str	r0, [r3, #20]	@ wave_buffer, g_armwave_state.wave_buffer
	bx	lr	@
.L85:
	.loc 1 327 5 is_stmt 1 discriminator 1 view .LVU467
@ armwave.c:326: {
	.loc 1 326 1 is_stmt 0 discriminator 1 view .LVU468
	push	{r4, lr}	@
	.cfi_def_cfa_offset 8
	.cfi_offset 4, -8
	.cfi_offset 14, -4
@ armwave.c:327:     assert(wave_buffer != NULL);
	.loc 1 327 5 discriminator 1 view .LVU469
	ldr	r3, .L86+4	@,
	ldr	r2, .L86+8	@,
	ldr	r1, .L86+12	@,
	ldr	r0, .L86+16	@,
.LVL83:
	.loc 1 327 5 discriminator 1 view .LVU470
	bl	__assert_fail		@
.LVL84:
.L87:
	.align	2
.L86:
	.word	g_armwave_state
	.word	.LANCHOR0+52
	.word	327
	.word	.LC2
	.word	.LC10
	.cfi_endproc
.LFE63:
	.size	armwave_set_wave_pointer, .-armwave_set_wave_pointer
	.align	2
	.global	armwave_set_wave_pointer_as_testbuf
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_set_wave_pointer_as_testbuf, %function
armwave_set_wave_pointer_as_testbuf:
.LVL85:
.LFB64:
	.loc 1 336 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	.loc 1 337 5 view .LVU472
@ armwave.c:337:     if(set > g_armwave_state.test_wave_buffer_nsets) {
	.loc 1 337 29 is_stmt 0 view .LVU473
	ldr	r3, .L91	@ tmp117,
@ armwave.c:337:     if(set > g_armwave_state.test_wave_buffer_nsets) {
	.loc 1 337 7 view .LVU474
	ldr	r2, [r3, #32]	@ g_armwave_state.test_wave_buffer_nsets, g_armwave_state.test_wave_buffer_nsets
	cmp	r2, r0	@ g_armwave_state.test_wave_buffer_nsets, set
	bcc	.L90		@,
	.loc 1 342 5 is_stmt 1 view .LVU475
@ armwave.c:342:     g_armwave_state.wave_buffer = g_armwave_state.test_wave_buffer + (g_armwave_state.test_wave_buffer_stride * set);
	.loc 1 342 111 is_stmt 0 view .LVU476
	ldr	r1, [r3, #28]	@ g_armwave_state.test_wave_buffer_stride, g_armwave_state.test_wave_buffer_stride
@ armwave.c:342:     g_armwave_state.wave_buffer = g_armwave_state.test_wave_buffer + (g_armwave_state.test_wave_buffer_stride * set);
	.loc 1 342 68 view .LVU477
	ldr	r2, [r3, #24]	@ g_armwave_state.test_wave_buffer, g_armwave_state.test_wave_buffer
	mla	r0, r1, r0, r2	@ tmp125, g_armwave_state.test_wave_buffer_stride, set, g_armwave_state.test_wave_buffer
.LVL86:
@ armwave.c:342:     g_armwave_state.wave_buffer = g_armwave_state.test_wave_buffer + (g_armwave_state.test_wave_buffer_stride * set);
	.loc 1 342 33 view .LVU478
	str	r0, [r3, #20]	@ tmp125, g_armwave_state.wave_buffer
@ armwave.c:343: }
	.loc 1 343 1 view .LVU479
	bx	lr	@
.LVL87:
.L90:
	.loc 1 338 9 is_stmt 1 view .LVU480
	ldr	r0, .L91+4	@,
.LVL88:
	.loc 1 338 9 is_stmt 0 view .LVU481
	b	puts		@
.LVL89:
.L92:
	.align	2
.L91:
	.word	g_armwave_state
	.word	.LC11
	.cfi_endproc
.LFE64:
	.size	armwave_set_wave_pointer_as_testbuf, .-armwave_set_wave_pointer_as_testbuf
	.align	2
	.global	armwave_set_wave_pointer_u32
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_set_wave_pointer_u32, %function
armwave_set_wave_pointer_u32:
.LVL90:
.LFB65:
	.loc 1 350 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 351 5 view .LVU483
	cmp	r0, #0	@ wave_buffer_ptr
	.loc 1 351 5 is_stmt 0 view .LVU484
	beq	.L98		@,
	.loc 1 352 5 is_stmt 1 view .LVU485
@ armwave.c:352:     g_armwave_state.wave_buffer = (uint8_t*)wave_buffer_ptr;
	.loc 1 352 33 is_stmt 0 view .LVU486
	ldr	r3, .L99	@ tmp117,
	str	r0, [r3, #20]	@ wave_buffer_ptr, g_armwave_state.wave_buffer
	bx	lr	@
.L98:
	.loc 1 351 5 is_stmt 1 discriminator 1 view .LVU487
@ armwave.c:350: {
	.loc 1 350 1 is_stmt 0 discriminator 1 view .LVU488
	push	{r4, lr}	@
	.cfi_def_cfa_offset 8
	.cfi_offset 4, -8
	.cfi_offset 14, -4
@ armwave.c:351:     assert(wave_buffer_ptr != 0);
	.loc 1 351 5 discriminator 1 view .LVU489
	ldr	r3, .L99+4	@,
	ldr	r2, .L99+8	@,
	ldr	r1, .L99+12	@,
	ldr	r0, .L99+16	@,
.LVL91:
	.loc 1 351 5 discriminator 1 view .LVU490
	bl	__assert_fail		@
.LVL92:
.L100:
	.align	2
.L99:
	.word	g_armwave_state
	.word	.LANCHOR0+80
	.word	351
	.word	.LC2
	.word	.LC12
	.cfi_endproc
.LFE65:
	.size	armwave_set_wave_pointer_u32, .-armwave_set_wave_pointer_u32
	.align	2
	.global	armwave_clear_buffer
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_clear_buffer, %function
armwave_clear_buffer:
.LVL93:
.LFB66:
	.loc 1 359 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	.loc 1 361 5 view .LVU492
@ armwave.c:361:     memset(g_armwave_state.ch1_buffer, 0, g_armwave_state.ch_buff_size);
	.loc 1 361 58 is_stmt 0 view .LVU493
	ldr	r3, .L102	@ tmp114,
@ armwave.c:361:     memset(g_armwave_state.ch1_buffer, 0, g_armwave_state.ch_buff_size);
	.loc 1 361 5 view .LVU494
	mov	r1, #0	@,
	ldr	r2, [r3, #80]	@, g_armwave_state.ch_buff_size
	ldr	r0, [r3, #4]	@, g_armwave_state.ch1_buffer
.LVL94:
	.loc 1 361 5 view .LVU495
	b	memset		@
.LVL95:
.L103:
	.align	2
.L102:
	.word	g_armwave_state
	.cfi_endproc
.LFE66:
	.size	armwave_clear_buffer, .-armwave_clear_buffer
	.align	2
	.global	armwave_set_channel_colour
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_set_channel_colour, %function
armwave_set_channel_colour:
.LVL96:
.LFB67:
	.loc 1 368 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	.loc 1 370 5 view .LVU497
	cmp	r0, #1	@ ch,
	.loc 1 372 13 view .LVU498
@ armwave.c:372:             g_armwave_state.ch1_color.r = r;
	.loc 1 372 41 is_stmt 0 view .LVU499
	ldreq	r0, .L106	@ tmp117,
.LVL97:
	.loc 1 372 41 view .LVU500
	strheq	r1, [r0, #108]	@ movhi	@ r, g_armwave_state.ch1_color.r
	.loc 1 373 13 is_stmt 1 view .LVU501
@ armwave.c:373:             g_armwave_state.ch1_color.g = g;
	.loc 1 373 41 is_stmt 0 view .LVU502
	strheq	r2, [r0, #110]	@ movhi	@ g, g_armwave_state.ch1_color.g
	.loc 1 374 13 is_stmt 1 view .LVU503
@ armwave.c:374:             g_armwave_state.ch1_color.b = b;
	.loc 1 374 41 is_stmt 0 view .LVU504
	strheq	r3, [r0, #112]	@ movhi	@ b, g_armwave_state.ch1_color.b
	.loc 1 375 13 is_stmt 1 view .LVU505
@ armwave.c:377: }
	.loc 1 377 1 is_stmt 0 view .LVU506
	bx	lr	@
.L107:
	.align	2
.L106:
	.word	g_armwave_state
	.cfi_endproc
.LFE67:
	.size	armwave_set_channel_colour, .-armwave_set_channel_colour
	.align	2
	.global	armwave_dump_ppm_debug
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_dump_ppm_debug, %function
armwave_dump_ppm_debug:
.LVL98:
.LFB68:
	.loc 1 383 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 384 5 view .LVU508
@ armwave.c:383: {
	.loc 1 383 1 is_stmt 0 view .LVU509
	mov	r3, r1	@ fn, fn
	push	{r4, r5, r6, r7, r8, r9, lr}	@
	.cfi_def_cfa_offset 28
	.cfi_offset 4, -28
	.cfi_offset 5, -24
	.cfi_offset 6, -20
	.cfi_offset 7, -16
	.cfi_offset 8, -12
	.cfi_offset 9, -8
	.cfi_offset 14, -4
	mov	r8, r0	@ buffer, buffer
	sub	sp, sp, #12	@,,
	.cfi_def_cfa_offset 40
@ armwave.c:384:     FILE *fp = fopen(fn, "wb");
	.loc 1 384 16 view .LVU510
	ldr	r1, .L116	@,
.LVL99:
	.loc 1 384 16 view .LVU511
	mov	r0, r3	@, fn
.LVL100:
	.loc 1 384 16 view .LVU512
	bl	fopen64		@
.LVL101:
@ armwave.c:391:     fprintf(fp, "%d %d\n", g_armwave_state.target_width, g_armwave_state.target_height);
	.loc 1 391 5 view .LVU513
	ldr	r6, .L116+4	@ tmp150,
@ armwave.c:390:     fputs("P3\n", fp);
	.loc 1 390 5 view .LVU514
	mov	r2, #3	@,
	mov	r1, #1	@,
@ armwave.c:384:     FILE *fp = fopen(fn, "wb");
	.loc 1 384 16 view .LVU515
	mov	r7, r0	@ fp,
.LVL102:
	.loc 1 385 5 is_stmt 1 view .LVU516
	.loc 1 386 5 view .LVU517
	.loc 1 390 5 view .LVU518
	mov	r3, r0	@, fp
	ldr	r0, .L116+8	@,
.LVL103:
	.loc 1 390 5 is_stmt 0 view .LVU519
	bl	fwrite		@
.LVL104:
	.loc 1 391 5 is_stmt 1 view .LVU520
	ldrd	r2, [r6, #88]	@, tmp150,
	ldr	r1, .L116+12	@,
	mov	r0, r7	@, fp
	bl	fprintf		@
.LVL105:
	.loc 1 392 5 view .LVU521
	mov	r2, #4	@,
	mov	r3, r7	@, fp
	mov	r1, #1	@,
	ldr	r0, .L116+16	@,
	bl	fwrite		@
.LVL106:
	.loc 1 394 5 view .LVU522
@ armwave.c:394:     for(yy = 0; yy < g_armwave_state.target_height; yy++) {
	.loc 1 394 37 is_stmt 0 view .LVU523
	ldr	r2, [r6, #92]	@ prephitmp_53, g_armwave_state.target_height
@ armwave.c:394:     for(yy = 0; yy < g_armwave_state.target_height; yy++) {
	.loc 1 394 5 view .LVU524
	cmp	r2, #0	@ prephitmp_53,
	ble	.L109		@,
	ldr	r3, [r6, #88]	@ _13, g_armwave_state.target_width
@ armwave.c:399:             fprintf(fp, "%3d %3d %3d\n", data & 0xff, (data >> 8) & 0xff, (data >> 16) & 0xff);
	.loc 1 399 13 view .LVU525
	ldr	r9, .L116+20	@ tmp152,
@ armwave.c:394:     for(yy = 0; yy < g_armwave_state.target_height; yy++) {
	.loc 1 394 12 view .LVU526
	mov	r5, #0	@ yy,
.LVL107:
.L110:
@ armwave.c:395:         for(xx = 0; xx < g_armwave_state.target_width; xx++) {
	.loc 1 395 9 view .LVU527
	cmp	r3, #0	@ _13,
@ armwave.c:395:         for(xx = 0; xx < g_armwave_state.target_width; xx++) {
	.loc 1 395 16 view .LVU528
	movgt	r4, #0	@ xx,
@ armwave.c:395:         for(xx = 0; xx < g_armwave_state.target_width; xx++) {
	.loc 1 395 9 view .LVU529
	ble	.L112		@,
.LVL108:
.L111:
	.loc 1 396 13 is_stmt 1 discriminator 3 view .LVU530
@ armwave.c:396:             data = *(buffer + (xx + (yy * g_armwave_state.target_width)));
	.loc 1 396 29 is_stmt 0 discriminator 3 view .LVU531
	mla	r3, r5, r3, r4	@ tmp141, yy, _13, xx
@ armwave.c:399:             fprintf(fp, "%3d %3d %3d\n", data & 0xff, (data >> 8) & 0xff, (data >> 16) & 0xff);
	.loc 1 399 13 discriminator 3 view .LVU532
	mov	r1, r9	@, tmp152
	mov	r0, r7	@, fp
@ armwave.c:396:             data = *(buffer + (xx + (yy * g_armwave_state.target_width)));
	.loc 1 396 18 discriminator 3 view .LVU533
	ldr	r2, [r8, r3, lsl #2]	@ data, *_7
.LVL109:
	.loc 1 399 13 is_stmt 1 discriminator 3 view .LVU534
@ armwave.c:395:         for(xx = 0; xx < g_armwave_state.target_width; xx++) {
	.loc 1 395 58 is_stmt 0 discriminator 3 view .LVU535
	add	r4, r4, #1	@ xx, xx,
.LVL110:
@ armwave.c:399:             fprintf(fp, "%3d %3d %3d\n", data & 0xff, (data >> 8) & 0xff, (data >> 16) & 0xff);
	.loc 1 399 81 discriminator 3 view .LVU536
	lsr	ip, r2, #16	@ tmp146, data,
@ armwave.c:399:             fprintf(fp, "%3d %3d %3d\n", data & 0xff, (data >> 8) & 0xff, (data >> 16) & 0xff);
	.loc 1 399 61 discriminator 3 view .LVU537
	lsr	r3, r2, #8	@ tmp142, data,
@ armwave.c:399:             fprintf(fp, "%3d %3d %3d\n", data & 0xff, (data >> 8) & 0xff, (data >> 16) & 0xff);
	.loc 1 399 13 discriminator 3 view .LVU538
	uxtb	ip, ip	@ tmp147, tmp146
	uxtb	r3, r3	@, tmp142
	str	ip, [sp]	@ tmp147,
	uxtb	r2, r2	@, data
.LVL111:
	.loc 1 399 13 discriminator 3 view .LVU539
	bl	fprintf		@
.LVL112:
@ armwave.c:395:         for(xx = 0; xx < g_armwave_state.target_width; xx++) {
	.loc 1 395 41 discriminator 3 view .LVU540
	ldr	r3, [r6, #88]	@ _13, g_armwave_state.target_width
@ armwave.c:395:         for(xx = 0; xx < g_armwave_state.target_width; xx++) {
	.loc 1 395 9 discriminator 3 view .LVU541
	cmp	r3, r4	@ _13, xx
	bgt	.L111		@,
	ldr	r2, [r6, #92]	@ prephitmp_53, g_armwave_state.target_height
.LVL113:
.L112:
@ armwave.c:394:     for(yy = 0; yy < g_armwave_state.target_height; yy++) {
	.loc 1 394 55 discriminator 2 view .LVU542
	add	r5, r5, #1	@ yy, yy,
.LVL114:
@ armwave.c:394:     for(yy = 0; yy < g_armwave_state.target_height; yy++) {
	.loc 1 394 5 discriminator 2 view .LVU543
	cmp	r5, r2	@ yy, prephitmp_53
	blt	.L110		@,
.LVL115:
.L109:
	.loc 1 403 5 is_stmt 1 view .LVU544
	mov	r0, r7	@, fp
@ armwave.c:404: }
	.loc 1 404 1 is_stmt 0 view .LVU545
	add	sp, sp, #12	@,,
	.cfi_def_cfa_offset 28
	@ sp needed	@
	pop	{r4, r5, r6, r7, r8, r9, lr}	@
	.cfi_restore 14
	.cfi_restore 9
	.cfi_restore 8
	.cfi_restore 7
	.cfi_restore 6
	.cfi_restore 5
	.cfi_restore 4
	.cfi_def_cfa_offset 0
.LVL116:
@ armwave.c:403:     fclose(fp);
	.loc 1 403 5 view .LVU546
	b	fclose		@
.LVL117:
.L117:
	.loc 1 403 5 view .LVU547
	.align	2
.L116:
	.word	.LC13
	.word	g_armwave_state
	.word	.LC14
	.word	.LC15
	.word	.LC16
	.word	.LC17
	.cfi_endproc
.LFE68:
	.size	armwave_dump_ppm_debug, .-armwave_dump_ppm_debug
	.align	2
	.global	armwave_test_init
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_test_init, %function
armwave_test_init:
.LVL118:
.LFB69:
	.loc 1 410 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 411 5 view .LVU549
.LBB18:
.LBI18:
	.loc 1 34 6 view .LVU550
	.loc 1 34 6 is_stmt 0 view .LVU551
.LBE18:
@ armwave.c:410: {
	.loc 1 410 1 view .LVU552
	push	{r4, r5, r6, r7, r8, r9, r10, lr}	@
	.cfi_def_cfa_offset 32
	.cfi_offset 4, -32
	.cfi_offset 5, -28
	.cfi_offset 6, -24
	.cfi_offset 7, -20
	.cfi_offset 8, -16
	.cfi_offset 9, -12
	.cfi_offset 10, -8
	.cfi_offset 14, -4
	vpush.64	{d8, d9, d10}	@
	.cfi_def_cfa_offset 56
	.cfi_offset 80, -56
	.cfi_offset 81, -52
	.cfi_offset 82, -48
	.cfi_offset 83, -44
	.cfi_offset 84, -40
	.cfi_offset 85, -36
	ldr	r4, .L122+20	@ ivtmp.90,
.LBB21:
.LBB19:
@ armwave.c:40:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 40 32 view .LVU553
	vldr.32	s20, .L122+16	@ tmp136,
@ armwave.c:40:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 40 26 view .LVU554
	vldr.64	d9, .L122	@ tmp151,
@ armwave.c:40:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 40 49 view .LVU555
	vldr.64	d8, .L122+8	@ tmp139,
	add	ip, r4, #1	@ _38, ivtmp.90,
.LBE19:
.LBE21:
@ armwave.c:410: {
	.loc 1 410 1 view .LVU556
	sub	sp, sp, #16	@,,
	.cfi_def_cfa_offset 72
@ armwave.c:410: {
	.loc 1 410 1 view .LVU557
	mov	r6, r0	@ wave_size, wave_size
	mov	r7, r1	@ nwaves, nwaves
	mov	r8, r2	@ render_width, render_width
	mov	r9, r3	@ render_height, render_height
	add	r10, r4, #256	@ _39, ivtmp.90,
	rsb	r5, ip, #1	@ tmp150, _38,
.LVL119:
.L119:
.LBB22:
.LBB20:
	.loc 1 40 9 is_stmt 1 view .LVU558
@ armwave.c:40:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 40 32 is_stmt 0 view .LVU559
	add	r3, r5, r4	@ tmp133, tmp150, ivtmp.90
@ armwave.c:40:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 40 26 view .LVU560
	vmov.f64	d1, d9	@, tmp151
@ armwave.c:40:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 40 32 view .LVU561
	vmov	s15, r3	@ int	@ tmp133, tmp133
	vcvt.f32.s32	s15, s15	@ tmp134, tmp133
	vdiv.f32	s0, s15, s20	@ tmp135, tmp134, tmp136
@ armwave.c:40:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 40 26 view .LVU562
	vcvt.f64.f32	d0, s0	@, tmp135
	bl	pow		@
.LVL120:
@ armwave.c:40:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 40 49 view .LVU563
	vmul.f64	d0, d0, d8	@ tmp138,, tmp139
@ armwave.c:40:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 40 24 view .LVU564
	vcvt.u32.f64	s15, d0	@ tmp140, tmp138
	vmov	r0, s15	@ int	@ tmp140, tmp140
	strb	r0, [r4, #1]!	@ tmp140, MEM[base: _37, offset: 0B]
.LVL121:
@ armwave.c:39:     for(i = 0; i < 256; i++) {
	.loc 1 39 5 view .LVU565
	cmp	r4, r10	@ ivtmp.90, _39
	bne	.L119		@,
.LVL122:
	.loc 1 39 5 view .LVU566
.LBE20:
.LBE22:
	.loc 1 414 5 is_stmt 1 view .LVU567
.LBB23:
.LBI23:
	.loc 1 367 6 view .LVU568
.LBB24:
	.loc 1 370 5 view .LVU569
	.loc 1 372 13 view .LVU570
	.loc 1 373 13 view .LVU571
@ armwave.c:372:             g_armwave_state.ch1_color.r = r;
	.loc 1 372 41 is_stmt 0 view .LVU572
	ldr	ip, .L122+24	@ tmp142,
	ldr	r4, .L122+28	@ tmp143,
.LBE24:
.LBE23:
@ armwave.c:416:     armwave_setup_render(0, wave_size, nwaves, wave_size, render_width, render_height, 0x00000000);
	.loc 1 416 5 view .LVU573
	mov	r0, #0	@ tmp147,
.LBB28:
.LBB25:
@ armwave.c:374:             g_armwave_state.ch1_color.b = b;
	.loc 1 374 41 view .LVU574
	mov	lr, #250	@ tmp146,
.LBE25:
.LBE28:
@ armwave.c:416:     armwave_setup_render(0, wave_size, nwaves, wave_size, render_width, render_height, 0x00000000);
	.loc 1 416 5 view .LVU575
	str	r0, [sp, #8]	@ tmp147,
	str	r9, [sp, #4]	@ render_height,
	str	r8, [sp]	@ render_width,
	mov	r1, r6	@, tmp3
	mov	r2, r7	@, nwaves
.LBB29:
.LBB26:
@ armwave.c:372:             g_armwave_state.ch1_color.r = r;
	.loc 1 372 41 view .LVU576
	str	r4, [ip, #108]	@ tmp143, MEM[(short int *)&g_armwave_state + 108B]
	.loc 1 374 13 is_stmt 1 view .LVU577
.LBE26:
.LBE29:
@ armwave.c:416:     armwave_setup_render(0, wave_size, nwaves, wave_size, render_width, render_height, 0x00000000);
	.loc 1 416 5 is_stmt 0 view .LVU578
	mov	r3, r6	@, wave_size
.LBB30:
.LBB27:
@ armwave.c:374:             g_armwave_state.ch1_color.b = b;
	.loc 1 374 41 view .LVU579
	strh	lr, [ip, #112]	@ movhi	@ tmp146, g_armwave_state.ch1_color.b
	.loc 1 375 13 is_stmt 1 view .LVU580
.LVL123:
	.loc 1 375 13 is_stmt 0 view .LVU581
.LBE27:
.LBE30:
	.loc 1 416 5 is_stmt 1 view .LVU582
	bl	armwave_setup_render		@
.LVL124:
	.loc 1 418 5 view .LVU583
	ldr	r1, .L122+32	@,
	ldr	r0, .L122+36	@,
@ armwave.c:419: }
	.loc 1 419 1 is_stmt 0 view .LVU584
	add	sp, sp, #16	@,,
	.cfi_def_cfa_offset 56
	@ sp needed	@
	vldm	sp!, {d8-d10}	@,
	.cfi_restore 84
	.cfi_restore 85
	.cfi_restore 82
	.cfi_restore 83
	.cfi_restore 80
	.cfi_restore 81
	.cfi_def_cfa_offset 32
	pop	{r4, r5, r6, r7, r8, r9, r10, lr}	@
	.cfi_restore 14
	.cfi_restore 10
	.cfi_restore 9
	.cfi_restore 8
	.cfi_restore 7
	.cfi_restore 6
	.cfi_restore 5
	.cfi_restore 4
	.cfi_def_cfa_offset 0
.LVL125:
@ armwave.c:418:     printf("armwave version: %s\n", ARMWAVE_VER);
	.loc 1 418 5 view .LVU585
	b	printf		@
.LVL126:
.L123:
	.align	3
.L122:
	.word	-1073741824
	.word	1072483532
	.word	0
	.word	1081073664
	.word	1132396544
	.word	gamma_table-1
	.word	g_armwave_state
	.word	116656630
	.word	.LC0
	.word	.LC1
	.cfi_endproc
.LFE69:
	.size	armwave_test_init, .-armwave_test_init
	.align	2
	.global	armwave_test_fill_outbuf
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_test_fill_outbuf, %function
armwave_test_fill_outbuf:
.LFB70:
	.loc 1 425 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 426 5 view .LVU587
@ armwave.c:425: {
	.loc 1 425 1 is_stmt 0 view .LVU588
	push	{r4, r5, r6, r7, r8, r9, r10, lr}	@
	.cfi_def_cfa_offset 32
	.cfi_offset 4, -32
	.cfi_offset 5, -28
	.cfi_offset 6, -24
	.cfi_offset 7, -20
	.cfi_offset 8, -16
	.cfi_offset 9, -12
	.cfi_offset 10, -8
	.cfi_offset 14, -4
@ armwave.c:426:     armwave_fill_pixbuf_scaled(g_armwave_state.out_pixbuf);
	.loc 1 426 5 view .LVU589
	ldr	r4, .L135	@ tmp221,
	ldr	r5, [r4, #104]	@ _1, g_armwave_state.out_pixbuf
.LVL127:
.LBB33:
.LBI33:
	.loc 1 117 6 is_stmt 1 view .LVU590
.LBB34:
	.loc 1 119 5 view .LVU591
	.loc 1 121 5 view .LVU592
	.loc 1 122 5 view .LVU593
	.loc 1 124 5 view .LVU594
	.loc 1 125 5 view .LVU595
	.loc 1 126 5 view .LVU596
	.loc 1 128 5 view .LVU597
	cmp	r5, #0	@ _1,
	beq	.L134		@,
	.loc 1 131 5 view .LVU598
@ armwave.c:131:     memset(out_buffer, 0x00, g_armwave_state.target_width * g_armwave_state.target_height * 4);
	.loc 1 131 59 is_stmt 0 view .LVU599
	ldr	r3, [r4, #88]	@ g_armwave_state.target_width, g_armwave_state.target_width
	ldr	r2, [r4, #92]	@ g_armwave_state.target_height, g_armwave_state.target_height
@ armwave.c:131:     memset(out_buffer, 0x00, g_armwave_state.target_width * g_armwave_state.target_height * 4);
	.loc 1 131 5 view .LVU600
	mov	r1, #0	@,
	mov	r0, r5	@, _1
@ armwave.c:131:     memset(out_buffer, 0x00, g_armwave_state.target_width * g_armwave_state.target_height * 4);
	.loc 1 131 59 view .LVU601
	mul	r2, r2, r3	@ tmp176, g_armwave_state.target_height, g_armwave_state.target_width
@ armwave.c:131:     memset(out_buffer, 0x00, g_armwave_state.target_width * g_armwave_state.target_height * 4);
	.loc 1 131 5 view .LVU602
	lsl	r2, r2, #2	@, tmp176,
	bl	memset		@
.LVL128:
	.loc 1 133 5 is_stmt 1 view .LVU603
@ armwave.c:133:     npix = g_armwave_state.target_width * g_armwave_state.bitdepth_height; 
	.loc 1 133 10 is_stmt 0 view .LVU604
	ldr	r3, [r4, #88]	@ g_armwave_state.target_width, g_armwave_state.target_width
	ldr	r2, [r4, #64]	@ g_armwave_state.bitdepth_height, g_armwave_state.bitdepth_height
	mul	r2, r2, r3	@ npix, g_armwave_state.bitdepth_height, g_armwave_state.target_width
.LVL129:
	.loc 1 176 5 is_stmt 1 view .LVU605
	.loc 1 176 5 is_stmt 0 view .LVU606
	cmp	r2, #0	@ npix,
	pople	{r4, r5, r6, r7, r8, r9, r10, pc}	@
@ armwave.c:177:         wave_word = g_armwave_state.ch1_buffer[n];
	.loc 1 177 36 view .LVU607
	ldr	r1, [r4, #4]	@ _12, g_armwave_state.ch1_buffer
@ armwave.c:181:             rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 181 44 view .LVU608
	ldrsh	r7, [r4, #108]	@ _19, g_armwave_state.ch1_color.r
@ armwave.c:182:             gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 182 44 view .LVU609
	ldrsh	r6, [r4, #110]	@ _24, g_armwave_state.ch1_color.g
@ armwave.c:183:             bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 183 44 view .LVU610
	ldrsh	lr, [r4, #112]	@ _28, g_armwave_state.ch1_color.b
@ armwave.c:194:             yy = (nsub & 0xff) * g_armwave_state.vscale_frac;
	.loc 1 194 49 view .LVU611
	vldr.32	s14, [r4, #40]	@ _48, g_armwave_state.vscale_frac
	sub	r1, r1, #2	@ ivtmp.95, _12,
	add	r2, r1, r2, lsl #1	@ _96, ivtmp.95, npix,
.LVL130:
@ armwave.c:176:     for(n = 0; n < npix; n += 1) {
	.loc 1 176 11 view .LVU612
	mov	r0, #0	@ n,
.LVL131:
.L128:
	.loc 1 177 9 is_stmt 1 view .LVU613
	.loc 1 178 9 view .LVU614
@ armwave.c:178:         value = wave_word & 0xffff;
	.loc 1 178 15 is_stmt 0 view .LVU615
	ldrh	r8, [r1, #2]!	@ MEM[base: _101, offset: 0B], MEM[base: _101, offset: 0B]
.LVL132:
	.loc 1 180 9 is_stmt 1 view .LVU616
@ armwave.c:180:         if(value != 0) {
	.loc 1 180 11 is_stmt 0 view .LVU617
	ands	r8, r8, #255	@ value, MEM[base: _101, offset: 0B],
.LVL133:
	.loc 1 180 11 view .LVU618
	beq	.L127		@,
	.loc 1 181 13 is_stmt 1 view .LVU619
.LVL134:
	.loc 1 182 13 view .LVU620
	.loc 1 183 13 view .LVU621
	.loc 1 185 13 view .LVU622
	.loc 1 186 13 view .LVU623
	.loc 1 187 13 view .LVU624
	.loc 1 190 13 view .LVU625
	.loc 1 193 13 view .LVU626
	.loc 1 194 13 view .LVU627
	.loc 1 195 13 view .LVU628
	.loc 1 196 13 view .LVU629
	.loc 1 199 13 view .LVU630
	.loc 1 201 17 view .LVU631
	.loc 1 202 17 view .LVU632
@ armwave.c:194:             yy = (nsub & 0xff) * g_armwave_state.vscale_frac;
	.loc 1 194 24 is_stmt 0 view .LVU633
	uxtb	ip, r0	@ n, n
@ armwave.c:183:             bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 183 47 view .LVU634
	smulbb	r3, r8, lr	@ tmp207, value, _28
@ armwave.c:194:             yy = (nsub & 0xff) * g_armwave_state.vscale_frac;
	.loc 1 194 32 view .LVU635
	vmov	s15, ip	@ int	@ n, n
@ armwave.c:182:             gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 182 47 view .LVU636
	smulbb	r9, r8, r6	@ tmp211, value, _24
@ armwave.c:183:             bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 183 16 view .LVU637
	asr	r3, r3, #8	@ bb, tmp207,
@ armwave.c:187:             b = MIN(bb, 255);
	.loc 1 187 17 view .LVU638
	cmp	r3, #255	@ bb,
@ armwave.c:194:             yy = (nsub & 0xff) * g_armwave_state.vscale_frac;
	.loc 1 194 32 view .LVU639
	vcvt.f32.s32	s15, s15	@ tmp198, n
@ armwave.c:182:             gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 182 16 view .LVU640
	asr	r9, r9, #8	@ gg, tmp211,
@ armwave.c:181:             rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 181 47 view .LVU641
	smulbb	r8, r8, r7	@ tmp217, value, _19
.LVL135:
@ armwave.c:187:             b = MIN(bb, 255);
	.loc 1 187 17 view .LVU642
	movge	r3, #255	@ bb,
@ armwave.c:186:             g = MIN(gg, 255);
	.loc 1 186 17 view .LVU643
	cmp	r9, #255	@ gg,
	movge	r9, #255	@ gg,
@ armwave.c:181:             rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 181 16 view .LVU644
	asr	r8, r8, #8	@ rr, tmp217,
@ armwave.c:185:             r = MIN(rr, 255);
	.loc 1 185 17 view .LVU645
	cmp	r8, #255	@ rr,
@ armwave.c:194:             yy = (nsub & 0xff) * g_armwave_state.vscale_frac;
	.loc 1 194 32 view .LVU646
	vmul.f32	s15, s15, s14	@ tmp199, tmp198, _48
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 48 view .LVU647
	lsl	r9, r9, #8	@ tmp212, gg,
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 36 view .LVU648
	lsl	r3, r3, #16	@ tmp208, bb,
@ armwave.c:185:             r = MIN(rr, 255);
	.loc 1 185 17 view .LVU649
	movge	r8, #255	@ rr,
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 48 view .LVU650
	uxth	r9, r9	@ tmp213, tmp212
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 36 view .LVU651
	and	r3, r3, #16711680	@ tmp209, tmp208,
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 43 view .LVU652
	orr	r3, r3, r9	@ tmp215, tmp209, tmp213
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 54 view .LVU653
	uxtb	r8, r8	@ rr, rr
@ armwave.c:194:             yy = (nsub & 0xff) * g_armwave_state.vscale_frac;
	.loc 1 194 16 view .LVU654
	vcvt.u32.f32	s15, s15	@ yy, tmp199
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 54 view .LVU655
	orr	r3, r3, r8	@ tmp219, tmp215, rr
@ armwave.c:201:                 offset = (xx + (y * g_armwave_state.target_width)); 
	.loc 1 201 35 view .LVU656
	ldr	ip, [r4, #88]	@ g_armwave_state.target_width, g_armwave_state.target_width
@ armwave.c:196:             xx = (nsub >> 8) / 2;
	.loc 1 196 30 view .LVU657
	asr	r9, r0, #9	@ xx, n,
.LVL136:
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 18 view .LVU658
	orr	r3, r3, #-16777216	@ word, tmp219,
@ armwave.c:202:                 *(out_buffer_base + offset) = word;
	.loc 1 202 35 view .LVU659
	vmov	r8, s15	@ int	@ yy, yy
	mla	ip, ip, r8, r9	@ tmp205, g_armwave_state.target_width, yy, xx
@ armwave.c:202:                 *(out_buffer_base + offset) = word;
	.loc 1 202 45 view .LVU660
	str	r3, [r5, ip, lsl #2]	@ word, *_59
.LVL137:
.L127:
@ armwave.c:176:     for(n = 0; n < npix; n += 1) {
	.loc 1 176 5 view .LVU661
	cmp	r2, r1	@ _96, ivtmp.95
@ armwave.c:176:     for(n = 0; n < npix; n += 1) {
	.loc 1 176 28 view .LVU662
	add	r0, r0, #1	@ n, n,
.LVL138:
@ armwave.c:176:     for(n = 0; n < npix; n += 1) {
	.loc 1 176 5 view .LVU663
	bne	.L128		@,
	pop	{r4, r5, r6, r7, r8, r9, r10, pc}	@
.LVL139:
.L134:
	.loc 1 128 5 is_stmt 1 view .LVU664
	ldr	r3, .L135+4	@,
	mov	r2, #128	@,
	ldr	r1, .L135+8	@,
	ldr	r0, .L135+12	@,
	bl	__assert_fail		@
.LVL140:
.L136:
	.loc 1 128 5 is_stmt 0 view .LVU665
	.align	2
.L135:
	.word	g_armwave_state
	.word	.LANCHOR0
	.word	.LC2
	.word	.LC3
.LBE34:
.LBE33:
	.cfi_endproc
.LFE70:
	.size	armwave_test_fill_outbuf, .-armwave_test_fill_outbuf
	.align	2
	.global	armwave_test_dump_buffer_to_ppm
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_test_dump_buffer_to_ppm, %function
armwave_test_dump_buffer_to_ppm:
.LVL141:
.LFB71:
	.loc 1 433 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	.loc 1 434 5 view .LVU667
	ldr	r3, .L138	@ tmp112,
	mov	r1, r0	@, fn
	ldr	r0, [r3, #104]	@, g_armwave_state.out_pixbuf
.LVL142:
	.loc 1 434 5 is_stmt 0 view .LVU668
	b	armwave_dump_ppm_debug		@
.LVL143:
.L139:
	.loc 1 434 5 view .LVU669
	.align	2
.L138:
	.word	g_armwave_state
	.cfi_endproc
.LFE71:
	.size	armwave_test_dump_buffer_to_ppm, .-armwave_test_dump_buffer_to_ppm
	.align	2
	.global	armwave_test_fill_gdkbuf
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_test_fill_gdkbuf, %function
armwave_test_fill_gdkbuf:
.LVL144:
.LFB72:
	.loc 1 441 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 446 5 view .LVU671
@ armwave.c:446:     void *out_pixbuf = ((uint32_t ***)buf)[2][10];
	.loc 1 446 11 is_stmt 0 view .LVU672
	ldr	r3, [r0, #8]	@ MEM[(uint32_t * * *)buf_2(D) + 8B], MEM[(uint32_t * * *)buf_2(D) + 8B]
@ armwave.c:441: {
	.loc 1 441 1 view .LVU673
	push	{r4, r5, r6, r7, r8, r9, r10, lr}	@
	.cfi_def_cfa_offset 32
	.cfi_offset 4, -32
	.cfi_offset 5, -28
	.cfi_offset 6, -24
	.cfi_offset 7, -20
	.cfi_offset 8, -16
	.cfi_offset 9, -12
	.cfi_offset 10, -8
	.cfi_offset 14, -4
@ armwave.c:446:     void *out_pixbuf = ((uint32_t ***)buf)[2][10];
	.loc 1 446 11 view .LVU674
	ldr	r5, [r3, #40]	@ out_pixbuf, MEM[(uint32_t * *)_1 + 40B]
.LVL145:
	.loc 1 449 5 is_stmt 1 view .LVU675
.LBB37:
.LBI37:
	.loc 1 117 6 view .LVU676
.LBB38:
	.loc 1 119 5 view .LVU677
	.loc 1 121 5 view .LVU678
	.loc 1 122 5 view .LVU679
	.loc 1 124 5 view .LVU680
	.loc 1 125 5 view .LVU681
	.loc 1 126 5 view .LVU682
	.loc 1 128 5 view .LVU683
	cmp	r5, #0	@ out_pixbuf,
	beq	.L150		@,
	.loc 1 131 5 view .LVU684
@ armwave.c:131:     memset(out_buffer, 0x00, g_armwave_state.target_width * g_armwave_state.target_height * 4);
	.loc 1 131 45 is_stmt 0 view .LVU685
	ldr	r4, .L151	@ tmp223,
@ armwave.c:131:     memset(out_buffer, 0x00, g_armwave_state.target_width * g_armwave_state.target_height * 4);
	.loc 1 131 5 view .LVU686
	mov	r1, #0	@,
	mov	r0, r5	@, out_pixbuf
.LVL146:
@ armwave.c:131:     memset(out_buffer, 0x00, g_armwave_state.target_width * g_armwave_state.target_height * 4);
	.loc 1 131 59 view .LVU687
	ldr	r3, [r4, #88]	@ g_armwave_state.target_width, g_armwave_state.target_width
	ldr	r2, [r4, #92]	@ g_armwave_state.target_height, g_armwave_state.target_height
	mul	r2, r2, r3	@ tmp178, g_armwave_state.target_height, g_armwave_state.target_width
@ armwave.c:131:     memset(out_buffer, 0x00, g_armwave_state.target_width * g_armwave_state.target_height * 4);
	.loc 1 131 5 view .LVU688
	lsl	r2, r2, #2	@, tmp178,
	bl	memset		@
.LVL147:
	.loc 1 133 5 is_stmt 1 view .LVU689
@ armwave.c:133:     npix = g_armwave_state.target_width * g_armwave_state.bitdepth_height; 
	.loc 1 133 10 is_stmt 0 view .LVU690
	ldr	r3, [r4, #88]	@ g_armwave_state.target_width, g_armwave_state.target_width
	ldr	r2, [r4, #64]	@ g_armwave_state.bitdepth_height, g_armwave_state.bitdepth_height
	mul	r2, r2, r3	@ npix, g_armwave_state.bitdepth_height, g_armwave_state.target_width
.LVL148:
	.loc 1 176 5 is_stmt 1 view .LVU691
	.loc 1 176 5 is_stmt 0 view .LVU692
	cmp	r2, #0	@ npix,
	pople	{r4, r5, r6, r7, r8, r9, r10, pc}	@
@ armwave.c:177:         wave_word = g_armwave_state.ch1_buffer[n];
	.loc 1 177 36 view .LVU693
	ldr	r1, [r4, #4]	@ _14, g_armwave_state.ch1_buffer
@ armwave.c:181:             rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 181 44 view .LVU694
	ldrsh	r7, [r4, #108]	@ _21, g_armwave_state.ch1_color.r
@ armwave.c:182:             gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 182 44 view .LVU695
	ldrsh	r6, [r4, #110]	@ _26, g_armwave_state.ch1_color.g
@ armwave.c:183:             bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 183 44 view .LVU696
	ldrsh	lr, [r4, #112]	@ _30, g_armwave_state.ch1_color.b
@ armwave.c:194:             yy = (nsub & 0xff) * g_armwave_state.vscale_frac;
	.loc 1 194 49 view .LVU697
	vldr.32	s14, [r4, #40]	@ _50, g_armwave_state.vscale_frac
	sub	r1, r1, #2	@ ivtmp.100, _14,
	add	r2, r1, r2, lsl #1	@ _98, ivtmp.100, npix,
.LVL149:
@ armwave.c:176:     for(n = 0; n < npix; n += 1) {
	.loc 1 176 11 view .LVU698
	mov	r0, #0	@ n,
.LVL150:
.L144:
	.loc 1 177 9 is_stmt 1 view .LVU699
	.loc 1 178 9 view .LVU700
@ armwave.c:178:         value = wave_word & 0xffff;
	.loc 1 178 15 is_stmt 0 view .LVU701
	ldrh	r8, [r1, #2]!	@ MEM[base: _103, offset: 0B], MEM[base: _103, offset: 0B]
.LVL151:
	.loc 1 180 9 is_stmt 1 view .LVU702
@ armwave.c:180:         if(value != 0) {
	.loc 1 180 11 is_stmt 0 view .LVU703
	ands	r8, r8, #255	@ value, MEM[base: _103, offset: 0B],
.LVL152:
	.loc 1 180 11 view .LVU704
	beq	.L143		@,
	.loc 1 181 13 is_stmt 1 view .LVU705
.LVL153:
	.loc 1 182 13 view .LVU706
	.loc 1 183 13 view .LVU707
	.loc 1 185 13 view .LVU708
	.loc 1 186 13 view .LVU709
	.loc 1 187 13 view .LVU710
	.loc 1 190 13 view .LVU711
	.loc 1 193 13 view .LVU712
	.loc 1 194 13 view .LVU713
	.loc 1 195 13 view .LVU714
	.loc 1 196 13 view .LVU715
	.loc 1 199 13 view .LVU716
	.loc 1 201 17 view .LVU717
	.loc 1 202 17 view .LVU718
@ armwave.c:194:             yy = (nsub & 0xff) * g_armwave_state.vscale_frac;
	.loc 1 194 24 is_stmt 0 view .LVU719
	uxtb	ip, r0	@ n, n
@ armwave.c:183:             bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 183 47 view .LVU720
	smulbb	r3, r8, lr	@ tmp209, value, _30
@ armwave.c:194:             yy = (nsub & 0xff) * g_armwave_state.vscale_frac;
	.loc 1 194 32 view .LVU721
	vmov	s15, ip	@ int	@ n, n
@ armwave.c:182:             gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 182 47 view .LVU722
	smulbb	r9, r8, r6	@ tmp213, value, _26
@ armwave.c:183:             bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 183 16 view .LVU723
	asr	r3, r3, #8	@ bb, tmp209,
@ armwave.c:187:             b = MIN(bb, 255);
	.loc 1 187 17 view .LVU724
	cmp	r3, #255	@ bb,
@ armwave.c:194:             yy = (nsub & 0xff) * g_armwave_state.vscale_frac;
	.loc 1 194 32 view .LVU725
	vcvt.f32.s32	s15, s15	@ tmp200, n
@ armwave.c:182:             gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 182 16 view .LVU726
	asr	r9, r9, #8	@ gg, tmp213,
@ armwave.c:181:             rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 181 47 view .LVU727
	smulbb	r8, r8, r7	@ tmp219, value, _21
.LVL154:
@ armwave.c:187:             b = MIN(bb, 255);
	.loc 1 187 17 view .LVU728
	movge	r3, #255	@ bb,
@ armwave.c:186:             g = MIN(gg, 255);
	.loc 1 186 17 view .LVU729
	cmp	r9, #255	@ gg,
	movge	r9, #255	@ gg,
@ armwave.c:181:             rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 181 16 view .LVU730
	asr	r8, r8, #8	@ rr, tmp219,
@ armwave.c:185:             r = MIN(rr, 255);
	.loc 1 185 17 view .LVU731
	cmp	r8, #255	@ rr,
@ armwave.c:194:             yy = (nsub & 0xff) * g_armwave_state.vscale_frac;
	.loc 1 194 32 view .LVU732
	vmul.f32	s15, s15, s14	@ tmp201, tmp200, _50
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 48 view .LVU733
	lsl	r9, r9, #8	@ tmp214, gg,
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 36 view .LVU734
	lsl	r3, r3, #16	@ tmp210, bb,
@ armwave.c:185:             r = MIN(rr, 255);
	.loc 1 185 17 view .LVU735
	movge	r8, #255	@ rr,
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 48 view .LVU736
	uxth	r9, r9	@ tmp215, tmp214
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 36 view .LVU737
	and	r3, r3, #16711680	@ tmp211, tmp210,
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 43 view .LVU738
	orr	r3, r3, r9	@ tmp217, tmp211, tmp215
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 54 view .LVU739
	uxtb	r8, r8	@ rr, rr
@ armwave.c:194:             yy = (nsub & 0xff) * g_armwave_state.vscale_frac;
	.loc 1 194 16 view .LVU740
	vcvt.u32.f32	s15, s15	@ yy, tmp201
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 54 view .LVU741
	orr	r3, r3, r8	@ tmp221, tmp217, rr
@ armwave.c:201:                 offset = (xx + (y * g_armwave_state.target_width)); 
	.loc 1 201 35 view .LVU742
	ldr	ip, [r4, #88]	@ g_armwave_state.target_width, g_armwave_state.target_width
@ armwave.c:196:             xx = (nsub >> 8) / 2;
	.loc 1 196 30 view .LVU743
	asr	r9, r0, #9	@ xx, n,
.LVL155:
@ armwave.c:190:             word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 190 18 view .LVU744
	orr	r3, r3, #-16777216	@ word, tmp221,
@ armwave.c:202:                 *(out_buffer_base + offset) = word;
	.loc 1 202 35 view .LVU745
	vmov	r8, s15	@ int	@ yy, yy
	mla	ip, ip, r8, r9	@ tmp207, g_armwave_state.target_width, yy, xx
@ armwave.c:202:                 *(out_buffer_base + offset) = word;
	.loc 1 202 45 view .LVU746
	str	r3, [r5, ip, lsl #2]	@ word, *_61
.LVL156:
.L143:
@ armwave.c:176:     for(n = 0; n < npix; n += 1) {
	.loc 1 176 5 view .LVU747
	cmp	r2, r1	@ _98, ivtmp.100
@ armwave.c:176:     for(n = 0; n < npix; n += 1) {
	.loc 1 176 28 view .LVU748
	add	r0, r0, #1	@ n, n,
.LVL157:
@ armwave.c:176:     for(n = 0; n < npix; n += 1) {
	.loc 1 176 5 view .LVU749
	bne	.L144		@,
	pop	{r4, r5, r6, r7, r8, r9, r10, pc}	@
.LVL158:
.L150:
	.loc 1 128 5 is_stmt 1 view .LVU750
	ldr	r3, .L151+4	@,
	mov	r2, #128	@,
	ldr	r1, .L151+8	@,
	ldr	r0, .L151+12	@,
.LVL159:
	.loc 1 128 5 is_stmt 0 view .LVU751
	bl	__assert_fail		@
.LVL160:
.L152:
	.loc 1 128 5 view .LVU752
	.align	2
.L151:
	.word	g_armwave_state
	.word	.LANCHOR0
	.word	.LC2
	.word	.LC3
.LBE38:
.LBE37:
	.cfi_endproc
.LFE72:
	.size	armwave_test_fill_gdkbuf, .-armwave_test_fill_gdkbuf
	.align	2
	.global	armwave_test_buffer_alloc
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_test_buffer_alloc, %function
armwave_test_buffer_alloc:
.LVL161:
.LFB73:
	.loc 1 456 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 457 5 view .LVU754
@ armwave.c:456: {
	.loc 1 456 1 is_stmt 0 view .LVU755
	push	{r4, r5, r6, lr}	@
	.cfi_def_cfa_offset 16
	.cfi_offset 4, -16
	.cfi_offset 5, -12
	.cfi_offset 6, -8
	.cfi_offset 14, -4
@ armwave.c:456: {
	.loc 1 456 1 view .LVU756
	mov	r6, r0	@ nsets, nsets
@ armwave.c:457:     if(g_armwave_state.test_wave_buffer != NULL) {
	.loc 1 457 23 view .LVU757
	ldr	r4, .L160	@ tmp128,
	ldr	r0, [r4, #24]	@ _1, g_armwave_state.test_wave_buffer
.LVL162:
@ armwave.c:457:     if(g_armwave_state.test_wave_buffer != NULL) {
	.loc 1 457 7 view .LVU758
	cmp	r0, #0	@ _1,
	beq	.L154		@,
	.loc 1 458 9 is_stmt 1 view .LVU759
	bl	free		@
.LVL163:
.L154:
	.loc 1 463 5 view .LVU760
@ armwave.c:463:     g_armwave_state.test_wave_buffer = calloc(g_armwave_state.wave_length * g_armwave_state.waves_max, nsets);
	.loc 1 463 75 is_stmt 0 view .LVU761
	ldr	r3, [r4, #76]	@ g_armwave_state.wave_length, g_armwave_state.wave_length
	ldr	r5, [r4, #60]	@ g_armwave_state.waves_max, g_armwave_state.waves_max
@ armwave.c:463:     g_armwave_state.test_wave_buffer = calloc(g_armwave_state.wave_length * g_armwave_state.waves_max, nsets);
	.loc 1 463 40 view .LVU762
	mov	r1, r6	@, nsets
@ armwave.c:463:     g_armwave_state.test_wave_buffer = calloc(g_armwave_state.wave_length * g_armwave_state.waves_max, nsets);
	.loc 1 463 75 view .LVU763
	mul	r5, r5, r3	@ _4, g_armwave_state.waves_max, g_armwave_state.wave_length
@ armwave.c:463:     g_armwave_state.test_wave_buffer = calloc(g_armwave_state.wave_length * g_armwave_state.waves_max, nsets);
	.loc 1 463 40 view .LVU764
	mov	r0, r5	@, _4
	bl	calloc		@
.LVL164:
@ armwave.c:465:     if(g_armwave_state.test_wave_buffer == NULL) {
	.loc 1 465 7 view .LVU765
	cmp	r0, #0	@ tmp124,
@ armwave.c:463:     g_armwave_state.test_wave_buffer = calloc(g_armwave_state.wave_length * g_armwave_state.waves_max, nsets);
	.loc 1 463 38 view .LVU766
	str	r0, [r4, #24]	@ tmp124, g_armwave_state.test_wave_buffer
	.loc 1 465 5 is_stmt 1 view .LVU767
@ armwave.c:465:     if(g_armwave_state.test_wave_buffer == NULL) {
	.loc 1 465 7 is_stmt 0 view .LVU768
	popne	{r4, r5, r6, pc}	@
	.loc 1 466 9 is_stmt 1 view .LVU769
	mul	r1, r6, r5	@, nsets, _4
	mov	r2, r6	@, nsets
	ldr	r0, .L160+4	@,
@ armwave.c:470: }
	.loc 1 470 1 is_stmt 0 view .LVU770
	pop	{r4, r5, r6, lr}	@
	.cfi_restore 14
	.cfi_restore 6
	.cfi_restore 5
	.cfi_restore 4
	.cfi_def_cfa_offset 0
.LVL165:
@ armwave.c:466:         printf("armwave_test_buffer_alloc: failed to allocate test wave buffer (%d bytes, %d sets)\n", \
	.loc 1 466 9 view .LVU771
	b	printf		@
.LVL166:
.L161:
	.loc 1 466 9 view .LVU772
	.align	2
.L160:
	.word	g_armwave_state
	.word	.LC18
	.cfi_endproc
.LFE73:
	.size	armwave_test_buffer_alloc, .-armwave_test_buffer_alloc
	.align	2
	.global	armwave_fill_pixbuf_into_pybuffer
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_fill_pixbuf_into_pybuffer, %function
armwave_fill_pixbuf_into_pybuffer:
.LVL167:
.LFB74:
	.loc 1 476 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 48
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 477 5 view .LVU774
	.loc 1 478 5 view .LVU775
	.loc 1 482 5 view .LVU776
@ armwave.c:476: {
	.loc 1 476 1 is_stmt 0 view .LVU777
	str	lr, [sp, #-4]!	@,
	.cfi_def_cfa_offset 4
	.cfi_offset 14, -4
	sub	sp, sp, #52	@,,
	.cfi_def_cfa_offset 56
@ armwave.c:482:     ret = PyObject_GetBuffer(buf_obj, &buffer, PyBUF_SIMPLE | PyBUF_WRITABLE);
	.loc 1 482 11 view .LVU778
	add	r1, sp, #4	@ tmp131,,
	mov	r2, #1	@,
	bl	PyObject_GetBuffer		@
.LVL168:
	.loc 1 484 5 is_stmt 1 view .LVU779
@ armwave.c:484:     if(ret != 0) {
	.loc 1 484 7 is_stmt 0 view .LVU780
	cmp	r0, #0	@,
	bne	.L166		@,
	.loc 1 491 5 is_stmt 1 view .LVU781
	ldr	r0, [sp, #4]	@, buffer.buf
.LVL169:
	.loc 1 491 5 is_stmt 0 view .LVU782
	bl	armwave_fill_pixbuf_scaled		@
.LVL170:
	.loc 1 494 5 is_stmt 1 view .LVU783
	add	r0, sp, #4	@ tmp132,,
	bl	PyBuffer_Release		@
.LVL171:
	.loc 1 497 5 view .LVU784
.LBB39:
.LBI39:
	.file 2 "/usr/local/include/python3.8/object.h"
	.loc 2 456 20 view .LVU785
.LBB40:
	.loc 2 458 21 view .LVU786
	.loc 2 459 5 view .LVU787
@ /usr/local/include/python3.8/object.h:459:     op->ob_refcnt++;
	.loc 2 459 18 is_stmt 0 view .LVU788
	ldr	r3, .L167	@ tmp126,
.LBE40:
.LBE39:
@ armwave.c:497:     Py_RETURN_TRUE;
	.loc 1 497 5 view .LVU789
	mov	r0, r3	@ <retval>, tmp126
.LBB42:
.LBB41:
@ /usr/local/include/python3.8/object.h:459:     op->ob_refcnt++;
	.loc 2 459 18 view .LVU790
	ldr	r2, [r3]	@ MEM[(Py_ssize_t *)&_Py_TrueStruct],
	add	r2, r2, #1	@ tmp128, MEM[(Py_ssize_t *)&_Py_TrueStruct],
	str	r2, [r3]	@ tmp128,
.LBE41:
.LBE42:
@ armwave.c:498: }
	.loc 1 498 1 view .LVU791
	add	sp, sp, #52	@,,
	.cfi_remember_state
	.cfi_def_cfa_offset 4
	@ sp needed	@
	ldr	pc, [sp], #4	@
.LVL172:
.L166:
	.cfi_restore_state
	.loc 1 485 9 is_stmt 1 view .LVU792
	ldr	r0, .L167+4	@,
.LVL173:
	.loc 1 485 9 is_stmt 0 view .LVU793
	bl	puts		@
.LVL174:
	.loc 1 486 9 is_stmt 1 view .LVU794
.LBB43:
.LBI43:
	.loc 2 456 20 view .LVU795
.LBB44:
	.loc 2 458 21 view .LVU796
	.loc 2 459 5 view .LVU797
@ /usr/local/include/python3.8/object.h:459:     op->ob_refcnt++;
	.loc 2 459 18 is_stmt 0 view .LVU798
	ldr	r3, .L167+8	@ tmp120,
.LBE44:
.LBE43:
@ armwave.c:486:         Py_RETURN_FALSE;
	.loc 1 486 9 view .LVU799
	mov	r0, r3	@ <retval>, tmp120
.LBB46:
.LBB45:
@ /usr/local/include/python3.8/object.h:459:     op->ob_refcnt++;
	.loc 2 459 18 view .LVU800
	ldr	r2, [r3]	@ MEM[(Py_ssize_t *)&_Py_FalseStruct],
	add	r2, r2, #1	@ tmp122, MEM[(Py_ssize_t *)&_Py_FalseStruct],
	str	r2, [r3]	@ tmp122,
.LBE45:
.LBE46:
@ armwave.c:498: }
	.loc 1 498 1 view .LVU801
	add	sp, sp, #52	@,,
	.cfi_def_cfa_offset 4
	@ sp needed	@
	ldr	pc, [sp], #4	@
.L168:
	.align	2
.L167:
	.word	_Py_TrueStruct
	.word	.LC19
	.word	_Py_FalseStruct
	.cfi_endproc
.LFE74:
	.size	armwave_fill_pixbuf_into_pybuffer, .-armwave_fill_pixbuf_into_pybuffer
	.align	2
	.global	armwave_test_create_am_sine
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_test_create_am_sine, %function
armwave_test_create_am_sine:
.LVL175:
.LFB75:
	.loc 1 508 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 509 5 view .LVU803
	.loc 1 510 5 view .LVU804
@ armwave.c:508: {
	.loc 1 508 1 is_stmt 0 view .LVU805
	push	{r4, r5, r6, r7, r8, r9, r10, lr}	@
	.cfi_def_cfa_offset 32
	.cfi_offset 4, -32
	.cfi_offset 5, -28
	.cfi_offset 6, -24
	.cfi_offset 7, -20
	.cfi_offset 8, -16
	.cfi_offset 9, -12
	.cfi_offset 10, -8
	.cfi_offset 14, -4
@ armwave.c:510:     float _1_waves_mod = mod * (1.0f / g_armwave_state.waves);
	.loc 1 510 38 view .LVU806
	vldr.32	s14, .L200	@ tmp180,
@ armwave.c:510:     float _1_waves_mod = mod * (1.0f / g_armwave_state.waves);
	.loc 1 510 55 view .LVU807
	ldr	r5, .L200+28	@ tmp250,
@ armwave.c:508: {
	.loc 1 508 1 view .LVU808
	vpush.64	{d8, d9, d10, d11, d12, d13}	@
	.cfi_def_cfa_offset 80
	.cfi_offset 80, -80
	.cfi_offset 81, -76
	.cfi_offset 82, -72
	.cfi_offset 83, -68
	.cfi_offset 84, -64
	.cfi_offset 85, -60
	.cfi_offset 86, -56
	.cfi_offset 87, -52
	.cfi_offset 88, -48
	.cfi_offset 89, -44
	.cfi_offset 90, -40
	.cfi_offset 91, -36
	mov	r9, r0	@ sets, sets
@ armwave.c:510:     float _1_waves_mod = mod * (1.0f / g_armwave_state.waves);
	.loc 1 510 38 view .LVU809
	vldr.32	s15, [r5, #56]	@ int	@ tmp264, g_armwave_state.waves
@ armwave.c:514:     g_armwave_state.test_wave_buffer_stride = (g_armwave_state.waves * g_armwave_state.wave_stride);
	.loc 1 514 70 view .LVU810
	ldr	r3, [r5, #52]	@ g_armwave_state.wave_stride, g_armwave_state.wave_stride
	ldr	r2, [r5, #56]	@ tmp265, g_armwave_state.waves
.LBB49:
.LBB50:
@ armwave.c:457:     if(g_armwave_state.test_wave_buffer != NULL) {
	.loc 1 457 23 view .LVU811
	ldr	r0, [r5, #24]	@ _82, g_armwave_state.test_wave_buffer
.LVL176:
	.loc 1 457 23 view .LVU812
.LBE50:
.LBE49:
@ armwave.c:510:     float _1_waves_mod = mod * (1.0f / g_armwave_state.waves);
	.loc 1 510 38 view .LVU813
	vcvt.f32.s32	s15, s15	@ tmp178, tmp264
@ armwave.c:514:     g_armwave_state.test_wave_buffer_stride = (g_armwave_state.waves * g_armwave_state.wave_stride);
	.loc 1 514 70 view .LVU814
	mul	r3, r2, r3	@ tmp183, tmp265, g_armwave_state.wave_stride
@ armwave.c:508: {
	.loc 1 508 1 view .LVU815
	vmov.f32	s23, s1	@ noise_fraction, noise_fraction
.LBB54:
.LBB51:
@ armwave.c:457:     if(g_armwave_state.test_wave_buffer != NULL) {
	.loc 1 457 7 view .LVU816
	cmp	r0, #0	@ _82,
.LBE51:
.LBE54:
@ armwave.c:508: {
	.loc 1 508 1 view .LVU817
	sub	sp, sp, #8	@,,
	.cfi_def_cfa_offset 88
@ armwave.c:515:     g_armwave_state.test_wave_buffer_nsets = sets;
	.loc 1 515 44 view .LVU818
	str	r9, [r5, #32]	@ sets, g_armwave_state.test_wave_buffer_nsets
@ armwave.c:514:     g_armwave_state.test_wave_buffer_stride = (g_armwave_state.waves * g_armwave_state.wave_stride);
	.loc 1 514 45 view .LVU819
	str	r3, [r5, #28]	@ tmp183, g_armwave_state.test_wave_buffer_stride
@ armwave.c:510:     float _1_waves_mod = mod * (1.0f / g_armwave_state.waves);
	.loc 1 510 38 view .LVU820
	vdiv.f32	s26, s14, s15	@ tmp179, tmp180, tmp178
@ armwave.c:510:     float _1_waves_mod = mod * (1.0f / g_armwave_state.waves);
	.loc 1 510 11 view .LVU821
	vmul.f32	s26, s26, s0	@ _1_waves_mod, tmp179, mod
.LVL177:
	.loc 1 511 5 is_stmt 1 view .LVU822
	.loc 1 512 5 view .LVU823
	.loc 1 514 5 view .LVU824
	.loc 1 515 5 view .LVU825
	.loc 1 516 5 view .LVU826
.LBB55:
.LBI49:
	.loc 1 455 6 view .LVU827
.LBB52:
	.loc 1 457 5 view .LVU828
@ armwave.c:457:     if(g_armwave_state.test_wave_buffer != NULL) {
	.loc 1 457 7 is_stmt 0 view .LVU829
	beq	.L170		@,
	.loc 1 458 9 is_stmt 1 view .LVU830
	bl	free		@
.LVL178:
.L170:
	.loc 1 463 5 view .LVU831
@ armwave.c:463:     g_armwave_state.test_wave_buffer = calloc(g_armwave_state.wave_length * g_armwave_state.waves_max, nsets);
	.loc 1 463 75 is_stmt 0 view .LVU832
	ldr	r3, [r5, #76]	@ g_armwave_state.wave_length, g_armwave_state.wave_length
	ldr	r4, [r5, #60]	@ g_armwave_state.waves_max, g_armwave_state.waves_max
@ armwave.c:463:     g_armwave_state.test_wave_buffer = calloc(g_armwave_state.wave_length * g_armwave_state.waves_max, nsets);
	.loc 1 463 40 view .LVU833
	mov	r1, r9	@, sets
@ armwave.c:463:     g_armwave_state.test_wave_buffer = calloc(g_armwave_state.wave_length * g_armwave_state.waves_max, nsets);
	.loc 1 463 75 view .LVU834
	mul	r4, r4, r3	@ _85, g_armwave_state.waves_max, g_armwave_state.wave_length
@ armwave.c:463:     g_armwave_state.test_wave_buffer = calloc(g_armwave_state.wave_length * g_armwave_state.waves_max, nsets);
	.loc 1 463 40 view .LVU835
	mov	r0, r4	@, _85
	bl	calloc		@
.LVL179:
@ armwave.c:465:     if(g_armwave_state.test_wave_buffer == NULL) {
	.loc 1 465 7 view .LVU836
	cmp	r0, #0	@ tmp191,
@ armwave.c:463:     g_armwave_state.test_wave_buffer = calloc(g_armwave_state.wave_length * g_armwave_state.waves_max, nsets);
	.loc 1 463 38 view .LVU837
	str	r0, [r5, #24]	@ tmp191, g_armwave_state.test_wave_buffer
	.loc 1 465 5 is_stmt 1 view .LVU838
@ armwave.c:465:     if(g_armwave_state.test_wave_buffer == NULL) {
	.loc 1 465 7 is_stmt 0 view .LVU839
	beq	.L199		@,
.L171:
.LVL180:
	.loc 1 465 7 view .LVU840
.LBE52:
.LBE55:
@ armwave.c:518:     for(s = 0; s < sets; s++) {
	.loc 1 518 5 discriminator 1 view .LVU841
	cmp	r9, #0	@ sets,
	ble	.L169		@,
@ armwave.c:537:                 v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 537 45 view .LVU842
	vldr.32	s16, .L200	@ tmp258,
@ armwave.c:540:                 g_armwave_state.test_wave_buffer[x + (w * g_armwave_state.wave_stride) + set_offset] = (uint8_t)MIN(MAX(128 + (v * 127), 0), 255);
	.loc 1 540 104 view .LVU843
	vldr.32	s24, .L200+4	@ tmp259,
@ armwave.c:518:     for(s = 0; s < sets; s++) {
	.loc 1 518 5 view .LVU844
	mov	r7, #0	@ set_offset,
@ armwave.c:519:         printf("Calculating test set %d\n", s);
	.loc 1 519 9 view .LVU845
	ldr	r10, .L200+32	@ tmp257,
@ armwave.c:518:     for(s = 0; s < sets; s++) {
	.loc 1 518 5 view .LVU846
	mov	r8, r7	@ s, set_offset
.LVL181:
.L172:
	.loc 1 519 9 is_stmt 1 view .LVU847
	mov	r1, r8	@, s
	mov	r0, r10	@, tmp257
	bl	printf		@
.LVL182:
	.loc 1 521 9 view .LVU848
@ armwave.c:521:         for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 521 39 is_stmt 0 view .LVU849
	ldr	r2, [r5, #56]	@ prephitmp_120, g_armwave_state.waves
@ armwave.c:521:         for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 521 9 view .LVU850
	cmp	r2, #0	@ prephitmp_120,
	ble	.L173		@,
@ armwave.c:523:             mod_val = 0.5f + (_1_waves_mod * w);
	.loc 1 523 21 view .LVU851
	vldr.32	s25, .L200+8	@ tmp200,
@ armwave.c:537:                 v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 537 33 view .LVU852
	vldr.32	s22, .L200+12	@ tmp253,
@ armwave.c:535:                 xnoise = (rand() & 0xffff) / 6553500.0f;
	.loc 1 535 24 view .LVU853
	vldr.32	s21, .L200+16	@ tmp254,
	ldr	r3, [r5, #76]	@ prephitmp_124, g_armwave_state.wave_length
@ armwave.c:521:         for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 521 15 view .LVU854
	mov	r6, #0	@ w,
.LVL183:
.L180:
	.loc 1 523 13 is_stmt 1 view .LVU855
@ armwave.c:523:             mod_val = 0.5f + (_1_waves_mod * w);
	.loc 1 523 21 is_stmt 0 view .LVU856
	vmov	s15, r6	@ int	@ w, w
	vmov.f32	s18, s25	@ mod_val, tmp200
@ armwave.c:525:             for(x = 0; x < g_armwave_state.wave_length; x++) {
	.loc 1 525 13 view .LVU857
	cmp	r3, #0	@ prephitmp_124,
@ armwave.c:523:             mod_val = 0.5f + (_1_waves_mod * w);
	.loc 1 523 21 view .LVU858
	vcvt.f32.s32	s15, s15	@ tmp198, w
	vmla.f32	s18, s15, s26	@ mod_val, tmp198, _1_waves_mod
.LVL184:
	.loc 1 525 13 is_stmt 1 view .LVU859
	.loc 1 525 13 is_stmt 0 view .LVU860
	ble	.L174		@,
	vcvt.f64.f32	d9, s18	@ tmp249, mod_val
@ armwave.c:540:                 g_armwave_state.test_wave_buffer[x + (w * g_armwave_state.wave_stride) + set_offset] = (uint8_t)MIN(MAX(128 + (v * 127), 0), 255);
	.loc 1 540 113 view .LVU861
	vldr.32	s20, .L200+20	@ tmp229,
	vldr.32	s17, .L200+24	@ tmp230,
@ armwave.c:525:             for(x = 0; x < g_armwave_state.wave_length; x++) {
	.loc 1 525 19 view .LVU862
	mov	r4, #0	@ x,
.LVL185:
.L179:
	.loc 1 526 17 is_stmt 1 view .LVU863
@ armwave.c:526:                 noise  = ((rand() & 0xffff) * noise_fraction);
	.loc 1 526 28 is_stmt 0 view .LVU864
	bl	rand		@
.LVL186:
@ armwave.c:526:                 noise  = ((rand() & 0xffff) * noise_fraction);
	.loc 1 526 35 view .LVU865
	uxth	r0, r0	@ tmp201,
@ armwave.c:526:                 noise  = ((rand() & 0xffff) * noise_fraction);
	.loc 1 526 45 view .LVU866
	vmov	s15, r0	@ int	@ tmp201, tmp201
	vcvt.f32.s32	s15, s15	@ tmp203, tmp201
@ armwave.c:526:                 noise  = ((rand() & 0xffff) * noise_fraction);
	.loc 1 526 24 view .LVU867
	vmul.f32	s15, s15, s23	@ noise, tmp203, noise_fraction
.LVL187:
	.loc 1 527 17 is_stmt 1 view .LVU868
@ armwave.c:527:                 noise *= noise;
	.loc 1 527 23 is_stmt 0 view .LVU869
	vmul.f32	s15, s15, s15	@ noise, noise, noise
.LVL188:
	.loc 1 528 17 is_stmt 1 view .LVU870
@ armwave.c:528:                 noise *= noise;
	.loc 1 528 23 is_stmt 0 view .LVU871
	vmul.f32	s15, s15, s15	@ noise, noise, noise
.LVL189:
	.loc 1 529 17 is_stmt 1 view .LVU872
@ armwave.c:529:                 noise *= noise;
	.loc 1 529 23 is_stmt 0 view .LVU873
	vmul.f32	s27, s15, s15	@ noise, noise, noise
.LVL190:
	.loc 1 531 17 is_stmt 1 view .LVU874
@ armwave.c:531:                 if((rand() & 0xffff) > 0x7fff)
	.loc 1 531 21 is_stmt 0 view .LVU875
	bl	rand		@
.LVL191:
	.loc 1 532 21 is_stmt 1 view .LVU876
@ armwave.c:531:                 if((rand() & 0xffff) > 0x7fff)
	.loc 1 531 19 is_stmt 0 view .LVU877
	tst	r0, #32768	@,
@ armwave.c:532:                     noise = -noise;
	.loc 1 532 27 view .LVU878
	vnegne.f32	s27, s27	@ noise, noise
.LVL192:
	.loc 1 534 17 is_stmt 1 view .LVU879
	.loc 1 535 17 view .LVU880
@ armwave.c:535:                 xnoise = (rand() & 0xffff) / 6553500.0f;
	.loc 1 535 27 is_stmt 0 view .LVU881
	bl	rand		@
.LVL193:
	.loc 1 537 17 is_stmt 1 view .LVU882
@ armwave.c:537:                 v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 537 45 is_stmt 0 view .LVU883
	vldr.32	s14, [r5, #76]	@ int	@ tmp270, g_armwave_state.wave_length
@ armwave.c:537:                 v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 537 33 view .LVU884
	vmov	s15, r4	@ int	@ x, x
@ armwave.c:537:                 v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 537 45 view .LVU885
	vcvt.f32.s32	s14, s14	@ tmp206, tmp270
@ armwave.c:537:                 v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 537 33 view .LVU886
	vcvt.f32.s32	s15, s15	@ tmp210, x
@ armwave.c:537:                 v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 537 45 view .LVU887
	vdiv.f32	s13, s16, s14	@ tmp208, tmp258, tmp206
@ armwave.c:537:                 v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 537 33 view .LVU888
	vmul.f32	s15, s15, s22	@ tmp211, tmp210, tmp253
@ armwave.c:535:                 xnoise = (rand() & 0xffff) / 6553500.0f;
	.loc 1 535 34 view .LVU889
	uxth	r0, r0	@ tmp214,
.LVL194:
@ armwave.c:535:                 xnoise = (rand() & 0xffff) / 6553500.0f;
	.loc 1 535 44 view .LVU890
	vmov	s14, r0	@ int	@ tmp214, tmp214
	vcvt.f32.s32	s14, s14	@ tmp216, tmp214
@ armwave.c:535:                 xnoise = (rand() & 0xffff) / 6553500.0f;
	.loc 1 535 24 view .LVU891
	vdiv.f32	s0, s14, s21	@ xnoise, tmp216, tmp254
@ armwave.c:537:                 v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 537 77 view .LVU892
	vmla.f32	s0, s13, s15	@ tmp219, tmp208, tmp211
@ armwave.c:537:                 v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 537 22 view .LVU893
	vcvt.f64.f32	d0, s0	@, tmp219
	bl	sin		@
.LVL195:
	.loc 1 540 17 is_stmt 1 view .LVU894
@ armwave.c:534:                 noise += 1.0f;
	.loc 1 534 23 is_stmt 0 view .LVU895
	vadd.f32	s15, s27, s16	@ noise, noise, tmp258
@ armwave.c:540:                 g_armwave_state.test_wave_buffer[x + (w * g_armwave_state.wave_stride) + set_offset] = (uint8_t)MIN(MAX(128 + (v * 127), 0), 255);
	.loc 1 540 104 view .LVU896
	mov	r1, #0	@ iftmp.22_50,
@ armwave.c:537:                 v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 537 98 view .LVU897
	vcvt.f64.f32	d7, s15	@ tmp225, noise
@ armwave.c:537:                 v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 537 87 view .LVU898
	vmul.f64	d0, d9, d0	@ tmp222, tmp249,
@ armwave.c:537:                 v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 537 98 view .LVU899
	vmul.f64	d7, d0, d7	@ tmp226, tmp222, tmp225
@ armwave.c:537:                 v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 537 19 view .LVU900
	vcvt.f32.f64	s14, d7	@ v, tmp226
@ armwave.c:540:                 g_armwave_state.test_wave_buffer[x + (w * g_armwave_state.wave_stride) + set_offset] = (uint8_t)MIN(MAX(128 + (v * 127), 0), 255);
	.loc 1 540 113 view .LVU901
	vmov.f32	s15, s17	@ _33, tmp230
	vmla.f32	s15, s14, s20	@ _33, v, tmp229
	vcmpe.f32	s15, #0	@ _33
	vmrs	APSR_nzcv, FPSCR
	ble	.L176		@,
@ armwave.c:540:                 g_armwave_state.test_wave_buffer[x + (w * g_armwave_state.wave_stride) + set_offset] = (uint8_t)MIN(MAX(128 + (v * 127), 0), 255);
	.loc 1 540 104 discriminator 1 view .LVU902
	vcmpe.f32	s15, s24	@ _33, tmp259
	mov	r1, #255	@ iftmp.22_50,
	vmrs	APSR_nzcv, FPSCR
	vcvtmi.u32.f32	s15, s15	@ tmp234, _33
	vstrmi.32	s15, [sp, #4]	@ int	@ tmp234, %sfp
	ldrbmi	r1, [sp, #4]	@ zero_extendqisi2	@ iftmp.22_50, %sfp
.L176:
@ armwave.c:540:                 g_armwave_state.test_wave_buffer[x + (w * g_armwave_state.wave_stride) + set_offset] = (uint8_t)MIN(MAX(128 + (v * 127), 0), 255);
	.loc 1 540 57 discriminator 12 view .LVU903
	ldr	r2, [r5, #52]	@ g_armwave_state.wave_stride, g_armwave_state.wave_stride
@ armwave.c:540:                 g_armwave_state.test_wave_buffer[x + (w * g_armwave_state.wave_stride) + set_offset] = (uint8_t)MIN(MAX(128 + (v * 127), 0), 255);
	.loc 1 540 102 discriminator 12 view .LVU904
	ldr	r3, [r5, #24]	@ g_armwave_state.test_wave_buffer, g_armwave_state.test_wave_buffer
	mla	r2, r2, r6, r7	@ tmp241, g_armwave_state.wave_stride, w, set_offset
	add	r3, r3, r4	@ tmp240, g_armwave_state.test_wave_buffer, x
@ armwave.c:525:             for(x = 0; x < g_armwave_state.wave_length; x++) {
	.loc 1 525 58 discriminator 12 view .LVU905
	add	r4, r4, #1	@ x, x,
.LVL196:
@ armwave.c:540:                 g_armwave_state.test_wave_buffer[x + (w * g_armwave_state.wave_stride) + set_offset] = (uint8_t)MIN(MAX(128 + (v * 127), 0), 255);
	.loc 1 540 102 discriminator 12 view .LVU906
	strb	r1, [r2, r3]	@ iftmp.22_50, *_40
@ armwave.c:525:             for(x = 0; x < g_armwave_state.wave_length; x++) {
	.loc 1 525 43 discriminator 12 view .LVU907
	ldr	r3, [r5, #76]	@ prephitmp_124, g_armwave_state.wave_length
@ armwave.c:525:             for(x = 0; x < g_armwave_state.wave_length; x++) {
	.loc 1 525 13 discriminator 12 view .LVU908
	cmp	r3, r4	@ prephitmp_124, x
	bgt	.L179		@,
	ldr	r2, [r5, #56]	@ prephitmp_120, g_armwave_state.waves
.LVL197:
.L174:
@ armwave.c:521:         for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 521 48 discriminator 2 view .LVU909
	add	r6, r6, #1	@ w, w,
.LVL198:
@ armwave.c:521:         for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 521 9 discriminator 2 view .LVU910
	cmp	r6, r2	@ w, prephitmp_120
	blt	.L180		@,
.LVL199:
.L173:
	.loc 1 544 9 is_stmt 1 discriminator 2 view .LVU911
@ armwave.c:544:         set_offset += (g_armwave_state.waves * g_armwave_state.wave_stride);
	.loc 1 544 46 is_stmt 0 discriminator 2 view .LVU912
	ldr	r3, [r5, #52]	@ g_armwave_state.wave_stride, g_armwave_state.wave_stride
@ armwave.c:518:     for(s = 0; s < sets; s++) {
	.loc 1 518 27 discriminator 2 view .LVU913
	add	r8, r8, #1	@ s, s,
.LVL200:
@ armwave.c:518:     for(s = 0; s < sets; s++) {
	.loc 1 518 5 discriminator 2 view .LVU914
	cmp	r9, r8	@ sets, s
@ armwave.c:544:         set_offset += (g_armwave_state.waves * g_armwave_state.wave_stride);
	.loc 1 544 20 discriminator 2 view .LVU915
	mla	r7, r3, r2, r7	@ set_offset, g_armwave_state.wave_stride, prephitmp_120, set_offset
.LVL201:
@ armwave.c:518:     for(s = 0; s < sets; s++) {
	.loc 1 518 5 discriminator 2 view .LVU916
	bne	.L172		@,
.LVL202:
.L169:
@ armwave.c:546: }
	.loc 1 546 1 view .LVU917
	add	sp, sp, #8	@,,
	.cfi_remember_state
	.cfi_def_cfa_offset 80
	@ sp needed	@
	vldm	sp!, {d8-d13}	@,
	.cfi_restore 90
	.cfi_restore 91
	.cfi_restore 88
	.cfi_restore 89
	.cfi_restore 86
	.cfi_restore 87
	.cfi_restore 84
	.cfi_restore 85
	.cfi_restore 82
	.cfi_restore 83
	.cfi_restore 80
	.cfi_restore 81
	.cfi_def_cfa_offset 32
.LVL203:
	.loc 1 546 1 view .LVU918
	pop	{r4, r5, r6, r7, r8, r9, r10, pc}	@
.LVL204:
.L199:
	.cfi_restore_state
.LBB56:
.LBB53:
	.loc 1 466 9 is_stmt 1 view .LVU919
	mul	r1, r4, r9	@, _85, sets
	mov	r2, r9	@, sets
	ldr	r0, .L200+36	@,
	bl	printf		@
.LVL205:
	.loc 1 468 9 view .LVU920
	b	.L171		@
.L201:
	.align	2
.L200:
	.word	1065353216
	.word	1132396544
	.word	1056964608
	.word	1086911939
	.word	1254620984
	.word	1123942400
	.word	1124073472
	.word	g_armwave_state
	.word	.LC20
	.word	.LC18
.LBE53:
.LBE56:
	.cfi_endproc
.LFE75:
	.size	armwave_test_create_am_sine, .-armwave_test_create_am_sine
	.align	2
	.global	armwave_cleanup
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_cleanup, %function
armwave_cleanup:
.LFB76:
	.loc 1 596 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 597 5 view .LVU922
@ armwave.c:596: {
	.loc 1 596 1 is_stmt 0 view .LVU923
	push	{r4, lr}	@
	.cfi_def_cfa_offset 8
	.cfi_offset 4, -8
	.cfi_offset 14, -4
@ armwave.c:597:     free(g_armwave_state.out_pixbuf);
	.loc 1 597 25 view .LVU924
	ldr	r4, .L204	@ tmp114,
@ armwave.c:597:     free(g_armwave_state.out_pixbuf);
	.loc 1 597 5 view .LVU925
	ldr	r0, [r4, #104]	@, g_armwave_state.out_pixbuf
	bl	free		@
.LVL206:
	.loc 1 598 5 is_stmt 1 view .LVU926
	ldr	r0, [r4, #4]	@, g_armwave_state.ch1_buffer
	bl	free		@
.LVL207:
	.loc 1 599 5 view .LVU927
	ldr	r0, [r4, #132]	@, g_armwave_state.xcoord_to_xpixel
	bl	free		@
.LVL208:
	.loc 1 600 5 view .LVU928
	ldr	r0, [r4, #24]	@, g_armwave_state.test_wave_buffer
	bl	free		@
.LVL209:
	.loc 1 602 5 view .LVU929
@ armwave.c:602:     g_armwave_state.out_pixbuf = NULL;
	.loc 1 602 32 is_stmt 0 view .LVU930
	mov	r3, #0	@ tmp123,
	str	r3, [r4, #104]	@ tmp123, g_armwave_state.out_pixbuf
	.loc 1 603 5 is_stmt 1 view .LVU931
@ armwave.c:603:     g_armwave_state.ch1_buffer = NULL;
	.loc 1 603 32 is_stmt 0 view .LVU932
	str	r3, [r4, #4]	@ tmp123, g_armwave_state.ch1_buffer
	.loc 1 604 5 is_stmt 1 view .LVU933
@ armwave.c:604:     g_armwave_state.xcoord_to_xpixel = NULL;
	.loc 1 604 38 is_stmt 0 view .LVU934
	str	r3, [r4, #132]	@ tmp123, g_armwave_state.xcoord_to_xpixel
	.loc 1 605 5 is_stmt 1 view .LVU935
@ armwave.c:605:     g_armwave_state.test_wave_buffer = NULL;
	.loc 1 605 38 is_stmt 0 view .LVU936
	str	r3, [r4, #24]	@ tmp123, g_armwave_state.test_wave_buffer
@ armwave.c:606: }
	.loc 1 606 1 view .LVU937
	pop	{r4, pc}	@
.L205:
	.align	2
.L204:
	.word	g_armwave_state
	.cfi_endproc
.LFE76:
	.size	armwave_cleanup, .-armwave_cleanup
	.comm	gamma_table,256,4
	.comm	g_armwave_state,136,4
	.section	.rodata
	.align	2
	.set	.LANCHOR0,. + 0
	.type	__PRETTY_FUNCTION__.17096, %object
	.size	__PRETTY_FUNCTION__.17096, 27
__PRETTY_FUNCTION__.17096:
	.ascii	"armwave_fill_pixbuf_scaled\000"
	.space	1
	.type	__PRETTY_FUNCTION__.17120, %object
	.size	__PRETTY_FUNCTION__.17120, 21
__PRETTY_FUNCTION__.17120:
	.ascii	"armwave_setup_render\000"
	.space	3
	.type	__PRETTY_FUNCTION__.17127, %object
	.size	__PRETTY_FUNCTION__.17127, 25
__PRETTY_FUNCTION__.17127:
	.ascii	"armwave_set_wave_pointer\000"
	.space	3
	.type	__PRETTY_FUNCTION__.17134, %object
	.size	__PRETTY_FUNCTION__.17134, 29
__PRETTY_FUNCTION__.17134:
	.ascii	"armwave_set_wave_pointer_u32\000"
	.section	.rodata.str1.4,"aMS",%progbits,1
	.align	2
.LC0:
	.ascii	"v0.0.1\000"
	.space	1
.LC1:
	.ascii	"armwave version: %s\012\000"
	.space	3
.LC2:
	.ascii	"armwave.c\000"
	.space	2
.LC3:
	.ascii	"out_buffer != NULL\000"
	.space	1
.LC4:
	.ascii	"s=%d e=%d w=%d ws=%d tw=%d th=%d rf=0x%08x\012\000"
.LC5:
	.ascii	"start_point < end_point\000"
.LC6:
	.ascii	"ch_buff_size=%d, cmp_x_bitdepth_scale=%d (0x%08x), "
	.ascii	"targ_width=%d, wave_length=%d, scaler=%d\012\000"
	.space	3
.LC7:
	.ascii	"g_armwave_state.ch1_buffer != NULL\000"
	.space	1
.LC8:
	.ascii	"g_armwave_state.xcoord_to_xpixel != NULL\000"
	.space	3
.LC9:
	.ascii	"Ptrs: 0x%08x 0x%08x 0x%08x 0x%08x \012\000"
.LC10:
	.ascii	"wave_buffer != NULL\000"
.LC11:
	.ascii	"armwave_set_wave_pointer_as_testbuf: error, nsets e"
	.ascii	"xceeded\000"
	.space	1
.LC12:
	.ascii	"wave_buffer_ptr != 0\000"
	.space	3
.LC13:
	.ascii	"wb\000"
	.space	1
.LC14:
	.ascii	"P3\012\000"
.LC15:
	.ascii	"%d %d\012\000"
	.space	1
.LC16:
	.ascii	"255\012\000"
	.space	3
.LC17:
	.ascii	"%3d %3d %3d\012\000"
	.space	3
.LC18:
	.ascii	"armwave_test_buffer_alloc: failed to allocate test "
	.ascii	"wave buffer (%d bytes, %d sets)\012\000"
.LC19:
	.ascii	"armwave_fill_pixbuf_into_pybuffer: PyObject_GetBuff"
	.ascii	"er() failed, returning PyFalse\000"
	.space	2
.LC20:
	.ascii	"Calculating test set %d\012\000"
	.text
.Letext0:
	.file 3 "/usr/lib/gcc/arm-linux-gnueabihf/8/include/stddef.h"
	.file 4 "/usr/include/arm-linux-gnueabihf/bits/types.h"
	.file 5 "/usr/include/arm-linux-gnueabihf/bits/types/struct_FILE.h"
	.file 6 "/usr/include/arm-linux-gnueabihf/bits/types/FILE.h"
	.file 7 "/usr/include/stdio.h"
	.file 8 "/usr/include/arm-linux-gnueabihf/bits/sys_errlist.h"
	.file 9 "/usr/include/errno.h"
	.file 10 "/usr/include/arm-linux-gnueabihf/bits/stdint-intn.h"
	.file 11 "/usr/include/unistd.h"
	.file 12 "/usr/include/arm-linux-gnueabihf/bits/getopt_core.h"
	.file 13 "/usr/include/arm-linux-gnueabihf/bits/stdint-uintn.h"
	.file 14 "/usr/local/include/python3.8/pyport.h"
	.file 15 "/usr/include/math.h"
	.file 16 "/usr/include/arm-linux-gnueabihf/sys/time.h"
	.file 17 "/usr/include/time.h"
	.file 18 "/usr/local/include/python3.8/pymem.h"
	.file 19 "/usr/local/include/python3.8/cpython/object.h"
	.file 20 "/usr/local/include/python3.8/methodobject.h"
	.file 21 "/usr/local/include/python3.8/descrobject.h"
	.file 22 "/usr/local/include/python3.8/pyhash.h"
	.file 23 "/usr/local/include/python3.8/pydebug.h"
	.file 24 "/usr/local/include/python3.8/bytearrayobject.h"
	.file 25 "/usr/local/include/python3.8/bytesobject.h"
	.file 26 "/usr/local/include/python3.8/unicodeobject.h"
	.file 27 "/usr/local/include/python3.8/cpython/unicodeobject.h"
	.file 28 "/usr/local/include/python3.8/longintrepr.h"
	.file 29 "/usr/local/include/python3.8/longobject.h"
	.file 30 "/usr/local/include/python3.8/boolobject.h"
	.file 31 "/usr/local/include/python3.8/floatobject.h"
	.file 32 "/usr/local/include/python3.8/complexobject.h"
	.file 33 "/usr/local/include/python3.8/rangeobject.h"
	.file 34 "/usr/local/include/python3.8/memoryobject.h"
	.file 35 "/usr/local/include/python3.8/tupleobject.h"
	.file 36 "/usr/local/include/python3.8/listobject.h"
	.file 37 "/usr/local/include/python3.8/dictobject.h"
	.file 38 "/usr/local/include/python3.8/odictobject.h"
	.file 39 "/usr/local/include/python3.8/enumobject.h"
	.file 40 "/usr/local/include/python3.8/setobject.h"
	.file 41 "/usr/local/include/python3.8/moduleobject.h"
	.file 42 "/usr/local/include/python3.8/funcobject.h"
	.file 43 "/usr/local/include/python3.8/classobject.h"
	.file 44 "/usr/local/include/python3.8/fileobject.h"
	.file 45 "/usr/local/include/python3.8/cpython/fileobject.h"
	.file 46 "/usr/local/include/python3.8/pycapsule.h"
	.file 47 "/usr/local/include/python3.8/traceback.h"
	.file 48 "/usr/local/include/python3.8/sliceobject.h"
	.file 49 "/usr/local/include/python3.8/cellobject.h"
	.file 50 "/usr/local/include/python3.8/iterobject.h"
	.file 51 "/usr/local/include/python3.8/pystate.h"
	.file 52 "/usr/local/include/python3.8/cpython/pystate.h"
	.file 53 "/usr/local/include/python3.8/genobject.h"
	.file 54 "/usr/local/include/python3.8/weakrefobject.h"
	.file 55 "/usr/local/include/python3.8/structseq.h"
	.file 56 "/usr/local/include/python3.8/namespaceobject.h"
	.file 57 "/usr/local/include/python3.8/picklebufobject.h"
	.file 58 "/usr/local/include/python3.8/codecs.h"
	.file 59 "/usr/local/include/python3.8/pyerrors.h"
	.file 60 "/usr/local/include/python3.8/context.h"
	.file 61 "/usr/local/include/python3.8/modsupport.h"
	.file 62 "/usr/local/include/python3.8/code.h"
	.file 63 "/usr/local/include/python3.8/pythonrun.h"
	.file 64 "/usr/local/include/python3.8/ceval.h"
	.file 65 "/usr/local/include/python3.8/import.h"
	.file 66 "/usr/local/include/python3.8/bltinmodule.h"
	.file 67 "/usr/local/include/python3.8/pyctype.h"
	.file 68 "/usr/include/malloc.h"
	.file 69 "armwave.h"
	.file 70 "/usr/include/stdlib.h"
	.file 71 "/usr/local/include/python3.8/cpython/abstract.h"
	.file 72 "<built-in>"
	.file 73 "/usr/include/assert.h"
	.file 74 "/usr/include/arm-linux-gnueabihf/bits/mathcalls.h"
	.section	.debug_info,"",%progbits
.Ldebug_info0:
	.4byte	0x368c
	.2byte	0x4
	.4byte	.Ldebug_abbrev0
	.byte	0x4
	.uleb128 0x1
	.4byte	.LASF647
	.byte	0xc
	.4byte	.LASF648
	.4byte	.LASF649
	.4byte	.Ltext0
	.4byte	.Letext0-.Ltext0
	.4byte	.Ldebug_line0
	.uleb128 0x2
	.byte	0x4
	.byte	0x4
	.4byte	.LASF0
	.uleb128 0x3
	.byte	0x4
	.byte	0x5
	.ascii	"int\000"
	.uleb128 0x2
	.byte	0x4
	.byte	0x7
	.4byte	.LASF1
	.uleb128 0x2
	.byte	0x8
	.byte	0x4
	.4byte	.LASF2
	.uleb128 0x4
	.4byte	.LASF7
	.byte	0x3
	.byte	0xd8
	.byte	0x17
	.4byte	0x33
	.uleb128 0x5
	.4byte	0x33
	.uleb128 0x6
	.byte	0x4
	.uleb128 0x2
	.byte	0x1
	.byte	0x8
	.4byte	.LASF3
	.uleb128 0x5
	.4byte	0x54
	.uleb128 0x2
	.byte	0x2
	.byte	0x7
	.4byte	.LASF4
	.uleb128 0x2
	.byte	0x4
	.byte	0x7
	.4byte	.LASF5
	.uleb128 0x2
	.byte	0x1
	.byte	0x6
	.4byte	.LASF6
	.uleb128 0x4
	.4byte	.LASF8
	.byte	0x4
	.byte	0x25
	.byte	0x17
	.4byte	0x54
	.uleb128 0x4
	.4byte	.LASF9
	.byte	0x4
	.byte	0x26
	.byte	0x1a
	.4byte	0x8d
	.uleb128 0x2
	.byte	0x2
	.byte	0x5
	.4byte	.LASF10
	.uleb128 0x4
	.4byte	.LASF11
	.byte	0x4
	.byte	0x27
	.byte	0x1c
	.4byte	0x60
	.uleb128 0x4
	.4byte	.LASF12
	.byte	0x4
	.byte	0x29
	.byte	0x16
	.4byte	0x33
	.uleb128 0x2
	.byte	0x8
	.byte	0x5
	.4byte	.LASF13
	.uleb128 0x4
	.4byte	.LASF14
	.byte	0x4
	.byte	0x2f
	.byte	0x2e
	.4byte	0xbf
	.uleb128 0x2
	.byte	0x8
	.byte	0x7
	.4byte	.LASF15
	.uleb128 0x4
	.4byte	.LASF16
	.byte	0x4
	.byte	0x41
	.byte	0x25
	.4byte	0xac
	.uleb128 0x4
	.4byte	.LASF17
	.byte	0x4
	.byte	0x96
	.byte	0x19
	.4byte	0xde
	.uleb128 0x2
	.byte	0x4
	.byte	0x5
	.4byte	.LASF18
	.uleb128 0x4
	.4byte	.LASF19
	.byte	0x4
	.byte	0x97
	.byte	0x1b
	.4byte	0xc6
	.uleb128 0x4
	.4byte	.LASF20
	.byte	0x4
	.byte	0xbf
	.byte	0x1b
	.4byte	0x2c
	.uleb128 0x7
	.byte	0x4
	.4byte	0x103
	.uleb128 0x2
	.byte	0x1
	.byte	0x8
	.4byte	.LASF21
	.uleb128 0x5
	.4byte	0x103
	.uleb128 0x8
	.4byte	.LASF79
	.byte	0x98
	.byte	0x5
	.byte	0x31
	.byte	0x8
	.4byte	0x296
	.uleb128 0x9
	.4byte	.LASF22
	.byte	0x5
	.byte	0x33
	.byte	0x7
	.4byte	0x2c
	.byte	0
	.uleb128 0x9
	.4byte	.LASF23
	.byte	0x5
	.byte	0x36
	.byte	0x9
	.4byte	0xfd
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF24
	.byte	0x5
	.byte	0x37
	.byte	0x9
	.4byte	0xfd
	.byte	0x8
	.uleb128 0x9
	.4byte	.LASF25
	.byte	0x5
	.byte	0x38
	.byte	0x9
	.4byte	0xfd
	.byte	0xc
	.uleb128 0x9
	.4byte	.LASF26
	.byte	0x5
	.byte	0x39
	.byte	0x9
	.4byte	0xfd
	.byte	0x10
	.uleb128 0x9
	.4byte	.LASF27
	.byte	0x5
	.byte	0x3a
	.byte	0x9
	.4byte	0xfd
	.byte	0x14
	.uleb128 0x9
	.4byte	.LASF28
	.byte	0x5
	.byte	0x3b
	.byte	0x9
	.4byte	0xfd
	.byte	0x18
	.uleb128 0x9
	.4byte	.LASF29
	.byte	0x5
	.byte	0x3c
	.byte	0x9
	.4byte	0xfd
	.byte	0x1c
	.uleb128 0x9
	.4byte	.LASF30
	.byte	0x5
	.byte	0x3d
	.byte	0x9
	.4byte	0xfd
	.byte	0x20
	.uleb128 0x9
	.4byte	.LASF31
	.byte	0x5
	.byte	0x40
	.byte	0x9
	.4byte	0xfd
	.byte	0x24
	.uleb128 0x9
	.4byte	.LASF32
	.byte	0x5
	.byte	0x41
	.byte	0x9
	.4byte	0xfd
	.byte	0x28
	.uleb128 0x9
	.4byte	.LASF33
	.byte	0x5
	.byte	0x42
	.byte	0x9
	.4byte	0xfd
	.byte	0x2c
	.uleb128 0x9
	.4byte	.LASF34
	.byte	0x5
	.byte	0x44
	.byte	0x16
	.4byte	0x2af
	.byte	0x30
	.uleb128 0x9
	.4byte	.LASF35
	.byte	0x5
	.byte	0x46
	.byte	0x14
	.4byte	0x2b5
	.byte	0x34
	.uleb128 0x9
	.4byte	.LASF36
	.byte	0x5
	.byte	0x48
	.byte	0x7
	.4byte	0x2c
	.byte	0x38
	.uleb128 0x9
	.4byte	.LASF37
	.byte	0x5
	.byte	0x49
	.byte	0x7
	.4byte	0x2c
	.byte	0x3c
	.uleb128 0x9
	.4byte	.LASF38
	.byte	0x5
	.byte	0x4a
	.byte	0xb
	.4byte	0xd2
	.byte	0x40
	.uleb128 0x9
	.4byte	.LASF39
	.byte	0x5
	.byte	0x4d
	.byte	0x12
	.4byte	0x60
	.byte	0x44
	.uleb128 0x9
	.4byte	.LASF40
	.byte	0x5
	.byte	0x4e
	.byte	0xf
	.4byte	0x6e
	.byte	0x46
	.uleb128 0x9
	.4byte	.LASF41
	.byte	0x5
	.byte	0x4f
	.byte	0x8
	.4byte	0x2bb
	.byte	0x47
	.uleb128 0x9
	.4byte	.LASF42
	.byte	0x5
	.byte	0x51
	.byte	0xf
	.4byte	0x2cb
	.byte	0x48
	.uleb128 0x9
	.4byte	.LASF43
	.byte	0x5
	.byte	0x59
	.byte	0xd
	.4byte	0xe5
	.byte	0x50
	.uleb128 0x9
	.4byte	.LASF44
	.byte	0x5
	.byte	0x5b
	.byte	0x17
	.4byte	0x2d6
	.byte	0x58
	.uleb128 0x9
	.4byte	.LASF45
	.byte	0x5
	.byte	0x5c
	.byte	0x19
	.4byte	0x2e1
	.byte	0x5c
	.uleb128 0x9
	.4byte	.LASF46
	.byte	0x5
	.byte	0x5d
	.byte	0x14
	.4byte	0x2b5
	.byte	0x60
	.uleb128 0x9
	.4byte	.LASF47
	.byte	0x5
	.byte	0x5e
	.byte	0x9
	.4byte	0x52
	.byte	0x64
	.uleb128 0x9
	.4byte	.LASF48
	.byte	0x5
	.byte	0x5f
	.byte	0xa
	.4byte	0x41
	.byte	0x68
	.uleb128 0x9
	.4byte	.LASF49
	.byte	0x5
	.byte	0x60
	.byte	0x7
	.4byte	0x2c
	.byte	0x6c
	.uleb128 0x9
	.4byte	.LASF50
	.byte	0x5
	.byte	0x62
	.byte	0x8
	.4byte	0x2e7
	.byte	0x70
	.byte	0
	.uleb128 0x4
	.4byte	.LASF51
	.byte	0x6
	.byte	0x7
	.byte	0x19
	.4byte	0x10f
	.uleb128 0xa
	.4byte	.LASF650
	.byte	0x5
	.byte	0x2b
	.byte	0xe
	.uleb128 0xb
	.4byte	.LASF52
	.uleb128 0x7
	.byte	0x4
	.4byte	0x2aa
	.uleb128 0x7
	.byte	0x4
	.4byte	0x10f
	.uleb128 0xc
	.4byte	0x103
	.4byte	0x2cb
	.uleb128 0xd
	.4byte	0x33
	.byte	0
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x2a2
	.uleb128 0xb
	.4byte	.LASF53
	.uleb128 0x7
	.byte	0x4
	.4byte	0x2d1
	.uleb128 0xb
	.4byte	.LASF54
	.uleb128 0x7
	.byte	0x4
	.4byte	0x2dc
	.uleb128 0xc
	.4byte	0x103
	.4byte	0x2f7
	.uleb128 0xd
	.4byte	0x33
	.byte	0x27
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x10a
	.uleb128 0x5
	.4byte	0x2f7
	.uleb128 0x4
	.4byte	.LASF55
	.byte	0x7
	.byte	0x4d
	.byte	0x13
	.4byte	0xf1
	.uleb128 0xe
	.4byte	.LASF56
	.byte	0x7
	.byte	0x89
	.byte	0xe
	.4byte	0x31a
	.uleb128 0x7
	.byte	0x4
	.4byte	0x296
	.uleb128 0xe
	.4byte	.LASF57
	.byte	0x7
	.byte	0x8a
	.byte	0xe
	.4byte	0x31a
	.uleb128 0xe
	.4byte	.LASF58
	.byte	0x7
	.byte	0x8b
	.byte	0xe
	.4byte	0x31a
	.uleb128 0xe
	.4byte	.LASF59
	.byte	0x8
	.byte	0x1a
	.byte	0xc
	.4byte	0x2c
	.uleb128 0xc
	.4byte	0x2fd
	.4byte	0x34f
	.uleb128 0xf
	.byte	0
	.uleb128 0x5
	.4byte	0x344
	.uleb128 0xe
	.4byte	.LASF60
	.byte	0x8
	.byte	0x1b
	.byte	0x1a
	.4byte	0x34f
	.uleb128 0xe
	.4byte	.LASF61
	.byte	0x8
	.byte	0x1e
	.byte	0xc
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF62
	.byte	0x8
	.byte	0x1f
	.byte	0x1a
	.4byte	0x34f
	.uleb128 0xe
	.4byte	.LASF63
	.byte	0x9
	.byte	0x2d
	.byte	0xe
	.4byte	0xfd
	.uleb128 0xe
	.4byte	.LASF64
	.byte	0x9
	.byte	0x2e
	.byte	0xe
	.4byte	0xfd
	.uleb128 0x4
	.4byte	.LASF65
	.byte	0xa
	.byte	0x19
	.byte	0x13
	.4byte	0x81
	.uleb128 0x7
	.byte	0x4
	.4byte	0x3a2
	.uleb128 0x10
	.uleb128 0x11
	.4byte	.LASF66
	.byte	0xb
	.2byte	0x21f
	.byte	0xf
	.4byte	0x3b0
	.uleb128 0x7
	.byte	0x4
	.4byte	0xfd
	.uleb128 0x11
	.4byte	.LASF67
	.byte	0xb
	.2byte	0x221
	.byte	0xf
	.4byte	0x3b0
	.uleb128 0xe
	.4byte	.LASF68
	.byte	0xc
	.byte	0x24
	.byte	0xe
	.4byte	0xfd
	.uleb128 0xe
	.4byte	.LASF69
	.byte	0xc
	.byte	0x32
	.byte	0xc
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF70
	.byte	0xc
	.byte	0x37
	.byte	0xc
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF71
	.byte	0xc
	.byte	0x3b
	.byte	0xc
	.4byte	0x2c
	.uleb128 0x4
	.4byte	.LASF72
	.byte	0xd
	.byte	0x18
	.byte	0x13
	.4byte	0x75
	.uleb128 0x4
	.4byte	.LASF73
	.byte	0xd
	.byte	0x19
	.byte	0x14
	.4byte	0x94
	.uleb128 0x4
	.4byte	.LASF74
	.byte	0xd
	.byte	0x1a
	.byte	0x14
	.4byte	0xa0
	.uleb128 0x4
	.4byte	.LASF75
	.byte	0xd
	.byte	0x1b
	.byte	0x14
	.4byte	0xb3
	.uleb128 0x4
	.4byte	.LASF76
	.byte	0xe
	.byte	0x69
	.byte	0x19
	.4byte	0x302
	.uleb128 0x4
	.4byte	.LASF77
	.byte	0xe
	.byte	0x72
	.byte	0x14
	.4byte	0x423
	.uleb128 0x11
	.4byte	.LASF78
	.byte	0xf
	.2byte	0x305
	.byte	0xc
	.4byte	0x2c
	.uleb128 0x8
	.4byte	.LASF80
	.byte	0x8
	.byte	0x10
	.byte	0x34
	.byte	0x8
	.4byte	0x470
	.uleb128 0x9
	.4byte	.LASF81
	.byte	0x10
	.byte	0x36
	.byte	0x9
	.4byte	0x2c
	.byte	0
	.uleb128 0x9
	.4byte	.LASF82
	.byte	0x10
	.byte	0x37
	.byte	0x9
	.4byte	0x2c
	.byte	0x4
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x448
	.uleb128 0x12
	.4byte	0x470
	.uleb128 0xc
	.4byte	0xfd
	.4byte	0x48b
	.uleb128 0xd
	.4byte	0x33
	.byte	0x1
	.byte	0
	.uleb128 0xe
	.4byte	.LASF83
	.byte	0x11
	.byte	0x9f
	.byte	0xe
	.4byte	0x47b
	.uleb128 0xe
	.4byte	.LASF84
	.byte	0x11
	.byte	0xa0
	.byte	0xc
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF85
	.byte	0x11
	.byte	0xa1
	.byte	0x11
	.4byte	0xde
	.uleb128 0xe
	.4byte	.LASF86
	.byte	0x11
	.byte	0xa6
	.byte	0xe
	.4byte	0x47b
	.uleb128 0xe
	.4byte	.LASF87
	.byte	0x11
	.byte	0xae
	.byte	0xc
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF80
	.byte	0x11
	.byte	0xaf
	.byte	0x11
	.4byte	0xde
	.uleb128 0x11
	.4byte	.LASF88
	.byte	0x11
	.2byte	0x118
	.byte	0xc
	.4byte	0x2c
	.uleb128 0x13
	.byte	0x7
	.byte	0x4
	.4byte	0x33
	.byte	0x12
	.byte	0x70
	.byte	0xa
	.4byte	0x501
	.uleb128 0x14
	.4byte	.LASF89
	.byte	0
	.uleb128 0x14
	.4byte	.LASF90
	.byte	0x1
	.uleb128 0x14
	.4byte	.LASF91
	.byte	0x2
	.byte	0
	.uleb128 0x8
	.4byte	.LASF92
	.byte	0x10
	.byte	0x12
	.byte	0x6d
	.byte	0x8
	.4byte	0x543
	.uleb128 0x9
	.4byte	.LASF93
	.byte	0x12
	.byte	0x74
	.byte	0x7
	.4byte	0x4e0
	.byte	0
	.uleb128 0x9
	.4byte	.LASF94
	.byte	0x12
	.byte	0x78
	.byte	0x9
	.4byte	0x2c
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF95
	.byte	0x12
	.byte	0x7c
	.byte	0x9
	.4byte	0x2c
	.byte	0x8
	.uleb128 0x9
	.4byte	.LASF96
	.byte	0x12
	.byte	0x80
	.byte	0x9
	.4byte	0x2c
	.byte	0xc
	.byte	0
	.uleb128 0xe
	.4byte	.LASF97
	.byte	0x12
	.byte	0x83
	.byte	0x2a
	.4byte	0x501
	.uleb128 0x8
	.4byte	.LASF98
	.byte	0x8
	.byte	0x2
	.byte	0x68
	.byte	0x10
	.4byte	0x577
	.uleb128 0x9
	.4byte	.LASF99
	.byte	0x2
	.byte	0x6a
	.byte	0x10
	.4byte	0x423
	.byte	0
	.uleb128 0x9
	.4byte	.LASF100
	.byte	0x2
	.byte	0x6b
	.byte	0x19
	.4byte	0x812
	.byte	0x4
	.byte	0
	.uleb128 0x8
	.4byte	.LASF101
	.byte	0xd0
	.byte	0x13
	.byte	0xb1
	.byte	0x10
	.4byte	0x812
	.uleb128 0x9
	.4byte	.LASF102
	.byte	0x13
	.byte	0xb2
	.byte	0x5
	.4byte	0x848
	.byte	0
	.uleb128 0x9
	.4byte	.LASF103
	.byte	0x13
	.byte	0xb3
	.byte	0x11
	.4byte	0x2f7
	.byte	0xc
	.uleb128 0x9
	.4byte	.LASF104
	.byte	0x13
	.byte	0xb4
	.byte	0x10
	.4byte	0x423
	.byte	0x10
	.uleb128 0x9
	.4byte	.LASF105
	.byte	0x13
	.byte	0xb4
	.byte	0x1e
	.4byte	0x423
	.byte	0x14
	.uleb128 0x9
	.4byte	.LASF106
	.byte	0x13
	.byte	0xb8
	.byte	0x10
	.4byte	0xa23
	.byte	0x18
	.uleb128 0x9
	.4byte	.LASF107
	.byte	0x13
	.byte	0xb9
	.byte	0x10
	.4byte	0x423
	.byte	0x1c
	.uleb128 0x9
	.4byte	.LASF108
	.byte	0x13
	.byte	0xba
	.byte	0x11
	.4byte	0xa40
	.byte	0x20
	.uleb128 0x9
	.4byte	.LASF109
	.byte	0x13
	.byte	0xbb
	.byte	0x11
	.4byte	0xa72
	.byte	0x24
	.uleb128 0x9
	.4byte	.LASF110
	.byte	0x13
	.byte	0xbc
	.byte	0x15
	.4byte	0x1030
	.byte	0x28
	.uleb128 0x9
	.4byte	.LASF111
	.byte	0x13
	.byte	0xbe
	.byte	0xe
	.4byte	0xaa9
	.byte	0x2c
	.uleb128 0x9
	.4byte	.LASF112
	.byte	0x13
	.byte	0xc2
	.byte	0x16
	.4byte	0x1036
	.byte	0x30
	.uleb128 0x9
	.4byte	.LASF113
	.byte	0x13
	.byte	0xc3
	.byte	0x18
	.4byte	0x103c
	.byte	0x34
	.uleb128 0x9
	.4byte	.LASF114
	.byte	0x13
	.byte	0xc4
	.byte	0x17
	.4byte	0x1042
	.byte	0x38
	.uleb128 0x9
	.4byte	.LASF115
	.byte	0x13
	.byte	0xc8
	.byte	0xe
	.4byte	0xab5
	.byte	0x3c
	.uleb128 0x9
	.4byte	.LASF116
	.byte	0x13
	.byte	0xc9
	.byte	0x11
	.4byte	0x8a6
	.byte	0x40
	.uleb128 0x9
	.4byte	.LASF117
	.byte	0x13
	.byte	0xca
	.byte	0xe
	.4byte	0xaa9
	.byte	0x44
	.uleb128 0x9
	.4byte	.LASF118
	.byte	0x13
	.byte	0xcb
	.byte	0x12
	.4byte	0xa66
	.byte	0x48
	.uleb128 0x9
	.4byte	.LASF119
	.byte	0x13
	.byte	0xcc
	.byte	0x12
	.4byte	0xa9d
	.byte	0x4c
	.uleb128 0x9
	.4byte	.LASF120
	.byte	0x13
	.byte	0xcf
	.byte	0x14
	.4byte	0x1048
	.byte	0x50
	.uleb128 0x9
	.4byte	.LASF121
	.byte	0x13
	.byte	0xd2
	.byte	0x13
	.4byte	0x67
	.byte	0x54
	.uleb128 0x9
	.4byte	.LASF122
	.byte	0x13
	.byte	0xd4
	.byte	0x11
	.4byte	0x2f7
	.byte	0x58
	.uleb128 0x9
	.4byte	.LASF123
	.byte	0x13
	.byte	0xd8
	.byte	0x12
	.4byte	0x9db
	.byte	0x5c
	.uleb128 0x9
	.4byte	.LASF124
	.byte	0x13
	.byte	0xdb
	.byte	0xd
	.4byte	0x8d1
	.byte	0x60
	.uleb128 0x9
	.4byte	.LASF125
	.byte	0x13
	.byte	0xdf
	.byte	0x11
	.4byte	0xad6
	.byte	0x64
	.uleb128 0x9
	.4byte	.LASF126
	.byte	0x13
	.byte	0xe2
	.byte	0x10
	.4byte	0x423
	.byte	0x68
	.uleb128 0x9
	.4byte	.LASF127
	.byte	0x13
	.byte	0xe5
	.byte	0x11
	.4byte	0xb01
	.byte	0x6c
	.uleb128 0x9
	.4byte	.LASF128
	.byte	0x13
	.byte	0xe6
	.byte	0x12
	.4byte	0xb0d
	.byte	0x70
	.uleb128 0x9
	.4byte	.LASF129
	.byte	0x13
	.byte	0xe9
	.byte	0x19
	.4byte	0x1090
	.byte	0x74
	.uleb128 0x9
	.4byte	.LASF130
	.byte	0x13
	.byte	0xea
	.byte	0x19
	.4byte	0x109b
	.byte	0x78
	.uleb128 0x9
	.4byte	.LASF131
	.byte	0x13
	.byte	0xeb
	.byte	0x19
	.4byte	0x10f0
	.byte	0x7c
	.uleb128 0x9
	.4byte	.LASF132
	.byte	0x13
	.byte	0xec
	.byte	0x19
	.4byte	0x812
	.byte	0x80
	.uleb128 0x9
	.4byte	.LASF133
	.byte	0x13
	.byte	0xed
	.byte	0xf
	.4byte	0x875
	.byte	0x84
	.uleb128 0x9
	.4byte	.LASF134
	.byte	0x13
	.byte	0xee
	.byte	0x12
	.4byte	0xb19
	.byte	0x88
	.uleb128 0x9
	.4byte	.LASF135
	.byte	0x13
	.byte	0xef
	.byte	0x12
	.4byte	0xb25
	.byte	0x8c
	.uleb128 0x9
	.4byte	.LASF136
	.byte	0x13
	.byte	0xf0
	.byte	0x10
	.4byte	0x423
	.byte	0x90
	.uleb128 0x9
	.4byte	.LASF137
	.byte	0x13
	.byte	0xf1
	.byte	0xe
	.4byte	0xb31
	.byte	0x94
	.uleb128 0x9
	.4byte	.LASF138
	.byte	0x13
	.byte	0xf2
	.byte	0xf
	.4byte	0xb68
	.byte	0x98
	.uleb128 0x9
	.4byte	.LASF139
	.byte	0x13
	.byte	0xf3
	.byte	0xd
	.4byte	0xb3d
	.byte	0x9c
	.uleb128 0x9
	.4byte	.LASF140
	.byte	0x13
	.byte	0xf4
	.byte	0xe
	.4byte	0xa06
	.byte	0xa0
	.uleb128 0x9
	.4byte	.LASF141
	.byte	0x13
	.byte	0xf5
	.byte	0xd
	.4byte	0x8d1
	.byte	0xa4
	.uleb128 0x9
	.4byte	.LASF142
	.byte	0x13
	.byte	0xf6
	.byte	0xf
	.4byte	0x875
	.byte	0xa8
	.uleb128 0x9
	.4byte	.LASF143
	.byte	0x13
	.byte	0xf7
	.byte	0xf
	.4byte	0x875
	.byte	0xac
	.uleb128 0x9
	.4byte	.LASF144
	.byte	0x13
	.byte	0xf8
	.byte	0xf
	.4byte	0x875
	.byte	0xb0
	.uleb128 0x9
	.4byte	.LASF145
	.byte	0x13
	.byte	0xf9
	.byte	0xf
	.4byte	0x875
	.byte	0xb4
	.uleb128 0x9
	.4byte	.LASF146
	.byte	0x13
	.byte	0xfa
	.byte	0xf
	.4byte	0x875
	.byte	0xb8
	.uleb128 0x9
	.4byte	.LASF147
	.byte	0x13
	.byte	0xfb
	.byte	0x10
	.4byte	0xa23
	.byte	0xbc
	.uleb128 0x9
	.4byte	.LASF148
	.byte	0x13
	.byte	0xfe
	.byte	0x12
	.4byte	0x33
	.byte	0xc0
	.uleb128 0x15
	.4byte	.LASF149
	.byte	0x13
	.2byte	0x100
	.byte	0x10
	.4byte	0xa23
	.byte	0xc4
	.uleb128 0x15
	.4byte	.LASF150
	.byte	0x13
	.2byte	0x101
	.byte	0x14
	.4byte	0xcce
	.byte	0xc8
	.uleb128 0x15
	.4byte	.LASF151
	.byte	0x13
	.2byte	0x104
	.byte	0x1e
	.4byte	0x110f
	.byte	0xcc
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x577
	.uleb128 0x4
	.4byte	.LASF152
	.byte	0x2
	.byte	0x6c
	.byte	0x3
	.4byte	0x54f
	.uleb128 0x16
	.byte	0xc
	.byte	0x2
	.byte	0x71
	.byte	0x9
	.4byte	0x848
	.uleb128 0x9
	.4byte	.LASF102
	.byte	0x2
	.byte	0x72
	.byte	0xe
	.4byte	0x818
	.byte	0
	.uleb128 0x9
	.4byte	.LASF153
	.byte	0x2
	.byte	0x73
	.byte	0x10
	.4byte	0x423
	.byte	0x8
	.byte	0
	.uleb128 0x4
	.4byte	.LASF154
	.byte	0x2
	.byte	0x74
	.byte	0x3
	.4byte	0x824
	.uleb128 0x4
	.4byte	.LASF155
	.byte	0x2
	.byte	0x8c
	.byte	0x16
	.4byte	0x860
	.uleb128 0x7
	.byte	0x4
	.4byte	0x866
	.uleb128 0x17
	.4byte	0x875
	.4byte	0x875
	.uleb128 0x18
	.4byte	0x875
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x818
	.uleb128 0x5
	.4byte	0x875
	.uleb128 0x4
	.4byte	.LASF156
	.byte	0x2
	.byte	0x8d
	.byte	0x16
	.4byte	0x88c
	.uleb128 0x7
	.byte	0x4
	.4byte	0x892
	.uleb128 0x17
	.4byte	0x875
	.4byte	0x8a6
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0x875
	.byte	0
	.uleb128 0x4
	.4byte	.LASF157
	.byte	0x2
	.byte	0x8e
	.byte	0x16
	.4byte	0x8b2
	.uleb128 0x7
	.byte	0x4
	.4byte	0x8b8
	.uleb128 0x17
	.4byte	0x875
	.4byte	0x8d1
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0x875
	.byte	0
	.uleb128 0x4
	.4byte	.LASF158
	.byte	0x2
	.byte	0x8f
	.byte	0xf
	.4byte	0x8dd
	.uleb128 0x7
	.byte	0x4
	.4byte	0x8e3
	.uleb128 0x17
	.4byte	0x2c
	.4byte	0x8f2
	.uleb128 0x18
	.4byte	0x875
	.byte	0
	.uleb128 0x4
	.4byte	.LASF159
	.byte	0x2
	.byte	0x90
	.byte	0x16
	.4byte	0x8fe
	.uleb128 0x7
	.byte	0x4
	.4byte	0x904
	.uleb128 0x17
	.4byte	0x423
	.4byte	0x913
	.uleb128 0x18
	.4byte	0x875
	.byte	0
	.uleb128 0x4
	.4byte	.LASF160
	.byte	0x2
	.byte	0x91
	.byte	0x15
	.4byte	0x91f
	.uleb128 0x7
	.byte	0x4
	.4byte	0x925
	.uleb128 0x17
	.4byte	0x875
	.4byte	0x939
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0x423
	.byte	0
	.uleb128 0x4
	.4byte	.LASF161
	.byte	0x2
	.byte	0x93
	.byte	0xe
	.4byte	0x945
	.uleb128 0x7
	.byte	0x4
	.4byte	0x94b
	.uleb128 0x17
	.4byte	0x2c
	.4byte	0x964
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0x423
	.uleb128 0x18
	.4byte	0x875
	.byte	0
	.uleb128 0x4
	.4byte	.LASF162
	.byte	0x2
	.byte	0x95
	.byte	0xe
	.4byte	0x970
	.uleb128 0x7
	.byte	0x4
	.4byte	0x976
	.uleb128 0x17
	.4byte	0x2c
	.4byte	0x98f
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0x875
	.byte	0
	.uleb128 0x4
	.4byte	.LASF163
	.byte	0x2
	.byte	0x97
	.byte	0xf
	.4byte	0x99b
	.uleb128 0x7
	.byte	0x4
	.4byte	0x9a1
	.uleb128 0x17
	.4byte	0x2c
	.4byte	0x9b5
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0x875
	.byte	0
	.uleb128 0x4
	.4byte	.LASF164
	.byte	0x2
	.byte	0x98
	.byte	0xf
	.4byte	0x9c1
	.uleb128 0x7
	.byte	0x4
	.4byte	0x9c7
	.uleb128 0x17
	.4byte	0x2c
	.4byte	0x9db
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0x52
	.byte	0
	.uleb128 0x4
	.4byte	.LASF165
	.byte	0x2
	.byte	0x99
	.byte	0xf
	.4byte	0x9e7
	.uleb128 0x7
	.byte	0x4
	.4byte	0x9ed
	.uleb128 0x17
	.4byte	0x2c
	.4byte	0xa06
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0x9b5
	.uleb128 0x18
	.4byte	0x52
	.byte	0
	.uleb128 0x4
	.4byte	.LASF166
	.byte	0x2
	.byte	0x9c
	.byte	0x10
	.4byte	0xa12
	.uleb128 0x7
	.byte	0x4
	.4byte	0xa18
	.uleb128 0x19
	.4byte	0xa23
	.uleb128 0x18
	.4byte	0x52
	.byte	0
	.uleb128 0x4
	.4byte	.LASF167
	.byte	0x2
	.byte	0x9d
	.byte	0x10
	.4byte	0xa2f
	.uleb128 0x7
	.byte	0x4
	.4byte	0xa35
	.uleb128 0x19
	.4byte	0xa40
	.uleb128 0x18
	.4byte	0x875
	.byte	0
	.uleb128 0x4
	.4byte	.LASF168
	.byte	0x2
	.byte	0x9e
	.byte	0x15
	.4byte	0xa4c
	.uleb128 0x7
	.byte	0x4
	.4byte	0xa52
	.uleb128 0x17
	.4byte	0x875
	.4byte	0xa66
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0xfd
	.byte	0
	.uleb128 0x4
	.4byte	.LASF169
	.byte	0x2
	.byte	0x9f
	.byte	0x15
	.4byte	0x88c
	.uleb128 0x4
	.4byte	.LASF170
	.byte	0x2
	.byte	0xa0
	.byte	0xf
	.4byte	0xa7e
	.uleb128 0x7
	.byte	0x4
	.4byte	0xa84
	.uleb128 0x17
	.4byte	0x2c
	.4byte	0xa9d
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0xfd
	.uleb128 0x18
	.4byte	0x875
	.byte	0
	.uleb128 0x4
	.4byte	.LASF171
	.byte	0x2
	.byte	0xa1
	.byte	0xf
	.4byte	0x970
	.uleb128 0x4
	.4byte	.LASF172
	.byte	0x2
	.byte	0xa2
	.byte	0x15
	.4byte	0x860
	.uleb128 0x4
	.4byte	.LASF173
	.byte	0x2
	.byte	0xa3
	.byte	0x15
	.4byte	0xac1
	.uleb128 0x7
	.byte	0x4
	.4byte	0xac7
	.uleb128 0x17
	.4byte	0x42f
	.4byte	0xad6
	.uleb128 0x18
	.4byte	0x875
	.byte	0
	.uleb128 0x4
	.4byte	.LASF174
	.byte	0x2
	.byte	0xa4
	.byte	0x15
	.4byte	0xae2
	.uleb128 0x7
	.byte	0x4
	.4byte	0xae8
	.uleb128 0x17
	.4byte	0x875
	.4byte	0xb01
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0x2c
	.byte	0
	.uleb128 0x4
	.4byte	.LASF175
	.byte	0x2
	.byte	0xa5
	.byte	0x15
	.4byte	0x860
	.uleb128 0x4
	.4byte	.LASF176
	.byte	0x2
	.byte	0xa6
	.byte	0x15
	.4byte	0x860
	.uleb128 0x4
	.4byte	.LASF177
	.byte	0x2
	.byte	0xa7
	.byte	0x15
	.4byte	0x8b2
	.uleb128 0x4
	.4byte	.LASF178
	.byte	0x2
	.byte	0xa8
	.byte	0xf
	.4byte	0x970
	.uleb128 0x4
	.4byte	.LASF179
	.byte	0x2
	.byte	0xa9
	.byte	0xf
	.4byte	0x970
	.uleb128 0x4
	.4byte	.LASF180
	.byte	0x2
	.byte	0xaa
	.byte	0x15
	.4byte	0xb49
	.uleb128 0x7
	.byte	0x4
	.4byte	0xb4f
	.uleb128 0x17
	.4byte	0x875
	.4byte	0xb68
	.uleb128 0x18
	.4byte	0x812
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0x875
	.byte	0
	.uleb128 0x4
	.4byte	.LASF181
	.byte	0x2
	.byte	0xab
	.byte	0x15
	.4byte	0xb74
	.uleb128 0x7
	.byte	0x4
	.4byte	0xb7a
	.uleb128 0x17
	.4byte	0x875
	.4byte	0xb8e
	.uleb128 0x18
	.4byte	0x812
	.uleb128 0x18
	.4byte	0x423
	.byte	0
	.uleb128 0xe
	.4byte	.LASF182
	.byte	0x2
	.byte	0xce
	.byte	0x20
	.4byte	0x577
	.uleb128 0xe
	.4byte	.LASF183
	.byte	0x2
	.byte	0xcf
	.byte	0x20
	.4byte	0x577
	.uleb128 0xe
	.4byte	.LASF184
	.byte	0x2
	.byte	0xd0
	.byte	0x20
	.4byte	0x577
	.uleb128 0x11
	.4byte	.LASF185
	.byte	0x2
	.2byte	0x230
	.byte	0x16
	.4byte	0x818
	.uleb128 0x11
	.4byte	.LASF186
	.byte	0x2
	.2byte	0x23a
	.byte	0x16
	.4byte	0x818
	.uleb128 0x8
	.4byte	.LASF187
	.byte	0x2c
	.byte	0x13
	.byte	0x28
	.byte	0x10
	.4byte	0xc69
	.uleb128 0x1a
	.ascii	"buf\000"
	.byte	0x13
	.byte	0x29
	.byte	0xb
	.4byte	0x52
	.byte	0
	.uleb128 0x1a
	.ascii	"obj\000"
	.byte	0x13
	.byte	0x2a
	.byte	0xf
	.4byte	0x875
	.byte	0x4
	.uleb128 0x1a
	.ascii	"len\000"
	.byte	0x13
	.byte	0x2b
	.byte	0x10
	.4byte	0x423
	.byte	0x8
	.uleb128 0x9
	.4byte	.LASF188
	.byte	0x13
	.byte	0x2c
	.byte	0x10
	.4byte	0x423
	.byte	0xc
	.uleb128 0x9
	.4byte	.LASF189
	.byte	0x13
	.byte	0x2e
	.byte	0x9
	.4byte	0x2c
	.byte	0x10
	.uleb128 0x9
	.4byte	.LASF190
	.byte	0x13
	.byte	0x2f
	.byte	0x9
	.4byte	0x2c
	.byte	0x14
	.uleb128 0x9
	.4byte	.LASF191
	.byte	0x13
	.byte	0x30
	.byte	0xb
	.4byte	0xfd
	.byte	0x18
	.uleb128 0x9
	.4byte	.LASF192
	.byte	0x13
	.byte	0x31
	.byte	0x11
	.4byte	0xc69
	.byte	0x1c
	.uleb128 0x9
	.4byte	.LASF193
	.byte	0x13
	.byte	0x32
	.byte	0x11
	.4byte	0xc69
	.byte	0x20
	.uleb128 0x9
	.4byte	.LASF194
	.byte	0x13
	.byte	0x33
	.byte	0x11
	.4byte	0xc69
	.byte	0x24
	.uleb128 0x9
	.4byte	.LASF195
	.byte	0x13
	.byte	0x34
	.byte	0xb
	.4byte	0x52
	.byte	0x28
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x423
	.uleb128 0x4
	.4byte	.LASF196
	.byte	0x13
	.byte	0x35
	.byte	0x3
	.4byte	0xbcc
	.uleb128 0x4
	.4byte	.LASF197
	.byte	0x13
	.byte	0x37
	.byte	0xf
	.4byte	0xc87
	.uleb128 0x7
	.byte	0x4
	.4byte	0xc8d
	.uleb128 0x17
	.4byte	0x2c
	.4byte	0xca6
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0xca6
	.uleb128 0x18
	.4byte	0x2c
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0xc6f
	.uleb128 0x4
	.4byte	.LASF198
	.byte	0x13
	.byte	0x38
	.byte	0x10
	.4byte	0xcb8
	.uleb128 0x7
	.byte	0x4
	.4byte	0xcbe
	.uleb128 0x19
	.4byte	0xcce
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0xca6
	.byte	0
	.uleb128 0x4
	.4byte	.LASF199
	.byte	0x13
	.byte	0x3a
	.byte	0x15
	.4byte	0xcda
	.uleb128 0x7
	.byte	0x4
	.4byte	0xce0
	.uleb128 0x17
	.4byte	0x875
	.4byte	0xcfe
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0xcfe
	.uleb128 0x18
	.4byte	0x41
	.uleb128 0x18
	.4byte	0x875
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x87b
	.uleb128 0x16
	.byte	0x90
	.byte	0x13
	.byte	0x5f
	.byte	0x9
	.4byte	0xee2
	.uleb128 0x9
	.4byte	.LASF200
	.byte	0x13
	.byte	0x64
	.byte	0x10
	.4byte	0x880
	.byte	0
	.uleb128 0x9
	.4byte	.LASF201
	.byte	0x13
	.byte	0x65
	.byte	0x10
	.4byte	0x880
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF202
	.byte	0x13
	.byte	0x66
	.byte	0x10
	.4byte	0x880
	.byte	0x8
	.uleb128 0x9
	.4byte	.LASF203
	.byte	0x13
	.byte	0x67
	.byte	0x10
	.4byte	0x880
	.byte	0xc
	.uleb128 0x9
	.4byte	.LASF204
	.byte	0x13
	.byte	0x68
	.byte	0x10
	.4byte	0x880
	.byte	0x10
	.uleb128 0x9
	.4byte	.LASF205
	.byte	0x13
	.byte	0x69
	.byte	0x11
	.4byte	0x8a6
	.byte	0x14
	.uleb128 0x9
	.4byte	.LASF206
	.byte	0x13
	.byte	0x6a
	.byte	0xf
	.4byte	0x854
	.byte	0x18
	.uleb128 0x9
	.4byte	.LASF207
	.byte	0x13
	.byte	0x6b
	.byte	0xf
	.4byte	0x854
	.byte	0x1c
	.uleb128 0x9
	.4byte	.LASF208
	.byte	0x13
	.byte	0x6c
	.byte	0xf
	.4byte	0x854
	.byte	0x20
	.uleb128 0x9
	.4byte	.LASF209
	.byte	0x13
	.byte	0x6d
	.byte	0xd
	.4byte	0x8d1
	.byte	0x24
	.uleb128 0x9
	.4byte	.LASF210
	.byte	0x13
	.byte	0x6e
	.byte	0xf
	.4byte	0x854
	.byte	0x28
	.uleb128 0x9
	.4byte	.LASF211
	.byte	0x13
	.byte	0x6f
	.byte	0x10
	.4byte	0x880
	.byte	0x2c
	.uleb128 0x9
	.4byte	.LASF212
	.byte	0x13
	.byte	0x70
	.byte	0x10
	.4byte	0x880
	.byte	0x30
	.uleb128 0x9
	.4byte	.LASF213
	.byte	0x13
	.byte	0x71
	.byte	0x10
	.4byte	0x880
	.byte	0x34
	.uleb128 0x9
	.4byte	.LASF214
	.byte	0x13
	.byte	0x72
	.byte	0x10
	.4byte	0x880
	.byte	0x38
	.uleb128 0x9
	.4byte	.LASF215
	.byte	0x13
	.byte	0x73
	.byte	0x10
	.4byte	0x880
	.byte	0x3c
	.uleb128 0x9
	.4byte	.LASF216
	.byte	0x13
	.byte	0x74
	.byte	0xf
	.4byte	0x854
	.byte	0x40
	.uleb128 0x9
	.4byte	.LASF217
	.byte	0x13
	.byte	0x75
	.byte	0xb
	.4byte	0x52
	.byte	0x44
	.uleb128 0x9
	.4byte	.LASF218
	.byte	0x13
	.byte	0x76
	.byte	0xf
	.4byte	0x854
	.byte	0x48
	.uleb128 0x9
	.4byte	.LASF219
	.byte	0x13
	.byte	0x78
	.byte	0x10
	.4byte	0x880
	.byte	0x4c
	.uleb128 0x9
	.4byte	.LASF220
	.byte	0x13
	.byte	0x79
	.byte	0x10
	.4byte	0x880
	.byte	0x50
	.uleb128 0x9
	.4byte	.LASF221
	.byte	0x13
	.byte	0x7a
	.byte	0x10
	.4byte	0x880
	.byte	0x54
	.uleb128 0x9
	.4byte	.LASF222
	.byte	0x13
	.byte	0x7b
	.byte	0x10
	.4byte	0x880
	.byte	0x58
	.uleb128 0x9
	.4byte	.LASF223
	.byte	0x13
	.byte	0x7c
	.byte	0x11
	.4byte	0x8a6
	.byte	0x5c
	.uleb128 0x9
	.4byte	.LASF224
	.byte	0x13
	.byte	0x7d
	.byte	0x10
	.4byte	0x880
	.byte	0x60
	.uleb128 0x9
	.4byte	.LASF225
	.byte	0x13
	.byte	0x7e
	.byte	0x10
	.4byte	0x880
	.byte	0x64
	.uleb128 0x9
	.4byte	.LASF226
	.byte	0x13
	.byte	0x7f
	.byte	0x10
	.4byte	0x880
	.byte	0x68
	.uleb128 0x9
	.4byte	.LASF227
	.byte	0x13
	.byte	0x80
	.byte	0x10
	.4byte	0x880
	.byte	0x6c
	.uleb128 0x9
	.4byte	.LASF228
	.byte	0x13
	.byte	0x81
	.byte	0x10
	.4byte	0x880
	.byte	0x70
	.uleb128 0x9
	.4byte	.LASF229
	.byte	0x13
	.byte	0x83
	.byte	0x10
	.4byte	0x880
	.byte	0x74
	.uleb128 0x9
	.4byte	.LASF230
	.byte	0x13
	.byte	0x84
	.byte	0x10
	.4byte	0x880
	.byte	0x78
	.uleb128 0x9
	.4byte	.LASF231
	.byte	0x13
	.byte	0x85
	.byte	0x10
	.4byte	0x880
	.byte	0x7c
	.uleb128 0x9
	.4byte	.LASF232
	.byte	0x13
	.byte	0x86
	.byte	0x10
	.4byte	0x880
	.byte	0x80
	.uleb128 0x9
	.4byte	.LASF233
	.byte	0x13
	.byte	0x88
	.byte	0xf
	.4byte	0x854
	.byte	0x84
	.uleb128 0x9
	.4byte	.LASF234
	.byte	0x13
	.byte	0x8a
	.byte	0x10
	.4byte	0x880
	.byte	0x88
	.uleb128 0x9
	.4byte	.LASF235
	.byte	0x13
	.byte	0x8b
	.byte	0x10
	.4byte	0x880
	.byte	0x8c
	.byte	0
	.uleb128 0x4
	.4byte	.LASF236
	.byte	0x13
	.byte	0x8c
	.byte	0x3
	.4byte	0xd04
	.uleb128 0x16
	.byte	0x28
	.byte	0x13
	.byte	0x8e
	.byte	0x9
	.4byte	0xf7a
	.uleb128 0x9
	.4byte	.LASF237
	.byte	0x13
	.byte	0x8f
	.byte	0xd
	.4byte	0x8f2
	.byte	0
	.uleb128 0x9
	.4byte	.LASF238
	.byte	0x13
	.byte	0x90
	.byte	0x10
	.4byte	0x880
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF239
	.byte	0x13
	.byte	0x91
	.byte	0x12
	.4byte	0x913
	.byte	0x8
	.uleb128 0x9
	.4byte	.LASF240
	.byte	0x13
	.byte	0x92
	.byte	0x12
	.4byte	0x913
	.byte	0xc
	.uleb128 0x9
	.4byte	.LASF241
	.byte	0x13
	.byte	0x93
	.byte	0xb
	.4byte	0x52
	.byte	0x10
	.uleb128 0x9
	.4byte	.LASF242
	.byte	0x13
	.byte	0x94
	.byte	0x15
	.4byte	0x939
	.byte	0x14
	.uleb128 0x9
	.4byte	.LASF243
	.byte	0x13
	.byte	0x95
	.byte	0xb
	.4byte	0x52
	.byte	0x18
	.uleb128 0x9
	.4byte	.LASF244
	.byte	0x13
	.byte	0x96
	.byte	0x10
	.4byte	0x98f
	.byte	0x1c
	.uleb128 0x9
	.4byte	.LASF245
	.byte	0x13
	.byte	0x98
	.byte	0x10
	.4byte	0x880
	.byte	0x20
	.uleb128 0x9
	.4byte	.LASF246
	.byte	0x13
	.byte	0x99
	.byte	0x12
	.4byte	0x913
	.byte	0x24
	.byte	0
	.uleb128 0x4
	.4byte	.LASF247
	.byte	0x13
	.byte	0x9a
	.byte	0x3
	.4byte	0xeee
	.uleb128 0x16
	.byte	0xc
	.byte	0x13
	.byte	0x9c
	.byte	0x9
	.4byte	0xfb7
	.uleb128 0x9
	.4byte	.LASF248
	.byte	0x13
	.byte	0x9d
	.byte	0xd
	.4byte	0x8f2
	.byte	0
	.uleb128 0x9
	.4byte	.LASF249
	.byte	0x13
	.byte	0x9e
	.byte	0x10
	.4byte	0x880
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF250
	.byte	0x13
	.byte	0x9f
	.byte	0x13
	.4byte	0x964
	.byte	0x8
	.byte	0
	.uleb128 0x4
	.4byte	.LASF251
	.byte	0x13
	.byte	0xa0
	.byte	0x3
	.4byte	0xf86
	.uleb128 0x16
	.byte	0xc
	.byte	0x13
	.byte	0xa2
	.byte	0x9
	.4byte	0xff4
	.uleb128 0x9
	.4byte	.LASF252
	.byte	0x13
	.byte	0xa3
	.byte	0xf
	.4byte	0x854
	.byte	0
	.uleb128 0x9
	.4byte	.LASF253
	.byte	0x13
	.byte	0xa4
	.byte	0xf
	.4byte	0x854
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF254
	.byte	0x13
	.byte	0xa5
	.byte	0xf
	.4byte	0x854
	.byte	0x8
	.byte	0
	.uleb128 0x4
	.4byte	.LASF255
	.byte	0x13
	.byte	0xa6
	.byte	0x3
	.4byte	0xfc3
	.uleb128 0x16
	.byte	0x8
	.byte	0x13
	.byte	0xa8
	.byte	0x9
	.4byte	0x1024
	.uleb128 0x9
	.4byte	.LASF256
	.byte	0x13
	.byte	0xa9
	.byte	0x14
	.4byte	0xc7b
	.byte	0
	.uleb128 0x9
	.4byte	.LASF257
	.byte	0x13
	.byte	0xaa
	.byte	0x18
	.4byte	0xcac
	.byte	0x4
	.byte	0
	.uleb128 0x4
	.4byte	.LASF258
	.byte	0x13
	.byte	0xab
	.byte	0x3
	.4byte	0x1000
	.uleb128 0x7
	.byte	0x4
	.4byte	0xff4
	.uleb128 0x7
	.byte	0x4
	.4byte	0xee2
	.uleb128 0x7
	.byte	0x4
	.4byte	0xf7a
	.uleb128 0x7
	.byte	0x4
	.4byte	0xfb7
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1024
	.uleb128 0x8
	.4byte	.LASF259
	.byte	0x10
	.byte	0x14
	.byte	0x33
	.byte	0x8
	.4byte	0x1090
	.uleb128 0x9
	.4byte	.LASF260
	.byte	0x14
	.byte	0x34
	.byte	0x12
	.4byte	0x2f7
	.byte	0
	.uleb128 0x9
	.4byte	.LASF261
	.byte	0x14
	.byte	0x35
	.byte	0x11
	.4byte	0x15db
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF262
	.byte	0x14
	.byte	0x36
	.byte	0x11
	.4byte	0x2c
	.byte	0x8
	.uleb128 0x9
	.4byte	.LASF263
	.byte	0x14
	.byte	0x38
	.byte	0x12
	.4byte	0x2f7
	.byte	0xc
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x104e
	.uleb128 0xb
	.4byte	.LASF264
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1096
	.uleb128 0x8
	.4byte	.LASF265
	.byte	0x14
	.byte	0x15
	.byte	0xb
	.byte	0x10
	.4byte	0x10f0
	.uleb128 0x9
	.4byte	.LASF266
	.byte	0x15
	.byte	0xc
	.byte	0x11
	.4byte	0x2f7
	.byte	0
	.uleb128 0x1a
	.ascii	"get\000"
	.byte	0x15
	.byte	0xd
	.byte	0xc
	.4byte	0x19e6
	.byte	0x4
	.uleb128 0x1a
	.ascii	"set\000"
	.byte	0x15
	.byte	0xe
	.byte	0xc
	.4byte	0x19f2
	.byte	0x8
	.uleb128 0x1a
	.ascii	"doc\000"
	.byte	0x15
	.byte	0xf
	.byte	0x11
	.4byte	0x2f7
	.byte	0xc
	.uleb128 0x9
	.4byte	.LASF267
	.byte	0x15
	.byte	0x10
	.byte	0xb
	.4byte	0x52
	.byte	0x10
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x10a1
	.uleb128 0x17
	.4byte	0x2c
	.4byte	0x110f
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0x31a
	.uleb128 0x18
	.4byte	0x2c
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x10f6
	.uleb128 0x1b
	.4byte	.LASF268
	.byte	0x13
	.2byte	0x10e
	.byte	0x3
	.4byte	0x577
	.uleb128 0x11
	.4byte	.LASF269
	.byte	0x13
	.2byte	0x182
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0x11
	.4byte	.LASF270
	.byte	0x13
	.2byte	0x183
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xc
	.4byte	0x2c
	.4byte	0x1147
	.uleb128 0xf
	.byte	0
	.uleb128 0x11
	.4byte	.LASF271
	.byte	0x13
	.2byte	0x188
	.byte	0x11
	.4byte	0x113c
	.uleb128 0x16
	.byte	0x8
	.byte	0x16
	.byte	0x3a
	.byte	0x5
	.4byte	0x1178
	.uleb128 0x9
	.4byte	.LASF272
	.byte	0x16
	.byte	0x3b
	.byte	0x13
	.4byte	0x42f
	.byte	0
	.uleb128 0x9
	.4byte	.LASF273
	.byte	0x16
	.byte	0x3c
	.byte	0x13
	.4byte	0x42f
	.byte	0x4
	.byte	0
	.uleb128 0x16
	.byte	0x10
	.byte	0x16
	.byte	0x3f
	.byte	0x5
	.4byte	0x119a
	.uleb128 0x1a
	.ascii	"k0\000"
	.byte	0x16
	.byte	0x40
	.byte	0x12
	.4byte	0x417
	.byte	0
	.uleb128 0x1a
	.ascii	"k1\000"
	.byte	0x16
	.byte	0x41
	.byte	0x12
	.4byte	0x417
	.byte	0x8
	.byte	0
	.uleb128 0x16
	.byte	0x14
	.byte	0x16
	.byte	0x44
	.byte	0x5
	.4byte	0x11be
	.uleb128 0x9
	.4byte	.LASF274
	.byte	0x16
	.byte	0x45
	.byte	0x17
	.4byte	0x11be
	.byte	0
	.uleb128 0x9
	.4byte	.LASF273
	.byte	0x16
	.byte	0x46
	.byte	0x13
	.4byte	0x42f
	.byte	0x10
	.byte	0
	.uleb128 0xc
	.4byte	0x54
	.4byte	0x11ce
	.uleb128 0xd
	.4byte	0x33
	.byte	0xf
	.byte	0
	.uleb128 0x16
	.byte	0x14
	.byte	0x16
	.byte	0x48
	.byte	0x5
	.4byte	0x11f2
	.uleb128 0x9
	.4byte	.LASF274
	.byte	0x16
	.byte	0x49
	.byte	0x17
	.4byte	0x11be
	.byte	0
	.uleb128 0x9
	.4byte	.LASF275
	.byte	0x16
	.byte	0x4a
	.byte	0x13
	.4byte	0x42f
	.byte	0x10
	.byte	0
	.uleb128 0x1c
	.byte	0x18
	.byte	0x16
	.byte	0x36
	.byte	0x9
	.4byte	0x1237
	.uleb128 0x1d
	.ascii	"uc\000"
	.byte	0x16
	.byte	0x38
	.byte	0x13
	.4byte	0x1237
	.uleb128 0x1d
	.ascii	"fnv\000"
	.byte	0x16
	.byte	0x3d
	.byte	0x7
	.4byte	0x1154
	.uleb128 0x1e
	.4byte	.LASF276
	.byte	0x16
	.byte	0x42
	.byte	0x7
	.4byte	0x1178
	.uleb128 0x1e
	.4byte	.LASF277
	.byte	0x16
	.byte	0x47
	.byte	0x7
	.4byte	0x119a
	.uleb128 0x1e
	.4byte	.LASF278
	.byte	0x16
	.byte	0x4b
	.byte	0x7
	.4byte	0x11ce
	.byte	0
	.uleb128 0xc
	.4byte	0x54
	.4byte	0x1247
	.uleb128 0xd
	.4byte	0x33
	.byte	0x17
	.byte	0
	.uleb128 0x4
	.4byte	.LASF279
	.byte	0x16
	.byte	0x4c
	.byte	0x3
	.4byte	0x11f2
	.uleb128 0xe
	.4byte	.LASF280
	.byte	0x16
	.byte	0x4d
	.byte	0x1e
	.4byte	0x1247
	.uleb128 0xe
	.4byte	.LASF281
	.byte	0x17
	.byte	0xa
	.byte	0x11
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF282
	.byte	0x17
	.byte	0xb
	.byte	0x11
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF283
	.byte	0x17
	.byte	0xc
	.byte	0x11
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF284
	.byte	0x17
	.byte	0xd
	.byte	0x11
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF285
	.byte	0x17
	.byte	0xe
	.byte	0x11
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF286
	.byte	0x17
	.byte	0xf
	.byte	0x11
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF287
	.byte	0x17
	.byte	0x10
	.byte	0x11
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF288
	.byte	0x17
	.byte	0x11
	.byte	0x11
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF289
	.byte	0x17
	.byte	0x12
	.byte	0x11
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF290
	.byte	0x17
	.byte	0x13
	.byte	0x11
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF291
	.byte	0x17
	.byte	0x14
	.byte	0x11
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF292
	.byte	0x17
	.byte	0x15
	.byte	0x11
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF293
	.byte	0x17
	.byte	0x16
	.byte	0x11
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF294
	.byte	0x17
	.byte	0x17
	.byte	0x11
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF295
	.byte	0x17
	.byte	0x18
	.byte	0x11
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF296
	.byte	0x18
	.byte	0x22
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF297
	.byte	0x18
	.byte	0x23
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xc
	.4byte	0x103
	.4byte	0x1336
	.uleb128 0xf
	.byte	0
	.uleb128 0xe
	.4byte	.LASF298
	.byte	0x18
	.byte	0x38
	.byte	0x12
	.4byte	0x132b
	.uleb128 0xe
	.4byte	.LASF299
	.byte	0x19
	.byte	0x2c
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF300
	.byte	0x19
	.byte	0x2d
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF301
	.byte	0x1a
	.byte	0x6f
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF302
	.byte	0x1a
	.byte	0x70
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xc
	.4byte	0x5b
	.4byte	0x137d
	.uleb128 0xf
	.byte	0
	.uleb128 0x5
	.4byte	0x1372
	.uleb128 0x11
	.4byte	.LASF303
	.byte	0x1b
	.2byte	0x430
	.byte	0x21
	.4byte	0x137d
	.uleb128 0x8
	.4byte	.LASF304
	.byte	0x10
	.byte	0x1c
	.byte	0x55
	.byte	0x8
	.4byte	0x13b7
	.uleb128 0x9
	.4byte	.LASF102
	.byte	0x1c
	.byte	0x56
	.byte	0x5
	.4byte	0x848
	.byte	0
	.uleb128 0x9
	.4byte	.LASF305
	.byte	0x1c
	.byte	0x57
	.byte	0xb
	.4byte	0x1403
	.byte	0xc
	.byte	0
	.uleb128 0xe
	.4byte	.LASF306
	.byte	0x1d
	.byte	0xc
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xc
	.4byte	0x54
	.4byte	0x13d3
	.uleb128 0xd
	.4byte	0x33
	.byte	0xff
	.byte	0
	.uleb128 0xe
	.4byte	.LASF307
	.byte	0x1d
	.byte	0x4f
	.byte	0x1b
	.4byte	0x13c3
	.uleb128 0xe
	.4byte	.LASF308
	.byte	0x1d
	.byte	0xe8
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF309
	.byte	0x1d
	.byte	0xe9
	.byte	0x18
	.4byte	0x875
	.uleb128 0x4
	.4byte	.LASF310
	.byte	0x1c
	.byte	0x35
	.byte	0x18
	.4byte	0x60
	.uleb128 0xc
	.4byte	0x13f7
	.4byte	0x1413
	.uleb128 0xd
	.4byte	0x33
	.byte	0
	.byte	0
	.uleb128 0xe
	.4byte	.LASF311
	.byte	0x1e
	.byte	0xa
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF312
	.byte	0x1e
	.byte	0x12
	.byte	0x20
	.4byte	0x138f
	.uleb128 0xe
	.4byte	.LASF313
	.byte	0x1e
	.byte	0x12
	.byte	0x31
	.4byte	0x138f
	.uleb128 0xe
	.4byte	.LASF314
	.byte	0x1f
	.byte	0x15
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF315
	.byte	0x20
	.byte	0x27
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF316
	.byte	0x21
	.byte	0x12
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF317
	.byte	0x21
	.byte	0x13
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF318
	.byte	0x21
	.byte	0x14
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF319
	.byte	0x22
	.byte	0xa
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF320
	.byte	0x22
	.byte	0xc
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF321
	.byte	0x23
	.byte	0x17
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF322
	.byte	0x23
	.byte	0x18
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF323
	.byte	0x24
	.byte	0x2b
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF324
	.byte	0x24
	.byte	0x2c
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF325
	.byte	0x24
	.byte	0x2d
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF326
	.byte	0x24
	.byte	0x2e
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF327
	.byte	0x25
	.byte	0xf
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF328
	.byte	0x25
	.byte	0x3f
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF329
	.byte	0x25
	.byte	0x40
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF330
	.byte	0x25
	.byte	0x41
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF331
	.byte	0x25
	.byte	0x4c
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF332
	.byte	0x25
	.byte	0x4d
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF333
	.byte	0x25
	.byte	0x4e
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF334
	.byte	0x25
	.byte	0x50
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF335
	.byte	0x25
	.byte	0x51
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF336
	.byte	0x25
	.byte	0x52
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF337
	.byte	0x26
	.byte	0xf
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF338
	.byte	0x26
	.byte	0x10
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF339
	.byte	0x26
	.byte	0x11
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF340
	.byte	0x26
	.byte	0x12
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF341
	.byte	0x26
	.byte	0x13
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF342
	.byte	0x27
	.byte	0xa
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF343
	.byte	0x27
	.byte	0xb
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF344
	.byte	0x28
	.byte	0x45
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF345
	.byte	0x28
	.byte	0x4d
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF346
	.byte	0x28
	.byte	0x4e
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF347
	.byte	0x28
	.byte	0x4f
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF348
	.byte	0x14
	.byte	0xe
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0x4
	.4byte	.LASF349
	.byte	0x14
	.byte	0x12
	.byte	0x15
	.4byte	0x88c
	.uleb128 0xe
	.4byte	.LASF350
	.byte	0x29
	.byte	0xa
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF351
	.byte	0x29
	.byte	0x29
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0x1f
	.4byte	0x875
	.uleb128 0x7
	.byte	0x4
	.4byte	0x15ff
	.uleb128 0xe
	.4byte	.LASF352
	.byte	0x2a
	.byte	0x2c
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF353
	.byte	0x2a
	.byte	0x5e
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF354
	.byte	0x2a
	.byte	0x5f
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF355
	.byte	0x2b
	.byte	0x14
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF356
	.byte	0x2b
	.byte	0x2b
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF357
	.byte	0x2c
	.byte	0x16
	.byte	0x1a
	.4byte	0x2f7
	.uleb128 0xe
	.4byte	.LASF358
	.byte	0x2c
	.byte	0x17
	.byte	0x11
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF359
	.byte	0x2d
	.byte	0xc
	.byte	0x1a
	.4byte	0x2f7
	.uleb128 0xe
	.4byte	.LASF360
	.byte	0x2d
	.byte	0x10
	.byte	0x11
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF361
	.byte	0x2d
	.byte	0x16
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1688
	.uleb128 0x17
	.4byte	0x875
	.4byte	0x169c
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0x52
	.byte	0
	.uleb128 0xe
	.4byte	.LASF362
	.byte	0x2e
	.byte	0x15
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF363
	.byte	0x2f
	.byte	0xf
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xb
	.4byte	.LASF364
	.uleb128 0x7
	.byte	0x4
	.4byte	0x16b4
	.uleb128 0xe
	.4byte	.LASF365
	.byte	0x30
	.byte	0x9
	.byte	0x16
	.4byte	0x818
	.uleb128 0xe
	.4byte	.LASF366
	.byte	0x30
	.byte	0x1c
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF367
	.byte	0x30
	.byte	0x1d
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF368
	.byte	0x31
	.byte	0xe
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF369
	.byte	0x32
	.byte	0x8
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF370
	.byte	0x32
	.byte	0x9
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF371
	.byte	0x32
	.byte	0xa
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0x4
	.4byte	.LASF372
	.byte	0x33
	.byte	0x17
	.byte	0x14
	.4byte	0x171f
	.uleb128 0x20
	.ascii	"_ts\000"
	.byte	0x98
	.byte	0x34
	.byte	0x33
	.byte	0x8
	.4byte	0x18d9
	.uleb128 0x9
	.4byte	.LASF373
	.byte	0x34
	.byte	0x36
	.byte	0x11
	.4byte	0x196e
	.byte	0
	.uleb128 0x9
	.4byte	.LASF374
	.byte	0x34
	.byte	0x37
	.byte	0x11
	.4byte	0x196e
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF375
	.byte	0x34
	.byte	0x38
	.byte	0x19
	.4byte	0x1974
	.byte	0x8
	.uleb128 0x9
	.4byte	.LASF376
	.byte	0x34
	.byte	0x3b
	.byte	0x14
	.4byte	0x16b9
	.byte	0xc
	.uleb128 0x9
	.4byte	.LASF377
	.byte	0x34
	.byte	0x3c
	.byte	0x9
	.4byte	0x2c
	.byte	0x10
	.uleb128 0x9
	.4byte	.LASF378
	.byte	0x34
	.byte	0x3d
	.byte	0xa
	.4byte	0x103
	.byte	0x14
	.uleb128 0x9
	.4byte	.LASF379
	.byte	0x34
	.byte	0x3f
	.byte	0xa
	.4byte	0x103
	.byte	0x15
	.uleb128 0x9
	.4byte	.LASF380
	.byte	0x34
	.byte	0x41
	.byte	0x9
	.4byte	0x2c
	.byte	0x18
	.uleb128 0x9
	.4byte	.LASF94
	.byte	0x34
	.byte	0x46
	.byte	0x9
	.4byte	0x2c
	.byte	0x1c
	.uleb128 0x9
	.4byte	.LASF381
	.byte	0x34
	.byte	0x47
	.byte	0x9
	.4byte	0x2c
	.byte	0x20
	.uleb128 0x9
	.4byte	.LASF382
	.byte	0x34
	.byte	0x49
	.byte	0x12
	.4byte	0x18ea
	.byte	0x24
	.uleb128 0x9
	.4byte	.LASF383
	.byte	0x34
	.byte	0x4a
	.byte	0x12
	.4byte	0x18ea
	.byte	0x28
	.uleb128 0x9
	.4byte	.LASF384
	.byte	0x34
	.byte	0x4b
	.byte	0xf
	.4byte	0x875
	.byte	0x2c
	.uleb128 0x9
	.4byte	.LASF385
	.byte	0x34
	.byte	0x4c
	.byte	0xf
	.4byte	0x875
	.byte	0x30
	.uleb128 0x9
	.4byte	.LASF386
	.byte	0x34
	.byte	0x4f
	.byte	0xf
	.4byte	0x875
	.byte	0x34
	.uleb128 0x9
	.4byte	.LASF387
	.byte	0x34
	.byte	0x50
	.byte	0xf
	.4byte	0x875
	.byte	0x38
	.uleb128 0x9
	.4byte	.LASF388
	.byte	0x34
	.byte	0x51
	.byte	0xf
	.4byte	0x875
	.byte	0x3c
	.uleb128 0x9
	.4byte	.LASF389
	.byte	0x34
	.byte	0x56
	.byte	0x16
	.4byte	0x1962
	.byte	0x40
	.uleb128 0x9
	.4byte	.LASF390
	.byte	0x34
	.byte	0x5a
	.byte	0x17
	.4byte	0x197a
	.byte	0x50
	.uleb128 0x9
	.4byte	.LASF391
	.byte	0x34
	.byte	0x5c
	.byte	0xf
	.4byte	0x875
	.byte	0x54
	.uleb128 0x9
	.4byte	.LASF392
	.byte	0x34
	.byte	0x5e
	.byte	0x9
	.4byte	0x2c
	.byte	0x58
	.uleb128 0x9
	.4byte	.LASF393
	.byte	0x34
	.byte	0x60
	.byte	0xf
	.4byte	0x875
	.byte	0x5c
	.uleb128 0x9
	.4byte	.LASF394
	.byte	0x34
	.byte	0x61
	.byte	0x13
	.4byte	0x67
	.byte	0x60
	.uleb128 0x9
	.4byte	.LASF395
	.byte	0x34
	.byte	0x63
	.byte	0x9
	.4byte	0x2c
	.byte	0x64
	.uleb128 0x9
	.4byte	.LASF396
	.byte	0x34
	.byte	0x64
	.byte	0xf
	.4byte	0x875
	.byte	0x68
	.uleb128 0x9
	.4byte	.LASF397
	.byte	0x34
	.byte	0x7d
	.byte	0xc
	.4byte	0xa12
	.byte	0x6c
	.uleb128 0x9
	.4byte	.LASF398
	.byte	0x34
	.byte	0x7e
	.byte	0xb
	.4byte	0x52
	.byte	0x70
	.uleb128 0x9
	.4byte	.LASF399
	.byte	0x34
	.byte	0x80
	.byte	0x9
	.4byte	0x2c
	.byte	0x74
	.uleb128 0x9
	.4byte	.LASF400
	.byte	0x34
	.byte	0x82
	.byte	0xf
	.4byte	0x875
	.byte	0x78
	.uleb128 0x9
	.4byte	.LASF401
	.byte	0x34
	.byte	0x83
	.byte	0xf
	.4byte	0x875
	.byte	0x7c
	.uleb128 0x9
	.4byte	.LASF402
	.byte	0x34
	.byte	0x85
	.byte	0xf
	.4byte	0x875
	.byte	0x80
	.uleb128 0x9
	.4byte	.LASF403
	.byte	0x34
	.byte	0x86
	.byte	0xe
	.4byte	0x417
	.byte	0x88
	.uleb128 0x1a
	.ascii	"id\000"
	.byte	0x34
	.byte	0x89
	.byte	0xe
	.4byte	0x417
	.byte	0x90
	.byte	0
	.uleb128 0x4
	.4byte	.LASF404
	.byte	0x33
	.byte	0x19
	.byte	0x14
	.4byte	0x18e5
	.uleb128 0x21
	.ascii	"_is\000"
	.uleb128 0x4
	.4byte	.LASF405
	.byte	0x34
	.byte	0x13
	.byte	0xf
	.4byte	0x18f6
	.uleb128 0x7
	.byte	0x4
	.4byte	0x18fc
	.uleb128 0x17
	.4byte	0x2c
	.4byte	0x191a
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0x16b9
	.uleb128 0x18
	.4byte	0x2c
	.uleb128 0x18
	.4byte	0x875
	.byte	0
	.uleb128 0x8
	.4byte	.LASF406
	.byte	0x10
	.byte	0x34
	.byte	0x24
	.byte	0x10
	.4byte	0x195c
	.uleb128 0x9
	.4byte	.LASF407
	.byte	0x34
	.byte	0x2b
	.byte	0xf
	.4byte	0x875
	.byte	0
	.uleb128 0x9
	.4byte	.LASF408
	.byte	0x34
	.byte	0x2b
	.byte	0x1a
	.4byte	0x875
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF409
	.byte	0x34
	.byte	0x2b
	.byte	0x26
	.4byte	0x875
	.byte	0x8
	.uleb128 0x9
	.4byte	.LASF410
	.byte	0x34
	.byte	0x2d
	.byte	0x1c
	.4byte	0x195c
	.byte	0xc
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x191a
	.uleb128 0x4
	.4byte	.LASF411
	.byte	0x34
	.byte	0x2f
	.byte	0x3
	.4byte	0x191a
	.uleb128 0x7
	.byte	0x4
	.4byte	0x171f
	.uleb128 0x7
	.byte	0x4
	.4byte	0x18d9
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1962
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1713
	.uleb128 0xe
	.4byte	.LASF412
	.byte	0x35
	.byte	0x26
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF413
	.byte	0x35
	.byte	0x3b
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF414
	.byte	0x35
	.byte	0x3c
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF415
	.byte	0x35
	.byte	0x3e
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF416
	.byte	0x35
	.byte	0x57
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF417
	.byte	0x35
	.byte	0x58
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF418
	.byte	0x35
	.byte	0x59
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF419
	.byte	0x35
	.byte	0x5a
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0x4
	.4byte	.LASF420
	.byte	0x15
	.byte	0x8
	.byte	0x15
	.4byte	0x1682
	.uleb128 0x4
	.4byte	.LASF421
	.byte	0x15
	.byte	0x9
	.byte	0xf
	.4byte	0x19fe
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1a04
	.uleb128 0x17
	.4byte	0x2c
	.4byte	0x1a1d
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0x875
	.uleb128 0x18
	.4byte	0x52
	.byte	0
	.uleb128 0xe
	.4byte	.LASF422
	.byte	0x15
	.byte	0x4c
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF423
	.byte	0x15
	.byte	0x4d
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF424
	.byte	0x15
	.byte	0x4e
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF425
	.byte	0x15
	.byte	0x4f
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF426
	.byte	0x15
	.byte	0x50
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF427
	.byte	0x15
	.byte	0x51
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF428
	.byte	0x15
	.byte	0x53
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF429
	.byte	0x15
	.byte	0x67
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF430
	.byte	0x36
	.byte	0x2b
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF431
	.byte	0x36
	.byte	0x2c
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF432
	.byte	0x36
	.byte	0x2d
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF433
	.byte	0x37
	.byte	0x16
	.byte	0xe
	.4byte	0xfd
	.uleb128 0xe
	.4byte	.LASF434
	.byte	0x38
	.byte	0xb
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF435
	.byte	0x39
	.byte	0xd
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF436
	.byte	0x3a
	.byte	0xea
	.byte	0x1a
	.4byte	0x2f7
	.uleb128 0xe
	.4byte	.LASF437
	.byte	0x3b
	.byte	0x42
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF438
	.byte	0x3b
	.byte	0x43
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF439
	.byte	0x3b
	.byte	0x45
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF440
	.byte	0x3b
	.byte	0x47
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF441
	.byte	0x3b
	.byte	0x48
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF442
	.byte	0x3b
	.byte	0x49
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF443
	.byte	0x3b
	.byte	0x4a
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF444
	.byte	0x3b
	.byte	0x4c
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF445
	.byte	0x3b
	.byte	0x4d
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF446
	.byte	0x3b
	.byte	0x4e
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF447
	.byte	0x3b
	.byte	0x4f
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF448
	.byte	0x3b
	.byte	0x50
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF449
	.byte	0x3b
	.byte	0x51
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF450
	.byte	0x3b
	.byte	0x52
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF451
	.byte	0x3b
	.byte	0x54
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF452
	.byte	0x3b
	.byte	0x56
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF453
	.byte	0x3b
	.byte	0x57
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF454
	.byte	0x3b
	.byte	0x58
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF455
	.byte	0x3b
	.byte	0x59
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF456
	.byte	0x3b
	.byte	0x5a
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF457
	.byte	0x3b
	.byte	0x5b
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF458
	.byte	0x3b
	.byte	0x5c
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF459
	.byte	0x3b
	.byte	0x5e
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF460
	.byte	0x3b
	.byte	0x60
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF461
	.byte	0x3b
	.byte	0x61
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF462
	.byte	0x3b
	.byte	0x62
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF463
	.byte	0x3b
	.byte	0x63
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF464
	.byte	0x3b
	.byte	0x64
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF465
	.byte	0x3b
	.byte	0x65
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF466
	.byte	0x3b
	.byte	0x66
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF467
	.byte	0x3b
	.byte	0x67
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF468
	.byte	0x3b
	.byte	0x68
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF469
	.byte	0x3b
	.byte	0x69
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF470
	.byte	0x3b
	.byte	0x6a
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF471
	.byte	0x3b
	.byte	0x6b
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF472
	.byte	0x3b
	.byte	0x6c
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF473
	.byte	0x3b
	.byte	0x6d
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF474
	.byte	0x3b
	.byte	0x6e
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF475
	.byte	0x3b
	.byte	0x71
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF476
	.byte	0x3b
	.byte	0x72
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF477
	.byte	0x3b
	.byte	0x73
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF478
	.byte	0x3b
	.byte	0x74
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF479
	.byte	0x3b
	.byte	0x75
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF480
	.byte	0x3b
	.byte	0x76
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF481
	.byte	0x3b
	.byte	0x77
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF482
	.byte	0x3b
	.byte	0x78
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF483
	.byte	0x3b
	.byte	0x79
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF484
	.byte	0x3b
	.byte	0x7a
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF485
	.byte	0x3b
	.byte	0x7b
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF486
	.byte	0x3b
	.byte	0x7c
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF487
	.byte	0x3b
	.byte	0x7d
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF488
	.byte	0x3b
	.byte	0x7e
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF489
	.byte	0x3b
	.byte	0x7f
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF490
	.byte	0x3b
	.byte	0x84
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF491
	.byte	0x3b
	.byte	0x85
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF492
	.byte	0x3b
	.byte	0x8b
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF493
	.byte	0x3b
	.byte	0x8c
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF494
	.byte	0x3b
	.byte	0x8d
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF495
	.byte	0x3b
	.byte	0x8e
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF496
	.byte	0x3b
	.byte	0x8f
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF497
	.byte	0x3b
	.byte	0x90
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF498
	.byte	0x3b
	.byte	0x91
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF499
	.byte	0x3b
	.byte	0x92
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF500
	.byte	0x3b
	.byte	0x93
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF501
	.byte	0x3b
	.byte	0x94
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF502
	.byte	0x3b
	.byte	0x95
	.byte	0x18
	.4byte	0x875
	.uleb128 0xe
	.4byte	.LASF503
	.byte	0x3c
	.byte	0xa
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF504
	.byte	0x3c
	.byte	0xd
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF505
	.byte	0x3c
	.byte	0x10
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF506
	.byte	0x3d
	.byte	0xf2
	.byte	0x1a
	.4byte	0x2f7
	.uleb128 0xe
	.4byte	.LASF507
	.byte	0x3e
	.byte	0x74
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0x1f
	.4byte	0x2c
	.uleb128 0xe
	.4byte	.LASF508
	.byte	0x3f
	.byte	0xba
	.byte	0x13
	.4byte	0x1e36
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1e25
	.uleb128 0x17
	.4byte	0xfd
	.4byte	0x1e55
	.uleb128 0x18
	.4byte	0x31a
	.uleb128 0x18
	.4byte	0x31a
	.uleb128 0x18
	.4byte	0x2f7
	.byte	0
	.uleb128 0xe
	.4byte	.LASF509
	.byte	0x3f
	.byte	0xbb
	.byte	0x15
	.4byte	0x1e61
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1e3c
	.uleb128 0xe
	.4byte	.LASF510
	.byte	0x3f
	.byte	0xbd
	.byte	0x1c
	.4byte	0x1980
	.uleb128 0xe
	.4byte	.LASF511
	.byte	0x40
	.byte	0x65
	.byte	0x11
	.4byte	0x2c
	.uleb128 0x8
	.4byte	.LASF512
	.byte	0x8
	.byte	0x41
	.byte	0x76
	.byte	0x8
	.4byte	0x1ea7
	.uleb128 0x9
	.4byte	.LASF266
	.byte	0x41
	.byte	0x77
	.byte	0x11
	.4byte	0x2f7
	.byte	0
	.uleb128 0x9
	.4byte	.LASF513
	.byte	0x41
	.byte	0x78
	.byte	0x11
	.4byte	0x1604
	.byte	0x4
	.byte	0
	.uleb128 0xe
	.4byte	.LASF514
	.byte	0x41
	.byte	0x7a
	.byte	0x1f
	.4byte	0x1eb3
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1e7f
	.uleb128 0xe
	.4byte	.LASF515
	.byte	0x41
	.byte	0x7e
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0x8
	.4byte	.LASF516
	.byte	0xc
	.byte	0x41
	.byte	0x86
	.byte	0x8
	.4byte	0x1efa
	.uleb128 0x9
	.4byte	.LASF266
	.byte	0x41
	.byte	0x87
	.byte	0x11
	.4byte	0x2f7
	.byte	0
	.uleb128 0x9
	.4byte	.LASF517
	.byte	0x41
	.byte	0x88
	.byte	0x1a
	.4byte	0x1eff
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF518
	.byte	0x41
	.byte	0x89
	.byte	0x9
	.4byte	0x2c
	.byte	0x8
	.byte	0
	.uleb128 0x5
	.4byte	0x1ec5
	.uleb128 0x7
	.byte	0x4
	.4byte	0x5b
	.uleb128 0xe
	.4byte	.LASF519
	.byte	0x41
	.byte	0x8f
	.byte	0x24
	.4byte	0x1f11
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1efa
	.uleb128 0xe
	.4byte	.LASF520
	.byte	0x42
	.byte	0x7
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF521
	.byte	0x42
	.byte	0x8
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xe
	.4byte	.LASF522
	.byte	0x42
	.byte	0x9
	.byte	0x1a
	.4byte	0x1115
	.uleb128 0xc
	.4byte	0x4d
	.4byte	0x1f4b
	.uleb128 0xd
	.4byte	0x33
	.byte	0xff
	.byte	0
	.uleb128 0x5
	.4byte	0x1f3b
	.uleb128 0xe
	.4byte	.LASF523
	.byte	0x43
	.byte	0xd
	.byte	0x20
	.4byte	0x1f4b
	.uleb128 0xc
	.4byte	0x5b
	.4byte	0x1f6c
	.uleb128 0xd
	.4byte	0x33
	.byte	0xff
	.byte	0
	.uleb128 0x5
	.4byte	0x1f5c
	.uleb128 0xe
	.4byte	.LASF524
	.byte	0x43
	.byte	0x1a
	.byte	0x21
	.4byte	0x1f6c
	.uleb128 0xe
	.4byte	.LASF525
	.byte	0x43
	.byte	0x1b
	.byte	0x21
	.4byte	0x1f6c
	.uleb128 0x4
	.4byte	.LASF526
	.byte	0x3
	.byte	0x95
	.byte	0x1a
	.4byte	0x2c
	.uleb128 0x2
	.byte	0x8
	.byte	0x4
	.4byte	.LASF527
	.uleb128 0x17
	.4byte	0x52
	.4byte	0x1fab
	.uleb128 0x18
	.4byte	0x1f89
	.byte	0
	.uleb128 0xe
	.4byte	.LASF528
	.byte	0x44
	.byte	0x4c
	.byte	0x10
	.4byte	0x1fb7
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1f9c
	.uleb128 0x19
	.4byte	0x1fcd
	.uleb128 0x18
	.4byte	0x52
	.uleb128 0x18
	.4byte	0x39c
	.byte	0
	.uleb128 0xe
	.4byte	.LASF529
	.byte	0x44
	.byte	0x8f
	.byte	0x26
	.4byte	0x1fdf
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1fbd
	.uleb128 0x22
	.4byte	0x1fd9
	.uleb128 0x17
	.4byte	0x52
	.4byte	0x1ff8
	.uleb128 0x18
	.4byte	0x41
	.uleb128 0x18
	.4byte	0x39c
	.byte	0
	.uleb128 0xe
	.4byte	.LASF530
	.byte	0x44
	.byte	0x92
	.byte	0x27
	.4byte	0x200a
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1fe4
	.uleb128 0x22
	.4byte	0x2004
	.uleb128 0x17
	.4byte	0x52
	.4byte	0x2028
	.uleb128 0x18
	.4byte	0x52
	.uleb128 0x18
	.4byte	0x41
	.uleb128 0x18
	.4byte	0x39c
	.byte	0
	.uleb128 0xe
	.4byte	.LASF531
	.byte	0x44
	.byte	0x95
	.byte	0x27
	.4byte	0x203a
	.uleb128 0x7
	.byte	0x4
	.4byte	0x200f
	.uleb128 0x22
	.4byte	0x2034
	.uleb128 0x17
	.4byte	0x52
	.4byte	0x2058
	.uleb128 0x18
	.4byte	0x41
	.uleb128 0x18
	.4byte	0x41
	.uleb128 0x18
	.4byte	0x39c
	.byte	0
	.uleb128 0xe
	.4byte	.LASF532
	.byte	0x44
	.byte	0x99
	.byte	0x27
	.4byte	0x206a
	.uleb128 0x7
	.byte	0x4
	.4byte	0x203f
	.uleb128 0x22
	.4byte	0x2064
	.uleb128 0x23
	.uleb128 0xe
	.4byte	.LASF533
	.byte	0x44
	.byte	0x9d
	.byte	0x26
	.4byte	0x2082
	.uleb128 0x7
	.byte	0x4
	.4byte	0x206f
	.uleb128 0x22
	.4byte	0x207c
	.uleb128 0x4
	.4byte	.LASF534
	.byte	0x45
	.byte	0x24
	.byte	0x12
	.4byte	0x3ff
	.uleb128 0x8
	.4byte	.LASF535
	.byte	0x6
	.byte	0x45
	.byte	0x26
	.byte	0x8
	.4byte	0x20c2
	.uleb128 0x1a
	.ascii	"r\000"
	.byte	0x45
	.byte	0x27
	.byte	0xb
	.4byte	0x390
	.byte	0
	.uleb128 0x1a
	.ascii	"g\000"
	.byte	0x45
	.byte	0x27
	.byte	0xe
	.4byte	0x390
	.byte	0x2
	.uleb128 0x1a
	.ascii	"b\000"
	.byte	0x45
	.byte	0x27
	.byte	0x11
	.4byte	0x390
	.byte	0x4
	.byte	0
	.uleb128 0x8
	.4byte	.LASF536
	.byte	0x88
	.byte	0x45
	.byte	0x2a
	.byte	0x8
	.4byte	0x2270
	.uleb128 0x9
	.4byte	.LASF537
	.byte	0x45
	.byte	0x2b
	.byte	0xc
	.4byte	0x40b
	.byte	0
	.uleb128 0x9
	.4byte	.LASF538
	.byte	0x45
	.byte	0x2d
	.byte	0xe
	.4byte	0x2270
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF539
	.byte	0x45
	.byte	0x2e
	.byte	0xe
	.4byte	0x2270
	.byte	0x8
	.uleb128 0x9
	.4byte	.LASF540
	.byte	0x45
	.byte	0x2f
	.byte	0xe
	.4byte	0x2270
	.byte	0xc
	.uleb128 0x9
	.4byte	.LASF541
	.byte	0x45
	.byte	0x30
	.byte	0xe
	.4byte	0x2270
	.byte	0x10
	.uleb128 0x9
	.4byte	.LASF542
	.byte	0x45
	.byte	0x32
	.byte	0xc
	.4byte	0x2276
	.byte	0x14
	.uleb128 0x9
	.4byte	.LASF543
	.byte	0x45
	.byte	0x34
	.byte	0xc
	.4byte	0x2276
	.byte	0x18
	.uleb128 0x9
	.4byte	.LASF544
	.byte	0x45
	.byte	0x35
	.byte	0xc
	.4byte	0x40b
	.byte	0x1c
	.uleb128 0x9
	.4byte	.LASF545
	.byte	0x45
	.byte	0x36
	.byte	0xc
	.4byte	0x40b
	.byte	0x20
	.uleb128 0x9
	.4byte	.LASF546
	.byte	0x45
	.byte	0x38
	.byte	0xc
	.4byte	0x40b
	.byte	0x24
	.uleb128 0x9
	.4byte	.LASF547
	.byte	0x45
	.byte	0x3a
	.byte	0x9
	.4byte	0x25
	.byte	0x28
	.uleb128 0x9
	.4byte	.LASF548
	.byte	0x45
	.byte	0x3c
	.byte	0x7
	.4byte	0x2c
	.byte	0x2c
	.uleb128 0x9
	.4byte	.LASF549
	.byte	0x45
	.byte	0x3d
	.byte	0x7
	.4byte	0x2c
	.byte	0x30
	.uleb128 0x9
	.4byte	.LASF550
	.byte	0x45
	.byte	0x3e
	.byte	0x7
	.4byte	0x2c
	.byte	0x34
	.uleb128 0x9
	.4byte	.LASF551
	.byte	0x45
	.byte	0x3f
	.byte	0x7
	.4byte	0x2c
	.byte	0x38
	.uleb128 0x9
	.4byte	.LASF552
	.byte	0x45
	.byte	0x40
	.byte	0x7
	.4byte	0x2c
	.byte	0x3c
	.uleb128 0x9
	.4byte	.LASF553
	.byte	0x45
	.byte	0x41
	.byte	0x7
	.4byte	0x2c
	.byte	0x40
	.uleb128 0x9
	.4byte	.LASF554
	.byte	0x45
	.byte	0x42
	.byte	0x7
	.4byte	0x2c
	.byte	0x44
	.uleb128 0x9
	.4byte	.LASF555
	.byte	0x45
	.byte	0x43
	.byte	0x7
	.4byte	0x2c
	.byte	0x48
	.uleb128 0x9
	.4byte	.LASF556
	.byte	0x45
	.byte	0x44
	.byte	0x7
	.4byte	0x2c
	.byte	0x4c
	.uleb128 0x9
	.4byte	.LASF557
	.byte	0x45
	.byte	0x45
	.byte	0x7
	.4byte	0x2c
	.byte	0x50
	.uleb128 0x9
	.4byte	.LASF518
	.byte	0x45
	.byte	0x46
	.byte	0x7
	.4byte	0x2c
	.byte	0x54
	.uleb128 0x9
	.4byte	.LASF558
	.byte	0x45
	.byte	0x48
	.byte	0x7
	.4byte	0x2c
	.byte	0x58
	.uleb128 0x9
	.4byte	.LASF559
	.byte	0x45
	.byte	0x49
	.byte	0x7
	.4byte	0x2c
	.byte	0x5c
	.uleb128 0x9
	.4byte	.LASF560
	.byte	0x45
	.byte	0x4b
	.byte	0x7
	.4byte	0x2c
	.byte	0x60
	.uleb128 0x9
	.4byte	.LASF561
	.byte	0x45
	.byte	0x4c
	.byte	0xc
	.4byte	0x40b
	.byte	0x64
	.uleb128 0x9
	.4byte	.LASF562
	.byte	0x45
	.byte	0x4e
	.byte	0xd
	.4byte	0x227c
	.byte	0x68
	.uleb128 0x9
	.4byte	.LASF563
	.byte	0x45
	.byte	0x50
	.byte	0x1e
	.4byte	0x2093
	.byte	0x6c
	.uleb128 0x9
	.4byte	.LASF564
	.byte	0x45
	.byte	0x51
	.byte	0x1e
	.4byte	0x2093
	.byte	0x72
	.uleb128 0x9
	.4byte	.LASF565
	.byte	0x45
	.byte	0x52
	.byte	0x1e
	.4byte	0x2093
	.byte	0x78
	.uleb128 0x9
	.4byte	.LASF566
	.byte	0x45
	.byte	0x53
	.byte	0x1e
	.4byte	0x2093
	.byte	0x7e
	.uleb128 0x9
	.4byte	.LASF567
	.byte	0x45
	.byte	0x58
	.byte	0xd
	.4byte	0x2282
	.byte	0x84
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x2087
	.uleb128 0x7
	.byte	0x4
	.4byte	0x3f3
	.uleb128 0x7
	.byte	0x4
	.4byte	0x40b
	.uleb128 0x7
	.byte	0x4
	.4byte	0x3ff
	.uleb128 0x24
	.4byte	.LASF568
	.byte	0x1
	.byte	0x1b
	.byte	0x18
	.4byte	0x20c2
	.uleb128 0x5
	.byte	0x3
	.4byte	g_armwave_state
	.uleb128 0xc
	.4byte	0x3f3
	.4byte	0x22aa
	.uleb128 0xd
	.4byte	0x33
	.byte	0xff
	.byte	0
	.uleb128 0x24
	.4byte	.LASF569
	.byte	0x1
	.byte	0x1d
	.byte	0x9
	.4byte	0x229a
	.uleb128 0x5
	.byte	0x3
	.4byte	gamma_table
	.uleb128 0x25
	.4byte	.LASF570
	.byte	0x1
	.2byte	0x253
	.byte	0x6
	.4byte	.LFB76
	.4byte	.LFE76-.LFB76
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x22f8
	.uleb128 0x26
	.4byte	.LVL206
	.4byte	0x35c1
	.uleb128 0x26
	.4byte	.LVL207
	.4byte	0x35c1
	.uleb128 0x26
	.4byte	.LVL208
	.4byte	0x35c1
	.uleb128 0x26
	.4byte	.LVL209
	.4byte	0x35c1
	.byte	0
	.uleb128 0x25
	.4byte	.LASF571
	.byte	0x1
	.2byte	0x1fb
	.byte	0x6
	.4byte	.LFB75
	.4byte	.LFE75-.LFB75
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x249b
	.uleb128 0x27
	.ascii	"mod\000"
	.byte	0x1
	.2byte	0x1fb
	.byte	0x28
	.4byte	0x25
	.4byte	.LLST114
	.4byte	.LVUS114
	.uleb128 0x28
	.4byte	.LASF572
	.byte	0x1
	.2byte	0x1fb
	.byte	0x33
	.4byte	0x25
	.4byte	.LLST115
	.4byte	.LVUS115
	.uleb128 0x28
	.4byte	.LASF573
	.byte	0x1
	.2byte	0x1fb
	.byte	0x47
	.4byte	0x2c
	.4byte	.LLST116
	.4byte	.LVUS116
	.uleb128 0x29
	.ascii	"v\000"
	.byte	0x1
	.2byte	0x1fd
	.byte	0xb
	.4byte	0x25
	.uleb128 0x2a
	.4byte	.LASF574
	.byte	0x1
	.2byte	0x1fd
	.byte	0xe
	.4byte	0x25
	.4byte	.LLST117
	.4byte	.LVUS117
	.uleb128 0x2a
	.4byte	.LASF575
	.byte	0x1
	.2byte	0x1fd
	.byte	0x15
	.4byte	0x25
	.4byte	.LLST118
	.4byte	.LVUS118
	.uleb128 0x2b
	.4byte	.LASF576
	.byte	0x1
	.2byte	0x1fd
	.byte	0x1d
	.4byte	0x25
	.uleb128 0x2a
	.4byte	.LASF577
	.byte	0x1
	.2byte	0x1fe
	.byte	0xb
	.4byte	0x25
	.4byte	.LLST119
	.4byte	.LVUS119
	.uleb128 0x2c
	.ascii	"s\000"
	.byte	0x1
	.2byte	0x1ff
	.byte	0x9
	.4byte	0x2c
	.4byte	.LLST120
	.4byte	.LVUS120
	.uleb128 0x2a
	.4byte	.LASF578
	.byte	0x1
	.2byte	0x1ff
	.byte	0xc
	.4byte	0x2c
	.4byte	.LLST121
	.4byte	.LVUS121
	.uleb128 0x2c
	.ascii	"w\000"
	.byte	0x1
	.2byte	0x200
	.byte	0x9
	.4byte	0x2c
	.4byte	.LLST122
	.4byte	.LVUS122
	.uleb128 0x2c
	.ascii	"x\000"
	.byte	0x1
	.2byte	0x200
	.byte	0xc
	.4byte	0x2c
	.4byte	.LLST123
	.4byte	.LVUS123
	.uleb128 0x2d
	.4byte	0x2579
	.4byte	.LBI49
	.byte	.LVU827
	.4byte	.Ldebug_ranges0+0x90
	.byte	0x1
	.2byte	0x204
	.byte	0x5
	.4byte	0x245c
	.uleb128 0x2e
	.4byte	0x2587
	.4byte	.LLST124
	.4byte	.LVUS124
	.uleb128 0x26
	.4byte	.LVL178
	.4byte	0x35c1
	.uleb128 0x2f
	.4byte	.LVL179
	.4byte	0x35cd
	.4byte	0x2439
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x2
	.byte	0x74
	.sleb128 0
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x2
	.byte	0x79
	.sleb128 0
	.byte	0
	.uleb128 0x31
	.4byte	.LVL205
	.4byte	0x35d9
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC18
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x74
	.sleb128 0
	.byte	0x79
	.sleb128 0
	.byte	0x1e
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x2
	.byte	0x79
	.sleb128 0
	.byte	0
	.byte	0
	.uleb128 0x2f
	.4byte	.LVL182
	.4byte	0x35d9
	.4byte	0x2476
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x2
	.byte	0x7a
	.sleb128 0
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x2
	.byte	0x78
	.sleb128 0
	.byte	0
	.uleb128 0x26
	.4byte	.LVL186
	.4byte	0x35e6
	.uleb128 0x26
	.4byte	.LVL191
	.4byte	0x35e6
	.uleb128 0x26
	.4byte	.LVL193
	.4byte	0x35e6
	.uleb128 0x26
	.4byte	.LVL195
	.4byte	0x35f3
	.byte	0
	.uleb128 0x32
	.4byte	.LASF651
	.byte	0x1
	.2byte	0x1db
	.byte	0xb
	.4byte	0x875
	.4byte	.LFB74
	.4byte	.LFE74-.LFB74
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x2579
	.uleb128 0x28
	.4byte	.LASF579
	.byte	0x1
	.2byte	0x1db
	.byte	0x37
	.4byte	0x875
	.4byte	.LLST112
	.4byte	.LVUS112
	.uleb128 0x33
	.4byte	.LASF589
	.byte	0x1
	.2byte	0x1dd
	.byte	0xf
	.4byte	0xc6f
	.uleb128 0x2
	.byte	0x91
	.sleb128 -52
	.uleb128 0x2c
	.ascii	"ret\000"
	.byte	0x1
	.2byte	0x1de
	.byte	0x9
	.4byte	0x2c
	.4byte	.LLST113
	.4byte	.LVUS113
	.uleb128 0x2d
	.4byte	0x32c1
	.4byte	.LBI39
	.byte	.LVU785
	.4byte	.Ldebug_ranges0+0x60
	.byte	0x1
	.2byte	0x1f1
	.byte	0x5
	.4byte	0x250c
	.uleb128 0x34
	.4byte	0x32cf
	.byte	0
	.uleb128 0x2d
	.4byte	0x32c1
	.4byte	.LBI43
	.byte	.LVU795
	.4byte	.Ldebug_ranges0+0x78
	.byte	0x1
	.2byte	0x1e6
	.byte	0x9
	.4byte	0x2528
	.uleb128 0x34
	.4byte	0x32cf
	.byte	0
	.uleb128 0x2f
	.4byte	.LVL168
	.4byte	0x35ff
	.4byte	0x2548
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x3
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x2
	.byte	0x91
	.sleb128 -60
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x1
	.byte	0x31
	.byte	0
	.uleb128 0x26
	.4byte	.LVL170
	.4byte	0x30c0
	.uleb128 0x2f
	.4byte	.LVL171
	.4byte	0x360b
	.4byte	0x2565
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x2
	.byte	0x91
	.sleb128 -60
	.byte	0
	.uleb128 0x31
	.4byte	.LVL174
	.4byte	0x3618
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC19
	.byte	0
	.byte	0
	.uleb128 0x35
	.4byte	.LASF591
	.byte	0x1
	.2byte	0x1c7
	.byte	0x6
	.byte	0x1
	.4byte	0x2595
	.uleb128 0x36
	.4byte	.LASF593
	.byte	0x1
	.2byte	0x1c7
	.byte	0x24
	.4byte	0x2c
	.byte	0
	.uleb128 0x25
	.4byte	.LASF580
	.byte	0x1
	.2byte	0x1b8
	.byte	0x6
	.4byte	.LFB72
	.4byte	.LFE72-.LFB72
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x2745
	.uleb128 0x27
	.ascii	"buf\000"
	.byte	0x1
	.2byte	0x1b8
	.byte	0x29
	.4byte	0x875
	.4byte	.LLST89
	.4byte	.LVUS89
	.uleb128 0x2a
	.4byte	.LASF562
	.byte	0x1
	.2byte	0x1be
	.byte	0xb
	.4byte	0x52
	.4byte	.LLST90
	.4byte	.LVUS90
	.uleb128 0x37
	.4byte	0x30c0
	.4byte	.LBI37
	.byte	.LVU676
	.4byte	.LBB37
	.4byte	.LBE37-.LBB37
	.byte	0x1
	.2byte	0x1c1
	.byte	0x5
	.uleb128 0x2e
	.4byte	0x30cd
	.4byte	.LLST91
	.4byte	.LVUS91
	.uleb128 0x38
	.4byte	.LBB38
	.4byte	.LBE38-.LBB38
	.uleb128 0x39
	.4byte	0x30d9
	.4byte	.LLST92
	.4byte	.LVUS92
	.uleb128 0x39
	.4byte	0x30e4
	.4byte	.LLST93
	.4byte	.LVUS93
	.uleb128 0x39
	.4byte	0x30ef
	.4byte	.LLST94
	.4byte	.LVUS94
	.uleb128 0x39
	.4byte	0x30fa
	.4byte	.LLST95
	.4byte	.LVUS95
	.uleb128 0x39
	.4byte	0x3104
	.4byte	.LLST96
	.4byte	.LVUS96
	.uleb128 0x39
	.4byte	0x3110
	.4byte	.LLST97
	.4byte	.LVUS97
	.uleb128 0x39
	.4byte	0x311c
	.4byte	.LLST98
	.4byte	.LVUS98
	.uleb128 0x39
	.4byte	0x3127
	.4byte	.LLST99
	.4byte	.LVUS99
	.uleb128 0x39
	.4byte	0x3132
	.4byte	.LLST100
	.4byte	.LVUS100
	.uleb128 0x39
	.4byte	0x313d
	.4byte	.LLST101
	.4byte	.LVUS101
	.uleb128 0x39
	.4byte	0x3147
	.4byte	.LLST102
	.4byte	.LVUS102
	.uleb128 0x39
	.4byte	0x3153
	.4byte	.LLST103
	.4byte	.LVUS103
	.uleb128 0x3a
	.4byte	0x315f
	.uleb128 0x39
	.4byte	0x3169
	.4byte	.LLST104
	.4byte	.LVUS104
	.uleb128 0x39
	.4byte	0x3173
	.4byte	.LLST105
	.4byte	.LVUS105
	.uleb128 0x39
	.4byte	0x317d
	.4byte	.LLST106
	.4byte	.LVUS106
	.uleb128 0x39
	.4byte	0x3187
	.4byte	.LLST107
	.4byte	.LVUS107
	.uleb128 0x39
	.4byte	0x3193
	.4byte	.LLST108
	.4byte	.LVUS108
	.uleb128 0x39
	.4byte	0x319f
	.4byte	.LLST109
	.4byte	.LVUS109
	.uleb128 0x39
	.4byte	0x31ab
	.4byte	.LLST110
	.4byte	.LVUS110
	.uleb128 0x2f
	.4byte	.LVL147
	.4byte	0x3623
	.4byte	0x2717
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x2
	.byte	0x75
	.sleb128 0
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x1
	.byte	0x30
	.byte	0
	.uleb128 0x31
	.4byte	.LVL160
	.4byte	0x362e
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC3
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC2
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x2
	.byte	0x8
	.byte	0x80
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x5
	.byte	0x3
	.4byte	.LANCHOR0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x25
	.4byte	.LASF581
	.byte	0x1
	.2byte	0x1b0
	.byte	0x6
	.4byte	.LFB71
	.4byte	.LFE71-.LFB71
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x2782
	.uleb128 0x27
	.ascii	"fn\000"
	.byte	0x1
	.2byte	0x1b0
	.byte	0x2c
	.4byte	0xfd
	.4byte	.LLST88
	.4byte	.LVUS88
	.uleb128 0x3b
	.4byte	.LVL143
	.4byte	0x2a61
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x3
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0
	.byte	0
	.uleb128 0x25
	.4byte	.LASF582
	.byte	0x1
	.2byte	0x1a8
	.byte	0x6
	.4byte	.LFB70
	.4byte	.LFE70-.LFB70
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x2908
	.uleb128 0x37
	.4byte	0x30c0
	.4byte	.LBI33
	.byte	.LVU590
	.4byte	.LBB33
	.4byte	.LBE33-.LBB33
	.byte	0x1
	.2byte	0x1aa
	.byte	0x5
	.uleb128 0x2e
	.4byte	0x30cd
	.4byte	.LLST68
	.4byte	.LVUS68
	.uleb128 0x38
	.4byte	.LBB34
	.4byte	.LBE34-.LBB34
	.uleb128 0x39
	.4byte	0x30d9
	.4byte	.LLST69
	.4byte	.LVUS69
	.uleb128 0x39
	.4byte	0x30e4
	.4byte	.LLST70
	.4byte	.LVUS70
	.uleb128 0x39
	.4byte	0x30ef
	.4byte	.LLST71
	.4byte	.LVUS71
	.uleb128 0x39
	.4byte	0x30fa
	.4byte	.LLST72
	.4byte	.LVUS72
	.uleb128 0x39
	.4byte	0x3104
	.4byte	.LLST73
	.4byte	.LVUS73
	.uleb128 0x39
	.4byte	0x3110
	.4byte	.LLST74
	.4byte	.LVUS74
	.uleb128 0x39
	.4byte	0x311c
	.4byte	.LLST75
	.4byte	.LVUS75
	.uleb128 0x39
	.4byte	0x3127
	.4byte	.LLST76
	.4byte	.LVUS76
	.uleb128 0x39
	.4byte	0x3132
	.4byte	.LLST77
	.4byte	.LVUS77
	.uleb128 0x39
	.4byte	0x313d
	.4byte	.LLST78
	.4byte	.LVUS78
	.uleb128 0x39
	.4byte	0x3147
	.4byte	.LLST79
	.4byte	.LVUS79
	.uleb128 0x39
	.4byte	0x3153
	.4byte	.LLST80
	.4byte	.LVUS80
	.uleb128 0x3a
	.4byte	0x315f
	.uleb128 0x39
	.4byte	0x3169
	.4byte	.LLST81
	.4byte	.LVUS81
	.uleb128 0x39
	.4byte	0x3173
	.4byte	.LLST82
	.4byte	.LVUS82
	.uleb128 0x39
	.4byte	0x317d
	.4byte	.LLST83
	.4byte	.LVUS83
	.uleb128 0x39
	.4byte	0x3187
	.4byte	.LLST84
	.4byte	.LVUS84
	.uleb128 0x39
	.4byte	0x3193
	.4byte	.LLST85
	.4byte	.LVUS85
	.uleb128 0x39
	.4byte	0x319f
	.4byte	.LLST86
	.4byte	.LVUS86
	.uleb128 0x39
	.4byte	0x31ab
	.4byte	.LLST87
	.4byte	.LVUS87
	.uleb128 0x2f
	.4byte	.LVL128
	.4byte	0x3623
	.4byte	0x28da
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x2
	.byte	0x75
	.sleb128 0
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x1
	.byte	0x30
	.byte	0
	.uleb128 0x31
	.4byte	.LVL140
	.4byte	0x362e
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC3
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC2
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x2
	.byte	0x8
	.byte	0x80
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x5
	.byte	0x3
	.4byte	.LANCHOR0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x25
	.4byte	.LASF583
	.byte	0x1
	.2byte	0x199
	.byte	0x6
	.4byte	.LFB69
	.4byte	.LFE69-.LFB69
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x2a61
	.uleb128 0x28
	.4byte	.LASF584
	.byte	0x1
	.2byte	0x199
	.byte	0x1c
	.4byte	0x2c
	.4byte	.LLST59
	.4byte	.LVUS59
	.uleb128 0x28
	.4byte	.LASF585
	.byte	0x1
	.2byte	0x199
	.byte	0x2b
	.4byte	0x2c
	.4byte	.LLST60
	.4byte	.LVUS60
	.uleb128 0x28
	.4byte	.LASF586
	.byte	0x1
	.2byte	0x199
	.byte	0x37
	.4byte	0x2c
	.4byte	.LLST61
	.4byte	.LVUS61
	.uleb128 0x28
	.4byte	.LASF587
	.byte	0x1
	.2byte	0x199
	.byte	0x49
	.4byte	0x2c
	.4byte	.LLST62
	.4byte	.LVUS62
	.uleb128 0x2d
	.4byte	0x329d
	.4byte	.LBI18
	.byte	.LVU550
	.4byte	.Ldebug_ranges0+0x18
	.byte	0x1
	.2byte	0x19b
	.byte	0x5
	.4byte	0x29d4
	.uleb128 0x3c
	.4byte	.Ldebug_ranges0+0x20
	.uleb128 0x39
	.4byte	0x32aa
	.4byte	.LLST63
	.4byte	.LVUS63
	.uleb128 0x3a
	.4byte	0x32b4
	.uleb128 0x31
	.4byte	.LVL120
	.4byte	0x363a
	.uleb128 0x30
	.uleb128 0x8
	.byte	0x90
	.uleb128 0x40
	.byte	0x93
	.uleb128 0x4
	.byte	0x90
	.uleb128 0x41
	.byte	0x93
	.uleb128 0x4
	.uleb128 0xf
	.byte	0x75
	.sleb128 0
	.byte	0x74
	.sleb128 0
	.byte	0x22
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf7
	.uleb128 0x25
	.byte	0xf5
	.uleb128 0x54
	.uleb128 0x25
	.byte	0x1b
	.byte	0xf7
	.uleb128 0x3a
	.uleb128 0x30
	.uleb128 0x8
	.byte	0x90
	.uleb128 0x42
	.byte	0x93
	.uleb128 0x4
	.byte	0x90
	.uleb128 0x43
	.byte	0x93
	.uleb128 0x4
	.uleb128 0x3
	.byte	0xf5
	.uleb128 0x52
	.uleb128 0x3a
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x2d
	.4byte	0x2b9f
	.4byte	.LBI23
	.byte	.LVU568
	.4byte	.Ldebug_ranges0+0x38
	.byte	0x1
	.2byte	0x19e
	.byte	0x5
	.4byte	0x2a1f
	.uleb128 0x2e
	.4byte	0x2bcf
	.4byte	.LLST64
	.4byte	.LVUS64
	.uleb128 0x2e
	.4byte	0x2bc4
	.4byte	.LLST65
	.4byte	.LVUS65
	.uleb128 0x2e
	.4byte	0x2bb9
	.4byte	.LLST66
	.4byte	.LVUS66
	.uleb128 0x2e
	.4byte	0x2bad
	.4byte	.LLST67
	.4byte	.LVUS67
	.byte	0
	.uleb128 0x2f
	.4byte	.LVL124
	.4byte	0x2d51
	.4byte	0x2a44
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x1
	.byte	0x30
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x2
	.byte	0x76
	.sleb128 0
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x2
	.byte	0x77
	.sleb128 0
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x2
	.byte	0x76
	.sleb128 0
	.byte	0
	.uleb128 0x3b
	.4byte	.LVL126
	.4byte	0x35d9
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC1
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC0
	.byte	0
	.byte	0
	.uleb128 0x25
	.4byte	.LASF588
	.byte	0x1
	.2byte	0x17e
	.byte	0x6
	.4byte	.LFB68
	.4byte	.LFE68-.LFB68
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x2b9f
	.uleb128 0x28
	.4byte	.LASF589
	.byte	0x1
	.2byte	0x17e
	.byte	0x27
	.4byte	0x227c
	.4byte	.LLST53
	.4byte	.LVUS53
	.uleb128 0x27
	.ascii	"fn\000"
	.byte	0x1
	.2byte	0x17e
	.byte	0x35
	.4byte	0xfd
	.4byte	.LLST54
	.4byte	.LVUS54
	.uleb128 0x2c
	.ascii	"fp\000"
	.byte	0x1
	.2byte	0x180
	.byte	0xb
	.4byte	0x31a
	.4byte	.LLST55
	.4byte	.LVUS55
	.uleb128 0x2a
	.4byte	.LASF590
	.byte	0x1
	.2byte	0x181
	.byte	0xe
	.4byte	0x40b
	.4byte	.LLST56
	.4byte	.LVUS56
	.uleb128 0x2c
	.ascii	"xx\000"
	.byte	0x1
	.2byte	0x182
	.byte	0x9
	.4byte	0x2c
	.4byte	.LLST57
	.4byte	.LVUS57
	.uleb128 0x2c
	.ascii	"yy\000"
	.byte	0x1
	.2byte	0x182
	.byte	0xd
	.4byte	0x2c
	.4byte	.LLST58
	.4byte	.LVUS58
	.uleb128 0x2f
	.4byte	.LVL101
	.4byte	0x3646
	.4byte	0x2b10
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x3
	.byte	0xf3
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC13
	.byte	0
	.uleb128 0x2f
	.4byte	.LVL104
	.4byte	0x3653
	.4byte	0x2b37
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC14
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x1
	.byte	0x31
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x1
	.byte	0x33
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x2
	.byte	0x77
	.sleb128 0
	.byte	0
	.uleb128 0x2f
	.4byte	.LVL105
	.4byte	0x365e
	.4byte	0x2b54
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x2
	.byte	0x77
	.sleb128 0
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC15
	.byte	0
	.uleb128 0x2f
	.4byte	.LVL106
	.4byte	0x3653
	.4byte	0x2b7b
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC16
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x1
	.byte	0x31
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x1
	.byte	0x34
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x2
	.byte	0x77
	.sleb128 0
	.byte	0
	.uleb128 0x2f
	.4byte	.LVL112
	.4byte	0x365e
	.4byte	0x2b95
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x2
	.byte	0x77
	.sleb128 0
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x2
	.byte	0x79
	.sleb128 0
	.byte	0
	.uleb128 0x3d
	.4byte	.LVL117
	.4byte	0x366b
	.byte	0
	.uleb128 0x35
	.4byte	.LASF592
	.byte	0x1
	.2byte	0x16f
	.byte	0x6
	.byte	0x1
	.4byte	0x2bdb
	.uleb128 0x3e
	.ascii	"ch\000"
	.byte	0x1
	.2byte	0x16f
	.byte	0x25
	.4byte	0x2c
	.uleb128 0x3e
	.ascii	"r\000"
	.byte	0x1
	.2byte	0x16f
	.byte	0x2d
	.4byte	0x2c
	.uleb128 0x3e
	.ascii	"g\000"
	.byte	0x1
	.2byte	0x16f
	.byte	0x34
	.4byte	0x2c
	.uleb128 0x3e
	.ascii	"b\000"
	.byte	0x1
	.2byte	0x16f
	.byte	0x3b
	.4byte	0x2c
	.byte	0
	.uleb128 0x25
	.4byte	.LASF594
	.byte	0x1
	.2byte	0x166
	.byte	0x6
	.4byte	.LFB66
	.4byte	.LFE66-.LFB66
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x2c17
	.uleb128 0x28
	.4byte	.LASF537
	.byte	0x1
	.2byte	0x166
	.byte	0x24
	.4byte	0x40b
	.4byte	.LLST51
	.4byte	.LVUS51
	.uleb128 0x3b
	.4byte	.LVL95
	.4byte	0x3623
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x1
	.byte	0x30
	.byte	0
	.byte	0
	.uleb128 0x25
	.4byte	.LASF595
	.byte	0x1
	.2byte	0x15d
	.byte	0x6
	.4byte	.LFB65
	.4byte	.LFE65-.LFB65
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x2c7f
	.uleb128 0x28
	.4byte	.LASF596
	.byte	0x1
	.2byte	0x15d
	.byte	0x2c
	.4byte	0x40b
	.4byte	.LLST50
	.4byte	.LVUS50
	.uleb128 0x3f
	.4byte	.LASF599
	.4byte	0x2c8f
	.uleb128 0x5
	.byte	0x3
	.4byte	__PRETTY_FUNCTION__.17134
	.uleb128 0x31
	.4byte	.LVL92
	.4byte	0x362e
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC12
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC2
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x3
	.byte	0xa
	.2byte	0x15f
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x5
	.byte	0x3
	.4byte	.LANCHOR0+80
	.byte	0
	.byte	0
	.uleb128 0xc
	.4byte	0x10a
	.4byte	0x2c8f
	.uleb128 0xd
	.4byte	0x33
	.byte	0x1c
	.byte	0
	.uleb128 0x5
	.4byte	0x2c7f
	.uleb128 0x25
	.4byte	.LASF597
	.byte	0x1
	.2byte	0x14f
	.byte	0x6
	.4byte	.LFB64
	.4byte	.LFE64-.LFB64
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x2cd4
	.uleb128 0x27
	.ascii	"set\000"
	.byte	0x1
	.2byte	0x14f
	.byte	0x2e
	.4byte	0x2c
	.4byte	.LLST49
	.4byte	.LVUS49
	.uleb128 0x3b
	.4byte	.LVL89
	.4byte	0x3618
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC11
	.byte	0
	.byte	0
	.uleb128 0x25
	.4byte	.LASF598
	.byte	0x1
	.2byte	0x145
	.byte	0x6
	.4byte	.LFB63
	.4byte	.LFE63-.LFB63
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x2d3c
	.uleb128 0x28
	.4byte	.LASF542
	.byte	0x1
	.2byte	0x145
	.byte	0x28
	.4byte	0x2276
	.4byte	.LLST48
	.4byte	.LVUS48
	.uleb128 0x3f
	.4byte	.LASF599
	.4byte	0x2d4c
	.uleb128 0x5
	.byte	0x3
	.4byte	__PRETTY_FUNCTION__.17127
	.uleb128 0x31
	.4byte	.LVL84
	.4byte	0x362e
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC10
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC2
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x3
	.byte	0xa
	.2byte	0x147
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x5
	.byte	0x3
	.4byte	.LANCHOR0+52
	.byte	0
	.byte	0
	.uleb128 0xc
	.4byte	0x10a
	.4byte	0x2d4c
	.uleb128 0xd
	.4byte	0x33
	.byte	0x18
	.byte	0
	.uleb128 0x5
	.4byte	0x2d3c
	.uleb128 0x40
	.4byte	.LASF600
	.byte	0x1
	.byte	0xe9
	.byte	0x6
	.4byte	.LFB62
	.4byte	.LFE62-.LFB62
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x2fb7
	.uleb128 0x41
	.4byte	.LASF601
	.byte	0x1
	.byte	0xe9
	.byte	0x24
	.4byte	0x40b
	.4byte	.LLST41
	.4byte	.LVUS41
	.uleb128 0x41
	.4byte	.LASF602
	.byte	0x1
	.byte	0xe9
	.byte	0x3a
	.4byte	0x40b
	.4byte	.LLST42
	.4byte	.LVUS42
	.uleb128 0x41
	.4byte	.LASF552
	.byte	0x1
	.byte	0xe9
	.byte	0x4e
	.4byte	0x40b
	.4byte	.LLST43
	.4byte	.LVUS43
	.uleb128 0x41
	.4byte	.LASF550
	.byte	0x1
	.byte	0xe9
	.byte	0x62
	.4byte	0x40b
	.4byte	.LLST44
	.4byte	.LVUS44
	.uleb128 0x42
	.4byte	.LASF558
	.byte	0x1
	.byte	0xe9
	.byte	0x78
	.4byte	0x40b
	.uleb128 0x2
	.byte	0x91
	.sleb128 0
	.uleb128 0x42
	.4byte	.LASF559
	.byte	0x1
	.byte	0xe9
	.byte	0x8f
	.4byte	0x40b
	.uleb128 0x2
	.byte	0x91
	.sleb128 4
	.uleb128 0x42
	.4byte	.LASF603
	.byte	0x1
	.byte	0xe9
	.byte	0xa7
	.4byte	0x40b
	.uleb128 0x2
	.byte	0x91
	.sleb128 8
	.uleb128 0x43
	.4byte	.LASF604
	.byte	0x1
	.byte	0xeb
	.byte	0xe
	.4byte	0x40b
	.4byte	.LLST45
	.4byte	.LVUS45
	.uleb128 0x44
	.ascii	"xx\000"
	.byte	0x1
	.byte	0xeb
	.byte	0x16
	.4byte	0x40b
	.4byte	.LLST46
	.4byte	.LVUS46
	.uleb128 0x43
	.4byte	.LASF605
	.byte	0x1
	.byte	0xec
	.byte	0xb
	.4byte	0x25
	.4byte	.LLST47
	.4byte	.LVUS47
	.uleb128 0x3f
	.4byte	.LASF599
	.4byte	0x2fc7
	.uleb128 0x5
	.byte	0x3
	.4byte	__PRETTY_FUNCTION__.17120
	.uleb128 0x2f
	.4byte	.LVL63
	.4byte	0x35d9
	.4byte	0x2e66
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC4
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x2
	.byte	0x74
	.sleb128 0
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x2
	.byte	0x75
	.sleb128 0
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x2
	.byte	0x78
	.sleb128 0
	.uleb128 0x30
	.uleb128 0x2
	.byte	0x7d
	.sleb128 0
	.uleb128 0x2
	.byte	0x79
	.sleb128 0
	.uleb128 0x30
	.uleb128 0x2
	.byte	0x7d
	.sleb128 12
	.uleb128 0x3
	.byte	0x91
	.sleb128 0
	.byte	0x6
	.byte	0
	.uleb128 0x2f
	.4byte	.LVL66
	.4byte	0x35d9
	.4byte	0x2ec4
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC6
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x9
	.byte	0x76
	.sleb128 4
	.byte	0x76
	.sleb128 4
	.byte	0x37
	.byte	0x24
	.byte	0x22
	.byte	0x33
	.byte	0x24
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x19
	.byte	0x76
	.sleb128 0
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf7
	.uleb128 0x25
	.byte	0x75
	.sleb128 0
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf7
	.uleb128 0x25
	.byte	0x1b
	.byte	0xf4
	.uleb128 0x25
	.byte	0x4
	.4byte	0x47800000
	.byte	0x1e
	.byte	0xf7
	.uleb128 0x25
	.byte	0xf7
	.uleb128 0x33
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x19
	.byte	0x76
	.sleb128 0
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf7
	.uleb128 0x25
	.byte	0x75
	.sleb128 0
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf7
	.uleb128 0x25
	.byte	0x1b
	.byte	0xf4
	.uleb128 0x25
	.byte	0x4
	.4byte	0x47800000
	.byte	0x1e
	.byte	0xf7
	.uleb128 0x25
	.byte	0xf7
	.uleb128 0x33
	.byte	0
	.uleb128 0x26
	.4byte	.LVL67
	.4byte	0x35c1
	.uleb128 0x2f
	.4byte	.LVL68
	.4byte	0x35cd
	.4byte	0x2ee0
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x1
	.byte	0x31
	.byte	0
	.uleb128 0x2f
	.4byte	.LVL71
	.4byte	0x3677
	.4byte	0x2ef6
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x4
	.byte	0x75
	.sleb128 0
	.byte	0x31
	.byte	0x24
	.byte	0
	.uleb128 0x26
	.4byte	.LVL74
	.4byte	0x3677
	.uleb128 0x2f
	.4byte	.LVL75
	.4byte	0x35d9
	.4byte	0x2f22
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC9
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x2
	.byte	0x77
	.sleb128 0
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x2
	.byte	0x76
	.sleb128 0
	.byte	0
	.uleb128 0x3d
	.4byte	.LVL78
	.4byte	0x3683
	.uleb128 0x2f
	.4byte	.LVL79
	.4byte	0x362e
	.4byte	0x2f5a
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC5
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC2
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x2
	.byte	0x8
	.byte	0xf1
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x5
	.byte	0x3
	.4byte	.LANCHOR0+28
	.byte	0
	.uleb128 0x2f
	.4byte	.LVL80
	.4byte	0x362e
	.4byte	0x2f8a
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC8
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC2
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x3
	.byte	0xa
	.2byte	0x12e
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x5
	.byte	0x3
	.4byte	.LANCHOR0+28
	.byte	0
	.uleb128 0x31
	.4byte	.LVL81
	.4byte	0x362e
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC7
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC2
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x3
	.byte	0xa
	.2byte	0x126
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x5
	.byte	0x3
	.4byte	.LANCHOR0+28
	.byte	0
	.byte	0
	.uleb128 0xc
	.4byte	0x10a
	.4byte	0x2fc7
	.uleb128 0xd
	.4byte	0x33
	.byte	0x14
	.byte	0
	.uleb128 0x5
	.4byte	0x2fb7
	.uleb128 0x45
	.4byte	.LASF606
	.byte	0x1
	.byte	0xd3
	.byte	0x6
	.4byte	.LFB61
	.4byte	.LFE61-.LFB61
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x30c0
	.uleb128 0x44
	.ascii	"yy\000"
	.byte	0x1
	.byte	0xd5
	.byte	0xe
	.4byte	0x40b
	.4byte	.LLST30
	.4byte	.LVUS30
	.uleb128 0x43
	.4byte	.LASF607
	.byte	0x1
	.byte	0xd6
	.byte	0xe
	.4byte	0x40b
	.4byte	.LLST31
	.4byte	.LVUS31
	.uleb128 0x43
	.4byte	.LASF608
	.byte	0x1
	.byte	0xd6
	.byte	0x35
	.4byte	0x40b
	.4byte	.LLST32
	.4byte	.LVUS32
	.uleb128 0x46
	.4byte	0x31dc
	.4byte	.LBI10
	.byte	.LVU232
	.4byte	.Ldebug_ranges0+0
	.byte	0x1
	.byte	0xde
	.byte	0x9
	.4byte	0x30b0
	.uleb128 0x2e
	.4byte	0x31f5
	.4byte	.LLST33
	.4byte	.LVUS33
	.uleb128 0x34
	.4byte	0x31e9
	.uleb128 0x3c
	.4byte	.Ldebug_ranges0+0
	.uleb128 0x39
	.4byte	0x3201
	.4byte	.LLST34
	.4byte	.LVUS34
	.uleb128 0x39
	.4byte	0x320c
	.4byte	.LLST35
	.4byte	.LVUS35
	.uleb128 0x39
	.4byte	0x3217
	.4byte	.LLST36
	.4byte	.LVUS36
	.uleb128 0x39
	.4byte	0x3221
	.4byte	.LLST37
	.4byte	.LVUS37
	.uleb128 0x3a
	.4byte	0x322d
	.uleb128 0x39
	.4byte	0x3239
	.4byte	.LLST38
	.4byte	.LVUS38
	.uleb128 0x3a
	.4byte	0x3245
	.uleb128 0x39
	.4byte	0x3251
	.4byte	.LLST39
	.4byte	.LVUS39
	.uleb128 0x39
	.4byte	0x325d
	.4byte	.LLST40
	.4byte	.LVUS40
	.byte	0
	.byte	0
	.uleb128 0x31
	.4byte	.LVL39
	.4byte	0x3623
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x1
	.byte	0x30
	.byte	0
	.byte	0
	.uleb128 0x47
	.4byte	.LASF609
	.byte	0x1
	.byte	0x75
	.byte	0x6
	.byte	0x1
	.4byte	0x31c7
	.uleb128 0x48
	.4byte	.LASF610
	.byte	0x1
	.byte	0x75
	.byte	0x2b
	.4byte	0x227c
	.uleb128 0x49
	.ascii	"xx\000"
	.byte	0x1
	.byte	0x77
	.byte	0xe
	.4byte	0x40b
	.uleb128 0x49
	.ascii	"yy\000"
	.byte	0x1
	.byte	0x77
	.byte	0x12
	.4byte	0x40b
	.uleb128 0x49
	.ascii	"ye\000"
	.byte	0x1
	.byte	0x77
	.byte	0x16
	.4byte	0x40b
	.uleb128 0x49
	.ascii	"y\000"
	.byte	0x1
	.byte	0x77
	.byte	0x1a
	.4byte	0x40b
	.uleb128 0x4a
	.4byte	.LASF611
	.byte	0x1
	.byte	0x77
	.byte	0x1d
	.4byte	0x40b
	.uleb128 0x4a
	.4byte	.LASF612
	.byte	0x1
	.byte	0x77
	.byte	0x23
	.4byte	0x40b
	.uleb128 0x49
	.ascii	"rr\000"
	.byte	0x1
	.byte	0x79
	.byte	0x9
	.4byte	0x2c
	.uleb128 0x49
	.ascii	"gg\000"
	.byte	0x1
	.byte	0x79
	.byte	0xd
	.4byte	0x2c
	.uleb128 0x49
	.ascii	"bb\000"
	.byte	0x1
	.byte	0x79
	.byte	0x11
	.4byte	0x2c
	.uleb128 0x49
	.ascii	"n\000"
	.byte	0x1
	.byte	0x79
	.byte	0x15
	.4byte	0x2c
	.uleb128 0x4a
	.4byte	.LASF613
	.byte	0x1
	.byte	0x79
	.byte	0x18
	.4byte	0x2c
	.uleb128 0x4a
	.4byte	.LASF614
	.byte	0x1
	.byte	0x79
	.byte	0x1e
	.4byte	0x2c
	.uleb128 0x49
	.ascii	"w\000"
	.byte	0x1
	.byte	0x79
	.byte	0x24
	.4byte	0x2c
	.uleb128 0x49
	.ascii	"r\000"
	.byte	0x1
	.byte	0x7a
	.byte	0xd
	.4byte	0x3f3
	.uleb128 0x49
	.ascii	"g\000"
	.byte	0x1
	.byte	0x7a
	.byte	0x10
	.4byte	0x3f3
	.uleb128 0x49
	.ascii	"b\000"
	.byte	0x1
	.byte	0x7a
	.byte	0x13
	.4byte	0x3f3
	.uleb128 0x4a
	.4byte	.LASF615
	.byte	0x1
	.byte	0x7a
	.byte	0x16
	.4byte	0x3f3
	.uleb128 0x4a
	.4byte	.LASF616
	.byte	0x1
	.byte	0x7c
	.byte	0xf
	.4byte	0x227c
	.uleb128 0x4a
	.4byte	.LASF617
	.byte	0x1
	.byte	0x7d
	.byte	0xf
	.4byte	0x227c
	.uleb128 0x4a
	.4byte	.LASF618
	.byte	0x1
	.byte	0x7e
	.byte	0xe
	.4byte	0x40b
	.uleb128 0x3f
	.4byte	.LASF599
	.4byte	0x31d7
	.uleb128 0x5
	.byte	0x3
	.4byte	__PRETTY_FUNCTION__.17096
	.byte	0
	.uleb128 0xc
	.4byte	0x10a
	.4byte	0x31d7
	.uleb128 0xd
	.4byte	0x33
	.byte	0x1a
	.byte	0
	.uleb128 0x5
	.4byte	0x31c7
	.uleb128 0x47
	.4byte	.LASF619
	.byte	0x1
	.byte	0x3e
	.byte	0x6
	.byte	0x1
	.4byte	0x326a
	.uleb128 0x48
	.4byte	.LASF620
	.byte	0x1
	.byte	0x3e
	.byte	0x30
	.4byte	0x40b
	.uleb128 0x48
	.4byte	.LASF621
	.byte	0x1
	.byte	0x3e
	.byte	0x42
	.4byte	0x40b
	.uleb128 0x49
	.ascii	"yy\000"
	.byte	0x1
	.byte	0x40
	.byte	0x9
	.4byte	0x2c
	.uleb128 0x49
	.ascii	"ys\000"
	.byte	0x1
	.byte	0x40
	.byte	0xd
	.4byte	0x2c
	.uleb128 0x49
	.ascii	"w\000"
	.byte	0x1
	.byte	0x40
	.byte	0x11
	.4byte	0x2c
	.uleb128 0x4a
	.4byte	.LASF622
	.byte	0x1
	.byte	0x40
	.byte	0x14
	.4byte	0x2c
	.uleb128 0x4a
	.4byte	.LASF615
	.byte	0x1
	.byte	0x41
	.byte	0xe
	.4byte	0x40b
	.uleb128 0x4a
	.4byte	.LASF611
	.byte	0x1
	.byte	0x41
	.byte	0x15
	.4byte	0x40b
	.uleb128 0x4a
	.4byte	.LASF623
	.byte	0x1
	.byte	0x42
	.byte	0xe
	.4byte	0x2276
	.uleb128 0x4a
	.4byte	.LASF624
	.byte	0x1
	.byte	0x43
	.byte	0x10
	.4byte	0x2270
	.uleb128 0x4a
	.4byte	.LASF625
	.byte	0x1
	.byte	0x44
	.byte	0x10
	.4byte	0x2270
	.byte	0
	.uleb128 0x40
	.4byte	.LASF626
	.byte	0x1
	.byte	0x2f
	.byte	0x6
	.4byte	.LFB58
	.4byte	.LFE58-.LFB58
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x329d
	.uleb128 0x3b
	.4byte	.LVL5
	.4byte	0x35d9
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC1
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC0
	.byte	0
	.byte	0
	.uleb128 0x4b
	.4byte	.LASF652
	.byte	0x1
	.byte	0x22
	.byte	0x6
	.byte	0x1
	.4byte	0x32c1
	.uleb128 0x49
	.ascii	"i\000"
	.byte	0x1
	.byte	0x24
	.byte	0x9
	.4byte	0x2c
	.uleb128 0x4a
	.4byte	.LASF627
	.byte	0x1
	.byte	0x25
	.byte	0xb
	.4byte	0x25
	.byte	0
	.uleb128 0x4c
	.4byte	.LASF653
	.byte	0x2
	.2byte	0x1c8
	.byte	0x14
	.byte	0x3
	.4byte	0x32dc
	.uleb128 0x3e
	.ascii	"op\000"
	.byte	0x2
	.2byte	0x1c8
	.byte	0x29
	.4byte	0x875
	.byte	0
	.uleb128 0x4d
	.4byte	0x329d
	.4byte	.LFB57
	.4byte	.LFE57-.LFB57
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x3339
	.uleb128 0x39
	.4byte	0x32aa
	.4byte	.LLST0
	.4byte	.LVUS0
	.uleb128 0x4e
	.4byte	0x32b4
	.byte	0x4
	.4byte	0x3f666666
	.uleb128 0x31
	.4byte	.LVL2
	.4byte	0x363a
	.uleb128 0x30
	.uleb128 0x8
	.byte	0x90
	.uleb128 0x40
	.byte	0x93
	.uleb128 0x4
	.byte	0x90
	.uleb128 0x41
	.byte	0x93
	.uleb128 0x4
	.uleb128 0xf
	.byte	0x75
	.sleb128 0
	.byte	0x74
	.sleb128 0
	.byte	0x22
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf7
	.uleb128 0x25
	.byte	0xf5
	.uleb128 0x54
	.uleb128 0x25
	.byte	0x1b
	.byte	0xf7
	.uleb128 0x3a
	.uleb128 0x30
	.uleb128 0x8
	.byte	0x90
	.uleb128 0x42
	.byte	0x93
	.uleb128 0x4
	.byte	0x90
	.uleb128 0x43
	.byte	0x93
	.uleb128 0x4
	.uleb128 0x3
	.byte	0xf5
	.uleb128 0x52
	.uleb128 0x3a
	.byte	0
	.byte	0
	.uleb128 0x4d
	.4byte	0x31dc
	.4byte	.LFB59
	.4byte	.LFE59-.LFB59
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x33cc
	.uleb128 0x2e
	.4byte	0x31e9
	.4byte	.LLST1
	.4byte	.LVUS1
	.uleb128 0x2e
	.4byte	0x31f5
	.4byte	.LLST2
	.4byte	.LVUS2
	.uleb128 0x39
	.4byte	0x3201
	.4byte	.LLST3
	.4byte	.LVUS3
	.uleb128 0x39
	.4byte	0x320c
	.4byte	.LLST4
	.4byte	.LVUS4
	.uleb128 0x39
	.4byte	0x3217
	.4byte	.LLST5
	.4byte	.LVUS5
	.uleb128 0x39
	.4byte	0x3221
	.4byte	.LLST6
	.4byte	.LVUS6
	.uleb128 0x3a
	.4byte	0x322d
	.uleb128 0x39
	.4byte	0x3239
	.4byte	.LLST7
	.4byte	.LVUS7
	.uleb128 0x3a
	.4byte	0x3245
	.uleb128 0x39
	.4byte	0x3251
	.4byte	.LLST8
	.4byte	.LVUS8
	.uleb128 0x39
	.4byte	0x325d
	.4byte	.LLST9
	.4byte	.LVUS9
	.byte	0
	.uleb128 0x4d
	.4byte	0x30c0
	.4byte	.LFB60
	.4byte	.LFE60-.LFB60
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x352d
	.uleb128 0x2e
	.4byte	0x30cd
	.4byte	.LLST10
	.4byte	.LVUS10
	.uleb128 0x39
	.4byte	0x30d9
	.4byte	.LLST11
	.4byte	.LVUS11
	.uleb128 0x39
	.4byte	0x30e4
	.4byte	.LLST12
	.4byte	.LVUS12
	.uleb128 0x39
	.4byte	0x30ef
	.4byte	.LLST13
	.4byte	.LVUS13
	.uleb128 0x39
	.4byte	0x30fa
	.4byte	.LLST14
	.4byte	.LVUS14
	.uleb128 0x39
	.4byte	0x3104
	.4byte	.LLST15
	.4byte	.LVUS15
	.uleb128 0x39
	.4byte	0x3110
	.4byte	.LLST16
	.4byte	.LVUS16
	.uleb128 0x39
	.4byte	0x311c
	.4byte	.LLST17
	.4byte	.LVUS17
	.uleb128 0x39
	.4byte	0x3127
	.4byte	.LLST18
	.4byte	.LVUS18
	.uleb128 0x39
	.4byte	0x3132
	.4byte	.LLST19
	.4byte	.LVUS19
	.uleb128 0x39
	.4byte	0x313d
	.4byte	.LLST20
	.4byte	.LVUS20
	.uleb128 0x39
	.4byte	0x3147
	.4byte	.LLST21
	.4byte	.LVUS21
	.uleb128 0x39
	.4byte	0x3153
	.4byte	.LLST22
	.4byte	.LVUS22
	.uleb128 0x3a
	.4byte	0x315f
	.uleb128 0x39
	.4byte	0x3169
	.4byte	.LLST23
	.4byte	.LVUS23
	.uleb128 0x39
	.4byte	0x3173
	.4byte	.LLST24
	.4byte	.LVUS24
	.uleb128 0x39
	.4byte	0x317d
	.4byte	.LLST25
	.4byte	.LVUS25
	.uleb128 0x39
	.4byte	0x3187
	.4byte	.LLST26
	.4byte	.LVUS26
	.uleb128 0x39
	.4byte	0x3193
	.4byte	.LLST27
	.4byte	.LVUS27
	.uleb128 0x39
	.4byte	0x319f
	.4byte	.LLST28
	.4byte	.LVUS28
	.uleb128 0x39
	.4byte	0x31ab
	.4byte	.LLST29
	.4byte	.LVUS29
	.uleb128 0x2f
	.4byte	.LVL24
	.4byte	0x3623
	.4byte	0x3501
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x2
	.byte	0x75
	.sleb128 0
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x1
	.byte	0x30
	.byte	0
	.uleb128 0x31
	.4byte	.LVL37
	.4byte	0x362e
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC3
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC2
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x2
	.byte	0x8
	.byte	0x80
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x5
	.byte	0x3
	.4byte	.LANCHOR0
	.byte	0
	.byte	0
	.uleb128 0x4d
	.4byte	0x2b9f
	.4byte	.LFB67
	.4byte	.LFE67-.LFB67
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x3563
	.uleb128 0x2e
	.4byte	0x2bad
	.4byte	.LLST52
	.4byte	.LVUS52
	.uleb128 0x4f
	.4byte	0x2bb9
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x4f
	.4byte	0x2bc4
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x4f
	.4byte	0x2bcf
	.uleb128 0x1
	.byte	0x53
	.byte	0
	.uleb128 0x4d
	.4byte	0x2579
	.4byte	.LFB73
	.4byte	.LFE73-.LFB73
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x35c1
	.uleb128 0x2e
	.4byte	0x2587
	.4byte	.LLST111
	.4byte	.LVUS111
	.uleb128 0x26
	.4byte	.LVL163
	.4byte	0x35c1
	.uleb128 0x2f
	.4byte	.LVL164
	.4byte	0x35cd
	.4byte	0x35a6
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x2
	.byte	0x75
	.sleb128 0
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x2
	.byte	0x76
	.sleb128 0
	.byte	0
	.uleb128 0x3b
	.4byte	.LVL166
	.4byte	0x35d9
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC18
	.uleb128 0x30
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x3
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0
	.byte	0
	.uleb128 0x50
	.4byte	.LASF628
	.4byte	.LASF628
	.byte	0x44
	.byte	0x3d
	.byte	0xd
	.uleb128 0x50
	.4byte	.LASF629
	.4byte	.LASF629
	.byte	0x44
	.byte	0x29
	.byte	0xe
	.uleb128 0x51
	.4byte	.LASF630
	.4byte	.LASF630
	.byte	0x7
	.2byte	0x14c
	.byte	0xc
	.uleb128 0x51
	.4byte	.LASF631
	.4byte	.LASF631
	.byte	0x46
	.2byte	0x1c5
	.byte	0xc
	.uleb128 0x52
	.ascii	"sin\000"
	.ascii	"sin\000"
	.byte	0x4a
	.byte	0x40
	.byte	0x1
	.uleb128 0x50
	.4byte	.LASF632
	.4byte	.LASF632
	.byte	0x47
	.byte	0xcb
	.byte	0x11
	.uleb128 0x51
	.4byte	.LASF633
	.4byte	.LASF633
	.byte	0x47
	.2byte	0x103
	.byte	0x12
	.uleb128 0x53
	.4byte	.LASF634
	.4byte	.LASF636
	.byte	0x48
	.byte	0
	.uleb128 0x53
	.4byte	.LASF635
	.4byte	.LASF637
	.byte	0x48
	.byte	0
	.uleb128 0x50
	.4byte	.LASF638
	.4byte	.LASF638
	.byte	0x49
	.byte	0x45
	.byte	0xd
	.uleb128 0x52
	.ascii	"pow\000"
	.ascii	"pow\000"
	.byte	0x4a
	.byte	0x8c
	.byte	0x1
	.uleb128 0x51
	.4byte	.LASF639
	.4byte	.LASF640
	.byte	0x7
	.2byte	0x101
	.byte	0xe
	.uleb128 0x53
	.4byte	.LASF641
	.4byte	.LASF642
	.byte	0x48
	.byte	0
	.uleb128 0x51
	.4byte	.LASF643
	.4byte	.LASF643
	.byte	0x7
	.2byte	0x146
	.byte	0xc
	.uleb128 0x50
	.4byte	.LASF644
	.4byte	.LASF644
	.byte	0x7
	.byte	0xd5
	.byte	0xc
	.uleb128 0x50
	.4byte	.LASF645
	.4byte	.LASF645
	.byte	0x44
	.byte	0x26
	.byte	0xe
	.uleb128 0x50
	.4byte	.LASF646
	.4byte	.LASF646
	.byte	0x44
	.byte	0x89
	.byte	0xd
	.byte	0
	.section	.debug_abbrev,"",%progbits
.Ldebug_abbrev0:
	.uleb128 0x1
	.uleb128 0x11
	.byte	0x1
	.uleb128 0x25
	.uleb128 0xe
	.uleb128 0x13
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x1b
	.uleb128 0xe
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x6
	.uleb128 0x10
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x2
	.uleb128 0x24
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3e
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0xe
	.byte	0
	.byte	0
	.uleb128 0x3
	.uleb128 0x24
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3e
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0x8
	.byte	0
	.byte	0
	.uleb128 0x4
	.uleb128 0x16
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x5
	.uleb128 0x26
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x6
	.uleb128 0xf
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x7
	.uleb128 0xf
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x8
	.uleb128 0x13
	.byte	0x1
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x9
	.uleb128 0xd
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x38
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0xa
	.uleb128 0x16
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0xb
	.uleb128 0x13
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3c
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0xc
	.uleb128 0x1
	.byte	0x1
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0xd
	.uleb128 0x21
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2f
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0xe
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3c
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0xf
	.uleb128 0x21
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x10
	.uleb128 0x26
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x11
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3c
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x12
	.uleb128 0x37
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x13
	.uleb128 0x4
	.byte	0x1
	.uleb128 0x3e
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x14
	.uleb128 0x28
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x1c
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x15
	.uleb128 0xd
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x38
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x16
	.uleb128 0x13
	.byte	0x1
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x17
	.uleb128 0x15
	.byte	0x1
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x18
	.uleb128 0x5
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x19
	.uleb128 0x15
	.byte	0x1
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x1a
	.uleb128 0xd
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x38
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x1b
	.uleb128 0x16
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x1c
	.uleb128 0x17
	.byte	0x1
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x1d
	.uleb128 0xd
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x1e
	.uleb128 0xd
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x1f
	.uleb128 0x15
	.byte	0
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x20
	.uleb128 0x13
	.byte	0x1
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x21
	.uleb128 0x13
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3c
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x22
	.uleb128 0x35
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x23
	.uleb128 0x15
	.byte	0
	.uleb128 0x27
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x24
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x25
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x6
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x2117
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x26
	.uleb128 0x4109
	.byte	0
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x31
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x27
	.uleb128 0x5
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x17
	.uleb128 0x2137
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x28
	.uleb128 0x5
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x17
	.uleb128 0x2137
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x29
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x2a
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x17
	.uleb128 0x2137
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x2b
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x2c
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x17
	.uleb128 0x2137
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x2d
	.uleb128 0x1d
	.byte	0x1
	.uleb128 0x31
	.uleb128 0x13
	.uleb128 0x52
	.uleb128 0x1
	.uleb128 0x2138
	.uleb128 0xb
	.uleb128 0x55
	.uleb128 0x17
	.uleb128 0x58
	.uleb128 0xb
	.uleb128 0x59
	.uleb128 0x5
	.uleb128 0x57
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x2e
	.uleb128 0x5
	.byte	0
	.uleb128 0x31
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x17
	.uleb128 0x2137
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x2f
	.uleb128 0x4109
	.byte	0x1
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x31
	.uleb128 0x13
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x30
	.uleb128 0x410a
	.byte	0
	.uleb128 0x2
	.uleb128 0x18
	.uleb128 0x2111
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x31
	.uleb128 0x4109
	.byte	0x1
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x31
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x32
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x6
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x2117
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x33
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x34
	.uleb128 0x5
	.byte	0
	.uleb128 0x31
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x35
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x20
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x36
	.uleb128 0x5
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x37
	.uleb128 0x1d
	.byte	0x1
	.uleb128 0x31
	.uleb128 0x13
	.uleb128 0x52
	.uleb128 0x1
	.uleb128 0x2138
	.uleb128 0xb
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x6
	.uleb128 0x58
	.uleb128 0xb
	.uleb128 0x59
	.uleb128 0x5
	.uleb128 0x57
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x38
	.uleb128 0xb
	.byte	0x1
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x6
	.byte	0
	.byte	0
	.uleb128 0x39
	.uleb128 0x34
	.byte	0
	.uleb128 0x31
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x17
	.uleb128 0x2137
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x3a
	.uleb128 0x34
	.byte	0
	.uleb128 0x31
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x3b
	.uleb128 0x4109
	.byte	0x1
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x2115
	.uleb128 0x19
	.uleb128 0x31
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x3c
	.uleb128 0xb
	.byte	0x1
	.uleb128 0x55
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x3d
	.uleb128 0x4109
	.byte	0
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x2115
	.uleb128 0x19
	.uleb128 0x31
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x3e
	.uleb128 0x5
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x3f
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x34
	.uleb128 0x19
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x40
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x6
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x2117
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x41
	.uleb128 0x5
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x17
	.uleb128 0x2137
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x42
	.uleb128 0x5
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x43
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x17
	.uleb128 0x2137
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x44
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x17
	.uleb128 0x2137
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x45
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x6
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x2116
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x46
	.uleb128 0x1d
	.byte	0x1
	.uleb128 0x31
	.uleb128 0x13
	.uleb128 0x52
	.uleb128 0x1
	.uleb128 0x2138
	.uleb128 0xb
	.uleb128 0x55
	.uleb128 0x17
	.uleb128 0x58
	.uleb128 0xb
	.uleb128 0x59
	.uleb128 0xb
	.uleb128 0x57
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x47
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x20
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x48
	.uleb128 0x5
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x49
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x4a
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x4b
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x20
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x4c
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x20
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x4d
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x31
	.uleb128 0x13
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x6
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x2117
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x4e
	.uleb128 0x34
	.byte	0
	.uleb128 0x31
	.uleb128 0x13
	.uleb128 0x1c
	.uleb128 0xa
	.byte	0
	.byte	0
	.uleb128 0x4f
	.uleb128 0x5
	.byte	0
	.uleb128 0x31
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x50
	.uleb128 0x2e
	.byte	0
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3c
	.uleb128 0x19
	.uleb128 0x6e
	.uleb128 0xe
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x51
	.uleb128 0x2e
	.byte	0
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3c
	.uleb128 0x19
	.uleb128 0x6e
	.uleb128 0xe
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x39
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x52
	.uleb128 0x2e
	.byte	0
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3c
	.uleb128 0x19
	.uleb128 0x6e
	.uleb128 0x8
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x53
	.uleb128 0x2e
	.byte	0
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3c
	.uleb128 0x19
	.uleb128 0x6e
	.uleb128 0xe
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.byte	0
	.byte	0
	.byte	0
	.section	.debug_loc,"",%progbits
.Ldebug_loc0:
.LVUS114:
	.uleb128 0
	.uleb128 .LVU831
	.uleb128 .LVU831
	.uleb128 0
.LLST114:
	.4byte	.LVL175-.Ltext0
	.4byte	.LVL178-1-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x40
	.4byte	.LVL178-1-.Ltext0
	.4byte	.LFE75-.Ltext0
	.2byte	0x6
	.byte	0xf3
	.uleb128 0x3
	.byte	0xf5
	.uleb128 0x40
	.uleb128 0x25
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS115:
	.uleb128 0
	.uleb128 .LVU831
	.uleb128 .LVU831
	.uleb128 0
.LLST115:
	.4byte	.LVL175-.Ltext0
	.4byte	.LVL178-1-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x41
	.4byte	.LVL178-1-.Ltext0
	.4byte	.LFE75-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x57
	.4byte	0
	.4byte	0
.LVUS116:
	.uleb128 0
	.uleb128 .LVU812
	.uleb128 .LVU812
	.uleb128 0
.LLST116:
	.4byte	.LVL175-.Ltext0
	.4byte	.LVL176-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL176-.Ltext0
	.4byte	.LFE75-.Ltext0
	.2byte	0x1
	.byte	0x59
	.4byte	0
	.4byte	0
.LVUS117:
	.uleb128 .LVU868
	.uleb128 .LVU874
	.uleb128 .LVU874
	.uleb128 .LVU880
	.uleb128 .LVU880
	.uleb128 .LVU909
.LLST117:
	.4byte	.LVL187-.Ltext0
	.4byte	.LVL190-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x4f
	.4byte	.LVL190-.Ltext0
	.4byte	.LVL192-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x5b
	.4byte	.LVL192-.Ltext0
	.4byte	.LVL197-.Ltext0
	.2byte	0xc
	.byte	0xf5
	.uleb128 0x5b
	.uleb128 0x25
	.byte	0xf4
	.uleb128 0x25
	.byte	0x4
	.4byte	0x3f800000
	.byte	0x22
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS118:
	.uleb128 .LVU882
	.uleb128 .LVU890
.LLST118:
	.4byte	.LVL193-.Ltext0
	.4byte	.LVL194-.Ltext0
	.2byte	0x13
	.byte	0x70
	.sleb128 0
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf7
	.uleb128 0x25
	.byte	0xf4
	.uleb128 0x25
	.byte	0x4
	.4byte	0x4ac7ff38
	.byte	0x1b
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS119:
	.uleb128 .LVU822
	.uleb128 .LVU918
	.uleb128 .LVU919
	.uleb128 0
.LLST119:
	.4byte	.LVL177-.Ltext0
	.4byte	.LVL203-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x5a
	.4byte	.LVL204-.Ltext0
	.4byte	.LFE75-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x5a
	.4byte	0
	.4byte	0
.LVUS120:
	.uleb128 .LVU840
	.uleb128 .LVU847
	.uleb128 .LVU847
	.uleb128 .LVU914
	.uleb128 .LVU914
	.uleb128 .LVU916
	.uleb128 .LVU916
	.uleb128 .LVU917
.LLST120:
	.4byte	.LVL180-.Ltext0
	.4byte	.LVL181-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL181-.Ltext0
	.4byte	.LVL200-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	.LVL200-.Ltext0
	.4byte	.LVL201-.Ltext0
	.2byte	0x3
	.byte	0x78
	.sleb128 -1
	.byte	0x9f
	.4byte	.LVL201-.Ltext0
	.4byte	.LVL202-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	0
	.4byte	0
.LVUS121:
	.uleb128 .LVU823
	.uleb128 .LVU847
	.uleb128 .LVU847
	.uleb128 .LVU917
	.uleb128 .LVU919
	.uleb128 0
.LLST121:
	.4byte	.LVL177-.Ltext0
	.4byte	.LVL181-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL181-.Ltext0
	.4byte	.LVL202-.Ltext0
	.2byte	0x1
	.byte	0x57
	.4byte	.LVL204-.Ltext0
	.4byte	.LFE75-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS122:
	.uleb128 .LVU849
	.uleb128 .LVU855
	.uleb128 .LVU855
	.uleb128 .LVU911
.LLST122:
	.4byte	.LVL182-.Ltext0
	.4byte	.LVL183-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL183-.Ltext0
	.4byte	.LVL199-.Ltext0
	.2byte	0x1
	.byte	0x56
	.4byte	0
	.4byte	0
.LVUS123:
	.uleb128 .LVU860
	.uleb128 .LVU863
	.uleb128 .LVU863
	.uleb128 .LVU909
.LLST123:
	.4byte	.LVL184-.Ltext0
	.4byte	.LVL185-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL185-.Ltext0
	.4byte	.LVL197-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	0
	.4byte	0
.LVUS124:
	.uleb128 .LVU827
	.uleb128 0
.LLST124:
	.4byte	.LVL177-.Ltext0
	.4byte	.LFE75-.Ltext0
	.2byte	0x1
	.byte	0x59
	.4byte	0
	.4byte	0
.LVUS112:
	.uleb128 0
	.uleb128 .LVU779
	.uleb128 .LVU779
	.uleb128 0
.LLST112:
	.4byte	.LVL167-.Ltext0
	.4byte	.LVL168-1-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL168-1-.Ltext0
	.4byte	.LFE74-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS113:
	.uleb128 .LVU779
	.uleb128 .LVU782
	.uleb128 .LVU792
	.uleb128 .LVU793
.LLST113:
	.4byte	.LVL168-.Ltext0
	.4byte	.LVL169-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL172-.Ltext0
	.4byte	.LVL173-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	0
	.4byte	0
.LVUS89:
	.uleb128 0
	.uleb128 .LVU687
	.uleb128 .LVU687
	.uleb128 .LVU750
	.uleb128 .LVU750
	.uleb128 .LVU751
	.uleb128 .LVU751
	.uleb128 0
.LLST89:
	.4byte	.LVL144-.Ltext0
	.4byte	.LVL146-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL146-.Ltext0
	.4byte	.LVL158-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	.LVL158-.Ltext0
	.4byte	.LVL159-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL159-.Ltext0
	.4byte	.LFE72-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS90:
	.uleb128 .LVU675
	.uleb128 0
.LLST90:
	.4byte	.LVL145-.Ltext0
	.4byte	.LFE72-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	0
	.4byte	0
.LVUS91:
	.uleb128 .LVU676
	.uleb128 0
.LLST91:
	.4byte	.LVL145-.Ltext0
	.4byte	.LFE72-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	0
	.4byte	0
.LVUS92:
	.uleb128 .LVU716
	.uleb128 .LVU744
	.uleb128 .LVU744
	.uleb128 .LVU747
.LLST92:
	.4byte	.LVL153-.Ltext0
	.4byte	.LVL155-.Ltext0
	.2byte	0x5
	.byte	0x70
	.sleb128 0
	.byte	0x39
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL155-.Ltext0
	.4byte	.LVL156-.Ltext0
	.2byte	0x1
	.byte	0x59
	.4byte	0
	.4byte	0
.LVUS93:
	.uleb128 .LVU714
	.uleb128 .LVU747
.LLST93:
	.4byte	.LVL153-.Ltext0
	.4byte	.LVL156-.Ltext0
	.2byte	0x10
	.byte	0x70
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf7
	.uleb128 0x25
	.byte	0xf5
	.uleb128 0x4e
	.uleb128 0x25
	.byte	0x1e
	.byte	0xf7
	.uleb128 0x33
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS94:
	.uleb128 .LVU715
	.uleb128 .LVU747
.LLST94:
	.4byte	.LVL153-.Ltext0
	.4byte	.LVL156-.Ltext0
	.2byte	0x17
	.byte	0x70
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x23
	.uleb128 0x1
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf7
	.uleb128 0x25
	.byte	0x3
	.4byte	g_armwave_state+40
	.byte	0xf6
	.byte	0x4
	.uleb128 0x25
	.byte	0x1e
	.byte	0xf7
	.uleb128 0x33
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS95:
	.uleb128 .LVU717
	.uleb128 .LVU747
.LLST95:
	.4byte	.LVL153-.Ltext0
	.4byte	.LVL156-.Ltext0
	.2byte	0x10
	.byte	0x70
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf7
	.uleb128 0x25
	.byte	0xf5
	.uleb128 0x4e
	.uleb128 0x25
	.byte	0x1e
	.byte	0xf7
	.uleb128 0x33
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS96:
	.uleb128 .LVU712
	.uleb128 .LVU728
	.uleb128 .LVU728
	.uleb128 .LVU747
.LLST96:
	.4byte	.LVL153-.Ltext0
	.4byte	.LVL154-.Ltext0
	.2byte	0x63
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7e
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x40
	.byte	0x24
	.byte	0x8
	.byte	0xff
	.byte	0x40
	.byte	0x24
	.byte	0x1a
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x76
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x38
	.byte	0x24
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x21
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x77
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x21
	.byte	0x40
	.byte	0x44
	.byte	0x24
	.byte	0x1f
	.byte	0x21
	.byte	0x9f
	.4byte	.LVL154-.Ltext0
	.4byte	.LVL156-.Ltext0
	.2byte	0x75
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x7e
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x40
	.byte	0x24
	.byte	0x8
	.byte	0xff
	.byte	0x40
	.byte	0x24
	.byte	0x1a
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x76
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x38
	.byte	0x24
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x21
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x77
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x21
	.byte	0x40
	.byte	0x44
	.byte	0x24
	.byte	0x1f
	.byte	0x21
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS97:
	.uleb128 .LVU700
	.uleb128 .LVU702
	.uleb128 .LVU702
	.uleb128 .LVU747
.LLST97:
	.4byte	.LVL150-.Ltext0
	.4byte	.LVL151-.Ltext0
	.2byte	0x9
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x9f
	.4byte	.LVL151-.Ltext0
	.4byte	.LVL156-.Ltext0
	.2byte	0x9
	.byte	0x71
	.sleb128 -2
	.byte	0x94
	.byte	0x2
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS98:
	.uleb128 .LVU706
	.uleb128 .LVU728
	.uleb128 .LVU728
	.uleb128 .LVU747
.LLST98:
	.4byte	.LVL153-.Ltext0
	.4byte	.LVL154-.Ltext0
	.2byte	0xf
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x77
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL154-.Ltext0
	.4byte	.LVL156-.Ltext0
	.2byte	0x15
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x77
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS99:
	.uleb128 .LVU707
	.uleb128 .LVU728
	.uleb128 .LVU728
	.uleb128 .LVU747
.LLST99:
	.4byte	.LVL153-.Ltext0
	.4byte	.LVL154-.Ltext0
	.2byte	0xf
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x76
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL154-.Ltext0
	.4byte	.LVL156-.Ltext0
	.2byte	0x15
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x76
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS100:
	.uleb128 .LVU708
	.uleb128 .LVU728
	.uleb128 .LVU728
	.uleb128 .LVU747
.LLST100:
	.4byte	.LVL153-.Ltext0
	.4byte	.LVL154-.Ltext0
	.2byte	0xf
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7e
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL154-.Ltext0
	.4byte	.LVL156-.Ltext0
	.2byte	0x15
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x7e
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS101:
	.uleb128 .LVU692
	.uleb128 .LVU699
	.uleb128 .LVU699
	.uleb128 .LVU750
.LLST101:
	.4byte	.LVL148-.Ltext0
	.4byte	.LVL150-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL150-.Ltext0
	.4byte	.LVL158-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	0
	.4byte	0
.LVUS102:
	.uleb128 .LVU713
	.uleb128 .LVU747
.LLST102:
	.4byte	.LVL153-.Ltext0
	.4byte	.LVL156-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	0
	.4byte	0
.LVUS103:
	.uleb128 .LVU691
	.uleb128 .LVU698
	.uleb128 .LVU698
	.uleb128 .LVU699
.LLST103:
	.4byte	.LVL148-.Ltext0
	.4byte	.LVL149-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	.LVL149-.Ltext0
	.4byte	.LVL150-.Ltext0
	.2byte	0xa
	.byte	0x3
	.4byte	g_armwave_state+64
	.byte	0x6
	.byte	0x73
	.sleb128 0
	.byte	0x1e
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS104:
	.uleb128 .LVU709
	.uleb128 .LVU728
	.uleb128 .LVU728
	.uleb128 .LVU747
.LLST104:
	.4byte	.LVL153-.Ltext0
	.4byte	.LVL154-.Ltext0
	.2byte	0x1a
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x77
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x9f
	.4byte	.LVL154-.Ltext0
	.4byte	.LVL156-.Ltext0
	.2byte	0x20
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x77
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS105:
	.uleb128 .LVU710
	.uleb128 .LVU728
	.uleb128 .LVU728
	.uleb128 .LVU747
.LLST105:
	.4byte	.LVL153-.Ltext0
	.4byte	.LVL154-.Ltext0
	.2byte	0x1a
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x76
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x9f
	.4byte	.LVL154-.Ltext0
	.4byte	.LVL156-.Ltext0
	.2byte	0x20
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x76
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS106:
	.uleb128 .LVU711
	.uleb128 .LVU728
	.uleb128 .LVU728
	.uleb128 .LVU747
.LLST106:
	.4byte	.LVL153-.Ltext0
	.4byte	.LVL154-.Ltext0
	.2byte	0x1a
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7e
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x9f
	.4byte	.LVL154-.Ltext0
	.4byte	.LVL156-.Ltext0
	.2byte	0x20
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x7e
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS107:
	.uleb128 .LVU702
	.uleb128 .LVU704
	.uleb128 .LVU704
	.uleb128 .LVU750
.LLST107:
	.4byte	.LVL151-.Ltext0
	.4byte	.LVL152-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	.LVL152-.Ltext0
	.4byte	.LVL158-.Ltext0
	.2byte	0x2
	.byte	0x71
	.sleb128 0
	.4byte	0
	.4byte	0
.LVUS108:
	.uleb128 .LVU681
	.uleb128 .LVU689
	.uleb128 .LVU750
	.uleb128 .LVU752
.LLST108:
	.4byte	.LVL145-.Ltext0
	.4byte	.LVL147-1-.Ltext0
	.2byte	0x5
	.byte	0x3
	.4byte	g_armwave_state+4
	.4byte	.LVL158-.Ltext0
	.4byte	.LVL160-1-.Ltext0
	.2byte	0x5
	.byte	0x3
	.4byte	g_armwave_state+4
	.4byte	0
	.4byte	0
.LVUS109:
	.uleb128 .LVU682
	.uleb128 0
.LLST109:
	.4byte	.LVL145-.Ltext0
	.4byte	.LFE72-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	0
	.4byte	0
.LVUS110:
	.uleb128 .LVU718
	.uleb128 .LVU747
.LLST110:
	.4byte	.LVL153-.Ltext0
	.4byte	.LVL156-.Ltext0
	.2byte	0x1c
	.byte	0x70
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf7
	.uleb128 0x25
	.byte	0xf5
	.uleb128 0x4e
	.uleb128 0x25
	.byte	0x1e
	.byte	0xf7
	.uleb128 0x33
	.byte	0x3
	.4byte	g_armwave_state+88
	.byte	0x6
	.byte	0x1e
	.byte	0x70
	.sleb128 0
	.byte	0x39
	.byte	0x26
	.byte	0x22
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS88:
	.uleb128 0
	.uleb128 .LVU668
	.uleb128 .LVU668
	.uleb128 .LVU669
	.uleb128 .LVU669
	.uleb128 0
.LLST88:
	.4byte	.LVL141-.Ltext0
	.4byte	.LVL142-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL142-.Ltext0
	.4byte	.LVL143-1-.Ltext0
	.2byte	0x1
	.byte	0x51
	.4byte	.LVL143-1-.Ltext0
	.4byte	.LFE71-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS68:
	.uleb128 .LVU590
	.uleb128 0
.LLST68:
	.4byte	.LVL127-.Ltext0
	.4byte	.LFE70-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	0
	.4byte	0
.LVUS69:
	.uleb128 .LVU630
	.uleb128 .LVU658
	.uleb128 .LVU658
	.uleb128 .LVU661
.LLST69:
	.4byte	.LVL134-.Ltext0
	.4byte	.LVL136-.Ltext0
	.2byte	0x5
	.byte	0x70
	.sleb128 0
	.byte	0x39
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL136-.Ltext0
	.4byte	.LVL137-.Ltext0
	.2byte	0x1
	.byte	0x59
	.4byte	0
	.4byte	0
.LVUS70:
	.uleb128 .LVU628
	.uleb128 .LVU661
.LLST70:
	.4byte	.LVL134-.Ltext0
	.4byte	.LVL137-.Ltext0
	.2byte	0x10
	.byte	0x70
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf7
	.uleb128 0x25
	.byte	0xf5
	.uleb128 0x4e
	.uleb128 0x25
	.byte	0x1e
	.byte	0xf7
	.uleb128 0x33
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS71:
	.uleb128 .LVU629
	.uleb128 .LVU661
.LLST71:
	.4byte	.LVL134-.Ltext0
	.4byte	.LVL137-.Ltext0
	.2byte	0x17
	.byte	0x70
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x23
	.uleb128 0x1
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf7
	.uleb128 0x25
	.byte	0x3
	.4byte	g_armwave_state+40
	.byte	0xf6
	.byte	0x4
	.uleb128 0x25
	.byte	0x1e
	.byte	0xf7
	.uleb128 0x33
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS72:
	.uleb128 .LVU631
	.uleb128 .LVU661
.LLST72:
	.4byte	.LVL134-.Ltext0
	.4byte	.LVL137-.Ltext0
	.2byte	0x10
	.byte	0x70
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf7
	.uleb128 0x25
	.byte	0xf5
	.uleb128 0x4e
	.uleb128 0x25
	.byte	0x1e
	.byte	0xf7
	.uleb128 0x33
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS73:
	.uleb128 .LVU626
	.uleb128 .LVU642
	.uleb128 .LVU642
	.uleb128 .LVU661
.LLST73:
	.4byte	.LVL134-.Ltext0
	.4byte	.LVL135-.Ltext0
	.2byte	0x63
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7e
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x40
	.byte	0x24
	.byte	0x8
	.byte	0xff
	.byte	0x40
	.byte	0x24
	.byte	0x1a
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x76
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x38
	.byte	0x24
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x21
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x77
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x21
	.byte	0x40
	.byte	0x44
	.byte	0x24
	.byte	0x1f
	.byte	0x21
	.byte	0x9f
	.4byte	.LVL135-.Ltext0
	.4byte	.LVL137-.Ltext0
	.2byte	0x75
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x7e
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x40
	.byte	0x24
	.byte	0x8
	.byte	0xff
	.byte	0x40
	.byte	0x24
	.byte	0x1a
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x76
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x38
	.byte	0x24
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x21
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x77
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x21
	.byte	0x40
	.byte	0x44
	.byte	0x24
	.byte	0x1f
	.byte	0x21
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS74:
	.uleb128 .LVU614
	.uleb128 .LVU616
	.uleb128 .LVU616
	.uleb128 .LVU661
.LLST74:
	.4byte	.LVL131-.Ltext0
	.4byte	.LVL132-.Ltext0
	.2byte	0x9
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x9f
	.4byte	.LVL132-.Ltext0
	.4byte	.LVL137-.Ltext0
	.2byte	0x9
	.byte	0x71
	.sleb128 -2
	.byte	0x94
	.byte	0x2
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS75:
	.uleb128 .LVU620
	.uleb128 .LVU642
	.uleb128 .LVU642
	.uleb128 .LVU661
.LLST75:
	.4byte	.LVL134-.Ltext0
	.4byte	.LVL135-.Ltext0
	.2byte	0xf
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x77
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL135-.Ltext0
	.4byte	.LVL137-.Ltext0
	.2byte	0x15
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x77
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS76:
	.uleb128 .LVU621
	.uleb128 .LVU642
	.uleb128 .LVU642
	.uleb128 .LVU661
.LLST76:
	.4byte	.LVL134-.Ltext0
	.4byte	.LVL135-.Ltext0
	.2byte	0xf
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x76
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL135-.Ltext0
	.4byte	.LVL137-.Ltext0
	.2byte	0x15
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x76
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS77:
	.uleb128 .LVU622
	.uleb128 .LVU642
	.uleb128 .LVU642
	.uleb128 .LVU661
.LLST77:
	.4byte	.LVL134-.Ltext0
	.4byte	.LVL135-.Ltext0
	.2byte	0xf
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7e
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL135-.Ltext0
	.4byte	.LVL137-.Ltext0
	.2byte	0x15
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x7e
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS78:
	.uleb128 .LVU606
	.uleb128 .LVU613
	.uleb128 .LVU613
	.uleb128 .LVU664
.LLST78:
	.4byte	.LVL129-.Ltext0
	.4byte	.LVL131-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL131-.Ltext0
	.4byte	.LVL139-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	0
	.4byte	0
.LVUS79:
	.uleb128 .LVU627
	.uleb128 .LVU661
.LLST79:
	.4byte	.LVL134-.Ltext0
	.4byte	.LVL137-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	0
	.4byte	0
.LVUS80:
	.uleb128 .LVU605
	.uleb128 .LVU612
	.uleb128 .LVU612
	.uleb128 .LVU613
.LLST80:
	.4byte	.LVL129-.Ltext0
	.4byte	.LVL130-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	.LVL130-.Ltext0
	.4byte	.LVL131-.Ltext0
	.2byte	0xa
	.byte	0x3
	.4byte	g_armwave_state+64
	.byte	0x6
	.byte	0x73
	.sleb128 0
	.byte	0x1e
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS81:
	.uleb128 .LVU623
	.uleb128 .LVU642
	.uleb128 .LVU642
	.uleb128 .LVU661
.LLST81:
	.4byte	.LVL134-.Ltext0
	.4byte	.LVL135-.Ltext0
	.2byte	0x1a
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x77
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x9f
	.4byte	.LVL135-.Ltext0
	.4byte	.LVL137-.Ltext0
	.2byte	0x20
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x77
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS82:
	.uleb128 .LVU624
	.uleb128 .LVU642
	.uleb128 .LVU642
	.uleb128 .LVU661
.LLST82:
	.4byte	.LVL134-.Ltext0
	.4byte	.LVL135-.Ltext0
	.2byte	0x1a
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x76
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x9f
	.4byte	.LVL135-.Ltext0
	.4byte	.LVL137-.Ltext0
	.2byte	0x20
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x76
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS83:
	.uleb128 .LVU625
	.uleb128 .LVU642
	.uleb128 .LVU642
	.uleb128 .LVU661
.LLST83:
	.4byte	.LVL134-.Ltext0
	.4byte	.LVL135-.Ltext0
	.2byte	0x1a
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7e
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x9f
	.4byte	.LVL135-.Ltext0
	.4byte	.LVL137-.Ltext0
	.2byte	0x20
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x7e
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS84:
	.uleb128 .LVU616
	.uleb128 .LVU618
	.uleb128 .LVU618
	.uleb128 .LVU664
.LLST84:
	.4byte	.LVL132-.Ltext0
	.4byte	.LVL133-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	.LVL133-.Ltext0
	.4byte	.LVL139-.Ltext0
	.2byte	0x2
	.byte	0x71
	.sleb128 0
	.4byte	0
	.4byte	0
.LVUS85:
	.uleb128 .LVU595
	.uleb128 .LVU603
	.uleb128 .LVU664
	.uleb128 .LVU665
.LLST85:
	.4byte	.LVL127-.Ltext0
	.4byte	.LVL128-1-.Ltext0
	.2byte	0x5
	.byte	0x3
	.4byte	g_armwave_state+4
	.4byte	.LVL139-.Ltext0
	.4byte	.LVL140-1-.Ltext0
	.2byte	0x5
	.byte	0x3
	.4byte	g_armwave_state+4
	.4byte	0
	.4byte	0
.LVUS86:
	.uleb128 .LVU596
	.uleb128 0
.LLST86:
	.4byte	.LVL127-.Ltext0
	.4byte	.LFE70-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	0
	.4byte	0
.LVUS87:
	.uleb128 .LVU632
	.uleb128 .LVU661
.LLST87:
	.4byte	.LVL134-.Ltext0
	.4byte	.LVL137-.Ltext0
	.2byte	0x1c
	.byte	0x70
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf7
	.uleb128 0x25
	.byte	0xf5
	.uleb128 0x4e
	.uleb128 0x25
	.byte	0x1e
	.byte	0xf7
	.uleb128 0x33
	.byte	0x3
	.4byte	g_armwave_state+88
	.byte	0x6
	.byte	0x1e
	.byte	0x70
	.sleb128 0
	.byte	0x39
	.byte	0x26
	.byte	0x22
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS59:
	.uleb128 0
	.uleb128 .LVU558
	.uleb128 .LVU558
	.uleb128 .LVU585
	.uleb128 .LVU585
	.uleb128 0
.LLST59:
	.4byte	.LVL118-.Ltext0
	.4byte	.LVL119-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL119-.Ltext0
	.4byte	.LVL125-.Ltext0
	.2byte	0x1
	.byte	0x56
	.4byte	.LVL125-.Ltext0
	.4byte	.LFE69-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS60:
	.uleb128 0
	.uleb128 .LVU558
	.uleb128 .LVU558
	.uleb128 .LVU585
	.uleb128 .LVU585
	.uleb128 0
.LLST60:
	.4byte	.LVL118-.Ltext0
	.4byte	.LVL119-.Ltext0
	.2byte	0x1
	.byte	0x51
	.4byte	.LVL119-.Ltext0
	.4byte	.LVL125-.Ltext0
	.2byte	0x1
	.byte	0x57
	.4byte	.LVL125-.Ltext0
	.4byte	.LFE69-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x51
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS61:
	.uleb128 0
	.uleb128 .LVU558
	.uleb128 .LVU558
	.uleb128 .LVU585
	.uleb128 .LVU585
	.uleb128 0
.LLST61:
	.4byte	.LVL118-.Ltext0
	.4byte	.LVL119-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	.LVL119-.Ltext0
	.4byte	.LVL125-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	.LVL125-.Ltext0
	.4byte	.LFE69-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x52
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS62:
	.uleb128 0
	.uleb128 .LVU558
	.uleb128 .LVU558
	.uleb128 .LVU585
	.uleb128 .LVU585
	.uleb128 0
.LLST62:
	.4byte	.LVL118-.Ltext0
	.4byte	.LVL119-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	.LVL119-.Ltext0
	.4byte	.LVL125-.Ltext0
	.2byte	0x1
	.byte	0x59
	.4byte	.LVL125-.Ltext0
	.4byte	.LFE69-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x53
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS63:
	.uleb128 .LVU551
	.uleb128 .LVU558
	.uleb128 .LVU558
	.uleb128 .LVU566
.LLST63:
	.4byte	.LVL118-.Ltext0
	.4byte	.LVL119-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL119-.Ltext0
	.4byte	.LVL122-.Ltext0
	.2byte	0x9
	.byte	0x74
	.sleb128 0
	.byte	0x3
	.4byte	gamma_table-1
	.byte	0x1c
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS64:
	.uleb128 .LVU568
	.uleb128 .LVU581
.LLST64:
	.4byte	.LVL122-.Ltext0
	.4byte	.LVL123-.Ltext0
	.2byte	0x3
	.byte	0x8
	.byte	0xfa
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS65:
	.uleb128 .LVU568
	.uleb128 .LVU581
.LLST65:
	.4byte	.LVL122-.Ltext0
	.4byte	.LVL123-.Ltext0
	.2byte	0x4
	.byte	0xa
	.2byte	0x6f4
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS66:
	.uleb128 .LVU568
	.uleb128 .LVU581
.LLST66:
	.4byte	.LVL122-.Ltext0
	.4byte	.LVL123-.Ltext0
	.2byte	0x4
	.byte	0xa
	.2byte	0x9f6
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS67:
	.uleb128 .LVU568
	.uleb128 .LVU581
.LLST67:
	.4byte	.LVL122-.Ltext0
	.4byte	.LVL123-.Ltext0
	.2byte	0x2
	.byte	0x31
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS53:
	.uleb128 0
	.uleb128 .LVU512
	.uleb128 .LVU512
	.uleb128 .LVU546
	.uleb128 .LVU546
	.uleb128 0
.LLST53:
	.4byte	.LVL98-.Ltext0
	.4byte	.LVL100-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL100-.Ltext0
	.4byte	.LVL116-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	.LVL116-.Ltext0
	.4byte	.LFE68-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS54:
	.uleb128 0
	.uleb128 .LVU511
	.uleb128 .LVU511
	.uleb128 .LVU513
	.uleb128 .LVU513
	.uleb128 0
.LLST54:
	.4byte	.LVL98-.Ltext0
	.4byte	.LVL99-.Ltext0
	.2byte	0x1
	.byte	0x51
	.4byte	.LVL99-.Ltext0
	.4byte	.LVL101-1-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	.LVL101-1-.Ltext0
	.4byte	.LFE68-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x51
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS55:
	.uleb128 .LVU516
	.uleb128 .LVU519
	.uleb128 .LVU519
	.uleb128 .LVU520
	.uleb128 .LVU520
	.uleb128 .LVU546
	.uleb128 .LVU546
	.uleb128 .LVU547
.LLST55:
	.4byte	.LVL102-.Ltext0
	.4byte	.LVL103-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL103-.Ltext0
	.4byte	.LVL104-1-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	.LVL104-1-.Ltext0
	.4byte	.LVL116-.Ltext0
	.2byte	0x1
	.byte	0x57
	.4byte	.LVL116-.Ltext0
	.4byte	.LVL117-1-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	0
	.4byte	0
.LVUS56:
	.uleb128 .LVU534
	.uleb128 .LVU539
.LLST56:
	.4byte	.LVL109-.Ltext0
	.4byte	.LVL111-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	0
	.4byte	0
.LVUS57:
	.uleb128 .LVU527
	.uleb128 .LVU530
	.uleb128 .LVU530
	.uleb128 .LVU536
	.uleb128 .LVU536
	.uleb128 .LVU540
	.uleb128 .LVU540
	.uleb128 .LVU542
.LLST57:
	.4byte	.LVL107-.Ltext0
	.4byte	.LVL108-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL108-.Ltext0
	.4byte	.LVL110-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	.LVL110-.Ltext0
	.4byte	.LVL112-.Ltext0
	.2byte	0x3
	.byte	0x74
	.sleb128 -1
	.byte	0x9f
	.4byte	.LVL112-.Ltext0
	.4byte	.LVL113-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	0
	.4byte	0
.LVUS58:
	.uleb128 .LVU523
	.uleb128 .LVU527
	.uleb128 .LVU527
	.uleb128 .LVU544
.LLST58:
	.4byte	.LVL106-.Ltext0
	.4byte	.LVL107-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL107-.Ltext0
	.4byte	.LVL115-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	0
	.4byte	0
.LVUS51:
	.uleb128 0
	.uleb128 .LVU495
	.uleb128 .LVU495
	.uleb128 0
.LLST51:
	.4byte	.LVL93-.Ltext0
	.4byte	.LVL94-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL94-.Ltext0
	.4byte	.LFE66-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS50:
	.uleb128 0
	.uleb128 .LVU490
	.uleb128 .LVU490
	.uleb128 0
.LLST50:
	.4byte	.LVL90-.Ltext0
	.4byte	.LVL91-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL91-.Ltext0
	.4byte	.LFE65-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS49:
	.uleb128 0
	.uleb128 .LVU478
	.uleb128 .LVU478
	.uleb128 .LVU480
	.uleb128 .LVU480
	.uleb128 .LVU481
	.uleb128 .LVU481
	.uleb128 0
.LLST49:
	.4byte	.LVL85-.Ltext0
	.4byte	.LVL86-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL86-.Ltext0
	.4byte	.LVL87-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	.LVL87-.Ltext0
	.4byte	.LVL88-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL88-.Ltext0
	.4byte	.LFE64-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS48:
	.uleb128 0
	.uleb128 .LVU470
	.uleb128 .LVU470
	.uleb128 0
.LLST48:
	.4byte	.LVL82-.Ltext0
	.4byte	.LVL83-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL83-.Ltext0
	.4byte	.LFE63-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS41:
	.uleb128 0
	.uleb128 .LVU351
	.uleb128 .LVU351
	.uleb128 .LVU352
	.uleb128 .LVU352
	.uleb128 .LVU360
	.uleb128 .LVU360
	.uleb128 .LVU459
	.uleb128 .LVU459
	.uleb128 .LVU460
	.uleb128 .LVU460
	.uleb128 0
.LLST41:
	.4byte	.LVL58-.Ltext0
	.4byte	.LVL62-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL62-.Ltext0
	.4byte	.LVL63-1-.Ltext0
	.2byte	0x1
	.byte	0x51
	.4byte	.LVL63-1-.Ltext0
	.4byte	.LVL65-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	.LVL65-.Ltext0
	.4byte	.LVL78-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	.LVL78-.Ltext0
	.4byte	.LVL79-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	.LVL79-.Ltext0
	.4byte	.LFE62-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS42:
	.uleb128 0
	.uleb128 .LVU350
	.uleb128 .LVU350
	.uleb128 .LVU352
	.uleb128 .LVU352
	.uleb128 .LVU355
	.uleb128 .LVU355
	.uleb128 .LVU459
	.uleb128 .LVU459
	.uleb128 .LVU460
	.uleb128 .LVU460
	.uleb128 0
.LLST42:
	.4byte	.LVL58-.Ltext0
	.4byte	.LVL61-.Ltext0
	.2byte	0x1
	.byte	0x51
	.4byte	.LVL61-.Ltext0
	.4byte	.LVL63-1-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	.LVL63-1-.Ltext0
	.4byte	.LVL64-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	.LVL64-.Ltext0
	.4byte	.LVL78-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x51
	.byte	0x9f
	.4byte	.LVL78-.Ltext0
	.4byte	.LVL79-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	.LVL79-.Ltext0
	.4byte	.LFE62-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x51
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS43:
	.uleb128 0
	.uleb128 .LVU349
	.uleb128 .LVU349
	.uleb128 .LVU352
	.uleb128 .LVU352
	.uleb128 .LVU458
	.uleb128 .LVU458
	.uleb128 .LVU459
	.uleb128 .LVU459
	.uleb128 0
.LLST43:
	.4byte	.LVL58-.Ltext0
	.4byte	.LVL60-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	.LVL60-.Ltext0
	.4byte	.LVL63-1-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	.LVL63-1-.Ltext0
	.4byte	.LVL77-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	.LVL77-.Ltext0
	.4byte	.LVL78-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x52
	.byte	0x9f
	.4byte	.LVL78-.Ltext0
	.4byte	.LFE62-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	0
	.4byte	0
.LVUS44:
	.uleb128 0
	.uleb128 .LVU348
	.uleb128 .LVU348
	.uleb128 .LVU458
	.uleb128 .LVU458
	.uleb128 .LVU459
	.uleb128 .LVU459
	.uleb128 0
.LLST44:
	.4byte	.LVL58-.Ltext0
	.4byte	.LVL59-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	.LVL59-.Ltext0
	.4byte	.LVL77-.Ltext0
	.2byte	0x1
	.byte	0x59
	.4byte	.LVL77-.Ltext0
	.4byte	.LVL78-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x53
	.byte	0x9f
	.4byte	.LVL78-.Ltext0
	.4byte	.LFE62-.Ltext0
	.2byte	0x1
	.byte	0x59
	.4byte	0
	.4byte	0
.LVUS45:
	.uleb128 .LVU420
	.uleb128 .LVU458
	.uleb128 .LVU458
	.uleb128 .LVU459
	.uleb128 .LVU460
	.uleb128 .LVU461
.LLST45:
	.4byte	.LVL69-.Ltext0
	.4byte	.LVL77-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	.LVL77-.Ltext0
	.4byte	.LVL78-.Ltext0
	.2byte	0x8
	.byte	0xf3
	.uleb128 0x1
	.byte	0x51
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x1c
	.byte	0x9f
	.4byte	.LVL79-.Ltext0
	.4byte	.LVL80-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	0
	.4byte	0
.LVUS46:
	.uleb128 .LVU441
	.uleb128 .LVU450
.LLST46:
	.4byte	.LVL72-.Ltext0
	.4byte	.LVL74-1-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	0
	.4byte	0
.LVUS47:
	.uleb128 .LVU429
	.uleb128 .LVU457
	.uleb128 .LVU460
	.uleb128 .LVU461
.LLST47:
	.4byte	.LVL70-.Ltext0
	.4byte	.LVL76-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x50
	.4byte	.LVL79-.Ltext0
	.4byte	.LVL80-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x50
	.4byte	0
	.4byte	0
.LVUS30:
	.uleb128 .LVU217
	.uleb128 .LVU231
	.uleb128 .LVU231
	.uleb128 .LVU337
	.uleb128 .LVU337
	.uleb128 .LVU338
	.uleb128 .LVU338
	.uleb128 .LVU340
.LLST30:
	.4byte	.LVL39-.Ltext0
	.4byte	.LVL41-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL41-.Ltext0
	.4byte	.LVL55-.Ltext0
	.2byte	0x3
	.byte	0x91
	.sleb128 -80
	.4byte	.LVL55-.Ltext0
	.4byte	.LVL56-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	.LVL56-.Ltext0
	.4byte	.LVL57-.Ltext0
	.2byte	0x3
	.byte	0x91
	.sleb128 -80
	.4byte	0
	.4byte	0
.LVUS31:
	.uleb128 .LVU210
	.uleb128 .LVU216
.LLST31:
	.4byte	.LVL38-.Ltext0
	.4byte	.LVL39-1-.Ltext0
	.2byte	0x5
	.byte	0x3
	.4byte	g_armwave_state+88
	.4byte	0
	.4byte	0
.LVUS32:
	.uleb128 .LVU210
	.uleb128 .LVU217
.LLST32:
	.4byte	.LVL38-.Ltext0
	.4byte	.LVL39-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS33:
	.uleb128 .LVU232
	.uleb128 .LVU329
.LLST33:
	.4byte	.LVL41-.Ltext0
	.4byte	.LVL54-.Ltext0
	.2byte	0x3
	.byte	0x91
	.sleb128 -84
	.4byte	0
	.4byte	0
.LVUS34:
	.uleb128 .LVU248
	.uleb128 .LVU249
.LLST34:
	.4byte	.LVL43-.Ltext0
	.4byte	.LVL44-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS35:
	.uleb128 .LVU258
	.uleb128 .LVU271
	.uleb128 .LVU271
	.uleb128 .LVU294
	.uleb128 .LVU294
	.uleb128 .LVU306
	.uleb128 .LVU306
	.uleb128 .LVU316
	.uleb128 .LVU316
	.uleb128 .LVU320
.LLST35:
	.4byte	.LVL44-.Ltext0
	.4byte	.LVL45-.Ltext0
	.2byte	0x2
	.byte	0x34
	.byte	0x9f
	.4byte	.LVL45-.Ltext0
	.4byte	.LVL47-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL47-.Ltext0
	.4byte	.LVL48-.Ltext0
	.2byte	0x2
	.byte	0x31
	.byte	0x9f
	.4byte	.LVL48-.Ltext0
	.4byte	.LVL50-.Ltext0
	.2byte	0x2
	.byte	0x32
	.byte	0x9f
	.4byte	.LVL50-.Ltext0
	.4byte	.LVL51-.Ltext0
	.2byte	0x2
	.byte	0x33
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS36:
	.uleb128 .LVU244
	.uleb128 .LVU246
	.uleb128 .LVU246
	.uleb128 .LVU326
	.uleb128 .LVU326
	.uleb128 .LVU327
	.uleb128 .LVU327
	.uleb128 .LVU329
.LLST36:
	.4byte	.LVL42-.Ltext0
	.4byte	.LVL43-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL43-.Ltext0
	.4byte	.LVL52-.Ltext0
	.2byte	0x3
	.byte	0x91
	.sleb128 -108
	.4byte	.LVL52-.Ltext0
	.4byte	.LVL53-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	.LVL53-.Ltext0
	.4byte	.LVL54-.Ltext0
	.2byte	0x3
	.byte	0x91
	.sleb128 -108
	.4byte	0
	.4byte	0
.LVUS37:
	.uleb128 .LVU272
	.uleb128 .LVU295
	.uleb128 .LVU295
	.uleb128 .LVU307
	.uleb128 .LVU307
	.uleb128 .LVU317
	.uleb128 .LVU317
	.uleb128 .LVU320
.LLST37:
	.4byte	.LVL45-.Ltext0
	.4byte	.LVL47-.Ltext0
	.2byte	0x6
	.byte	0x70
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x9f
	.4byte	.LVL47-.Ltext0
	.4byte	.LVL48-.Ltext0
	.2byte	0x8
	.byte	0x70
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x9f
	.4byte	.LVL48-.Ltext0
	.4byte	.LVL50-.Ltext0
	.2byte	0x9
	.byte	0x91
	.sleb128 -88
	.byte	0x94
	.byte	0x1
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x9f
	.4byte	.LVL50-.Ltext0
	.4byte	.LVL51-.Ltext0
	.2byte	0xa
	.byte	0x91
	.sleb128 -112
	.byte	0x6
	.byte	0x34
	.byte	0x1c
	.byte	0x6
	.byte	0x48
	.byte	0x25
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS38:
	.uleb128 .LVU270
	.uleb128 .LVU294
	.uleb128 .LVU294
	.uleb128 .LVU306
	.uleb128 .LVU306
	.uleb128 .LVU310
	.uleb128 .LVU310
	.uleb128 .LVU316
	.uleb128 .LVU316
	.uleb128 .LVU320
.LLST38:
	.4byte	.LVL45-.Ltext0
	.4byte	.LVL47-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL47-.Ltext0
	.4byte	.LVL48-.Ltext0
	.2byte	0x5
	.byte	0x70
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x9f
	.4byte	.LVL48-.Ltext0
	.4byte	.LVL49-.Ltext0
	.2byte	0x5
	.byte	0x70
	.sleb128 0
	.byte	0x40
	.byte	0x25
	.byte	0x9f
	.4byte	.LVL49-.Ltext0
	.4byte	.LVL50-.Ltext0
	.2byte	0xa
	.byte	0x91
	.sleb128 -112
	.byte	0x6
	.byte	0x34
	.byte	0x1c
	.byte	0x6
	.byte	0x40
	.byte	0x25
	.byte	0x9f
	.4byte	.LVL50-.Ltext0
	.4byte	.LVL51-.Ltext0
	.2byte	0xa
	.byte	0x91
	.sleb128 -112
	.byte	0x6
	.byte	0x34
	.byte	0x1c
	.byte	0x6
	.byte	0x48
	.byte	0x25
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS39:
	.uleb128 .LVU243
	.uleb128 .LVU340
.LLST39:
	.4byte	.LVL42-.Ltext0
	.4byte	.LVL57-.Ltext0
	.2byte	0xc
	.byte	0x77
	.sleb128 0
	.byte	0x31
	.byte	0x24
	.byte	0x3
	.4byte	g_armwave_state+4
	.byte	0x6
	.byte	0x22
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS40:
	.uleb128 .LVU251
	.uleb128 .LVU253
	.uleb128 .LVU253
	.uleb128 .LVU255
	.uleb128 .LVU255
	.uleb128 .LVU257
	.uleb128 .LVU257
	.uleb128 .LVU280
.LLST40:
	.4byte	.LVL44-.Ltext0
	.4byte	.LVL44-.Ltext0
	.2byte	0x18
	.byte	0x91
	.sleb128 -60
	.byte	0x6
	.byte	0x75
	.sleb128 0
	.byte	0x22
	.byte	0x40
	.byte	0x25
	.byte	0x7b
	.sleb128 0
	.byte	0x1e
	.byte	0x77
	.sleb128 0
	.byte	0x22
	.byte	0x31
	.byte	0x24
	.byte	0x3
	.4byte	g_armwave_state+4
	.byte	0x6
	.byte	0x22
	.byte	0x9f
	.4byte	.LVL44-.Ltext0
	.4byte	.LVL44-.Ltext0
	.2byte	0x18
	.byte	0x91
	.sleb128 -60
	.byte	0x6
	.byte	0x7a
	.sleb128 0
	.byte	0x22
	.byte	0x40
	.byte	0x25
	.byte	0x7b
	.sleb128 0
	.byte	0x1e
	.byte	0x77
	.sleb128 0
	.byte	0x22
	.byte	0x31
	.byte	0x24
	.byte	0x3
	.4byte	g_armwave_state+4
	.byte	0x6
	.byte	0x22
	.byte	0x9f
	.4byte	.LVL44-.Ltext0
	.4byte	.LVL44-.Ltext0
	.2byte	0x14
	.byte	0x75
	.sleb128 0
	.byte	0x40
	.byte	0x25
	.byte	0x7b
	.sleb128 0
	.byte	0x1e
	.byte	0x77
	.sleb128 0
	.byte	0x22
	.byte	0x31
	.byte	0x24
	.byte	0x3
	.4byte	g_armwave_state+4
	.byte	0x6
	.byte	0x22
	.byte	0x9f
	.4byte	.LVL44-.Ltext0
	.4byte	.LVL46-.Ltext0
	.2byte	0x14
	.byte	0x7a
	.sleb128 0
	.byte	0x40
	.byte	0x25
	.byte	0x7b
	.sleb128 0
	.byte	0x1e
	.byte	0x77
	.sleb128 0
	.byte	0x22
	.byte	0x31
	.byte	0x24
	.byte	0x3
	.4byte	g_armwave_state+4
	.byte	0x6
	.byte	0x22
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS0:
	.uleb128 .LVU4
	.uleb128 .LVU8
	.uleb128 .LVU8
	.uleb128 .LVU17
.LLST0:
	.4byte	.LVL0-.Ltext0
	.4byte	.LVL1-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL1-.Ltext0
	.4byte	.LVL4-.Ltext0
	.2byte	0x9
	.byte	0x74
	.sleb128 0
	.byte	0x3
	.4byte	gamma_table-1
	.byte	0x1c
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS1:
	.uleb128 0
	.uleb128 .LVU47
	.uleb128 .LVU47
	.uleb128 0
.LLST1:
	.4byte	.LVL6-.Ltext0
	.4byte	.LVL9-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL9-.Ltext0
	.4byte	.LFE59-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS2:
	.uleb128 0
	.uleb128 .LVU36
	.uleb128 .LVU36
	.uleb128 .LVU49
	.uleb128 .LVU49
	.uleb128 0
.LLST2:
	.4byte	.LVL6-.Ltext0
	.4byte	.LVL7-.Ltext0
	.2byte	0x1
	.byte	0x51
	.4byte	.LVL7-.Ltext0
	.4byte	.LVL10-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	.LVL10-.Ltext0
	.4byte	.LFE59-.Ltext0
	.2byte	0x3
	.byte	0x91
	.sleb128 -80
	.4byte	0
	.4byte	0
.LVUS3:
	.uleb128 .LVU51
	.uleb128 .LVU52
.LLST3:
	.4byte	.LVL10-.Ltext0
	.4byte	.LVL11-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS4:
	.uleb128 .LVU61
	.uleb128 .LVU67
	.uleb128 .LVU67
	.uleb128 .LVU90
	.uleb128 .LVU90
	.uleb128 .LVU102
	.uleb128 .LVU102
	.uleb128 .LVU113
	.uleb128 .LVU113
	.uleb128 .LVU117
.LLST4:
	.4byte	.LVL11-.Ltext0
	.4byte	.LVL12-.Ltext0
	.2byte	0x2
	.byte	0x34
	.byte	0x9f
	.4byte	.LVL12-.Ltext0
	.4byte	.LVL14-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL14-.Ltext0
	.4byte	.LVL15-.Ltext0
	.2byte	0x2
	.byte	0x31
	.byte	0x9f
	.4byte	.LVL15-.Ltext0
	.4byte	.LVL17-.Ltext0
	.2byte	0x2
	.byte	0x32
	.byte	0x9f
	.4byte	.LVL17-.Ltext0
	.4byte	.LVL18-.Ltext0
	.2byte	0x2
	.byte	0x33
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS5:
	.uleb128 .LVU44
	.uleb128 .LVU49
	.uleb128 .LVU49
	.uleb128 .LVU123
	.uleb128 .LVU123
	.uleb128 .LVU124
	.uleb128 .LVU124
	.uleb128 .LVU126
.LLST5:
	.4byte	.LVL8-.Ltext0
	.4byte	.LVL10-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL10-.Ltext0
	.4byte	.LVL19-.Ltext0
	.2byte	0x3
	.byte	0x91
	.sleb128 -68
	.4byte	.LVL19-.Ltext0
	.4byte	.LVL20-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	.LVL20-.Ltext0
	.4byte	.LVL21-.Ltext0
	.2byte	0x3
	.byte	0x91
	.sleb128 -68
	.4byte	0
	.4byte	0
.LVUS6:
	.uleb128 .LVU68
	.uleb128 .LVU91
	.uleb128 .LVU91
	.uleb128 .LVU103
	.uleb128 .LVU103
	.uleb128 .LVU107
	.uleb128 .LVU107
	.uleb128 .LVU114
	.uleb128 .LVU114
	.uleb128 .LVU117
.LLST6:
	.4byte	.LVL12-.Ltext0
	.4byte	.LVL14-.Ltext0
	.2byte	0x6
	.byte	0x70
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x9f
	.4byte	.LVL14-.Ltext0
	.4byte	.LVL15-.Ltext0
	.2byte	0x8
	.byte	0x70
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x9f
	.4byte	.LVL15-.Ltext0
	.4byte	.LVL16-.Ltext0
	.2byte	0x8
	.byte	0x70
	.sleb128 0
	.byte	0x40
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x9f
	.4byte	.LVL16-.Ltext0
	.4byte	.LVL17-.Ltext0
	.2byte	0x8
	.byte	0x78
	.sleb128 -2
	.byte	0x94
	.byte	0x1
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x9f
	.4byte	.LVL17-.Ltext0
	.4byte	.LVL18-.Ltext0
	.2byte	0x6
	.byte	0x78
	.sleb128 -4
	.byte	0x6
	.byte	0x48
	.byte	0x25
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS7:
	.uleb128 .LVU66
	.uleb128 .LVU90
	.uleb128 .LVU90
	.uleb128 .LVU102
	.uleb128 .LVU102
	.uleb128 .LVU107
	.uleb128 .LVU107
	.uleb128 .LVU113
	.uleb128 .LVU113
	.uleb128 .LVU117
.LLST7:
	.4byte	.LVL12-.Ltext0
	.4byte	.LVL14-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL14-.Ltext0
	.4byte	.LVL15-.Ltext0
	.2byte	0x5
	.byte	0x70
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x9f
	.4byte	.LVL15-.Ltext0
	.4byte	.LVL16-.Ltext0
	.2byte	0x5
	.byte	0x70
	.sleb128 0
	.byte	0x40
	.byte	0x25
	.byte	0x9f
	.4byte	.LVL16-.Ltext0
	.4byte	.LVL17-.Ltext0
	.2byte	0x6
	.byte	0x78
	.sleb128 -4
	.byte	0x6
	.byte	0x40
	.byte	0x25
	.byte	0x9f
	.4byte	.LVL17-.Ltext0
	.4byte	.LVL18-.Ltext0
	.2byte	0x6
	.byte	0x78
	.sleb128 -4
	.byte	0x6
	.byte	0x48
	.byte	0x25
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS8:
	.uleb128 .LVU43
	.uleb128 .LVU127
.LLST8:
	.4byte	.LVL8-.Ltext0
	.4byte	.LVL22-.Ltext0
	.2byte	0x8
	.byte	0x7a
	.sleb128 0
	.byte	0x31
	.byte	0x24
	.byte	0x7e
	.sleb128 0
	.byte	0x22
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS9:
	.uleb128 .LVU54
	.uleb128 .LVU56
	.uleb128 .LVU56
	.uleb128 .LVU58
	.uleb128 .LVU58
	.uleb128 .LVU60
	.uleb128 .LVU60
	.uleb128 .LVU78
.LLST9:
	.4byte	.LVL11-.Ltext0
	.4byte	.LVL11-.Ltext0
	.2byte	0x15
	.byte	0x76
	.sleb128 0
	.byte	0x91
	.sleb128 -76
	.byte	0x6
	.byte	0x22
	.byte	0x40
	.byte	0x25
	.byte	0x7b
	.sleb128 0
	.byte	0x1e
	.byte	0x7a
	.sleb128 0
	.byte	0x22
	.byte	0x31
	.byte	0x24
	.byte	0x7e
	.sleb128 0
	.byte	0x22
	.byte	0x9f
	.4byte	.LVL11-.Ltext0
	.4byte	.LVL11-.Ltext0
	.2byte	0x15
	.byte	0x79
	.sleb128 0
	.byte	0x91
	.sleb128 -76
	.byte	0x6
	.byte	0x22
	.byte	0x40
	.byte	0x25
	.byte	0x7b
	.sleb128 0
	.byte	0x1e
	.byte	0x7a
	.sleb128 0
	.byte	0x22
	.byte	0x31
	.byte	0x24
	.byte	0x7e
	.sleb128 0
	.byte	0x22
	.byte	0x9f
	.4byte	.LVL11-.Ltext0
	.4byte	.LVL11-.Ltext0
	.2byte	0x10
	.byte	0x76
	.sleb128 0
	.byte	0x40
	.byte	0x25
	.byte	0x7b
	.sleb128 0
	.byte	0x1e
	.byte	0x7a
	.sleb128 0
	.byte	0x22
	.byte	0x31
	.byte	0x24
	.byte	0x7e
	.sleb128 0
	.byte	0x22
	.byte	0x9f
	.4byte	.LVL11-.Ltext0
	.4byte	.LVL13-.Ltext0
	.2byte	0x10
	.byte	0x79
	.sleb128 0
	.byte	0x40
	.byte	0x25
	.byte	0x7b
	.sleb128 0
	.byte	0x1e
	.byte	0x7a
	.sleb128 0
	.byte	0x22
	.byte	0x31
	.byte	0x24
	.byte	0x7e
	.sleb128 0
	.byte	0x22
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS10:
	.uleb128 0
	.uleb128 .LVU143
	.uleb128 .LVU143
	.uleb128 .LVU204
	.uleb128 .LVU204
	.uleb128 .LVU205
	.uleb128 .LVU205
	.uleb128 0
.LLST10:
	.4byte	.LVL23-.Ltext0
	.4byte	.LVL24-1-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL24-1-.Ltext0
	.4byte	.LVL35-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	.LVL35-.Ltext0
	.4byte	.LVL36-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL36-.Ltext0
	.4byte	.LFE60-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	0
	.4byte	0
.LVUS11:
	.uleb128 .LVU170
	.uleb128 .LVU198
	.uleb128 .LVU198
	.uleb128 .LVU201
.LLST11:
	.4byte	.LVL30-.Ltext0
	.4byte	.LVL32-.Ltext0
	.2byte	0x5
	.byte	0x70
	.sleb128 0
	.byte	0x39
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL32-.Ltext0
	.4byte	.LVL33-.Ltext0
	.2byte	0x1
	.byte	0x59
	.4byte	0
	.4byte	0
.LVUS12:
	.uleb128 .LVU168
	.uleb128 .LVU201
.LLST12:
	.4byte	.LVL30-.Ltext0
	.4byte	.LVL33-.Ltext0
	.2byte	0x10
	.byte	0x70
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf7
	.uleb128 0x25
	.byte	0xf5
	.uleb128 0x4e
	.uleb128 0x25
	.byte	0x1e
	.byte	0xf7
	.uleb128 0x33
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS13:
	.uleb128 .LVU169
	.uleb128 .LVU201
.LLST13:
	.4byte	.LVL30-.Ltext0
	.4byte	.LVL33-.Ltext0
	.2byte	0x17
	.byte	0x70
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x23
	.uleb128 0x1
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf7
	.uleb128 0x25
	.byte	0x3
	.4byte	g_armwave_state+40
	.byte	0xf6
	.byte	0x4
	.uleb128 0x25
	.byte	0x1e
	.byte	0xf7
	.uleb128 0x33
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS14:
	.uleb128 .LVU171
	.uleb128 .LVU201
.LLST14:
	.4byte	.LVL30-.Ltext0
	.4byte	.LVL33-.Ltext0
	.2byte	0x10
	.byte	0x70
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf7
	.uleb128 0x25
	.byte	0xf5
	.uleb128 0x4e
	.uleb128 0x25
	.byte	0x1e
	.byte	0xf7
	.uleb128 0x33
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS15:
	.uleb128 .LVU166
	.uleb128 .LVU182
	.uleb128 .LVU182
	.uleb128 .LVU201
.LLST15:
	.4byte	.LVL30-.Ltext0
	.4byte	.LVL31-.Ltext0
	.2byte	0x63
	.byte	0x73
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x76
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x40
	.byte	0x24
	.byte	0x8
	.byte	0xff
	.byte	0x40
	.byte	0x24
	.byte	0x1a
	.byte	0x73
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x77
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x38
	.byte	0x24
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x21
	.byte	0x73
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x78
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x21
	.byte	0x40
	.byte	0x44
	.byte	0x24
	.byte	0x1f
	.byte	0x21
	.byte	0x9f
	.4byte	.LVL31-.Ltext0
	.4byte	.LVL33-.Ltext0
	.2byte	0x75
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x76
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x40
	.byte	0x24
	.byte	0x8
	.byte	0xff
	.byte	0x40
	.byte	0x24
	.byte	0x1a
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x77
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x38
	.byte	0x24
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x21
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x78
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x21
	.byte	0x40
	.byte	0x44
	.byte	0x24
	.byte	0x1f
	.byte	0x21
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS16:
	.uleb128 .LVU154
	.uleb128 .LVU156
	.uleb128 .LVU156
	.uleb128 .LVU201
.LLST16:
	.4byte	.LVL27-.Ltext0
	.4byte	.LVL28-.Ltext0
	.2byte	0x9
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x9f
	.4byte	.LVL28-.Ltext0
	.4byte	.LVL33-.Ltext0
	.2byte	0x9
	.byte	0x71
	.sleb128 -2
	.byte	0x94
	.byte	0x2
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS17:
	.uleb128 .LVU160
	.uleb128 .LVU182
	.uleb128 .LVU182
	.uleb128 .LVU201
.LLST17:
	.4byte	.LVL30-.Ltext0
	.4byte	.LVL31-.Ltext0
	.2byte	0xf
	.byte	0x73
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x78
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL31-.Ltext0
	.4byte	.LVL33-.Ltext0
	.2byte	0x15
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x78
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS18:
	.uleb128 .LVU161
	.uleb128 .LVU182
	.uleb128 .LVU182
	.uleb128 .LVU201
.LLST18:
	.4byte	.LVL30-.Ltext0
	.4byte	.LVL31-.Ltext0
	.2byte	0xf
	.byte	0x73
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x77
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL31-.Ltext0
	.4byte	.LVL33-.Ltext0
	.2byte	0x15
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x77
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS19:
	.uleb128 .LVU162
	.uleb128 .LVU182
	.uleb128 .LVU182
	.uleb128 .LVU201
.LLST19:
	.4byte	.LVL30-.Ltext0
	.4byte	.LVL31-.Ltext0
	.2byte	0xf
	.byte	0x73
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x76
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL31-.Ltext0
	.4byte	.LVL33-.Ltext0
	.2byte	0x15
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x76
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS20:
	.uleb128 .LVU146
	.uleb128 .LVU153
	.uleb128 .LVU153
	.uleb128 .LVU204
.LLST20:
	.4byte	.LVL25-.Ltext0
	.4byte	.LVL27-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL27-.Ltext0
	.4byte	.LVL35-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	0
	.4byte	0
.LVUS21:
	.uleb128 .LVU167
	.uleb128 .LVU201
.LLST21:
	.4byte	.LVL30-.Ltext0
	.4byte	.LVL33-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	0
	.4byte	0
.LVUS22:
	.uleb128 .LVU145
	.uleb128 .LVU152
	.uleb128 .LVU152
	.uleb128 .LVU153
.LLST22:
	.4byte	.LVL25-.Ltext0
	.4byte	.LVL26-.Ltext0
	.2byte	0x1
	.byte	0x5c
	.4byte	.LVL26-.Ltext0
	.4byte	.LVL27-.Ltext0
	.2byte	0xa
	.byte	0x3
	.4byte	g_armwave_state+64
	.byte	0x6
	.byte	0x73
	.sleb128 0
	.byte	0x1e
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS23:
	.uleb128 .LVU163
	.uleb128 .LVU182
	.uleb128 .LVU182
	.uleb128 .LVU201
.LLST23:
	.4byte	.LVL30-.Ltext0
	.4byte	.LVL31-.Ltext0
	.2byte	0x1a
	.byte	0x73
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x78
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x9f
	.4byte	.LVL31-.Ltext0
	.4byte	.LVL33-.Ltext0
	.2byte	0x20
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x78
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS24:
	.uleb128 .LVU164
	.uleb128 .LVU182
	.uleb128 .LVU182
	.uleb128 .LVU201
.LLST24:
	.4byte	.LVL30-.Ltext0
	.4byte	.LVL31-.Ltext0
	.2byte	0x1a
	.byte	0x73
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x77
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x9f
	.4byte	.LVL31-.Ltext0
	.4byte	.LVL33-.Ltext0
	.2byte	0x20
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x77
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS25:
	.uleb128 .LVU165
	.uleb128 .LVU182
	.uleb128 .LVU182
	.uleb128 .LVU201
.LLST25:
	.4byte	.LVL30-.Ltext0
	.4byte	.LVL31-.Ltext0
	.2byte	0x1a
	.byte	0x73
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x76
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x9f
	.4byte	.LVL31-.Ltext0
	.4byte	.LVL33-.Ltext0
	.2byte	0x20
	.byte	0x71
	.sleb128 0
	.byte	0x94
	.byte	0x2
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0x76
	.sleb128 0
	.byte	0x40
	.byte	0x24
	.byte	0x40
	.byte	0x26
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x12
	.byte	0x8
	.byte	0xff
	.byte	0x16
	.byte	0x14
	.byte	0x2d
	.byte	0x28
	.2byte	0x1
	.byte	0x16
	.byte	0x13
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS26:
	.uleb128 .LVU156
	.uleb128 .LVU158
	.uleb128 .LVU158
	.uleb128 .LVU204
.LLST26:
	.4byte	.LVL28-.Ltext0
	.4byte	.LVL29-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	.LVL29-.Ltext0
	.4byte	.LVL35-.Ltext0
	.2byte	0x2
	.byte	0x71
	.sleb128 0
	.4byte	0
	.4byte	0
.LVUS27:
	.uleb128 .LVU133
	.uleb128 .LVU143
	.uleb128 .LVU204
	.uleb128 .LVU206
.LLST27:
	.4byte	.LVL23-.Ltext0
	.4byte	.LVL24-1-.Ltext0
	.2byte	0x5
	.byte	0x3
	.4byte	g_armwave_state+4
	.4byte	.LVL35-.Ltext0
	.4byte	.LVL37-1-.Ltext0
	.2byte	0x5
	.byte	0x3
	.4byte	g_armwave_state+4
	.4byte	0
	.4byte	0
.LVUS28:
	.uleb128 .LVU134
	.uleb128 .LVU143
	.uleb128 .LVU143
	.uleb128 .LVU204
	.uleb128 .LVU204
	.uleb128 .LVU205
	.uleb128 .LVU205
	.uleb128 0
.LLST28:
	.4byte	.LVL23-.Ltext0
	.4byte	.LVL24-1-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL24-1-.Ltext0
	.4byte	.LVL35-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	.LVL35-.Ltext0
	.4byte	.LVL36-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL36-.Ltext0
	.4byte	.LFE60-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	0
	.4byte	0
.LVUS29:
	.uleb128 .LVU172
	.uleb128 .LVU201
.LLST29:
	.4byte	.LVL30-.Ltext0
	.4byte	.LVL33-.Ltext0
	.2byte	0x1c
	.byte	0x70
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf7
	.uleb128 0x25
	.byte	0xf5
	.uleb128 0x4e
	.uleb128 0x25
	.byte	0x1e
	.byte	0xf7
	.uleb128 0x33
	.byte	0x3
	.4byte	g_armwave_state+88
	.byte	0x6
	.byte	0x1e
	.byte	0x70
	.sleb128 0
	.byte	0x39
	.byte	0x26
	.byte	0x22
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS52:
	.uleb128 0
	.uleb128 .LVU500
	.uleb128 .LVU500
	.uleb128 0
.LLST52:
	.4byte	.LVL96-.Ltext0
	.4byte	.LVL97-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL97-.Ltext0
	.4byte	.LFE67-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS111:
	.uleb128 0
	.uleb128 .LVU758
	.uleb128 .LVU758
	.uleb128 .LVU771
	.uleb128 .LVU771
	.uleb128 .LVU772
	.uleb128 .LVU772
	.uleb128 0
.LLST111:
	.4byte	.LVL161-.Ltext0
	.4byte	.LVL162-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL162-.Ltext0
	.4byte	.LVL165-.Ltext0
	.2byte	0x1
	.byte	0x56
	.4byte	.LVL165-.Ltext0
	.4byte	.LVL166-1-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	.LVL166-1-.Ltext0
	.4byte	.LFE73-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
	.section	.debug_aranges,"",%progbits
	.4byte	0x1c
	.2byte	0x2
	.4byte	.Ldebug_info0
	.byte	0x4
	.byte	0
	.2byte	0
	.2byte	0
	.4byte	.Ltext0
	.4byte	.Letext0-.Ltext0
	.4byte	0
	.4byte	0
	.section	.debug_ranges,"",%progbits
.Ldebug_ranges0:
	.4byte	.LBB10-.Ltext0
	.4byte	.LBE10-.Ltext0
	.4byte	.LBB13-.Ltext0
	.4byte	.LBE13-.Ltext0
	.4byte	0
	.4byte	0
	.4byte	.LBB18-.Ltext0
	.4byte	.LBE18-.Ltext0
	.4byte	.LBB21-.Ltext0
	.4byte	.LBE21-.Ltext0
	.4byte	.LBB22-.Ltext0
	.4byte	.LBE22-.Ltext0
	.4byte	0
	.4byte	0
	.4byte	.LBB23-.Ltext0
	.4byte	.LBE23-.Ltext0
	.4byte	.LBB28-.Ltext0
	.4byte	.LBE28-.Ltext0
	.4byte	.LBB29-.Ltext0
	.4byte	.LBE29-.Ltext0
	.4byte	.LBB30-.Ltext0
	.4byte	.LBE30-.Ltext0
	.4byte	0
	.4byte	0
	.4byte	.LBB39-.Ltext0
	.4byte	.LBE39-.Ltext0
	.4byte	.LBB42-.Ltext0
	.4byte	.LBE42-.Ltext0
	.4byte	0
	.4byte	0
	.4byte	.LBB43-.Ltext0
	.4byte	.LBE43-.Ltext0
	.4byte	.LBB46-.Ltext0
	.4byte	.LBE46-.Ltext0
	.4byte	0
	.4byte	0
	.4byte	.LBB49-.Ltext0
	.4byte	.LBE49-.Ltext0
	.4byte	.LBB54-.Ltext0
	.4byte	.LBE54-.Ltext0
	.4byte	.LBB55-.Ltext0
	.4byte	.LBE55-.Ltext0
	.4byte	.LBB56-.Ltext0
	.4byte	.LBE56-.Ltext0
	.4byte	0
	.4byte	0
	.section	.debug_line,"",%progbits
.Ldebug_line0:
	.section	.debug_str,"MS",%progbits,1
.LASF275:
	.ascii	"hashsalt\000"
.LASF412:
	.ascii	"PyGen_Type\000"
.LASF405:
	.ascii	"Py_tracefunc\000"
.LASF581:
	.ascii	"armwave_test_dump_buffer_to_ppm\000"
.LASF62:
	.ascii	"_sys_errlist\000"
.LASF50:
	.ascii	"_unused2\000"
.LASF36:
	.ascii	"_fileno\000"
.LASF159:
	.ascii	"lenfunc\000"
.LASF451:
	.ascii	"PyExc_ModuleNotFoundError\000"
.LASF601:
	.ascii	"start_point\000"
.LASF453:
	.ascii	"PyExc_KeyError\000"
.LASF108:
	.ascii	"tp_getattr\000"
.LASF467:
	.ascii	"PyExc_TypeError\000"
.LASF240:
	.ascii	"sq_item\000"
.LASF200:
	.ascii	"nb_add\000"
.LASF423:
	.ascii	"PyGetSetDescr_Type\000"
.LASF8:
	.ascii	"__uint8_t\000"
.LASF483:
	.ascii	"PyExc_FileNotFoundError\000"
.LASF544:
	.ascii	"test_wave_buffer_stride\000"
.LASF99:
	.ascii	"ob_refcnt\000"
.LASF321:
	.ascii	"PyTuple_Type\000"
.LASF242:
	.ascii	"sq_ass_item\000"
.LASF183:
	.ascii	"PyBaseObject_Type\000"
.LASF444:
	.ascii	"PyExc_AssertionError\000"
.LASF94:
	.ascii	"tracing\000"
.LASF246:
	.ascii	"sq_inplace_repeat\000"
.LASF110:
	.ascii	"tp_as_async\000"
.LASF418:
	.ascii	"_PyAsyncGenWrappedValue_Type\000"
.LASF234:
	.ascii	"nb_matrix_multiply\000"
.LASF211:
	.ascii	"nb_lshift\000"
.LASF245:
	.ascii	"sq_inplace_concat\000"
.LASF141:
	.ascii	"tp_is_gc\000"
.LASF41:
	.ascii	"_shortbuf\000"
.LASF205:
	.ascii	"nb_power\000"
.LASF641:
	.ascii	"fwrite\000"
.LASF254:
	.ascii	"am_anext\000"
.LASF617:
	.ascii	"out_buffer_base\000"
.LASF368:
	.ascii	"PyCell_Type\000"
.LASF462:
	.ascii	"PyExc_IndentationError\000"
.LASF239:
	.ascii	"sq_repeat\000"
.LASF66:
	.ascii	"__environ\000"
.LASF649:
	.ascii	"/home/pi/git/scopeapp/ZynqScope/armwave\000"
.LASF364:
	.ascii	"_frame\000"
.LASF105:
	.ascii	"tp_itemsize\000"
.LASF276:
	.ascii	"siphash\000"
.LASF496:
	.ascii	"PyExc_SyntaxWarning\000"
.LASF238:
	.ascii	"sq_concat\000"
.LASF73:
	.ascii	"uint16_t\000"
.LASF447:
	.ascii	"PyExc_EOFError\000"
.LASF515:
	.ascii	"PyNullImporter_Type\000"
.LASF179:
	.ascii	"initproc\000"
.LASF509:
	.ascii	"PyOS_ReadlineFunctionPointer\000"
.LASF22:
	.ascii	"_flags\000"
.LASF555:
	.ascii	"slice_record_height\000"
.LASF508:
	.ascii	"PyOS_InputHook\000"
.LASF374:
	.ascii	"next\000"
.LASF491:
	.ascii	"PyExc_IOError\000"
.LASF265:
	.ascii	"PyGetSetDef\000"
.LASF629:
	.ascii	"calloc\000"
.LASF142:
	.ascii	"tp_bases\000"
.LASF17:
	.ascii	"__off_t\000"
.LASF463:
	.ascii	"PyExc_TabError\000"
.LASF425:
	.ascii	"PyMethodDescr_Type\000"
.LASF543:
	.ascii	"test_wave_buffer\000"
.LASF286:
	.ascii	"Py_OptimizeFlag\000"
.LASF621:
	.ascii	"height\000"
.LASF42:
	.ascii	"_lock\000"
.LASF314:
	.ascii	"PyFloat_Type\000"
.LASF318:
	.ascii	"PyLongRangeIter_Type\000"
.LASF171:
	.ascii	"setattrofunc\000"
.LASF106:
	.ascii	"tp_dealloc\000"
.LASF554:
	.ascii	"slice_height\000"
.LASF408:
	.ascii	"exc_value\000"
.LASF89:
	.ascii	"TRACEMALLOC_NOT_INITIALIZED\000"
.LASF298:
	.ascii	"_PyByteArray_empty_string\000"
.LASF101:
	.ascii	"_typeobject\000"
.LASF229:
	.ascii	"nb_floor_divide\000"
.LASF586:
	.ascii	"render_width\000"
.LASF224:
	.ascii	"nb_inplace_lshift\000"
.LASF590:
	.ascii	"data\000"
.LASF480:
	.ascii	"PyExc_ConnectionRefusedError\000"
.LASF479:
	.ascii	"PyExc_ConnectionAbortedError\000"
.LASF449:
	.ascii	"PyExc_OSError\000"
.LASF575:
	.ascii	"xnoise\000"
.LASF292:
	.ascii	"Py_NoUserSiteDirectory\000"
.LASF478:
	.ascii	"PyExc_ConnectionError\000"
.LASF476:
	.ascii	"PyExc_BrokenPipeError\000"
.LASF492:
	.ascii	"PyExc_Warning\000"
.LASF233:
	.ascii	"nb_index\000"
.LASF125:
	.ascii	"tp_richcompare\000"
.LASF511:
	.ascii	"_Py_CheckRecursionLimit\000"
.LASF472:
	.ascii	"PyExc_UnicodeTranslateError\000"
.LASF565:
	.ascii	"ch3_color\000"
.LASF440:
	.ascii	"PyExc_StopIteration\000"
.LASF477:
	.ascii	"PyExc_ChildProcessError\000"
.LASF637:
	.ascii	"__builtin_memset\000"
.LASF28:
	.ascii	"_IO_write_end\000"
.LASF372:
	.ascii	"PyThreadState\000"
.LASF203:
	.ascii	"nb_remainder\000"
.LASF644:
	.ascii	"fclose\000"
.LASF164:
	.ascii	"visitproc\000"
.LASF355:
	.ascii	"PyMethod_Type\000"
.LASF610:
	.ascii	"out_buffer\000"
.LASF313:
	.ascii	"_Py_TrueStruct\000"
.LASF221:
	.ascii	"nb_inplace_multiply\000"
.LASF512:
	.ascii	"_inittab\000"
.LASF628:
	.ascii	"free\000"
.LASF282:
	.ascii	"Py_VerboseFlag\000"
.LASF516:
	.ascii	"_frozen\000"
.LASF241:
	.ascii	"was_sq_slice\000"
.LASF83:
	.ascii	"__tzname\000"
.LASF264:
	.ascii	"PyMemberDef\000"
.LASF514:
	.ascii	"PyImport_Inittab\000"
.LASF375:
	.ascii	"interp\000"
.LASF530:
	.ascii	"__malloc_hook\000"
.LASF100:
	.ascii	"ob_type\000"
.LASF495:
	.ascii	"PyExc_PendingDeprecationWarning\000"
.LASF140:
	.ascii	"tp_free\000"
.LASF497:
	.ascii	"PyExc_RuntimeWarning\000"
.LASF320:
	.ascii	"PyMemoryView_Type\000"
.LASF579:
	.ascii	"buf_obj\000"
.LASF154:
	.ascii	"PyVarObject\000"
.LASF319:
	.ascii	"_PyManagedBuffer_Type\000"
.LASF90:
	.ascii	"TRACEMALLOC_INITIALIZED\000"
.LASF384:
	.ascii	"c_profileobj\000"
.LASF607:
	.ascii	"xx_rem\000"
.LASF556:
	.ascii	"wave_length\000"
.LASF557:
	.ascii	"ch_buff_size\000"
.LASF213:
	.ascii	"nb_and\000"
.LASF475:
	.ascii	"PyExc_BlockingIOError\000"
.LASF68:
	.ascii	"optarg\000"
.LASF406:
	.ascii	"_err_stackitem\000"
.LASF488:
	.ascii	"PyExc_ProcessLookupError\000"
.LASF116:
	.ascii	"tp_call\000"
.LASF393:
	.ascii	"async_exc\000"
.LASF585:
	.ascii	"nwaves\000"
.LASF587:
	.ascii	"render_height\000"
.LASF536:
	.ascii	"armwave_state_t\000"
.LASF269:
	.ascii	"_PyNone_Type\000"
.LASF392:
	.ascii	"gilstate_counter\000"
.LASF60:
	.ascii	"sys_errlist\000"
.LASF569:
	.ascii	"gamma_table\000"
.LASF117:
	.ascii	"tp_str\000"
.LASF560:
	.ascii	"row_shift\000"
.LASF576:
	.ascii	"mod_val\000"
.LASF243:
	.ascii	"was_sq_ass_slice\000"
.LASF458:
	.ascii	"PyExc_RuntimeError\000"
.LASF87:
	.ascii	"daylight\000"
.LASF9:
	.ascii	"__int16_t\000"
.LASF11:
	.ascii	"__uint16_t\000"
.LASF157:
	.ascii	"ternaryfunc\000"
.LASF442:
	.ascii	"PyExc_ArithmeticError\000"
.LASF102:
	.ascii	"ob_base\000"
.LASF268:
	.ascii	"PyTypeObject\000"
.LASF363:
	.ascii	"PyTraceBack_Type\000"
.LASF435:
	.ascii	"PyPickleBuffer_Type\000"
.LASF446:
	.ascii	"PyExc_BufferError\000"
.LASF285:
	.ascii	"Py_InspectFlag\000"
.LASF415:
	.ascii	"_PyAIterWrapper_Type\000"
.LASF244:
	.ascii	"sq_contains\000"
.LASF274:
	.ascii	"padding\000"
.LASF602:
	.ascii	"end_point\000"
.LASF35:
	.ascii	"_chain\000"
.LASF109:
	.ascii	"tp_setattr\000"
.LASF507:
	.ascii	"PyCode_Type\000"
.LASF645:
	.ascii	"malloc\000"
.LASF174:
	.ascii	"richcmpfunc\000"
.LASF3:
	.ascii	"unsigned char\000"
.LASF351:
	.ascii	"PyModuleDef_Type\000"
.LASF277:
	.ascii	"djbx33a\000"
.LASF429:
	.ascii	"PyProperty_Type\000"
.LASF250:
	.ascii	"mp_ass_subscript\000"
.LASF485:
	.ascii	"PyExc_IsADirectoryError\000"
.LASF605:
	.ascii	"points_per_pixel\000"
.LASF335:
	.ascii	"PyDictRevIterItem_Type\000"
.LASF93:
	.ascii	"initialized\000"
.LASF650:
	.ascii	"_IO_lock_t\000"
.LASF468:
	.ascii	"PyExc_UnboundLocalError\000"
.LASF0:
	.ascii	"float\000"
.LASF136:
	.ascii	"tp_dictoffset\000"
.LASF627:
	.ascii	"gamma\000"
.LASF441:
	.ascii	"PyExc_GeneratorExit\000"
.LASF236:
	.ascii	"PyNumberMethods\000"
.LASF259:
	.ascii	"PyMethodDef\000"
.LASF545:
	.ascii	"test_wave_buffer_nsets\000"
.LASF149:
	.ascii	"tp_finalize\000"
.LASF422:
	.ascii	"PyClassMethodDescr_Type\000"
.LASF346:
	.ascii	"PyFrozenSet_Type\000"
.LASF633:
	.ascii	"PyBuffer_Release\000"
.LASF558:
	.ascii	"target_width\000"
.LASF249:
	.ascii	"mp_subscript\000"
.LASF124:
	.ascii	"tp_clear\000"
.LASF283:
	.ascii	"Py_QuietFlag\000"
.LASF428:
	.ascii	"_PyMethodWrapper_Type\000"
.LASF529:
	.ascii	"__free_hook\000"
.LASF331:
	.ascii	"PyDictIterKey_Type\000"
.LASF535:
	.ascii	"armwave_color_mix_t\000"
.LASF75:
	.ascii	"uint64_t\000"
.LASF466:
	.ascii	"PyExc_SystemExit\000"
.LASF357:
	.ascii	"Py_FileSystemDefaultEncoding\000"
.LASF381:
	.ascii	"use_tracing\000"
.LASF323:
	.ascii	"PyList_Type\000"
.LASF391:
	.ascii	"dict\000"
.LASF209:
	.ascii	"nb_bool\000"
.LASF344:
	.ascii	"_PySet_Dummy\000"
.LASF362:
	.ascii	"PyCapsule_Type\000"
.LASF293:
	.ascii	"Py_UnbufferedStdioFlag\000"
.LASF137:
	.ascii	"tp_init\000"
.LASF162:
	.ascii	"objobjargproc\000"
.LASF153:
	.ascii	"ob_size\000"
.LASF636:
	.ascii	"__builtin_puts\000"
.LASF133:
	.ascii	"tp_dict\000"
.LASF482:
	.ascii	"PyExc_FileExistsError\000"
.LASF27:
	.ascii	"_IO_write_ptr\000"
.LASF114:
	.ascii	"tp_as_mapping\000"
.LASF170:
	.ascii	"setattrfunc\000"
.LASF592:
	.ascii	"armwave_set_channel_colour\000"
.LASF615:
	.ascii	"value\000"
.LASF366:
	.ascii	"PySlice_Type\000"
.LASF546:
	.ascii	"cmp_x_bitdepth_scale\000"
.LASF460:
	.ascii	"PyExc_NotImplementedError\000"
.LASF156:
	.ascii	"binaryfunc\000"
.LASF424:
	.ascii	"PyMemberDescr_Type\000"
.LASF548:
	.ascii	"xstride\000"
.LASF518:
	.ascii	"size\000"
.LASF51:
	.ascii	"FILE\000"
.LASF256:
	.ascii	"bf_getbuffer\000"
.LASF199:
	.ascii	"vectorcallfunc\000"
.LASF567:
	.ascii	"xcoord_to_xpixel\000"
.LASF175:
	.ascii	"getiterfunc\000"
.LASF414:
	.ascii	"_PyCoroWrapper_Type\000"
.LASF524:
	.ascii	"_Py_ctype_tolower\000"
.LASF160:
	.ascii	"ssizeargfunc\000"
.LASF638:
	.ascii	"__assert_fail\000"
.LASF612:
	.ascii	"wave_word\000"
.LASF566:
	.ascii	"ch4_color\000"
.LASF389:
	.ascii	"exc_state\000"
.LASF7:
	.ascii	"size_t\000"
.LASF91:
	.ascii	"TRACEMALLOC_FINALIZED\000"
.LASF88:
	.ascii	"getdate_err\000"
.LASF72:
	.ascii	"uint8_t\000"
.LASF178:
	.ascii	"descrsetfunc\000"
.LASF426:
	.ascii	"PyWrapperDescr_Type\000"
.LASF598:
	.ascii	"armwave_set_wave_pointer\000"
.LASF533:
	.ascii	"__after_morecore_hook\000"
.LASF177:
	.ascii	"descrgetfunc\000"
.LASF600:
	.ascii	"armwave_setup_render\000"
.LASF279:
	.ascii	"_Py_HashSecret_t\000"
.LASF407:
	.ascii	"exc_type\000"
.LASF219:
	.ascii	"nb_inplace_add\000"
.LASF217:
	.ascii	"nb_reserved\000"
.LASF613:
	.ascii	"nsub\000"
.LASF397:
	.ascii	"on_delete\000"
.LASF653:
	.ascii	"_Py_INCREF\000"
.LASF31:
	.ascii	"_IO_save_base\000"
.LASF499:
	.ascii	"PyExc_ImportWarning\000"
.LASF301:
	.ascii	"PyUnicode_Type\000"
.LASF67:
	.ascii	"environ\000"
.LASF172:
	.ascii	"reprfunc\000"
.LASF326:
	.ascii	"PySortWrapper_Type\000"
.LASF542:
	.ascii	"wave_buffer\000"
.LASF388:
	.ascii	"curexc_traceback\000"
.LASF281:
	.ascii	"Py_DebugFlag\000"
.LASF532:
	.ascii	"__memalign_hook\000"
.LASF517:
	.ascii	"code\000"
.LASF45:
	.ascii	"_wide_data\000"
.LASF538:
	.ascii	"ch1_buffer\000"
.LASF520:
	.ascii	"PyFilter_Type\000"
.LASF433:
	.ascii	"PyStructSequence_UnnamedField\000"
.LASF571:
	.ascii	"armwave_test_create_am_sine\000"
.LASF456:
	.ascii	"PyExc_NameError\000"
.LASF97:
	.ascii	"_Py_tracemalloc_config\000"
.LASF378:
	.ascii	"overflowed\000"
.LASF588:
	.ascii	"armwave_dump_ppm_debug\000"
.LASF78:
	.ascii	"signgam\000"
.LASF327:
	.ascii	"PyDict_Type\000"
.LASF539:
	.ascii	"ch2_buffer\000"
.LASF77:
	.ascii	"Py_hash_t\000"
.LASF14:
	.ascii	"__uint64_t\000"
.LASF626:
	.ascii	"armwave_init\000"
.LASF152:
	.ascii	"PyObject\000"
.LASF214:
	.ascii	"nb_xor\000"
.LASF502:
	.ascii	"PyExc_ResourceWarning\000"
.LASF206:
	.ascii	"nb_negative\000"
.LASF639:
	.ascii	"fopen64\000"
.LASF361:
	.ascii	"PyStdPrinter_Type\000"
.LASF92:
	.ascii	"_PyTraceMalloc_Config\000"
.LASF519:
	.ascii	"PyImport_FrozenModules\000"
.LASF643:
	.ascii	"fprintf\000"
.LASF540:
	.ascii	"ch3_buffer\000"
.LASF417:
	.ascii	"_PyAsyncGenASend_Type\000"
.LASF20:
	.ascii	"__ssize_t\000"
.LASF551:
	.ascii	"waves\000"
.LASF333:
	.ascii	"PyDictIterItem_Type\000"
.LASF340:
	.ascii	"PyODictItems_Type\000"
.LASF337:
	.ascii	"PyODict_Type\000"
.LASF338:
	.ascii	"PyODictIter_Type\000"
.LASF379:
	.ascii	"recursion_critical\000"
.LASF85:
	.ascii	"__timezone\000"
.LASF427:
	.ascii	"PyDictProxy_Type\000"
.LASF370:
	.ascii	"PyCallIter_Type\000"
.LASF409:
	.ascii	"exc_traceback\000"
.LASF258:
	.ascii	"PyBufferProcs\000"
.LASF632:
	.ascii	"PyObject_GetBuffer\000"
.LASF570:
	.ascii	"armwave_cleanup\000"
.LASF371:
	.ascii	"PyCmpWrapper_Type\000"
.LASF593:
	.ascii	"nsets\000"
.LASF262:
	.ascii	"ml_flags\000"
.LASF139:
	.ascii	"tp_new\000"
.LASF353:
	.ascii	"PyClassMethod_Type\000"
.LASF591:
	.ascii	"armwave_test_buffer_alloc\000"
.LASF232:
	.ascii	"nb_inplace_true_divide\000"
.LASF411:
	.ascii	"_PyErr_StackItem\000"
.LASF167:
	.ascii	"destructor\000"
.LASF349:
	.ascii	"PyCFunction\000"
.LASF582:
	.ascii	"armwave_test_fill_outbuf\000"
.LASF635:
	.ascii	"memset\000"
.LASF58:
	.ascii	"stderr\000"
.LASF345:
	.ascii	"PySet_Type\000"
.LASF266:
	.ascii	"name\000"
.LASF303:
	.ascii	"_Py_ascii_whitespace\000"
.LASF64:
	.ascii	"program_invocation_short_name\000"
.LASF493:
	.ascii	"PyExc_UserWarning\000"
.LASF33:
	.ascii	"_IO_save_end\000"
.LASF504:
	.ascii	"PyContextVar_Type\000"
.LASF147:
	.ascii	"tp_del\000"
.LASF316:
	.ascii	"PyRange_Type\000"
.LASF356:
	.ascii	"PyInstanceMethod_Type\000"
.LASF367:
	.ascii	"PyEllipsis_Type\000"
.LASF96:
	.ascii	"use_domain\000"
.LASF57:
	.ascii	"stdout\000"
.LASF103:
	.ascii	"tp_name\000"
.LASF71:
	.ascii	"optopt\000"
.LASF267:
	.ascii	"closure\000"
.LASF513:
	.ascii	"initfunc\000"
.LASF634:
	.ascii	"puts\000"
.LASF550:
	.ascii	"wave_stride\000"
.LASF287:
	.ascii	"Py_NoSiteFlag\000"
.LASF599:
	.ascii	"__PRETTY_FUNCTION__\000"
.LASF382:
	.ascii	"c_profilefunc\000"
.LASF534:
	.ascii	"bufftyp_t\000"
.LASF113:
	.ascii	"tp_as_sequence\000"
.LASF120:
	.ascii	"tp_as_buffer\000"
.LASF188:
	.ascii	"itemsize\000"
.LASF616:
	.ascii	"base_32ptr\000"
.LASF226:
	.ascii	"nb_inplace_and\000"
.LASF310:
	.ascii	"digit\000"
.LASF65:
	.ascii	"int16_t\000"
.LASF4:
	.ascii	"short unsigned int\000"
.LASF6:
	.ascii	"signed char\000"
.LASF400:
	.ascii	"async_gen_firstiter\000"
.LASF350:
	.ascii	"PyModule_Type\000"
.LASF583:
	.ascii	"armwave_test_init\000"
.LASF387:
	.ascii	"curexc_value\000"
.LASF526:
	.ascii	"ptrdiff_t\000"
.LASF342:
	.ascii	"PyEnum_Type\000"
.LASF138:
	.ascii	"tp_alloc\000"
.LASF194:
	.ascii	"suboffsets\000"
.LASF606:
	.ascii	"armwave_generate\000"
.LASF489:
	.ascii	"PyExc_TimeoutError\000"
.LASF580:
	.ascii	"armwave_test_fill_gdkbuf\000"
.LASF395:
	.ascii	"trash_delete_nesting\000"
.LASF19:
	.ascii	"__off64_t\000"
.LASF25:
	.ascii	"_IO_read_base\000"
.LASF522:
	.ascii	"PyZip_Type\000"
.LASF43:
	.ascii	"_offset\000"
.LASF322:
	.ascii	"PyTupleIter_Type\000"
.LASF552:
	.ascii	"waves_max\000"
.LASF30:
	.ascii	"_IO_buf_end\000"
.LASF118:
	.ascii	"tp_getattro\000"
.LASF181:
	.ascii	"allocfunc\000"
.LASF270:
	.ascii	"_PyNotImplemented_Type\000"
.LASF385:
	.ascii	"c_traceobj\000"
.LASF70:
	.ascii	"opterr\000"
.LASF95:
	.ascii	"max_nframe\000"
.LASF49:
	.ascii	"_mode\000"
.LASF343:
	.ascii	"PyReversed_Type\000"
.LASF129:
	.ascii	"tp_methods\000"
.LASF26:
	.ascii	"_IO_write_base\000"
.LASF143:
	.ascii	"tp_mro\000"
.LASF455:
	.ascii	"PyExc_MemoryError\000"
.LASF82:
	.ascii	"tz_dsttime\000"
.LASF437:
	.ascii	"PyExc_BaseException\000"
.LASF271:
	.ascii	"_Py_SwappedOp\000"
.LASF431:
	.ascii	"_PyWeakref_ProxyType\000"
.LASF647:
	.ascii	"GNU C17 8.3.0 -mfloat-abi=hard -mfpu=vfp -mtls-dial"
	.ascii	"ect=gnu -marm -march=armv6+fp -g -O3 -fwrapv\000"
.LASF503:
	.ascii	"PyContext_Type\000"
.LASF505:
	.ascii	"PyContextToken_Type\000"
.LASF448:
	.ascii	"PyExc_FloatingPointError\000"
.LASF324:
	.ascii	"PyListIter_Type\000"
.LASF549:
	.ascii	"vscale\000"
.LASF18:
	.ascii	"long int\000"
.LASF215:
	.ascii	"nb_or\000"
.LASF648:
	.ascii	"armwave.c\000"
.LASF191:
	.ascii	"format\000"
.LASF155:
	.ascii	"unaryfunc\000"
.LASF52:
	.ascii	"_IO_marker\000"
.LASF296:
	.ascii	"PyByteArray_Type\000"
.LASF523:
	.ascii	"_Py_ctype_table\000"
.LASF218:
	.ascii	"nb_float\000"
.LASF465:
	.ascii	"PyExc_SystemError\000"
.LASF329:
	.ascii	"PyDictValues_Type\000"
.LASF547:
	.ascii	"vscale_frac\000"
.LASF473:
	.ascii	"PyExc_ValueError\000"
.LASF506:
	.ascii	"_Py_PackageContext\000"
.LASF165:
	.ascii	"traverseproc\000"
.LASF402:
	.ascii	"context\000"
.LASF107:
	.ascii	"tp_vectorcall_offset\000"
.LASF158:
	.ascii	"inquiry\000"
.LASF74:
	.ascii	"uint32_t\000"
.LASF210:
	.ascii	"nb_invert\000"
.LASF263:
	.ascii	"ml_doc\000"
.LASF53:
	.ascii	"_IO_codecvt\000"
.LASF260:
	.ascii	"ml_name\000"
.LASF559:
	.ascii	"target_height\000"
.LASF452:
	.ascii	"PyExc_IndexError\000"
.LASF369:
	.ascii	"PySeqIter_Type\000"
.LASF527:
	.ascii	"long double\000"
.LASF328:
	.ascii	"PyDictKeys_Type\000"
.LASF438:
	.ascii	"PyExc_Exception\000"
.LASF112:
	.ascii	"tp_as_number\000"
.LASF354:
	.ascii	"PyStaticMethod_Type\000"
.LASF288:
	.ascii	"Py_BytesWarningFlag\000"
.LASF255:
	.ascii	"PyAsyncMethods\000"
.LASF126:
	.ascii	"tp_weaklistoffset\000"
.LASF5:
	.ascii	"long unsigned int\000"
.LASF261:
	.ascii	"ml_meth\000"
.LASF189:
	.ascii	"readonly\000"
.LASF122:
	.ascii	"tp_doc\000"
.LASF403:
	.ascii	"context_ver\000"
.LASF169:
	.ascii	"getattrofunc\000"
.LASF325:
	.ascii	"PyListRevIter_Type\000"
.LASF21:
	.ascii	"char\000"
.LASF531:
	.ascii	"__realloc_hook\000"
.LASF247:
	.ascii	"PySequenceMethods\000"
.LASF56:
	.ascii	"stdin\000"
.LASF398:
	.ascii	"on_delete_data\000"
.LASF146:
	.ascii	"tp_weaklist\000"
.LASF642:
	.ascii	"__builtin_fwrite\000"
.LASF29:
	.ascii	"_IO_buf_base\000"
.LASF187:
	.ascii	"bufferinfo\000"
.LASF180:
	.ascii	"newfunc\000"
.LASF521:
	.ascii	"PyMap_Type\000"
.LASF173:
	.ascii	"hashfunc\000"
.LASF168:
	.ascii	"getattrfunc\000"
.LASF24:
	.ascii	"_IO_read_end\000"
.LASF464:
	.ascii	"PyExc_ReferenceError\000"
.LASF79:
	.ascii	"_IO_FILE\000"
.LASF572:
	.ascii	"noise_fraction\000"
.LASF54:
	.ascii	"_IO_wide_data\000"
.LASF563:
	.ascii	"ch1_color\000"
.LASF486:
	.ascii	"PyExc_NotADirectoryError\000"
.LASF494:
	.ascii	"PyExc_DeprecationWarning\000"
.LASF86:
	.ascii	"tzname\000"
.LASF611:
	.ascii	"word\000"
.LASF622:
	.ascii	"scale_value\000"
.LASF386:
	.ascii	"curexc_type\000"
.LASF589:
	.ascii	"buffer\000"
.LASF192:
	.ascii	"shape\000"
.LASF115:
	.ascii	"tp_hash\000"
.LASF297:
	.ascii	"PyByteArrayIter_Type\000"
.LASF273:
	.ascii	"suffix\000"
.LASF603:
	.ascii	"render_flags\000"
.LASF500:
	.ascii	"PyExc_UnicodeWarning\000"
.LASF190:
	.ascii	"ndim\000"
.LASF161:
	.ascii	"ssizeobjargproc\000"
.LASF608:
	.ascii	"ypos\000"
.LASF150:
	.ascii	"tp_vectorcall\000"
.LASF459:
	.ascii	"PyExc_RecursionError\000"
.LASF148:
	.ascii	"tp_version_tag\000"
.LASF383:
	.ascii	"c_tracefunc\000"
.LASF48:
	.ascii	"__pad5\000"
.LASF197:
	.ascii	"getbufferproc\000"
.LASF308:
	.ascii	"_PyLong_Zero\000"
.LASF295:
	.ascii	"Py_IsolatedFlag\000"
.LASF34:
	.ascii	"_markers\000"
.LASF421:
	.ascii	"setter\000"
.LASF410:
	.ascii	"previous_item\000"
.LASF619:
	.ascii	"render_nonaa_to_buffer_1ch_slice\000"
.LASF252:
	.ascii	"am_await\000"
.LASF490:
	.ascii	"PyExc_EnvironmentError\000"
.LASF432:
	.ascii	"_PyWeakref_CallableProxyType\000"
.LASF597:
	.ascii	"armwave_set_wave_pointer_as_testbuf\000"
.LASF44:
	.ascii	"_codecvt\000"
.LASF130:
	.ascii	"tp_members\000"
.LASF123:
	.ascii	"tp_traverse\000"
.LASF248:
	.ascii	"mp_length\000"
.LASF2:
	.ascii	"double\000"
.LASF253:
	.ascii	"am_aiter\000"
.LASF227:
	.ascii	"nb_inplace_xor\000"
.LASF55:
	.ascii	"ssize_t\000"
.LASF652:
	.ascii	"test_create_gamma\000"
.LASF307:
	.ascii	"_PyLong_DigitValue\000"
.LASF145:
	.ascii	"tp_subclasses\000"
.LASF223:
	.ascii	"nb_inplace_power\000"
.LASF577:
	.ascii	"_1_waves_mod\000"
.LASF280:
	.ascii	"_Py_HashSecret\000"
.LASF119:
	.ascii	"tp_setattro\000"
.LASF311:
	.ascii	"PyBool_Type\000"
.LASF166:
	.ascii	"freefunc\000"
.LASF12:
	.ascii	"__uint32_t\000"
.LASF630:
	.ascii	"printf\000"
.LASF202:
	.ascii	"nb_multiply\000"
.LASF631:
	.ascii	"rand\000"
.LASF84:
	.ascii	"__daylight\000"
.LASF230:
	.ascii	"nb_true_divide\000"
.LASF131:
	.ascii	"tp_getset\000"
.LASF306:
	.ascii	"PyLong_Type\000"
.LASF594:
	.ascii	"armwave_clear_buffer\000"
.LASF128:
	.ascii	"tp_iternext\000"
.LASF501:
	.ascii	"PyExc_BytesWarning\000"
.LASF347:
	.ascii	"PySetIter_Type\000"
.LASF434:
	.ascii	"_PyNamespace_Type\000"
.LASF237:
	.ascii	"sq_length\000"
.LASF419:
	.ascii	"_PyAsyncGenAThrow_Type\000"
.LASF134:
	.ascii	"tp_descr_get\000"
.LASF289:
	.ascii	"Py_FrozenFlag\000"
.LASF595:
	.ascii	"armwave_set_wave_pointer_u32\000"
.LASF127:
	.ascii	"tp_iter\000"
.LASF231:
	.ascii	"nb_inplace_floor_divide\000"
.LASF63:
	.ascii	"program_invocation_name\000"
.LASF609:
	.ascii	"armwave_fill_pixbuf_scaled\000"
.LASF299:
	.ascii	"PyBytes_Type\000"
.LASF193:
	.ascii	"strides\000"
.LASF413:
	.ascii	"PyCoro_Type\000"
.LASF278:
	.ascii	"expat\000"
.LASF336:
	.ascii	"PyDictRevIterValue_Type\000"
.LASF304:
	.ascii	"_longobject\000"
.LASF330:
	.ascii	"PyDictItems_Type\000"
.LASF309:
	.ascii	"_PyLong_One\000"
.LASF132:
	.ascii	"tp_base\000"
.LASF212:
	.ascii	"nb_rshift\000"
.LASF47:
	.ascii	"_freeres_buf\000"
.LASF450:
	.ascii	"PyExc_ImportError\000"
.LASF390:
	.ascii	"exc_info\000"
.LASF436:
	.ascii	"Py_hexdigits\000"
.LASF257:
	.ascii	"bf_releasebuffer\000"
.LASF469:
	.ascii	"PyExc_UnicodeError\000"
.LASF401:
	.ascii	"async_gen_finalizer\000"
.LASF151:
	.ascii	"tp_print\000"
.LASF15:
	.ascii	"long long unsigned int\000"
.LASF377:
	.ascii	"recursion_depth\000"
.LASF618:
	.ascii	"offset\000"
.LASF604:
	.ascii	"length\000"
.LASF651:
	.ascii	"armwave_fill_pixbuf_into_pybuffer\000"
.LASF39:
	.ascii	"_cur_column\000"
.LASF198:
	.ascii	"releasebufferproc\000"
.LASF332:
	.ascii	"PyDictIterValue_Type\000"
.LASF222:
	.ascii	"nb_inplace_remainder\000"
.LASF394:
	.ascii	"thread_id\000"
.LASF573:
	.ascii	"sets\000"
.LASF98:
	.ascii	"_object\000"
.LASF294:
	.ascii	"Py_HashRandomizationFlag\000"
.LASF640:
	.ascii	"fopen\000"
.LASF208:
	.ascii	"nb_absolute\000"
.LASF561:
	.ascii	"row_mask\000"
.LASF454:
	.ascii	"PyExc_KeyboardInterrupt\000"
.LASF32:
	.ascii	"_IO_backup_base\000"
.LASF23:
	.ascii	"_IO_read_ptr\000"
.LASF399:
	.ascii	"coroutine_origin_tracking_depth\000"
.LASF195:
	.ascii	"internal\000"
.LASF396:
	.ascii	"trash_delete_later\000"
.LASF46:
	.ascii	"_freeres_list\000"
.LASF359:
	.ascii	"Py_FileSystemDefaultEncodeErrors\000"
.LASF291:
	.ascii	"Py_DontWriteBytecodeFlag\000"
.LASF228:
	.ascii	"nb_inplace_or\000"
.LASF61:
	.ascii	"_sys_nerr\000"
.LASF623:
	.ascii	"wave_base\000"
.LASF80:
	.ascii	"timezone\000"
.LASF111:
	.ascii	"tp_repr\000"
.LASF445:
	.ascii	"PyExc_AttributeError\000"
.LASF144:
	.ascii	"tp_cache\000"
.LASF443:
	.ascii	"PyExc_LookupError\000"
.LASF76:
	.ascii	"Py_ssize_t\000"
.LASF300:
	.ascii	"PyBytesIter_Type\000"
.LASF360:
	.ascii	"Py_UTF8Mode\000"
.LASF614:
	.ascii	"npix\000"
.LASF38:
	.ascii	"_old_offset\000"
.LASF457:
	.ascii	"PyExc_OverflowError\000"
.LASF225:
	.ascii	"nb_inplace_rshift\000"
.LASF358:
	.ascii	"Py_HasFileSystemDefaultEncoding\000"
.LASF341:
	.ascii	"PyODictValues_Type\000"
.LASF302:
	.ascii	"PyUnicodeIter_Type\000"
.LASF315:
	.ascii	"PyComplex_Type\000"
.LASF186:
	.ascii	"_Py_NotImplementedStruct\000"
.LASF69:
	.ascii	"optind\000"
.LASF207:
	.ascii	"nb_positive\000"
.LASF352:
	.ascii	"PyFunction_Type\000"
.LASF553:
	.ascii	"bitdepth_height\000"
.LASF13:
	.ascii	"long long int\000"
.LASF471:
	.ascii	"PyExc_UnicodeDecodeError\000"
.LASF185:
	.ascii	"_Py_NoneStruct\000"
.LASF380:
	.ascii	"stackcheck_counter\000"
.LASF37:
	.ascii	"_flags2\000"
.LASF624:
	.ascii	"write_buffer_base\000"
.LASF541:
	.ascii	"ch4_buffer\000"
.LASF251:
	.ascii	"PyMappingMethods\000"
.LASF430:
	.ascii	"_PyWeakref_RefType\000"
.LASF272:
	.ascii	"prefix\000"
.LASF620:
	.ascii	"slice_y\000"
.LASF461:
	.ascii	"PyExc_SyntaxError\000"
.LASF574:
	.ascii	"noise\000"
.LASF510:
	.ascii	"_PyOS_ReadlineTState\000"
.LASF646:
	.ascii	"malloc_stats\000"
.LASF184:
	.ascii	"PySuper_Type\000"
.LASF348:
	.ascii	"PyCFunction_Type\000"
.LASF121:
	.ascii	"tp_flags\000"
.LASF564:
	.ascii	"ch2_color\000"
.LASF474:
	.ascii	"PyExc_ZeroDivisionError\000"
.LASF284:
	.ascii	"Py_InteractiveFlag\000"
.LASF528:
	.ascii	"__morecore\000"
.LASF59:
	.ascii	"sys_nerr\000"
.LASF305:
	.ascii	"ob_digit\000"
.LASF339:
	.ascii	"PyODictKeys_Type\000"
.LASF201:
	.ascii	"nb_subtract\000"
.LASF182:
	.ascii	"PyType_Type\000"
.LASF562:
	.ascii	"out_pixbuf\000"
.LASF481:
	.ascii	"PyExc_ConnectionResetError\000"
.LASF334:
	.ascii	"PyDictRevIterKey_Type\000"
.LASF484:
	.ascii	"PyExc_InterruptedError\000"
.LASF365:
	.ascii	"_Py_EllipsisObject\000"
.LASF439:
	.ascii	"PyExc_StopAsyncIteration\000"
.LASF317:
	.ascii	"PyRangeIter_Type\000"
.LASF625:
	.ascii	"write_buffer\000"
.LASF176:
	.ascii	"iternextfunc\000"
.LASF578:
	.ascii	"set_offset\000"
.LASF487:
	.ascii	"PyExc_PermissionError\000"
.LASF1:
	.ascii	"unsigned int\000"
.LASF420:
	.ascii	"getter\000"
.LASF470:
	.ascii	"PyExc_UnicodeEncodeError\000"
.LASF216:
	.ascii	"nb_int\000"
.LASF584:
	.ascii	"wave_size\000"
.LASF525:
	.ascii	"_Py_ctype_toupper\000"
.LASF135:
	.ascii	"tp_descr_set\000"
.LASF196:
	.ascii	"Py_buffer\000"
.LASF290:
	.ascii	"Py_IgnoreEnvironmentFlag\000"
.LASF10:
	.ascii	"short int\000"
.LASF596:
	.ascii	"wave_buffer_ptr\000"
.LASF498:
	.ascii	"PyExc_FutureWarning\000"
.LASF373:
	.ascii	"prev\000"
.LASF40:
	.ascii	"_vtable_offset\000"
.LASF376:
	.ascii	"frame\000"
.LASF235:
	.ascii	"nb_inplace_matrix_multiply\000"
.LASF104:
	.ascii	"tp_basicsize\000"
.LASF220:
	.ascii	"nb_inplace_subtract\000"
.LASF312:
	.ascii	"_Py_FalseStruct\000"
.LASF568:
	.ascii	"g_armwave_state\000"
.LASF204:
	.ascii	"nb_divmod\000"
.LASF537:
	.ascii	"flags\000"
.LASF81:
	.ascii	"tz_minuteswest\000"
.LASF163:
	.ascii	"objobjproc\000"
.LASF16:
	.ascii	"__quad_t\000"
.LASF416:
	.ascii	"PyAsyncGen_Type\000"
.LASF404:
	.ascii	"PyInterpreterState\000"
	.ident	"GCC: (Raspbian 8.3.0-6+rpi1) 8.3.0"
	.section	.note.GNU-stack,"",%progbits
