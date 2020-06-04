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
	.loc 1 45 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 46 5 view .LVU1
	.loc 1 47 5 view .LVU2
.LVL0:
	.loc 1 49 5 view .LVU3
@ armwave.c:45: {
	.loc 1 45 1 is_stmt 0 view .LVU4
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
	ldr	r4, .L6+20	@ ivtmp.43,
@ armwave.c:50:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 50 32 view .LVU5
	vldr.32	s20, .L6+16	@ tmp128,
@ armwave.c:50:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 50 26 view .LVU6
	vldr.64	d9, .L6	@ tmp135,
@ armwave.c:50:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 50 49 view .LVU7
	vldr.64	d8, .L6+8	@ tmp131,
	add	r5, r4, #1	@ _25, ivtmp.43,
	add	r6, r4, #256	@ _26, ivtmp.43,
	rsb	r5, r5, #1	@ tmp134, _25,
.LVL1:
.L2:
	.loc 1 50 9 is_stmt 1 discriminator 3 view .LVU8
@ armwave.c:50:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 50 32 is_stmt 0 discriminator 3 view .LVU9
	add	r3, r5, r4	@ tmp125, tmp134, ivtmp.43
@ armwave.c:50:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 50 26 discriminator 3 view .LVU10
	vmov.f64	d1, d9	@, tmp135
@ armwave.c:50:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 50 32 discriminator 3 view .LVU11
	vmov	s15, r3	@ int	@ tmp125, tmp125
	vcvt.f32.s32	s15, s15	@ tmp126, tmp125
	vdiv.f32	s0, s15, s20	@ tmp127, tmp126, tmp128
@ armwave.c:50:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 50 26 discriminator 3 view .LVU12
	vcvt.f64.f32	d0, s0	@, tmp127
	bl	pow		@
.LVL2:
@ armwave.c:50:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 50 49 discriminator 3 view .LVU13
	vmul.f64	d0, d0, d8	@ tmp130,, tmp131
@ armwave.c:50:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 50 24 discriminator 3 view .LVU14
	vcvt.u32.f64	s15, d0	@ tmp132, tmp130
	vmov	r3, s15	@ int	@ tmp132, tmp132
	strb	r3, [r4, #1]!	@ tmp132, MEM[base: _24, offset: 0B]
.LVL3:
@ armwave.c:49:     for(i = 0; i < 256; i++) {
	.loc 1 49 5 discriminator 3 view .LVU15
	cmp	r4, r6	@ ivtmp.43, _26
	bne	.L2		@,
@ armwave.c:52: }
	.loc 1 52 1 view .LVU16
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
	.loc 1 52 1 view .LVU17
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
	.loc 1 58 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	.loc 1 59 5 view .LVU19
@ armwave.c:59:     g_armwave_state.flags = 0;
	.loc 1 59 27 is_stmt 0 view .LVU20
	ldr	r3, .L9	@ tmp110,
	mov	r2, #0	@ tmp111,
@ armwave.c:61:     printf("armwave version: %s\n", ARMWAVE_VER);
	.loc 1 61 5 view .LVU21
	ldr	r1, .L9+4	@,
	ldr	r0, .L9+8	@,
@ armwave.c:59:     g_armwave_state.flags = 0;
	.loc 1 59 27 view .LVU22
	str	r2, [r3]	@ tmp111, g_armwave_state.flags
	.loc 1 61 5 is_stmt 1 view .LVU23
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
	.loc 1 73 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 74 5 view .LVU25
	.loc 1 75 5 view .LVU26
	.loc 1 76 5 view .LVU27
	.loc 1 77 5 view .LVU28
	.loc 1 78 5 view .LVU29
	.loc 1 80 5 view .LVU30
@ armwave.c:73: {
	.loc 1 73 1 is_stmt 0 view .LVU31
	push	{r3, r4, r5, r6, r7, r8, r9, r10, fp, lr}	@
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	.cfi_offset 4, -36
	.cfi_offset 5, -32
	.cfi_offset 6, -28
	.cfi_offset 7, -24
	.cfi_offset 8, -20
	.cfi_offset 9, -16
	.cfi_offset 10, -12
	.cfi_offset 11, -8
	.cfi_offset 14, -4
@ armwave.c:73: {
	.loc 1 73 1 view .LVU32
	mov	r7, r0	@ slice_y, slice_y
@ armwave.c:80:     printf("ch1_buffer=0x%08x\n", g_armwave_state.ch1_buffer);
	.loc 1 80 5 view .LVU33
	ldr	r5, .L25	@ tmp247,
@ armwave.c:73: {
	.loc 1 73 1 view .LVU34
	mov	r9, r1	@ height, height
@ armwave.c:80:     printf("ch1_buffer=0x%08x\n", g_armwave_state.ch1_buffer);
	.loc 1 80 5 view .LVU35
	ldr	r0, .L25+4	@,
.LVL7:
	.loc 1 80 5 view .LVU36
	ldr	r1, [r5, #4]	@, g_armwave_state.ch1_buffer
.LVL8:
	.loc 1 80 5 view .LVU37
	bl	printf		@
.LVL9:
	.loc 1 81 5 is_stmt 1 view .LVU38
@ armwave.c:84:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 84 5 is_stmt 0 view .LVU39
	ldr	r3, [r5, #40]	@ g_armwave_state.waves, g_armwave_state.waves
@ armwave.c:81:     write_buffer_base = g_armwave_state.ch1_buffer + (slice_y * g_armwave_state.bitdepth_height);
	.loc 1 81 63 view .LVU40
	ldr	r6, [r5, #48]	@ g_armwave_state.bitdepth_height, g_armwave_state.bitdepth_height
@ armwave.c:84:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 84 5 view .LVU41
	cmp	r3, #0	@ g_armwave_state.waves,
@ armwave.c:81:     write_buffer_base = g_armwave_state.ch1_buffer + (slice_y * g_armwave_state.bitdepth_height);
	.loc 1 81 40 view .LVU42
	ldr	r4, [r5, #4]	@ _2, g_armwave_state.ch1_buffer
@ armwave.c:81:     write_buffer_base = g_armwave_state.ch1_buffer + (slice_y * g_armwave_state.bitdepth_height);
	.loc 1 81 63 view .LVU43
	mul	r6, r6, r7	@ _4, g_armwave_state.bitdepth_height, slice_y
.LVL10:
	.loc 1 84 5 is_stmt 1 view .LVU44
	.loc 1 84 5 is_stmt 0 view .LVU45
	popeq	{r3, r4, r5, r6, r7, r8, r9, r10, fp, pc}	@
@ armwave.c:85:         printf("wave_buffer=0x%08x\n", g_armwave_state.wave_buffer);
	.loc 1 85 9 view .LVU46
	ldr	r8, .L25+8	@ tmp249,
	sub	r7, r7, #4	@ _36, slice_y,
.LVL11:
@ armwave.c:84:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 84 11 view .LVU47
	mov	r10, #0	@ w,
.LVL12:
.L15:
	.loc 1 85 9 is_stmt 1 view .LVU48
	ldr	r1, [r5, #20]	@, g_armwave_state.wave_buffer
	mov	r0, r8	@, tmp249
	bl	printf		@
.LVL13:
	.loc 1 86 9 view .LVU49
@ armwave.c:86:         wave_base = g_armwave_state.wave_buffer + slice_y + (w * g_armwave_state.wave_stride);
	.loc 1 86 64 is_stmt 0 view .LVU50
	ldr	r3, [r5, #36]	@ g_armwave_state.wave_stride, g_armwave_state.wave_stride
@ armwave.c:90:         for(yy = 0; yy < height; yy += 4) {
	.loc 1 90 9 view .LVU51
	cmp	r9, #0	@ height,
@ armwave.c:86:         wave_base = g_armwave_state.wave_buffer + slice_y + (w * g_armwave_state.wave_stride);
	.loc 1 86 36 view .LVU52
	ldr	ip, [r5, #20]	@ _6, g_armwave_state.wave_buffer
@ armwave.c:86:         wave_base = g_armwave_state.wave_buffer + slice_y + (w * g_armwave_state.wave_stride);
	.loc 1 86 64 view .LVU53
	mul	r3, r3, r10	@ _9, g_armwave_state.wave_stride, w
.LVL14:
	.loc 1 90 9 is_stmt 1 view .LVU54
	.loc 1 90 9 is_stmt 0 view .LVU55
	beq	.L13		@,
	add	r3, r3, r7	@ tmp199, _9, _36
.LVL15:
	.loc 1 90 9 view .LVU56
	add	ip, ip, r3	@ ivtmp.50, _6, tmp199
.LVL16:
@ armwave.c:90:         for(yy = 0; yy < height; yy += 4) {
	.loc 1 90 16 view .LVU57
	mov	r3, #0	@ yy,
.LVL17:
.L14:
	.loc 1 91 13 is_stmt 1 view .LVU58
@ armwave.c:91:             word = *(uint32_t*)(wave_base + yy);
	.loc 1 91 18 is_stmt 0 view .LVU59
	ldr	r2, [ip, #4]!	@ word, MEM[base: _149, offset: 0B]
.LVL18:
	.loc 1 93 13 is_stmt 1 view .LVU60
	.loc 1 95 17 view .LVU61
	.loc 1 96 17 view .LVU62
	.loc 1 97 17 view .LVU63
@ armwave.c:96:                 write_buffer = write_buffer_base + ((yy + ys) * g_armwave_state.bitdepth_height);
	.loc 1 96 63 is_stmt 0 view .LVU64
	ldr	r0, [r5, #48]	@ g_armwave_state.bitdepth_height, g_armwave_state.bitdepth_height
	add	fp, r3, #1	@ tmp211, yy,
@ armwave.c:97:                 *(write_buffer + scale_value) += 1;
	.loc 1 97 47 view .LVU65
	uxtab	r1, r6, r2	@ tmp204, _4, word
	mla	r0, r0, r3, r1	@ tmp205, g_armwave_state.bitdepth_height, yy, tmp204
@ armwave.c:98:                 word >>= 8;
	.loc 1 98 22 view .LVU66
	lsr	lr, r2, #8	@ word, word,
@ armwave.c:97:                 *(write_buffer + scale_value) += 1;
	.loc 1 97 47 view .LVU67
	ldrb	r1, [r4, r0]	@ zero_extendqisi2	@ *_59, *_59
	uxtab	lr, r6, lr	@ tmp216, _4, word
	add	r1, r1, #1	@ tmp208, *_59,
	strb	r1, [r4, r0]	@ tmp208, *_59
.LVL19:
	.loc 1 98 17 is_stmt 1 view .LVU68
	.loc 1 95 17 view .LVU69
	.loc 1 96 17 view .LVU70
	.loc 1 97 17 view .LVU71
@ armwave.c:96:                 write_buffer = write_buffer_base + ((yy + ys) * g_armwave_state.bitdepth_height);
	.loc 1 96 63 is_stmt 0 view .LVU72
	ldr	r1, [r5, #48]	@ g_armwave_state.bitdepth_height, g_armwave_state.bitdepth_height
@ armwave.c:98:                 word >>= 8;
	.loc 1 98 22 view .LVU73
	lsr	r0, r2, #16	@ word, word,
@ armwave.c:97:                 *(write_buffer + scale_value) += 1;
	.loc 1 97 47 view .LVU74
	add	r2, r6, r2, lsr #24	@ tmp239, _4, word,
.LVL20:
	.loc 1 97 47 view .LVU75
	mla	r1, r1, fp, lr	@ tmp217, g_armwave_state.bitdepth_height, tmp211, tmp216
	uxtab	lr, r6, r0	@ tmp228, _4, word
	ldrb	fp, [r4, r1]	@ zero_extendqisi2	@ *_79, *_79
@ armwave.c:96:                 write_buffer = write_buffer_base + ((yy + ys) * g_armwave_state.bitdepth_height);
	.loc 1 96 63 view .LVU76
	add	r0, r3, #2	@ tmp223, yy,
@ armwave.c:97:                 *(write_buffer + scale_value) += 1;
	.loc 1 97 47 view .LVU77
	add	fp, fp, #1	@ tmp220, *_79,
	strb	fp, [r4, r1]	@ tmp220, *_79
.LVL21:
	.loc 1 98 17 is_stmt 1 view .LVU78
	.loc 1 95 17 view .LVU79
	.loc 1 96 17 view .LVU80
	.loc 1 97 17 view .LVU81
@ armwave.c:96:                 write_buffer = write_buffer_base + ((yy + ys) * g_armwave_state.bitdepth_height);
	.loc 1 96 63 is_stmt 0 view .LVU82
	ldr	fp, [r5, #48]	@ g_armwave_state.bitdepth_height, g_armwave_state.bitdepth_height
	add	r1, r3, #3	@ tmp235, yy,
@ armwave.c:90:         for(yy = 0; yy < height; yy += 4) {
	.loc 1 90 37 view .LVU83
	add	r3, r3, #4	@ yy, yy,
.LVL22:
@ armwave.c:97:                 *(write_buffer + scale_value) += 1;
	.loc 1 97 47 view .LVU84
	mla	r0, fp, r0, lr	@ tmp229, g_armwave_state.bitdepth_height, tmp223, tmp228
@ armwave.c:90:         for(yy = 0; yy < height; yy += 4) {
	.loc 1 90 9 view .LVU85
	cmp	r9, r3	@ height, yy
@ armwave.c:97:                 *(write_buffer + scale_value) += 1;
	.loc 1 97 47 view .LVU86
	ldrb	lr, [r4, r0]	@ zero_extendqisi2	@ *_99, *_99
	add	lr, lr, #1	@ tmp232, *_99,
	strb	lr, [r4, r0]	@ tmp232, *_99
.LVL23:
	.loc 1 98 17 is_stmt 1 view .LVU87
	.loc 1 95 17 view .LVU88
	.loc 1 96 17 view .LVU89
	.loc 1 97 17 view .LVU90
@ armwave.c:96:                 write_buffer = write_buffer_base + ((yy + ys) * g_armwave_state.bitdepth_height);
	.loc 1 96 63 is_stmt 0 view .LVU91
	ldr	r0, [r5, #48]	@ g_armwave_state.bitdepth_height, g_armwave_state.bitdepth_height
@ armwave.c:97:                 *(write_buffer + scale_value) += 1;
	.loc 1 97 47 view .LVU92
	mla	r2, r0, r1, r2	@ tmp240, g_armwave_state.bitdepth_height, tmp235, tmp239
	ldrb	r1, [r4, r2]	@ zero_extendqisi2	@ *_119, *_119
	add	r1, r1, #1	@ tmp243, *_119,
	strb	r1, [r4, r2]	@ tmp243, *_119
	.loc 1 98 17 is_stmt 1 view .LVU93
.LVL24:
@ armwave.c:90:         for(yy = 0; yy < height; yy += 4) {
	.loc 1 90 9 is_stmt 0 view .LVU94
	bhi	.L14		@,
.LVL25:
.L13:
@ armwave.c:84:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 84 5 view .LVU95
	ldr	r3, [r5, #40]	@ g_armwave_state.waves, g_armwave_state.waves
@ armwave.c:84:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 84 44 view .LVU96
	add	r10, r10, #1	@ w, w,
.LVL26:
@ armwave.c:84:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 84 5 view .LVU97
	cmp	r3, r10	@ g_armwave_state.waves, w
	bhi	.L15		@,
	pop	{r3, r4, r5, r6, r7, r8, r9, r10, fp, pc}	@
.L26:
	.align	2
.L25:
	.word	g_armwave_state
	.word	.LC2
	.word	.LC3
	.cfi_endproc
.LFE59:
	.size	render_nonaa_to_buffer_1ch_slice, .-render_nonaa_to_buffer_1ch_slice
	.align	2
	.global	armwave_fill_pixbuf_256
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_fill_pixbuf_256, %function
armwave_fill_pixbuf_256:
.LVL27:
.LFB60:
	.loc 1 108 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 109 5 view .LVU99
	.loc 1 110 5 view .LVU100
	.loc 1 111 5 view .LVU101
	.loc 1 112 5 view .LVU102
@ armwave.c:108: {
	.loc 1 108 1 is_stmt 0 view .LVU103
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
@ armwave.c:116:     assert(out_buffer != NULL);
	.loc 1 116 5 view .LVU104
	cmp	r0, #0	@ out_buffer
@ armwave.c:112:     uint32_t *base_32ptr = (uint32_t*)g_armwave_state.ch1_buffer;
	.loc 1 112 15 view .LVU105
	ldr	r5, .L54	@ tmp360,
@ armwave.c:108: {
	.loc 1 108 1 view .LVU106
	sub	sp, sp, #12	@,,
	.cfi_def_cfa_offset 48
@ armwave.c:112:     uint32_t *base_32ptr = (uint32_t*)g_armwave_state.ch1_buffer;
	.loc 1 112 15 view .LVU107
	ldr	r1, [r5, #4]	@ base_32ptr, g_armwave_state.ch1_buffer
.LVL28:
	.loc 1 113 5 is_stmt 1 view .LVU108
	.loc 1 114 5 view .LVU109
	.loc 1 116 5 view .LVU110
	beq	.L52		@,
	.loc 1 120 5 view .LVU111
@ armwave.c:120:     npix = g_armwave_state.target_width * 256;
	.loc 1 120 41 is_stmt 0 view .LVU112
	ldr	ip, [r5, #72]	@ g_armwave_state.target_width, g_armwave_state.target_width
	lsl	ip, ip, #8	@ npix, g_armwave_state.target_width,
.LVL29:
	.loc 1 122 5 is_stmt 1 view .LVU113
	.loc 1 122 5 is_stmt 0 view .LVU114
	cmp	ip, #0	@ npix,
	ble	.L27		@,
@ armwave.c:134:                     rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 134 52 view .LVU115
	ldrsh	r10, [r5, #92]	@ _70, g_armwave_state.ch1_color.r
@ armwave.c:135:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 135 52 view .LVU116
	ldrsh	r9, [r5, #94]	@ _76, g_armwave_state.ch1_color.g
@ armwave.c:136:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 136 52 view .LVU117
	ldrsh	fp, [r5, #96]	@ _80, g_armwave_state.ch1_color.b
@ armwave.c:122:     for(n = 0; n < npix; n += 4) {
	.loc 1 122 11 view .LVU118
	mov	r3, #0	@ n,
	str	r0, [sp]	@ out_buffer, %sfp
.LVL30:
.L36:
	.loc 1 126 9 is_stmt 1 view .LVU119
@ armwave.c:126:         wave_word = *base_32ptr++;
	.loc 1 126 19 is_stmt 0 view .LVU120
	ldr	r2, [r1], #4	@ wave_word, MEM[base: base_32ptr_46, offset: 4294967292B]
.LVL31:
	.loc 1 128 9 is_stmt 1 view .LVU121
@ armwave.c:128:         if(COND_UNLIKELY(wave_word != 0)) {
	.loc 1 128 11 is_stmt 0 view .LVU122
	cmp	r2, #0	@ wave_word,
	bne	.L53		@,
.LVL32:
.L31:
@ armwave.c:122:     for(n = 0; n < npix; n += 4) {
	.loc 1 122 28 discriminator 2 view .LVU123
	add	r3, r3, #4	@ n, n,
.LVL33:
@ armwave.c:122:     for(n = 0; n < npix; n += 4) {
	.loc 1 122 5 discriminator 2 view .LVU124
	cmp	ip, r3	@ npix, n
	bgt	.L36		@,
.LVL34:
.L27:
@ armwave.c:155: }
	.loc 1 155 1 view .LVU125
	add	sp, sp, #12	@,,
	.cfi_remember_state
	.cfi_def_cfa_offset 36
	@ sp needed	@
	pop	{r4, r5, r6, r7, r8, r9, r10, fp, pc}	@
.LVL35:
.L53:
	.cfi_restore_state
	.loc 1 130 17 is_stmt 1 view .LVU126
	.loc 1 131 17 view .LVU127
@ armwave.c:133:                 if(value != 0) {
	.loc 1 133 19 is_stmt 0 view .LVU128
	ands	r6, r2, #255	@ _72, wave_word,
@ armwave.c:131:                 wave_word >>= 8;
	.loc 1 131 27 view .LVU129
	lsr	lr, r2, #8	@ wave_word, wave_word,
.LVL36:
	.loc 1 133 17 is_stmt 1 view .LVU130
@ armwave.c:133:                 if(value != 0) {
	.loc 1 133 19 is_stmt 0 view .LVU131
	beq	.L32		@,
	.loc 1 134 21 is_stmt 1 view .LVU132
.LVL37:
	.loc 1 135 21 view .LVU133
	.loc 1 136 21 view .LVU134
	.loc 1 138 21 view .LVU135
	.loc 1 139 21 view .LVU136
	.loc 1 140 21 view .LVU137
	.loc 1 143 21 view .LVU138
	.loc 1 146 21 view .LVU139
	.loc 1 147 21 view .LVU140
	.loc 1 148 21 view .LVU141
	.loc 1 149 21 view .LVU142
	.loc 1 150 21 view .LVU143
@ armwave.c:136:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 136 55 is_stmt 0 view .LVU144
	mul	r4, fp, r6	@ tmp276, _80, _72
@ armwave.c:135:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 135 55 view .LVU145
	mul	r8, r9, r6	@ tmp280, _76, _72
@ armwave.c:136:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 136 24 view .LVU146
	asr	r4, r4, #8	@ bb, tmp276,
.LVL38:
@ armwave.c:140:                     b = MIN(bb, 255);
	.loc 1 140 25 view .LVU147
	cmp	r4, #255	@ bb,
@ armwave.c:135:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 135 24 view .LVU148
	asr	r8, r8, #8	@ gg, tmp280,
.LVL39:
@ armwave.c:140:                     b = MIN(bb, 255);
	.loc 1 140 25 view .LVU149
	movge	r4, #255	@ bb,
.LVL40:
@ armwave.c:134:                     rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 134 55 view .LVU150
	mul	r6, r6, r10	@ tmp286, _72, _70
.LVL41:
@ armwave.c:139:                     g = MIN(gg, 255);
	.loc 1 139 25 view .LVU151
	cmp	r8, #255	@ gg,
	movge	r8, #255	@ gg,
.LVL42:
@ armwave.c:134:                     rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 134 24 view .LVU152
	asr	r6, r6, #8	@ rr, tmp286,
.LVL43:
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 56 view .LVU153
	lsl	r8, r8, #8	@ tmp281, gg,
@ armwave.c:150:                     *(out_buffer_base + offset) = word;
	.loc 1 150 39 view .LVU154
	ldr	r0, [r5, #72]	@ tmp372, g_armwave_state.target_width
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 44 view .LVU155
	lsl	r4, r4, #16	@ tmp277, bb,
@ armwave.c:138:                     r = MIN(rr, 255);
	.loc 1 138 25 view .LVU156
	cmp	r6, #255	@ rr,
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 56 view .LVU157
	uxth	r8, r8	@ tmp282, tmp281
@ armwave.c:147:                     xx = nsub & 0xff;
	.loc 1 147 24 view .LVU158
	uxtb	r7, r3	@ xx, n
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 44 view .LVU159
	and	r4, r4, #16711680	@ tmp278, tmp277,
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 51 view .LVU160
	orr	r4, r4, r8	@ tmp284, tmp278, tmp282
@ armwave.c:138:                     r = MIN(rr, 255);
	.loc 1 138 25 view .LVU161
	movge	r6, #255	@ rr,
.LVL44:
@ armwave.c:148:                     yy = nsub >> 8;
	.loc 1 148 31 view .LVU162
	asr	r8, r3, #8	@ yy, n,
.LVL45:
@ armwave.c:150:                     *(out_buffer_base + offset) = word;
	.loc 1 150 39 view .LVU163
	mla	r7, r0, r7, r8	@ tmp274, tmp372, xx, yy
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 62 view .LVU164
	uxtb	r6, r6	@ rr, rr
@ armwave.c:150:                     *(out_buffer_base + offset) = word;
	.loc 1 150 49 view .LVU165
	ldr	r0, [sp]	@ out_buffer, %sfp
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 62 view .LVU166
	orr	r6, r4, r6	@ tmp288, tmp284, rr
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 26 view .LVU167
	orr	r6, r6, #-16777216	@ word, tmp288,
@ armwave.c:150:                     *(out_buffer_base + offset) = word;
	.loc 1 150 49 view .LVU168
	str	r6, [r0, r7, lsl #2]	@ word, *_106
.LVL46:
.L32:
	.loc 1 130 17 is_stmt 1 view .LVU169
	.loc 1 131 17 view .LVU170
@ armwave.c:133:                 if(value != 0) {
	.loc 1 133 19 is_stmt 0 view .LVU171
	ands	lr, lr, #255	@ _118, wave_word,
.LVL47:
@ armwave.c:131:                 wave_word >>= 8;
	.loc 1 131 27 view .LVU172
	lsr	r8, r2, #16	@ wave_word, wave_word,
.LVL48:
	.loc 1 133 17 is_stmt 1 view .LVU173
@ armwave.c:133:                 if(value != 0) {
	.loc 1 133 19 is_stmt 0 view .LVU174
	beq	.L33		@,
	.loc 1 134 21 is_stmt 1 view .LVU175
.LVL49:
	.loc 1 135 21 view .LVU176
	.loc 1 136 21 view .LVU177
	.loc 1 138 21 view .LVU178
	.loc 1 139 21 view .LVU179
	.loc 1 140 21 view .LVU180
	.loc 1 143 21 view .LVU181
	.loc 1 146 21 view .LVU182
@ armwave.c:136:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 136 55 is_stmt 0 view .LVU183
	mul	r4, lr, fp	@ tmp299, _118, _80
@ armwave.c:135:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 135 55 view .LVU184
	mul	r7, lr, r9	@ tmp303, _118, _76
@ armwave.c:136:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 136 24 view .LVU185
	asr	r4, r4, #8	@ bb, tmp299,
.LVL50:
@ armwave.c:140:                     b = MIN(bb, 255);
	.loc 1 140 25 view .LVU186
	cmp	r4, #255	@ bb,
	movge	r4, #255	@ bb,
.LVL51:
@ armwave.c:134:                     rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 134 55 view .LVU187
	mul	lr, lr, r10	@ tmp309, _118, _70
.LVL52:
@ armwave.c:135:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 135 24 view .LVU188
	asr	r7, r7, #8	@ gg, tmp303,
.LVL53:
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 44 view .LVU189
	lsl	r4, r4, #16	@ tmp300, bb,
@ armwave.c:139:                     g = MIN(gg, 255);
	.loc 1 139 25 view .LVU190
	cmp	r7, #255	@ gg,
	add	r6, r3, #1	@ _158, n,
	.loc 1 147 21 is_stmt 1 view .LVU191
	.loc 1 148 21 view .LVU192
	.loc 1 149 21 view .LVU193
	.loc 1 150 21 view .LVU194
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 44 is_stmt 0 view .LVU195
	and	r4, r4, #16711680	@ tmp301, tmp300,
@ armwave.c:139:                     g = MIN(gg, 255);
	.loc 1 139 25 view .LVU196
	movge	r7, #255	@ gg,
.LVL54:
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 44 view .LVU197
	str	r4, [sp, #4]	@ tmp301, %sfp
@ armwave.c:150:                     *(out_buffer_base + offset) = word;
	.loc 1 150 39 view .LVU198
	ldr	r4, [r5, #72]	@ tmp375, g_armwave_state.target_width
@ armwave.c:134:                     rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 134 24 view .LVU199
	asr	lr, lr, #8	@ rr, tmp309,
.LVL55:
@ armwave.c:147:                     xx = nsub & 0xff;
	.loc 1 147 24 view .LVU200
	uxtb	r0, r6	@ xx, _158
@ armwave.c:138:                     r = MIN(rr, 255);
	.loc 1 138 25 view .LVU201
	cmp	lr, #255	@ rr,
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 56 view .LVU202
	lsl	r7, r7, #8	@ tmp304, gg,
@ armwave.c:148:                     yy = nsub >> 8;
	.loc 1 148 31 view .LVU203
	asr	r6, r6, #8	@ yy, _158,
@ armwave.c:150:                     *(out_buffer_base + offset) = word;
	.loc 1 150 39 view .LVU204
	mla	r6, r4, r0, r6	@ tmp297, tmp375, xx, yy
@ armwave.c:138:                     r = MIN(rr, 255);
	.loc 1 138 25 view .LVU205
	movge	lr, #255	@ rr,
.LVL56:
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 51 view .LVU206
	ldr	r0, [sp, #4]	@ tmp301, %sfp
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 56 view .LVU207
	uxth	r7, r7	@ tmp305, tmp304
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 51 view .LVU208
	orr	r4, r0, r7	@ tmp307, tmp301, tmp305
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 62 view .LVU209
	uxtb	lr, lr	@ rr, rr
@ armwave.c:150:                     *(out_buffer_base + offset) = word;
	.loc 1 150 49 view .LVU210
	ldr	r0, [sp]	@ out_buffer, %sfp
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 62 view .LVU211
	orr	lr, r4, lr	@ tmp311, tmp307, rr
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 26 view .LVU212
	orr	lr, lr, #-16777216	@ word, tmp311,
@ armwave.c:150:                     *(out_buffer_base + offset) = word;
	.loc 1 150 49 view .LVU213
	str	lr, [r0, r6, lsl #2]	@ word, *_152
.LVL57:
.L33:
	.loc 1 130 17 is_stmt 1 view .LVU214
	.loc 1 131 17 view .LVU215
@ armwave.c:133:                 if(value != 0) {
	.loc 1 133 19 is_stmt 0 view .LVU216
	ands	lr, r8, #255	@ _164, wave_word,
@ armwave.c:131:                 wave_word >>= 8;
	.loc 1 131 27 view .LVU217
	lsr	r2, r2, #24	@ wave_word, wave_word,
.LVL58:
	.loc 1 133 17 is_stmt 1 view .LVU218
@ armwave.c:133:                 if(value != 0) {
	.loc 1 133 19 is_stmt 0 view .LVU219
	beq	.L34		@,
	.loc 1 134 21 is_stmt 1 view .LVU220
.LVL59:
	.loc 1 135 21 view .LVU221
	.loc 1 136 21 view .LVU222
	.loc 1 138 21 view .LVU223
	.loc 1 139 21 view .LVU224
	.loc 1 140 21 view .LVU225
	.loc 1 143 21 view .LVU226
	.loc 1 146 21 view .LVU227
@ armwave.c:136:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 136 55 is_stmt 0 view .LVU228
	mul	r4, lr, fp	@ tmp322, _164, _80
@ armwave.c:135:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 135 55 view .LVU229
	mul	r6, lr, r9	@ tmp326, _164, _76
@ armwave.c:136:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 136 24 view .LVU230
	asr	r4, r4, #8	@ bb, tmp322,
.LVL60:
@ armwave.c:134:                     rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 134 55 view .LVU231
	mul	lr, lr, r10	@ tmp332, _164, _70
.LVL61:
@ armwave.c:140:                     b = MIN(bb, 255);
	.loc 1 140 25 view .LVU232
	cmp	r4, #255	@ bb,
@ armwave.c:135:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 135 24 view .LVU233
	asr	r6, r6, #8	@ gg, tmp326,
.LVL62:
@ armwave.c:140:                     b = MIN(bb, 255);
	.loc 1 140 25 view .LVU234
	movge	r4, #255	@ bb,
.LVL63:
@ armwave.c:139:                     g = MIN(gg, 255);
	.loc 1 139 25 view .LVU235
	cmp	r6, #255	@ gg,
	movge	r6, #255	@ gg,
.LVL64:
	.loc 1 139 25 view .LVU236
	add	r7, r3, #2	@ _66, n,
	.loc 1 147 21 is_stmt 1 view .LVU237
	.loc 1 148 21 view .LVU238
	.loc 1 149 21 view .LVU239
	.loc 1 150 21 view .LVU240
@ armwave.c:134:                     rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 134 24 is_stmt 0 view .LVU241
	asr	lr, lr, #8	@ rr, tmp332,
.LVL65:
@ armwave.c:150:                     *(out_buffer_base + offset) = word;
	.loc 1 150 39 view .LVU242
	ldr	r0, [r5, #72]	@ tmp376, g_armwave_state.target_width
@ armwave.c:138:                     r = MIN(rr, 255);
	.loc 1 138 25 view .LVU243
	cmp	lr, #255	@ rr,
@ armwave.c:147:                     xx = nsub & 0xff;
	.loc 1 147 24 view .LVU244
	uxtb	r8, r7	@ xx, _66
.LVL66:
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 56 view .LVU245
	lsl	r6, r6, #8	@ tmp327, gg,
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 44 view .LVU246
	lsl	r4, r4, #16	@ tmp323, bb,
@ armwave.c:138:                     r = MIN(rr, 255);
	.loc 1 138 25 view .LVU247
	movge	lr, #255	@ rr,
.LVL67:
@ armwave.c:148:                     yy = nsub >> 8;
	.loc 1 148 31 view .LVU248
	asr	r7, r7, #8	@ yy, _66,
@ armwave.c:150:                     *(out_buffer_base + offset) = word;
	.loc 1 150 39 view .LVU249
	mla	r7, r0, r8, r7	@ tmp320, tmp376, xx, yy
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 44 view .LVU250
	and	r4, r4, #16711680	@ tmp324, tmp323,
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 56 view .LVU251
	uxth	r6, r6	@ tmp328, tmp327
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 51 view .LVU252
	orr	r6, r4, r6	@ tmp330, tmp324, tmp328
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 62 view .LVU253
	uxtb	lr, lr	@ rr, rr
@ armwave.c:150:                     *(out_buffer_base + offset) = word;
	.loc 1 150 49 view .LVU254
	ldr	r0, [sp]	@ out_buffer, %sfp
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 62 view .LVU255
	orr	lr, r6, lr	@ tmp334, tmp330, rr
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 26 view .LVU256
	orr	lr, lr, #-16777216	@ word, tmp334,
@ armwave.c:150:                     *(out_buffer_base + offset) = word;
	.loc 1 150 49 view .LVU257
	str	lr, [r0, r7, lsl #2]	@ word, *_198
.L34:
.LVL68:
	.loc 1 130 17 is_stmt 1 view .LVU258
	.loc 1 131 17 view .LVU259
	.loc 1 133 17 view .LVU260
@ armwave.c:133:                 if(value != 0) {
	.loc 1 133 19 is_stmt 0 view .LVU261
	cmp	r2, #0	@ wave_word,
	beq	.L31		@,
	.loc 1 134 21 is_stmt 1 view .LVU262
.LVL69:
	.loc 1 135 21 view .LVU263
	.loc 1 136 21 view .LVU264
	.loc 1 138 21 view .LVU265
	.loc 1 139 21 view .LVU266
	.loc 1 140 21 view .LVU267
	.loc 1 143 21 view .LVU268
	.loc 1 146 21 view .LVU269
@ armwave.c:136:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 136 55 is_stmt 0 view .LVU270
	mul	lr, r2, fp	@ tmp343, wave_word, _80
@ armwave.c:135:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 135 55 view .LVU271
	mul	r4, r2, r9	@ tmp347, wave_word, _76
@ armwave.c:136:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 136 24 view .LVU272
	asr	lr, lr, #8	@ bb, tmp343,
.LVL70:
@ armwave.c:134:                     rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 134 55 view .LVU273
	mul	r2, r2, r10	@ tmp353, wave_word, _70
.LVL71:
@ armwave.c:140:                     b = MIN(bb, 255);
	.loc 1 140 25 view .LVU274
	cmp	lr, #255	@ bb,
@ armwave.c:135:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 135 24 view .LVU275
	asr	r4, r4, #8	@ gg, tmp347,
.LVL72:
@ armwave.c:140:                     b = MIN(bb, 255);
	.loc 1 140 25 view .LVU276
	movge	lr, #255	@ bb,
.LVL73:
@ armwave.c:139:                     g = MIN(gg, 255);
	.loc 1 139 25 view .LVU277
	cmp	r4, #255	@ gg,
	movge	r4, #255	@ gg,
.LVL74:
	.loc 1 139 25 view .LVU278
	add	r6, r3, #3	@ _59, n,
	.loc 1 147 21 is_stmt 1 view .LVU279
	.loc 1 148 21 view .LVU280
	.loc 1 149 21 view .LVU281
	.loc 1 150 21 view .LVU282
@ armwave.c:134:                     rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 134 24 is_stmt 0 view .LVU283
	asr	r2, r2, #8	@ rr, tmp353,
.LVL75:
@ armwave.c:149:                     offset = yy + (xx * g_armwave_state.target_width);
	.loc 1 149 39 view .LVU284
	ldr	r7, [r5, #72]	@ g_armwave_state.target_width, g_armwave_state.target_width
@ armwave.c:138:                     r = MIN(rr, 255);
	.loc 1 138 25 view .LVU285
	cmp	r2, #255	@ rr,
@ armwave.c:147:                     xx = nsub & 0xff;
	.loc 1 147 24 view .LVU286
	uxtb	r8, r6	@ xx, _59
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 56 view .LVU287
	lsl	r4, r4, #8	@ tmp348, gg,
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 44 view .LVU288
	lsl	lr, lr, #16	@ tmp344, bb,
@ armwave.c:138:                     r = MIN(rr, 255);
	.loc 1 138 25 view .LVU289
	movge	r2, #255	@ rr,
.LVL76:
@ armwave.c:148:                     yy = nsub >> 8;
	.loc 1 148 31 view .LVU290
	asr	r6, r6, #8	@ yy, _59,
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 44 view .LVU291
	and	lr, lr, #16711680	@ tmp345, tmp344,
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 56 view .LVU292
	uxth	r4, r4	@ tmp349, tmp348
@ armwave.c:150:                     *(out_buffer_base + offset) = word;
	.loc 1 150 39 view .LVU293
	mla	r6, r7, r8, r6	@ tmp341, g_armwave_state.target_width, xx, yy
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 51 view .LVU294
	orr	lr, lr, r4	@ tmp351, tmp345, tmp349
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 62 view .LVU295
	uxtb	r2, r2	@ rr, rr
@ armwave.c:150:                     *(out_buffer_base + offset) = word;
	.loc 1 150 49 view .LVU296
	ldr	r0, [sp]	@ out_buffer, %sfp
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 62 view .LVU297
	orr	r2, lr, r2	@ tmp355, tmp351, rr
@ armwave.c:143:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 143 26 view .LVU298
	orr	r2, r2, #-16777216	@ word, tmp355,
@ armwave.c:150:                     *(out_buffer_base + offset) = word;
	.loc 1 150 49 view .LVU299
	str	r2, [r0, r6, lsl #2]	@ word, *_244
	b	.L31		@
.LVL77:
.L52:
	.loc 1 116 5 is_stmt 1 discriminator 1 view .LVU300
	ldr	r3, .L54+4	@,
	mov	r2, #116	@,
	ldr	r1, .L54+8	@,
.LVL78:
	.loc 1 116 5 is_stmt 0 discriminator 1 view .LVU301
	ldr	r0, .L54+12	@,
.LVL79:
	.loc 1 116 5 discriminator 1 view .LVU302
	bl	__assert_fail		@
.LVL80:
.L55:
	.loc 1 116 5 discriminator 1 view .LVU303
	.align	2
.L54:
	.word	g_armwave_state
	.word	.LANCHOR0
	.word	.LC4
	.word	.LC5
	.cfi_endproc
.LFE60:
	.size	armwave_fill_pixbuf_256, .-armwave_fill_pixbuf_256
	.align	2
	.global	armwave_fill_pixbuf_scaled
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_fill_pixbuf_scaled, %function
armwave_fill_pixbuf_scaled:
.LVL81:
.LFB61:
	.loc 1 162 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 163 5 view .LVU305
	.loc 1 164 5 view .LVU306
	.loc 1 165 5 view .LVU307
	.loc 1 166 5 view .LVU308
@ armwave.c:162: {
	.loc 1 162 1 is_stmt 0 view .LVU309
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
@ armwave.c:170:     assert(out_buffer != NULL);
	.loc 1 170 5 view .LVU310
	cmp	r0, #0	@ out_buffer
@ armwave.c:166:     uint32_t *base_32ptr = (uint32_t*)g_armwave_state.ch1_buffer;
	.loc 1 166 15 view .LVU311
	ldr	r5, .L81	@ tmp202,
@ armwave.c:162: {
	.loc 1 162 1 view .LVU312
	sub	sp, sp, #12	@,,
	.cfi_def_cfa_offset 48
@ armwave.c:166:     uint32_t *base_32ptr = (uint32_t*)g_armwave_state.ch1_buffer;
	.loc 1 166 15 view .LVU313
	ldr	r6, [r5, #4]	@ base_32ptr, g_armwave_state.ch1_buffer
.LVL82:
	.loc 1 167 5 is_stmt 1 view .LVU314
	.loc 1 168 5 view .LVU315
	.loc 1 170 5 view .LVU316
	beq	.L78		@,
	.loc 1 172 5 view .LVU317
@ armwave.c:172:     npix = g_armwave_state.target_width * 256; 
	.loc 1 172 41 is_stmt 0 view .LVU318
	ldr	r8, [r5, #72]	@ g_armwave_state.target_width, g_armwave_state.target_width
	lsl	r8, r8, #8	@ npix, g_armwave_state.target_width,
.LVL83:
	.loc 1 173 5 is_stmt 1 view .LVU319
	.loc 1 175 5 view .LVU320
	.loc 1 175 5 is_stmt 0 view .LVU321
	cmp	r8, #0	@ npix,
	ble	.L56		@,
@ armwave.c:188:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 188 52 view .LVU322
	ldrsh	r3, [r5, #94]	@ _11, g_armwave_state.ch1_color.g
@ armwave.c:187:                     rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 187 52 view .LVU323
	ldrsh	fp, [r5, #92]	@ _7, g_armwave_state.ch1_color.r
@ armwave.c:175:     for(n = 0; n < npix; n += 4) {
	.loc 1 175 11 view .LVU324
	mov	lr, #0	@ n,
@ armwave.c:188:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 188 52 view .LVU325
	str	r3, [sp]	@ _11, %sfp
@ armwave.c:189:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 189 52 view .LVU326
	ldrsh	r3, [r5, #96]	@ _14, g_armwave_state.ch1_color.b
	str	r3, [sp, #4]	@ _14, %sfp
.LVL84:
.L63:
	.loc 1 179 9 is_stmt 1 view .LVU327
@ armwave.c:179:         wave_word = *base_32ptr++;
	.loc 1 179 19 is_stmt 0 view .LVU328
	ldr	ip, [r6], #4	@ wave_word, MEM[base: base_32ptr_53, offset: 4294967292B]
.LVL85:
	.loc 1 181 9 is_stmt 1 view .LVU329
@ armwave.c:181:         if(COND_UNLIKELY(wave_word != 0)) {
	.loc 1 181 11 is_stmt 0 view .LVU330
	cmp	ip, #0	@ wave_word,
	bne	.L79		@,
.L59:
@ armwave.c:175:     for(n = 0; n < npix; n += 4) {
	.loc 1 175 28 view .LVU331
	add	lr, lr, #4	@ n, n,
.LVL86:
@ armwave.c:175:     for(n = 0; n < npix; n += 4) {
	.loc 1 175 5 view .LVU332
	cmp	r8, lr	@ npix, n
	bgt	.L63		@,
.LVL87:
.L56:
@ armwave.c:211: }
	.loc 1 211 1 view .LVU333
	add	sp, sp, #12	@,,
	.cfi_remember_state
	.cfi_def_cfa_offset 36
	@ sp needed	@
	pop	{r4, r5, r6, r7, r8, r9, r10, fp, pc}	@
.LVL88:
.L79:
	.cfi_restore_state
@ armwave.c:186:                 if(value != 0) {
	.loc 1 186 19 view .LVU334
	ands	r3, ip, #255	@ _8, wave_word,
@ armwave.c:182:             for(w = 0; w < 4; w++) {
	.loc 1 182 19 view .LVU335
	mov	r9, #0	@ w,
.LVL89:
	.loc 1 183 17 is_stmt 1 view .LVU336
	.loc 1 184 17 view .LVU337
@ armwave.c:184:                 wave_word >>= 8;
	.loc 1 184 27 is_stmt 0 view .LVU338
	lsr	ip, ip, #8	@ wave_word, wave_word,
.LVL90:
	.loc 1 186 17 is_stmt 1 view .LVU339
@ armwave.c:186:                 if(value != 0) {
	.loc 1 186 19 is_stmt 0 view .LVU340
	bne	.L80		@,
.L60:
@ armwave.c:182:             for(w = 0; w < 4; w++) {
	.loc 1 182 32 view .LVU341
	add	r9, r9, #1	@ w, w,
.LVL91:
@ armwave.c:182:             for(w = 0; w < 4; w++) {
	.loc 1 182 13 view .LVU342
	cmp	r9, #4	@ w,
	beq	.L59		@,
	.loc 1 183 17 is_stmt 1 view .LVU343
.LVL92:
	.loc 1 184 17 view .LVU344
@ armwave.c:186:                 if(value != 0) {
	.loc 1 186 19 is_stmt 0 view .LVU345
	ands	r3, ip, #255	@ _8, wave_word,
@ armwave.c:184:                 wave_word >>= 8;
	.loc 1 184 27 view .LVU346
	lsr	ip, ip, #8	@ wave_word, wave_word,
.LVL93:
	.loc 1 186 17 is_stmt 1 view .LVU347
@ armwave.c:186:                 if(value != 0) {
	.loc 1 186 19 is_stmt 0 view .LVU348
	beq	.L60		@,
.L80:
	.loc 1 187 21 is_stmt 1 view .LVU349
.LVL94:
	.loc 1 188 21 view .LVU350
	.loc 1 189 21 view .LVU351
	.loc 1 191 21 view .LVU352
	.loc 1 192 21 view .LVU353
	.loc 1 193 21 view .LVU354
	.loc 1 196 21 view .LVU355
@ armwave.c:189:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 189 55 is_stmt 0 view .LVU356
	ldr	r2, [sp, #4]	@ _14, %sfp
@ armwave.c:188:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 188 55 view .LVU357
	ldr	r1, [sp]	@ _11, %sfp
	add	r7, lr, r9	@ _113, n, w
@ armwave.c:189:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 189 55 view .LVU358
	mul	r2, r2, r3	@ tmp179, _14, _8
@ armwave.c:188:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 188 55 view .LVU359
	mul	r1, r1, r3	@ tmp183, _11, _8
@ armwave.c:189:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 189 24 view .LVU360
	asr	r2, r2, #8	@ bb, tmp179,
.LVL95:
@ armwave.c:187:                     rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 187 55 view .LVU361
	mul	r3, r3, fp	@ tmp189, _8, _7
.LVL96:
@ armwave.c:188:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 188 24 view .LVU362
	asr	r1, r1, #8	@ gg, tmp183,
.LVL97:
@ armwave.c:193:                     b = MIN(bb, 255);
	.loc 1 193 25 view .LVU363
	cmp	r2, #255	@ bb,
	movge	r2, #255	@ bb,
.LVL98:
@ armwave.c:192:                     g = MIN(gg, 255);
	.loc 1 192 25 view .LVU364
	cmp	r1, #255	@ gg,
	movge	r1, #255	@ gg,
.LVL99:
@ armwave.c:187:                     rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 187 24 view .LVU365
	asr	r3, r3, #8	@ rr, tmp189,
.LVL100:
@ armwave.c:191:                     r = MIN(rr, 255);
	.loc 1 191 25 view .LVU366
	cmp	r3, #255	@ rr,
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 56 view .LVU367
	lsl	r1, r1, #8	@ tmp184, gg,
@ armwave.c:191:                     r = MIN(rr, 255);
	.loc 1 191 25 view .LVU368
	movge	r3, #255	@ rr,
.LVL101:
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 44 view .LVU369
	lsl	r4, r2, #16	@ tmp180, bb,
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 56 view .LVU370
	uxth	r1, r1	@ tmp185, tmp184
@ armwave.c:200:                     yy = (nsub & 0xff) * g_armwave_state.vscale;
	.loc 1 200 57 view .LVU371
	ldr	r10, [r5, #32]	@ _28, g_armwave_state.vscale
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 44 view .LVU372
	and	r4, r4, #16711680	@ tmp181, tmp180,
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 51 view .LVU373
	orr	r4, r4, r1	@ tmp187, tmp181, tmp185
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 62 view .LVU374
	uxtb	r3, r3	@ rr, rr
@ armwave.c:200:                     yy = (nsub & 0xff) * g_armwave_state.vscale;
	.loc 1 200 32 view .LVU375
	uxtb	r1, r7	@ _113, _113
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 62 view .LVU376
	orr	r4, r4, r3	@ tmp191, tmp187, rr
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 21 view .LVU377
	cmp	r10, #0	@ _28,
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 26 view .LVU378
	orr	r4, r4, #-16777216	@ word, tmp191,
.LVL102:
	.loc 1 199 21 is_stmt 1 view .LVU379
	.loc 1 200 21 view .LVU380
@ armwave.c:201:                     xx = (nsub >> 8);
	.loc 1 201 32 is_stmt 0 view .LVU381
	asr	r7, r7, #8	@ xx, _113,
.LVL103:
@ armwave.c:200:                     yy = (nsub & 0xff) * g_armwave_state.vscale;
	.loc 1 200 24 view .LVU382
	mul	r10, r10, r1	@ yy, _28, _113
.LVL104:
	.loc 1 201 21 is_stmt 1 view .LVU383
	.loc 1 203 21 view .LVU384
	.loc 1 203 21 is_stmt 0 view .LVU385
	beq	.L60		@,
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 29 view .LVU386
	mov	r3, #0	@ row,
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 38 view .LVU387
	mov	r2, r3	@ _35, row
.LVL105:
.L61:
	.loc 1 204 25 is_stmt 1 discriminator 3 view .LVU388
	.loc 1 205 25 discriminator 3 view .LVU389
@ armwave.c:204:                         offset = (xx + ((yy + row) * g_armwave_state.target_width)); 
	.loc 1 204 52 is_stmt 0 discriminator 3 view .LVU390
	ldr	r1, [r5, #72]	@ g_armwave_state.target_width, g_armwave_state.target_width
@ armwave.c:204:                         offset = (xx + ((yy + row) * g_armwave_state.target_width)); 
	.loc 1 204 45 discriminator 3 view .LVU391
	add	r2, r2, r10	@ tmp194, _35, yy
.LVL106:
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 67 discriminator 3 view .LVU392
	add	r3, r3, #1	@ tmp199, row,
.LVL107:
@ armwave.c:205:                         *(out_buffer_base + offset) = word;
	.loc 1 205 43 discriminator 3 view .LVU393
	mla	r1, r1, r2, r7	@ tmp198, g_armwave_state.target_width, tmp194, xx
.LVL108:
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 67 discriminator 3 view .LVU394
	uxtb	r3, r3	@ row, tmp199
.LVL109:
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 38 discriminator 3 view .LVU395
	mov	r2, r3	@ _35, row
@ armwave.c:205:                         *(out_buffer_base + offset) = word;
	.loc 1 205 53 discriminator 3 view .LVU396
	str	r4, [r0, r1, lsl #2]	@ word, *_34
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 21 discriminator 3 view .LVU397
	ldr	r1, [r5, #32]	@ g_armwave_state.vscale, g_armwave_state.vscale
.LVL110:
	.loc 1 203 21 discriminator 3 view .LVU398
	cmp	r3, r1	@ row, g_armwave_state.vscale
	bcc	.L61		@,
	.loc 1 203 21 discriminator 3 view .LVU399
	b	.L60		@
.LVL111:
.L78:
	.loc 1 170 5 is_stmt 1 discriminator 1 view .LVU400
	ldr	r3, .L81+4	@,
	mov	r2, #170	@,
	ldr	r1, .L81+8	@,
	ldr	r0, .L81+12	@,
.LVL112:
	.loc 1 170 5 is_stmt 0 discriminator 1 view .LVU401
	bl	__assert_fail		@
.LVL113:
.L82:
	.align	2
.L81:
	.word	g_armwave_state
	.word	.LANCHOR0+24
	.word	.LC4
	.word	.LC5
	.cfi_endproc
.LFE61:
	.size	armwave_fill_pixbuf_scaled, .-armwave_fill_pixbuf_scaled
	.align	2
	.global	armwave_setup_render
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_setup_render, %function
armwave_setup_render:
.LVL114:
.LFB62:
	.loc 1 217 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 12, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 218 5 view .LVU403
	.loc 1 219 5 view .LVU404
	.loc 1 221 5 view .LVU405
@ armwave.c:217: {
	.loc 1 217 1 is_stmt 0 view .LVU406
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
	vpush.64	{d8}	@
	.cfi_def_cfa_offset 40
	.cfi_offset 80, -40
	.cfi_offset 81, -36
	mov	r5, r0	@ start_point, start_point
	mov	r9, r1	@ end_point, end_point
	mov	r8, r2	@ waves_max, waves_max
	sub	sp, sp, #16	@,,
	.cfi_def_cfa_offset 56
@ armwave.c:217: {
	.loc 1 217 1 view .LVU407
	mov	r10, r3	@ wave_stride, wave_stride
	ldr	r7, [sp, #56]	@ target_width, target_width
	ldr	r6, [sp, #60]	@ target_height, target_height
	ldr	ip, [sp, #64]	@ render_flags, render_flags
@ armwave.c:221:     printf("s=%d e=%d w=%d ws=%d tw=%d th=%d rf=0x%08x\n", start_point, end_point, waves_max, wave_stride, target_width, target_height, render_flags);
	.loc 1 221 5 view .LVU408
	str	r3, [sp]	@ wave_stride,
	str	r7, [sp, #4]	@ target_width,
	mov	r3, r2	@, waves_max
.LVL115:
	.loc 1 221 5 view .LVU409
	str	r6, [sp, #8]	@ target_height,
	mov	r2, r1	@, end_point
.LVL116:
	.loc 1 221 5 view .LVU410
	str	ip, [sp, #12]	@ render_flags,
	mov	r1, r0	@, start_point
.LVL117:
	.loc 1 221 5 view .LVU411
	ldr	r0, .L106+4	@,
.LVL118:
	.loc 1 221 5 view .LVU412
	bl	printf		@
.LVL119:
	.loc 1 224 5 is_stmt 1 view .LVU413
	cmp	r5, r9	@ start_point, end_point
	bcs	.L101		@,
	.loc 1 227 5 view .LVU414
	sub	r3, r6, #256	@ tmp154, target_height,
	bics	r3, r3, #256	@ tmp231, tmp154,
	beq	.L85		@,
@ armwave.c:227:     assert(target_height == 256 || target_height == 512 || target_height == 1024 || target_height == 2048);
	.loc 1 227 5 is_stmt 0 discriminator 1 view .LVU415
	sub	r3, r6, #1024	@ tmp156, target_height,
	bics	r3, r3, #1024	@ tmp232, tmp156,
	bne	.L102		@,
	.loc 1 232 12 is_stmt 1 view .LVU416
	.loc 1 235 12 view .LVU417
@ armwave.c:235:     } else if(target_height == 1024) {
	.loc 1 235 14 is_stmt 0 view .LVU418
	cmp	r6, #1024	@ target_height,
@ armwave.c:236:         g_armwave_state.row_shift = 10;
	.loc 1 236 35 view .LVU419
	ldr	r4, .L106+8	@ tmp228,
@ armwave.c:235:     } else if(target_height == 1024) {
	.loc 1 235 14 view .LVU420
	beq	.L103		@,
	.loc 1 238 12 is_stmt 1 view .LVU421
@ armwave.c:238:     } else if(target_height == 2048) {
	.loc 1 238 14 is_stmt 0 view .LVU422
	cmp	r6, #2048	@ target_height,
	.loc 1 239 9 is_stmt 1 view .LVU423
@ armwave.c:239:         g_armwave_state.row_shift = 11;
	.loc 1 239 35 is_stmt 0 view .LVU424
	moveq	r2, #11	@ tmp175,
@ armwave.c:240:         g_armwave_state.row_mask = 0x7ff;
	.loc 1 240 34 view .LVU425
	ldreq	r3, .L106+12	@ tmp177,
	strdeq	r2, [r4, #80]	@, tmp228,
.L88:
	.loc 1 244 5 is_stmt 1 view .LVU426
@ armwave.c:249:     g_armwave_state.size = target_height * target_width;
	.loc 1 249 42 is_stmt 0 view .LVU427
	mul	r0, r6, r7	@ tmp185, target_height, target_width
@ armwave.c:251:     g_armwave_state.ch_buff_size = g_armwave_state.bitdepth_height * target_width;
	.loc 1 251 68 view .LVU428
	lsl	r3, r7, #8	@ _9, target_width,
@ armwave.c:245:     g_armwave_state.vscale = target_height / 256;
	.loc 1 245 44 view .LVU429
	lsr	ip, r6, #8	@ tmp180, target_height,
@ armwave.c:256:     printf("ch_buff_size=%d\n", g_armwave_state.ch_buff_size);
	.loc 1 256 5 view .LVU430
	mov	r1, r3	@, _9
@ armwave.c:250:     g_armwave_state.bitdepth_height = 256;  // Always 256 possible levels in 8-bit mode
	.loc 1 250 37 view .LVU431
	mov	r2, #256	@ tmp187,
@ armwave.c:249:     g_armwave_state.size = target_height * target_width;
	.loc 1 249 26 view .LVU432
	str	r0, [r4, #68]	@ tmp185, g_armwave_state.size
@ armwave.c:254:     g_armwave_state.wave_length = end_point - start_point;
	.loc 1 254 45 view .LVU433
	sub	r5, r9, r5	@ _10, end_point, start_point
.LVL120:
@ armwave.c:256:     printf("ch_buff_size=%d\n", g_armwave_state.ch_buff_size);
	.loc 1 256 5 view .LVU434
	ldr	r0, .L106+16	@,
@ armwave.c:251:     g_armwave_state.ch_buff_size = g_armwave_state.bitdepth_height * target_width;
	.loc 1 251 34 view .LVU435
	str	r3, [r4, #64]	@ _9, g_armwave_state.ch_buff_size
@ armwave.c:254:     g_armwave_state.wave_length = end_point - start_point;
	.loc 1 254 33 view .LVU436
	str	r5, [r4, #60]	@ _10, g_armwave_state.wave_length
@ armwave.c:244:     g_armwave_state.xstride = target_height;
	.loc 1 244 29 view .LVU437
	str	r6, [r4, #28]	@ target_height, g_armwave_state.xstride
	.loc 1 245 5 is_stmt 1 view .LVU438
@ armwave.c:246:     g_armwave_state.wave_stride = wave_stride;
	.loc 1 246 33 is_stmt 0 view .LVU439
	str	r10, [r4, #36]	@ wave_stride, g_armwave_state.wave_stride
@ armwave.c:247:     g_armwave_state.waves_max = waves_max;
	.loc 1 247 31 view .LVU440
	str	r8, [r4, #44]	@ waves_max, g_armwave_state.waves_max
@ armwave.c:248:     g_armwave_state.waves = waves_max;  // Need a function to be able to change this on the fly
	.loc 1 248 27 view .LVU441
	str	r8, [r4, #40]	@ waves_max, g_armwave_state.waves
@ armwave.c:252:     g_armwave_state.target_width = target_width;
	.loc 1 252 34 view .LVU442
	str	r7, [r4, #72]	@ target_width, g_armwave_state.target_width
@ armwave.c:253:     g_armwave_state.target_height = target_height;
	.loc 1 253 35 view .LVU443
	str	r6, [r4, #76]	@ target_height, g_armwave_state.target_height
@ armwave.c:245:     g_armwave_state.vscale = target_height / 256;
	.loc 1 245 28 view .LVU444
	str	ip, [r4, #32]	@ tmp180, g_armwave_state.vscale
	.loc 1 246 5 is_stmt 1 view .LVU445
	.loc 1 247 5 view .LVU446
	.loc 1 248 5 view .LVU447
	.loc 1 249 5 view .LVU448
	.loc 1 250 5 view .LVU449
@ armwave.c:250:     g_armwave_state.bitdepth_height = 256;  // Always 256 possible levels in 8-bit mode
	.loc 1 250 37 is_stmt 0 view .LVU450
	str	r2, [r4, #48]	@ tmp187, g_armwave_state.bitdepth_height
	.loc 1 251 5 is_stmt 1 view .LVU451
	.loc 1 252 5 view .LVU452
	.loc 1 253 5 view .LVU453
	.loc 1 254 5 view .LVU454
	.loc 1 256 5 view .LVU455
	bl	printf		@
.LVL121:
	.loc 1 261 5 view .LVU456
@ armwave.c:263:     if(g_armwave_state.ch1_buffer != NULL)
	.loc 1 263 23 is_stmt 0 view .LVU457
	ldr	r0, [r4, #4]	@ _11, g_armwave_state.ch1_buffer
@ armwave.c:261:     g_armwave_state.slice_height = 64;  
	.loc 1 261 34 view .LVU458
	mov	r3, #64	@ tmp194,
@ armwave.c:263:     if(g_armwave_state.ch1_buffer != NULL)
	.loc 1 263 7 view .LVU459
	cmp	r0, #0	@ _11,
@ armwave.c:261:     g_armwave_state.slice_height = 64;  
	.loc 1 261 34 view .LVU460
	str	r3, [r4, #52]	@ tmp194, g_armwave_state.slice_height
	.loc 1 263 5 is_stmt 1 view .LVU461
@ armwave.c:263:     if(g_armwave_state.ch1_buffer != NULL)
	.loc 1 263 7 is_stmt 0 view .LVU462
	beq	.L90		@,
	.loc 1 264 9 is_stmt 1 view .LVU463
	bl	free		@
.LVL122:
.L90:
	.loc 1 266 5 view .LVU464
@ armwave.c:266:     g_armwave_state.ch1_buffer = calloc(g_armwave_state.ch_buff_size, 1);
	.loc 1 266 34 is_stmt 0 view .LVU465
	mov	r1, #1	@,
	ldr	r0, [r4, #64]	@, g_armwave_state.ch_buff_size
	bl	calloc		@
.LVL123:
@ armwave.c:268:     assert(g_armwave_state.ch1_buffer != NULL);
	.loc 1 268 5 view .LVU466
	cmp	r0, #0	@ tmp198,
@ armwave.c:266:     g_armwave_state.ch1_buffer = calloc(g_armwave_state.ch_buff_size, 1);
	.loc 1 266 32 view .LVU467
	str	r0, [r4, #4]	@ tmp198, g_armwave_state.ch1_buffer
	.loc 1 268 5 is_stmt 1 view .LVU468
	beq	.L104		@,
	.loc 1 271 5 view .LVU469
.LVL124:
	.loc 1 272 5 view .LVU470
@ armwave.c:272:     points_per_pixel = length / ((float)(target_width));
	.loc 1 272 34 is_stmt 0 view .LVU471
	vmov	s15, r7	@ int	@ target_width, target_width
@ armwave.c:272:     points_per_pixel = length / ((float)(target_width));
	.loc 1 272 22 view .LVU472
	vmov	s13, r5	@ int	@ _10, _10
@ armwave.c:274:     g_armwave_state.xcoord_to_xpixel = malloc(length * sizeof(uint16_t));
	.loc 1 274 40 view .LVU473
	lsl	r0, r5, #1	@, _10,
@ armwave.c:272:     points_per_pixel = length / ((float)(target_width));
	.loc 1 272 34 view .LVU474
	vcvt.f32.u32	s14, s15	@ tmp205, target_width
@ armwave.c:272:     points_per_pixel = length / ((float)(target_width));
	.loc 1 272 22 view .LVU475
	vcvt.f32.u32	s13, s13	@ tmp204, _10
@ armwave.c:273:     g_armwave_state.slice_record_height = points_per_pixel * g_armwave_state.slice_height;
	.loc 1 273 60 view .LVU476
	vldr.32	s15, [r4, #52]	@ int	@ tmp234, g_armwave_state.slice_height
	vcvt.f32.u32	s15, s15	@ tmp208, tmp234
@ armwave.c:272:     points_per_pixel = length / ((float)(target_width));
	.loc 1 272 22 view .LVU477
	vdiv.f32	s16, s13, s14	@ points_per_pixel, tmp204, tmp205
.LVL125:
	.loc 1 273 5 is_stmt 1 view .LVU478
@ armwave.c:273:     g_armwave_state.slice_record_height = points_per_pixel * g_armwave_state.slice_height;
	.loc 1 273 60 is_stmt 0 view .LVU479
	vmul.f32	s15, s15, s16	@ tmp210, tmp208, points_per_pixel
@ armwave.c:273:     g_armwave_state.slice_record_height = points_per_pixel * g_armwave_state.slice_height;
	.loc 1 273 41 view .LVU480
	vcvt.u32.f32	s15, s15	@ tmp211, tmp210
	vstr.32	s15, [r4, #56]	@ int	@ tmp211, g_armwave_state.slice_record_height
	.loc 1 274 5 is_stmt 1 view .LVU481
@ armwave.c:274:     g_armwave_state.xcoord_to_xpixel = malloc(length * sizeof(uint16_t));
	.loc 1 274 40 is_stmt 0 view .LVU482
	bl	malloc		@
.LVL126:
@ armwave.c:276:     assert(g_armwave_state.xcoord_to_xpixel != NULL);
	.loc 1 276 5 view .LVU483
	cmp	r0, #0	@ tmp213,
@ armwave.c:274:     g_armwave_state.xcoord_to_xpixel = malloc(length * sizeof(uint16_t));
	.loc 1 274 38 view .LVU484
	str	r0, [r4, #116]	@ tmp213, g_armwave_state.xcoord_to_xpixel
	.loc 1 276 5 is_stmt 1 view .LVU485
	beq	.L105		@,
@ armwave.c:279:         g_armwave_state.xcoord_to_xpixel[xx] = (1.0f / points_per_pixel) * xx;
	.loc 1 279 54 is_stmt 0 view .LVU486
	vldr.32	s15, .L106	@ tmp219,
	sub	r0, r0, #2	@ ivtmp.86, tmp213,
@ armwave.c:278:     for(xx = 0; xx < length; xx++) {
	.loc 1 278 12 view .LVU487
	mov	r3, #0	@ xx,
@ armwave.c:279:         g_armwave_state.xcoord_to_xpixel[xx] = (1.0f / points_per_pixel) * xx;
	.loc 1 279 54 view .LVU488
	vdiv.f32	s14, s15, s16	@ _22, tmp219, points_per_pixel
.L93:
.LVL127:
	.loc 1 279 9 is_stmt 1 discriminator 3 view .LVU489
@ armwave.c:279:         g_armwave_state.xcoord_to_xpixel[xx] = (1.0f / points_per_pixel) * xx;
	.loc 1 279 74 is_stmt 0 discriminator 3 view .LVU490
	vmov	s15, r3	@ int	@ xx, xx
@ armwave.c:278:     for(xx = 0; xx < length; xx++) {
	.loc 1 278 32 discriminator 3 view .LVU491
	add	r3, r3, #1	@ xx, xx,
.LVL128:
@ armwave.c:278:     for(xx = 0; xx < length; xx++) {
	.loc 1 278 5 discriminator 3 view .LVU492
	cmp	r5, r3	@ _10, xx
@ armwave.c:279:         g_armwave_state.xcoord_to_xpixel[xx] = (1.0f / points_per_pixel) * xx;
	.loc 1 279 74 discriminator 3 view .LVU493
	vcvt.f32.s32	s15, s15	@ tmp220, xx
	vmul.f32	s15, s15, s14	@ tmp221, tmp220, _22
@ armwave.c:279:         g_armwave_state.xcoord_to_xpixel[xx] = (1.0f / points_per_pixel) * xx;
	.loc 1 279 46 discriminator 3 view .LVU494
	vcvt.u32.f32	s15, s15	@ tmp222, tmp221
	vmov	r2, s15	@ int	@ tmp222, tmp222
	strh	r2, [r0, #2]!	@ movhi	@ tmp222, MEM[base: _80, offset: 0B]
@ armwave.c:278:     for(xx = 0; xx < length; xx++) {
	.loc 1 278 5 discriminator 3 view .LVU495
	bne	.L93		@,
	.loc 1 284 5 is_stmt 1 view .LVU496
@ armwave.c:284:     g_armwave_state.out_pixbuf = malloc(sizeof(uint32_t) * g_armwave_state.size);
	.loc 1 284 34 is_stmt 0 view .LVU497
	ldr	r0, [r4, #68]	@ g_armwave_state.size, g_armwave_state.size
	lsl	r0, r0, #2	@, g_armwave_state.size,
	bl	malloc		@
.LVL129:
@ armwave.c:284:     g_armwave_state.out_pixbuf = malloc(sizeof(uint32_t) * g_armwave_state.size);
	.loc 1 284 32 view .LVU498
	str	r0, [r4, #88]	@, g_armwave_state.out_pixbuf
@ armwave.c:285: }
	.loc 1 285 1 view .LVU499
	add	sp, sp, #16	@,,
	.cfi_remember_state
	.cfi_def_cfa_offset 40
	@ sp needed	@
	vldm	sp!, {d8}	@
	.cfi_restore 80
	.cfi_restore 81
	.cfi_def_cfa_offset 32
.LVL130:
	.loc 1 285 1 view .LVU500
	pop	{r4, r5, r6, r7, r8, r9, r10, pc}	@
.LVL131:
.L85:
	.cfi_restore_state
	.loc 1 229 5 is_stmt 1 view .LVU501
@ armwave.c:229:     if(target_height == 256) {
	.loc 1 229 7 is_stmt 0 view .LVU502
	cmp	r6, #256	@ target_height,
	.loc 1 230 9 is_stmt 1 view .LVU503
@ armwave.c:230:         g_armwave_state.row_shift = 8;
	.loc 1 230 35 is_stmt 0 view .LVU504
	ldr	r4, .L106+8	@ tmp228,
@ armwave.c:234:         g_armwave_state.row_mask = 0x1ff;
	.loc 1 234 34 view .LVU505
	ldrne	r3, .L106+20	@ tmp169,
@ armwave.c:231:         g_armwave_state.row_mask = 0x0ff;
	.loc 1 231 34 view .LVU506
	moveq	r2, #8	@ tmp163,
	moveq	r3, #255	@ tmp165,
	.loc 1 232 12 is_stmt 1 view .LVU507
	.loc 1 233 9 view .LVU508
@ armwave.c:233:         g_armwave_state.row_shift = 9;
	.loc 1 233 35 is_stmt 0 view .LVU509
	movne	r2, #9	@ tmp167,
@ armwave.c:234:         g_armwave_state.row_mask = 0x1ff;
	.loc 1 234 34 view .LVU510
	strd	r2, [r4, #80]	@, tmp228,
	b	.L88		@
.L103:
	.loc 1 236 9 is_stmt 1 view .LVU511
@ armwave.c:237:         g_armwave_state.row_mask = 0x3ff;
	.loc 1 237 34 is_stmt 0 view .LVU512
	ldr	r3, .L106+24	@ tmp173,
@ armwave.c:236:         g_armwave_state.row_shift = 10;
	.loc 1 236 35 view .LVU513
	mov	r2, #10	@ tmp171,
@ armwave.c:237:         g_armwave_state.row_mask = 0x3ff;
	.loc 1 237 34 view .LVU514
	strd	r2, [r4, #80]	@, tmp228,
	b	.L88		@
.L102:
	.loc 1 227 5 is_stmt 1 discriminator 2 view .LVU515
	ldr	r3, .L106+28	@,
	mov	r2, #227	@,
	ldr	r1, .L106+32	@,
	ldr	r0, .L106+36	@,
	bl	__assert_fail		@
.LVL132:
.L104:
	.loc 1 268 5 discriminator 1 view .LVU516
	ldr	r3, .L106+28	@,
	mov	r2, #268	@,
	ldr	r1, .L106+32	@,
	ldr	r0, .L106+40	@,
	bl	__assert_fail		@
.LVL133:
.L105:
	.loc 1 276 5 discriminator 1 view .LVU517
	ldr	r3, .L106+28	@,
	mov	r2, #276	@,
	ldr	r1, .L106+32	@,
	ldr	r0, .L106+44	@,
	bl	__assert_fail		@
.LVL134:
.L101:
	.loc 1 224 5 discriminator 1 view .LVU518
	ldr	r3, .L106+28	@,
	mov	r2, #224	@,
	ldr	r1, .L106+32	@,
	ldr	r0, .L106+48	@,
	bl	__assert_fail		@
.LVL135:
.L107:
	.align	2
.L106:
	.word	1065353216
	.word	.LC6
	.word	g_armwave_state
	.word	2047
	.word	.LC9
	.word	511
	.word	1023
	.word	.LANCHOR0+52
	.word	.LC4
	.word	.LC8
	.word	.LC10
	.word	.LC11
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
.LVL136:
.LFB63:
	.loc 1 292 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 293 5 view .LVU520
	cmp	r0, #0	@ wave_buffer
	.loc 1 293 5 is_stmt 0 view .LVU521
	beq	.L113		@,
	.loc 1 294 5 is_stmt 1 view .LVU522
@ armwave.c:294:     g_armwave_state.wave_buffer = wave_buffer;
	.loc 1 294 33 is_stmt 0 view .LVU523
	ldr	r3, .L114	@ tmp116,
	str	r0, [r3, #20]	@ wave_buffer, g_armwave_state.wave_buffer
	bx	lr	@
.L113:
	.loc 1 293 5 is_stmt 1 discriminator 1 view .LVU524
@ armwave.c:292: {
	.loc 1 292 1 is_stmt 0 discriminator 1 view .LVU525
	push	{r4, lr}	@
	.cfi_def_cfa_offset 8
	.cfi_offset 4, -8
	.cfi_offset 14, -4
@ armwave.c:293:     assert(wave_buffer != NULL);
	.loc 1 293 5 discriminator 1 view .LVU526
	ldr	r3, .L114+4	@,
	ldr	r2, .L114+8	@,
	ldr	r1, .L114+12	@,
	ldr	r0, .L114+16	@,
.LVL137:
	.loc 1 293 5 discriminator 1 view .LVU527
	bl	__assert_fail		@
.LVL138:
.L115:
	.align	2
.L114:
	.word	g_armwave_state
	.word	.LANCHOR0+76
	.word	293
	.word	.LC4
	.word	.LC12
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
.LFB64:
	.loc 1 302 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	.loc 1 303 5 view .LVU529
@ armwave.c:303:     g_armwave_state.wave_buffer = g_armwave_state.test_wave_buffer;
	.loc 1 303 33 is_stmt 0 view .LVU530
	ldr	r3, .L117	@ tmp111,
	ldr	r2, [r3, #24]	@ g_armwave_state.test_wave_buffer, g_armwave_state.test_wave_buffer
	str	r2, [r3, #20]	@ g_armwave_state.test_wave_buffer, g_armwave_state.wave_buffer
@ armwave.c:304: }
	.loc 1 304 1 view .LVU531
	bx	lr	@
.L118:
	.align	2
.L117:
	.word	g_armwave_state
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
.LVL139:
.LFB65:
	.loc 1 311 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 312 5 view .LVU533
	cmp	r0, #0	@ wave_buffer_ptr
	.loc 1 312 5 is_stmt 0 view .LVU534
	beq	.L124		@,
	.loc 1 313 5 is_stmt 1 view .LVU535
@ armwave.c:313:     g_armwave_state.wave_buffer = (uint8_t*)wave_buffer_ptr;
	.loc 1 313 33 is_stmt 0 view .LVU536
	ldr	r3, .L125	@ tmp116,
	str	r0, [r3, #20]	@ wave_buffer_ptr, g_armwave_state.wave_buffer
	bx	lr	@
.L124:
	.loc 1 312 5 is_stmt 1 discriminator 1 view .LVU537
@ armwave.c:311: {
	.loc 1 311 1 is_stmt 0 discriminator 1 view .LVU538
	push	{r4, lr}	@
	.cfi_def_cfa_offset 8
	.cfi_offset 4, -8
	.cfi_offset 14, -4
@ armwave.c:312:     assert(wave_buffer_ptr != 0);
	.loc 1 312 5 discriminator 1 view .LVU539
	mov	r2, #312	@,
	ldr	r3, .L125+4	@,
	ldr	r1, .L125+8	@,
	ldr	r0, .L125+12	@,
.LVL140:
	.loc 1 312 5 discriminator 1 view .LVU540
	bl	__assert_fail		@
.LVL141:
.L126:
	.align	2
.L125:
	.word	g_armwave_state
	.word	.LANCHOR0+104
	.word	.LC4
	.word	.LC13
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
.LVL142:
.LFB66:
	.loc 1 320 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	.loc 1 322 5 view .LVU542
	ldr	r3, .L128	@ tmp113,
	mov	r1, #0	@,
	ldr	r2, [r3, #64]	@, g_armwave_state.ch_buff_size
	ldr	r0, [r3, #4]	@, g_armwave_state.ch1_buffer
.LVL143:
	.loc 1 322 5 is_stmt 0 view .LVU543
	b	memset		@
.LVL144:
.L129:
	.align	2
.L128:
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
.LVL145:
.LFB67:
	.loc 1 329 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	.loc 1 331 5 view .LVU545
	cmp	r0, #1	@ ch,
	.loc 1 333 13 view .LVU546
@ armwave.c:333:             g_armwave_state.ch1_color.r = r;
	.loc 1 333 41 is_stmt 0 view .LVU547
	ldreq	r0, .L132	@ tmp117,
.LVL146:
	.loc 1 333 41 view .LVU548
	strheq	r1, [r0, #92]	@ movhi	@ r, g_armwave_state.ch1_color.r
	.loc 1 334 13 is_stmt 1 view .LVU549
@ armwave.c:334:             g_armwave_state.ch1_color.g = g;
	.loc 1 334 41 is_stmt 0 view .LVU550
	strheq	r2, [r0, #94]	@ movhi	@ g, g_armwave_state.ch1_color.g
	.loc 1 335 13 is_stmt 1 view .LVU551
@ armwave.c:335:             g_armwave_state.ch1_color.b = b;
	.loc 1 335 41 is_stmt 0 view .LVU552
	strheq	r3, [r0, #96]	@ movhi	@ b, g_armwave_state.ch1_color.b
	.loc 1 336 13 is_stmt 1 view .LVU553
@ armwave.c:338: }
	.loc 1 338 1 is_stmt 0 view .LVU554
	bx	lr	@
.L133:
	.align	2
.L132:
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
.LVL147:
.LFB68:
	.loc 1 344 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 345 5 view .LVU556
@ armwave.c:344: {
	.loc 1 344 1 is_stmt 0 view .LVU557
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
@ armwave.c:345:     FILE *fp = fopen(fn, "wb");
	.loc 1 345 16 view .LVU558
	ldr	r1, .L146	@,
.LVL148:
	.loc 1 345 16 view .LVU559
	mov	r0, r3	@, fn
.LVL149:
	.loc 1 345 16 view .LVU560
	bl	fopen64		@
.LVL150:
@ armwave.c:352:     fprintf(fp, "%d %d\n", g_armwave_state.target_width, g_armwave_state.target_height);
	.loc 1 352 5 view .LVU561
	ldr	r6, .L146+4	@ tmp154,
@ armwave.c:351:     fputs("P3\n", fp);
	.loc 1 351 5 view .LVU562
	mov	r2, #3	@,
	mov	r1, #1	@,
@ armwave.c:345:     FILE *fp = fopen(fn, "wb");
	.loc 1 345 16 view .LVU563
	mov	r7, r0	@ fp,
.LVL151:
	.loc 1 346 5 is_stmt 1 view .LVU564
	.loc 1 347 5 view .LVU565
	.loc 1 351 5 view .LVU566
	mov	r3, r0	@, fp
	ldr	r0, .L146+8	@,
.LVL152:
	.loc 1 351 5 is_stmt 0 view .LVU567
	bl	fwrite		@
.LVL153:
	.loc 1 352 5 is_stmt 1 view .LVU568
	ldrd	r2, [r6, #72]	@, tmp154,
	ldr	r1, .L146+12	@,
	mov	r0, r7	@, fp
	bl	fprintf		@
.LVL154:
	.loc 1 353 5 view .LVU569
	mov	r2, #4	@,
	mov	r3, r7	@, fp
	mov	r1, #1	@,
	ldr	r0, .L146+16	@,
	bl	fwrite		@
.LVL155:
	.loc 1 355 5 view .LVU570
@ armwave.c:355:     for(yy = 0; yy < g_armwave_state.target_height; yy++) {
	.loc 1 355 37 is_stmt 0 view .LVU571
	ldr	r2, [r6, #76]	@ prephitmp_57, g_armwave_state.target_height
@ armwave.c:355:     for(yy = 0; yy < g_armwave_state.target_height; yy++) {
	.loc 1 355 5 view .LVU572
	cmp	r2, #0	@ prephitmp_57,
	beq	.L135		@,
	ldr	r3, [r6, #72]	@ _12, g_armwave_state.target_width
@ armwave.c:360:             fprintf(fp, "%3d %3d %3d\n", data & 0xff, (data >> 8) & 0xff, (data >> 16) & 0xff);
	.loc 1 360 13 view .LVU573
	ldr	r9, .L146+20	@ tmp156,
@ armwave.c:355:     for(yy = 0; yy < g_armwave_state.target_height; yy++) {
	.loc 1 355 12 view .LVU574
	mov	r5, #0	@ yy,
.LVL156:
.L136:
@ armwave.c:356:         for(xx = 0; xx < g_armwave_state.target_width; xx++) {
	.loc 1 356 9 view .LVU575
	cmp	r3, #0	@ _12,
@ armwave.c:356:         for(xx = 0; xx < g_armwave_state.target_width; xx++) {
	.loc 1 356 16 view .LVU576
	movne	r4, #0	@ xx,
@ armwave.c:356:         for(xx = 0; xx < g_armwave_state.target_width; xx++) {
	.loc 1 356 9 view .LVU577
	beq	.L145		@,
.LVL157:
.L137:
	.loc 1 357 13 is_stmt 1 discriminator 3 view .LVU578
@ armwave.c:357:             data = *(buffer + (xx + (yy * g_armwave_state.target_width)));
	.loc 1 357 29 is_stmt 0 discriminator 3 view .LVU579
	mla	r3, r5, r3, r4	@ tmp145, yy, _12, xx
@ armwave.c:360:             fprintf(fp, "%3d %3d %3d\n", data & 0xff, (data >> 8) & 0xff, (data >> 16) & 0xff);
	.loc 1 360 13 discriminator 3 view .LVU580
	mov	r1, r9	@, tmp156
	mov	r0, r7	@, fp
@ armwave.c:357:             data = *(buffer + (xx + (yy * g_armwave_state.target_width)));
	.loc 1 357 18 discriminator 3 view .LVU581
	ldr	r2, [r8, r3, lsl #2]	@ data, *_6
.LVL158:
	.loc 1 360 13 is_stmt 1 discriminator 3 view .LVU582
@ armwave.c:356:         for(xx = 0; xx < g_armwave_state.target_width; xx++) {
	.loc 1 356 58 is_stmt 0 discriminator 3 view .LVU583
	add	r4, r4, #1	@ xx, xx,
.LVL159:
@ armwave.c:360:             fprintf(fp, "%3d %3d %3d\n", data & 0xff, (data >> 8) & 0xff, (data >> 16) & 0xff);
	.loc 1 360 81 discriminator 3 view .LVU584
	lsr	ip, r2, #16	@ tmp150, data,
@ armwave.c:360:             fprintf(fp, "%3d %3d %3d\n", data & 0xff, (data >> 8) & 0xff, (data >> 16) & 0xff);
	.loc 1 360 61 discriminator 3 view .LVU585
	lsr	r3, r2, #8	@ tmp146, data,
@ armwave.c:360:             fprintf(fp, "%3d %3d %3d\n", data & 0xff, (data >> 8) & 0xff, (data >> 16) & 0xff);
	.loc 1 360 13 discriminator 3 view .LVU586
	uxtb	ip, ip	@ tmp151, tmp150
	uxtb	r3, r3	@, tmp146
	str	ip, [sp]	@ tmp151,
	uxtb	r2, r2	@, data
.LVL160:
	.loc 1 360 13 discriminator 3 view .LVU587
	bl	fprintf		@
.LVL161:
@ armwave.c:356:         for(xx = 0; xx < g_armwave_state.target_width; xx++) {
	.loc 1 356 41 discriminator 3 view .LVU588
	ldr	r3, [r6, #72]	@ _12, g_armwave_state.target_width
@ armwave.c:356:         for(xx = 0; xx < g_armwave_state.target_width; xx++) {
	.loc 1 356 9 discriminator 3 view .LVU589
	cmp	r3, r4	@ _12, xx
	bhi	.L137		@,
	ldr	r2, [r6, #76]	@ prephitmp_57, g_armwave_state.target_height
.LVL162:
.L138:
@ armwave.c:355:     for(yy = 0; yy < g_armwave_state.target_height; yy++) {
	.loc 1 355 55 discriminator 2 view .LVU590
	add	r5, r5, #1	@ yy, yy,
.LVL163:
@ armwave.c:355:     for(yy = 0; yy < g_armwave_state.target_height; yy++) {
	.loc 1 355 5 discriminator 2 view .LVU591
	cmp	r5, r2	@ yy, prephitmp_57
	bcc	.L136		@,
.LVL164:
.L135:
	.loc 1 364 5 is_stmt 1 view .LVU592
	mov	r0, r7	@, fp
@ armwave.c:365: }
	.loc 1 365 1 is_stmt 0 view .LVU593
	add	sp, sp, #12	@,,
	.cfi_remember_state
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
.LVL165:
@ armwave.c:364:     fclose(fp);
	.loc 1 364 5 view .LVU594
	b	fclose		@
.LVL166:
.L145:
	.cfi_restore_state
@ armwave.c:355:     for(yy = 0; yy < g_armwave_state.target_height; yy++) {
	.loc 1 355 55 view .LVU595
	add	r5, r5, #1	@ yy, yy,
.LVL167:
@ armwave.c:355:     for(yy = 0; yy < g_armwave_state.target_height; yy++) {
	.loc 1 355 5 view .LVU596
	cmp	r2, r5	@ prephitmp_57, yy
	bhi	.L138		@,
	b	.L135		@
.L147:
	.align	2
.L146:
	.word	.LC14
	.word	g_armwave_state
	.word	.LC15
	.word	.LC16
	.word	.LC17
	.word	.LC18
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
.LVL168:
.LFB69:
	.loc 1 371 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 372 5 view .LVU598
.LBB10:
.LBI10:
	.loc 1 44 6 view .LVU599
	.loc 1 44 6 is_stmt 0 view .LVU600
.LBE10:
@ armwave.c:371: {
	.loc 1 371 1 view .LVU601
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
	ldr	r4, .L152+20	@ ivtmp.100,
.LBB13:
.LBB11:
@ armwave.c:50:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 50 32 view .LVU602
	vldr.32	s20, .L152+16	@ tmp136,
@ armwave.c:50:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 50 26 view .LVU603
	vldr.64	d9, .L152	@ tmp151,
@ armwave.c:50:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 50 49 view .LVU604
	vldr.64	d8, .L152+8	@ tmp139,
	add	ip, r4, #1	@ _38, ivtmp.100,
.LBE11:
.LBE13:
@ armwave.c:371: {
	.loc 1 371 1 view .LVU605
	sub	sp, sp, #16	@,,
	.cfi_def_cfa_offset 72
@ armwave.c:371: {
	.loc 1 371 1 view .LVU606
	mov	r6, r0	@ wave_size, wave_size
	mov	r7, r1	@ nwaves, nwaves
	mov	r8, r2	@ render_width, render_width
	mov	r9, r3	@ render_height, render_height
	add	r10, r4, #256	@ _39, ivtmp.100,
	rsb	r5, ip, #1	@ tmp150, _38,
.LVL169:
.L149:
.LBB14:
.LBB12:
	.loc 1 50 9 is_stmt 1 view .LVU607
@ armwave.c:50:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 50 32 is_stmt 0 view .LVU608
	add	r3, r5, r4	@ tmp133, tmp150, ivtmp.100
@ armwave.c:50:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 50 26 view .LVU609
	vmov.f64	d1, d9	@, tmp151
@ armwave.c:50:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 50 32 view .LVU610
	vmov	s15, r3	@ int	@ tmp133, tmp133
	vcvt.f32.s32	s15, s15	@ tmp134, tmp133
	vdiv.f32	s0, s15, s20	@ tmp135, tmp134, tmp136
@ armwave.c:50:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 50 26 view .LVU611
	vcvt.f64.f32	d0, s0	@, tmp135
	bl	pow		@
.LVL170:
@ armwave.c:50:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 50 49 view .LVU612
	vmul.f64	d0, d0, d8	@ tmp138,, tmp139
@ armwave.c:50:         gamma_table[i] = pow(i / 255.0f, gamma) * 255.0f;
	.loc 1 50 24 view .LVU613
	vcvt.u32.f64	s15, d0	@ tmp140, tmp138
	vmov	r0, s15	@ int	@ tmp140, tmp140
	strb	r0, [r4, #1]!	@ tmp140, MEM[base: _37, offset: 0B]
.LVL171:
@ armwave.c:49:     for(i = 0; i < 256; i++) {
	.loc 1 49 5 view .LVU614
	cmp	r4, r10	@ ivtmp.100, _39
	bne	.L149		@,
.LVL172:
	.loc 1 49 5 view .LVU615
.LBE12:
.LBE14:
	.loc 1 375 5 is_stmt 1 view .LVU616
.LBB15:
.LBI15:
	.loc 1 328 6 view .LVU617
.LBB16:
	.loc 1 331 5 view .LVU618
	.loc 1 333 13 view .LVU619
	.loc 1 334 13 view .LVU620
@ armwave.c:333:             g_armwave_state.ch1_color.r = r;
	.loc 1 333 41 is_stmt 0 view .LVU621
	ldr	ip, .L152+24	@ tmp142,
	ldr	r4, .L152+28	@ tmp143,
.LBE16:
.LBE15:
@ armwave.c:377:     armwave_setup_render(0, wave_size, nwaves, wave_size, render_width, render_height, 0x00000000);
	.loc 1 377 5 view .LVU622
	mov	r0, #0	@ tmp147,
.LBB20:
.LBB17:
@ armwave.c:335:             g_armwave_state.ch1_color.b = b;
	.loc 1 335 41 view .LVU623
	mov	lr, #250	@ tmp146,
.LBE17:
.LBE20:
@ armwave.c:377:     armwave_setup_render(0, wave_size, nwaves, wave_size, render_width, render_height, 0x00000000);
	.loc 1 377 5 view .LVU624
	str	r0, [sp, #8]	@ tmp147,
	str	r9, [sp, #4]	@ render_height,
	str	r8, [sp]	@ render_width,
	mov	r1, r6	@, tmp3
	mov	r2, r7	@, nwaves
.LBB21:
.LBB18:
@ armwave.c:333:             g_armwave_state.ch1_color.r = r;
	.loc 1 333 41 view .LVU625
	str	r4, [ip, #92]	@ tmp143, MEM[(short int *)&g_armwave_state + 92B]
	.loc 1 335 13 is_stmt 1 view .LVU626
.LBE18:
.LBE21:
@ armwave.c:377:     armwave_setup_render(0, wave_size, nwaves, wave_size, render_width, render_height, 0x00000000);
	.loc 1 377 5 is_stmt 0 view .LVU627
	mov	r3, r6	@, wave_size
.LBB22:
.LBB19:
@ armwave.c:335:             g_armwave_state.ch1_color.b = b;
	.loc 1 335 41 view .LVU628
	strh	lr, [ip, #96]	@ movhi	@ tmp146, g_armwave_state.ch1_color.b
	.loc 1 336 13 is_stmt 1 view .LVU629
.LVL173:
	.loc 1 336 13 is_stmt 0 view .LVU630
.LBE19:
.LBE22:
	.loc 1 377 5 is_stmt 1 view .LVU631
	bl	armwave_setup_render		@
.LVL174:
	.loc 1 379 5 view .LVU632
	ldr	r1, .L152+32	@,
	ldr	r0, .L152+36	@,
@ armwave.c:380: }
	.loc 1 380 1 is_stmt 0 view .LVU633
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
.LVL175:
@ armwave.c:379:     printf("armwave version: %s\n", ARMWAVE_VER);
	.loc 1 379 5 view .LVU634
	b	printf		@
.LVL176:
.L153:
	.align	3
.L152:
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
	.global	__aeabi_uidiv
	.align	2
	.global	armwave_test_generate
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_test_generate, %function
armwave_test_generate:
.LFB70:
	.loc 1 386 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 387 5 view .LVU636
	.loc 1 389 5 view .LVU637
@ armwave.c:386: {
	.loc 1 386 1 is_stmt 0 view .LVU638
	push	{r4, r5, r6, r7, r8, lr}	@
	.cfi_def_cfa_offset 24
	.cfi_offset 4, -24
	.cfi_offset 5, -20
	.cfi_offset 6, -16
	.cfi_offset 7, -12
	.cfi_offset 8, -8
	.cfi_offset 14, -4
@ armwave.c:389:     memset(g_armwave_state.ch1_buffer, 0, g_armwave_state.ch_buff_size);
	.loc 1 389 5 view .LVU639
	mov	r1, #0	@,
	ldr	r5, .L159	@ tmp142,
	ldr	r2, [r5, #64]	@, g_armwave_state.ch_buff_size
	ldr	r0, [r5, #4]	@, g_armwave_state.ch1_buffer
	bl	memset		@
.LVL177:
	.loc 1 391 5 is_stmt 1 view .LVU640
@ armwave.c:391:     for(yy = 0; yy < (2048 / g_armwave_state.slice_height); yy++) {
	.loc 1 391 45 is_stmt 0 view .LVU641
	ldr	r6, [r5, #52]	@ _8, g_armwave_state.slice_height
@ armwave.c:391:     for(yy = 0; yy < (2048 / g_armwave_state.slice_height); yy++) {
	.loc 1 391 5 view .LVU642
	cmp	r6, #2048	@ _8,
	pophi	{r4, r5, r6, r7, r8, pc}	@
@ armwave.c:392:         printf("armwave_test_generate: slice %d (y=%d, h=%d)\n", yy, yy * g_armwave_state.slice_height, g_armwave_state.slice_record_height);
	.loc 1 392 9 view .LVU643
	ldr	r7, .L159+4	@ tmp143,
@ armwave.c:391:     for(yy = 0; yy < (2048 / g_armwave_state.slice_height); yy++) {
	.loc 1 391 12 view .LVU644
	mov	r4, #0	@ yy,
.LVL178:
.L156:
	.loc 1 392 9 is_stmt 1 discriminator 3 view .LVU645
	mul	r2, r4, r6	@, yy, _8
	ldr	r3, [r5, #56]	@, g_armwave_state.slice_record_height
	mov	r1, r4	@, yy
	mov	r0, r7	@, tmp143
	bl	printf		@
.LVL179:
	.loc 1 394 9 discriminator 3 view .LVU646
	ldrd	r0, [r5, #52]	@, tmp142,
	mul	r0, r0, r4	@, g_armwave_state.slice_height, yy
	bl	render_nonaa_to_buffer_1ch_slice		@
.LVL180:
@ armwave.c:391:     for(yy = 0; yy < (2048 / g_armwave_state.slice_height); yy++) {
	.loc 1 391 45 is_stmt 0 discriminator 3 view .LVU647
	ldr	r6, [r5, #52]	@ _8, g_armwave_state.slice_height
@ armwave.c:391:     for(yy = 0; yy < (2048 / g_armwave_state.slice_height); yy++) {
	.loc 1 391 28 discriminator 3 view .LVU648
	mov	r0, #2048	@,
	mov	r1, r6	@, _8
	bl	__aeabi_uidiv		@
.LVL181:
@ armwave.c:391:     for(yy = 0; yy < (2048 / g_armwave_state.slice_height); yy++) {
	.loc 1 391 63 discriminator 3 view .LVU649
	add	r4, r4, #1	@ yy, yy,
.LVL182:
@ armwave.c:391:     for(yy = 0; yy < (2048 / g_armwave_state.slice_height); yy++) {
	.loc 1 391 5 discriminator 3 view .LVU650
	cmp	r0, r4	@, yy
	bhi	.L156		@,
	pop	{r4, r5, r6, r7, r8, pc}	@
.L160:
	.align	2
.L159:
	.word	g_armwave_state
	.word	.LC19
	.cfi_endproc
.LFE70:
	.size	armwave_test_generate, .-armwave_test_generate
	.align	2
	.global	armwave_test_fill_outbuf
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_test_fill_outbuf, %function
armwave_test_fill_outbuf:
.LFB71:
	.loc 1 402 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 403 5 view .LVU652
	ldr	r2, .L185	@ tmp203,
@ armwave.c:402: {
	.loc 1 402 1 is_stmt 0 view .LVU653
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
	sub	sp, sp, #12	@,,
	.cfi_def_cfa_offset 48
@ armwave.c:403:     armwave_fill_pixbuf_scaled(g_armwave_state.out_pixbuf);
	.loc 1 403 5 view .LVU654
	ldr	r8, [r2, #88]	@ _1, g_armwave_state.out_pixbuf
.LVL183:
.LBB25:
.LBI25:
	.loc 1 161 6 is_stmt 1 view .LVU655
.LBB26:
	.loc 1 163 5 view .LVU656
	.loc 1 164 5 view .LVU657
	.loc 1 165 5 view .LVU658
	.loc 1 166 5 view .LVU659
@ armwave.c:166:     uint32_t *base_32ptr = (uint32_t*)g_armwave_state.ch1_buffer;
	.loc 1 166 15 is_stmt 0 view .LVU660
	ldr	r9, [r2, #4]	@ base_32ptr, g_armwave_state.ch1_buffer
.LVL184:
	.loc 1 167 5 is_stmt 1 view .LVU661
	.loc 1 168 5 view .LVU662
	.loc 1 170 5 view .LVU663
	cmp	r8, #0	@ _1,
	beq	.L183		@,
	.loc 1 172 5 view .LVU664
@ armwave.c:172:     npix = g_armwave_state.target_width * 256; 
	.loc 1 172 41 is_stmt 0 view .LVU665
	ldr	r5, [r2, #72]	@ g_armwave_state.target_width, g_armwave_state.target_width
	lsl	r5, r5, #8	@ npix, g_armwave_state.target_width,
.LVL185:
	.loc 1 173 5 is_stmt 1 view .LVU666
	.loc 1 175 5 view .LVU667
	.loc 1 175 5 is_stmt 0 view .LVU668
	cmp	r5, #0	@ npix,
	ble	.L161		@,
@ armwave.c:188:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 188 52 view .LVU669
	ldrsh	r3, [r2, #94]	@ _24, g_armwave_state.ch1_color.g
@ armwave.c:187:                     rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 187 52 view .LVU670
	ldrsh	fp, [r2, #92]	@ _18, g_armwave_state.ch1_color.r
@ armwave.c:175:     for(n = 0; n < npix; n += 4) {
	.loc 1 175 11 view .LVU671
	mov	r1, #0	@ n,
@ armwave.c:188:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 188 52 view .LVU672
	str	r3, [sp]	@ _24, %sfp
@ armwave.c:189:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 189 52 view .LVU673
	ldrsh	r3, [r2, #96]	@ _28, g_armwave_state.ch1_color.b
	str	r3, [sp, #4]	@ _28, %sfp
.LVL186:
.L168:
	.loc 1 179 9 is_stmt 1 view .LVU674
@ armwave.c:179:         wave_word = *base_32ptr++;
	.loc 1 179 19 is_stmt 0 view .LVU675
	ldr	r0, [r9], #4	@ wave_word, MEM[base: base_32ptr_9, offset: 4294967292B]
.LVL187:
	.loc 1 181 9 is_stmt 1 view .LVU676
@ armwave.c:181:         if(COND_UNLIKELY(wave_word != 0)) {
	.loc 1 181 11 is_stmt 0 view .LVU677
	cmp	r0, #0	@ wave_word,
	bne	.L180		@,
	add	r1, r1, #4	@ n, n,
.L164:
.LVL188:
@ armwave.c:175:     for(n = 0; n < npix; n += 4) {
	.loc 1 175 5 view .LVU678
	cmp	r5, r1	@ npix, n
	bgt	.L168		@,
.LVL189:
.L161:
	.loc 1 175 5 view .LVU679
.LBE26:
.LBE25:
@ armwave.c:404: }
	.loc 1 404 1 view .LVU680
	add	sp, sp, #12	@,,
	.cfi_remember_state
	.cfi_def_cfa_offset 36
	@ sp needed	@
	pop	{r4, r5, r6, r7, r8, r9, r10, fp, pc}	@
.LVL190:
.L180:
	.cfi_restore_state
.LBB28:
.LBB27:
@ armwave.c:186:                 if(value != 0) {
	.loc 1 186 19 view .LVU681
	ands	lr, r0, #255	@ _20, wave_word,
@ armwave.c:181:         if(COND_UNLIKELY(wave_word != 0)) {
	.loc 1 181 11 view .LVU682
	mov	r6, r1	@ ivtmp.112, n
.LVL191:
	.loc 1 183 17 is_stmt 1 view .LVU683
	.loc 1 184 17 view .LVU684
@ armwave.c:184:                 wave_word >>= 8;
	.loc 1 184 27 is_stmt 0 view .LVU685
	lsr	r0, r0, #8	@ wave_word, wave_word,
.LVL192:
	.loc 1 186 17 is_stmt 1 view .LVU686
	add	r1, r1, #4	@ n, ivtmp.112,
@ armwave.c:186:                 if(value != 0) {
	.loc 1 186 19 is_stmt 0 view .LVU687
	bne	.L184		@,
.L165:
	.loc 1 186 19 view .LVU688
	add	r6, r6, #1	@ ivtmp.112, ivtmp.112,
@ armwave.c:182:             for(w = 0; w < 4; w++) {
	.loc 1 182 13 view .LVU689
	cmp	r1, r6	@ n, ivtmp.112
	beq	.L164		@,
	.loc 1 183 17 is_stmt 1 view .LVU690
.LVL193:
	.loc 1 184 17 view .LVU691
@ armwave.c:186:                 if(value != 0) {
	.loc 1 186 19 is_stmt 0 view .LVU692
	ands	lr, r0, #255	@ _20, wave_word,
@ armwave.c:184:                 wave_word >>= 8;
	.loc 1 184 27 view .LVU693
	lsr	r0, r0, #8	@ wave_word, wave_word,
.LVL194:
	.loc 1 186 17 is_stmt 1 view .LVU694
@ armwave.c:186:                 if(value != 0) {
	.loc 1 186 19 is_stmt 0 view .LVU695
	beq	.L165		@,
.L184:
	.loc 1 187 21 is_stmt 1 view .LVU696
.LVL195:
	.loc 1 188 21 view .LVU697
	.loc 1 189 21 view .LVU698
	.loc 1 191 21 view .LVU699
	.loc 1 192 21 view .LVU700
	.loc 1 193 21 view .LVU701
	.loc 1 196 21 view .LVU702
@ armwave.c:189:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 189 55 is_stmt 0 view .LVU703
	ldr	r3, [sp, #4]	@ _28, %sfp
@ armwave.c:200:                     yy = (nsub & 0xff) * g_armwave_state.vscale;
	.loc 1 200 57 view .LVU704
	ldr	r10, [r2, #32]	@ _50, g_armwave_state.vscale
@ armwave.c:200:                     yy = (nsub & 0xff) * g_armwave_state.vscale;
	.loc 1 200 32 view .LVU705
	uxtb	r7, r6	@ ivtmp.112, ivtmp.112
@ armwave.c:189:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 189 55 view .LVU706
	mul	ip, r3, lr	@ tmp179, _28, _20
@ armwave.c:188:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 188 55 view .LVU707
	ldr	r3, [sp]	@ _24, %sfp
@ armwave.c:200:                     yy = (nsub & 0xff) * g_armwave_state.vscale;
	.loc 1 200 24 view .LVU708
	mul	r7, r10, r7	@ yy, _50, ivtmp.112
@ armwave.c:188:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 188 55 view .LVU709
	mul	r4, r3, lr	@ tmp183, _24, _20
@ armwave.c:189:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 189 24 view .LVU710
	asr	ip, ip, #8	@ bb, tmp179,
.LVL196:
@ armwave.c:187:                     rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 187 55 view .LVU711
	mul	lr, lr, fp	@ tmp189, _20, _18
.LVL197:
@ armwave.c:188:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 188 24 view .LVU712
	asr	r4, r4, #8	@ gg, tmp183,
.LVL198:
@ armwave.c:193:                     b = MIN(bb, 255);
	.loc 1 193 25 view .LVU713
	cmp	ip, #255	@ bb,
	movge	ip, #255	@ bb,
.LVL199:
@ armwave.c:192:                     g = MIN(gg, 255);
	.loc 1 192 25 view .LVU714
	cmp	r4, #255	@ gg,
	movge	r4, #255	@ gg,
.LVL200:
@ armwave.c:187:                     rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 187 24 view .LVU715
	asr	lr, lr, #8	@ rr, tmp189,
.LVL201:
@ armwave.c:191:                     r = MIN(rr, 255);
	.loc 1 191 25 view .LVU716
	cmp	lr, #255	@ rr,
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 56 view .LVU717
	lsl	r4, r4, #8	@ tmp184, gg,
@ armwave.c:191:                     r = MIN(rr, 255);
	.loc 1 191 25 view .LVU718
	movge	lr, #255	@ rr,
.LVL202:
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 44 view .LVU719
	lsl	r3, ip, #16	@ tmp180, bb,
	and	r3, r3, #16711680	@ tmp181, tmp180,
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 56 view .LVU720
	uxth	r4, r4	@ tmp185, tmp184
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 51 view .LVU721
	orr	r3, r3, r4	@ tmp187, tmp181, tmp185
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 62 view .LVU722
	uxtb	lr, lr	@ rr, rr
	orr	r3, r3, lr	@ tmp191, tmp187, rr
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 21 view .LVU723
	cmp	r10, #0	@ _50,
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 26 view .LVU724
	orr	r3, r3, #-16777216	@ word, tmp191,
.LVL203:
	.loc 1 199 21 is_stmt 1 view .LVU725
	.loc 1 200 21 view .LVU726
	.loc 1 201 21 view .LVU727
@ armwave.c:201:                     xx = (nsub >> 8);
	.loc 1 201 32 is_stmt 0 view .LVU728
	asr	r10, r6, #8	@ xx, ivtmp.112,
.LVL204:
	.loc 1 203 21 is_stmt 1 view .LVU729
	.loc 1 203 21 is_stmt 0 view .LVU730
	beq	.L165		@,
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 29 view .LVU731
	mov	ip, #0	@ row,
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 38 view .LVU732
	mov	lr, ip	@ _54, row
.LVL205:
.L166:
	.loc 1 204 25 is_stmt 1 view .LVU733
	.loc 1 205 25 view .LVU734
@ armwave.c:204:                         offset = (xx + ((yy + row) * g_armwave_state.target_width)); 
	.loc 1 204 52 is_stmt 0 view .LVU735
	ldr	r4, [r2, #72]	@ g_armwave_state.target_width, g_armwave_state.target_width
@ armwave.c:204:                         offset = (xx + ((yy + row) * g_armwave_state.target_width)); 
	.loc 1 204 45 view .LVU736
	add	lr, r7, lr	@ tmp194, yy, _54
.LVL206:
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 67 view .LVU737
	add	ip, ip, #1	@ tmp199, row,
.LVL207:
@ armwave.c:205:                         *(out_buffer_base + offset) = word;
	.loc 1 205 43 view .LVU738
	mla	r4, r4, lr, r10	@ tmp198, g_armwave_state.target_width, tmp194, xx
.LVL208:
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 67 view .LVU739
	uxtb	ip, ip	@ row, tmp199
.LVL209:
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 38 view .LVU740
	mov	lr, ip	@ _54, row
@ armwave.c:205:                         *(out_buffer_base + offset) = word;
	.loc 1 205 53 view .LVU741
	str	r3, [r8, r4, lsl #2]	@ word, *_60
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 21 view .LVU742
	ldr	r4, [r2, #32]	@ g_armwave_state.vscale, g_armwave_state.vscale
.LVL210:
	.loc 1 203 21 view .LVU743
	cmp	ip, r4	@ row, g_armwave_state.vscale
	bcc	.L166		@,
	.loc 1 203 21 view .LVU744
	b	.L165		@
.LVL211:
.L183:
	.loc 1 170 5 is_stmt 1 view .LVU745
	ldr	r3, .L185+4	@,
	mov	r2, #170	@,
	ldr	r1, .L185+8	@,
	ldr	r0, .L185+12	@,
	bl	__assert_fail		@
.LVL212:
.L186:
	.align	2
.L185:
	.word	g_armwave_state
	.word	.LANCHOR0+24
	.word	.LC4
	.word	.LC5
.LBE27:
.LBE28:
	.cfi_endproc
.LFE71:
	.size	armwave_test_fill_outbuf, .-armwave_test_fill_outbuf
	.align	2
	.global	armwave_test_dump_buffer_to_ppm
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_test_dump_buffer_to_ppm, %function
armwave_test_dump_buffer_to_ppm:
.LVL213:
.LFB72:
	.loc 1 410 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	.loc 1 411 5 view .LVU747
	ldr	r3, .L188	@ tmp112,
	mov	r1, r0	@, fn
	ldr	r0, [r3, #88]	@, g_armwave_state.out_pixbuf
.LVL214:
	.loc 1 411 5 is_stmt 0 view .LVU748
	b	armwave_dump_ppm_debug		@
.LVL215:
.L189:
	.loc 1 411 5 view .LVU749
	.align	2
.L188:
	.word	g_armwave_state
	.cfi_endproc
.LFE72:
	.size	armwave_test_dump_buffer_to_ppm, .-armwave_test_dump_buffer_to_ppm
	.align	2
	.global	armwave_test_fill_gdkbuf
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_test_fill_gdkbuf, %function
armwave_test_fill_gdkbuf:
.LVL216:
.LFB73:
	.loc 1 418 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 423 5 view .LVU751
@ armwave.c:423:     void *out_pixbuf = ((uint32_t ***)buf)[2][10];
	.loc 1 423 11 is_stmt 0 view .LVU752
	ldr	r3, [r0, #8]	@ MEM[(uint32_t * * *)buf_2(D) + 8B], MEM[(uint32_t * * *)buf_2(D) + 8B]
@ armwave.c:418: {
	.loc 1 418 1 view .LVU753
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
	sub	sp, sp, #12	@,,
	.cfi_def_cfa_offset 48
.LBB31:
.LBB32:
@ armwave.c:166:     uint32_t *base_32ptr = (uint32_t*)g_armwave_state.ch1_buffer;
	.loc 1 166 15 view .LVU754
	ldr	r2, .L214	@ tmp206,
.LBE32:
.LBE31:
@ armwave.c:423:     void *out_pixbuf = ((uint32_t ***)buf)[2][10];
	.loc 1 423 11 view .LVU755
	ldr	r8, [r3, #40]	@ out_pixbuf, MEM[(uint32_t * *)_1 + 40B]
.LVL217:
	.loc 1 426 5 is_stmt 1 view .LVU756
.LBB35:
.LBI31:
	.loc 1 161 6 view .LVU757
.LBB33:
	.loc 1 163 5 view .LVU758
	.loc 1 164 5 view .LVU759
	.loc 1 165 5 view .LVU760
	.loc 1 166 5 view .LVU761
@ armwave.c:170:     assert(out_buffer != NULL);
	.loc 1 170 5 is_stmt 0 view .LVU762
	cmp	r8, #0	@ out_pixbuf,
@ armwave.c:166:     uint32_t *base_32ptr = (uint32_t*)g_armwave_state.ch1_buffer;
	.loc 1 166 15 view .LVU763
	ldr	r9, [r2, #4]	@ base_32ptr, g_armwave_state.ch1_buffer
.LVL218:
	.loc 1 167 5 is_stmt 1 view .LVU764
	.loc 1 168 5 view .LVU765
	.loc 1 170 5 view .LVU766
	beq	.L212		@,
	.loc 1 172 5 view .LVU767
@ armwave.c:172:     npix = g_armwave_state.target_width * 256; 
	.loc 1 172 41 is_stmt 0 view .LVU768
	ldr	r5, [r2, #72]	@ g_armwave_state.target_width, g_armwave_state.target_width
	lsl	r5, r5, #8	@ npix, g_armwave_state.target_width,
.LVL219:
	.loc 1 173 5 is_stmt 1 view .LVU769
	.loc 1 175 5 view .LVU770
	.loc 1 175 5 is_stmt 0 view .LVU771
	cmp	r5, #0	@ npix,
	ble	.L190		@,
@ armwave.c:188:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 188 52 view .LVU772
	ldrsh	r3, [r2, #94]	@ _26, g_armwave_state.ch1_color.g
@ armwave.c:187:                     rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 187 52 view .LVU773
	ldrsh	fp, [r2, #92]	@ _20, g_armwave_state.ch1_color.r
@ armwave.c:175:     for(n = 0; n < npix; n += 4) {
	.loc 1 175 11 view .LVU774
	mov	r1, #0	@ n,
@ armwave.c:188:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 188 52 view .LVU775
	str	r3, [sp]	@ _26, %sfp
@ armwave.c:189:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 189 52 view .LVU776
	ldrsh	r3, [r2, #96]	@ _30, g_armwave_state.ch1_color.b
	str	r3, [sp, #4]	@ _30, %sfp
.LVL220:
.L197:
	.loc 1 179 9 is_stmt 1 view .LVU777
@ armwave.c:179:         wave_word = *base_32ptr++;
	.loc 1 179 19 is_stmt 0 view .LVU778
	ldr	r0, [r9], #4	@ wave_word, MEM[base: base_32ptr_11, offset: 4294967292B]
.LVL221:
	.loc 1 181 9 is_stmt 1 view .LVU779
@ armwave.c:181:         if(COND_UNLIKELY(wave_word != 0)) {
	.loc 1 181 11 is_stmt 0 view .LVU780
	cmp	r0, #0	@ wave_word,
	bne	.L209		@,
	add	r1, r1, #4	@ n, n,
.L193:
.LVL222:
@ armwave.c:175:     for(n = 0; n < npix; n += 4) {
	.loc 1 175 5 view .LVU781
	cmp	r5, r1	@ npix, n
	bgt	.L197		@,
.LVL223:
.L190:
	.loc 1 175 5 view .LVU782
.LBE33:
.LBE35:
@ armwave.c:427: }
	.loc 1 427 1 view .LVU783
	add	sp, sp, #12	@,,
	.cfi_remember_state
	.cfi_def_cfa_offset 36
	@ sp needed	@
	pop	{r4, r5, r6, r7, r8, r9, r10, fp, pc}	@
.LVL224:
.L209:
	.cfi_restore_state
.LBB36:
.LBB34:
@ armwave.c:186:                 if(value != 0) {
	.loc 1 186 19 view .LVU784
	ands	lr, r0, #255	@ _22, wave_word,
@ armwave.c:181:         if(COND_UNLIKELY(wave_word != 0)) {
	.loc 1 181 11 view .LVU785
	mov	r6, r1	@ ivtmp.128, n
.LVL225:
	.loc 1 183 17 is_stmt 1 view .LVU786
	.loc 1 184 17 view .LVU787
@ armwave.c:184:                 wave_word >>= 8;
	.loc 1 184 27 is_stmt 0 view .LVU788
	lsr	r0, r0, #8	@ wave_word, wave_word,
.LVL226:
	.loc 1 186 17 is_stmt 1 view .LVU789
	add	r1, r1, #4	@ n, ivtmp.128,
@ armwave.c:186:                 if(value != 0) {
	.loc 1 186 19 is_stmt 0 view .LVU790
	bne	.L213		@,
.L194:
	.loc 1 186 19 view .LVU791
	add	r6, r6, #1	@ ivtmp.128, ivtmp.128,
@ armwave.c:182:             for(w = 0; w < 4; w++) {
	.loc 1 182 13 view .LVU792
	cmp	r1, r6	@ n, ivtmp.128
	beq	.L193		@,
	.loc 1 183 17 is_stmt 1 view .LVU793
.LVL227:
	.loc 1 184 17 view .LVU794
@ armwave.c:186:                 if(value != 0) {
	.loc 1 186 19 is_stmt 0 view .LVU795
	ands	lr, r0, #255	@ _22, wave_word,
@ armwave.c:184:                 wave_word >>= 8;
	.loc 1 184 27 view .LVU796
	lsr	r0, r0, #8	@ wave_word, wave_word,
.LVL228:
	.loc 1 186 17 is_stmt 1 view .LVU797
@ armwave.c:186:                 if(value != 0) {
	.loc 1 186 19 is_stmt 0 view .LVU798
	beq	.L194		@,
.L213:
	.loc 1 187 21 is_stmt 1 view .LVU799
.LVL229:
	.loc 1 188 21 view .LVU800
	.loc 1 189 21 view .LVU801
	.loc 1 191 21 view .LVU802
	.loc 1 192 21 view .LVU803
	.loc 1 193 21 view .LVU804
	.loc 1 196 21 view .LVU805
@ armwave.c:189:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 189 55 is_stmt 0 view .LVU806
	ldr	r3, [sp, #4]	@ _30, %sfp
@ armwave.c:200:                     yy = (nsub & 0xff) * g_armwave_state.vscale;
	.loc 1 200 57 view .LVU807
	ldr	r10, [r2, #32]	@ _52, g_armwave_state.vscale
@ armwave.c:200:                     yy = (nsub & 0xff) * g_armwave_state.vscale;
	.loc 1 200 32 view .LVU808
	uxtb	r7, r6	@ ivtmp.128, ivtmp.128
@ armwave.c:189:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 189 55 view .LVU809
	mul	ip, r3, lr	@ tmp181, _30, _22
@ armwave.c:188:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 188 55 view .LVU810
	ldr	r3, [sp]	@ _26, %sfp
@ armwave.c:200:                     yy = (nsub & 0xff) * g_armwave_state.vscale;
	.loc 1 200 24 view .LVU811
	mul	r7, r10, r7	@ yy, _52, ivtmp.128
@ armwave.c:188:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 188 55 view .LVU812
	mul	r4, r3, lr	@ tmp185, _26, _22
@ armwave.c:189:                     bb = (g_armwave_state.ch1_color.b * value) >> 8;
	.loc 1 189 24 view .LVU813
	asr	ip, ip, #8	@ bb, tmp181,
.LVL230:
@ armwave.c:187:                     rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 187 55 view .LVU814
	mul	lr, lr, fp	@ tmp191, _22, _20
.LVL231:
@ armwave.c:188:                     gg = (g_armwave_state.ch1_color.g * value) >> 8;
	.loc 1 188 24 view .LVU815
	asr	r4, r4, #8	@ gg, tmp185,
.LVL232:
@ armwave.c:193:                     b = MIN(bb, 255);
	.loc 1 193 25 view .LVU816
	cmp	ip, #255	@ bb,
	movge	ip, #255	@ bb,
.LVL233:
@ armwave.c:192:                     g = MIN(gg, 255);
	.loc 1 192 25 view .LVU817
	cmp	r4, #255	@ gg,
	movge	r4, #255	@ gg,
.LVL234:
@ armwave.c:187:                     rr = (g_armwave_state.ch1_color.r * value) >> 8;
	.loc 1 187 24 view .LVU818
	asr	lr, lr, #8	@ rr, tmp191,
.LVL235:
@ armwave.c:191:                     r = MIN(rr, 255);
	.loc 1 191 25 view .LVU819
	cmp	lr, #255	@ rr,
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 56 view .LVU820
	lsl	r4, r4, #8	@ tmp186, gg,
@ armwave.c:191:                     r = MIN(rr, 255);
	.loc 1 191 25 view .LVU821
	movge	lr, #255	@ rr,
.LVL236:
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 44 view .LVU822
	lsl	r3, ip, #16	@ tmp182, bb,
	and	r3, r3, #16711680	@ tmp183, tmp182,
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 56 view .LVU823
	uxth	r4, r4	@ tmp187, tmp186
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 51 view .LVU824
	orr	r3, r3, r4	@ tmp189, tmp183, tmp187
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 62 view .LVU825
	uxtb	lr, lr	@ rr, rr
	orr	r3, r3, lr	@ tmp193, tmp189, rr
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 21 view .LVU826
	cmp	r10, #0	@ _52,
@ armwave.c:196:                     word = 0xff000000 | (b << 16) | (g << 8) | r;
	.loc 1 196 26 view .LVU827
	orr	r3, r3, #-16777216	@ word, tmp193,
.LVL237:
	.loc 1 199 21 is_stmt 1 view .LVU828
	.loc 1 200 21 view .LVU829
	.loc 1 201 21 view .LVU830
@ armwave.c:201:                     xx = (nsub >> 8);
	.loc 1 201 32 is_stmt 0 view .LVU831
	asr	r10, r6, #8	@ xx, ivtmp.128,
.LVL238:
	.loc 1 203 21 is_stmt 1 view .LVU832
	.loc 1 203 21 is_stmt 0 view .LVU833
	beq	.L194		@,
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 29 view .LVU834
	mov	ip, #0	@ row,
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 38 view .LVU835
	mov	lr, ip	@ _56, row
.LVL239:
.L195:
	.loc 1 204 25 is_stmt 1 view .LVU836
	.loc 1 205 25 view .LVU837
@ armwave.c:204:                         offset = (xx + ((yy + row) * g_armwave_state.target_width)); 
	.loc 1 204 52 is_stmt 0 view .LVU838
	ldr	r4, [r2, #72]	@ g_armwave_state.target_width, g_armwave_state.target_width
@ armwave.c:204:                         offset = (xx + ((yy + row) * g_armwave_state.target_width)); 
	.loc 1 204 45 view .LVU839
	add	lr, r7, lr	@ tmp196, yy, _56
.LVL240:
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 67 view .LVU840
	add	ip, ip, #1	@ tmp201, row,
.LVL241:
@ armwave.c:205:                         *(out_buffer_base + offset) = word;
	.loc 1 205 43 view .LVU841
	mla	r4, r4, lr, r10	@ tmp200, g_armwave_state.target_width, tmp196, xx
.LVL242:
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 67 view .LVU842
	uxtb	ip, ip	@ row, tmp201
.LVL243:
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 38 view .LVU843
	mov	lr, ip	@ _56, row
@ armwave.c:205:                         *(out_buffer_base + offset) = word;
	.loc 1 205 53 view .LVU844
	str	r3, [r8, r4, lsl #2]	@ word, *_62
@ armwave.c:203:                     for(row = 0; row < g_armwave_state.vscale; row++) {
	.loc 1 203 21 view .LVU845
	ldr	r4, [r2, #32]	@ g_armwave_state.vscale, g_armwave_state.vscale
.LVL244:
	.loc 1 203 21 view .LVU846
	cmp	ip, r4	@ row, g_armwave_state.vscale
	bcc	.L195		@,
	.loc 1 203 21 view .LVU847
	b	.L194		@
.LVL245:
.L212:
	.loc 1 170 5 is_stmt 1 view .LVU848
	ldr	r3, .L214+4	@,
	mov	r2, #170	@,
	ldr	r1, .L214+8	@,
	ldr	r0, .L214+12	@,
.LVL246:
	.loc 1 170 5 is_stmt 0 view .LVU849
	bl	__assert_fail		@
.LVL247:
.L215:
	.align	2
.L214:
	.word	g_armwave_state
	.word	.LANCHOR0+24
	.word	.LC4
	.word	.LC5
.LBE34:
.LBE36:
	.cfi_endproc
.LFE73:
	.size	armwave_test_fill_gdkbuf, .-armwave_test_fill_gdkbuf
	.align	2
	.global	armwave_test_buffer_alloc
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_test_buffer_alloc, %function
armwave_test_buffer_alloc:
.LFB74:
	.loc 1 433 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 434 5 view .LVU851
@ armwave.c:433: {
	.loc 1 433 1 is_stmt 0 view .LVU852
	push	{r4, r5, r6, lr}	@
	.cfi_def_cfa_offset 16
	.cfi_offset 4, -16
	.cfi_offset 5, -12
	.cfi_offset 6, -8
	.cfi_offset 14, -4
@ armwave.c:434:     if(g_armwave_state.test_wave_buffer != NULL) {
	.loc 1 434 23 view .LVU853
	ldr	r4, .L223	@ tmp130,
	ldr	r0, [r4, #24]	@ _1, g_armwave_state.test_wave_buffer
@ armwave.c:434:     if(g_armwave_state.test_wave_buffer != NULL) {
	.loc 1 434 7 view .LVU854
	cmp	r0, #0	@ _1,
	beq	.L217		@,
	.loc 1 435 9 is_stmt 1 view .LVU855
	bl	free		@
.LVL248:
.L217:
	.loc 1 438 5 view .LVU856
	ldr	r2, [r4, #44]	@, g_armwave_state.waves_max
	ldr	r1, [r4, #60]	@, g_armwave_state.wave_length
	ldr	r0, .L223+4	@,
	bl	printf		@
.LVL249:
	.loc 1 440 5 view .LVU857
@ armwave.c:440:     g_armwave_state.test_wave_buffer = calloc(g_armwave_state.wave_length * g_armwave_state.waves_max, 1);
	.loc 1 440 40 is_stmt 0 view .LVU858
	ldr	r3, [r4, #60]	@ g_armwave_state.wave_length, g_armwave_state.wave_length
	ldr	r5, [r4, #44]	@ g_armwave_state.waves_max, g_armwave_state.waves_max
	mov	r1, #1	@,
	mul	r5, r5, r3	@ _6, g_armwave_state.waves_max, g_armwave_state.wave_length
	mov	r0, r5	@, _6
	bl	calloc		@
.LVL250:
@ armwave.c:442:     if(g_armwave_state.test_wave_buffer == NULL) {
	.loc 1 442 7 view .LVU859
	cmp	r0, #0	@ tmp127,
@ armwave.c:440:     g_armwave_state.test_wave_buffer = calloc(g_armwave_state.wave_length * g_armwave_state.waves_max, 1);
	.loc 1 440 38 view .LVU860
	str	r0, [r4, #24]	@ tmp127, g_armwave_state.test_wave_buffer
	.loc 1 442 5 is_stmt 1 view .LVU861
@ armwave.c:442:     if(g_armwave_state.test_wave_buffer == NULL) {
	.loc 1 442 7 is_stmt 0 view .LVU862
	popne	{r4, r5, r6, pc}	@
	.loc 1 443 9 is_stmt 1 view .LVU863
	mov	r1, r5	@, _6
	ldr	r0, .L223+8	@,
@ armwave.c:446: }
	.loc 1 446 1 is_stmt 0 view .LVU864
	pop	{r4, r5, r6, lr}	@
	.cfi_restore 14
	.cfi_restore 6
	.cfi_restore 5
	.cfi_restore 4
	.cfi_def_cfa_offset 0
@ armwave.c:443:         printf("armwave_test_buffer_alloc: failed to allocate test wave buffer (%d bytes)\n", g_armwave_state.wave_length * g_armwave_state.waves_max);
	.loc 1 443 9 view .LVU865
	b	printf		@
.LVL251:
.L224:
	.align	2
.L223:
	.word	g_armwave_state
	.word	.LC20
	.word	.LC21
	.cfi_endproc
.LFE74:
	.size	armwave_test_buffer_alloc, .-armwave_test_buffer_alloc
	.align	2
	.global	armwave_fill_pixbuf_into_pybuffer
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_fill_pixbuf_into_pybuffer, %function
armwave_fill_pixbuf_into_pybuffer:
.LVL252:
.LFB75:
	.loc 1 452 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 48
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 453 5 view .LVU867
	.loc 1 454 5 view .LVU868
	.loc 1 456 5 view .LVU869
@ armwave.c:452: {
	.loc 1 452 1 is_stmt 0 view .LVU870
	push	{r4, lr}	@
	.cfi_def_cfa_offset 8
	.cfi_offset 4, -8
	.cfi_offset 14, -4
	mov	r4, r0	@ buf_obj, buf_obj
	sub	sp, sp, #48	@,,
	.cfi_def_cfa_offset 56
@ armwave.c:456:     printf("armwave_fill_pixbuf_into_pybuffer: start\n");
	.loc 1 456 5 view .LVU871
	ldr	r0, .L229	@,
.LVL253:
	.loc 1 456 5 view .LVU872
	bl	puts		@
.LVL254:
	.loc 1 458 5 is_stmt 1 view .LVU873
@ armwave.c:458:     ret = PyObject_GetBuffer(buf_obj, &buffer, PyBUF_SIMPLE | PyBUF_WRITABLE);
	.loc 1 458 11 is_stmt 0 view .LVU874
	mov	r0, r4	@, buf_obj
	add	r1, sp, #4	@ tmp128,,
	mov	r2, #1	@,
	bl	PyObject_GetBuffer		@
.LVL255:
	.loc 1 460 5 is_stmt 1 view .LVU875
@ armwave.c:460:     if(ret != 0) {
	.loc 1 460 7 is_stmt 0 view .LVU876
	cmp	r0, #0	@,
	bne	.L228		@,
	.loc 1 467 5 is_stmt 1 view .LVU877
	ldr	r0, [sp, #4]	@, buffer.buf
.LVL256:
	.loc 1 467 5 is_stmt 0 view .LVU878
	bl	armwave_fill_pixbuf_scaled		@
.LVL257:
	.loc 1 468 5 is_stmt 1 view .LVU879
	ldr	r0, .L229+4	@,
	bl	puts		@
.LVL258:
	.loc 1 470 5 view .LVU880
	add	r0, sp, #4	@ tmp129,,
	bl	PyBuffer_Release		@
.LVL259:
	.loc 1 471 5 view .LVU881
	ldr	r0, .L229+8	@,
	bl	puts		@
.LVL260:
@ armwave.c:472: }
	.loc 1 472 1 is_stmt 0 view .LVU882
	add	sp, sp, #48	@,,
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	@ sp needed	@
	pop	{r4, pc}	@
.LVL261:
.L228:
	.cfi_restore_state
	.loc 1 461 9 is_stmt 1 view .LVU883
	ldr	r0, .L229+12	@,
.LVL262:
	.loc 1 461 9 is_stmt 0 view .LVU884
	bl	puts		@
.LVL263:
	.loc 1 462 9 is_stmt 1 view .LVU885
.LBB37:
.LBI37:
	.file 2 "/usr/local/include/python3.8/object.h"
	.loc 2 456 20 view .LVU886
.LBB38:
	.loc 2 458 21 view .LVU887
	.loc 2 459 5 view .LVU888
@ /usr/local/include/python3.8/object.h:459:     op->ob_refcnt++;
	.loc 2 459 18 is_stmt 0 view .LVU889
	ldr	r3, .L229+16	@ tmp119,
.LBE38:
.LBE37:
@ armwave.c:462:         Py_RETURN_FALSE;
	.loc 1 462 9 view .LVU890
	mov	r0, r3	@ <retval>, tmp119
.LBB40:
.LBB39:
@ /usr/local/include/python3.8/object.h:459:     op->ob_refcnt++;
	.loc 2 459 18 view .LVU891
	ldr	r2, [r3]	@ MEM[(Py_ssize_t *)&_Py_FalseStruct], MEM[(Py_ssize_t *)&_Py_FalseStruct]
	add	r2, r2, #1	@ tmp121, MEM[(Py_ssize_t *)&_Py_FalseStruct],
	str	r2, [r3]	@ tmp121, MEM[(Py_ssize_t *)&_Py_FalseStruct]
.LBE39:
.LBE40:
@ armwave.c:472: }
	.loc 1 472 1 view .LVU892
	add	sp, sp, #48	@,,
	.cfi_def_cfa_offset 8
	@ sp needed	@
	pop	{r4, pc}	@
.LVL264:
.L230:
	.loc 1 472 1 view .LVU893
	.align	2
.L229:
	.word	.LC22
	.word	.LC24
	.word	.LC25
	.word	.LC23
	.word	_Py_FalseStruct
	.cfi_endproc
.LFE75:
	.size	armwave_fill_pixbuf_into_pybuffer, .-armwave_fill_pixbuf_into_pybuffer
	.align	2
	.global	armwave_test_create_am_sine
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_test_create_am_sine, %function
armwave_test_create_am_sine:
.LVL265:
.LFB76:
	.loc 1 481 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 482 5 view .LVU895
	.loc 1 483 5 view .LVU896
	.loc 1 485 5 view .LVU897
.LBB43:
.LBI43:
	.loc 1 432 6 view .LVU898
.LBB44:
	.loc 1 434 5 view .LVU899
.LBE44:
.LBE43:
@ armwave.c:481: {
	.loc 1 481 1 is_stmt 0 view .LVU900
	push	{r4, r5, r6, lr}	@
	.cfi_def_cfa_offset 16
	.cfi_offset 4, -16
	.cfi_offset 5, -12
	.cfi_offset 6, -8
	.cfi_offset 14, -4
	vpush.64	{d8, d9, d10, d11, d12, d13, d14}	@
	.cfi_def_cfa_offset 72
	.cfi_offset 80, -72
	.cfi_offset 81, -68
	.cfi_offset 82, -64
	.cfi_offset 83, -60
	.cfi_offset 84, -56
	.cfi_offset 85, -52
	.cfi_offset 86, -48
	.cfi_offset 87, -44
	.cfi_offset 88, -40
	.cfi_offset 89, -36
	.cfi_offset 90, -32
	.cfi_offset 91, -28
	.cfi_offset 92, -24
	.cfi_offset 93, -20
.LBB49:
.LBB45:
@ armwave.c:434:     if(g_armwave_state.test_wave_buffer != NULL) {
	.loc 1 434 23 view .LVU901
	ldr	r5, .L262+32	@ tmp232,
.LBE45:
.LBE49:
@ armwave.c:481: {
	.loc 1 481 1 view .LVU902
	vmov.f32	s25, s0	@ mod, mod
	vmov.f32	s22, s1	@ noise_fraction, noise_fraction
.LBB50:
.LBB46:
@ armwave.c:434:     if(g_armwave_state.test_wave_buffer != NULL) {
	.loc 1 434 23 view .LVU903
	ldr	r0, [r5, #24]	@ _63, g_armwave_state.test_wave_buffer
.LBE46:
.LBE50:
@ armwave.c:481: {
	.loc 1 481 1 view .LVU904
	sub	sp, sp, #8	@,,
	.cfi_def_cfa_offset 80
.LBB51:
.LBB47:
@ armwave.c:434:     if(g_armwave_state.test_wave_buffer != NULL) {
	.loc 1 434 7 view .LVU905
	cmp	r0, #0	@ _63,
	beq	.L232		@,
	.loc 1 435 9 is_stmt 1 view .LVU906
	bl	free		@
.LVL266:
.L232:
	.loc 1 438 5 view .LVU907
	ldr	r2, [r5, #44]	@, g_armwave_state.waves_max
	ldr	r1, [r5, #60]	@, g_armwave_state.wave_length
	ldr	r0, .L262+36	@,
	bl	printf		@
.LVL267:
	.loc 1 440 5 view .LVU908
@ armwave.c:440:     g_armwave_state.test_wave_buffer = calloc(g_armwave_state.wave_length * g_armwave_state.waves_max, 1);
	.loc 1 440 40 is_stmt 0 view .LVU909
	ldr	r3, [r5, #60]	@ g_armwave_state.wave_length, g_armwave_state.wave_length
	ldr	r4, [r5, #44]	@ g_armwave_state.waves_max, g_armwave_state.waves_max
	mov	r1, #1	@,
	mul	r4, r4, r3	@ _68, g_armwave_state.waves_max, g_armwave_state.wave_length
	mov	r0, r4	@, _68
	bl	calloc		@
.LVL268:
@ armwave.c:442:     if(g_armwave_state.test_wave_buffer == NULL) {
	.loc 1 442 7 view .LVU910
	cmp	r0, #0	@ tmp177,
@ armwave.c:440:     g_armwave_state.test_wave_buffer = calloc(g_armwave_state.wave_length * g_armwave_state.waves_max, 1);
	.loc 1 440 38 view .LVU911
	str	r0, [r5, #24]	@ tmp177, g_armwave_state.test_wave_buffer
	.loc 1 442 5 is_stmt 1 view .LVU912
@ armwave.c:442:     if(g_armwave_state.test_wave_buffer == NULL) {
	.loc 1 442 7 is_stmt 0 view .LVU913
	beq	.L261		@,
.L233:
.LVL269:
	.loc 1 442 7 view .LVU914
.LBE47:
.LBE51:
@ armwave.c:487:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 487 35 discriminator 1 view .LVU915
	ldr	r2, [r5, #40]	@ prephitmp_80, g_armwave_state.waves
@ armwave.c:487:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 487 5 discriminator 1 view .LVU916
	cmp	r2, #0	@ prephitmp_80,
	beq	.L231		@,
@ armwave.c:488:         mod_val = 0.5f + (((float)w / TEST_NWAVES) * mod);
	.loc 1 488 37 view .LVU917
	vldr.32	s27, .L262	@ tmp184,
@ armwave.c:488:         mod_val = 0.5f + (((float)w / TEST_NWAVES) * mod);
	.loc 1 488 17 view .LVU918
	vldr.32	s26, .L262+4	@ tmp186,
@ armwave.c:503:             v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 503 41 view .LVU919
	vldr.32	s16, .L262+8	@ tmp234,
@ armwave.c:503:             v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 503 29 view .LVU920
	vldr.32	s23, .L262+12	@ tmp235,
@ armwave.c:506:             g_armwave_state.test_wave_buffer[x + (w * g_armwave_state.wave_stride)] = (uint8_t)MIN(MAX(128 + (v * 127), 0), 255);
	.loc 1 506 87 view .LVU921
	vldr.32	s24, .L262+16	@ tmp236,
	ldr	r3, [r5, #60]	@ prephitmp_96, g_armwave_state.wave_length
@ armwave.c:487:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 487 5 view .LVU922
	mov	r6, #0	@ w,
.LVL270:
.L241:
	.loc 1 488 9 is_stmt 1 view .LVU923
@ armwave.c:488:         mod_val = 0.5f + (((float)w / TEST_NWAVES) * mod);
	.loc 1 488 28 is_stmt 0 view .LVU924
	vmov	s15, r6	@ int	@ w, w
@ armwave.c:488:         mod_val = 0.5f + (((float)w / TEST_NWAVES) * mod);
	.loc 1 488 17 view .LVU925
	vmov.f32	s18, s26	@ mod_val, tmp186
@ armwave.c:491:         for(x = 0; x < g_armwave_state.wave_length; x++) {
	.loc 1 491 9 view .LVU926
	cmp	r3, #0	@ prephitmp_96,
@ armwave.c:488:         mod_val = 0.5f + (((float)w / TEST_NWAVES) * mod);
	.loc 1 488 28 view .LVU927
	vcvt.f32.s32	s15, s15	@ tmp182, w
@ armwave.c:488:         mod_val = 0.5f + (((float)w / TEST_NWAVES) * mod);
	.loc 1 488 37 view .LVU928
	vmul.f32	s15, s15, s27	@ tmp183, tmp182, tmp184
@ armwave.c:488:         mod_val = 0.5f + (((float)w / TEST_NWAVES) * mod);
	.loc 1 488 17 view .LVU929
	vmla.f32	s18, s15, s25	@ mod_val, tmp183, mod
.LVL271:
	.loc 1 491 9 is_stmt 1 view .LVU930
	.loc 1 491 9 is_stmt 0 view .LVU931
	beq	.L235		@,
	vcvt.f64.f32	d9, s18	@ tmp231, mod_val
@ armwave.c:501:             xnoise = (rand() & 0xffff) / 6553500.0f;
	.loc 1 501 20 view .LVU932
	vldr.32	s21, .L262+20	@ tmp204,
@ armwave.c:506:             g_armwave_state.test_wave_buffer[x + (w * g_armwave_state.wave_stride)] = (uint8_t)MIN(MAX(128 + (v * 127), 0), 255);
	.loc 1 506 96 view .LVU933
	vldr.32	s20, .L262+24	@ tmp215,
	vldr.32	s17, .L262+28	@ tmp216,
@ armwave.c:491:         for(x = 0; x < g_armwave_state.wave_length; x++) {
	.loc 1 491 15 view .LVU934
	mov	r4, #0	@ x,
.LVL272:
.L240:
	.loc 1 492 13 is_stmt 1 view .LVU935
@ armwave.c:492:             noise  = ((rand() & 0xffff) * noise_fraction);
	.loc 1 492 24 is_stmt 0 view .LVU936
	bl	rand		@
.LVL273:
@ armwave.c:492:             noise  = ((rand() & 0xffff) * noise_fraction);
	.loc 1 492 31 view .LVU937
	uxth	r0, r0	@ tmp187,
@ armwave.c:492:             noise  = ((rand() & 0xffff) * noise_fraction);
	.loc 1 492 41 view .LVU938
	vmov	s15, r0	@ int	@ tmp187, tmp187
	vcvt.f32.s32	s15, s15	@ tmp189, tmp187
@ armwave.c:492:             noise  = ((rand() & 0xffff) * noise_fraction);
	.loc 1 492 20 view .LVU939
	vmul.f32	s15, s15, s22	@ noise, tmp189, noise_fraction
.LVL274:
	.loc 1 493 13 is_stmt 1 view .LVU940
@ armwave.c:493:             noise *= noise;
	.loc 1 493 19 is_stmt 0 view .LVU941
	vmul.f32	s15, s15, s15	@ noise, noise, noise
.LVL275:
	.loc 1 494 13 is_stmt 1 view .LVU942
@ armwave.c:494:             noise *= noise;
	.loc 1 494 19 is_stmt 0 view .LVU943
	vmul.f32	s15, s15, s15	@ noise, noise, noise
.LVL276:
	.loc 1 495 13 is_stmt 1 view .LVU944
@ armwave.c:495:             noise *= noise;
	.loc 1 495 19 is_stmt 0 view .LVU945
	vmul.f32	s28, s15, s15	@ noise, noise, noise
.LVL277:
	.loc 1 497 13 is_stmt 1 view .LVU946
@ armwave.c:497:             if((rand() & 0xffff) > 0x7fff)
	.loc 1 497 17 is_stmt 0 view .LVU947
	bl	rand		@
.LVL278:
	.loc 1 498 17 is_stmt 1 view .LVU948
@ armwave.c:497:             if((rand() & 0xffff) > 0x7fff)
	.loc 1 497 15 is_stmt 0 view .LVU949
	tst	r0, #32768	@,
@ armwave.c:498:                 noise = -noise;
	.loc 1 498 23 view .LVU950
	vnegne.f32	s28, s28	@ noise, noise
.LVL279:
	.loc 1 500 13 is_stmt 1 view .LVU951
	.loc 1 501 13 view .LVU952
@ armwave.c:501:             xnoise = (rand() & 0xffff) / 6553500.0f;
	.loc 1 501 23 is_stmt 0 view .LVU953
	bl	rand		@
.LVL280:
	.loc 1 503 13 is_stmt 1 view .LVU954
@ armwave.c:503:             v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 503 41 is_stmt 0 view .LVU955
	vldr.32	s14, [r5, #60]	@ int	@ tmp242, g_armwave_state.wave_length
@ armwave.c:503:             v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 503 29 view .LVU956
	vmov	s15, r4	@ int	@ x, x
@ armwave.c:503:             v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 503 41 view .LVU957
	vcvt.f32.u32	s14, s14	@ tmp192, tmp242
@ armwave.c:503:             v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 503 29 view .LVU958
	vcvt.f32.s32	s15, s15	@ tmp196, x
@ armwave.c:503:             v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 503 41 view .LVU959
	vdiv.f32	s13, s16, s14	@ tmp194, tmp234, tmp192
@ armwave.c:503:             v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 503 29 view .LVU960
	vmul.f32	s15, s15, s23	@ tmp197, tmp196, tmp235
@ armwave.c:501:             xnoise = (rand() & 0xffff) / 6553500.0f;
	.loc 1 501 30 view .LVU961
	uxth	r0, r0	@ tmp200,
.LVL281:
@ armwave.c:501:             xnoise = (rand() & 0xffff) / 6553500.0f;
	.loc 1 501 40 view .LVU962
	vmov	s14, r0	@ int	@ tmp200, tmp200
	vcvt.f32.s32	s14, s14	@ tmp202, tmp200
@ armwave.c:501:             xnoise = (rand() & 0xffff) / 6553500.0f;
	.loc 1 501 20 view .LVU963
	vdiv.f32	s0, s14, s21	@ xnoise, tmp202, tmp204
@ armwave.c:503:             v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 503 73 view .LVU964
	vmla.f32	s0, s13, s15	@ tmp205, tmp194, tmp197
@ armwave.c:503:             v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 503 18 view .LVU965
	vcvt.f64.f32	d0, s0	@, tmp205
	bl	sin		@
.LVL282:
	.loc 1 506 13 is_stmt 1 view .LVU966
@ armwave.c:500:             noise += 1.0f;
	.loc 1 500 19 is_stmt 0 view .LVU967
	vadd.f32	s15, s28, s16	@ noise, noise, tmp234
@ armwave.c:506:             g_armwave_state.test_wave_buffer[x + (w * g_armwave_state.wave_stride)] = (uint8_t)MIN(MAX(128 + (v * 127), 0), 255);
	.loc 1 506 87 view .LVU968
	mov	r2, #0	@ iftmp.22_39,
@ armwave.c:503:             v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 503 94 view .LVU969
	vcvt.f64.f32	d7, s15	@ tmp211, noise
@ armwave.c:503:             v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 503 83 view .LVU970
	vmul.f64	d0, d9, d0	@ tmp208, tmp231,
.LVL283:
@ armwave.c:503:             v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 503 94 view .LVU971
	vmul.f64	d7, d0, d7	@ tmp212, tmp208, tmp211
@ armwave.c:503:             v = (sin((6.28f * x * (1.0f / g_armwave_state.wave_length)) + xnoise) * mod_val) * noise;
	.loc 1 503 15 view .LVU972
	vcvt.f32.f64	s14, d7	@ v, tmp212
@ armwave.c:506:             g_armwave_state.test_wave_buffer[x + (w * g_armwave_state.wave_stride)] = (uint8_t)MIN(MAX(128 + (v * 127), 0), 255);
	.loc 1 506 96 view .LVU973
	vmov.f32	s15, s17	@ _26, tmp216
	vmla.f32	s15, s14, s20	@ _26, v, tmp215
	vcmpe.f32	s15, #0	@ _26
	vmrs	APSR_nzcv, FPSCR
	ble	.L237		@,
@ armwave.c:506:             g_armwave_state.test_wave_buffer[x + (w * g_armwave_state.wave_stride)] = (uint8_t)MIN(MAX(128 + (v * 127), 0), 255);
	.loc 1 506 87 discriminator 1 view .LVU974
	vcmpe.f32	s15, s24	@ _26, tmp236
	mov	r2, #255	@ iftmp.22_39,
	vmrs	APSR_nzcv, FPSCR
	vcvtmi.u32.f32	s15, s15	@ tmp220, _26
	vstrmi.32	s15, [sp, #4]	@ int	@ tmp220, %sfp
	ldrbmi	r2, [sp, #4]	@ zero_extendqisi2	@ iftmp.22_39, %sfp
.L237:
@ armwave.c:506:             g_armwave_state.test_wave_buffer[x + (w * g_armwave_state.wave_stride)] = (uint8_t)MIN(MAX(128 + (v * 127), 0), 255);
	.loc 1 506 53 discriminator 12 view .LVU975
	ldr	r3, [r5, #36]	@ g_armwave_state.wave_stride, g_armwave_state.wave_stride
@ armwave.c:506:             g_armwave_state.test_wave_buffer[x + (w * g_armwave_state.wave_stride)] = (uint8_t)MIN(MAX(128 + (v * 127), 0), 255);
	.loc 1 506 85 discriminator 12 view .LVU976
	ldr	r1, [r5, #24]	@ g_armwave_state.test_wave_buffer, g_armwave_state.test_wave_buffer
	mla	r3, r3, r6, r1	@ tmp226, g_armwave_state.wave_stride, w, g_armwave_state.test_wave_buffer
	strb	r2, [r3, r4]	@ iftmp.22_39, *_31
@ armwave.c:491:         for(x = 0; x < g_armwave_state.wave_length; x++) {
	.loc 1 491 39 discriminator 12 view .LVU977
	ldr	r3, [r5, #60]	@ prephitmp_96, g_armwave_state.wave_length
@ armwave.c:491:         for(x = 0; x < g_armwave_state.wave_length; x++) {
	.loc 1 491 54 discriminator 12 view .LVU978
	add	r4, r4, #1	@ x, x,
.LVL284:
@ armwave.c:491:         for(x = 0; x < g_armwave_state.wave_length; x++) {
	.loc 1 491 9 discriminator 12 view .LVU979
	cmp	r3, r4	@ prephitmp_96, x
	bhi	.L240		@,
	ldr	r2, [r5, #40]	@ prephitmp_80, g_armwave_state.waves
.LVL285:
.L235:
@ armwave.c:487:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 487 44 discriminator 2 view .LVU980
	add	r6, r6, #1	@ w, w,
.LVL286:
@ armwave.c:487:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 487 5 discriminator 2 view .LVU981
	cmp	r6, r2	@ w, prephitmp_80
	bcc	.L241		@,
.LVL287:
.L231:
@ armwave.c:509: }
	.loc 1 509 1 view .LVU982
	add	sp, sp, #8	@,,
	.cfi_remember_state
	.cfi_def_cfa_offset 72
	@ sp needed	@
	vldm	sp!, {d8-d14}	@,
	.cfi_restore 92
	.cfi_restore 93
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
	.cfi_def_cfa_offset 16
.LVL288:
	.loc 1 509 1 view .LVU983
	pop	{r4, r5, r6, pc}	@
.LVL289:
.L261:
	.cfi_restore_state
.LBB52:
.LBB48:
	.loc 1 443 9 is_stmt 1 view .LVU984
	mov	r1, r4	@, _68
	ldr	r0, .L262+40	@,
	bl	printf		@
.LVL290:
	.loc 1 444 9 view .LVU985
	b	.L233		@
.L263:
	.align	2
.L262:
	.word	1015021568
	.word	1056964608
	.word	1065353216
	.word	1086911939
	.word	1132396544
	.word	1254620984
	.word	1123942400
	.word	1124073472
	.word	g_armwave_state
	.word	.LC20
	.word	.LC21
.LBE48:
.LBE52:
	.cfi_endproc
.LFE76:
	.size	armwave_test_create_am_sine, .-armwave_test_create_am_sine
	.align	2
	.global	armwave_test_create_square
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_test_create_square, %function
armwave_test_create_square:
.LVL291:
.LFB77:
	.loc 1 517 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 518 5 view .LVU987
	.loc 1 519 5 view .LVU988
	.loc 1 520 5 view .LVU989
	.loc 1 521 5 view .LVU990
	.loc 1 523 5 view .LVU991
@ armwave.c:517: {
	.loc 1 517 1 is_stmt 0 view .LVU992
	push	{r4, r5, r6, lr}	@
	.cfi_def_cfa_offset 16
	.cfi_offset 4, -16
	.cfi_offset 5, -12
	.cfi_offset 6, -8
	.cfi_offset 14, -4
	vpush.64	{d8, d9, d10, d11, d12}	@
	.cfi_def_cfa_offset 56
	.cfi_offset 80, -56
	.cfi_offset 81, -52
	.cfi_offset 82, -48
	.cfi_offset 83, -44
	.cfi_offset 84, -40
	.cfi_offset 85, -36
	.cfi_offset 86, -32
	.cfi_offset 87, -28
	.cfi_offset 88, -24
	.cfi_offset 89, -20
@ armwave.c:523:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 523 35 view .LVU993
	ldr	r5, .L292+32	@ tmp179,
	ldr	r2, [r5, #40]	@ prephitmp_33, g_armwave_state.waves
@ armwave.c:517: {
	.loc 1 517 1 view .LVU994
	sub	sp, sp, #8	@,,
	.cfi_def_cfa_offset 64
@ armwave.c:523:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 523 5 view .LVU995
	cmp	r2, #0	@ prephitmp_33,
	beq	.L264		@,
	vmov.f32	s19, s0	@ noise_fraction, noise_fraction
@ armwave.c:520:     float level = 0.8f, new_level = 0.8f;
	.loc 1 520 11 view .LVU996
	vldr.32	s24, .L292	@ level,
@ armwave.c:535:             if(x > (g_armwave_state.wave_length * 0.75f)) {
	.loc 1 535 49 view .LVU997
	vldr.32	s20, .L292+4	@ tmp186,
@ armwave.c:540:                 new_level = 0.2f;
	.loc 1 540 27 view .LVU998
	vldr.32	s21, .L292+8	@ new_level,
@ armwave.c:537:             } else if(x > (g_armwave_state.wave_length * 0.5f)) {
	.loc 1 537 56 view .LVU999
	vldr.32	s23, .L292+12	@ tmp188,
@ armwave.c:539:             } else if(x > (g_armwave_state.wave_length * 0.25f)) {
	.loc 1 539 56 view .LVU1000
	vldr.32	s17, .L292+16	@ tmp189,
	ldr	r3, [r5, #60]	@ prephitmp_70, g_armwave_state.wave_length
@ armwave.c:523:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 523 11 view .LVU1001
	mov	r6, #0	@ w,
.LVL292:
.L266:
@ armwave.c:524:         for(x = 0; x < g_armwave_state.wave_length; x++) {
	.loc 1 524 9 view .LVU1002
	cmp	r3, #0	@ prephitmp_70,
	beq	.L273		@,
@ armwave.c:538:                 new_level = 0.8f;
	.loc 1 538 27 view .LVU1003
	vldr.32	s22, .L292	@ new_level,
@ armwave.c:545:             level = ((level * 3) + new_level) * 0.25f;
	.loc 1 545 29 view .LVU1004
	vldr.32	s18, .L292+20	@ tmp161,
@ armwave.c:524:         for(x = 0; x < g_armwave_state.wave_length; x++) {
	.loc 1 524 15 view .LVU1005
	mov	r4, #0	@ x,
.LVL293:
.L272:
	.loc 1 525 13 is_stmt 1 view .LVU1006
@ armwave.c:525:             noise  = ((rand() & 0xffff) * noise_fraction);
	.loc 1 525 24 is_stmt 0 view .LVU1007
	bl	rand		@
.LVL294:
@ armwave.c:525:             noise  = ((rand() & 0xffff) * noise_fraction);
	.loc 1 525 31 view .LVU1008
	uxth	r0, r0	@ tmp148,
@ armwave.c:525:             noise  = ((rand() & 0xffff) * noise_fraction);
	.loc 1 525 41 view .LVU1009
	vmov	s15, r0	@ int	@ tmp148, tmp148
	vcvt.f32.s32	s15, s15	@ tmp150, tmp148
@ armwave.c:525:             noise  = ((rand() & 0xffff) * noise_fraction);
	.loc 1 525 20 view .LVU1010
	vmul.f32	s15, s15, s19	@ noise, tmp150, noise_fraction
.LVL295:
	.loc 1 526 13 is_stmt 1 view .LVU1011
@ armwave.c:526:             noise *= noise;
	.loc 1 526 19 is_stmt 0 view .LVU1012
	vmul.f32	s15, s15, s15	@ noise, noise, noise
.LVL296:
	.loc 1 527 13 is_stmt 1 view .LVU1013
@ armwave.c:527:             noise *= noise;
	.loc 1 527 19 is_stmt 0 view .LVU1014
	vmul.f32	s15, s15, s15	@ noise, noise, noise
.LVL297:
	.loc 1 528 13 is_stmt 1 view .LVU1015
@ armwave.c:528:             noise *= noise;
	.loc 1 528 19 is_stmt 0 view .LVU1016
	vmul.f32	s16, s15, s15	@ noise, noise, noise
.LVL298:
	.loc 1 530 13 is_stmt 1 view .LVU1017
@ armwave.c:530:             if((rand() & 0xff) > 0x7f)
	.loc 1 530 17 is_stmt 0 view .LVU1018
	bl	rand		@
.LVL299:
@ armwave.c:535:             if(x > (g_armwave_state.wave_length * 0.75f)) {
	.loc 1 535 18 view .LVU1019
	vmov	s15, r4	@ int	@ x, x
	vcvt.f32.s32	s14, s15	@ _6, x
@ armwave.c:535:             if(x > (g_armwave_state.wave_length * 0.75f)) {
	.loc 1 535 49 view .LVU1020
	vldr.32	s15, [r5, #60]	@ int	@ tmp195, g_armwave_state.wave_length
	vcvt.f32.u32	s15, s15	@ _8, tmp195
	vmul.f32	s13, s15, s20	@ tmp154, _8, tmp186
@ armwave.c:535:             if(x > (g_armwave_state.wave_length * 0.75f)) {
	.loc 1 535 15 view .LVU1021
	vcmpe.f32	s14, s13	@ _6, tmp154
@ armwave.c:530:             if((rand() & 0xff) > 0x7f)
	.loc 1 530 15 view .LVU1022
	tst	r0, #128	@,
	.loc 1 531 17 is_stmt 1 view .LVU1023
@ armwave.c:531:                 noise = -noise;
	.loc 1 531 23 is_stmt 0 view .LVU1024
	vnegne.f32	s16, s16	@ noise, noise
.LVL300:
	.loc 1 535 13 is_stmt 1 view .LVU1025
@ armwave.c:535:             if(x > (g_armwave_state.wave_length * 0.75f)) {
	.loc 1 535 15 is_stmt 0 view .LVU1026
	vmrs	APSR_nzcv, FPSCR
@ armwave.c:540:                 new_level = 0.2f;
	.loc 1 540 27 view .LVU1027
	vmovgt.f32	s15, s21	@ new_level, new_level
@ armwave.c:535:             if(x > (g_armwave_state.wave_length * 0.75f)) {
	.loc 1 535 15 view .LVU1028
	bgt	.L268		@,
	.loc 1 537 20 is_stmt 1 view .LVU1029
@ armwave.c:537:             } else if(x > (g_armwave_state.wave_length * 0.5f)) {
	.loc 1 537 56 is_stmt 0 view .LVU1030
	vmul.f32	s13, s15, s23	@ tmp156, _8, tmp188
@ armwave.c:537:             } else if(x > (g_armwave_state.wave_length * 0.5f)) {
	.loc 1 537 22 view .LVU1031
	vcmpe.f32	s14, s13	@ _6, tmp156
	vmrs	APSR_nzcv, FPSCR
@ armwave.c:538:                 new_level = 0.8f;
	.loc 1 538 27 view .LVU1032
	vmovgt.f32	s15, s22	@ new_level, new_level
@ armwave.c:537:             } else if(x > (g_armwave_state.wave_length * 0.5f)) {
	.loc 1 537 22 view .LVU1033
	bgt	.L268		@,
	.loc 1 539 20 is_stmt 1 view .LVU1034
@ armwave.c:539:             } else if(x > (g_armwave_state.wave_length * 0.25f)) {
	.loc 1 539 56 is_stmt 0 view .LVU1035
	vmul.f32	s15, s15, s17	@ tmp158, _8, tmp189
@ armwave.c:538:                 new_level = 0.8f;
	.loc 1 538 27 view .LVU1036
	vcmpe.f32	s14, s15	@ _6, tmp158
	vmrs	APSR_nzcv, FPSCR
	vmovle.f32	s15, s22	@, new_level, new_level
	vmovgt.f32	s15, s21	@, new_level, new_level
.L268:
.LVL301:
	.loc 1 545 13 is_stmt 1 view .LVU1037
@ armwave.c:545:             level = ((level * 3) + new_level) * 0.25f;
	.loc 1 545 34 is_stmt 0 view .LVU1038
	vmla.f32	s15, s24, s18	@ _14, level, tmp161
.LVL302:
@ armwave.c:545:             level = ((level * 3) + new_level) * 0.25f;
	.loc 1 545 19 view .LVU1039
	vmul.f32	s24, s15, s17	@ level, _14, tmp189
.LVL303:
	.loc 1 547 13 is_stmt 1 view .LVU1040
@ armwave.c:547:             v = (uint8_t)(CLAMP(level + noise, 0.0f, 1.0f) * 255);
	.loc 1 547 27 is_stmt 0 view .LVU1041
	vadd.f32	s15, s16, s24	@ _15, noise, level
	vcmpe.f32	s15, #0	@ _15
	vmrs	APSR_nzcv, FPSCR
@ armwave.c:547:             v = (uint8_t)(CLAMP(level + noise, 0.0f, 1.0f) * 255);
	.loc 1 547 17 view .LVU1042
	movle	r2, #0	@ iftmp.29_32,
@ armwave.c:547:             v = (uint8_t)(CLAMP(level + noise, 0.0f, 1.0f) * 255);
	.loc 1 547 27 view .LVU1043
	ble	.L269		@,
@ armwave.c:547:             v = (uint8_t)(CLAMP(level + noise, 0.0f, 1.0f) * 255);
	.loc 1 547 17 discriminator 1 view .LVU1044
	vldr.32	s14, .L292+24	@ tmp164,
	vcmpe.f32	s15, s14	@ _15, tmp164
	vmrs	APSR_nzcv, FPSCR
@ armwave.c:547:             v = (uint8_t)(CLAMP(level + noise, 0.0f, 1.0f) * 255);
	.loc 1 547 60 discriminator 1 view .LVU1045
	vldrmi.32	s14, .L292+28	@ tmp166,
@ armwave.c:547:             v = (uint8_t)(CLAMP(level + noise, 0.0f, 1.0f) * 255);
	.loc 1 547 17 discriminator 1 view .LVU1046
	movpl	r2, #255	@ iftmp.29_32,
@ armwave.c:547:             v = (uint8_t)(CLAMP(level + noise, 0.0f, 1.0f) * 255);
	.loc 1 547 60 discriminator 1 view .LVU1047
	vmulmi.f32	s15, s15, s14	@ tmp165, _15, tmp166
@ armwave.c:547:             v = (uint8_t)(CLAMP(level + noise, 0.0f, 1.0f) * 255);
	.loc 1 547 17 discriminator 1 view .LVU1048
	vcvtmi.u32.f32	s15, s15	@ tmp168, tmp165
	vstrmi.32	s15, [sp, #4]	@ int	@ tmp168, %sfp
	ldrbmi	r2, [sp, #4]	@ zero_extendqisi2	@ iftmp.29_32, %sfp
.L269:
.LVL304:
	.loc 1 548 13 is_stmt 1 discriminator 12 view .LVU1049
@ armwave.c:548:             g_armwave_state.test_wave_buffer[x + (w * g_armwave_state.wave_stride)] = v;
	.loc 1 548 53 is_stmt 0 discriminator 12 view .LVU1050
	ldr	r3, [r5, #36]	@ g_armwave_state.wave_stride, g_armwave_state.wave_stride
@ armwave.c:548:             g_armwave_state.test_wave_buffer[x + (w * g_armwave_state.wave_stride)] = v;
	.loc 1 548 85 discriminator 12 view .LVU1051
	ldr	r1, [r5, #24]	@ g_armwave_state.test_wave_buffer, g_armwave_state.test_wave_buffer
	mla	r3, r3, r6, r1	@ tmp174, g_armwave_state.wave_stride, w, g_armwave_state.test_wave_buffer
	strb	r2, [r3, r4]	@ iftmp.29_32, *_21
@ armwave.c:524:         for(x = 0; x < g_armwave_state.wave_length; x++) {
	.loc 1 524 39 discriminator 12 view .LVU1052
	ldr	r3, [r5, #60]	@ prephitmp_70, g_armwave_state.wave_length
@ armwave.c:524:         for(x = 0; x < g_armwave_state.wave_length; x++) {
	.loc 1 524 54 discriminator 12 view .LVU1053
	add	r4, r4, #1	@ x, x,
.LVL305:
@ armwave.c:524:         for(x = 0; x < g_armwave_state.wave_length; x++) {
	.loc 1 524 9 discriminator 12 view .LVU1054
	cmp	r3, r4	@ prephitmp_70, x
	bhi	.L272		@,
	ldr	r2, [r5, #40]	@ prephitmp_33, g_armwave_state.waves
.LVL306:
.L273:
@ armwave.c:523:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 523 44 discriminator 2 view .LVU1055
	add	r6, r6, #1	@ w, w,
.LVL307:
@ armwave.c:523:     for(w = 0; w < g_armwave_state.waves; w++) {
	.loc 1 523 5 discriminator 2 view .LVU1056
	cmp	r6, r2	@ w, prephitmp_33
	bcc	.L266		@,
.LVL308:
.L264:
@ armwave.c:551: }
	.loc 1 551 1 view .LVU1057
	add	sp, sp, #8	@,,
	.cfi_def_cfa_offset 56
	@ sp needed	@
	vldm	sp!, {d8-d12}	@,
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
	.cfi_def_cfa_offset 16
	pop	{r4, r5, r6, pc}	@
.L293:
	.align	2
.L292:
	.word	1061997773
	.word	1061158912
	.word	1045220557
	.word	1056964608
	.word	1048576000
	.word	1077936128
	.word	1065353216
	.word	1132396544
	.word	g_armwave_state
	.cfi_endproc
.LFE77:
	.size	armwave_test_create_square, .-armwave_test_create_square
	.align	2
	.global	armwave_cleanup
	.syntax unified
	.arm
	.fpu vfp
	.type	armwave_cleanup, %function
armwave_cleanup:
.LFB78:
	.loc 1 557 1 is_stmt 1 view -0
	.cfi_startproc
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	.loc 1 558 5 view .LVU1059
@ armwave.c:557: {
	.loc 1 557 1 is_stmt 0 view .LVU1060
	push	{r4, lr}	@
	.cfi_def_cfa_offset 8
	.cfi_offset 4, -8
	.cfi_offset 14, -4
@ armwave.c:558:     free(g_armwave_state.out_pixbuf);
	.loc 1 558 25 view .LVU1061
	ldr	r4, .L296	@ tmp114,
@ armwave.c:558:     free(g_armwave_state.out_pixbuf);
	.loc 1 558 5 view .LVU1062
	ldr	r0, [r4, #88]	@, g_armwave_state.out_pixbuf
	bl	free		@
.LVL309:
	.loc 1 559 5 is_stmt 1 view .LVU1063
	ldr	r0, [r4, #4]	@, g_armwave_state.ch1_buffer
	bl	free		@
.LVL310:
	.loc 1 560 5 view .LVU1064
	ldr	r0, [r4, #116]	@, g_armwave_state.xcoord_to_xpixel
	bl	free		@
.LVL311:
	.loc 1 561 5 view .LVU1065
	ldr	r0, [r4, #24]	@, g_armwave_state.test_wave_buffer
	bl	free		@
.LVL312:
	.loc 1 563 5 view .LVU1066
@ armwave.c:563:     g_armwave_state.out_pixbuf = NULL;
	.loc 1 563 32 is_stmt 0 view .LVU1067
	mov	r3, #0	@ tmp123,
	str	r3, [r4, #88]	@ tmp123, g_armwave_state.out_pixbuf
	.loc 1 564 5 is_stmt 1 view .LVU1068
@ armwave.c:564:     g_armwave_state.ch1_buffer = NULL;
	.loc 1 564 32 is_stmt 0 view .LVU1069
	str	r3, [r4, #4]	@ tmp123, g_armwave_state.ch1_buffer
	.loc 1 565 5 is_stmt 1 view .LVU1070
@ armwave.c:565:     g_armwave_state.xcoord_to_xpixel = NULL;
	.loc 1 565 38 is_stmt 0 view .LVU1071
	str	r3, [r4, #116]	@ tmp123, g_armwave_state.xcoord_to_xpixel
	.loc 1 566 5 is_stmt 1 view .LVU1072
@ armwave.c:566:     g_armwave_state.test_wave_buffer = NULL;
	.loc 1 566 38 is_stmt 0 view .LVU1073
	str	r3, [r4, #24]	@ tmp123, g_armwave_state.test_wave_buffer
@ armwave.c:567: }
	.loc 1 567 1 view .LVU1074
	pop	{r4, pc}	@
.L297:
	.align	2
.L296:
	.word	g_armwave_state
	.cfi_endproc
.LFE78:
	.size	armwave_cleanup, .-armwave_cleanup
	.comm	gamma_table,256,4
	.comm	g_armwave_state,120,4
	.section	.rodata
	.align	2
	.set	.LANCHOR0,. + 0
	.type	__PRETTY_FUNCTION__.17017, %object
	.size	__PRETTY_FUNCTION__.17017, 24
__PRETTY_FUNCTION__.17017:
	.ascii	"armwave_fill_pixbuf_256\000"
	.type	__PRETTY_FUNCTION__.17048, %object
	.size	__PRETTY_FUNCTION__.17048, 27
__PRETTY_FUNCTION__.17048:
	.ascii	"armwave_fill_pixbuf_scaled\000"
	.space	1
	.type	__PRETTY_FUNCTION__.17070, %object
	.size	__PRETTY_FUNCTION__.17070, 21
__PRETTY_FUNCTION__.17070:
	.ascii	"armwave_setup_render\000"
	.space	3
	.type	__PRETTY_FUNCTION__.17077, %object
	.size	__PRETTY_FUNCTION__.17077, 25
__PRETTY_FUNCTION__.17077:
	.ascii	"armwave_set_wave_pointer\000"
	.space	3
	.type	__PRETTY_FUNCTION__.17083, %object
	.size	__PRETTY_FUNCTION__.17083, 29
__PRETTY_FUNCTION__.17083:
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
	.ascii	"ch1_buffer=0x%08x\012\000"
	.space	1
.LC3:
	.ascii	"wave_buffer=0x%08x\012\000"
.LC4:
	.ascii	"armwave.c\000"
	.space	2
.LC5:
	.ascii	"out_buffer != NULL\000"
	.space	1
.LC6:
	.ascii	"s=%d e=%d w=%d ws=%d tw=%d th=%d rf=0x%08x\012\000"
.LC7:
	.ascii	"start_point < end_point\000"
.LC8:
	.ascii	"target_height == 256 || target_height == 512 || tar"
	.ascii	"get_height == 1024 || target_height == 2048\000"
	.space	1
.LC9:
	.ascii	"ch_buff_size=%d\012\000"
	.space	3
.LC10:
	.ascii	"g_armwave_state.ch1_buffer != NULL\000"
	.space	1
.LC11:
	.ascii	"g_armwave_state.xcoord_to_xpixel != NULL\000"
	.space	3
.LC12:
	.ascii	"wave_buffer != NULL\000"
.LC13:
	.ascii	"wave_buffer_ptr != 0\000"
	.space	3
.LC14:
	.ascii	"wb\000"
	.space	1
.LC15:
	.ascii	"P3\012\000"
.LC16:
	.ascii	"%d %d\012\000"
	.space	1
.LC17:
	.ascii	"255\012\000"
	.space	3
.LC18:
	.ascii	"%3d %3d %3d\012\000"
	.space	3
.LC19:
	.ascii	"armwave_test_generate: slice %d (y=%d, h=%d)\012\000"
	.space	2
.LC20:
	.ascii	"armwave_test_buffer_alloc: length=%d max=%d\012\000"
	.space	3
.LC21:
	.ascii	"armwave_test_buffer_alloc: failed to allocate test "
	.ascii	"wave buffer (%d bytes)\012\000"
	.space	1
.LC22:
	.ascii	"armwave_fill_pixbuf_into_pybuffer: start\000"
	.space	3
.LC23:
	.ascii	"armwave_fill_pixbuf_into_pybuffer: PyObject_GetBuff"
	.ascii	"er() failed, returning PyFalse\000"
	.space	2
.LC24:
	.ascii	"armwave_fill_pixbuf_into_pybuffer: buffer fill done"
	.ascii	"\000"
.LC25:
	.ascii	"armwave_fill_pixbuf_into_pybuffer: done\000"
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
	.file 68 "armwave.h"
	.file 69 "/usr/include/stdlib.h"
	.file 70 "/usr/local/include/python3.8/cpython/abstract.h"
	.file 71 "/usr/include/assert.h"
	.file 72 "<built-in>"
	.file 73 "/usr/include/arm-linux-gnueabihf/bits/mathcalls.h"
	.section	.debug_info,"",%progbits
.Ldebug_info0:
	.4byte	0x364f
	.2byte	0x4
	.4byte	.Ldebug_abbrev0
	.byte	0x4
	.uleb128 0x1
	.4byte	.LASF630
	.byte	0xc
	.4byte	.LASF631
	.4byte	.LASF632
	.4byte	.Ltext0
	.4byte	.Letext0-.Ltext0
	.4byte	.Ldebug_line0
	.uleb128 0x2
	.byte	0x4
	.byte	0x4
	.4byte	.LASF0
	.uleb128 0x2
	.byte	0x8
	.byte	0x4
	.4byte	.LASF1
	.uleb128 0x3
	.byte	0x4
	.byte	0x5
	.ascii	"int\000"
	.uleb128 0x2
	.byte	0x4
	.byte	0x7
	.4byte	.LASF2
	.uleb128 0x4
	.4byte	.LASF7
	.byte	0x3
	.byte	0xd8
	.byte	0x17
	.4byte	0x3a
	.uleb128 0x5
	.4byte	0x3a
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
	.4byte	0x3a
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
	.4byte	0x33
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
	.4byte	0x33
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
	.4byte	0x33
	.byte	0x38
	.uleb128 0x9
	.4byte	.LASF37
	.byte	0x5
	.byte	0x49
	.byte	0x7
	.4byte	0x33
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
	.4byte	0x33
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
	.4byte	.LASF633
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
	.4byte	0x3a
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
	.4byte	0x3a
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
	.4byte	0x33
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
	.4byte	0x33
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
	.uleb128 0x10
	.4byte	.LASF66
	.byte	0xb
	.2byte	0x21f
	.byte	0xf
	.4byte	0x3a9
	.uleb128 0x7
	.byte	0x4
	.4byte	0xfd
	.uleb128 0x10
	.4byte	.LASF67
	.byte	0xb
	.2byte	0x221
	.byte	0xf
	.4byte	0x3a9
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
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF70
	.byte	0xc
	.byte	0x37
	.byte	0xc
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF71
	.byte	0xc
	.byte	0x3b
	.byte	0xc
	.4byte	0x33
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
	.4byte	0x41c
	.uleb128 0x10
	.4byte	.LASF78
	.byte	0xf
	.2byte	0x305
	.byte	0xc
	.4byte	0x33
	.uleb128 0x8
	.4byte	.LASF80
	.byte	0x8
	.byte	0x10
	.byte	0x34
	.byte	0x8
	.4byte	0x469
	.uleb128 0x9
	.4byte	.LASF81
	.byte	0x10
	.byte	0x36
	.byte	0x9
	.4byte	0x33
	.byte	0
	.uleb128 0x9
	.4byte	.LASF82
	.byte	0x10
	.byte	0x37
	.byte	0x9
	.4byte	0x33
	.byte	0x4
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x441
	.uleb128 0x11
	.4byte	0x469
	.uleb128 0xc
	.4byte	0xfd
	.4byte	0x484
	.uleb128 0xd
	.4byte	0x3a
	.byte	0x1
	.byte	0
	.uleb128 0xe
	.4byte	.LASF83
	.byte	0x11
	.byte	0x9f
	.byte	0xe
	.4byte	0x474
	.uleb128 0xe
	.4byte	.LASF84
	.byte	0x11
	.byte	0xa0
	.byte	0xc
	.4byte	0x33
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
	.4byte	0x474
	.uleb128 0xe
	.4byte	.LASF87
	.byte	0x11
	.byte	0xae
	.byte	0xc
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF80
	.byte	0x11
	.byte	0xaf
	.byte	0x11
	.4byte	0xde
	.uleb128 0x10
	.4byte	.LASF88
	.byte	0x11
	.2byte	0x118
	.byte	0xc
	.4byte	0x33
	.uleb128 0x12
	.byte	0x7
	.byte	0x4
	.4byte	0x3a
	.byte	0x12
	.byte	0x70
	.byte	0xa
	.4byte	0x4fa
	.uleb128 0x13
	.4byte	.LASF89
	.byte	0
	.uleb128 0x13
	.4byte	.LASF90
	.byte	0x1
	.uleb128 0x13
	.4byte	.LASF91
	.byte	0x2
	.byte	0
	.uleb128 0x8
	.4byte	.LASF92
	.byte	0x10
	.byte	0x12
	.byte	0x6d
	.byte	0x8
	.4byte	0x53c
	.uleb128 0x9
	.4byte	.LASF93
	.byte	0x12
	.byte	0x74
	.byte	0x7
	.4byte	0x4d9
	.byte	0
	.uleb128 0x9
	.4byte	.LASF94
	.byte	0x12
	.byte	0x78
	.byte	0x9
	.4byte	0x33
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF95
	.byte	0x12
	.byte	0x7c
	.byte	0x9
	.4byte	0x33
	.byte	0x8
	.uleb128 0x9
	.4byte	.LASF96
	.byte	0x12
	.byte	0x80
	.byte	0x9
	.4byte	0x33
	.byte	0xc
	.byte	0
	.uleb128 0xe
	.4byte	.LASF97
	.byte	0x12
	.byte	0x83
	.byte	0x2a
	.4byte	0x4fa
	.uleb128 0x8
	.4byte	.LASF98
	.byte	0x8
	.byte	0x2
	.byte	0x68
	.byte	0x10
	.4byte	0x570
	.uleb128 0x9
	.4byte	.LASF99
	.byte	0x2
	.byte	0x6a
	.byte	0x10
	.4byte	0x41c
	.byte	0
	.uleb128 0x9
	.4byte	.LASF100
	.byte	0x2
	.byte	0x6b
	.byte	0x19
	.4byte	0x80b
	.byte	0x4
	.byte	0
	.uleb128 0x8
	.4byte	.LASF101
	.byte	0xd0
	.byte	0x13
	.byte	0xb1
	.byte	0x10
	.4byte	0x80b
	.uleb128 0x9
	.4byte	.LASF102
	.byte	0x13
	.byte	0xb2
	.byte	0x5
	.4byte	0x841
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
	.4byte	0x41c
	.byte	0x10
	.uleb128 0x9
	.4byte	.LASF105
	.byte	0x13
	.byte	0xb4
	.byte	0x1e
	.4byte	0x41c
	.byte	0x14
	.uleb128 0x9
	.4byte	.LASF106
	.byte	0x13
	.byte	0xb8
	.byte	0x10
	.4byte	0xa1c
	.byte	0x18
	.uleb128 0x9
	.4byte	.LASF107
	.byte	0x13
	.byte	0xb9
	.byte	0x10
	.4byte	0x41c
	.byte	0x1c
	.uleb128 0x9
	.4byte	.LASF108
	.byte	0x13
	.byte	0xba
	.byte	0x11
	.4byte	0xa39
	.byte	0x20
	.uleb128 0x9
	.4byte	.LASF109
	.byte	0x13
	.byte	0xbb
	.byte	0x11
	.4byte	0xa6b
	.byte	0x24
	.uleb128 0x9
	.4byte	.LASF110
	.byte	0x13
	.byte	0xbc
	.byte	0x15
	.4byte	0x1029
	.byte	0x28
	.uleb128 0x9
	.4byte	.LASF111
	.byte	0x13
	.byte	0xbe
	.byte	0xe
	.4byte	0xaa2
	.byte	0x2c
	.uleb128 0x9
	.4byte	.LASF112
	.byte	0x13
	.byte	0xc2
	.byte	0x16
	.4byte	0x102f
	.byte	0x30
	.uleb128 0x9
	.4byte	.LASF113
	.byte	0x13
	.byte	0xc3
	.byte	0x18
	.4byte	0x1035
	.byte	0x34
	.uleb128 0x9
	.4byte	.LASF114
	.byte	0x13
	.byte	0xc4
	.byte	0x17
	.4byte	0x103b
	.byte	0x38
	.uleb128 0x9
	.4byte	.LASF115
	.byte	0x13
	.byte	0xc8
	.byte	0xe
	.4byte	0xaae
	.byte	0x3c
	.uleb128 0x9
	.4byte	.LASF116
	.byte	0x13
	.byte	0xc9
	.byte	0x11
	.4byte	0x89f
	.byte	0x40
	.uleb128 0x9
	.4byte	.LASF117
	.byte	0x13
	.byte	0xca
	.byte	0xe
	.4byte	0xaa2
	.byte	0x44
	.uleb128 0x9
	.4byte	.LASF118
	.byte	0x13
	.byte	0xcb
	.byte	0x12
	.4byte	0xa5f
	.byte	0x48
	.uleb128 0x9
	.4byte	.LASF119
	.byte	0x13
	.byte	0xcc
	.byte	0x12
	.4byte	0xa96
	.byte	0x4c
	.uleb128 0x9
	.4byte	.LASF120
	.byte	0x13
	.byte	0xcf
	.byte	0x14
	.4byte	0x1041
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
	.4byte	0x9d4
	.byte	0x5c
	.uleb128 0x9
	.4byte	.LASF124
	.byte	0x13
	.byte	0xdb
	.byte	0xd
	.4byte	0x8ca
	.byte	0x60
	.uleb128 0x9
	.4byte	.LASF125
	.byte	0x13
	.byte	0xdf
	.byte	0x11
	.4byte	0xacf
	.byte	0x64
	.uleb128 0x9
	.4byte	.LASF126
	.byte	0x13
	.byte	0xe2
	.byte	0x10
	.4byte	0x41c
	.byte	0x68
	.uleb128 0x9
	.4byte	.LASF127
	.byte	0x13
	.byte	0xe5
	.byte	0x11
	.4byte	0xafa
	.byte	0x6c
	.uleb128 0x9
	.4byte	.LASF128
	.byte	0x13
	.byte	0xe6
	.byte	0x12
	.4byte	0xb06
	.byte	0x70
	.uleb128 0x9
	.4byte	.LASF129
	.byte	0x13
	.byte	0xe9
	.byte	0x19
	.4byte	0x1089
	.byte	0x74
	.uleb128 0x9
	.4byte	.LASF130
	.byte	0x13
	.byte	0xea
	.byte	0x19
	.4byte	0x1094
	.byte	0x78
	.uleb128 0x9
	.4byte	.LASF131
	.byte	0x13
	.byte	0xeb
	.byte	0x19
	.4byte	0x10e9
	.byte	0x7c
	.uleb128 0x9
	.4byte	.LASF132
	.byte	0x13
	.byte	0xec
	.byte	0x19
	.4byte	0x80b
	.byte	0x80
	.uleb128 0x9
	.4byte	.LASF133
	.byte	0x13
	.byte	0xed
	.byte	0xf
	.4byte	0x86e
	.byte	0x84
	.uleb128 0x9
	.4byte	.LASF134
	.byte	0x13
	.byte	0xee
	.byte	0x12
	.4byte	0xb12
	.byte	0x88
	.uleb128 0x9
	.4byte	.LASF135
	.byte	0x13
	.byte	0xef
	.byte	0x12
	.4byte	0xb1e
	.byte	0x8c
	.uleb128 0x9
	.4byte	.LASF136
	.byte	0x13
	.byte	0xf0
	.byte	0x10
	.4byte	0x41c
	.byte	0x90
	.uleb128 0x9
	.4byte	.LASF137
	.byte	0x13
	.byte	0xf1
	.byte	0xe
	.4byte	0xb2a
	.byte	0x94
	.uleb128 0x9
	.4byte	.LASF138
	.byte	0x13
	.byte	0xf2
	.byte	0xf
	.4byte	0xb61
	.byte	0x98
	.uleb128 0x9
	.4byte	.LASF139
	.byte	0x13
	.byte	0xf3
	.byte	0xd
	.4byte	0xb36
	.byte	0x9c
	.uleb128 0x9
	.4byte	.LASF140
	.byte	0x13
	.byte	0xf4
	.byte	0xe
	.4byte	0x9ff
	.byte	0xa0
	.uleb128 0x9
	.4byte	.LASF141
	.byte	0x13
	.byte	0xf5
	.byte	0xd
	.4byte	0x8ca
	.byte	0xa4
	.uleb128 0x9
	.4byte	.LASF142
	.byte	0x13
	.byte	0xf6
	.byte	0xf
	.4byte	0x86e
	.byte	0xa8
	.uleb128 0x9
	.4byte	.LASF143
	.byte	0x13
	.byte	0xf7
	.byte	0xf
	.4byte	0x86e
	.byte	0xac
	.uleb128 0x9
	.4byte	.LASF144
	.byte	0x13
	.byte	0xf8
	.byte	0xf
	.4byte	0x86e
	.byte	0xb0
	.uleb128 0x9
	.4byte	.LASF145
	.byte	0x13
	.byte	0xf9
	.byte	0xf
	.4byte	0x86e
	.byte	0xb4
	.uleb128 0x9
	.4byte	.LASF146
	.byte	0x13
	.byte	0xfa
	.byte	0xf
	.4byte	0x86e
	.byte	0xb8
	.uleb128 0x9
	.4byte	.LASF147
	.byte	0x13
	.byte	0xfb
	.byte	0x10
	.4byte	0xa1c
	.byte	0xbc
	.uleb128 0x9
	.4byte	.LASF148
	.byte	0x13
	.byte	0xfe
	.byte	0x12
	.4byte	0x3a
	.byte	0xc0
	.uleb128 0x14
	.4byte	.LASF149
	.byte	0x13
	.2byte	0x100
	.byte	0x10
	.4byte	0xa1c
	.byte	0xc4
	.uleb128 0x14
	.4byte	.LASF150
	.byte	0x13
	.2byte	0x101
	.byte	0x14
	.4byte	0xcc7
	.byte	0xc8
	.uleb128 0x14
	.4byte	.LASF151
	.byte	0x13
	.2byte	0x104
	.byte	0x1e
	.4byte	0x1108
	.byte	0xcc
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x570
	.uleb128 0x4
	.4byte	.LASF152
	.byte	0x2
	.byte	0x6c
	.byte	0x3
	.4byte	0x548
	.uleb128 0x15
	.byte	0xc
	.byte	0x2
	.byte	0x71
	.byte	0x9
	.4byte	0x841
	.uleb128 0x9
	.4byte	.LASF102
	.byte	0x2
	.byte	0x72
	.byte	0xe
	.4byte	0x811
	.byte	0
	.uleb128 0x9
	.4byte	.LASF153
	.byte	0x2
	.byte	0x73
	.byte	0x10
	.4byte	0x41c
	.byte	0x8
	.byte	0
	.uleb128 0x4
	.4byte	.LASF154
	.byte	0x2
	.byte	0x74
	.byte	0x3
	.4byte	0x81d
	.uleb128 0x4
	.4byte	.LASF155
	.byte	0x2
	.byte	0x8c
	.byte	0x16
	.4byte	0x859
	.uleb128 0x7
	.byte	0x4
	.4byte	0x85f
	.uleb128 0x16
	.4byte	0x86e
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0x86e
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x811
	.uleb128 0x5
	.4byte	0x86e
	.uleb128 0x4
	.4byte	.LASF156
	.byte	0x2
	.byte	0x8d
	.byte	0x16
	.4byte	0x885
	.uleb128 0x7
	.byte	0x4
	.4byte	0x88b
	.uleb128 0x16
	.4byte	0x86e
	.4byte	0x89f
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0x86e
	.byte	0
	.uleb128 0x4
	.4byte	.LASF157
	.byte	0x2
	.byte	0x8e
	.byte	0x16
	.4byte	0x8ab
	.uleb128 0x7
	.byte	0x4
	.4byte	0x8b1
	.uleb128 0x16
	.4byte	0x86e
	.4byte	0x8ca
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0x86e
	.byte	0
	.uleb128 0x4
	.4byte	.LASF158
	.byte	0x2
	.byte	0x8f
	.byte	0xf
	.4byte	0x8d6
	.uleb128 0x7
	.byte	0x4
	.4byte	0x8dc
	.uleb128 0x16
	.4byte	0x33
	.4byte	0x8eb
	.uleb128 0x17
	.4byte	0x86e
	.byte	0
	.uleb128 0x4
	.4byte	.LASF159
	.byte	0x2
	.byte	0x90
	.byte	0x16
	.4byte	0x8f7
	.uleb128 0x7
	.byte	0x4
	.4byte	0x8fd
	.uleb128 0x16
	.4byte	0x41c
	.4byte	0x90c
	.uleb128 0x17
	.4byte	0x86e
	.byte	0
	.uleb128 0x4
	.4byte	.LASF160
	.byte	0x2
	.byte	0x91
	.byte	0x15
	.4byte	0x918
	.uleb128 0x7
	.byte	0x4
	.4byte	0x91e
	.uleb128 0x16
	.4byte	0x86e
	.4byte	0x932
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0x41c
	.byte	0
	.uleb128 0x4
	.4byte	.LASF161
	.byte	0x2
	.byte	0x93
	.byte	0xe
	.4byte	0x93e
	.uleb128 0x7
	.byte	0x4
	.4byte	0x944
	.uleb128 0x16
	.4byte	0x33
	.4byte	0x95d
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0x41c
	.uleb128 0x17
	.4byte	0x86e
	.byte	0
	.uleb128 0x4
	.4byte	.LASF162
	.byte	0x2
	.byte	0x95
	.byte	0xe
	.4byte	0x969
	.uleb128 0x7
	.byte	0x4
	.4byte	0x96f
	.uleb128 0x16
	.4byte	0x33
	.4byte	0x988
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0x86e
	.byte	0
	.uleb128 0x4
	.4byte	.LASF163
	.byte	0x2
	.byte	0x97
	.byte	0xf
	.4byte	0x994
	.uleb128 0x7
	.byte	0x4
	.4byte	0x99a
	.uleb128 0x16
	.4byte	0x33
	.4byte	0x9ae
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0x86e
	.byte	0
	.uleb128 0x4
	.4byte	.LASF164
	.byte	0x2
	.byte	0x98
	.byte	0xf
	.4byte	0x9ba
	.uleb128 0x7
	.byte	0x4
	.4byte	0x9c0
	.uleb128 0x16
	.4byte	0x33
	.4byte	0x9d4
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0x52
	.byte	0
	.uleb128 0x4
	.4byte	.LASF165
	.byte	0x2
	.byte	0x99
	.byte	0xf
	.4byte	0x9e0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x9e6
	.uleb128 0x16
	.4byte	0x33
	.4byte	0x9ff
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0x9ae
	.uleb128 0x17
	.4byte	0x52
	.byte	0
	.uleb128 0x4
	.4byte	.LASF166
	.byte	0x2
	.byte	0x9c
	.byte	0x10
	.4byte	0xa0b
	.uleb128 0x7
	.byte	0x4
	.4byte	0xa11
	.uleb128 0x18
	.4byte	0xa1c
	.uleb128 0x17
	.4byte	0x52
	.byte	0
	.uleb128 0x4
	.4byte	.LASF167
	.byte	0x2
	.byte	0x9d
	.byte	0x10
	.4byte	0xa28
	.uleb128 0x7
	.byte	0x4
	.4byte	0xa2e
	.uleb128 0x18
	.4byte	0xa39
	.uleb128 0x17
	.4byte	0x86e
	.byte	0
	.uleb128 0x4
	.4byte	.LASF168
	.byte	0x2
	.byte	0x9e
	.byte	0x15
	.4byte	0xa45
	.uleb128 0x7
	.byte	0x4
	.4byte	0xa4b
	.uleb128 0x16
	.4byte	0x86e
	.4byte	0xa5f
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0xfd
	.byte	0
	.uleb128 0x4
	.4byte	.LASF169
	.byte	0x2
	.byte	0x9f
	.byte	0x15
	.4byte	0x885
	.uleb128 0x4
	.4byte	.LASF170
	.byte	0x2
	.byte	0xa0
	.byte	0xf
	.4byte	0xa77
	.uleb128 0x7
	.byte	0x4
	.4byte	0xa7d
	.uleb128 0x16
	.4byte	0x33
	.4byte	0xa96
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0xfd
	.uleb128 0x17
	.4byte	0x86e
	.byte	0
	.uleb128 0x4
	.4byte	.LASF171
	.byte	0x2
	.byte	0xa1
	.byte	0xf
	.4byte	0x969
	.uleb128 0x4
	.4byte	.LASF172
	.byte	0x2
	.byte	0xa2
	.byte	0x15
	.4byte	0x859
	.uleb128 0x4
	.4byte	.LASF173
	.byte	0x2
	.byte	0xa3
	.byte	0x15
	.4byte	0xaba
	.uleb128 0x7
	.byte	0x4
	.4byte	0xac0
	.uleb128 0x16
	.4byte	0x428
	.4byte	0xacf
	.uleb128 0x17
	.4byte	0x86e
	.byte	0
	.uleb128 0x4
	.4byte	.LASF174
	.byte	0x2
	.byte	0xa4
	.byte	0x15
	.4byte	0xadb
	.uleb128 0x7
	.byte	0x4
	.4byte	0xae1
	.uleb128 0x16
	.4byte	0x86e
	.4byte	0xafa
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0x33
	.byte	0
	.uleb128 0x4
	.4byte	.LASF175
	.byte	0x2
	.byte	0xa5
	.byte	0x15
	.4byte	0x859
	.uleb128 0x4
	.4byte	.LASF176
	.byte	0x2
	.byte	0xa6
	.byte	0x15
	.4byte	0x859
	.uleb128 0x4
	.4byte	.LASF177
	.byte	0x2
	.byte	0xa7
	.byte	0x15
	.4byte	0x8ab
	.uleb128 0x4
	.4byte	.LASF178
	.byte	0x2
	.byte	0xa8
	.byte	0xf
	.4byte	0x969
	.uleb128 0x4
	.4byte	.LASF179
	.byte	0x2
	.byte	0xa9
	.byte	0xf
	.4byte	0x969
	.uleb128 0x4
	.4byte	.LASF180
	.byte	0x2
	.byte	0xaa
	.byte	0x15
	.4byte	0xb42
	.uleb128 0x7
	.byte	0x4
	.4byte	0xb48
	.uleb128 0x16
	.4byte	0x86e
	.4byte	0xb61
	.uleb128 0x17
	.4byte	0x80b
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0x86e
	.byte	0
	.uleb128 0x4
	.4byte	.LASF181
	.byte	0x2
	.byte	0xab
	.byte	0x15
	.4byte	0xb6d
	.uleb128 0x7
	.byte	0x4
	.4byte	0xb73
	.uleb128 0x16
	.4byte	0x86e
	.4byte	0xb87
	.uleb128 0x17
	.4byte	0x80b
	.uleb128 0x17
	.4byte	0x41c
	.byte	0
	.uleb128 0xe
	.4byte	.LASF182
	.byte	0x2
	.byte	0xce
	.byte	0x20
	.4byte	0x570
	.uleb128 0xe
	.4byte	.LASF183
	.byte	0x2
	.byte	0xcf
	.byte	0x20
	.4byte	0x570
	.uleb128 0xe
	.4byte	.LASF184
	.byte	0x2
	.byte	0xd0
	.byte	0x20
	.4byte	0x570
	.uleb128 0x10
	.4byte	.LASF185
	.byte	0x2
	.2byte	0x230
	.byte	0x16
	.4byte	0x811
	.uleb128 0x10
	.4byte	.LASF186
	.byte	0x2
	.2byte	0x23a
	.byte	0x16
	.4byte	0x811
	.uleb128 0x8
	.4byte	.LASF187
	.byte	0x2c
	.byte	0x13
	.byte	0x28
	.byte	0x10
	.4byte	0xc62
	.uleb128 0x19
	.ascii	"buf\000"
	.byte	0x13
	.byte	0x29
	.byte	0xb
	.4byte	0x52
	.byte	0
	.uleb128 0x19
	.ascii	"obj\000"
	.byte	0x13
	.byte	0x2a
	.byte	0xf
	.4byte	0x86e
	.byte	0x4
	.uleb128 0x19
	.ascii	"len\000"
	.byte	0x13
	.byte	0x2b
	.byte	0x10
	.4byte	0x41c
	.byte	0x8
	.uleb128 0x9
	.4byte	.LASF188
	.byte	0x13
	.byte	0x2c
	.byte	0x10
	.4byte	0x41c
	.byte	0xc
	.uleb128 0x9
	.4byte	.LASF189
	.byte	0x13
	.byte	0x2e
	.byte	0x9
	.4byte	0x33
	.byte	0x10
	.uleb128 0x9
	.4byte	.LASF190
	.byte	0x13
	.byte	0x2f
	.byte	0x9
	.4byte	0x33
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
	.4byte	0xc62
	.byte	0x1c
	.uleb128 0x9
	.4byte	.LASF193
	.byte	0x13
	.byte	0x32
	.byte	0x11
	.4byte	0xc62
	.byte	0x20
	.uleb128 0x9
	.4byte	.LASF194
	.byte	0x13
	.byte	0x33
	.byte	0x11
	.4byte	0xc62
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
	.4byte	0x41c
	.uleb128 0x4
	.4byte	.LASF196
	.byte	0x13
	.byte	0x35
	.byte	0x3
	.4byte	0xbc5
	.uleb128 0x4
	.4byte	.LASF197
	.byte	0x13
	.byte	0x37
	.byte	0xf
	.4byte	0xc80
	.uleb128 0x7
	.byte	0x4
	.4byte	0xc86
	.uleb128 0x16
	.4byte	0x33
	.4byte	0xc9f
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0xc9f
	.uleb128 0x17
	.4byte	0x33
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0xc68
	.uleb128 0x4
	.4byte	.LASF198
	.byte	0x13
	.byte	0x38
	.byte	0x10
	.4byte	0xcb1
	.uleb128 0x7
	.byte	0x4
	.4byte	0xcb7
	.uleb128 0x18
	.4byte	0xcc7
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0xc9f
	.byte	0
	.uleb128 0x4
	.4byte	.LASF199
	.byte	0x13
	.byte	0x3a
	.byte	0x15
	.4byte	0xcd3
	.uleb128 0x7
	.byte	0x4
	.4byte	0xcd9
	.uleb128 0x16
	.4byte	0x86e
	.4byte	0xcf7
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0xcf7
	.uleb128 0x17
	.4byte	0x41
	.uleb128 0x17
	.4byte	0x86e
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x874
	.uleb128 0x15
	.byte	0x90
	.byte	0x13
	.byte	0x5f
	.byte	0x9
	.4byte	0xedb
	.uleb128 0x9
	.4byte	.LASF200
	.byte	0x13
	.byte	0x64
	.byte	0x10
	.4byte	0x879
	.byte	0
	.uleb128 0x9
	.4byte	.LASF201
	.byte	0x13
	.byte	0x65
	.byte	0x10
	.4byte	0x879
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF202
	.byte	0x13
	.byte	0x66
	.byte	0x10
	.4byte	0x879
	.byte	0x8
	.uleb128 0x9
	.4byte	.LASF203
	.byte	0x13
	.byte	0x67
	.byte	0x10
	.4byte	0x879
	.byte	0xc
	.uleb128 0x9
	.4byte	.LASF204
	.byte	0x13
	.byte	0x68
	.byte	0x10
	.4byte	0x879
	.byte	0x10
	.uleb128 0x9
	.4byte	.LASF205
	.byte	0x13
	.byte	0x69
	.byte	0x11
	.4byte	0x89f
	.byte	0x14
	.uleb128 0x9
	.4byte	.LASF206
	.byte	0x13
	.byte	0x6a
	.byte	0xf
	.4byte	0x84d
	.byte	0x18
	.uleb128 0x9
	.4byte	.LASF207
	.byte	0x13
	.byte	0x6b
	.byte	0xf
	.4byte	0x84d
	.byte	0x1c
	.uleb128 0x9
	.4byte	.LASF208
	.byte	0x13
	.byte	0x6c
	.byte	0xf
	.4byte	0x84d
	.byte	0x20
	.uleb128 0x9
	.4byte	.LASF209
	.byte	0x13
	.byte	0x6d
	.byte	0xd
	.4byte	0x8ca
	.byte	0x24
	.uleb128 0x9
	.4byte	.LASF210
	.byte	0x13
	.byte	0x6e
	.byte	0xf
	.4byte	0x84d
	.byte	0x28
	.uleb128 0x9
	.4byte	.LASF211
	.byte	0x13
	.byte	0x6f
	.byte	0x10
	.4byte	0x879
	.byte	0x2c
	.uleb128 0x9
	.4byte	.LASF212
	.byte	0x13
	.byte	0x70
	.byte	0x10
	.4byte	0x879
	.byte	0x30
	.uleb128 0x9
	.4byte	.LASF213
	.byte	0x13
	.byte	0x71
	.byte	0x10
	.4byte	0x879
	.byte	0x34
	.uleb128 0x9
	.4byte	.LASF214
	.byte	0x13
	.byte	0x72
	.byte	0x10
	.4byte	0x879
	.byte	0x38
	.uleb128 0x9
	.4byte	.LASF215
	.byte	0x13
	.byte	0x73
	.byte	0x10
	.4byte	0x879
	.byte	0x3c
	.uleb128 0x9
	.4byte	.LASF216
	.byte	0x13
	.byte	0x74
	.byte	0xf
	.4byte	0x84d
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
	.4byte	0x84d
	.byte	0x48
	.uleb128 0x9
	.4byte	.LASF219
	.byte	0x13
	.byte	0x78
	.byte	0x10
	.4byte	0x879
	.byte	0x4c
	.uleb128 0x9
	.4byte	.LASF220
	.byte	0x13
	.byte	0x79
	.byte	0x10
	.4byte	0x879
	.byte	0x50
	.uleb128 0x9
	.4byte	.LASF221
	.byte	0x13
	.byte	0x7a
	.byte	0x10
	.4byte	0x879
	.byte	0x54
	.uleb128 0x9
	.4byte	.LASF222
	.byte	0x13
	.byte	0x7b
	.byte	0x10
	.4byte	0x879
	.byte	0x58
	.uleb128 0x9
	.4byte	.LASF223
	.byte	0x13
	.byte	0x7c
	.byte	0x11
	.4byte	0x89f
	.byte	0x5c
	.uleb128 0x9
	.4byte	.LASF224
	.byte	0x13
	.byte	0x7d
	.byte	0x10
	.4byte	0x879
	.byte	0x60
	.uleb128 0x9
	.4byte	.LASF225
	.byte	0x13
	.byte	0x7e
	.byte	0x10
	.4byte	0x879
	.byte	0x64
	.uleb128 0x9
	.4byte	.LASF226
	.byte	0x13
	.byte	0x7f
	.byte	0x10
	.4byte	0x879
	.byte	0x68
	.uleb128 0x9
	.4byte	.LASF227
	.byte	0x13
	.byte	0x80
	.byte	0x10
	.4byte	0x879
	.byte	0x6c
	.uleb128 0x9
	.4byte	.LASF228
	.byte	0x13
	.byte	0x81
	.byte	0x10
	.4byte	0x879
	.byte	0x70
	.uleb128 0x9
	.4byte	.LASF229
	.byte	0x13
	.byte	0x83
	.byte	0x10
	.4byte	0x879
	.byte	0x74
	.uleb128 0x9
	.4byte	.LASF230
	.byte	0x13
	.byte	0x84
	.byte	0x10
	.4byte	0x879
	.byte	0x78
	.uleb128 0x9
	.4byte	.LASF231
	.byte	0x13
	.byte	0x85
	.byte	0x10
	.4byte	0x879
	.byte	0x7c
	.uleb128 0x9
	.4byte	.LASF232
	.byte	0x13
	.byte	0x86
	.byte	0x10
	.4byte	0x879
	.byte	0x80
	.uleb128 0x9
	.4byte	.LASF233
	.byte	0x13
	.byte	0x88
	.byte	0xf
	.4byte	0x84d
	.byte	0x84
	.uleb128 0x9
	.4byte	.LASF234
	.byte	0x13
	.byte	0x8a
	.byte	0x10
	.4byte	0x879
	.byte	0x88
	.uleb128 0x9
	.4byte	.LASF235
	.byte	0x13
	.byte	0x8b
	.byte	0x10
	.4byte	0x879
	.byte	0x8c
	.byte	0
	.uleb128 0x4
	.4byte	.LASF236
	.byte	0x13
	.byte	0x8c
	.byte	0x3
	.4byte	0xcfd
	.uleb128 0x15
	.byte	0x28
	.byte	0x13
	.byte	0x8e
	.byte	0x9
	.4byte	0xf73
	.uleb128 0x9
	.4byte	.LASF237
	.byte	0x13
	.byte	0x8f
	.byte	0xd
	.4byte	0x8eb
	.byte	0
	.uleb128 0x9
	.4byte	.LASF238
	.byte	0x13
	.byte	0x90
	.byte	0x10
	.4byte	0x879
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF239
	.byte	0x13
	.byte	0x91
	.byte	0x12
	.4byte	0x90c
	.byte	0x8
	.uleb128 0x9
	.4byte	.LASF240
	.byte	0x13
	.byte	0x92
	.byte	0x12
	.4byte	0x90c
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
	.4byte	0x932
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
	.4byte	0x988
	.byte	0x1c
	.uleb128 0x9
	.4byte	.LASF245
	.byte	0x13
	.byte	0x98
	.byte	0x10
	.4byte	0x879
	.byte	0x20
	.uleb128 0x9
	.4byte	.LASF246
	.byte	0x13
	.byte	0x99
	.byte	0x12
	.4byte	0x90c
	.byte	0x24
	.byte	0
	.uleb128 0x4
	.4byte	.LASF247
	.byte	0x13
	.byte	0x9a
	.byte	0x3
	.4byte	0xee7
	.uleb128 0x15
	.byte	0xc
	.byte	0x13
	.byte	0x9c
	.byte	0x9
	.4byte	0xfb0
	.uleb128 0x9
	.4byte	.LASF248
	.byte	0x13
	.byte	0x9d
	.byte	0xd
	.4byte	0x8eb
	.byte	0
	.uleb128 0x9
	.4byte	.LASF249
	.byte	0x13
	.byte	0x9e
	.byte	0x10
	.4byte	0x879
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF250
	.byte	0x13
	.byte	0x9f
	.byte	0x13
	.4byte	0x95d
	.byte	0x8
	.byte	0
	.uleb128 0x4
	.4byte	.LASF251
	.byte	0x13
	.byte	0xa0
	.byte	0x3
	.4byte	0xf7f
	.uleb128 0x15
	.byte	0xc
	.byte	0x13
	.byte	0xa2
	.byte	0x9
	.4byte	0xfed
	.uleb128 0x9
	.4byte	.LASF252
	.byte	0x13
	.byte	0xa3
	.byte	0xf
	.4byte	0x84d
	.byte	0
	.uleb128 0x9
	.4byte	.LASF253
	.byte	0x13
	.byte	0xa4
	.byte	0xf
	.4byte	0x84d
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF254
	.byte	0x13
	.byte	0xa5
	.byte	0xf
	.4byte	0x84d
	.byte	0x8
	.byte	0
	.uleb128 0x4
	.4byte	.LASF255
	.byte	0x13
	.byte	0xa6
	.byte	0x3
	.4byte	0xfbc
	.uleb128 0x15
	.byte	0x8
	.byte	0x13
	.byte	0xa8
	.byte	0x9
	.4byte	0x101d
	.uleb128 0x9
	.4byte	.LASF256
	.byte	0x13
	.byte	0xa9
	.byte	0x14
	.4byte	0xc74
	.byte	0
	.uleb128 0x9
	.4byte	.LASF257
	.byte	0x13
	.byte	0xaa
	.byte	0x18
	.4byte	0xca5
	.byte	0x4
	.byte	0
	.uleb128 0x4
	.4byte	.LASF258
	.byte	0x13
	.byte	0xab
	.byte	0x3
	.4byte	0xff9
	.uleb128 0x7
	.byte	0x4
	.4byte	0xfed
	.uleb128 0x7
	.byte	0x4
	.4byte	0xedb
	.uleb128 0x7
	.byte	0x4
	.4byte	0xf73
	.uleb128 0x7
	.byte	0x4
	.4byte	0xfb0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x101d
	.uleb128 0x8
	.4byte	.LASF259
	.byte	0x10
	.byte	0x14
	.byte	0x33
	.byte	0x8
	.4byte	0x1089
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
	.4byte	0x15d4
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF262
	.byte	0x14
	.byte	0x36
	.byte	0x11
	.4byte	0x33
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
	.4byte	0x1047
	.uleb128 0xb
	.4byte	.LASF264
	.uleb128 0x7
	.byte	0x4
	.4byte	0x108f
	.uleb128 0x8
	.4byte	.LASF265
	.byte	0x14
	.byte	0x15
	.byte	0xb
	.byte	0x10
	.4byte	0x10e9
	.uleb128 0x9
	.4byte	.LASF266
	.byte	0x15
	.byte	0xc
	.byte	0x11
	.4byte	0x2f7
	.byte	0
	.uleb128 0x19
	.ascii	"get\000"
	.byte	0x15
	.byte	0xd
	.byte	0xc
	.4byte	0x19df
	.byte	0x4
	.uleb128 0x19
	.ascii	"set\000"
	.byte	0x15
	.byte	0xe
	.byte	0xc
	.4byte	0x19eb
	.byte	0x8
	.uleb128 0x19
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
	.4byte	0x109a
	.uleb128 0x16
	.4byte	0x33
	.4byte	0x1108
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0x31a
	.uleb128 0x17
	.4byte	0x33
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x10ef
	.uleb128 0x1a
	.4byte	.LASF268
	.byte	0x13
	.2byte	0x10e
	.byte	0x3
	.4byte	0x570
	.uleb128 0x10
	.4byte	.LASF269
	.byte	0x13
	.2byte	0x182
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0x10
	.4byte	.LASF270
	.byte	0x13
	.2byte	0x183
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xc
	.4byte	0x33
	.4byte	0x1140
	.uleb128 0xf
	.byte	0
	.uleb128 0x10
	.4byte	.LASF271
	.byte	0x13
	.2byte	0x188
	.byte	0x11
	.4byte	0x1135
	.uleb128 0x15
	.byte	0x8
	.byte	0x16
	.byte	0x3a
	.byte	0x5
	.4byte	0x1171
	.uleb128 0x9
	.4byte	.LASF272
	.byte	0x16
	.byte	0x3b
	.byte	0x13
	.4byte	0x428
	.byte	0
	.uleb128 0x9
	.4byte	.LASF273
	.byte	0x16
	.byte	0x3c
	.byte	0x13
	.4byte	0x428
	.byte	0x4
	.byte	0
	.uleb128 0x15
	.byte	0x10
	.byte	0x16
	.byte	0x3f
	.byte	0x5
	.4byte	0x1193
	.uleb128 0x19
	.ascii	"k0\000"
	.byte	0x16
	.byte	0x40
	.byte	0x12
	.4byte	0x410
	.byte	0
	.uleb128 0x19
	.ascii	"k1\000"
	.byte	0x16
	.byte	0x41
	.byte	0x12
	.4byte	0x410
	.byte	0x8
	.byte	0
	.uleb128 0x15
	.byte	0x14
	.byte	0x16
	.byte	0x44
	.byte	0x5
	.4byte	0x11b7
	.uleb128 0x9
	.4byte	.LASF274
	.byte	0x16
	.byte	0x45
	.byte	0x17
	.4byte	0x11b7
	.byte	0
	.uleb128 0x9
	.4byte	.LASF273
	.byte	0x16
	.byte	0x46
	.byte	0x13
	.4byte	0x428
	.byte	0x10
	.byte	0
	.uleb128 0xc
	.4byte	0x54
	.4byte	0x11c7
	.uleb128 0xd
	.4byte	0x3a
	.byte	0xf
	.byte	0
	.uleb128 0x15
	.byte	0x14
	.byte	0x16
	.byte	0x48
	.byte	0x5
	.4byte	0x11eb
	.uleb128 0x9
	.4byte	.LASF274
	.byte	0x16
	.byte	0x49
	.byte	0x17
	.4byte	0x11b7
	.byte	0
	.uleb128 0x9
	.4byte	.LASF275
	.byte	0x16
	.byte	0x4a
	.byte	0x13
	.4byte	0x428
	.byte	0x10
	.byte	0
	.uleb128 0x1b
	.byte	0x18
	.byte	0x16
	.byte	0x36
	.byte	0x9
	.4byte	0x1230
	.uleb128 0x1c
	.ascii	"uc\000"
	.byte	0x16
	.byte	0x38
	.byte	0x13
	.4byte	0x1230
	.uleb128 0x1c
	.ascii	"fnv\000"
	.byte	0x16
	.byte	0x3d
	.byte	0x7
	.4byte	0x114d
	.uleb128 0x1d
	.4byte	.LASF276
	.byte	0x16
	.byte	0x42
	.byte	0x7
	.4byte	0x1171
	.uleb128 0x1d
	.4byte	.LASF277
	.byte	0x16
	.byte	0x47
	.byte	0x7
	.4byte	0x1193
	.uleb128 0x1d
	.4byte	.LASF278
	.byte	0x16
	.byte	0x4b
	.byte	0x7
	.4byte	0x11c7
	.byte	0
	.uleb128 0xc
	.4byte	0x54
	.4byte	0x1240
	.uleb128 0xd
	.4byte	0x3a
	.byte	0x17
	.byte	0
	.uleb128 0x4
	.4byte	.LASF279
	.byte	0x16
	.byte	0x4c
	.byte	0x3
	.4byte	0x11eb
	.uleb128 0xe
	.4byte	.LASF280
	.byte	0x16
	.byte	0x4d
	.byte	0x1e
	.4byte	0x1240
	.uleb128 0xe
	.4byte	.LASF281
	.byte	0x17
	.byte	0xa
	.byte	0x11
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF282
	.byte	0x17
	.byte	0xb
	.byte	0x11
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF283
	.byte	0x17
	.byte	0xc
	.byte	0x11
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF284
	.byte	0x17
	.byte	0xd
	.byte	0x11
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF285
	.byte	0x17
	.byte	0xe
	.byte	0x11
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF286
	.byte	0x17
	.byte	0xf
	.byte	0x11
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF287
	.byte	0x17
	.byte	0x10
	.byte	0x11
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF288
	.byte	0x17
	.byte	0x11
	.byte	0x11
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF289
	.byte	0x17
	.byte	0x12
	.byte	0x11
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF290
	.byte	0x17
	.byte	0x13
	.byte	0x11
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF291
	.byte	0x17
	.byte	0x14
	.byte	0x11
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF292
	.byte	0x17
	.byte	0x15
	.byte	0x11
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF293
	.byte	0x17
	.byte	0x16
	.byte	0x11
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF294
	.byte	0x17
	.byte	0x17
	.byte	0x11
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF295
	.byte	0x17
	.byte	0x18
	.byte	0x11
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF296
	.byte	0x18
	.byte	0x22
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF297
	.byte	0x18
	.byte	0x23
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xc
	.4byte	0x103
	.4byte	0x132f
	.uleb128 0xf
	.byte	0
	.uleb128 0xe
	.4byte	.LASF298
	.byte	0x18
	.byte	0x38
	.byte	0x12
	.4byte	0x1324
	.uleb128 0xe
	.4byte	.LASF299
	.byte	0x19
	.byte	0x2c
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF300
	.byte	0x19
	.byte	0x2d
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF301
	.byte	0x1a
	.byte	0x6f
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF302
	.byte	0x1a
	.byte	0x70
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xc
	.4byte	0x5b
	.4byte	0x1376
	.uleb128 0xf
	.byte	0
	.uleb128 0x5
	.4byte	0x136b
	.uleb128 0x10
	.4byte	.LASF303
	.byte	0x1b
	.2byte	0x430
	.byte	0x21
	.4byte	0x1376
	.uleb128 0x8
	.4byte	.LASF304
	.byte	0x10
	.byte	0x1c
	.byte	0x55
	.byte	0x8
	.4byte	0x13b0
	.uleb128 0x9
	.4byte	.LASF102
	.byte	0x1c
	.byte	0x56
	.byte	0x5
	.4byte	0x841
	.byte	0
	.uleb128 0x9
	.4byte	.LASF305
	.byte	0x1c
	.byte	0x57
	.byte	0xb
	.4byte	0x13fc
	.byte	0xc
	.byte	0
	.uleb128 0xe
	.4byte	.LASF306
	.byte	0x1d
	.byte	0xc
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xc
	.4byte	0x54
	.4byte	0x13cc
	.uleb128 0xd
	.4byte	0x3a
	.byte	0xff
	.byte	0
	.uleb128 0xe
	.4byte	.LASF307
	.byte	0x1d
	.byte	0x4f
	.byte	0x1b
	.4byte	0x13bc
	.uleb128 0xe
	.4byte	.LASF308
	.byte	0x1d
	.byte	0xe8
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF309
	.byte	0x1d
	.byte	0xe9
	.byte	0x18
	.4byte	0x86e
	.uleb128 0x4
	.4byte	.LASF310
	.byte	0x1c
	.byte	0x35
	.byte	0x18
	.4byte	0x60
	.uleb128 0xc
	.4byte	0x13f0
	.4byte	0x140c
	.uleb128 0xd
	.4byte	0x3a
	.byte	0
	.byte	0
	.uleb128 0xe
	.4byte	.LASF311
	.byte	0x1e
	.byte	0xa
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF312
	.byte	0x1e
	.byte	0x12
	.byte	0x20
	.4byte	0x1388
	.uleb128 0xe
	.4byte	.LASF313
	.byte	0x1e
	.byte	0x12
	.byte	0x31
	.4byte	0x1388
	.uleb128 0xe
	.4byte	.LASF314
	.byte	0x1f
	.byte	0x15
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF315
	.byte	0x20
	.byte	0x27
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF316
	.byte	0x21
	.byte	0x12
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF317
	.byte	0x21
	.byte	0x13
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF318
	.byte	0x21
	.byte	0x14
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF319
	.byte	0x22
	.byte	0xa
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF320
	.byte	0x22
	.byte	0xc
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF321
	.byte	0x23
	.byte	0x17
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF322
	.byte	0x23
	.byte	0x18
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF323
	.byte	0x24
	.byte	0x2b
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF324
	.byte	0x24
	.byte	0x2c
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF325
	.byte	0x24
	.byte	0x2d
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF326
	.byte	0x24
	.byte	0x2e
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF327
	.byte	0x25
	.byte	0xf
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF328
	.byte	0x25
	.byte	0x3f
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF329
	.byte	0x25
	.byte	0x40
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF330
	.byte	0x25
	.byte	0x41
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF331
	.byte	0x25
	.byte	0x4c
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF332
	.byte	0x25
	.byte	0x4d
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF333
	.byte	0x25
	.byte	0x4e
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF334
	.byte	0x25
	.byte	0x50
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF335
	.byte	0x25
	.byte	0x51
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF336
	.byte	0x25
	.byte	0x52
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF337
	.byte	0x26
	.byte	0xf
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF338
	.byte	0x26
	.byte	0x10
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF339
	.byte	0x26
	.byte	0x11
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF340
	.byte	0x26
	.byte	0x12
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF341
	.byte	0x26
	.byte	0x13
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF342
	.byte	0x27
	.byte	0xa
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF343
	.byte	0x27
	.byte	0xb
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF344
	.byte	0x28
	.byte	0x45
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF345
	.byte	0x28
	.byte	0x4d
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF346
	.byte	0x28
	.byte	0x4e
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF347
	.byte	0x28
	.byte	0x4f
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF348
	.byte	0x14
	.byte	0xe
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0x4
	.4byte	.LASF349
	.byte	0x14
	.byte	0x12
	.byte	0x15
	.4byte	0x885
	.uleb128 0xe
	.4byte	.LASF350
	.byte	0x29
	.byte	0xa
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF351
	.byte	0x29
	.byte	0x29
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0x1e
	.4byte	0x86e
	.uleb128 0x7
	.byte	0x4
	.4byte	0x15f8
	.uleb128 0xe
	.4byte	.LASF352
	.byte	0x2a
	.byte	0x2c
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF353
	.byte	0x2a
	.byte	0x5e
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF354
	.byte	0x2a
	.byte	0x5f
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF355
	.byte	0x2b
	.byte	0x14
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF356
	.byte	0x2b
	.byte	0x2b
	.byte	0x1a
	.4byte	0x110e
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
	.4byte	0x33
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
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF361
	.byte	0x2d
	.byte	0x16
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1681
	.uleb128 0x16
	.4byte	0x86e
	.4byte	0x1695
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0x52
	.byte	0
	.uleb128 0xe
	.4byte	.LASF362
	.byte	0x2e
	.byte	0x15
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF363
	.byte	0x2f
	.byte	0xf
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xb
	.4byte	.LASF364
	.uleb128 0x7
	.byte	0x4
	.4byte	0x16ad
	.uleb128 0xe
	.4byte	.LASF365
	.byte	0x30
	.byte	0x9
	.byte	0x16
	.4byte	0x811
	.uleb128 0xe
	.4byte	.LASF366
	.byte	0x30
	.byte	0x1c
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF367
	.byte	0x30
	.byte	0x1d
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF368
	.byte	0x31
	.byte	0xe
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF369
	.byte	0x32
	.byte	0x8
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF370
	.byte	0x32
	.byte	0x9
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF371
	.byte	0x32
	.byte	0xa
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0x4
	.4byte	.LASF372
	.byte	0x33
	.byte	0x17
	.byte	0x14
	.4byte	0x1718
	.uleb128 0x1f
	.ascii	"_ts\000"
	.byte	0x98
	.byte	0x34
	.byte	0x33
	.byte	0x8
	.4byte	0x18d2
	.uleb128 0x9
	.4byte	.LASF373
	.byte	0x34
	.byte	0x36
	.byte	0x11
	.4byte	0x1967
	.byte	0
	.uleb128 0x9
	.4byte	.LASF374
	.byte	0x34
	.byte	0x37
	.byte	0x11
	.4byte	0x1967
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF375
	.byte	0x34
	.byte	0x38
	.byte	0x19
	.4byte	0x196d
	.byte	0x8
	.uleb128 0x9
	.4byte	.LASF376
	.byte	0x34
	.byte	0x3b
	.byte	0x14
	.4byte	0x16b2
	.byte	0xc
	.uleb128 0x9
	.4byte	.LASF377
	.byte	0x34
	.byte	0x3c
	.byte	0x9
	.4byte	0x33
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
	.4byte	0x33
	.byte	0x18
	.uleb128 0x9
	.4byte	.LASF94
	.byte	0x34
	.byte	0x46
	.byte	0x9
	.4byte	0x33
	.byte	0x1c
	.uleb128 0x9
	.4byte	.LASF381
	.byte	0x34
	.byte	0x47
	.byte	0x9
	.4byte	0x33
	.byte	0x20
	.uleb128 0x9
	.4byte	.LASF382
	.byte	0x34
	.byte	0x49
	.byte	0x12
	.4byte	0x18e3
	.byte	0x24
	.uleb128 0x9
	.4byte	.LASF383
	.byte	0x34
	.byte	0x4a
	.byte	0x12
	.4byte	0x18e3
	.byte	0x28
	.uleb128 0x9
	.4byte	.LASF384
	.byte	0x34
	.byte	0x4b
	.byte	0xf
	.4byte	0x86e
	.byte	0x2c
	.uleb128 0x9
	.4byte	.LASF385
	.byte	0x34
	.byte	0x4c
	.byte	0xf
	.4byte	0x86e
	.byte	0x30
	.uleb128 0x9
	.4byte	.LASF386
	.byte	0x34
	.byte	0x4f
	.byte	0xf
	.4byte	0x86e
	.byte	0x34
	.uleb128 0x9
	.4byte	.LASF387
	.byte	0x34
	.byte	0x50
	.byte	0xf
	.4byte	0x86e
	.byte	0x38
	.uleb128 0x9
	.4byte	.LASF388
	.byte	0x34
	.byte	0x51
	.byte	0xf
	.4byte	0x86e
	.byte	0x3c
	.uleb128 0x9
	.4byte	.LASF389
	.byte	0x34
	.byte	0x56
	.byte	0x16
	.4byte	0x195b
	.byte	0x40
	.uleb128 0x9
	.4byte	.LASF390
	.byte	0x34
	.byte	0x5a
	.byte	0x17
	.4byte	0x1973
	.byte	0x50
	.uleb128 0x9
	.4byte	.LASF391
	.byte	0x34
	.byte	0x5c
	.byte	0xf
	.4byte	0x86e
	.byte	0x54
	.uleb128 0x9
	.4byte	.LASF392
	.byte	0x34
	.byte	0x5e
	.byte	0x9
	.4byte	0x33
	.byte	0x58
	.uleb128 0x9
	.4byte	.LASF393
	.byte	0x34
	.byte	0x60
	.byte	0xf
	.4byte	0x86e
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
	.4byte	0x33
	.byte	0x64
	.uleb128 0x9
	.4byte	.LASF396
	.byte	0x34
	.byte	0x64
	.byte	0xf
	.4byte	0x86e
	.byte	0x68
	.uleb128 0x9
	.4byte	.LASF397
	.byte	0x34
	.byte	0x7d
	.byte	0xc
	.4byte	0xa0b
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
	.4byte	0x33
	.byte	0x74
	.uleb128 0x9
	.4byte	.LASF400
	.byte	0x34
	.byte	0x82
	.byte	0xf
	.4byte	0x86e
	.byte	0x78
	.uleb128 0x9
	.4byte	.LASF401
	.byte	0x34
	.byte	0x83
	.byte	0xf
	.4byte	0x86e
	.byte	0x7c
	.uleb128 0x9
	.4byte	.LASF402
	.byte	0x34
	.byte	0x85
	.byte	0xf
	.4byte	0x86e
	.byte	0x80
	.uleb128 0x9
	.4byte	.LASF403
	.byte	0x34
	.byte	0x86
	.byte	0xe
	.4byte	0x410
	.byte	0x88
	.uleb128 0x19
	.ascii	"id\000"
	.byte	0x34
	.byte	0x89
	.byte	0xe
	.4byte	0x410
	.byte	0x90
	.byte	0
	.uleb128 0x4
	.4byte	.LASF404
	.byte	0x33
	.byte	0x19
	.byte	0x14
	.4byte	0x18de
	.uleb128 0x20
	.ascii	"_is\000"
	.uleb128 0x4
	.4byte	.LASF405
	.byte	0x34
	.byte	0x13
	.byte	0xf
	.4byte	0x18ef
	.uleb128 0x7
	.byte	0x4
	.4byte	0x18f5
	.uleb128 0x16
	.4byte	0x33
	.4byte	0x1913
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0x16b2
	.uleb128 0x17
	.4byte	0x33
	.uleb128 0x17
	.4byte	0x86e
	.byte	0
	.uleb128 0x8
	.4byte	.LASF406
	.byte	0x10
	.byte	0x34
	.byte	0x24
	.byte	0x10
	.4byte	0x1955
	.uleb128 0x9
	.4byte	.LASF407
	.byte	0x34
	.byte	0x2b
	.byte	0xf
	.4byte	0x86e
	.byte	0
	.uleb128 0x9
	.4byte	.LASF408
	.byte	0x34
	.byte	0x2b
	.byte	0x1a
	.4byte	0x86e
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF409
	.byte	0x34
	.byte	0x2b
	.byte	0x26
	.4byte	0x86e
	.byte	0x8
	.uleb128 0x9
	.4byte	.LASF410
	.byte	0x34
	.byte	0x2d
	.byte	0x1c
	.4byte	0x1955
	.byte	0xc
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1913
	.uleb128 0x4
	.4byte	.LASF411
	.byte	0x34
	.byte	0x2f
	.byte	0x3
	.4byte	0x1913
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1718
	.uleb128 0x7
	.byte	0x4
	.4byte	0x18d2
	.uleb128 0x7
	.byte	0x4
	.4byte	0x195b
	.uleb128 0x7
	.byte	0x4
	.4byte	0x170c
	.uleb128 0xe
	.4byte	.LASF412
	.byte	0x35
	.byte	0x26
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF413
	.byte	0x35
	.byte	0x3b
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF414
	.byte	0x35
	.byte	0x3c
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF415
	.byte	0x35
	.byte	0x3e
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF416
	.byte	0x35
	.byte	0x57
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF417
	.byte	0x35
	.byte	0x58
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF418
	.byte	0x35
	.byte	0x59
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF419
	.byte	0x35
	.byte	0x5a
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0x4
	.4byte	.LASF420
	.byte	0x15
	.byte	0x8
	.byte	0x15
	.4byte	0x167b
	.uleb128 0x4
	.4byte	.LASF421
	.byte	0x15
	.byte	0x9
	.byte	0xf
	.4byte	0x19f7
	.uleb128 0x7
	.byte	0x4
	.4byte	0x19fd
	.uleb128 0x16
	.4byte	0x33
	.4byte	0x1a16
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0x86e
	.uleb128 0x17
	.4byte	0x52
	.byte	0
	.uleb128 0xe
	.4byte	.LASF422
	.byte	0x15
	.byte	0x4c
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF423
	.byte	0x15
	.byte	0x4d
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF424
	.byte	0x15
	.byte	0x4e
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF425
	.byte	0x15
	.byte	0x4f
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF426
	.byte	0x15
	.byte	0x50
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF427
	.byte	0x15
	.byte	0x51
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF428
	.byte	0x15
	.byte	0x53
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF429
	.byte	0x15
	.byte	0x67
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF430
	.byte	0x36
	.byte	0x2b
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF431
	.byte	0x36
	.byte	0x2c
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF432
	.byte	0x36
	.byte	0x2d
	.byte	0x1a
	.4byte	0x110e
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
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF435
	.byte	0x39
	.byte	0xd
	.byte	0x1a
	.4byte	0x110e
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
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF438
	.byte	0x3b
	.byte	0x43
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF439
	.byte	0x3b
	.byte	0x45
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF440
	.byte	0x3b
	.byte	0x47
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF441
	.byte	0x3b
	.byte	0x48
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF442
	.byte	0x3b
	.byte	0x49
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF443
	.byte	0x3b
	.byte	0x4a
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF444
	.byte	0x3b
	.byte	0x4c
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF445
	.byte	0x3b
	.byte	0x4d
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF446
	.byte	0x3b
	.byte	0x4e
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF447
	.byte	0x3b
	.byte	0x4f
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF448
	.byte	0x3b
	.byte	0x50
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF449
	.byte	0x3b
	.byte	0x51
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF450
	.byte	0x3b
	.byte	0x52
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF451
	.byte	0x3b
	.byte	0x54
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF452
	.byte	0x3b
	.byte	0x56
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF453
	.byte	0x3b
	.byte	0x57
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF454
	.byte	0x3b
	.byte	0x58
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF455
	.byte	0x3b
	.byte	0x59
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF456
	.byte	0x3b
	.byte	0x5a
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF457
	.byte	0x3b
	.byte	0x5b
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF458
	.byte	0x3b
	.byte	0x5c
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF459
	.byte	0x3b
	.byte	0x5e
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF460
	.byte	0x3b
	.byte	0x60
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF461
	.byte	0x3b
	.byte	0x61
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF462
	.byte	0x3b
	.byte	0x62
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF463
	.byte	0x3b
	.byte	0x63
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF464
	.byte	0x3b
	.byte	0x64
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF465
	.byte	0x3b
	.byte	0x65
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF466
	.byte	0x3b
	.byte	0x66
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF467
	.byte	0x3b
	.byte	0x67
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF468
	.byte	0x3b
	.byte	0x68
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF469
	.byte	0x3b
	.byte	0x69
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF470
	.byte	0x3b
	.byte	0x6a
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF471
	.byte	0x3b
	.byte	0x6b
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF472
	.byte	0x3b
	.byte	0x6c
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF473
	.byte	0x3b
	.byte	0x6d
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF474
	.byte	0x3b
	.byte	0x6e
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF475
	.byte	0x3b
	.byte	0x71
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF476
	.byte	0x3b
	.byte	0x72
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF477
	.byte	0x3b
	.byte	0x73
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF478
	.byte	0x3b
	.byte	0x74
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF479
	.byte	0x3b
	.byte	0x75
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF480
	.byte	0x3b
	.byte	0x76
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF481
	.byte	0x3b
	.byte	0x77
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF482
	.byte	0x3b
	.byte	0x78
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF483
	.byte	0x3b
	.byte	0x79
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF484
	.byte	0x3b
	.byte	0x7a
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF485
	.byte	0x3b
	.byte	0x7b
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF486
	.byte	0x3b
	.byte	0x7c
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF487
	.byte	0x3b
	.byte	0x7d
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF488
	.byte	0x3b
	.byte	0x7e
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF489
	.byte	0x3b
	.byte	0x7f
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF490
	.byte	0x3b
	.byte	0x84
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF491
	.byte	0x3b
	.byte	0x85
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF492
	.byte	0x3b
	.byte	0x8b
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF493
	.byte	0x3b
	.byte	0x8c
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF494
	.byte	0x3b
	.byte	0x8d
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF495
	.byte	0x3b
	.byte	0x8e
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF496
	.byte	0x3b
	.byte	0x8f
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF497
	.byte	0x3b
	.byte	0x90
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF498
	.byte	0x3b
	.byte	0x91
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF499
	.byte	0x3b
	.byte	0x92
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF500
	.byte	0x3b
	.byte	0x93
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF501
	.byte	0x3b
	.byte	0x94
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF502
	.byte	0x3b
	.byte	0x95
	.byte	0x18
	.4byte	0x86e
	.uleb128 0xe
	.4byte	.LASF503
	.byte	0x3c
	.byte	0xa
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF504
	.byte	0x3c
	.byte	0xd
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF505
	.byte	0x3c
	.byte	0x10
	.byte	0x1a
	.4byte	0x110e
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
	.4byte	0x110e
	.uleb128 0x1e
	.4byte	0x33
	.uleb128 0xe
	.4byte	.LASF508
	.byte	0x3f
	.byte	0xba
	.byte	0x13
	.4byte	0x1e2f
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1e1e
	.uleb128 0x16
	.4byte	0xfd
	.4byte	0x1e4e
	.uleb128 0x17
	.4byte	0x31a
	.uleb128 0x17
	.4byte	0x31a
	.uleb128 0x17
	.4byte	0x2f7
	.byte	0
	.uleb128 0xe
	.4byte	.LASF509
	.byte	0x3f
	.byte	0xbb
	.byte	0x15
	.4byte	0x1e5a
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1e35
	.uleb128 0xe
	.4byte	.LASF510
	.byte	0x3f
	.byte	0xbd
	.byte	0x1c
	.4byte	0x1979
	.uleb128 0xe
	.4byte	.LASF511
	.byte	0x40
	.byte	0x65
	.byte	0x11
	.4byte	0x33
	.uleb128 0x8
	.4byte	.LASF512
	.byte	0x8
	.byte	0x41
	.byte	0x76
	.byte	0x8
	.4byte	0x1ea0
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
	.4byte	0x15fd
	.byte	0x4
	.byte	0
	.uleb128 0xe
	.4byte	.LASF514
	.byte	0x41
	.byte	0x7a
	.byte	0x1f
	.4byte	0x1eac
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1e78
	.uleb128 0xe
	.4byte	.LASF515
	.byte	0x41
	.byte	0x7e
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0x8
	.4byte	.LASF516
	.byte	0xc
	.byte	0x41
	.byte	0x86
	.byte	0x8
	.4byte	0x1ef3
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
	.4byte	0x1ef8
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF518
	.byte	0x41
	.byte	0x89
	.byte	0x9
	.4byte	0x33
	.byte	0x8
	.byte	0
	.uleb128 0x5
	.4byte	0x1ebe
	.uleb128 0x7
	.byte	0x4
	.4byte	0x5b
	.uleb128 0xe
	.4byte	.LASF519
	.byte	0x41
	.byte	0x8f
	.byte	0x24
	.4byte	0x1f0a
	.uleb128 0x7
	.byte	0x4
	.4byte	0x1ef3
	.uleb128 0xe
	.4byte	.LASF520
	.byte	0x42
	.byte	0x7
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF521
	.byte	0x42
	.byte	0x8
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xe
	.4byte	.LASF522
	.byte	0x42
	.byte	0x9
	.byte	0x1a
	.4byte	0x110e
	.uleb128 0xc
	.4byte	0x4d
	.4byte	0x1f44
	.uleb128 0xd
	.4byte	0x3a
	.byte	0xff
	.byte	0
	.uleb128 0x5
	.4byte	0x1f34
	.uleb128 0xe
	.4byte	.LASF523
	.byte	0x43
	.byte	0xd
	.byte	0x20
	.4byte	0x1f44
	.uleb128 0xc
	.4byte	0x5b
	.4byte	0x1f65
	.uleb128 0xd
	.4byte	0x3a
	.byte	0xff
	.byte	0
	.uleb128 0x5
	.4byte	0x1f55
	.uleb128 0xe
	.4byte	.LASF524
	.byte	0x43
	.byte	0x1a
	.byte	0x21
	.4byte	0x1f65
	.uleb128 0xe
	.4byte	.LASF525
	.byte	0x43
	.byte	0x1b
	.byte	0x21
	.4byte	0x1f65
	.uleb128 0x8
	.4byte	.LASF526
	.byte	0x6
	.byte	0x44
	.byte	0x14
	.byte	0x8
	.4byte	0x1fb1
	.uleb128 0x19
	.ascii	"r\000"
	.byte	0x44
	.byte	0x15
	.byte	0xb
	.4byte	0x390
	.byte	0
	.uleb128 0x19
	.ascii	"g\000"
	.byte	0x44
	.byte	0x15
	.byte	0xe
	.4byte	0x390
	.byte	0x2
	.uleb128 0x19
	.ascii	"b\000"
	.byte	0x44
	.byte	0x15
	.byte	0x11
	.4byte	0x390
	.byte	0x4
	.byte	0
	.uleb128 0x8
	.4byte	.LASF527
	.byte	0x78
	.byte	0x44
	.byte	0x18
	.byte	0x8
	.4byte	0x212b
	.uleb128 0x9
	.4byte	.LASF528
	.byte	0x44
	.byte	0x19
	.byte	0xc
	.4byte	0x404
	.byte	0
	.uleb128 0x9
	.4byte	.LASF529
	.byte	0x44
	.byte	0x1b
	.byte	0xc
	.4byte	0x212b
	.byte	0x4
	.uleb128 0x9
	.4byte	.LASF530
	.byte	0x44
	.byte	0x1c
	.byte	0xc
	.4byte	0x212b
	.byte	0x8
	.uleb128 0x9
	.4byte	.LASF531
	.byte	0x44
	.byte	0x1d
	.byte	0xc
	.4byte	0x212b
	.byte	0xc
	.uleb128 0x9
	.4byte	.LASF532
	.byte	0x44
	.byte	0x1e
	.byte	0xc
	.4byte	0x212b
	.byte	0x10
	.uleb128 0x9
	.4byte	.LASF533
	.byte	0x44
	.byte	0x20
	.byte	0xc
	.4byte	0x212b
	.byte	0x14
	.uleb128 0x9
	.4byte	.LASF534
	.byte	0x44
	.byte	0x21
	.byte	0xc
	.4byte	0x212b
	.byte	0x18
	.uleb128 0x9
	.4byte	.LASF535
	.byte	0x44
	.byte	0x23
	.byte	0xc
	.4byte	0x404
	.byte	0x1c
	.uleb128 0x9
	.4byte	.LASF536
	.byte	0x44
	.byte	0x24
	.byte	0xc
	.4byte	0x404
	.byte	0x20
	.uleb128 0x9
	.4byte	.LASF537
	.byte	0x44
	.byte	0x25
	.byte	0xc
	.4byte	0x404
	.byte	0x24
	.uleb128 0x9
	.4byte	.LASF538
	.byte	0x44
	.byte	0x26
	.byte	0xc
	.4byte	0x404
	.byte	0x28
	.uleb128 0x9
	.4byte	.LASF539
	.byte	0x44
	.byte	0x27
	.byte	0xc
	.4byte	0x404
	.byte	0x2c
	.uleb128 0x9
	.4byte	.LASF540
	.byte	0x44
	.byte	0x28
	.byte	0xc
	.4byte	0x404
	.byte	0x30
	.uleb128 0x9
	.4byte	.LASF541
	.byte	0x44
	.byte	0x29
	.byte	0xc
	.4byte	0x404
	.byte	0x34
	.uleb128 0x9
	.4byte	.LASF542
	.byte	0x44
	.byte	0x2a
	.byte	0xc
	.4byte	0x404
	.byte	0x38
	.uleb128 0x9
	.4byte	.LASF543
	.byte	0x44
	.byte	0x2b
	.byte	0xc
	.4byte	0x404
	.byte	0x3c
	.uleb128 0x9
	.4byte	.LASF544
	.byte	0x44
	.byte	0x2c
	.byte	0xc
	.4byte	0x404
	.byte	0x40
	.uleb128 0x9
	.4byte	.LASF518
	.byte	0x44
	.byte	0x2d
	.byte	0xc
	.4byte	0x404
	.byte	0x44
	.uleb128 0x9
	.4byte	.LASF545
	.byte	0x44
	.byte	0x2f
	.byte	0xc
	.4byte	0x404
	.byte	0x48
	.uleb128 0x9
	.4byte	.LASF546
	.byte	0x44
	.byte	0x30
	.byte	0xc
	.4byte	0x404
	.byte	0x4c
	.uleb128 0x9
	.4byte	.LASF547
	.byte	0x44
	.byte	0x32
	.byte	0xc
	.4byte	0x404
	.byte	0x50
	.uleb128 0x9
	.4byte	.LASF548
	.byte	0x44
	.byte	0x33
	.byte	0xc
	.4byte	0x404
	.byte	0x54
	.uleb128 0x9
	.4byte	.LASF549
	.byte	0x44
	.byte	0x35
	.byte	0xd
	.4byte	0x2131
	.byte	0x58
	.uleb128 0x9
	.4byte	.LASF550
	.byte	0x44
	.byte	0x37
	.byte	0x1e
	.4byte	0x1f82
	.byte	0x5c
	.uleb128 0x9
	.4byte	.LASF551
	.byte	0x44
	.byte	0x38
	.byte	0x1e
	.4byte	0x1f82
	.byte	0x62
	.uleb128 0x9
	.4byte	.LASF552
	.byte	0x44
	.byte	0x39
	.byte	0x1e
	.4byte	0x1f82
	.byte	0x68
	.uleb128 0x9
	.4byte	.LASF553
	.byte	0x44
	.byte	0x3a
	.byte	0x1e
	.4byte	0x1f82
	.byte	0x6e
	.uleb128 0x9
	.4byte	.LASF554
	.byte	0x44
	.byte	0x3f
	.byte	0xd
	.4byte	0x2137
	.byte	0x74
	.byte	0
	.uleb128 0x7
	.byte	0x4
	.4byte	0x3ec
	.uleb128 0x7
	.byte	0x4
	.4byte	0x404
	.uleb128 0x7
	.byte	0x4
	.4byte	0x3f8
	.uleb128 0x21
	.4byte	.LASF555
	.byte	0x1
	.byte	0x25
	.byte	0x18
	.4byte	0x1fb1
	.uleb128 0x5
	.byte	0x3
	.4byte	g_armwave_state
	.uleb128 0xc
	.4byte	0x3ec
	.4byte	0x215f
	.uleb128 0xd
	.4byte	0x3a
	.byte	0xff
	.byte	0
	.uleb128 0x21
	.4byte	.LASF556
	.byte	0x1
	.byte	0x27
	.byte	0x9
	.4byte	0x214f
	.uleb128 0x5
	.byte	0x3
	.4byte	gamma_table
	.uleb128 0x22
	.4byte	.LASF557
	.byte	0x1
	.2byte	0x22c
	.byte	0x6
	.4byte	.LFB78
	.4byte	.LFE78-.LFB78
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x21ad
	.uleb128 0x23
	.4byte	.LVL309
	.4byte	0x358d
	.uleb128 0x23
	.4byte	.LVL310
	.4byte	0x358d
	.uleb128 0x23
	.4byte	.LVL311
	.4byte	0x358d
	.uleb128 0x23
	.4byte	.LVL312
	.4byte	0x358d
	.byte	0
	.uleb128 0x22
	.4byte	.LASF558
	.byte	0x1
	.2byte	0x204
	.byte	0x6
	.4byte	.LFB77
	.4byte	.LFE77-.LFB77
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x2271
	.uleb128 0x24
	.4byte	.LASF563
	.byte	0x1
	.2byte	0x204
	.byte	0x27
	.4byte	0x25
	.4byte	.LLST129
	.4byte	.LVUS129
	.uleb128 0x25
	.ascii	"v\000"
	.byte	0x1
	.2byte	0x206
	.byte	0xd
	.4byte	0x3ec
	.4byte	.LLST130
	.4byte	.LVUS130
	.uleb128 0x26
	.4byte	.LASF559
	.byte	0x1
	.2byte	0x207
	.byte	0xb
	.4byte	0x25
	.4byte	.LLST131
	.4byte	.LVUS131
	.uleb128 0x27
	.4byte	.LASF564
	.byte	0x1
	.2byte	0x207
	.byte	0x12
	.4byte	0x25
	.uleb128 0x26
	.4byte	.LASF560
	.byte	0x1
	.2byte	0x208
	.byte	0xb
	.4byte	0x25
	.4byte	.LLST132
	.4byte	.LVUS132
	.uleb128 0x26
	.4byte	.LASF561
	.byte	0x1
	.2byte	0x208
	.byte	0x19
	.4byte	0x25
	.4byte	.LLST133
	.4byte	.LVUS133
	.uleb128 0x25
	.ascii	"w\000"
	.byte	0x1
	.2byte	0x209
	.byte	0x9
	.4byte	0x33
	.4byte	.LLST134
	.4byte	.LVUS134
	.uleb128 0x25
	.ascii	"x\000"
	.byte	0x1
	.2byte	0x209
	.byte	0xc
	.4byte	0x33
	.4byte	.LLST135
	.4byte	.LVUS135
	.uleb128 0x23
	.4byte	.LVL294
	.4byte	0x359a
	.uleb128 0x23
	.4byte	.LVL299
	.4byte	0x359a
	.byte	0
	.uleb128 0x22
	.4byte	.LASF562
	.byte	0x1
	.2byte	0x1e0
	.byte	0x6
	.4byte	.LFB76
	.4byte	.LFE76-.LFB76
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x23b8
	.uleb128 0x28
	.ascii	"mod\000"
	.byte	0x1
	.2byte	0x1e0
	.byte	0x28
	.4byte	0x25
	.4byte	.LLST121
	.4byte	.LVUS121
	.uleb128 0x24
	.4byte	.LASF563
	.byte	0x1
	.2byte	0x1e0
	.byte	0x33
	.4byte	0x25
	.4byte	.LLST122
	.4byte	.LVUS122
	.uleb128 0x25
	.ascii	"v\000"
	.byte	0x1
	.2byte	0x1e2
	.byte	0xb
	.4byte	0x25
	.4byte	.LLST123
	.4byte	.LVUS123
	.uleb128 0x26
	.4byte	.LASF559
	.byte	0x1
	.2byte	0x1e2
	.byte	0xe
	.4byte	0x25
	.4byte	.LLST124
	.4byte	.LVUS124
	.uleb128 0x26
	.4byte	.LASF564
	.byte	0x1
	.2byte	0x1e2
	.byte	0x15
	.4byte	0x25
	.4byte	.LLST125
	.4byte	.LVUS125
	.uleb128 0x26
	.4byte	.LASF565
	.byte	0x1
	.2byte	0x1e2
	.byte	0x1d
	.4byte	0x25
	.4byte	.LLST126
	.4byte	.LVUS126
	.uleb128 0x25
	.ascii	"w\000"
	.byte	0x1
	.2byte	0x1e3
	.byte	0x9
	.4byte	0x33
	.4byte	.LLST127
	.4byte	.LVUS127
	.uleb128 0x25
	.ascii	"x\000"
	.byte	0x1
	.2byte	0x1e3
	.byte	0xc
	.4byte	0x33
	.4byte	.LLST128
	.4byte	.LVUS128
	.uleb128 0x29
	.4byte	0x24be
	.4byte	.LBI43
	.byte	.LVU898
	.4byte	.Ldebug_ranges0+0x98
	.byte	0x1
	.2byte	0x1e5
	.byte	0x5
	.4byte	0x2393
	.uleb128 0x23
	.4byte	.LVL266
	.4byte	0x358d
	.uleb128 0x2a
	.4byte	.LVL267
	.4byte	0x35a7
	.4byte	0x2360
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC20
	.byte	0
	.uleb128 0x2a
	.4byte	.LVL268
	.4byte	0x35b4
	.4byte	0x2379
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x2
	.byte	0x74
	.sleb128 0
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x1
	.byte	0x31
	.byte	0
	.uleb128 0x2c
	.4byte	.LVL290
	.4byte	0x35a7
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC21
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x2
	.byte	0x74
	.sleb128 0
	.byte	0
	.byte	0
	.uleb128 0x23
	.4byte	.LVL273
	.4byte	0x359a
	.uleb128 0x23
	.4byte	.LVL278
	.4byte	0x359a
	.uleb128 0x23
	.4byte	.LVL280
	.4byte	0x359a
	.uleb128 0x23
	.4byte	.LVL282
	.4byte	0x35c1
	.byte	0
	.uleb128 0x2d
	.4byte	.LASF634
	.byte	0x1
	.2byte	0x1c3
	.byte	0xb
	.4byte	0x86e
	.4byte	.LFB75
	.4byte	.LFE75-.LFB75
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x24be
	.uleb128 0x24
	.4byte	.LASF566
	.byte	0x1
	.2byte	0x1c3
	.byte	0x37
	.4byte	0x86e
	.4byte	.LLST119
	.4byte	.LVUS119
	.uleb128 0x2e
	.4byte	.LASF577
	.byte	0x1
	.2byte	0x1c5
	.byte	0xf
	.4byte	0xc68
	.uleb128 0x2
	.byte	0x91
	.sleb128 -52
	.uleb128 0x25
	.ascii	"ret\000"
	.byte	0x1
	.2byte	0x1c6
	.byte	0x9
	.4byte	0x33
	.4byte	.LLST120
	.4byte	.LVUS120
	.uleb128 0x29
	.4byte	0x3332
	.4byte	.LBI37
	.byte	.LVU886
	.4byte	.Ldebug_ranges0+0x80
	.byte	0x1
	.2byte	0x1ce
	.byte	0x9
	.4byte	0x2429
	.uleb128 0x2f
	.4byte	0x3340
	.byte	0
	.uleb128 0x2a
	.4byte	.LVL254
	.4byte	0x35cd
	.4byte	0x2440
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC22
	.byte	0
	.uleb128 0x2a
	.4byte	.LVL255
	.4byte	0x35d8
	.4byte	0x245f
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x2
	.byte	0x74
	.sleb128 0
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x2
	.byte	0x91
	.sleb128 -60
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x1
	.byte	0x31
	.byte	0
	.uleb128 0x23
	.4byte	.LVL257
	.4byte	0x2ed7
	.uleb128 0x2a
	.4byte	.LVL258
	.4byte	0x35cd
	.4byte	0x247f
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC24
	.byte	0
	.uleb128 0x2a
	.4byte	.LVL259
	.4byte	0x35e4
	.4byte	0x2493
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x2
	.byte	0x91
	.sleb128 -60
	.byte	0
	.uleb128 0x2a
	.4byte	.LVL260
	.4byte	0x35cd
	.4byte	0x24aa
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC25
	.byte	0
	.uleb128 0x2c
	.4byte	.LVL263
	.4byte	0x35cd
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC23
	.byte	0
	.byte	0
	.uleb128 0x30
	.4byte	.LASF635
	.byte	0x1
	.2byte	0x1b0
	.byte	0x6
	.byte	0x1
	.uleb128 0x22
	.4byte	.LASF567
	.byte	0x1
	.2byte	0x1a1
	.byte	0x6
	.4byte	.LFB73
	.4byte	.LFE73-.LFB73
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x265c
	.uleb128 0x28
	.ascii	"buf\000"
	.byte	0x1
	.2byte	0x1a1
	.byte	0x29
	.4byte	0x86e
	.4byte	.LLST97
	.4byte	.LVUS97
	.uleb128 0x26
	.4byte	.LASF549
	.byte	0x1
	.2byte	0x1a7
	.byte	0xb
	.4byte	0x52
	.4byte	.LLST98
	.4byte	.LVUS98
	.uleb128 0x31
	.4byte	0x2ed7
	.4byte	.LBI31
	.byte	.LVU757
	.4byte	.Ldebug_ranges0+0x60
	.byte	0x1
	.2byte	0x1aa
	.byte	0x5
	.uleb128 0x32
	.4byte	0x2ee4
	.4byte	.LLST99
	.4byte	.LVUS99
	.uleb128 0x33
	.4byte	.Ldebug_ranges0+0x60
	.uleb128 0x34
	.4byte	0x2ef0
	.4byte	.LLST100
	.4byte	.LVUS100
	.uleb128 0x34
	.4byte	0x2efb
	.4byte	.LLST101
	.4byte	.LVUS101
	.uleb128 0x35
	.4byte	0x2f06
	.uleb128 0x34
	.4byte	0x2f12
	.4byte	.LLST102
	.4byte	.LVUS102
	.uleb128 0x34
	.4byte	0x2f1e
	.4byte	.LLST103
	.4byte	.LVUS103
	.uleb128 0x34
	.4byte	0x2f2a
	.4byte	.LLST104
	.4byte	.LVUS104
	.uleb128 0x34
	.4byte	0x2f35
	.4byte	.LLST105
	.4byte	.LVUS105
	.uleb128 0x34
	.4byte	0x2f40
	.4byte	.LLST106
	.4byte	.LVUS106
	.uleb128 0x34
	.4byte	0x2f4b
	.4byte	.LLST107
	.4byte	.LVUS107
	.uleb128 0x34
	.4byte	0x2f55
	.4byte	.LLST108
	.4byte	.LVUS108
	.uleb128 0x34
	.4byte	0x2f61
	.4byte	.LLST109
	.4byte	.LVUS109
	.uleb128 0x35
	.4byte	0x2f6d
	.uleb128 0x34
	.4byte	0x2f77
	.4byte	.LLST110
	.4byte	.LVUS110
	.uleb128 0x34
	.4byte	0x2f83
	.4byte	.LLST111
	.4byte	.LVUS111
	.uleb128 0x34
	.4byte	0x2f8d
	.4byte	.LLST112
	.4byte	.LVUS112
	.uleb128 0x34
	.4byte	0x2f97
	.4byte	.LLST113
	.4byte	.LVUS113
	.uleb128 0x34
	.4byte	0x2fa1
	.4byte	.LLST114
	.4byte	.LVUS114
	.uleb128 0x34
	.4byte	0x2fad
	.4byte	.LLST115
	.4byte	.LVUS115
	.uleb128 0x34
	.4byte	0x2fb9
	.4byte	.LLST116
	.4byte	.LVUS116
	.uleb128 0x34
	.4byte	0x2fc5
	.4byte	.LLST117
	.4byte	.LVUS117
	.uleb128 0x34
	.4byte	0x2fd1
	.4byte	.LLST118
	.4byte	.LVUS118
	.uleb128 0x2c
	.4byte	.LVL247
	.4byte	0x35f1
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC5
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC4
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x2
	.byte	0x8
	.byte	0xaa
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x5
	.byte	0x3
	.4byte	.LANCHOR0+24
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x22
	.4byte	.LASF568
	.byte	0x1
	.2byte	0x199
	.byte	0x6
	.4byte	.LFB72
	.4byte	.LFE72-.LFB72
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x2699
	.uleb128 0x28
	.ascii	"fn\000"
	.byte	0x1
	.2byte	0x199
	.byte	0x2c
	.4byte	0xfd
	.4byte	.LLST96
	.4byte	.LVUS96
	.uleb128 0x36
	.4byte	.LVL215
	.4byte	0x29c7
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x3
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0
	.byte	0
	.uleb128 0x22
	.4byte	.LASF569
	.byte	0x1
	.2byte	0x191
	.byte	0x6
	.4byte	.LFB71
	.4byte	.LFE71-.LFB71
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x2803
	.uleb128 0x31
	.4byte	0x2ed7
	.4byte	.LBI25
	.byte	.LVU655
	.4byte	.Ldebug_ranges0+0x48
	.byte	0x1
	.2byte	0x193
	.byte	0x5
	.uleb128 0x32
	.4byte	0x2ee4
	.4byte	.LLST76
	.4byte	.LVUS76
	.uleb128 0x33
	.4byte	.Ldebug_ranges0+0x48
	.uleb128 0x34
	.4byte	0x2ef0
	.4byte	.LLST77
	.4byte	.LVUS77
	.uleb128 0x34
	.4byte	0x2efb
	.4byte	.LLST78
	.4byte	.LVUS78
	.uleb128 0x35
	.4byte	0x2f06
	.uleb128 0x34
	.4byte	0x2f12
	.4byte	.LLST79
	.4byte	.LVUS79
	.uleb128 0x34
	.4byte	0x2f1e
	.4byte	.LLST80
	.4byte	.LVUS80
	.uleb128 0x34
	.4byte	0x2f2a
	.4byte	.LLST81
	.4byte	.LVUS81
	.uleb128 0x34
	.4byte	0x2f35
	.4byte	.LLST82
	.4byte	.LVUS82
	.uleb128 0x34
	.4byte	0x2f40
	.4byte	.LLST83
	.4byte	.LVUS83
	.uleb128 0x34
	.4byte	0x2f4b
	.4byte	.LLST84
	.4byte	.LVUS84
	.uleb128 0x34
	.4byte	0x2f55
	.4byte	.LLST85
	.4byte	.LVUS85
	.uleb128 0x34
	.4byte	0x2f61
	.4byte	.LLST86
	.4byte	.LVUS86
	.uleb128 0x35
	.4byte	0x2f6d
	.uleb128 0x34
	.4byte	0x2f77
	.4byte	.LLST87
	.4byte	.LVUS87
	.uleb128 0x34
	.4byte	0x2f83
	.4byte	.LLST88
	.4byte	.LVUS88
	.uleb128 0x34
	.4byte	0x2f8d
	.4byte	.LLST89
	.4byte	.LVUS89
	.uleb128 0x34
	.4byte	0x2f97
	.4byte	.LLST90
	.4byte	.LVUS90
	.uleb128 0x34
	.4byte	0x2fa1
	.4byte	.LLST91
	.4byte	.LVUS91
	.uleb128 0x34
	.4byte	0x2fad
	.4byte	.LLST92
	.4byte	.LVUS92
	.uleb128 0x34
	.4byte	0x2fb9
	.4byte	.LLST93
	.4byte	.LVUS93
	.uleb128 0x34
	.4byte	0x2fc5
	.4byte	.LLST94
	.4byte	.LVUS94
	.uleb128 0x34
	.4byte	0x2fd1
	.4byte	.LLST95
	.4byte	.LVUS95
	.uleb128 0x2c
	.4byte	.LVL212
	.4byte	0x35f1
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC5
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC4
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x2
	.byte	0x8
	.byte	0xaa
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x5
	.byte	0x3
	.4byte	.LANCHOR0+24
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x37
	.4byte	.LASF570
	.byte	0x1
	.2byte	0x181
	.byte	0x6
	.4byte	.LFB70
	.4byte	.LFE70-.LFB70
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x286e
	.uleb128 0x25
	.ascii	"yy\000"
	.byte	0x1
	.2byte	0x183
	.byte	0xe
	.4byte	0x404
	.4byte	.LLST75
	.4byte	.LVUS75
	.uleb128 0x2a
	.4byte	.LVL177
	.4byte	0x35fd
	.4byte	0x2841
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x1
	.byte	0x30
	.byte	0
	.uleb128 0x2a
	.4byte	.LVL179
	.4byte	0x35a7
	.4byte	0x2864
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x2
	.byte	0x77
	.sleb128 0
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x2
	.byte	0x74
	.sleb128 0
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x5
	.byte	0x74
	.sleb128 0
	.byte	0x76
	.sleb128 0
	.byte	0x1e
	.byte	0
	.uleb128 0x23
	.4byte	.LVL180
	.4byte	0x31cd
	.byte	0
	.uleb128 0x22
	.4byte	.LASF571
	.byte	0x1
	.2byte	0x172
	.byte	0x6
	.4byte	.LFB69
	.4byte	.LFE69-.LFB69
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x29c7
	.uleb128 0x24
	.4byte	.LASF572
	.byte	0x1
	.2byte	0x172
	.byte	0x1c
	.4byte	0x33
	.4byte	.LLST66
	.4byte	.LVUS66
	.uleb128 0x24
	.4byte	.LASF573
	.byte	0x1
	.2byte	0x172
	.byte	0x2b
	.4byte	0x33
	.4byte	.LLST67
	.4byte	.LVUS67
	.uleb128 0x24
	.4byte	.LASF574
	.byte	0x1
	.2byte	0x172
	.byte	0x37
	.4byte	0x33
	.4byte	.LLST68
	.4byte	.LVUS68
	.uleb128 0x24
	.4byte	.LASF575
	.byte	0x1
	.2byte	0x172
	.byte	0x49
	.4byte	0x33
	.4byte	.LLST69
	.4byte	.LVUS69
	.uleb128 0x29
	.4byte	0x330e
	.4byte	.LBI10
	.byte	.LVU599
	.4byte	.Ldebug_ranges0+0
	.byte	0x1
	.2byte	0x174
	.byte	0x5
	.4byte	0x293a
	.uleb128 0x33
	.4byte	.Ldebug_ranges0+0x8
	.uleb128 0x34
	.4byte	0x331b
	.4byte	.LLST70
	.4byte	.LVUS70
	.uleb128 0x35
	.4byte	0x3325
	.uleb128 0x2c
	.4byte	.LVL170
	.4byte	0x3608
	.uleb128 0x2b
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
	.uleb128 0x33
	.byte	0xf7
	.uleb128 0x25
	.byte	0xf5
	.uleb128 0x54
	.uleb128 0x25
	.byte	0x1b
	.byte	0xf7
	.uleb128 0x2c
	.uleb128 0x2b
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
	.uleb128 0x2c
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x29
	.4byte	0x2b05
	.4byte	.LBI15
	.byte	.LVU617
	.4byte	.Ldebug_ranges0+0x20
	.byte	0x1
	.2byte	0x177
	.byte	0x5
	.4byte	0x2985
	.uleb128 0x32
	.4byte	0x2b35
	.4byte	.LLST71
	.4byte	.LVUS71
	.uleb128 0x32
	.4byte	0x2b2a
	.4byte	.LLST72
	.4byte	.LVUS72
	.uleb128 0x32
	.4byte	0x2b1f
	.4byte	.LLST73
	.4byte	.LVUS73
	.uleb128 0x32
	.4byte	0x2b13
	.4byte	.LLST74
	.4byte	.LVUS74
	.byte	0
	.uleb128 0x2a
	.4byte	.LVL174
	.4byte	0x2c8a
	.4byte	0x29aa
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x1
	.byte	0x30
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x2
	.byte	0x76
	.sleb128 0
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x2
	.byte	0x77
	.sleb128 0
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x2
	.byte	0x76
	.sleb128 0
	.byte	0
	.uleb128 0x36
	.4byte	.LVL176
	.4byte	0x35a7
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC1
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC0
	.byte	0
	.byte	0
	.uleb128 0x22
	.4byte	.LASF576
	.byte	0x1
	.2byte	0x157
	.byte	0x6
	.4byte	.LFB68
	.4byte	.LFE68-.LFB68
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x2b05
	.uleb128 0x24
	.4byte	.LASF577
	.byte	0x1
	.2byte	0x157
	.byte	0x27
	.4byte	0x2131
	.4byte	.LLST60
	.4byte	.LVUS60
	.uleb128 0x28
	.ascii	"fn\000"
	.byte	0x1
	.2byte	0x157
	.byte	0x35
	.4byte	0xfd
	.4byte	.LLST61
	.4byte	.LVUS61
	.uleb128 0x25
	.ascii	"fp\000"
	.byte	0x1
	.2byte	0x159
	.byte	0xb
	.4byte	0x31a
	.4byte	.LLST62
	.4byte	.LVUS62
	.uleb128 0x26
	.4byte	.LASF578
	.byte	0x1
	.2byte	0x15a
	.byte	0xe
	.4byte	0x404
	.4byte	.LLST63
	.4byte	.LVUS63
	.uleb128 0x25
	.ascii	"xx\000"
	.byte	0x1
	.2byte	0x15b
	.byte	0x9
	.4byte	0x33
	.4byte	.LLST64
	.4byte	.LVUS64
	.uleb128 0x25
	.ascii	"yy\000"
	.byte	0x1
	.2byte	0x15b
	.byte	0xd
	.4byte	0x33
	.4byte	.LLST65
	.4byte	.LVUS65
	.uleb128 0x2a
	.4byte	.LVL150
	.4byte	0x3614
	.4byte	0x2a76
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x3
	.byte	0xf3
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC14
	.byte	0
	.uleb128 0x2a
	.4byte	.LVL153
	.4byte	0x3621
	.4byte	0x2a9d
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC15
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x1
	.byte	0x31
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x1
	.byte	0x33
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x2
	.byte	0x77
	.sleb128 0
	.byte	0
	.uleb128 0x2a
	.4byte	.LVL154
	.4byte	0x362c
	.4byte	0x2aba
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x2
	.byte	0x77
	.sleb128 0
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC16
	.byte	0
	.uleb128 0x2a
	.4byte	.LVL155
	.4byte	0x3621
	.4byte	0x2ae1
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC17
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x1
	.byte	0x31
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x1
	.byte	0x34
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x2
	.byte	0x77
	.sleb128 0
	.byte	0
	.uleb128 0x2a
	.4byte	.LVL161
	.4byte	0x362c
	.4byte	0x2afb
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x2
	.byte	0x77
	.sleb128 0
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x2
	.byte	0x79
	.sleb128 0
	.byte	0
	.uleb128 0x38
	.4byte	.LVL166
	.4byte	0x3639
	.byte	0
	.uleb128 0x39
	.4byte	.LASF590
	.byte	0x1
	.2byte	0x148
	.byte	0x6
	.byte	0x1
	.4byte	0x2b41
	.uleb128 0x3a
	.ascii	"ch\000"
	.byte	0x1
	.2byte	0x148
	.byte	0x25
	.4byte	0x33
	.uleb128 0x3a
	.ascii	"r\000"
	.byte	0x1
	.2byte	0x148
	.byte	0x2d
	.4byte	0x33
	.uleb128 0x3a
	.ascii	"g\000"
	.byte	0x1
	.2byte	0x148
	.byte	0x34
	.4byte	0x33
	.uleb128 0x3a
	.ascii	"b\000"
	.byte	0x1
	.2byte	0x148
	.byte	0x3b
	.4byte	0x33
	.byte	0
	.uleb128 0x22
	.4byte	.LASF579
	.byte	0x1
	.2byte	0x13f
	.byte	0x6
	.4byte	.LFB66
	.4byte	.LFE66-.LFB66
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x2b7d
	.uleb128 0x24
	.4byte	.LASF528
	.byte	0x1
	.2byte	0x13f
	.byte	0x24
	.4byte	0x404
	.4byte	.LLST58
	.4byte	.LVUS58
	.uleb128 0x36
	.4byte	.LVL144
	.4byte	0x35fd
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x1
	.byte	0x30
	.byte	0
	.byte	0
	.uleb128 0x22
	.4byte	.LASF580
	.byte	0x1
	.2byte	0x136
	.byte	0x6
	.4byte	.LFB65
	.4byte	.LFE65-.LFB65
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x2be5
	.uleb128 0x24
	.4byte	.LASF581
	.byte	0x1
	.2byte	0x136
	.byte	0x2c
	.4byte	0x404
	.4byte	.LLST57
	.4byte	.LVUS57
	.uleb128 0x3b
	.4byte	.LASF583
	.4byte	0x2bf5
	.uleb128 0x5
	.byte	0x3
	.4byte	__PRETTY_FUNCTION__.17083
	.uleb128 0x2c
	.4byte	.LVL141
	.4byte	0x35f1
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC13
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC4
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x3
	.byte	0xa
	.2byte	0x138
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x5
	.byte	0x3
	.4byte	.LANCHOR0+104
	.byte	0
	.byte	0
	.uleb128 0xc
	.4byte	0x10a
	.4byte	0x2bf5
	.uleb128 0xd
	.4byte	0x3a
	.byte	0x1c
	.byte	0
	.uleb128 0x5
	.4byte	0x2be5
	.uleb128 0x3c
	.4byte	.LASF636
	.byte	0x1
	.2byte	0x12d
	.byte	0x6
	.4byte	.LFB64
	.4byte	.LFE64-.LFB64
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x22
	.4byte	.LASF582
	.byte	0x1
	.2byte	0x123
	.byte	0x6
	.4byte	.LFB63
	.4byte	.LFE63-.LFB63
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x2c75
	.uleb128 0x24
	.4byte	.LASF533
	.byte	0x1
	.2byte	0x123
	.byte	0x28
	.4byte	0x212b
	.4byte	.LLST56
	.4byte	.LVUS56
	.uleb128 0x3b
	.4byte	.LASF583
	.4byte	0x2c85
	.uleb128 0x5
	.byte	0x3
	.4byte	__PRETTY_FUNCTION__.17077
	.uleb128 0x2c
	.4byte	.LVL138
	.4byte	0x35f1
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC12
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC4
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x3
	.byte	0xa
	.2byte	0x125
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x5
	.byte	0x3
	.4byte	.LANCHOR0+76
	.byte	0
	.byte	0
	.uleb128 0xc
	.4byte	0x10a
	.4byte	0x2c85
	.uleb128 0xd
	.4byte	0x3a
	.byte	0x18
	.byte	0
	.uleb128 0x5
	.4byte	0x2c75
	.uleb128 0x3d
	.4byte	.LASF584
	.byte	0x1
	.byte	0xd8
	.byte	0x6
	.4byte	.LFB62
	.4byte	.LFE62-.LFB62
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x2ec2
	.uleb128 0x3e
	.4byte	.LASF585
	.byte	0x1
	.byte	0xd8
	.byte	0x24
	.4byte	0x404
	.4byte	.LLST49
	.4byte	.LVUS49
	.uleb128 0x3e
	.4byte	.LASF586
	.byte	0x1
	.byte	0xd8
	.byte	0x3a
	.4byte	0x404
	.4byte	.LLST50
	.4byte	.LVUS50
	.uleb128 0x3e
	.4byte	.LASF539
	.byte	0x1
	.byte	0xd8
	.byte	0x4e
	.4byte	0x404
	.4byte	.LLST51
	.4byte	.LVUS51
	.uleb128 0x3e
	.4byte	.LASF537
	.byte	0x1
	.byte	0xd8
	.byte	0x62
	.4byte	0x404
	.4byte	.LLST52
	.4byte	.LVUS52
	.uleb128 0x3f
	.4byte	.LASF545
	.byte	0x1
	.byte	0xd8
	.byte	0x78
	.4byte	0x404
	.uleb128 0x2
	.byte	0x91
	.sleb128 0
	.uleb128 0x3f
	.4byte	.LASF546
	.byte	0x1
	.byte	0xd8
	.byte	0x8f
	.4byte	0x404
	.uleb128 0x2
	.byte	0x91
	.sleb128 4
	.uleb128 0x3f
	.4byte	.LASF587
	.byte	0x1
	.byte	0xd8
	.byte	0xa7
	.4byte	0x404
	.uleb128 0x2
	.byte	0x91
	.sleb128 8
	.uleb128 0x40
	.4byte	.LASF588
	.byte	0x1
	.byte	0xda
	.byte	0xe
	.4byte	0x404
	.4byte	.LLST53
	.4byte	.LVUS53
	.uleb128 0x41
	.ascii	"xx\000"
	.byte	0x1
	.byte	0xda
	.byte	0x16
	.4byte	0x404
	.4byte	.LLST54
	.4byte	.LVUS54
	.uleb128 0x40
	.4byte	.LASF589
	.byte	0x1
	.byte	0xdb
	.byte	0xb
	.4byte	0x25
	.4byte	.LLST55
	.4byte	.LVUS55
	.uleb128 0x3b
	.4byte	.LASF583
	.4byte	0x2ed2
	.uleb128 0x5
	.byte	0x3
	.4byte	__PRETTY_FUNCTION__.17070
	.uleb128 0x2a
	.4byte	.LVL119
	.4byte	0x35a7
	.4byte	0x2dad
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC6
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x2
	.byte	0x75
	.sleb128 0
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x2
	.byte	0x79
	.sleb128 0
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x2
	.byte	0x78
	.sleb128 0
	.uleb128 0x2b
	.uleb128 0x2
	.byte	0x7d
	.sleb128 0
	.uleb128 0x2
	.byte	0x7a
	.sleb128 0
	.uleb128 0x2b
	.uleb128 0x2
	.byte	0x7d
	.sleb128 4
	.uleb128 0x2
	.byte	0x77
	.sleb128 0
	.uleb128 0x2b
	.uleb128 0x2
	.byte	0x7d
	.sleb128 8
	.uleb128 0x2
	.byte	0x76
	.sleb128 0
	.uleb128 0x2b
	.uleb128 0x2
	.byte	0x7d
	.sleb128 12
	.uleb128 0x3
	.byte	0x91
	.sleb128 0
	.byte	0x6
	.byte	0
	.uleb128 0x2a
	.4byte	.LVL121
	.4byte	0x35a7
	.4byte	0x2dcc
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC9
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x4
	.byte	0x77
	.sleb128 0
	.byte	0x38
	.byte	0x24
	.byte	0
	.uleb128 0x23
	.4byte	.LVL122
	.4byte	0x358d
	.uleb128 0x2a
	.4byte	.LVL123
	.4byte	0x35b4
	.4byte	0x2de8
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x1
	.byte	0x31
	.byte	0
	.uleb128 0x2a
	.4byte	.LVL126
	.4byte	0x3645
	.4byte	0x2dfe
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x4
	.byte	0x75
	.sleb128 0
	.byte	0x31
	.byte	0x24
	.byte	0
	.uleb128 0x23
	.4byte	.LVL129
	.4byte	0x3645
	.uleb128 0x2a
	.4byte	.LVL132
	.4byte	0x35f1
	.4byte	0x2e36
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC8
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC4
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x2
	.byte	0x8
	.byte	0xe3
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x5
	.byte	0x3
	.4byte	.LANCHOR0+52
	.byte	0
	.uleb128 0x2a
	.4byte	.LVL133
	.4byte	0x35f1
	.4byte	0x2e66
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC10
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC4
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x3
	.byte	0xa
	.2byte	0x10c
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x5
	.byte	0x3
	.4byte	.LANCHOR0+52
	.byte	0
	.uleb128 0x2a
	.4byte	.LVL134
	.4byte	0x35f1
	.4byte	0x2e96
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC11
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC4
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x3
	.byte	0xa
	.2byte	0x114
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x5
	.byte	0x3
	.4byte	.LANCHOR0+52
	.byte	0
	.uleb128 0x2c
	.4byte	.LVL135
	.4byte	0x35f1
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC7
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC4
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x2
	.byte	0x8
	.byte	0xe0
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x5
	.byte	0x3
	.4byte	.LANCHOR0+52
	.byte	0
	.byte	0
	.uleb128 0xc
	.4byte	0x10a
	.4byte	0x2ed2
	.uleb128 0xd
	.4byte	0x3a
	.byte	0x14
	.byte	0
	.uleb128 0x5
	.4byte	0x2ec2
	.uleb128 0x42
	.4byte	.LASF591
	.byte	0x1
	.byte	0xa1
	.byte	0x6
	.byte	0x1
	.4byte	0x2fed
	.uleb128 0x43
	.4byte	.LASF592
	.byte	0x1
	.byte	0xa1
	.byte	0x2b
	.4byte	0x2131
	.uleb128 0x44
	.ascii	"xx\000"
	.byte	0x1
	.byte	0xa3
	.byte	0xe
	.4byte	0x404
	.uleb128 0x44
	.ascii	"yy\000"
	.byte	0x1
	.byte	0xa3
	.byte	0x12
	.4byte	0x404
	.uleb128 0x45
	.4byte	.LASF593
	.byte	0x1
	.byte	0xa3
	.byte	0x16
	.4byte	0x404
	.uleb128 0x45
	.4byte	.LASF594
	.byte	0x1
	.byte	0xa3
	.byte	0x1c
	.4byte	0x404
	.uleb128 0x45
	.4byte	.LASF595
	.byte	0x1
	.byte	0xa3
	.byte	0x22
	.4byte	0x404
	.uleb128 0x44
	.ascii	"rr\000"
	.byte	0x1
	.byte	0xa4
	.byte	0x9
	.4byte	0x33
	.uleb128 0x44
	.ascii	"gg\000"
	.byte	0x1
	.byte	0xa4
	.byte	0xd
	.4byte	0x33
	.uleb128 0x44
	.ascii	"bb\000"
	.byte	0x1
	.byte	0xa4
	.byte	0x11
	.4byte	0x33
	.uleb128 0x44
	.ascii	"n\000"
	.byte	0x1
	.byte	0xa4
	.byte	0x15
	.4byte	0x33
	.uleb128 0x45
	.4byte	.LASF596
	.byte	0x1
	.byte	0xa4
	.byte	0x18
	.4byte	0x33
	.uleb128 0x45
	.4byte	.LASF597
	.byte	0x1
	.byte	0xa4
	.byte	0x1e
	.4byte	0x33
	.uleb128 0x44
	.ascii	"w\000"
	.byte	0x1
	.byte	0xa4
	.byte	0x24
	.4byte	0x33
	.uleb128 0x45
	.4byte	.LASF536
	.byte	0x1
	.byte	0xa4
	.byte	0x27
	.4byte	0x33
	.uleb128 0x44
	.ascii	"r\000"
	.byte	0x1
	.byte	0xa5
	.byte	0xd
	.4byte	0x3ec
	.uleb128 0x44
	.ascii	"g\000"
	.byte	0x1
	.byte	0xa5
	.byte	0x10
	.4byte	0x3ec
	.uleb128 0x44
	.ascii	"b\000"
	.byte	0x1
	.byte	0xa5
	.byte	0x13
	.4byte	0x3ec
	.uleb128 0x45
	.4byte	.LASF598
	.byte	0x1
	.byte	0xa5
	.byte	0x16
	.4byte	0x3ec
	.uleb128 0x44
	.ascii	"row\000"
	.byte	0x1
	.byte	0xa5
	.byte	0x1d
	.4byte	0x3ec
	.uleb128 0x45
	.4byte	.LASF599
	.byte	0x1
	.byte	0xa6
	.byte	0xf
	.4byte	0x2131
	.uleb128 0x45
	.4byte	.LASF600
	.byte	0x1
	.byte	0xa7
	.byte	0xf
	.4byte	0x2131
	.uleb128 0x45
	.4byte	.LASF601
	.byte	0x1
	.byte	0xa8
	.byte	0xe
	.4byte	0x404
	.uleb128 0x3b
	.4byte	.LASF583
	.4byte	0x2ffd
	.uleb128 0x5
	.byte	0x3
	.4byte	__PRETTY_FUNCTION__.17048
	.byte	0
	.uleb128 0xc
	.4byte	0x10a
	.4byte	0x2ffd
	.uleb128 0xd
	.4byte	0x3a
	.byte	0x1a
	.byte	0
	.uleb128 0x5
	.4byte	0x2fed
	.uleb128 0x3d
	.4byte	.LASF602
	.byte	0x1
	.byte	0x6b
	.byte	0x6
	.4byte	.LFB60
	.4byte	.LFE60-.LFB60
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x31b8
	.uleb128 0x3e
	.4byte	.LASF592
	.byte	0x1
	.byte	0x6b
	.byte	0x28
	.4byte	0x2131
	.4byte	.LLST11
	.4byte	.LVUS11
	.uleb128 0x41
	.ascii	"xx\000"
	.byte	0x1
	.byte	0x6d
	.byte	0xe
	.4byte	0x404
	.4byte	.LLST12
	.4byte	.LVUS12
	.uleb128 0x41
	.ascii	"yy\000"
	.byte	0x1
	.byte	0x6d
	.byte	0x12
	.4byte	0x404
	.4byte	.LLST13
	.4byte	.LVUS13
	.uleb128 0x40
	.4byte	.LASF594
	.byte	0x1
	.byte	0x6d
	.byte	0x16
	.4byte	0x404
	.4byte	.LLST14
	.4byte	.LVUS14
	.uleb128 0x40
	.4byte	.LASF595
	.byte	0x1
	.byte	0x6d
	.byte	0x1c
	.4byte	0x404
	.4byte	.LLST15
	.4byte	.LVUS15
	.uleb128 0x41
	.ascii	"rr\000"
	.byte	0x1
	.byte	0x6e
	.byte	0x9
	.4byte	0x33
	.4byte	.LLST16
	.4byte	.LVUS16
	.uleb128 0x41
	.ascii	"gg\000"
	.byte	0x1
	.byte	0x6e
	.byte	0xd
	.4byte	0x33
	.4byte	.LLST17
	.4byte	.LVUS17
	.uleb128 0x41
	.ascii	"bb\000"
	.byte	0x1
	.byte	0x6e
	.byte	0x11
	.4byte	0x33
	.4byte	.LLST18
	.4byte	.LVUS18
	.uleb128 0x41
	.ascii	"n\000"
	.byte	0x1
	.byte	0x6e
	.byte	0x15
	.4byte	0x33
	.4byte	.LLST19
	.4byte	.LVUS19
	.uleb128 0x40
	.4byte	.LASF596
	.byte	0x1
	.byte	0x6e
	.byte	0x18
	.4byte	0x33
	.4byte	.LLST20
	.4byte	.LVUS20
	.uleb128 0x40
	.4byte	.LASF597
	.byte	0x1
	.byte	0x6e
	.byte	0x1e
	.4byte	0x33
	.4byte	.LLST21
	.4byte	.LVUS21
	.uleb128 0x41
	.ascii	"i\000"
	.byte	0x1
	.byte	0x6e
	.byte	0x24
	.4byte	0x33
	.4byte	.LLST22
	.4byte	.LVUS22
	.uleb128 0x41
	.ascii	"r\000"
	.byte	0x1
	.byte	0x6f
	.byte	0xd
	.4byte	0x3ec
	.4byte	.LLST23
	.4byte	.LVUS23
	.uleb128 0x41
	.ascii	"g\000"
	.byte	0x1
	.byte	0x6f
	.byte	0x10
	.4byte	0x3ec
	.4byte	.LLST24
	.4byte	.LVUS24
	.uleb128 0x41
	.ascii	"b\000"
	.byte	0x1
	.byte	0x6f
	.byte	0x13
	.4byte	0x3ec
	.4byte	.LLST25
	.4byte	.LVUS25
	.uleb128 0x40
	.4byte	.LASF598
	.byte	0x1
	.byte	0x6f
	.byte	0x16
	.4byte	0x3ec
	.4byte	.LLST26
	.4byte	.LVUS26
	.uleb128 0x40
	.4byte	.LASF599
	.byte	0x1
	.byte	0x70
	.byte	0xf
	.4byte	0x2131
	.4byte	.LLST27
	.4byte	.LVUS27
	.uleb128 0x45
	.4byte	.LASF600
	.byte	0x1
	.byte	0x71
	.byte	0xf
	.4byte	0x2131
	.uleb128 0x40
	.4byte	.LASF601
	.byte	0x1
	.byte	0x72
	.byte	0xe
	.4byte	0x404
	.4byte	.LLST28
	.4byte	.LVUS28
	.uleb128 0x3b
	.4byte	.LASF583
	.4byte	0x31c8
	.uleb128 0x5
	.byte	0x3
	.4byte	__PRETTY_FUNCTION__.17017
	.uleb128 0x2c
	.4byte	.LVL80
	.4byte	0x35f1
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC5
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC4
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x2
	.byte	0x8
	.byte	0x74
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x5
	.byte	0x3
	.4byte	.LANCHOR0
	.byte	0
	.byte	0
	.uleb128 0xc
	.4byte	0x10a
	.4byte	0x31c8
	.uleb128 0xd
	.4byte	0x3a
	.byte	0x17
	.byte	0
	.uleb128 0x5
	.4byte	0x31b8
	.uleb128 0x3d
	.4byte	.LASF603
	.byte	0x1
	.byte	0x48
	.byte	0x6
	.4byte	.LFB59
	.4byte	.LFE59-.LFB59
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x32db
	.uleb128 0x3e
	.4byte	.LASF604
	.byte	0x1
	.byte	0x48
	.byte	0x30
	.4byte	0x404
	.4byte	.LLST1
	.4byte	.LVUS1
	.uleb128 0x3e
	.4byte	.LASF605
	.byte	0x1
	.byte	0x48
	.byte	0x42
	.4byte	0x404
	.4byte	.LLST2
	.4byte	.LVUS2
	.uleb128 0x41
	.ascii	"yy\000"
	.byte	0x1
	.byte	0x4a
	.byte	0x9
	.4byte	0x33
	.4byte	.LLST3
	.4byte	.LVUS3
	.uleb128 0x41
	.ascii	"ys\000"
	.byte	0x1
	.byte	0x4a
	.byte	0xd
	.4byte	0x33
	.4byte	.LLST4
	.4byte	.LVUS4
	.uleb128 0x41
	.ascii	"w\000"
	.byte	0x1
	.byte	0x4a
	.byte	0x11
	.4byte	0x33
	.4byte	.LLST5
	.4byte	.LVUS5
	.uleb128 0x40
	.4byte	.LASF606
	.byte	0x1
	.byte	0x4a
	.byte	0x14
	.4byte	0x33
	.4byte	.LLST6
	.4byte	.LVUS6
	.uleb128 0x45
	.4byte	.LASF598
	.byte	0x1
	.byte	0x4b
	.byte	0xe
	.4byte	0x404
	.uleb128 0x40
	.4byte	.LASF594
	.byte	0x1
	.byte	0x4b
	.byte	0x15
	.4byte	0x404
	.4byte	.LLST7
	.4byte	.LVUS7
	.uleb128 0x40
	.4byte	.LASF607
	.byte	0x1
	.byte	0x4c
	.byte	0xe
	.4byte	0x212b
	.4byte	.LLST8
	.4byte	.LVUS8
	.uleb128 0x40
	.4byte	.LASF608
	.byte	0x1
	.byte	0x4d
	.byte	0xe
	.4byte	0x212b
	.4byte	.LLST9
	.4byte	.LVUS9
	.uleb128 0x40
	.4byte	.LASF609
	.byte	0x1
	.byte	0x4e
	.byte	0xe
	.4byte	0x212b
	.4byte	.LLST10
	.4byte	.LVUS10
	.uleb128 0x2a
	.4byte	.LVL9
	.4byte	0x35a7
	.4byte	0x32ca
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC2
	.byte	0
	.uleb128 0x2c
	.4byte	.LVL13
	.4byte	0x35a7
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x2
	.byte	0x78
	.sleb128 0
	.byte	0
	.byte	0
	.uleb128 0x3d
	.4byte	.LASF610
	.byte	0x1
	.byte	0x39
	.byte	0x6
	.4byte	.LFB58
	.4byte	.LFE58-.LFB58
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x330e
	.uleb128 0x36
	.4byte	.LVL5
	.4byte	0x35a7
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC1
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC0
	.byte	0
	.byte	0
	.uleb128 0x46
	.4byte	.LASF637
	.byte	0x1
	.byte	0x2c
	.byte	0x6
	.byte	0x1
	.4byte	0x3332
	.uleb128 0x44
	.ascii	"i\000"
	.byte	0x1
	.byte	0x2e
	.byte	0x9
	.4byte	0x33
	.uleb128 0x45
	.4byte	.LASF611
	.byte	0x1
	.byte	0x2f
	.byte	0xb
	.4byte	0x25
	.byte	0
	.uleb128 0x47
	.4byte	.LASF638
	.byte	0x2
	.2byte	0x1c8
	.byte	0x14
	.byte	0x3
	.4byte	0x334d
	.uleb128 0x3a
	.ascii	"op\000"
	.byte	0x2
	.2byte	0x1c8
	.byte	0x29
	.4byte	0x86e
	.byte	0
	.uleb128 0x48
	.4byte	0x330e
	.4byte	.LFB57
	.4byte	.LFE57-.LFB57
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x33aa
	.uleb128 0x34
	.4byte	0x331b
	.4byte	.LLST0
	.4byte	.LVUS0
	.uleb128 0x49
	.4byte	0x3325
	.byte	0x4
	.4byte	0x3f666666
	.uleb128 0x2c
	.4byte	.LVL2
	.4byte	0x3608
	.uleb128 0x2b
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
	.uleb128 0x33
	.byte	0xf7
	.uleb128 0x25
	.byte	0xf5
	.uleb128 0x54
	.uleb128 0x25
	.byte	0x1b
	.byte	0xf7
	.uleb128 0x2c
	.uleb128 0x2b
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
	.uleb128 0x2c
	.byte	0
	.byte	0
	.uleb128 0x48
	.4byte	0x2ed7
	.4byte	.LFB61
	.4byte	.LFE61-.LFB61
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x34f7
	.uleb128 0x32
	.4byte	0x2ee4
	.4byte	.LLST29
	.4byte	.LVUS29
	.uleb128 0x34
	.4byte	0x2ef0
	.4byte	.LLST30
	.4byte	.LVUS30
	.uleb128 0x34
	.4byte	0x2efb
	.4byte	.LLST31
	.4byte	.LVUS31
	.uleb128 0x35
	.4byte	0x2f06
	.uleb128 0x34
	.4byte	0x2f12
	.4byte	.LLST32
	.4byte	.LVUS32
	.uleb128 0x34
	.4byte	0x2f1e
	.4byte	.LLST33
	.4byte	.LVUS33
	.uleb128 0x34
	.4byte	0x2f2a
	.4byte	.LLST34
	.4byte	.LVUS34
	.uleb128 0x34
	.4byte	0x2f35
	.4byte	.LLST35
	.4byte	.LVUS35
	.uleb128 0x34
	.4byte	0x2f40
	.4byte	.LLST36
	.4byte	.LVUS36
	.uleb128 0x34
	.4byte	0x2f4b
	.4byte	.LLST37
	.4byte	.LVUS37
	.uleb128 0x34
	.4byte	0x2f55
	.4byte	.LLST38
	.4byte	.LVUS38
	.uleb128 0x34
	.4byte	0x2f61
	.4byte	.LLST39
	.4byte	.LVUS39
	.uleb128 0x34
	.4byte	0x2f6d
	.4byte	.LLST40
	.4byte	.LVUS40
	.uleb128 0x34
	.4byte	0x2f77
	.4byte	.LLST41
	.4byte	.LVUS41
	.uleb128 0x34
	.4byte	0x2f83
	.4byte	.LLST42
	.4byte	.LVUS42
	.uleb128 0x34
	.4byte	0x2f8d
	.4byte	.LLST43
	.4byte	.LVUS43
	.uleb128 0x34
	.4byte	0x2f97
	.4byte	.LLST44
	.4byte	.LVUS44
	.uleb128 0x34
	.4byte	0x2fa1
	.4byte	.LLST45
	.4byte	.LVUS45
	.uleb128 0x34
	.4byte	0x2fad
	.4byte	.LLST46
	.4byte	.LVUS46
	.uleb128 0x34
	.4byte	0x2fb9
	.4byte	.LLST47
	.4byte	.LVUS47
	.uleb128 0x35
	.4byte	0x2fc5
	.uleb128 0x34
	.4byte	0x2fd1
	.4byte	.LLST48
	.4byte	.LVUS48
	.uleb128 0x2c
	.4byte	.LVL113
	.4byte	0x35f1
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC5
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC4
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x2
	.byte	0x8
	.byte	0xaa
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x53
	.uleb128 0x5
	.byte	0x3
	.4byte	.LANCHOR0+24
	.byte	0
	.byte	0
	.uleb128 0x48
	.4byte	0x2b05
	.4byte	.LFB67
	.4byte	.LFE67-.LFB67
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x352d
	.uleb128 0x32
	.4byte	0x2b13
	.4byte	.LLST59
	.4byte	.LVUS59
	.uleb128 0x4a
	.4byte	0x2b1f
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x4a
	.4byte	0x2b2a
	.uleb128 0x1
	.byte	0x52
	.uleb128 0x4a
	.4byte	0x2b35
	.uleb128 0x1
	.byte	0x53
	.byte	0
	.uleb128 0x48
	.4byte	0x24be
	.4byte	.LFB74
	.4byte	.LFE74-.LFB74
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x358d
	.uleb128 0x23
	.4byte	.LVL248
	.4byte	0x358d
	.uleb128 0x2a
	.4byte	.LVL249
	.4byte	0x35a7
	.4byte	0x3560
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC20
	.byte	0
	.uleb128 0x2a
	.4byte	.LVL250
	.4byte	0x35b4
	.4byte	0x3579
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x2
	.byte	0x75
	.sleb128 0
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x51
	.uleb128 0x1
	.byte	0x31
	.byte	0
	.uleb128 0x36
	.4byte	.LVL251
	.4byte	0x35a7
	.uleb128 0x2b
	.uleb128 0x1
	.byte	0x50
	.uleb128 0x5
	.byte	0x3
	.4byte	.LC21
	.byte	0
	.byte	0
	.uleb128 0x4b
	.4byte	.LASF612
	.4byte	.LASF612
	.byte	0x45
	.2byte	0x233
	.byte	0xd
	.uleb128 0x4b
	.4byte	.LASF613
	.4byte	.LASF613
	.byte	0x45
	.2byte	0x1c5
	.byte	0xc
	.uleb128 0x4b
	.4byte	.LASF614
	.4byte	.LASF614
	.byte	0x7
	.2byte	0x14c
	.byte	0xc
	.uleb128 0x4b
	.4byte	.LASF615
	.4byte	.LASF615
	.byte	0x45
	.2byte	0x21d
	.byte	0xe
	.uleb128 0x4c
	.ascii	"sin\000"
	.ascii	"sin\000"
	.byte	0x49
	.byte	0x40
	.byte	0x1
	.uleb128 0x4d
	.4byte	.LASF619
	.4byte	.LASF621
	.byte	0x48
	.byte	0
	.uleb128 0x4e
	.4byte	.LASF616
	.4byte	.LASF616
	.byte	0x46
	.byte	0xcb
	.byte	0x11
	.uleb128 0x4b
	.4byte	.LASF617
	.4byte	.LASF617
	.byte	0x46
	.2byte	0x103
	.byte	0x12
	.uleb128 0x4e
	.4byte	.LASF618
	.4byte	.LASF618
	.byte	0x47
	.byte	0x45
	.byte	0xd
	.uleb128 0x4d
	.4byte	.LASF620
	.4byte	.LASF622
	.byte	0x48
	.byte	0
	.uleb128 0x4c
	.ascii	"pow\000"
	.ascii	"pow\000"
	.byte	0x49
	.byte	0x8c
	.byte	0x1
	.uleb128 0x4b
	.4byte	.LASF623
	.4byte	.LASF624
	.byte	0x7
	.2byte	0x101
	.byte	0xe
	.uleb128 0x4d
	.4byte	.LASF625
	.4byte	.LASF626
	.byte	0x48
	.byte	0
	.uleb128 0x4b
	.4byte	.LASF627
	.4byte	.LASF627
	.byte	0x7
	.2byte	0x146
	.byte	0xc
	.uleb128 0x4e
	.4byte	.LASF628
	.4byte	.LASF628
	.byte	0x7
	.byte	0xd5
	.byte	0xc
	.uleb128 0x4b
	.4byte	.LASF629
	.4byte	.LASF629
	.byte	0x45
	.2byte	0x21b
	.byte	0xe
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
	.uleb128 0x11
	.uleb128 0x37
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x12
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
	.uleb128 0x13
	.uleb128 0x28
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x1c
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x14
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
	.uleb128 0x15
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
	.uleb128 0x16
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
	.uleb128 0x17
	.uleb128 0x5
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x18
	.uleb128 0x15
	.byte	0x1
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x19
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
	.uleb128 0x1a
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
	.uleb128 0x1b
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
	.uleb128 0x1c
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
	.uleb128 0x1d
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
	.uleb128 0x1e
	.uleb128 0x15
	.byte	0
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x1f
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
	.uleb128 0x20
	.uleb128 0x13
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3c
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x21
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
	.uleb128 0x22
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
	.uleb128 0x23
	.uleb128 0x4109
	.byte	0
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x31
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x24
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
	.uleb128 0x25
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
	.uleb128 0x26
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
	.uleb128 0x27
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
	.uleb128 0x28
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
	.uleb128 0x29
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
	.uleb128 0x2a
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
	.uleb128 0x2b
	.uleb128 0x410a
	.byte	0
	.uleb128 0x2
	.uleb128 0x18
	.uleb128 0x2111
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x2c
	.uleb128 0x4109
	.byte	0x1
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x31
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x2d
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
	.uleb128 0x2e
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
	.uleb128 0x2f
	.uleb128 0x5
	.byte	0
	.uleb128 0x31
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x30
	.uleb128 0x2e
	.byte	0
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
	.uleb128 0x20
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x31
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
	.byte	0
	.byte	0
	.uleb128 0x32
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
	.uleb128 0x33
	.uleb128 0xb
	.byte	0x1
	.uleb128 0x55
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x34
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
	.uleb128 0x35
	.uleb128 0x34
	.byte	0
	.uleb128 0x31
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x36
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
	.uleb128 0x37
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
	.uleb128 0x2116
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x38
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
	.uleb128 0x39
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
	.uleb128 0x3a
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
	.uleb128 0x3b
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
	.uleb128 0x3c
	.uleb128 0x2e
	.byte	0
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
	.byte	0
	.byte	0
	.uleb128 0x3d
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
	.uleb128 0x3e
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
	.uleb128 0x3f
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
	.uleb128 0x40
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
	.uleb128 0x41
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
	.uleb128 0x42
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
	.uleb128 0x43
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
	.byte	0
	.byte	0
	.uleb128 0x45
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
	.uleb128 0x46
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
	.uleb128 0x47
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
	.uleb128 0x48
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
	.uleb128 0x49
	.uleb128 0x34
	.byte	0
	.uleb128 0x31
	.uleb128 0x13
	.uleb128 0x1c
	.uleb128 0xa
	.byte	0
	.byte	0
	.uleb128 0x4a
	.uleb128 0x5
	.byte	0
	.uleb128 0x31
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x4b
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
	.uleb128 0x4c
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
	.uleb128 0x4d
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
	.uleb128 0x4e
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
	.byte	0
	.section	.debug_loc,"",%progbits
.Ldebug_loc0:
.LVUS129:
	.uleb128 0
	.uleb128 .LVU1002
	.uleb128 .LVU1002
	.uleb128 .LVU1057
	.uleb128 .LVU1057
	.uleb128 0
.LLST129:
	.4byte	.LVL291-.Ltext0
	.4byte	.LVL292-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x40
	.4byte	.LVL292-.Ltext0
	.4byte	.LVL308-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x53
	.4byte	.LVL308-.Ltext0
	.4byte	.LFE77-.Ltext0
	.2byte	0x6
	.byte	0xf3
	.uleb128 0x3
	.byte	0xf5
	.uleb128 0x40
	.uleb128 0x25
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS130:
	.uleb128 .LVU1049
	.uleb128 .LVU1055
.LLST130:
	.4byte	.LVL304-.Ltext0
	.4byte	.LVL306-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	0
	.4byte	0
.LVUS131:
	.uleb128 .LVU1011
	.uleb128 .LVU1017
	.uleb128 .LVU1017
	.uleb128 .LVU1055
.LLST131:
	.4byte	.LVL295-.Ltext0
	.4byte	.LVL298-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x4f
	.4byte	.LVL298-.Ltext0
	.4byte	.LVL306-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x50
	.4byte	0
	.4byte	0
.LVUS132:
	.uleb128 .LVU990
	.uleb128 .LVU1002
	.uleb128 .LVU1002
	.uleb128 .LVU1057
.LLST132:
	.4byte	.LVL291-.Ltext0
	.4byte	.LVL292-.Ltext0
	.2byte	0x6
	.byte	0x9e
	.uleb128 0x4
	.4byte	0x3f4ccccd
	.4byte	.LVL292-.Ltext0
	.4byte	.LVL308-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x58
	.4byte	0
	.4byte	0
.LVUS133:
	.uleb128 .LVU990
	.uleb128 .LVU1002
	.uleb128 .LVU1037
	.uleb128 .LVU1039
.LLST133:
	.4byte	.LVL291-.Ltext0
	.4byte	.LVL292-.Ltext0
	.2byte	0x6
	.byte	0x9e
	.uleb128 0x4
	.4byte	0x3f4ccccd
	.4byte	.LVL301-.Ltext0
	.4byte	.LVL302-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x4f
	.4byte	0
	.4byte	0
.LVUS134:
	.uleb128 .LVU992
	.uleb128 .LVU1002
	.uleb128 .LVU1002
	.uleb128 .LVU1057
.LLST134:
	.4byte	.LVL291-.Ltext0
	.4byte	.LVL292-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL292-.Ltext0
	.4byte	.LVL308-.Ltext0
	.2byte	0x1
	.byte	0x56
	.4byte	0
	.4byte	0
.LVUS135:
	.uleb128 .LVU1002
	.uleb128 .LVU1006
	.uleb128 .LVU1006
	.uleb128 .LVU1055
.LLST135:
	.4byte	.LVL292-.Ltext0
	.4byte	.LVL293-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL293-.Ltext0
	.4byte	.LVL306-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	0
	.4byte	0
.LVUS121:
	.uleb128 0
	.uleb128 .LVU907
	.uleb128 .LVU907
	.uleb128 0
.LLST121:
	.4byte	.LVL265-.Ltext0
	.4byte	.LVL266-1-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x40
	.4byte	.LVL266-1-.Ltext0
	.4byte	.LFE76-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x59
	.4byte	0
	.4byte	0
.LVUS122:
	.uleb128 0
	.uleb128 .LVU907
	.uleb128 .LVU907
	.uleb128 .LVU983
	.uleb128 .LVU983
	.uleb128 .LVU984
	.uleb128 .LVU984
	.uleb128 0
.LLST122:
	.4byte	.LVL265-.Ltext0
	.4byte	.LVL266-1-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x41
	.4byte	.LVL266-1-.Ltext0
	.4byte	.LVL288-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x56
	.4byte	.LVL288-.Ltext0
	.4byte	.LVL289-.Ltext0
	.2byte	0x6
	.byte	0xf3
	.uleb128 0x3
	.byte	0xf5
	.uleb128 0x41
	.uleb128 0x25
	.byte	0x9f
	.4byte	.LVL289-.Ltext0
	.4byte	.LFE76-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x56
	.4byte	0
	.4byte	0
.LVUS123:
	.uleb128 .LVU966
	.uleb128 .LVU971
.LLST123:
	.4byte	.LVL282-.Ltext0
	.4byte	.LVL283-.Ltext0
	.2byte	0x29
	.byte	0x76
	.sleb128 0
	.byte	0xf7
	.uleb128 0x33
	.byte	0xf7
	.uleb128 0x25
	.byte	0xf5
	.uleb128 0x5b
	.uleb128 0x25
	.byte	0x1e
	.byte	0xf5
	.uleb128 0x59
	.uleb128 0x25
	.byte	0x1e
	.byte	0xf5
	.uleb128 0x5a
	.uleb128 0x25
	.byte	0x22
	.byte	0xf7
	.uleb128 0x2c
	.byte	0xf5
	.uleb128 0x40
	.uleb128 0x2c
	.byte	0x1e
	.byte	0xf5
	.uleb128 0x5c
	.uleb128 0x25
	.byte	0xf4
	.uleb128 0x25
	.byte	0x4
	.4byte	0x3f800000
	.byte	0x22
	.byte	0xf7
	.uleb128 0x2c
	.byte	0x1e
	.byte	0xf7
	.uleb128 0x25
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS124:
	.uleb128 .LVU940
	.uleb128 .LVU946
	.uleb128 .LVU946
	.uleb128 .LVU952
	.uleb128 .LVU952
	.uleb128 .LVU980
.LLST124:
	.4byte	.LVL274-.Ltext0
	.4byte	.LVL277-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x4f
	.4byte	.LVL277-.Ltext0
	.4byte	.LVL279-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x5c
	.4byte	.LVL279-.Ltext0
	.4byte	.LVL285-.Ltext0
	.2byte	0xc
	.byte	0xf5
	.uleb128 0x5c
	.uleb128 0x25
	.byte	0xf4
	.uleb128 0x25
	.byte	0x4
	.4byte	0x3f800000
	.byte	0x22
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS125:
	.uleb128 .LVU954
	.uleb128 .LVU962
.LLST125:
	.4byte	.LVL280-.Ltext0
	.4byte	.LVL281-.Ltext0
	.2byte	0x13
	.byte	0x70
	.sleb128 0
	.byte	0xa
	.2byte	0xffff
	.byte	0x1a
	.byte	0xf7
	.uleb128 0x33
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
.LVUS126:
	.uleb128 .LVU930
	.uleb128 .LVU935
	.uleb128 .LVU935
	.uleb128 .LVU981
	.uleb128 .LVU981
	.uleb128 .LVU982
.LLST126:
	.4byte	.LVL271-.Ltext0
	.4byte	.LVL272-.Ltext0
	.2byte	0xc
	.byte	0xf5
	.uleb128 0x4f
	.uleb128 0x25
	.byte	0xf5
	.uleb128 0x59
	.uleb128 0x25
	.byte	0x1e
	.byte	0xf5
	.uleb128 0x5a
	.uleb128 0x25
	.byte	0x22
	.byte	0x9f
	.4byte	.LVL272-.Ltext0
	.4byte	.LVL286-.Ltext0
	.2byte	0x13
	.byte	0x76
	.sleb128 0
	.byte	0xf7
	.uleb128 0x33
	.byte	0xf7
	.uleb128 0x25
	.byte	0xf5
	.uleb128 0x5b
	.uleb128 0x25
	.byte	0x1e
	.byte	0xf5
	.uleb128 0x59
	.uleb128 0x25
	.byte	0x1e
	.byte	0xf5
	.uleb128 0x5a
	.uleb128 0x25
	.byte	0x22
	.byte	0x9f
	.4byte	.LVL286-.Ltext0
	.4byte	.LVL287-.Ltext0
	.2byte	0x13
	.byte	0x76
	.sleb128 -1
	.byte	0xf7
	.uleb128 0x33
	.byte	0xf7
	.uleb128 0x25
	.byte	0xf5
	.uleb128 0x5b
	.uleb128 0x25
	.byte	0x1e
	.byte	0xf5
	.uleb128 0x59
	.uleb128 0x25
	.byte	0x1e
	.byte	0xf5
	.uleb128 0x5a
	.uleb128 0x25
	.byte	0x22
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS127:
	.uleb128 .LVU914
	.uleb128 .LVU923
	.uleb128 .LVU923
	.uleb128 .LVU982
.LLST127:
	.4byte	.LVL269-.Ltext0
	.4byte	.LVL270-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL270-.Ltext0
	.4byte	.LVL287-.Ltext0
	.2byte	0x1
	.byte	0x56
	.4byte	0
	.4byte	0
.LVUS128:
	.uleb128 .LVU931
	.uleb128 .LVU935
	.uleb128 .LVU935
	.uleb128 .LVU980
.LLST128:
	.4byte	.LVL271-.Ltext0
	.4byte	.LVL272-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL272-.Ltext0
	.4byte	.LVL285-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	0
	.4byte	0
.LVUS119:
	.uleb128 0
	.uleb128 .LVU872
	.uleb128 .LVU872
	.uleb128 .LVU893
	.uleb128 .LVU893
	.uleb128 0
.LLST119:
	.4byte	.LVL252-.Ltext0
	.4byte	.LVL253-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL253-.Ltext0
	.4byte	.LVL264-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	.LVL264-.Ltext0
	.4byte	.LFE75-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS120:
	.uleb128 .LVU875
	.uleb128 .LVU878
	.uleb128 .LVU883
	.uleb128 .LVU884
.LLST120:
	.4byte	.LVL255-.Ltext0
	.4byte	.LVL256-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL261-.Ltext0
	.4byte	.LVL262-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	0
	.4byte	0
.LVUS97:
	.uleb128 0
	.uleb128 .LVU777
	.uleb128 .LVU777
	.uleb128 .LVU848
	.uleb128 .LVU848
	.uleb128 .LVU849
	.uleb128 .LVU849
	.uleb128 0
.LLST97:
	.4byte	.LVL216-.Ltext0
	.4byte	.LVL220-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL220-.Ltext0
	.4byte	.LVL245-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	.LVL245-.Ltext0
	.4byte	.LVL246-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL246-.Ltext0
	.4byte	.LFE73-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS98:
	.uleb128 .LVU756
	.uleb128 0
.LLST98:
	.4byte	.LVL217-.Ltext0
	.4byte	.LFE73-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	0
	.4byte	0
.LVUS99:
	.uleb128 .LVU757
	.uleb128 0
.LLST99:
	.4byte	.LVL217-.Ltext0
	.4byte	.LFE73-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	0
	.4byte	0
.LVUS100:
	.uleb128 .LVU832
	.uleb128 .LVU848
.LLST100:
	.4byte	.LVL238-.Ltext0
	.4byte	.LVL245-.Ltext0
	.2byte	0x1
	.byte	0x5a
	.4byte	0
	.4byte	0
.LVUS101:
	.uleb128 .LVU830
	.uleb128 .LVU848
.LLST101:
	.4byte	.LVL237-.Ltext0
	.4byte	.LVL245-.Ltext0
	.2byte	0x1
	.byte	0x57
	.4byte	0
	.4byte	0
.LVUS102:
	.uleb128 .LVU828
	.uleb128 .LVU848
.LLST102:
	.4byte	.LVL237-.Ltext0
	.4byte	.LVL245-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	0
	.4byte	0
.LVUS103:
	.uleb128 .LVU779
	.uleb128 .LVU782
	.uleb128 .LVU784
	.uleb128 .LVU848
.LLST103:
	.4byte	.LVL221-.Ltext0
	.4byte	.LVL223-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL224-.Ltext0
	.4byte	.LVL245-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	0
	.4byte	0
.LVUS104:
	.uleb128 .LVU800
	.uleb128 .LVU815
	.uleb128 .LVU819
	.uleb128 .LVU822
.LLST104:
	.4byte	.LVL229-.Ltext0
	.4byte	.LVL231-.Ltext0
	.2byte	0x8
	.byte	0x7b
	.sleb128 0
	.byte	0x7e
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL235-.Ltext0
	.4byte	.LVL236-.Ltext0
	.2byte	0x1
	.byte	0x5e
	.4byte	0
	.4byte	0
.LVUS105:
	.uleb128 .LVU801
	.uleb128 .LVU815
	.uleb128 .LVU816
	.uleb128 .LVU818
.LLST105:
	.4byte	.LVL229-.Ltext0
	.4byte	.LVL231-.Ltext0
	.2byte	0x9
	.byte	0x7e
	.sleb128 0
	.byte	0x7d
	.sleb128 0
	.byte	0x6
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL232-.Ltext0
	.4byte	.LVL234-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	0
	.4byte	0
.LVUS106:
	.uleb128 .LVU802
	.uleb128 .LVU814
	.uleb128 .LVU814
	.uleb128 .LVU817
.LLST106:
	.4byte	.LVL229-.Ltext0
	.4byte	.LVL230-.Ltext0
	.2byte	0x9
	.byte	0x7e
	.sleb128 0
	.byte	0x91
	.sleb128 -52
	.byte	0x6
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL230-.Ltext0
	.4byte	.LVL233-.Ltext0
	.2byte	0x1
	.byte	0x5c
	.4byte	0
	.4byte	0
.LVUS107:
	.uleb128 .LVU771
	.uleb128 .LVU777
	.uleb128 .LVU781
	.uleb128 .LVU782
.LLST107:
	.4byte	.LVL219-.Ltext0
	.4byte	.LVL220-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL222-.Ltext0
	.4byte	.LVL223-.Ltext0
	.2byte	0x1
	.byte	0x51
	.4byte	0
	.4byte	0
.LVUS108:
	.uleb128 .LVU829
	.uleb128 .LVU848
.LLST108:
	.4byte	.LVL237-.Ltext0
	.4byte	.LVL245-.Ltext0
	.2byte	0x1
	.byte	0x56
	.4byte	0
	.4byte	0
.LVUS109:
	.uleb128 .LVU769
	.uleb128 .LVU848
.LLST109:
	.4byte	.LVL219-.Ltext0
	.4byte	.LVL245-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	0
	.4byte	0
.LVUS110:
	.uleb128 .LVU770
	.uleb128 .LVU777
.LLST110:
	.4byte	.LVL219-.Ltext0
	.4byte	.LVL220-.Ltext0
	.2byte	0x9
	.byte	0x3
	.4byte	g_armwave_state+76
	.byte	0x6
	.byte	0x38
	.byte	0x25
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS111:
	.uleb128 .LVU803
	.uleb128 .LVU815
.LLST111:
	.4byte	.LVL229-.Ltext0
	.4byte	.LVL231-.Ltext0
	.2byte	0x13
	.byte	0x7b
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
.LVUS112:
	.uleb128 .LVU804
	.uleb128 .LVU815
.LLST112:
	.4byte	.LVL229-.Ltext0
	.4byte	.LVL231-.Ltext0
	.2byte	0x14
	.byte	0x7e
	.sleb128 0
	.byte	0x7d
	.sleb128 0
	.byte	0x6
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
.LVUS113:
	.uleb128 .LVU805
	.uleb128 .LVU815
.LLST113:
	.4byte	.LVL229-.Ltext0
	.4byte	.LVL231-.Ltext0
	.2byte	0x14
	.byte	0x7e
	.sleb128 0
	.byte	0x91
	.sleb128 -52
	.byte	0x6
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
.LVUS114:
	.uleb128 .LVU787
	.uleb128 .LVU789
	.uleb128 .LVU794
	.uleb128 .LVU797
.LLST114:
	.4byte	.LVL225-.Ltext0
	.4byte	.LVL226-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL227-.Ltext0
	.4byte	.LVL228-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	0
	.4byte	0
.LVUS115:
	.uleb128 .LVU833
	.uleb128 .LVU836
	.uleb128 .LVU836
	.uleb128 .LVU841
	.uleb128 .LVU843
	.uleb128 .LVU848
.LLST115:
	.4byte	.LVL238-.Ltext0
	.4byte	.LVL239-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL239-.Ltext0
	.4byte	.LVL241-.Ltext0
	.2byte	0x1
	.byte	0x5c
	.4byte	.LVL243-.Ltext0
	.4byte	.LVL245-.Ltext0
	.2byte	0x1
	.byte	0x5c
	.4byte	0
	.4byte	0
.LVUS116:
	.uleb128 .LVU764
	.uleb128 .LVU779
	.uleb128 .LVU779
	.uleb128 .LVU781
	.uleb128 .LVU781
	.uleb128 .LVU784
	.uleb128 .LVU784
	.uleb128 .LVU848
	.uleb128 .LVU848
	.uleb128 0
.LLST116:
	.4byte	.LVL218-.Ltext0
	.4byte	.LVL221-.Ltext0
	.2byte	0x1
	.byte	0x59
	.4byte	.LVL221-.Ltext0
	.4byte	.LVL222-.Ltext0
	.2byte	0x3
	.byte	0x79
	.sleb128 -4
	.byte	0x9f
	.4byte	.LVL222-.Ltext0
	.4byte	.LVL224-.Ltext0
	.2byte	0x1
	.byte	0x59
	.4byte	.LVL224-.Ltext0
	.4byte	.LVL245-.Ltext0
	.2byte	0x3
	.byte	0x79
	.sleb128 -4
	.byte	0x9f
	.4byte	.LVL245-.Ltext0
	.4byte	.LFE73-.Ltext0
	.2byte	0x1
	.byte	0x59
	.4byte	0
	.4byte	0
.LVUS117:
	.uleb128 .LVU765
	.uleb128 0
.LLST117:
	.4byte	.LVL218-.Ltext0
	.4byte	.LFE73-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	0
	.4byte	0
.LVUS118:
	.uleb128 .LVU837
	.uleb128 .LVU840
	.uleb128 .LVU842
	.uleb128 .LVU846
.LLST118:
	.4byte	.LVL239-.Ltext0
	.4byte	.LVL240-.Ltext0
	.2byte	0x10
	.byte	0x77
	.sleb128 0
	.byte	0x7e
	.sleb128 0
	.byte	0x22
	.byte	0x3
	.4byte	g_armwave_state+72
	.byte	0x6
	.byte	0x1e
	.byte	0x7a
	.sleb128 0
	.byte	0x22
	.byte	0x9f
	.4byte	.LVL242-.Ltext0
	.4byte	.LVL244-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	0
	.4byte	0
.LVUS96:
	.uleb128 0
	.uleb128 .LVU748
	.uleb128 .LVU748
	.uleb128 .LVU749
	.uleb128 .LVU749
	.uleb128 0
.LLST96:
	.4byte	.LVL213-.Ltext0
	.4byte	.LVL214-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL214-.Ltext0
	.4byte	.LVL215-1-.Ltext0
	.2byte	0x1
	.byte	0x51
	.4byte	.LVL215-1-.Ltext0
	.4byte	.LFE72-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS76:
	.uleb128 .LVU655
	.uleb128 0
.LLST76:
	.4byte	.LVL183-.Ltext0
	.4byte	.LFE71-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	0
	.4byte	0
.LVUS77:
	.uleb128 .LVU729
	.uleb128 .LVU745
.LLST77:
	.4byte	.LVL204-.Ltext0
	.4byte	.LVL211-.Ltext0
	.2byte	0x1
	.byte	0x5a
	.4byte	0
	.4byte	0
.LVUS78:
	.uleb128 .LVU727
	.uleb128 .LVU745
.LLST78:
	.4byte	.LVL203-.Ltext0
	.4byte	.LVL211-.Ltext0
	.2byte	0x1
	.byte	0x57
	.4byte	0
	.4byte	0
.LVUS79:
	.uleb128 .LVU725
	.uleb128 .LVU745
.LLST79:
	.4byte	.LVL203-.Ltext0
	.4byte	.LVL211-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	0
	.4byte	0
.LVUS80:
	.uleb128 .LVU676
	.uleb128 .LVU679
	.uleb128 .LVU681
	.uleb128 .LVU745
.LLST80:
	.4byte	.LVL187-.Ltext0
	.4byte	.LVL189-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL190-.Ltext0
	.4byte	.LVL211-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	0
	.4byte	0
.LVUS81:
	.uleb128 .LVU697
	.uleb128 .LVU712
	.uleb128 .LVU716
	.uleb128 .LVU719
.LLST81:
	.4byte	.LVL195-.Ltext0
	.4byte	.LVL197-.Ltext0
	.2byte	0x8
	.byte	0x7b
	.sleb128 0
	.byte	0x7e
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL201-.Ltext0
	.4byte	.LVL202-.Ltext0
	.2byte	0x1
	.byte	0x5e
	.4byte	0
	.4byte	0
.LVUS82:
	.uleb128 .LVU698
	.uleb128 .LVU712
	.uleb128 .LVU713
	.uleb128 .LVU715
.LLST82:
	.4byte	.LVL195-.Ltext0
	.4byte	.LVL197-.Ltext0
	.2byte	0x9
	.byte	0x7e
	.sleb128 0
	.byte	0x7d
	.sleb128 0
	.byte	0x6
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL198-.Ltext0
	.4byte	.LVL200-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	0
	.4byte	0
.LVUS83:
	.uleb128 .LVU699
	.uleb128 .LVU711
	.uleb128 .LVU711
	.uleb128 .LVU714
.LLST83:
	.4byte	.LVL195-.Ltext0
	.4byte	.LVL196-.Ltext0
	.2byte	0x9
	.byte	0x7e
	.sleb128 0
	.byte	0x91
	.sleb128 -52
	.byte	0x6
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL196-.Ltext0
	.4byte	.LVL199-.Ltext0
	.2byte	0x1
	.byte	0x5c
	.4byte	0
	.4byte	0
.LVUS84:
	.uleb128 .LVU668
	.uleb128 .LVU674
	.uleb128 .LVU678
	.uleb128 .LVU679
.LLST84:
	.4byte	.LVL185-.Ltext0
	.4byte	.LVL186-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL188-.Ltext0
	.4byte	.LVL189-.Ltext0
	.2byte	0x1
	.byte	0x51
	.4byte	0
	.4byte	0
.LVUS85:
	.uleb128 .LVU726
	.uleb128 .LVU745
.LLST85:
	.4byte	.LVL203-.Ltext0
	.4byte	.LVL211-.Ltext0
	.2byte	0x1
	.byte	0x56
	.4byte	0
	.4byte	0
.LVUS86:
	.uleb128 .LVU666
	.uleb128 .LVU745
.LLST86:
	.4byte	.LVL185-.Ltext0
	.4byte	.LVL211-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	0
	.4byte	0
.LVUS87:
	.uleb128 .LVU667
	.uleb128 .LVU674
.LLST87:
	.4byte	.LVL185-.Ltext0
	.4byte	.LVL186-.Ltext0
	.2byte	0x9
	.byte	0x3
	.4byte	g_armwave_state+76
	.byte	0x6
	.byte	0x38
	.byte	0x25
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS88:
	.uleb128 .LVU700
	.uleb128 .LVU712
.LLST88:
	.4byte	.LVL195-.Ltext0
	.4byte	.LVL197-.Ltext0
	.2byte	0x13
	.byte	0x7b
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
.LVUS89:
	.uleb128 .LVU701
	.uleb128 .LVU712
.LLST89:
	.4byte	.LVL195-.Ltext0
	.4byte	.LVL197-.Ltext0
	.2byte	0x14
	.byte	0x7e
	.sleb128 0
	.byte	0x7d
	.sleb128 0
	.byte	0x6
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
.LVUS90:
	.uleb128 .LVU702
	.uleb128 .LVU712
.LLST90:
	.4byte	.LVL195-.Ltext0
	.4byte	.LVL197-.Ltext0
	.2byte	0x14
	.byte	0x7e
	.sleb128 0
	.byte	0x91
	.sleb128 -52
	.byte	0x6
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
.LVUS91:
	.uleb128 .LVU684
	.uleb128 .LVU686
	.uleb128 .LVU691
	.uleb128 .LVU694
.LLST91:
	.4byte	.LVL191-.Ltext0
	.4byte	.LVL192-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL193-.Ltext0
	.4byte	.LVL194-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	0
	.4byte	0
.LVUS92:
	.uleb128 .LVU730
	.uleb128 .LVU733
	.uleb128 .LVU733
	.uleb128 .LVU738
	.uleb128 .LVU740
	.uleb128 .LVU745
.LLST92:
	.4byte	.LVL204-.Ltext0
	.4byte	.LVL205-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL205-.Ltext0
	.4byte	.LVL207-.Ltext0
	.2byte	0x1
	.byte	0x5c
	.4byte	.LVL209-.Ltext0
	.4byte	.LVL211-.Ltext0
	.2byte	0x1
	.byte	0x5c
	.4byte	0
	.4byte	0
.LVUS93:
	.uleb128 .LVU661
	.uleb128 .LVU676
	.uleb128 .LVU676
	.uleb128 .LVU678
	.uleb128 .LVU678
	.uleb128 .LVU681
	.uleb128 .LVU681
	.uleb128 .LVU745
	.uleb128 .LVU745
	.uleb128 0
.LLST93:
	.4byte	.LVL184-.Ltext0
	.4byte	.LVL187-.Ltext0
	.2byte	0x1
	.byte	0x59
	.4byte	.LVL187-.Ltext0
	.4byte	.LVL188-.Ltext0
	.2byte	0x3
	.byte	0x79
	.sleb128 -4
	.byte	0x9f
	.4byte	.LVL188-.Ltext0
	.4byte	.LVL190-.Ltext0
	.2byte	0x1
	.byte	0x59
	.4byte	.LVL190-.Ltext0
	.4byte	.LVL211-.Ltext0
	.2byte	0x3
	.byte	0x79
	.sleb128 -4
	.byte	0x9f
	.4byte	.LVL211-.Ltext0
	.4byte	.LFE71-.Ltext0
	.2byte	0x1
	.byte	0x59
	.4byte	0
	.4byte	0
.LVUS94:
	.uleb128 .LVU662
	.uleb128 0
.LLST94:
	.4byte	.LVL184-.Ltext0
	.4byte	.LFE71-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	0
	.4byte	0
.LVUS95:
	.uleb128 .LVU734
	.uleb128 .LVU737
	.uleb128 .LVU739
	.uleb128 .LVU743
.LLST95:
	.4byte	.LVL205-.Ltext0
	.4byte	.LVL206-.Ltext0
	.2byte	0x10
	.byte	0x77
	.sleb128 0
	.byte	0x7e
	.sleb128 0
	.byte	0x22
	.byte	0x3
	.4byte	g_armwave_state+72
	.byte	0x6
	.byte	0x1e
	.byte	0x7a
	.sleb128 0
	.byte	0x22
	.byte	0x9f
	.4byte	.LVL208-.Ltext0
	.4byte	.LVL210-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	0
	.4byte	0
.LVUS75:
	.uleb128 .LVU641
	.uleb128 .LVU645
	.uleb128 .LVU645
	.uleb128 0
.LLST75:
	.4byte	.LVL177-.Ltext0
	.4byte	.LVL178-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL178-.Ltext0
	.4byte	.LFE70-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	0
	.4byte	0
.LVUS66:
	.uleb128 0
	.uleb128 .LVU607
	.uleb128 .LVU607
	.uleb128 .LVU634
	.uleb128 .LVU634
	.uleb128 0
.LLST66:
	.4byte	.LVL168-.Ltext0
	.4byte	.LVL169-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL169-.Ltext0
	.4byte	.LVL175-.Ltext0
	.2byte	0x1
	.byte	0x56
	.4byte	.LVL175-.Ltext0
	.4byte	.LFE69-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS67:
	.uleb128 0
	.uleb128 .LVU607
	.uleb128 .LVU607
	.uleb128 .LVU634
	.uleb128 .LVU634
	.uleb128 0
.LLST67:
	.4byte	.LVL168-.Ltext0
	.4byte	.LVL169-.Ltext0
	.2byte	0x1
	.byte	0x51
	.4byte	.LVL169-.Ltext0
	.4byte	.LVL175-.Ltext0
	.2byte	0x1
	.byte	0x57
	.4byte	.LVL175-.Ltext0
	.4byte	.LFE69-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x51
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS68:
	.uleb128 0
	.uleb128 .LVU607
	.uleb128 .LVU607
	.uleb128 .LVU634
	.uleb128 .LVU634
	.uleb128 0
.LLST68:
	.4byte	.LVL168-.Ltext0
	.4byte	.LVL169-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	.LVL169-.Ltext0
	.4byte	.LVL175-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	.LVL175-.Ltext0
	.4byte	.LFE69-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x52
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS69:
	.uleb128 0
	.uleb128 .LVU607
	.uleb128 .LVU607
	.uleb128 .LVU634
	.uleb128 .LVU634
	.uleb128 0
.LLST69:
	.4byte	.LVL168-.Ltext0
	.4byte	.LVL169-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	.LVL169-.Ltext0
	.4byte	.LVL175-.Ltext0
	.2byte	0x1
	.byte	0x59
	.4byte	.LVL175-.Ltext0
	.4byte	.LFE69-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x53
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS70:
	.uleb128 .LVU600
	.uleb128 .LVU607
	.uleb128 .LVU607
	.uleb128 .LVU615
.LLST70:
	.4byte	.LVL168-.Ltext0
	.4byte	.LVL169-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL169-.Ltext0
	.4byte	.LVL172-.Ltext0
	.2byte	0x9
	.byte	0x74
	.sleb128 0
	.byte	0x3
	.4byte	gamma_table-1
	.byte	0x1c
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS71:
	.uleb128 .LVU617
	.uleb128 .LVU630
.LLST71:
	.4byte	.LVL172-.Ltext0
	.4byte	.LVL173-.Ltext0
	.2byte	0x3
	.byte	0x8
	.byte	0xfa
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS72:
	.uleb128 .LVU617
	.uleb128 .LVU630
.LLST72:
	.4byte	.LVL172-.Ltext0
	.4byte	.LVL173-.Ltext0
	.2byte	0x4
	.byte	0xa
	.2byte	0x6f4
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS73:
	.uleb128 .LVU617
	.uleb128 .LVU630
.LLST73:
	.4byte	.LVL172-.Ltext0
	.4byte	.LVL173-.Ltext0
	.2byte	0x4
	.byte	0xa
	.2byte	0x9f6
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS74:
	.uleb128 .LVU617
	.uleb128 .LVU630
.LLST74:
	.4byte	.LVL172-.Ltext0
	.4byte	.LVL173-.Ltext0
	.2byte	0x2
	.byte	0x31
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS60:
	.uleb128 0
	.uleb128 .LVU560
	.uleb128 .LVU560
	.uleb128 .LVU594
	.uleb128 .LVU594
	.uleb128 .LVU595
	.uleb128 .LVU595
	.uleb128 0
.LLST60:
	.4byte	.LVL147-.Ltext0
	.4byte	.LVL149-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL149-.Ltext0
	.4byte	.LVL165-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	.LVL165-.Ltext0
	.4byte	.LVL166-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	.LVL166-.Ltext0
	.4byte	.LFE68-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	0
	.4byte	0
.LVUS61:
	.uleb128 0
	.uleb128 .LVU559
	.uleb128 .LVU559
	.uleb128 .LVU561
	.uleb128 .LVU561
	.uleb128 0
.LLST61:
	.4byte	.LVL147-.Ltext0
	.4byte	.LVL148-.Ltext0
	.2byte	0x1
	.byte	0x51
	.4byte	.LVL148-.Ltext0
	.4byte	.LVL150-1-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	.LVL150-1-.Ltext0
	.4byte	.LFE68-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x51
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS62:
	.uleb128 .LVU564
	.uleb128 .LVU567
	.uleb128 .LVU567
	.uleb128 .LVU568
	.uleb128 .LVU568
	.uleb128 .LVU594
	.uleb128 .LVU594
	.uleb128 .LVU595
	.uleb128 .LVU595
	.uleb128 0
.LLST62:
	.4byte	.LVL151-.Ltext0
	.4byte	.LVL152-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL152-.Ltext0
	.4byte	.LVL153-1-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	.LVL153-1-.Ltext0
	.4byte	.LVL165-.Ltext0
	.2byte	0x1
	.byte	0x57
	.4byte	.LVL165-.Ltext0
	.4byte	.LVL166-1-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL166-.Ltext0
	.4byte	.LFE68-.Ltext0
	.2byte	0x1
	.byte	0x57
	.4byte	0
	.4byte	0
.LVUS63:
	.uleb128 .LVU582
	.uleb128 .LVU587
.LLST63:
	.4byte	.LVL158-.Ltext0
	.4byte	.LVL160-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	0
	.4byte	0
.LVUS64:
	.uleb128 .LVU575
	.uleb128 .LVU578
	.uleb128 .LVU578
	.uleb128 .LVU584
	.uleb128 .LVU584
	.uleb128 .LVU588
	.uleb128 .LVU588
	.uleb128 .LVU590
	.uleb128 .LVU595
	.uleb128 0
.LLST64:
	.4byte	.LVL156-.Ltext0
	.4byte	.LVL157-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL157-.Ltext0
	.4byte	.LVL159-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	.LVL159-.Ltext0
	.4byte	.LVL161-.Ltext0
	.2byte	0x3
	.byte	0x74
	.sleb128 -1
	.byte	0x9f
	.4byte	.LVL161-.Ltext0
	.4byte	.LVL162-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	.LVL166-.Ltext0
	.4byte	.LFE68-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS65:
	.uleb128 .LVU571
	.uleb128 .LVU575
	.uleb128 .LVU575
	.uleb128 .LVU592
	.uleb128 .LVU595
	.uleb128 0
.LLST65:
	.4byte	.LVL155-.Ltext0
	.4byte	.LVL156-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL156-.Ltext0
	.4byte	.LVL164-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	.LVL166-.Ltext0
	.4byte	.LFE68-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	0
	.4byte	0
.LVUS58:
	.uleb128 0
	.uleb128 .LVU543
	.uleb128 .LVU543
	.uleb128 0
.LLST58:
	.4byte	.LVL142-.Ltext0
	.4byte	.LVL143-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL143-.Ltext0
	.4byte	.LFE66-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS57:
	.uleb128 0
	.uleb128 .LVU540
	.uleb128 .LVU540
	.uleb128 0
.LLST57:
	.4byte	.LVL139-.Ltext0
	.4byte	.LVL140-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL140-.Ltext0
	.4byte	.LFE65-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS56:
	.uleb128 0
	.uleb128 .LVU527
	.uleb128 .LVU527
	.uleb128 0
.LLST56:
	.4byte	.LVL136-.Ltext0
	.4byte	.LVL137-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL137-.Ltext0
	.4byte	.LFE63-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS49:
	.uleb128 0
	.uleb128 .LVU412
	.uleb128 .LVU412
	.uleb128 .LVU413
	.uleb128 .LVU413
	.uleb128 .LVU434
	.uleb128 .LVU434
	.uleb128 .LVU501
	.uleb128 .LVU501
	.uleb128 .LVU516
	.uleb128 .LVU516
	.uleb128 .LVU518
	.uleb128 .LVU518
	.uleb128 0
.LLST49:
	.4byte	.LVL114-.Ltext0
	.4byte	.LVL118-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL118-.Ltext0
	.4byte	.LVL119-1-.Ltext0
	.2byte	0x1
	.byte	0x51
	.4byte	.LVL119-1-.Ltext0
	.4byte	.LVL120-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	.LVL120-.Ltext0
	.4byte	.LVL131-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	.LVL131-.Ltext0
	.4byte	.LVL132-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	.LVL132-.Ltext0
	.4byte	.LVL134-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	.LVL134-.Ltext0
	.4byte	.LFE62-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	0
	.4byte	0
.LVUS50:
	.uleb128 0
	.uleb128 .LVU411
	.uleb128 .LVU411
	.uleb128 .LVU413
	.uleb128 .LVU413
	.uleb128 0
.LLST50:
	.4byte	.LVL114-.Ltext0
	.4byte	.LVL117-.Ltext0
	.2byte	0x1
	.byte	0x51
	.4byte	.LVL117-.Ltext0
	.4byte	.LVL119-1-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	.LVL119-1-.Ltext0
	.4byte	.LFE62-.Ltext0
	.2byte	0x1
	.byte	0x59
	.4byte	0
	.4byte	0
.LVUS51:
	.uleb128 0
	.uleb128 .LVU410
	.uleb128 .LVU410
	.uleb128 .LVU413
	.uleb128 .LVU413
	.uleb128 0
.LLST51:
	.4byte	.LVL114-.Ltext0
	.4byte	.LVL116-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	.LVL116-.Ltext0
	.4byte	.LVL119-1-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	.LVL119-1-.Ltext0
	.4byte	.LFE62-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	0
	.4byte	0
.LVUS52:
	.uleb128 0
	.uleb128 .LVU409
	.uleb128 .LVU409
	.uleb128 0
.LLST52:
	.4byte	.LVL114-.Ltext0
	.4byte	.LVL115-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	.LVL115-.Ltext0
	.4byte	.LFE62-.Ltext0
	.2byte	0x1
	.byte	0x5a
	.4byte	0
	.4byte	0
.LVUS53:
	.uleb128 .LVU470
	.uleb128 .LVU501
	.uleb128 .LVU517
	.uleb128 .LVU518
.LLST53:
	.4byte	.LVL124-.Ltext0
	.4byte	.LVL131-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	.LVL133-.Ltext0
	.4byte	.LVL134-.Ltext0
	.2byte	0x1
	.byte	0x55
	.4byte	0
	.4byte	0
.LVUS54:
	.uleb128 .LVU489
	.uleb128 .LVU498
.LLST54:
	.4byte	.LVL127-.Ltext0
	.4byte	.LVL129-1-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	0
	.4byte	0
.LVUS55:
	.uleb128 .LVU478
	.uleb128 .LVU500
	.uleb128 .LVU500
	.uleb128 .LVU501
	.uleb128 .LVU517
	.uleb128 .LVU518
.LLST55:
	.4byte	.LVL125-.Ltext0
	.4byte	.LVL130-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x50
	.4byte	.LVL130-.Ltext0
	.4byte	.LVL131-.Ltext0
	.2byte	0xe
	.byte	0x75
	.sleb128 0
	.byte	0xf7
	.uleb128 0x3a
	.byte	0xf7
	.uleb128 0x25
	.byte	0x77
	.sleb128 0
	.byte	0xf7
	.uleb128 0x3a
	.byte	0xf7
	.uleb128 0x25
	.byte	0x1b
	.byte	0x9f
	.4byte	.LVL133-.Ltext0
	.4byte	.LVL134-.Ltext0
	.2byte	0x2
	.byte	0x90
	.uleb128 0x50
	.4byte	0
	.4byte	0
.LVUS11:
	.uleb128 0
	.uleb128 .LVU119
	.uleb128 .LVU119
	.uleb128 .LVU125
	.uleb128 .LVU125
	.uleb128 .LVU126
	.uleb128 .LVU126
	.uleb128 .LVU300
	.uleb128 .LVU300
	.uleb128 .LVU302
	.uleb128 .LVU302
	.uleb128 0
.LLST11:
	.4byte	.LVL27-.Ltext0
	.4byte	.LVL30-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL30-.Ltext0
	.4byte	.LVL34-.Ltext0
	.2byte	0x2
	.byte	0x7d
	.sleb128 0
	.4byte	.LVL34-.Ltext0
	.4byte	.LVL35-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	.LVL35-.Ltext0
	.4byte	.LVL77-.Ltext0
	.2byte	0x2
	.byte	0x7d
	.sleb128 0
	.4byte	.LVL77-.Ltext0
	.4byte	.LVL79-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL79-.Ltext0
	.4byte	.LFE60-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS12:
	.uleb128 .LVU141
	.uleb128 .LVU169
.LLST12:
	.4byte	.LVL37-.Ltext0
	.4byte	.LVL46-.Ltext0
	.2byte	0x6
	.byte	0x73
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS13:
	.uleb128 .LVU142
	.uleb128 .LVU163
	.uleb128 .LVU163
	.uleb128 .LVU169
.LLST13:
	.4byte	.LVL37-.Ltext0
	.4byte	.LVL45-.Ltext0
	.2byte	0x5
	.byte	0x73
	.sleb128 0
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL45-.Ltext0
	.4byte	.LVL46-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	0
	.4byte	0
.LVUS14:
	.uleb128 .LVU139
	.uleb128 .LVU147
	.uleb128 .LVU147
	.uleb128 .LVU149
	.uleb128 .LVU149
	.uleb128 .LVU150
	.uleb128 .LVU150
	.uleb128 .LVU151
	.uleb128 .LVU151
	.uleb128 .LVU152
	.uleb128 .LVU152
	.uleb128 .LVU153
	.uleb128 .LVU153
	.uleb128 .LVU162
	.uleb128 .LVU162
	.uleb128 .LVU169
	.uleb128 .LVU182
	.uleb128 .LVU186
	.uleb128 .LVU186
	.uleb128 .LVU187
	.uleb128 .LVU187
	.uleb128 .LVU188
	.uleb128 .LVU188
	.uleb128 .LVU189
	.uleb128 .LVU189
	.uleb128 .LVU197
	.uleb128 .LVU197
	.uleb128 .LVU200
	.uleb128 .LVU200
	.uleb128 .LVU206
	.uleb128 .LVU206
	.uleb128 .LVU214
	.uleb128 .LVU227
	.uleb128 .LVU231
	.uleb128 .LVU231
	.uleb128 .LVU232
	.uleb128 .LVU232
	.uleb128 .LVU234
	.uleb128 .LVU234
	.uleb128 .LVU235
	.uleb128 .LVU235
	.uleb128 .LVU236
	.uleb128 .LVU236
	.uleb128 .LVU242
	.uleb128 .LVU242
	.uleb128 .LVU245
	.uleb128 .LVU269
	.uleb128 .LVU273
	.uleb128 .LVU273
	.uleb128 .LVU274
.LLST14:
	.4byte	.LVL37-.Ltext0
	.4byte	.LVL38-.Ltext0
	.2byte	0x4e
	.byte	0x76
	.sleb128 0
	.byte	0x7b
	.sleb128 0
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
	.byte	0x76
	.sleb128 0
	.byte	0x79
	.sleb128 0
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
	.byte	0x7a
	.sleb128 0
	.byte	0x76
	.sleb128 0
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
	.4byte	.LVL38-.Ltext0
	.4byte	.LVL39-.Ltext0
	.2byte	0x49
	.byte	0x74
	.sleb128 0
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
	.byte	0x76
	.sleb128 0
	.byte	0x79
	.sleb128 0
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
	.byte	0x7a
	.sleb128 0
	.byte	0x76
	.sleb128 0
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
	.4byte	.LVL39-.Ltext0
	.4byte	.LVL40-.Ltext0
	.2byte	0x44
	.byte	0x74
	.sleb128 0
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
	.byte	0x7a
	.sleb128 0
	.byte	0x76
	.sleb128 0
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
	.4byte	.LVL40-.Ltext0
	.4byte	.LVL41-.Ltext0
	.2byte	0x49
	.byte	0x76
	.sleb128 0
	.byte	0x7b
	.sleb128 0
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
	.byte	0x7a
	.sleb128 0
	.byte	0x76
	.sleb128 0
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
	.4byte	.LVL41-.Ltext0
	.4byte	.LVL42-.Ltext0
	.2byte	0x4f
	.byte	0x72
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7b
	.sleb128 0
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
	.byte	0x72
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7a
	.sleb128 0
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
	.4byte	.LVL42-.Ltext0
	.4byte	.LVL43-.Ltext0
	.2byte	0x57
	.byte	0x72
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7b
	.sleb128 0
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
	.byte	0x72
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x79
	.sleb128 0
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
	.byte	0x72
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7a
	.sleb128 0
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
	.4byte	.LVL43-.Ltext0
	.4byte	.LVL44-.Ltext0
	.2byte	0x4f
	.byte	0x72
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7b
	.sleb128 0
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
	.byte	0x72
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x79
	.sleb128 0
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
	.byte	0x76
	.sleb128 0
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
	.4byte	.LVL44-.Ltext0
	.4byte	.LVL46-.Ltext0
	.2byte	0x57
	.byte	0x72
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7b
	.sleb128 0
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
	.byte	0x72
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x79
	.sleb128 0
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
	.byte	0x72
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7a
	.sleb128 0
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
	.4byte	.LVL49-.Ltext0
	.4byte	.LVL50-.Ltext0
	.2byte	0x4e
	.byte	0x7b
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.byte	0x79
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.byte	0x7a
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.4byte	.LVL50-.Ltext0
	.4byte	.LVL51-.Ltext0
	.2byte	0x49
	.byte	0x74
	.sleb128 0
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
	.byte	0x79
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.byte	0x7a
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.4byte	.LVL51-.Ltext0
	.4byte	.LVL52-.Ltext0
	.2byte	0x4e
	.byte	0x7b
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.byte	0x79
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.byte	0x7a
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.4byte	.LVL52-.Ltext0
	.4byte	.LVL53-.Ltext0
	.2byte	0x5d
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7b
	.sleb128 0
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
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x79
	.sleb128 0
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
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7a
	.sleb128 0
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
	.4byte	.LVL53-.Ltext0
	.4byte	.LVL54-.Ltext0
	.2byte	0x53
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7b
	.sleb128 0
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
	.byte	0x77
	.sleb128 0
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
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7a
	.sleb128 0
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
	.4byte	.LVL54-.Ltext0
	.4byte	.LVL55-.Ltext0
	.2byte	0x5d
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7b
	.sleb128 0
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
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x79
	.sleb128 0
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
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7a
	.sleb128 0
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
	.4byte	.LVL55-.Ltext0
	.4byte	.LVL56-.Ltext0
	.2byte	0x53
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7b
	.sleb128 0
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
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x79
	.sleb128 0
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
	.byte	0x7e
	.sleb128 0
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
	.4byte	.LVL56-.Ltext0
	.4byte	.LVL57-.Ltext0
	.2byte	0x5d
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7b
	.sleb128 0
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
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x79
	.sleb128 0
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
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7a
	.sleb128 0
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
	.4byte	.LVL59-.Ltext0
	.4byte	.LVL60-.Ltext0
	.2byte	0x4e
	.byte	0x7b
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.byte	0x79
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.byte	0x7a
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.4byte	.LVL60-.Ltext0
	.4byte	.LVL61-.Ltext0
	.2byte	0x49
	.byte	0x74
	.sleb128 0
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
	.byte	0x79
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.byte	0x7a
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.4byte	.LVL61-.Ltext0
	.4byte	.LVL62-.Ltext0
	.2byte	0x4f
	.byte	0x74
	.sleb128 0
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
	.byte	0x79
	.sleb128 0
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
	.byte	0x7a
	.sleb128 0
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
	.4byte	.LVL62-.Ltext0
	.4byte	.LVL63-.Ltext0
	.2byte	0x47
	.byte	0x74
	.sleb128 0
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
	.byte	0x76
	.sleb128 0
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
	.byte	0x7a
	.sleb128 0
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
	.4byte	.LVL63-.Ltext0
	.4byte	.LVL64-.Ltext0
	.2byte	0x4f
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7b
	.sleb128 0
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
	.byte	0x76
	.sleb128 0
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
	.byte	0x7a
	.sleb128 0
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
	.4byte	.LVL64-.Ltext0
	.4byte	.LVL65-.Ltext0
	.2byte	0x57
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7b
	.sleb128 0
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
	.byte	0x79
	.sleb128 0
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
	.byte	0x7a
	.sleb128 0
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
	.4byte	.LVL65-.Ltext0
	.4byte	.LVL66-.Ltext0
	.2byte	0x4f
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7b
	.sleb128 0
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
	.byte	0x79
	.sleb128 0
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
	.byte	0x7e
	.sleb128 0
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
	.4byte	.LVL69-.Ltext0
	.4byte	.LVL70-.Ltext0
	.2byte	0x4e
	.byte	0x7b
	.sleb128 0
	.byte	0x72
	.sleb128 0
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
	.byte	0x79
	.sleb128 0
	.byte	0x72
	.sleb128 0
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
	.byte	0x7a
	.sleb128 0
	.byte	0x72
	.sleb128 0
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
	.4byte	.LVL70-.Ltext0
	.4byte	.LVL71-.Ltext0
	.2byte	0x49
	.byte	0x7e
	.sleb128 0
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
	.byte	0x79
	.sleb128 0
	.byte	0x72
	.sleb128 0
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
	.byte	0x7a
	.sleb128 0
	.byte	0x72
	.sleb128 0
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
.LVUS15:
	.uleb128 .LVU121
	.uleb128 .LVU123
	.uleb128 .LVU126
	.uleb128 .LVU130
	.uleb128 .LVU130
	.uleb128 .LVU172
	.uleb128 .LVU172
	.uleb128 .LVU173
	.uleb128 .LVU173
	.uleb128 .LVU218
	.uleb128 .LVU218
	.uleb128 .LVU260
	.uleb128 .LVU260
	.uleb128 .LVU300
.LLST15:
	.4byte	.LVL31-.Ltext0
	.4byte	.LVL32-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	.LVL35-.Ltext0
	.4byte	.LVL36-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	.LVL36-.Ltext0
	.4byte	.LVL47-.Ltext0
	.2byte	0x1
	.byte	0x5e
	.4byte	.LVL47-.Ltext0
	.4byte	.LVL48-.Ltext0
	.2byte	0x5
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x9f
	.4byte	.LVL48-.Ltext0
	.4byte	.LVL58-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	.LVL58-.Ltext0
	.4byte	.LVL68-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	.LVL68-.Ltext0
	.4byte	.LVL77-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS16:
	.uleb128 .LVU133
	.uleb128 .LVU151
	.uleb128 .LVU151
	.uleb128 .LVU153
	.uleb128 .LVU153
	.uleb128 .LVU162
	.uleb128 .LVU162
	.uleb128 .LVU169
	.uleb128 .LVU176
	.uleb128 .LVU188
	.uleb128 .LVU188
	.uleb128 .LVU200
	.uleb128 .LVU200
	.uleb128 .LVU206
	.uleb128 .LVU206
	.uleb128 .LVU214
	.uleb128 .LVU221
	.uleb128 .LVU232
	.uleb128 .LVU232
	.uleb128 .LVU242
	.uleb128 .LVU242
	.uleb128 .LVU248
	.uleb128 .LVU263
	.uleb128 .LVU274
	.uleb128 .LVU284
	.uleb128 .LVU290
.LLST16:
	.4byte	.LVL37-.Ltext0
	.4byte	.LVL41-.Ltext0
	.2byte	0x8
	.byte	0x7a
	.sleb128 0
	.byte	0x76
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL41-.Ltext0
	.4byte	.LVL43-.Ltext0
	.2byte	0xb
	.byte	0x72
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7a
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL43-.Ltext0
	.4byte	.LVL44-.Ltext0
	.2byte	0x1
	.byte	0x56
	.4byte	.LVL44-.Ltext0
	.4byte	.LVL46-.Ltext0
	.2byte	0xb
	.byte	0x72
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7a
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL49-.Ltext0
	.4byte	.LVL52-.Ltext0
	.2byte	0x8
	.byte	0x7a
	.sleb128 0
	.byte	0x7e
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL52-.Ltext0
	.4byte	.LVL55-.Ltext0
	.2byte	0xd
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7a
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL55-.Ltext0
	.4byte	.LVL56-.Ltext0
	.2byte	0x1
	.byte	0x5e
	.4byte	.LVL56-.Ltext0
	.4byte	.LVL57-.Ltext0
	.2byte	0xd
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7a
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL59-.Ltext0
	.4byte	.LVL61-.Ltext0
	.2byte	0x8
	.byte	0x7a
	.sleb128 0
	.byte	0x7e
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL61-.Ltext0
	.4byte	.LVL65-.Ltext0
	.2byte	0xb
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7a
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL65-.Ltext0
	.4byte	.LVL67-.Ltext0
	.2byte	0x1
	.byte	0x5e
	.4byte	.LVL69-.Ltext0
	.4byte	.LVL71-.Ltext0
	.2byte	0x8
	.byte	0x7a
	.sleb128 0
	.byte	0x72
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL75-.Ltext0
	.4byte	.LVL76-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	0
	.4byte	0
.LVUS17:
	.uleb128 .LVU134
	.uleb128 .LVU149
	.uleb128 .LVU149
	.uleb128 .LVU152
	.uleb128 .LVU152
	.uleb128 .LVU169
	.uleb128 .LVU177
	.uleb128 .LVU188
	.uleb128 .LVU188
	.uleb128 .LVU189
	.uleb128 .LVU189
	.uleb128 .LVU197
	.uleb128 .LVU197
	.uleb128 .LVU214
	.uleb128 .LVU222
	.uleb128 .LVU232
	.uleb128 .LVU232
	.uleb128 .LVU234
	.uleb128 .LVU234
	.uleb128 .LVU236
	.uleb128 .LVU236
	.uleb128 .LVU245
	.uleb128 .LVU264
	.uleb128 .LVU274
	.uleb128 .LVU276
	.uleb128 .LVU278
.LLST17:
	.4byte	.LVL37-.Ltext0
	.4byte	.LVL39-.Ltext0
	.2byte	0x8
	.byte	0x76
	.sleb128 0
	.byte	0x79
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL39-.Ltext0
	.4byte	.LVL42-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	.LVL42-.Ltext0
	.4byte	.LVL46-.Ltext0
	.2byte	0xb
	.byte	0x72
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x79
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL49-.Ltext0
	.4byte	.LVL52-.Ltext0
	.2byte	0x8
	.byte	0x79
	.sleb128 0
	.byte	0x7e
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL52-.Ltext0
	.4byte	.LVL53-.Ltext0
	.2byte	0xd
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x79
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL53-.Ltext0
	.4byte	.LVL54-.Ltext0
	.2byte	0x1
	.byte	0x57
	.4byte	.LVL54-.Ltext0
	.4byte	.LVL57-.Ltext0
	.2byte	0xd
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x79
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL59-.Ltext0
	.4byte	.LVL61-.Ltext0
	.2byte	0x8
	.byte	0x79
	.sleb128 0
	.byte	0x7e
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL61-.Ltext0
	.4byte	.LVL62-.Ltext0
	.2byte	0xb
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x79
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL62-.Ltext0
	.4byte	.LVL64-.Ltext0
	.2byte	0x1
	.byte	0x56
	.4byte	.LVL64-.Ltext0
	.4byte	.LVL66-.Ltext0
	.2byte	0xb
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x79
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL69-.Ltext0
	.4byte	.LVL71-.Ltext0
	.2byte	0x8
	.byte	0x79
	.sleb128 0
	.byte	0x72
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL72-.Ltext0
	.4byte	.LVL74-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	0
	.4byte	0
.LVUS18:
	.uleb128 .LVU135
	.uleb128 .LVU147
	.uleb128 .LVU147
	.uleb128 .LVU150
	.uleb128 .LVU150
	.uleb128 .LVU151
	.uleb128 .LVU151
	.uleb128 .LVU169
	.uleb128 .LVU178
	.uleb128 .LVU186
	.uleb128 .LVU186
	.uleb128 .LVU187
	.uleb128 .LVU187
	.uleb128 .LVU188
	.uleb128 .LVU188
	.uleb128 .LVU214
	.uleb128 .LVU223
	.uleb128 .LVU231
	.uleb128 .LVU231
	.uleb128 .LVU235
	.uleb128 .LVU235
	.uleb128 .LVU245
	.uleb128 .LVU265
	.uleb128 .LVU273
	.uleb128 .LVU273
	.uleb128 .LVU277
.LLST18:
	.4byte	.LVL37-.Ltext0
	.4byte	.LVL38-.Ltext0
	.2byte	0x8
	.byte	0x76
	.sleb128 0
	.byte	0x7b
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL38-.Ltext0
	.4byte	.LVL40-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	.LVL40-.Ltext0
	.4byte	.LVL41-.Ltext0
	.2byte	0x8
	.byte	0x76
	.sleb128 0
	.byte	0x7b
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL41-.Ltext0
	.4byte	.LVL46-.Ltext0
	.2byte	0xb
	.byte	0x72
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7b
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL49-.Ltext0
	.4byte	.LVL50-.Ltext0
	.2byte	0x8
	.byte	0x7b
	.sleb128 0
	.byte	0x7e
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL50-.Ltext0
	.4byte	.LVL51-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	.LVL51-.Ltext0
	.4byte	.LVL52-.Ltext0
	.2byte	0x8
	.byte	0x7b
	.sleb128 0
	.byte	0x7e
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL52-.Ltext0
	.4byte	.LVL57-.Ltext0
	.2byte	0xd
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7b
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL59-.Ltext0
	.4byte	.LVL60-.Ltext0
	.2byte	0x8
	.byte	0x7b
	.sleb128 0
	.byte	0x7e
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL60-.Ltext0
	.4byte	.LVL63-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	.LVL63-.Ltext0
	.4byte	.LVL66-.Ltext0
	.2byte	0xb
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7b
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL69-.Ltext0
	.4byte	.LVL70-.Ltext0
	.2byte	0x8
	.byte	0x7b
	.sleb128 0
	.byte	0x72
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL70-.Ltext0
	.4byte	.LVL73-.Ltext0
	.2byte	0x1
	.byte	0x5e
	.4byte	0
	.4byte	0
.LVUS19:
	.uleb128 .LVU114
	.uleb128 .LVU119
	.uleb128 .LVU119
	.uleb128 .LVU125
	.uleb128 .LVU126
	.uleb128 .LVU300
.LLST19:
	.4byte	.LVL29-.Ltext0
	.4byte	.LVL30-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL30-.Ltext0
	.4byte	.LVL34-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	.LVL35-.Ltext0
	.4byte	.LVL77-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	0
	.4byte	0
.LVUS20:
	.uleb128 .LVU140
	.uleb128 .LVU169
.LLST20:
	.4byte	.LVL37-.Ltext0
	.4byte	.LVL46-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	0
	.4byte	0
.LVUS21:
	.uleb128 .LVU113
	.uleb128 .LVU300
.LLST21:
	.4byte	.LVL29-.Ltext0
	.4byte	.LVL77-.Ltext0
	.2byte	0x1
	.byte	0x5c
	.4byte	0
	.4byte	0
.LVUS22:
	.uleb128 .LVU126
	.uleb128 .LVU169
	.uleb128 .LVU169
	.uleb128 .LVU214
	.uleb128 .LVU214
	.uleb128 .LVU258
	.uleb128 .LVU258
	.uleb128 .LVU300
.LLST22:
	.4byte	.LVL35-.Ltext0
	.4byte	.LVL46-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL46-.Ltext0
	.4byte	.LVL57-.Ltext0
	.2byte	0x2
	.byte	0x31
	.byte	0x9f
	.4byte	.LVL57-.Ltext0
	.4byte	.LVL68-.Ltext0
	.2byte	0x2
	.byte	0x32
	.byte	0x9f
	.4byte	.LVL68-.Ltext0
	.4byte	.LVL77-.Ltext0
	.2byte	0x2
	.byte	0x33
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS23:
	.uleb128 .LVU136
	.uleb128 .LVU151
	.uleb128 .LVU151
	.uleb128 .LVU169
	.uleb128 .LVU179
	.uleb128 .LVU188
	.uleb128 .LVU188
	.uleb128 .LVU214
	.uleb128 .LVU224
	.uleb128 .LVU232
	.uleb128 .LVU232
	.uleb128 .LVU245
	.uleb128 .LVU266
	.uleb128 .LVU274
.LLST23:
	.4byte	.LVL37-.Ltext0
	.4byte	.LVL41-.Ltext0
	.2byte	0x13
	.byte	0x7a
	.sleb128 0
	.byte	0x76
	.sleb128 0
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
	.4byte	.LVL41-.Ltext0
	.4byte	.LVL46-.Ltext0
	.2byte	0x16
	.byte	0x72
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7a
	.sleb128 0
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
	.4byte	.LVL49-.Ltext0
	.4byte	.LVL52-.Ltext0
	.2byte	0x13
	.byte	0x7a
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.4byte	.LVL52-.Ltext0
	.4byte	.LVL57-.Ltext0
	.2byte	0x18
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7a
	.sleb128 0
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
	.4byte	.LVL59-.Ltext0
	.4byte	.LVL61-.Ltext0
	.2byte	0x13
	.byte	0x7a
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.4byte	.LVL61-.Ltext0
	.4byte	.LVL66-.Ltext0
	.2byte	0x16
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7a
	.sleb128 0
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
	.4byte	.LVL69-.Ltext0
	.4byte	.LVL71-.Ltext0
	.2byte	0x13
	.byte	0x7a
	.sleb128 0
	.byte	0x72
	.sleb128 0
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
	.uleb128 .LVU137
	.uleb128 .LVU149
	.uleb128 .LVU149
	.uleb128 .LVU152
	.uleb128 .LVU152
	.uleb128 .LVU169
	.uleb128 .LVU180
	.uleb128 .LVU188
	.uleb128 .LVU188
	.uleb128 .LVU189
	.uleb128 .LVU189
	.uleb128 .LVU197
	.uleb128 .LVU197
	.uleb128 .LVU214
	.uleb128 .LVU225
	.uleb128 .LVU232
	.uleb128 .LVU232
	.uleb128 .LVU234
	.uleb128 .LVU234
	.uleb128 .LVU236
	.uleb128 .LVU236
	.uleb128 .LVU245
	.uleb128 .LVU267
	.uleb128 .LVU274
	.uleb128 .LVU276
	.uleb128 .LVU278
.LLST24:
	.4byte	.LVL37-.Ltext0
	.4byte	.LVL39-.Ltext0
	.2byte	0x13
	.byte	0x76
	.sleb128 0
	.byte	0x79
	.sleb128 0
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
	.4byte	.LVL39-.Ltext0
	.4byte	.LVL42-.Ltext0
	.2byte	0xe
	.byte	0x78
	.sleb128 0
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
	.4byte	.LVL42-.Ltext0
	.4byte	.LVL46-.Ltext0
	.2byte	0x16
	.byte	0x72
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x79
	.sleb128 0
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
	.4byte	.LVL49-.Ltext0
	.4byte	.LVL52-.Ltext0
	.2byte	0x13
	.byte	0x79
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.4byte	.LVL52-.Ltext0
	.4byte	.LVL53-.Ltext0
	.2byte	0x18
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x79
	.sleb128 0
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
	.4byte	.LVL53-.Ltext0
	.4byte	.LVL54-.Ltext0
	.2byte	0xe
	.byte	0x77
	.sleb128 0
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
	.4byte	.LVL54-.Ltext0
	.4byte	.LVL57-.Ltext0
	.2byte	0x18
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x79
	.sleb128 0
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
	.4byte	.LVL59-.Ltext0
	.4byte	.LVL61-.Ltext0
	.2byte	0x13
	.byte	0x79
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.4byte	.LVL61-.Ltext0
	.4byte	.LVL62-.Ltext0
	.2byte	0x16
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x79
	.sleb128 0
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
	.4byte	.LVL62-.Ltext0
	.4byte	.LVL64-.Ltext0
	.2byte	0xe
	.byte	0x76
	.sleb128 0
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
	.4byte	.LVL64-.Ltext0
	.4byte	.LVL66-.Ltext0
	.2byte	0x16
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x79
	.sleb128 0
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
	.4byte	.LVL69-.Ltext0
	.4byte	.LVL71-.Ltext0
	.2byte	0x13
	.byte	0x79
	.sleb128 0
	.byte	0x72
	.sleb128 0
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
	.4byte	.LVL72-.Ltext0
	.4byte	.LVL74-.Ltext0
	.2byte	0xe
	.byte	0x74
	.sleb128 0
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
	.uleb128 .LVU138
	.uleb128 .LVU147
	.uleb128 .LVU147
	.uleb128 .LVU150
	.uleb128 .LVU150
	.uleb128 .LVU151
	.uleb128 .LVU151
	.uleb128 .LVU169
	.uleb128 .LVU181
	.uleb128 .LVU186
	.uleb128 .LVU186
	.uleb128 .LVU187
	.uleb128 .LVU187
	.uleb128 .LVU188
	.uleb128 .LVU188
	.uleb128 .LVU214
	.uleb128 .LVU226
	.uleb128 .LVU231
	.uleb128 .LVU231
	.uleb128 .LVU235
	.uleb128 .LVU235
	.uleb128 .LVU245
	.uleb128 .LVU268
	.uleb128 .LVU273
	.uleb128 .LVU273
	.uleb128 .LVU277
.LLST25:
	.4byte	.LVL37-.Ltext0
	.4byte	.LVL38-.Ltext0
	.2byte	0x13
	.byte	0x76
	.sleb128 0
	.byte	0x7b
	.sleb128 0
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
	.4byte	.LVL38-.Ltext0
	.4byte	.LVL40-.Ltext0
	.2byte	0xe
	.byte	0x74
	.sleb128 0
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
	.4byte	.LVL40-.Ltext0
	.4byte	.LVL41-.Ltext0
	.2byte	0x13
	.byte	0x76
	.sleb128 0
	.byte	0x7b
	.sleb128 0
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
	.4byte	.LVL41-.Ltext0
	.4byte	.LVL46-.Ltext0
	.2byte	0x16
	.byte	0x72
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7b
	.sleb128 0
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
	.4byte	.LVL49-.Ltext0
	.4byte	.LVL50-.Ltext0
	.2byte	0x13
	.byte	0x7b
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.4byte	.LVL50-.Ltext0
	.4byte	.LVL51-.Ltext0
	.2byte	0xe
	.byte	0x74
	.sleb128 0
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
	.4byte	.LVL51-.Ltext0
	.4byte	.LVL52-.Ltext0
	.2byte	0x13
	.byte	0x7b
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.4byte	.LVL52-.Ltext0
	.4byte	.LVL57-.Ltext0
	.2byte	0x18
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7b
	.sleb128 0
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
	.4byte	.LVL59-.Ltext0
	.4byte	.LVL60-.Ltext0
	.2byte	0x13
	.byte	0x7b
	.sleb128 0
	.byte	0x7e
	.sleb128 0
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
	.4byte	.LVL60-.Ltext0
	.4byte	.LVL63-.Ltext0
	.2byte	0xe
	.byte	0x74
	.sleb128 0
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
	.4byte	.LVL63-.Ltext0
	.4byte	.LVL66-.Ltext0
	.2byte	0x16
	.byte	0x78
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x7b
	.sleb128 0
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
	.4byte	.LVL69-.Ltext0
	.4byte	.LVL70-.Ltext0
	.2byte	0x13
	.byte	0x7b
	.sleb128 0
	.byte	0x72
	.sleb128 0
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
	.4byte	.LVL70-.Ltext0
	.4byte	.LVL73-.Ltext0
	.2byte	0xe
	.byte	0x7e
	.sleb128 0
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
	.uleb128 .LVU127
	.uleb128 .LVU170
	.uleb128 .LVU170
	.uleb128 .LVU172
	.uleb128 .LVU215
	.uleb128 .LVU245
	.uleb128 .LVU259
	.uleb128 .LVU274
.LLST26:
	.4byte	.LVL35-.Ltext0
	.4byte	.LVL46-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	.LVL46-.Ltext0
	.4byte	.LVL47-.Ltext0
	.2byte	0x1
	.byte	0x5e
	.4byte	.LVL57-.Ltext0
	.4byte	.LVL66-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	.LVL68-.Ltext0
	.4byte	.LVL71-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	0
	.4byte	0
.LVUS27:
	.uleb128 .LVU108
	.uleb128 .LVU121
	.uleb128 .LVU121
	.uleb128 .LVU124
	.uleb128 .LVU124
	.uleb128 .LVU126
	.uleb128 .LVU126
	.uleb128 .LVU300
	.uleb128 .LVU300
	.uleb128 .LVU301
	.uleb128 .LVU301
	.uleb128 .LVU303
.LLST27:
	.4byte	.LVL28-.Ltext0
	.4byte	.LVL31-.Ltext0
	.2byte	0x1
	.byte	0x51
	.4byte	.LVL31-.Ltext0
	.4byte	.LVL33-.Ltext0
	.2byte	0x3
	.byte	0x71
	.sleb128 -4
	.byte	0x9f
	.4byte	.LVL33-.Ltext0
	.4byte	.LVL35-.Ltext0
	.2byte	0x1
	.byte	0x51
	.4byte	.LVL35-.Ltext0
	.4byte	.LVL77-.Ltext0
	.2byte	0x3
	.byte	0x71
	.sleb128 -4
	.byte	0x9f
	.4byte	.LVL77-.Ltext0
	.4byte	.LVL78-.Ltext0
	.2byte	0x1
	.byte	0x51
	.4byte	.LVL78-.Ltext0
	.4byte	.LVL80-1-.Ltext0
	.2byte	0x5
	.byte	0x3
	.4byte	g_armwave_state+4
	.4byte	0
	.4byte	0
.LVUS28:
	.uleb128 .LVU143
	.uleb128 .LVU169
.LLST28:
	.4byte	.LVL37-.Ltext0
	.4byte	.LVL46-.Ltext0
	.2byte	0x12
	.byte	0x73
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x3
	.4byte	g_armwave_state+72
	.byte	0x6
	.byte	0x1e
	.byte	0x73
	.sleb128 0
	.byte	0x38
	.byte	0x26
	.byte	0x22
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS1:
	.uleb128 0
	.uleb128 .LVU36
	.uleb128 .LVU36
	.uleb128 .LVU47
	.uleb128 .LVU47
	.uleb128 0
.LLST1:
	.4byte	.LVL6-.Ltext0
	.4byte	.LVL7-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL7-.Ltext0
	.4byte	.LVL11-.Ltext0
	.2byte	0x1
	.byte	0x57
	.4byte	.LVL11-.Ltext0
	.4byte	.LFE59-.Ltext0
	.2byte	0x3
	.byte	0x77
	.sleb128 4
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS2:
	.uleb128 0
	.uleb128 .LVU37
	.uleb128 .LVU37
	.uleb128 0
.LLST2:
	.4byte	.LVL6-.Ltext0
	.4byte	.LVL8-.Ltext0
	.2byte	0x1
	.byte	0x51
	.4byte	.LVL8-.Ltext0
	.4byte	.LFE59-.Ltext0
	.2byte	0x1
	.byte	0x59
	.4byte	0
	.4byte	0
.LVUS3:
	.uleb128 .LVU55
	.uleb128 .LVU58
	.uleb128 .LVU58
	.uleb128 .LVU84
	.uleb128 .LVU84
	.uleb128 .LVU94
	.uleb128 .LVU94
	.uleb128 .LVU95
.LLST3:
	.4byte	.LVL14-.Ltext0
	.4byte	.LVL17-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL17-.Ltext0
	.4byte	.LVL22-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	.LVL22-.Ltext0
	.4byte	.LVL24-.Ltext0
	.2byte	0x3
	.byte	0x73
	.sleb128 -4
	.byte	0x9f
	.4byte	.LVL24-.Ltext0
	.4byte	.LVL25-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	0
	.4byte	0
.LVUS4:
	.uleb128 .LVU61
	.uleb128 .LVU69
	.uleb128 .LVU69
	.uleb128 .LVU79
	.uleb128 .LVU79
	.uleb128 .LVU88
	.uleb128 .LVU88
	.uleb128 .LVU94
	.uleb128 .LVU94
	.uleb128 .LVU95
.LLST4:
	.4byte	.LVL18-.Ltext0
	.4byte	.LVL19-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL19-.Ltext0
	.4byte	.LVL21-.Ltext0
	.2byte	0x2
	.byte	0x31
	.byte	0x9f
	.4byte	.LVL21-.Ltext0
	.4byte	.LVL23-.Ltext0
	.2byte	0x2
	.byte	0x32
	.byte	0x9f
	.4byte	.LVL23-.Ltext0
	.4byte	.LVL24-.Ltext0
	.2byte	0x2
	.byte	0x33
	.byte	0x9f
	.4byte	.LVL24-.Ltext0
	.4byte	.LVL25-.Ltext0
	.2byte	0x2
	.byte	0x34
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS5:
	.uleb128 .LVU45
	.uleb128 .LVU48
	.uleb128 .LVU48
	.uleb128 0
.LLST5:
	.4byte	.LVL10-.Ltext0
	.4byte	.LVL12-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL12-.Ltext0
	.4byte	.LFE59-.Ltext0
	.2byte	0x1
	.byte	0x5a
	.4byte	0
	.4byte	0
.LVUS6:
	.uleb128 .LVU62
	.uleb128 .LVU70
	.uleb128 .LVU70
	.uleb128 .LVU75
.LLST6:
	.4byte	.LVL18-.Ltext0
	.4byte	.LVL19-.Ltext0
	.2byte	0x6
	.byte	0x72
	.sleb128 0
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x9f
	.4byte	.LVL19-.Ltext0
	.4byte	.LVL20-.Ltext0
	.2byte	0x8
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x8
	.byte	0xff
	.byte	0x1a
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS7:
	.uleb128 .LVU60
	.uleb128 .LVU69
	.uleb128 .LVU69
	.uleb128 .LVU75
	.uleb128 .LVU94
	.uleb128 .LVU95
.LLST7:
	.4byte	.LVL18-.Ltext0
	.4byte	.LVL19-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	.LVL19-.Ltext0
	.4byte	.LVL20-.Ltext0
	.2byte	0x5
	.byte	0x72
	.sleb128 0
	.byte	0x38
	.byte	0x25
	.byte	0x9f
	.4byte	.LVL24-.Ltext0
	.4byte	.LVL25-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS8:
	.uleb128 .LVU54
	.uleb128 .LVU56
	.uleb128 .LVU56
	.uleb128 .LVU57
	.uleb128 .LVU57
	.uleb128 .LVU58
.LLST8:
	.4byte	.LVL14-.Ltext0
	.4byte	.LVL15-.Ltext0
	.2byte	0xb
	.byte	0x7c
	.sleb128 0
	.byte	0x73
	.sleb128 0
	.byte	0x22
	.byte	0x77
	.sleb128 0
	.byte	0x22
	.byte	0x23
	.uleb128 0x4
	.byte	0x9f
	.4byte	.LVL15-.Ltext0
	.4byte	.LVL16-.Ltext0
	.2byte	0x12
	.byte	0x3
	.4byte	g_armwave_state+36
	.byte	0x6
	.byte	0x7a
	.sleb128 0
	.byte	0x1e
	.byte	0x7c
	.sleb128 0
	.byte	0x22
	.byte	0x77
	.sleb128 0
	.byte	0x22
	.byte	0x23
	.uleb128 0x4
	.byte	0x9f
	.4byte	.LVL16-.Ltext0
	.4byte	.LVL17-.Ltext0
	.2byte	0x16
	.byte	0x3
	.4byte	g_armwave_state+36
	.byte	0x6
	.byte	0x7a
	.sleb128 0
	.byte	0x1e
	.byte	0x3
	.4byte	g_armwave_state+20
	.byte	0x6
	.byte	0x22
	.byte	0x77
	.sleb128 0
	.byte	0x22
	.byte	0x23
	.uleb128 0x4
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS9:
	.uleb128 .LVU44
	.uleb128 0
.LLST9:
	.4byte	.LVL10-.Ltext0
	.4byte	.LFE59-.Ltext0
	.2byte	0x6
	.byte	0x74
	.sleb128 0
	.byte	0x76
	.sleb128 0
	.byte	0x22
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS10:
	.uleb128 .LVU63
	.uleb128 .LVU68
	.uleb128 .LVU71
	.uleb128 .LVU78
	.uleb128 .LVU81
	.uleb128 .LVU84
	.uleb128 .LVU84
	.uleb128 .LVU87
	.uleb128 .LVU87
	.uleb128 .LVU90
.LLST10:
	.4byte	.LVL18-.Ltext0
	.4byte	.LVL19-.Ltext0
	.2byte	0x10
	.byte	0x3
	.4byte	g_armwave_state+48
	.byte	0x6
	.byte	0x73
	.sleb128 0
	.byte	0x1e
	.byte	0x74
	.sleb128 0
	.byte	0x22
	.byte	0x76
	.sleb128 0
	.byte	0x22
	.byte	0x9f
	.4byte	.LVL19-.Ltext0
	.4byte	.LVL21-.Ltext0
	.2byte	0x10
	.byte	0x73
	.sleb128 1
	.byte	0x3
	.4byte	g_armwave_state+48
	.byte	0x6
	.byte	0x1e
	.byte	0x74
	.sleb128 0
	.byte	0x22
	.byte	0x76
	.sleb128 0
	.byte	0x22
	.byte	0x9f
	.4byte	.LVL21-.Ltext0
	.4byte	.LVL22-.Ltext0
	.2byte	0x10
	.byte	0x73
	.sleb128 2
	.byte	0x3
	.4byte	g_armwave_state+48
	.byte	0x6
	.byte	0x1e
	.byte	0x74
	.sleb128 0
	.byte	0x22
	.byte	0x76
	.sleb128 0
	.byte	0x22
	.byte	0x9f
	.4byte	.LVL22-.Ltext0
	.4byte	.LVL23-.Ltext0
	.2byte	0x10
	.byte	0x73
	.sleb128 -2
	.byte	0x3
	.4byte	g_armwave_state+48
	.byte	0x6
	.byte	0x1e
	.byte	0x74
	.sleb128 0
	.byte	0x22
	.byte	0x76
	.sleb128 0
	.byte	0x22
	.byte	0x9f
	.4byte	.LVL23-.Ltext0
	.4byte	.LVL23-.Ltext0
	.2byte	0xc
	.byte	0x73
	.sleb128 -2
	.byte	0x7b
	.sleb128 0
	.byte	0x1e
	.byte	0x74
	.sleb128 0
	.byte	0x22
	.byte	0x76
	.sleb128 0
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
.LVUS29:
	.uleb128 0
	.uleb128 .LVU401
	.uleb128 .LVU401
	.uleb128 0
.LLST29:
	.4byte	.LVL81-.Ltext0
	.4byte	.LVL112-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL112-.Ltext0
	.4byte	.LFE61-.Ltext0
	.2byte	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x50
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS30:
	.uleb128 .LVU384
	.uleb128 .LVU400
.LLST30:
	.4byte	.LVL104-.Ltext0
	.4byte	.LVL111-.Ltext0
	.2byte	0x1
	.byte	0x57
	.4byte	0
	.4byte	0
.LVUS31:
	.uleb128 .LVU383
	.uleb128 .LVU400
.LLST31:
	.4byte	.LVL104-.Ltext0
	.4byte	.LVL111-.Ltext0
	.2byte	0x1
	.byte	0x5a
	.4byte	0
	.4byte	0
.LVUS32:
	.uleb128 .LVU379
	.uleb128 .LVU400
.LLST32:
	.4byte	.LVL102-.Ltext0
	.4byte	.LVL111-.Ltext0
	.2byte	0x1
	.byte	0x54
	.4byte	0
	.4byte	0
.LVUS33:
	.uleb128 .LVU329
	.uleb128 .LVU333
	.uleb128 .LVU334
	.uleb128 .LVU400
.LLST33:
	.4byte	.LVL85-.Ltext0
	.4byte	.LVL87-.Ltext0
	.2byte	0x1
	.byte	0x5c
	.4byte	.LVL88-.Ltext0
	.4byte	.LVL111-.Ltext0
	.2byte	0x1
	.byte	0x5c
	.4byte	0
	.4byte	0
.LVUS34:
	.uleb128 .LVU350
	.uleb128 .LVU362
	.uleb128 .LVU366
	.uleb128 .LVU369
.LLST34:
	.4byte	.LVL94-.Ltext0
	.4byte	.LVL96-.Ltext0
	.2byte	0x8
	.byte	0x7b
	.sleb128 0
	.byte	0x73
	.sleb128 0
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL100-.Ltext0
	.4byte	.LVL101-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	0
	.4byte	0
.LVUS35:
	.uleb128 .LVU351
	.uleb128 .LVU362
	.uleb128 .LVU363
	.uleb128 .LVU365
.LLST35:
	.4byte	.LVL94-.Ltext0
	.4byte	.LVL96-.Ltext0
	.2byte	0x9
	.byte	0x73
	.sleb128 0
	.byte	0x7d
	.sleb128 0
	.byte	0x6
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL97-.Ltext0
	.4byte	.LVL99-.Ltext0
	.2byte	0x1
	.byte	0x51
	.4byte	0
	.4byte	0
.LVUS36:
	.uleb128 .LVU352
	.uleb128 .LVU361
	.uleb128 .LVU361
	.uleb128 .LVU364
.LLST36:
	.4byte	.LVL94-.Ltext0
	.4byte	.LVL95-.Ltext0
	.2byte	0x9
	.byte	0x73
	.sleb128 0
	.byte	0x91
	.sleb128 -52
	.byte	0x6
	.byte	0x1e
	.byte	0x38
	.byte	0x26
	.byte	0x9f
	.4byte	.LVL95-.Ltext0
	.4byte	.LVL98-.Ltext0
	.2byte	0x1
	.byte	0x52
	.4byte	0
	.4byte	0
.LVUS37:
	.uleb128 .LVU321
	.uleb128 .LVU327
	.uleb128 .LVU327
	.uleb128 .LVU333
	.uleb128 .LVU334
	.uleb128 .LVU400
.LLST37:
	.4byte	.LVL83-.Ltext0
	.4byte	.LVL84-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL84-.Ltext0
	.4byte	.LVL87-.Ltext0
	.2byte	0x1
	.byte	0x5e
	.4byte	.LVL88-.Ltext0
	.4byte	.LVL111-.Ltext0
	.2byte	0x1
	.byte	0x5e
	.4byte	0
	.4byte	0
.LVUS38:
	.uleb128 .LVU380
	.uleb128 .LVU382
	.uleb128 .LVU382
	.uleb128 .LVU400
.LLST38:
	.4byte	.LVL102-.Ltext0
	.4byte	.LVL103-.Ltext0
	.2byte	0x1
	.byte	0x57
	.4byte	.LVL103-.Ltext0
	.4byte	.LVL111-.Ltext0
	.2byte	0x6
	.byte	0x7e
	.sleb128 0
	.byte	0x79
	.sleb128 0
	.byte	0x22
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS39:
	.uleb128 .LVU319
	.uleb128 .LVU400
.LLST39:
	.4byte	.LVL83-.Ltext0
	.4byte	.LVL111-.Ltext0
	.2byte	0x1
	.byte	0x58
	.4byte	0
	.4byte	0
.LVUS40:
	.uleb128 .LVU336
	.uleb128 .LVU400
.LLST40:
	.4byte	.LVL89-.Ltext0
	.4byte	.LVL111-.Ltext0
	.2byte	0x1
	.byte	0x59
	.4byte	0
	.4byte	0
.LVUS41:
	.uleb128 .LVU320
	.uleb128 .LVU327
.LLST41:
	.4byte	.LVL83-.Ltext0
	.4byte	.LVL84-.Ltext0
	.2byte	0x9
	.byte	0x3
	.4byte	g_armwave_state+76
	.byte	0x6
	.byte	0x38
	.byte	0x25
	.byte	0x9f
	.4byte	0
	.4byte	0
.LVUS42:
	.uleb128 .LVU353
	.uleb128 .LVU362
.LLST42:
	.4byte	.LVL94-.Ltext0
	.4byte	.LVL96-.Ltext0
	.2byte	0x13
	.byte	0x7b
	.sleb128 0
	.byte	0x73
	.sleb128 0
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
.LVUS43:
	.uleb128 .LVU354
	.uleb128 .LVU362
.LLST43:
	.4byte	.LVL94-.Ltext0
	.4byte	.LVL96-.Ltext0
	.2byte	0x14
	.byte	0x73
	.sleb128 0
	.byte	0x7d
	.sleb128 0
	.byte	0x6
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
.LVUS44:
	.uleb128 .LVU355
	.uleb128 .LVU362
.LLST44:
	.4byte	.LVL94-.Ltext0
	.4byte	.LVL96-.Ltext0
	.2byte	0x14
	.byte	0x73
	.sleb128 0
	.byte	0x91
	.sleb128 -52
	.byte	0x6
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
.LVUS45:
	.uleb128 .LVU337
	.uleb128 .LVU339
	.uleb128 .LVU344
	.uleb128 .LVU347
.LLST45:
	.4byte	.LVL89-.Ltext0
	.4byte	.LVL90-.Ltext0
	.2byte	0x1
	.byte	0x5c
	.4byte	.LVL92-.Ltext0
	.4byte	.LVL93-.Ltext0
	.2byte	0x1
	.byte	0x5c
	.4byte	0
	.4byte	0
.LVUS46:
	.uleb128 .LVU385
	.uleb128 .LVU388
	.uleb128 .LVU388
	.uleb128 .LVU393
	.uleb128 .LVU395
	.uleb128 .LVU400
.LLST46:
	.4byte	.LVL104-.Ltext0
	.4byte	.LVL105-.Ltext0
	.2byte	0x2
	.byte	0x30
	.byte	0x9f
	.4byte	.LVL105-.Ltext0
	.4byte	.LVL107-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	.LVL109-.Ltext0
	.4byte	.LVL111-.Ltext0
	.2byte	0x1
	.byte	0x53
	.4byte	0
	.4byte	0
.LVUS47:
	.uleb128 .LVU314
	.uleb128 .LVU329
	.uleb128 .LVU329
	.uleb128 .LVU332
	.uleb128 .LVU332
	.uleb128 .LVU334
	.uleb128 .LVU334
	.uleb128 .LVU400
	.uleb128 .LVU400
	.uleb128 0
.LLST47:
	.4byte	.LVL82-.Ltext0
	.4byte	.LVL85-.Ltext0
	.2byte	0x1
	.byte	0x56
	.4byte	.LVL85-.Ltext0
	.4byte	.LVL86-.Ltext0
	.2byte	0x3
	.byte	0x76
	.sleb128 -4
	.byte	0x9f
	.4byte	.LVL86-.Ltext0
	.4byte	.LVL88-.Ltext0
	.2byte	0x1
	.byte	0x56
	.4byte	.LVL88-.Ltext0
	.4byte	.LVL111-.Ltext0
	.2byte	0x3
	.byte	0x76
	.sleb128 -4
	.byte	0x9f
	.4byte	.LVL111-.Ltext0
	.4byte	.LFE61-.Ltext0
	.2byte	0x1
	.byte	0x56
	.4byte	0
	.4byte	0
.LVUS48:
	.uleb128 .LVU389
	.uleb128 .LVU392
	.uleb128 .LVU394
	.uleb128 .LVU398
.LLST48:
	.4byte	.LVL105-.Ltext0
	.4byte	.LVL106-.Ltext0
	.2byte	0x10
	.byte	0x72
	.sleb128 0
	.byte	0x7a
	.sleb128 0
	.byte	0x22
	.byte	0x3
	.4byte	g_armwave_state+72
	.byte	0x6
	.byte	0x1e
	.byte	0x77
	.sleb128 0
	.byte	0x22
	.byte	0x9f
	.4byte	.LVL108-.Ltext0
	.4byte	.LVL110-.Ltext0
	.2byte	0x1
	.byte	0x51
	.4byte	0
	.4byte	0
.LVUS59:
	.uleb128 0
	.uleb128 .LVU548
	.uleb128 .LVU548
	.uleb128 0
.LLST59:
	.4byte	.LVL145-.Ltext0
	.4byte	.LVL146-.Ltext0
	.2byte	0x1
	.byte	0x50
	.4byte	.LVL146-.Ltext0
	.4byte	.LFE67-.Ltext0
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
	.4byte	.LBB14-.Ltext0
	.4byte	.LBE14-.Ltext0
	.4byte	0
	.4byte	0
	.4byte	.LBB15-.Ltext0
	.4byte	.LBE15-.Ltext0
	.4byte	.LBB20-.Ltext0
	.4byte	.LBE20-.Ltext0
	.4byte	.LBB21-.Ltext0
	.4byte	.LBE21-.Ltext0
	.4byte	.LBB22-.Ltext0
	.4byte	.LBE22-.Ltext0
	.4byte	0
	.4byte	0
	.4byte	.LBB25-.Ltext0
	.4byte	.LBE25-.Ltext0
	.4byte	.LBB28-.Ltext0
	.4byte	.LBE28-.Ltext0
	.4byte	0
	.4byte	0
	.4byte	.LBB31-.Ltext0
	.4byte	.LBE31-.Ltext0
	.4byte	.LBB35-.Ltext0
	.4byte	.LBE35-.Ltext0
	.4byte	.LBB36-.Ltext0
	.4byte	.LBE36-.Ltext0
	.4byte	0
	.4byte	0
	.4byte	.LBB37-.Ltext0
	.4byte	.LBE37-.Ltext0
	.4byte	.LBB40-.Ltext0
	.4byte	.LBE40-.Ltext0
	.4byte	0
	.4byte	0
	.4byte	.LBB43-.Ltext0
	.4byte	.LBE43-.Ltext0
	.4byte	.LBB49-.Ltext0
	.4byte	.LBE49-.Ltext0
	.4byte	.LBB50-.Ltext0
	.4byte	.LBE50-.Ltext0
	.4byte	.LBB51-.Ltext0
	.4byte	.LBE51-.Ltext0
	.4byte	.LBB52-.Ltext0
	.4byte	.LBE52-.Ltext0
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
.LASF585:
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
.LASF625:
	.ascii	"fwrite\000"
.LASF254:
	.ascii	"am_anext\000"
.LASF600:
	.ascii	"out_buffer_base\000"
.LASF368:
	.ascii	"PyCell_Type\000"
.LASF462:
	.ascii	"PyExc_IndentationError\000"
.LASF239:
	.ascii	"sq_repeat\000"
.LASF66:
	.ascii	"__environ\000"
.LASF632:
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
.LASF542:
	.ascii	"slice_record_height\000"
.LASF508:
	.ascii	"PyOS_InputHook\000"
.LASF374:
	.ascii	"next\000"
.LASF491:
	.ascii	"PyExc_IOError\000"
.LASF265:
	.ascii	"PyGetSetDef\000"
.LASF615:
	.ascii	"calloc\000"
.LASF142:
	.ascii	"tp_bases\000"
.LASF17:
	.ascii	"__off_t\000"
.LASF463:
	.ascii	"PyExc_TabError\000"
.LASF425:
	.ascii	"PyMethodDescr_Type\000"
.LASF534:
	.ascii	"test_wave_buffer\000"
.LASF286:
	.ascii	"Py_OptimizeFlag\000"
.LASF605:
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
.LASF541:
	.ascii	"slice_height\000"
.LASF408:
	.ascii	"exc_value\000"
.LASF602:
	.ascii	"armwave_fill_pixbuf_256\000"
.LASF89:
	.ascii	"TRACEMALLOC_NOT_INITIALIZED\000"
.LASF298:
	.ascii	"_PyByteArray_empty_string\000"
.LASF101:
	.ascii	"_typeobject\000"
.LASF229:
	.ascii	"nb_floor_divide\000"
.LASF574:
	.ascii	"render_width\000"
.LASF224:
	.ascii	"nb_inplace_lshift\000"
.LASF578:
	.ascii	"data\000"
.LASF480:
	.ascii	"PyExc_ConnectionRefusedError\000"
.LASF593:
	.ascii	"ysub\000"
.LASF479:
	.ascii	"PyExc_ConnectionAbortedError\000"
.LASF449:
	.ascii	"PyExc_OSError\000"
.LASF564:
	.ascii	"xnoise\000"
.LASF292:
	.ascii	"Py_NoUserSiteDirectory\000"
.LASF478:
	.ascii	"PyExc_ConnectionError\000"
.LASF476:
	.ascii	"PyExc_BrokenPipeError\000"
.LASF492:
	.ascii	"PyExc_Warning\000"
.LASF561:
	.ascii	"new_level\000"
.LASF233:
	.ascii	"nb_index\000"
.LASF125:
	.ascii	"tp_richcompare\000"
.LASF511:
	.ascii	"_Py_CheckRecursionLimit\000"
.LASF472:
	.ascii	"PyExc_UnicodeTranslateError\000"
.LASF552:
	.ascii	"ch3_color\000"
.LASF440:
	.ascii	"PyExc_StopIteration\000"
.LASF477:
	.ascii	"PyExc_ChildProcessError\000"
.LASF622:
	.ascii	"__builtin_memset\000"
.LASF28:
	.ascii	"_IO_write_end\000"
.LASF372:
	.ascii	"PyThreadState\000"
.LASF203:
	.ascii	"nb_remainder\000"
.LASF628:
	.ascii	"fclose\000"
.LASF164:
	.ascii	"visitproc\000"
.LASF355:
	.ascii	"PyMethod_Type\000"
.LASF592:
	.ascii	"out_buffer\000"
.LASF313:
	.ascii	"_Py_TrueStruct\000"
.LASF221:
	.ascii	"nb_inplace_multiply\000"
.LASF512:
	.ascii	"_inittab\000"
.LASF612:
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
.LASF545:
	.ascii	"target_width\000"
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
.LASF566:
	.ascii	"buf_obj\000"
.LASF154:
	.ascii	"PyVarObject\000"
.LASF319:
	.ascii	"_PyManagedBuffer_Type\000"
.LASF90:
	.ascii	"TRACEMALLOC_INITIALIZED\000"
.LASF384:
	.ascii	"c_profileobj\000"
.LASF543:
	.ascii	"wave_length\000"
.LASF544:
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
.LASF573:
	.ascii	"nwaves\000"
.LASF575:
	.ascii	"render_height\000"
.LASF527:
	.ascii	"armwave_state_t\000"
.LASF269:
	.ascii	"_PyNone_Type\000"
.LASF392:
	.ascii	"gilstate_counter\000"
.LASF60:
	.ascii	"sys_errlist\000"
.LASF556:
	.ascii	"gamma_table\000"
.LASF117:
	.ascii	"tp_str\000"
.LASF547:
	.ascii	"row_shift\000"
.LASF565:
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
.LASF586:
	.ascii	"end_point\000"
.LASF35:
	.ascii	"_chain\000"
.LASF109:
	.ascii	"tp_setattr\000"
.LASF507:
	.ascii	"PyCode_Type\000"
.LASF629:
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
.LASF589:
	.ascii	"points_per_pixel\000"
.LASF335:
	.ascii	"PyDictRevIterItem_Type\000"
.LASF93:
	.ascii	"initialized\000"
.LASF633:
	.ascii	"_IO_lock_t\000"
.LASF468:
	.ascii	"PyExc_UnboundLocalError\000"
.LASF0:
	.ascii	"float\000"
.LASF136:
	.ascii	"tp_dictoffset\000"
.LASF611:
	.ascii	"gamma\000"
.LASF441:
	.ascii	"PyExc_GeneratorExit\000"
.LASF236:
	.ascii	"PyNumberMethods\000"
.LASF259:
	.ascii	"PyMethodDef\000"
.LASF149:
	.ascii	"tp_finalize\000"
.LASF422:
	.ascii	"PyClassMethodDescr_Type\000"
.LASF346:
	.ascii	"PyFrozenSet_Type\000"
.LASF617:
	.ascii	"PyBuffer_Release\000"
.LASF249:
	.ascii	"mp_subscript\000"
.LASF124:
	.ascii	"tp_clear\000"
.LASF283:
	.ascii	"Py_QuietFlag\000"
.LASF428:
	.ascii	"_PyMethodWrapper_Type\000"
.LASF331:
	.ascii	"PyDictIterKey_Type\000"
.LASF526:
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
.LASF621:
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
.LASF590:
	.ascii	"armwave_set_channel_colour\000"
.LASF598:
	.ascii	"value\000"
.LASF366:
	.ascii	"PySlice_Type\000"
.LASF460:
	.ascii	"PyExc_NotImplementedError\000"
.LASF156:
	.ascii	"binaryfunc\000"
.LASF424:
	.ascii	"PyMemberDescr_Type\000"
.LASF535:
	.ascii	"xstride\000"
.LASF518:
	.ascii	"size\000"
.LASF51:
	.ascii	"FILE\000"
.LASF256:
	.ascii	"bf_getbuffer\000"
.LASF199:
	.ascii	"vectorcallfunc\000"
.LASF554:
	.ascii	"xcoord_to_xpixel\000"
.LASF175:
	.ascii	"getiterfunc\000"
.LASF414:
	.ascii	"_PyCoroWrapper_Type\000"
.LASF524:
	.ascii	"_Py_ctype_tolower\000"
.LASF160:
	.ascii	"ssizeargfunc\000"
.LASF618:
	.ascii	"__assert_fail\000"
.LASF595:
	.ascii	"wave_word\000"
.LASF553:
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
.LASF558:
	.ascii	"armwave_test_create_square\000"
.LASF426:
	.ascii	"PyWrapperDescr_Type\000"
.LASF582:
	.ascii	"armwave_set_wave_pointer\000"
.LASF177:
	.ascii	"descrgetfunc\000"
.LASF584:
	.ascii	"armwave_setup_render\000"
.LASF279:
	.ascii	"_Py_HashSecret_t\000"
.LASF407:
	.ascii	"exc_type\000"
.LASF219:
	.ascii	"nb_inplace_add\000"
.LASF217:
	.ascii	"nb_reserved\000"
.LASF596:
	.ascii	"nsub\000"
.LASF397:
	.ascii	"on_delete\000"
.LASF638:
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
.LASF533:
	.ascii	"wave_buffer\000"
.LASF388:
	.ascii	"curexc_traceback\000"
.LASF281:
	.ascii	"Py_DebugFlag\000"
.LASF568:
	.ascii	"armwave_test_dump_buffer_to_ppm\000"
.LASF517:
	.ascii	"code\000"
.LASF45:
	.ascii	"_wide_data\000"
.LASF529:
	.ascii	"ch1_buffer\000"
.LASF520:
	.ascii	"PyFilter_Type\000"
.LASF433:
	.ascii	"PyStructSequence_UnnamedField\000"
.LASF562:
	.ascii	"armwave_test_create_am_sine\000"
.LASF456:
	.ascii	"PyExc_NameError\000"
.LASF97:
	.ascii	"_Py_tracemalloc_config\000"
.LASF378:
	.ascii	"overflowed\000"
.LASF576:
	.ascii	"armwave_dump_ppm_debug\000"
.LASF78:
	.ascii	"signgam\000"
.LASF327:
	.ascii	"PyDict_Type\000"
.LASF530:
	.ascii	"ch2_buffer\000"
.LASF77:
	.ascii	"Py_hash_t\000"
.LASF14:
	.ascii	"__uint64_t\000"
.LASF610:
	.ascii	"armwave_init\000"
.LASF152:
	.ascii	"PyObject\000"
.LASF214:
	.ascii	"nb_xor\000"
.LASF502:
	.ascii	"PyExc_ResourceWarning\000"
.LASF206:
	.ascii	"nb_negative\000"
.LASF623:
	.ascii	"fopen64\000"
.LASF361:
	.ascii	"PyStdPrinter_Type\000"
.LASF92:
	.ascii	"_PyTraceMalloc_Config\000"
.LASF519:
	.ascii	"PyImport_FrozenModules\000"
.LASF627:
	.ascii	"fprintf\000"
.LASF531:
	.ascii	"ch3_buffer\000"
.LASF417:
	.ascii	"_PyAsyncGenASend_Type\000"
.LASF20:
	.ascii	"__ssize_t\000"
.LASF538:
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
.LASF616:
	.ascii	"PyObject_GetBuffer\000"
.LASF557:
	.ascii	"armwave_cleanup\000"
.LASF371:
	.ascii	"PyCmpWrapper_Type\000"
.LASF262:
	.ascii	"ml_flags\000"
.LASF139:
	.ascii	"tp_new\000"
.LASF353:
	.ascii	"PyClassMethod_Type\000"
.LASF635:
	.ascii	"armwave_test_buffer_alloc\000"
.LASF232:
	.ascii	"nb_inplace_true_divide\000"
.LASF411:
	.ascii	"_PyErr_StackItem\000"
.LASF167:
	.ascii	"destructor\000"
.LASF349:
	.ascii	"PyCFunction\000"
.LASF569:
	.ascii	"armwave_test_fill_outbuf\000"
.LASF620:
	.ascii	"memset\000"
.LASF560:
	.ascii	"level\000"
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
.LASF619:
	.ascii	"puts\000"
.LASF537:
	.ascii	"wave_stride\000"
.LASF287:
	.ascii	"Py_NoSiteFlag\000"
.LASF583:
	.ascii	"__PRETTY_FUNCTION__\000"
.LASF382:
	.ascii	"c_profilefunc\000"
.LASF113:
	.ascii	"tp_as_sequence\000"
.LASF120:
	.ascii	"tp_as_buffer\000"
.LASF188:
	.ascii	"itemsize\000"
.LASF599:
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
.LASF571:
	.ascii	"armwave_test_init\000"
.LASF387:
	.ascii	"curexc_value\000"
.LASF342:
	.ascii	"PyEnum_Type\000"
.LASF138:
	.ascii	"tp_alloc\000"
.LASF194:
	.ascii	"suboffsets\000"
.LASF489:
	.ascii	"PyExc_TimeoutError\000"
.LASF567:
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
.LASF539:
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
.LASF630:
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
.LASF536:
	.ascii	"vscale\000"
.LASF18:
	.ascii	"long int\000"
.LASF215:
	.ascii	"nb_or\000"
.LASF631:
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
.LASF546:
	.ascii	"target_height\000"
.LASF452:
	.ascii	"PyExc_IndexError\000"
.LASF369:
	.ascii	"PySeqIter_Type\000"
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
.LASF247:
	.ascii	"PySequenceMethods\000"
.LASF56:
	.ascii	"stdin\000"
.LASF398:
	.ascii	"on_delete_data\000"
.LASF146:
	.ascii	"tp_weaklist\000"
.LASF626:
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
.LASF563:
	.ascii	"noise_fraction\000"
.LASF54:
	.ascii	"_IO_wide_data\000"
.LASF550:
	.ascii	"ch1_color\000"
.LASF486:
	.ascii	"PyExc_NotADirectoryError\000"
.LASF494:
	.ascii	"PyExc_DeprecationWarning\000"
.LASF86:
	.ascii	"tzname\000"
.LASF594:
	.ascii	"word\000"
.LASF606:
	.ascii	"scale_value\000"
.LASF386:
	.ascii	"curexc_type\000"
.LASF577:
	.ascii	"buffer\000"
.LASF192:
	.ascii	"shape\000"
.LASF115:
	.ascii	"tp_hash\000"
.LASF297:
	.ascii	"PyByteArrayIter_Type\000"
.LASF273:
	.ascii	"suffix\000"
.LASF587:
	.ascii	"render_flags\000"
.LASF500:
	.ascii	"PyExc_UnicodeWarning\000"
.LASF190:
	.ascii	"ndim\000"
.LASF161:
	.ascii	"ssizeobjargproc\000"
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
.LASF603:
	.ascii	"render_nonaa_to_buffer_1ch_slice\000"
.LASF252:
	.ascii	"am_await\000"
.LASF490:
	.ascii	"PyExc_EnvironmentError\000"
.LASF432:
	.ascii	"_PyWeakref_CallableProxyType\000"
.LASF636:
	.ascii	"armwave_set_wave_pointer_as_testbuf\000"
.LASF44:
	.ascii	"_codecvt\000"
.LASF130:
	.ascii	"tp_members\000"
.LASF123:
	.ascii	"tp_traverse\000"
.LASF248:
	.ascii	"mp_length\000"
.LASF1:
	.ascii	"double\000"
.LASF253:
	.ascii	"am_aiter\000"
.LASF227:
	.ascii	"nb_inplace_xor\000"
.LASF55:
	.ascii	"ssize_t\000"
.LASF637:
	.ascii	"test_create_gamma\000"
.LASF307:
	.ascii	"_PyLong_DigitValue\000"
.LASF145:
	.ascii	"tp_subclasses\000"
.LASF223:
	.ascii	"nb_inplace_power\000"
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
.LASF614:
	.ascii	"printf\000"
.LASF202:
	.ascii	"nb_multiply\000"
.LASF613:
	.ascii	"rand\000"
.LASF84:
	.ascii	"__daylight\000"
.LASF230:
	.ascii	"nb_true_divide\000"
.LASF131:
	.ascii	"tp_getset\000"
.LASF306:
	.ascii	"PyLong_Type\000"
.LASF579:
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
.LASF580:
	.ascii	"armwave_set_wave_pointer_u32\000"
.LASF127:
	.ascii	"tp_iter\000"
.LASF231:
	.ascii	"nb_inplace_floor_divide\000"
.LASF63:
	.ascii	"program_invocation_name\000"
.LASF591:
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
.LASF601:
	.ascii	"offset\000"
.LASF588:
	.ascii	"length\000"
.LASF634:
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
.LASF98:
	.ascii	"_object\000"
.LASF294:
	.ascii	"Py_HashRandomizationFlag\000"
.LASF624:
	.ascii	"fopen\000"
.LASF208:
	.ascii	"nb_absolute\000"
.LASF548:
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
.LASF607:
	.ascii	"wave_base\000"
.LASF80:
	.ascii	"timezone\000"
.LASF111:
	.ascii	"tp_repr\000"
.LASF445:
	.ascii	"PyExc_AttributeError\000"
.LASF144:
	.ascii	"tp_cache\000"
.LASF570:
	.ascii	"armwave_test_generate\000"
.LASF443:
	.ascii	"PyExc_LookupError\000"
.LASF76:
	.ascii	"Py_ssize_t\000"
.LASF300:
	.ascii	"PyBytesIter_Type\000"
.LASF360:
	.ascii	"Py_UTF8Mode\000"
.LASF597:
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
.LASF540:
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
.LASF608:
	.ascii	"write_buffer_base\000"
.LASF532:
	.ascii	"ch4_buffer\000"
.LASF251:
	.ascii	"PyMappingMethods\000"
.LASF430:
	.ascii	"_PyWeakref_RefType\000"
.LASF272:
	.ascii	"prefix\000"
.LASF604:
	.ascii	"slice_y\000"
.LASF461:
	.ascii	"PyExc_SyntaxError\000"
.LASF559:
	.ascii	"noise\000"
.LASF510:
	.ascii	"_PyOS_ReadlineTState\000"
.LASF184:
	.ascii	"PySuper_Type\000"
.LASF348:
	.ascii	"PyCFunction_Type\000"
.LASF121:
	.ascii	"tp_flags\000"
.LASF551:
	.ascii	"ch2_color\000"
.LASF474:
	.ascii	"PyExc_ZeroDivisionError\000"
.LASF284:
	.ascii	"Py_InteractiveFlag\000"
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
.LASF549:
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
.LASF609:
	.ascii	"write_buffer\000"
.LASF176:
	.ascii	"iternextfunc\000"
.LASF487:
	.ascii	"PyExc_PermissionError\000"
.LASF2:
	.ascii	"unsigned int\000"
.LASF420:
	.ascii	"getter\000"
.LASF470:
	.ascii	"PyExc_UnicodeEncodeError\000"
.LASF216:
	.ascii	"nb_int\000"
.LASF572:
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
.LASF581:
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
.LASF555:
	.ascii	"g_armwave_state\000"
.LASF204:
	.ascii	"nb_divmod\000"
.LASF528:
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
