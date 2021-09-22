unit NES.APU.OPLL;

(* Copyright (C) 2019 Nuke.YKT
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 *
 *  Yamaha YM2413 emulator
 *  Thanks:
 *      siliconpr0n.org(digshadow, John McMaster):
 *          VRC VII decap and die shot.
 *
 *  version: 1.0.1
 *
 *  FreePascal conversion by hukka 2021-08-13 (VRC7 relevant portions only)
 *)

{$mode Delphi}

interface

uses
	Classes, SysUtils,
	NES.SaveState;

const
	opll_patch_1      = 00;
	opll_patch_2      = 01;
	opll_patch_3      = 02;
	opll_patch_4      = 03;
	opll_patch_5      = 04;
	opll_patch_6      = 05;
	opll_patch_7      = 06;
	opll_patch_8      = 07;
	opll_patch_9      = 08;
	opll_patch_10     = 09;
	opll_patch_11     = 10;
	opll_patch_12     = 11;
	opll_patch_13     = 12;
	opll_patch_14     = 13;
	opll_patch_15     = 14;
	opll_patch_drum_0 = 15;
	opll_patch_drum_1 = 16;
	opll_patch_drum_2 = 17;
	opll_patch_drum_3 = 18;
	opll_patch_drum_4 = 19;
	opll_patch_drum_5 = 20;

type
	Topll_patch = record
		tl, dc, dm, fb: Byte;
		am, vib, et, ksr, multi,
		ksl, ar, dr, sl, rr: array[0..1] of Byte;
	end;
	Popll_patch = ^Topll_patch;

	TOPLL = class(TSnapshotable)
	private
		patches: array[opll_patch_1..opll_patch_drum_5] of Topll_patch;

		cycles:  Cardinal;

		// IO
		write_data,
		write_a, write_d,
		write_mode_address,
		address,
		data: Byte;
		write_fm_address, write_fm_data,
		write_a_en, write_d_en: Boolean;

		// Envelope generator
		eg_counter_state,
		eg_counter_state_prev: Byte;
		eg_timer, eg_dokon: Cardinal;

		eg_state, eg_level: array [0..17] of Byte;

		eg_timer_low_lock, eg_timer_carry,
		eg_timer_shift, eg_timer_shift_lock, eg_timer_shift_stop,
		eg_kon, eg_off,
		eg_rate,
		eg_inc_lo, eg_inc_hi, eg_rate_hi,
		eg_sl, eg_ksltl,
		eg_out: Byte;

		eg_maxrate, eg_zerorate,
		eg_silent: Boolean;

		// Phase generator
		pg_out: Word;
		pg_phase: array [0..17] of Cardinal;
		pg_inc,
		pg_phase_next: Cardinal;

		// Operator
		op_fb1, op_fb2: array [0..8] of Int16;
		op_fbsum,
		op_mod: Int16;
		op_neg: Byte;
		op_logsin,
		op_exp_m, op_exp_s: Word;

		// Channel
		output_m,
		ch_out: Int16;

		// LFO
		lfo_counter,
		lfo_am_counter: Word;
		lfo_vib_counter,
		lfo_am_step,
		lfo_am_dir,
		lfo_am_car,
		lfo_am_out: Byte;

		// Register set
		block, kon, son, vol, inst: array [0..8] of Byte;
		fnum: array [0..8] of Word;

		patch: Topll_patch;

		rhythm, testmode,
		c_tl, c_dc, c_dm, c_fb, c_am,
		c_vib, c_et,
		c_ksr, c_ksr_freq,
		c_ksl, c_ksl_freq, c_ksl_block,
		c_multi, c_sl: Byte;

		c_adrr: array [0..2] of Byte;
		c_fnum, c_block: Word;

		// Rhythm mode
		rm_enable: Int8;
		rm_noise, rm_select: Cardinal;
		rm_hh_bit2, rm_hh_bit3, rm_hh_bit7,
		rm_hh_bit8, rm_tc_bit3, rm_tc_bit5: Byte;

	protected
		procedure DoIO;
		procedure DoModeWrite;
		procedure DoRegWrite;
		procedure PreparePatch1;
		procedure PreparePatch2;
		procedure PhaseGenerate;
		procedure PhaseCalcIncrement;
		procedure EnvelopeKSLTL;
		procedure EnvelopeOutput;
		procedure EnvelopeGenerate;
		procedure DoChannel;
		procedure DoOperator;
		procedure DoRhythm;
		procedure DoLFO;

	public
		procedure Reset;
		procedure Write(_port: Cardinal; _data: Byte);
		function  Clock: SmallInt;

		procedure LoadSnapshot; override;
		procedure SaveSnapshot; override;

		constructor Create;
	end;

const
	eg_num_attack  = 0;
	eg_num_decay   = 1;
	eg_num_sustain = 2;
	eg_num_release = 3;

	rm_num_bd0 = 0;
	rm_num_hh  = 1;
	rm_num_tom = 2;
	rm_num_bd1 = 3;
	rm_num_sd  = 4;
	rm_num_tc  = 5;

	// logsin table
	logsinrom: array[Byte] of Word = (
	    $859, $6c3, $607, $58b, $52e, $4e4, $4a6, $471,
	    $443, $41a, $3f5, $3d3, $3b5, $398, $37e, $365,
	    $34e, $339, $324, $311, $2ff, $2ed, $2dc, $2cd,
	    $2bd, $2af, $2a0, $293, $286, $279, $26d, $261,
	    $256, $24b, $240, $236, $22c, $222, $218, $20f,
	    $206, $1fd, $1f5, $1ec, $1e4, $1dc, $1d4, $1cd,
	    $1c5, $1be, $1b7, $1b0, $1a9, $1a2, $19b, $195,
	    $18f, $188, $182, $17c, $177, $171, $16b, $166,
	    $160, $15b, $155, $150, $14b, $146, $141, $13c,
	    $137, $133, $12e, $129, $125, $121, $11c, $118,
	    $114, $10f, $10b, $107, $103, $0ff, $0fb, $0f8,
	    $0f4, $0f0, $0ec, $0e9, $0e5, $0e2, $0de, $0db,
	    $0d7, $0d4, $0d1, $0cd, $0ca, $0c7, $0c4, $0c1,
	    $0be, $0bb, $0b8, $0b5, $0b2, $0af, $0ac, $0a9,
	    $0a7, $0a4, $0a1, $09f, $09c, $099, $097, $094,
	    $092, $08f, $08d, $08a, $088, $086, $083, $081,
	    $07f, $07d, $07a, $078, $076, $074, $072, $070,
	    $06e, $06c, $06a, $068, $066, $064, $062, $060,
	    $05e, $05c, $05b, $059, $057, $055, $053, $052,
	    $050, $04e, $04d, $04b, $04a, $048, $046, $045,
	    $043, $042, $040, $03f, $03e, $03c, $03b, $039,
	    $038, $037, $035, $034, $033, $031, $030, $02f,
	    $02e, $02d, $02b, $02a, $029, $028, $027, $026,
	    $025, $024, $023, $022, $021, $020, $01f, $01e,
	    $01d, $01c, $01b, $01a, $019, $018, $017, $017,
	    $016, $015, $014, $014, $013, $012, $011, $011,
	    $010, $00f, $00f, $00e, $00d, $00d, $00c, $00c,
	    $00b, $00a, $00a, $009, $009, $008, $008, $007,
	    $007, $007, $006, $006, $005, $005, $005, $004,
	    $004, $004, $003, $003, $003, $002, $002, $002,
	    $002, $001, $001, $001, $001, $001, $001, $001,
	    $000, $000, $000, $000, $000, $000, $000, $000
	);

	// exp table
	exprom: array[Byte] of Word = (
	    $7fa, $7f5, $7ef, $7ea, $7e4, $7df, $7da, $7d4,
	    $7cf, $7c9, $7c4, $7bf, $7b9, $7b4, $7ae, $7a9,
	    $7a4, $79f, $799, $794, $78f, $78a, $784, $77f,
	    $77a, $775, $770, $76a, $765, $760, $75b, $756,
	    $751, $74c, $747, $742, $73d, $738, $733, $72e,
	    $729, $724, $71f, $71a, $715, $710, $70b, $706,
	    $702, $6fd, $6f8, $6f3, $6ee, $6e9, $6e5, $6e0,
	    $6db, $6d6, $6d2, $6cd, $6c8, $6c4, $6bf, $6ba,
	    $6b5, $6b1, $6ac, $6a8, $6a3, $69e, $69a, $695,
	    $691, $68c, $688, $683, $67f, $67a, $676, $671,
	    $66d, $668, $664, $65f, $65b, $657, $652, $64e,
	    $649, $645, $641, $63c, $638, $634, $630, $62b,
	    $627, $623, $61e, $61a, $616, $612, $60e, $609,
	    $605, $601, $5fd, $5f9, $5f5, $5f0, $5ec, $5e8,
	    $5e4, $5e0, $5dc, $5d8, $5d4, $5d0, $5cc, $5c8,
	    $5c4, $5c0, $5bc, $5b8, $5b4, $5b0, $5ac, $5a8,
	    $5a4, $5a0, $59c, $599, $595, $591, $58d, $589,
	    $585, $581, $57e, $57a, $576, $572, $56f, $56b,
	    $567, $563, $560, $55c, $558, $554, $551, $54d,
	    $549, $546, $542, $53e, $53b, $537, $534, $530,
	    $52c, $529, $525, $522, $51e, $51b, $517, $514,
	    $510, $50c, $509, $506, $502, $4ff, $4fb, $4f8,
	    $4f4, $4f1, $4ed, $4ea, $4e7, $4e3, $4e0, $4dc,
	    $4d9, $4d6, $4d2, $4cf, $4cc, $4c8, $4c5, $4c2,
	    $4be, $4bb, $4b8, $4b5, $4b1, $4ae, $4ab, $4a8,
	    $4a4, $4a1, $49e, $49b, $498, $494, $491, $48e,
	    $48b, $488, $485, $482, $47e, $47b, $478, $475,
	    $472, $46f, $46c, $469, $466, $463, $460, $45d,
	    $45a, $457, $454, $451, $44e, $44b, $448, $445,
	    $442, $43f, $43c, $439, $436, $433, $430, $42d,
	    $42a, $428, $425, $422, $41f, $41c, $419, $416,
	    $414, $411, $40e, $40b, $408, $406, $403, $400
	);

	patch_ds1001: array [opll_patch_1..opll_patch_drum_5] of array [0..23] of Byte = (
		($05,$00,$00,$06, $00,$00,$00,$00,$00,$01,$00,$00,$03,$01,$00,$00,$0e,$08,$08,$01,$04,$02,$02,$07),
		($14,$00,$01,$05, $00,$00,$00,$01,$00,$00,$01,$00,$03,$01,$00,$00,$0d,$0f,$08,$06,$02,$01,$03,$02),
		($08,$00,$01,$00, $00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$00,$00,$0f,$0b,$0a,$02,$02,$01,$00,$02),
		($0c,$00,$00,$07, $00,$00,$00,$01,$01,$01,$01,$00,$01,$01,$00,$00,$0a,$06,$08,$04,$06,$02,$01,$07),
		($1e,$00,$00,$06, $00,$00,$00,$00,$01,$01,$01,$00,$02,$01,$00,$00,$0e,$07,$01,$06,$00,$02,$01,$08),
		($06,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00,$02,$01,$00,$00,$0a,$0e,$03,$02,$0f,$0f,$04,$04),
		($1d,$00,$00,$07, $00,$00,$00,$01,$01,$01,$00,$00,$01,$01,$00,$00,$08,$08,$02,$01,$01,$00,$01,$07),
		($22,$01,$00,$07, $00,$00,$00,$00,$01,$01,$00,$00,$03,$01,$00,$00,$0a,$07,$02,$02,$00,$01,$01,$07),
		($25,$00,$00,$00, $00,$00,$00,$00,$01,$00,$01,$01,$05,$01,$00,$00,$04,$07,$00,$03,$07,$00,$02,$01),
		($0f,$00,$01,$07, $01,$00,$00,$00,$01,$00,$01,$00,$05,$01,$00,$00,$0a,$0a,$08,$05,$05,$00,$01,$02),
		($24,$00,$00,$07, $00,$01,$00,$01,$00,$00,$01,$00,$07,$01,$00,$00,$0f,$0f,$08,$08,$02,$01,$02,$02),
		($11,$00,$00,$06, $00,$00,$01,$00,$01,$01,$01,$00,$01,$03,$00,$00,$06,$07,$05,$04,$01,$01,$08,$06),
		($13,$00,$00,$05, $00,$00,$00,$00,$00,$00,$00,$00,$01,$02,$03,$00,$0c,$09,$09,$05,$00,$00,$03,$02),
		($0c,$00,$00,$00, $00,$00,$01,$01,$01,$01,$00,$00,$01,$03,$00,$00,$09,$0c,$04,$00,$03,$0f,$03,$06),
		($0d,$00,$00,$00, $00,$00,$00,$01,$01,$01,$00,$01,$01,$02,$00,$00,$0c,$0d,$01,$05,$05,$00,$06,$06),
		($18,$00,$01,$07, $00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$0d,$00,$0f,$00,$06,$00,$0a,$00),
		($00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$0c,$00,$08,$00,$0a,$00,$07,$00),
		($00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00,$05,$00,$00,$00,$0f,$00,$08,$00,$05,$00,$09,$00),
		($00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$0f,$00,$08,$00,$06,$00,$0d),
		($00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$0d,$00,$08,$00,$06,$00,$08),
		($00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$0a,$00,$0a,$00,$05,$00,$05)
	);

	ch_offset: array [0..17] of Byte = (
		1, 2, 0, 1, 2, 3, 4, 5, 3, 4, 5, 6, 7, 8, 6, 7, 8, 0
	);

	pg_multi: array [0..15] of Byte = (
		1, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 20, 24, 24, 30, 30
	);

	eg_stephi: array [0..3, 0..3] of Byte = (
	    (0, 0, 0, 0),  (1, 0, 0, 0),  (1, 0, 1, 0),  (1, 1, 1, 0)
	);

	eg_ksltable: array [0..15] of Byte = (
		0, 32, 40, 45, 48, 51, 53, 55, 56, 58, 59, 60, 61, 62, 63, 64
	);

	BoolVal: array [Boolean] of Byte = ( 0, 1 );


implementation

uses
	Math;

constructor TOPLL.Create;
var
	p, i: Integer;
begin
	inherited Create('OPLL');

	for p := opll_patch_1 to opll_patch_drum_5 do
	with patches[p] do
	begin
		tl := patch_ds1001[p, 0];
		dc := patch_ds1001[p, 1];
		dm := patch_ds1001[p, 2];
		fb := patch_ds1001[p, 3];
		for i := 0 to 1 do
		begin
			am[i]    := patch_ds1001[p, 04+i];
			vib[i]   := patch_ds1001[p, 06+i];
			et[i]    := patch_ds1001[p, 08+i];
			ksr[i]   := patch_ds1001[p, 10+i];
			multi[i] := patch_ds1001[p, 12+i];
			ksl[i]   := patch_ds1001[p, 14+i];
			ar[i]    := patch_ds1001[p, 16+i];
			dr[i]    := patch_ds1001[p, 18+i];
			sl[i]    := patch_ds1001[p, 20+i];
			rr[i]    := patch_ds1001[p, 22+i];
		end;
	end;

	RegisterProperty(32, @cycles);

	// IO
	RegisterProperty(8, @write_data);
	RegisterProperty(8, @write_a);
	RegisterProperty(8, @write_d);
	RegisterProperty(8, @write_mode_address);
	RegisterProperty(8, @address);
	RegisterProperty(8, @data);

	RegisterProperty(1, @write_fm_address);
	RegisterProperty(1, @write_fm_data);
	RegisterProperty(1, @write_a_en);
	RegisterProperty(1, @write_d_en);

	// Envelope generator
	RegisterProperty(8, @eg_counter_state);
	RegisterProperty(8, @eg_counter_state_prev);
	RegisterProperty(32, @eg_timer);
	RegisterProperty(32, @eg_dokon);

	RegisterArray(Length(eg_state), @eg_state[0]);
	RegisterArray(Length(eg_level), @eg_level[0]);

	RegisterProperty(8, @eg_timer_low_lock);
	RegisterProperty(8, @eg_timer_carry);
	RegisterProperty(8, @eg_timer_shift);
	RegisterProperty(8, @eg_timer_shift_lock);
	RegisterProperty(8, @eg_timer_shift_stop);
	RegisterProperty(8, @eg_kon);
	RegisterProperty(8, @eg_off);
	RegisterProperty(8, @eg_rate);
	RegisterProperty(8, @eg_inc_lo);
	RegisterProperty(8, @eg_inc_hi);
	RegisterProperty(8, @eg_rate_hi);
	RegisterProperty(8, @eg_sl);
	RegisterProperty(8, @eg_ksltl);
	RegisterProperty(8, @eg_out);

	RegisterProperty(1, @eg_maxrate);
	RegisterProperty(1, @eg_zerorate);
	RegisterProperty(1, @eg_silent);

	// Phase generator
	RegisterProperty(16, @pg_out);
	RegisterArray(Length(pg_phase), @pg_phase[0]);
	RegisterProperty(32, @pg_inc);
	RegisterProperty(32, @pg_phase_next);

	// Operator
	RegisterArray(Length(op_fb1), @op_fb1[0]);
	RegisterArray(Length(op_fb2), @op_fb2[0]);
	RegisterProperty(16, @op_fbsum);
	RegisterProperty(16, @op_mod);
	RegisterProperty(8,  @op_neg);
	RegisterProperty(16, @op_logsin);
	RegisterProperty(16, @op_exp_m);
	RegisterProperty(16, @op_exp_s);

	// Channel
	RegisterProperty(16, @output_m);
	RegisterProperty(16, @ch_out);

	// LFO
	RegisterProperty(16, @lfo_counter);
	RegisterProperty(16, @lfo_am_counter);
	RegisterProperty(8,  @lfo_vib_counter);
	RegisterProperty(8,  @lfo_am_step);
	RegisterProperty(8,  @lfo_am_dir);
	RegisterProperty(8,  @lfo_am_car);
	RegisterProperty(8,  @lfo_am_out);

	// Register set
	RegisterArray(Length(block), @block[0]);
	RegisterArray(Length(kon),   @kon[0]);
	RegisterArray(Length(son),   @son[0]);
	RegisterArray(Length(vol),   @vol[0]);
	RegisterArray(Length(inst),  @inst[0]);
	RegisterArray(Length(fnum),  @fnum[0]);

	RegisterProperty(8, @rhythm);
	RegisterProperty(8, @testmode);
	RegisterProperty(8, @c_tl);
	RegisterProperty(8, @c_dc);
	RegisterProperty(8, @c_dm);
	RegisterProperty(8, @c_fb);
	RegisterProperty(8, @c_am);
	RegisterProperty(8, @c_vib);
	RegisterProperty(8, @c_et);
	RegisterProperty(8, @c_ksr);
	RegisterProperty(8, @c_ksr_freq);
	RegisterProperty(8, @c_ksl);
	RegisterProperty(8, @c_ksl_freq);
	RegisterProperty(8, @c_ksl_block);
	RegisterProperty(8, @c_multi);
	RegisterProperty(8, @c_sl);

	RegisterArray(Length(c_adrr), @c_adrr[0]);

	RegisterProperty(16, @c_fnum);
	RegisterProperty(16, @c_block);
	RegisterProperty(8,  @rm_enable);
	RegisterProperty(32, @rm_noise);
	RegisterProperty(32, @rm_select);
	RegisterProperty(8,  @rm_hh_bit2);
	RegisterProperty(8,  @rm_hh_bit3);
	RegisterProperty(8,  @rm_hh_bit7);
	RegisterProperty(8,  @rm_hh_bit8);
	RegisterProperty(8,  @rm_tc_bit3);
	RegisterProperty(8,  @rm_tc_bit5);

	Reset;
end;

procedure TOPLL.LoadSnapshot;

	procedure LoadPatch(P: Popll_patch);
	var
		i: Integer;
	begin
		P.tl := Stream.ReadByte;
		P.dc := Stream.ReadByte;
		P.dm := Stream.ReadByte;
		P.fb := Stream.ReadByte;
		for i := 0 to 1 do
		begin
			P.am[i]   := Stream.ReadByte;
			P.vib[i]  := Stream.ReadByte;
			P.et[i]   := Stream.ReadByte;
			P.ksr[i]  := Stream.ReadByte;
			P.multi[i]:= Stream.ReadByte;
			P.ksl[i]  := Stream.ReadByte;
			P.ar[i]   := Stream.ReadByte;
			P.dr[i]   := Stream.ReadByte;
			P.sl[i]   := Stream.ReadByte;
			P.rr[i]   := Stream.ReadByte;
		end;
	end;

var
	i: Integer;
begin
	inherited LoadSnapshot;
writeln('LoadSnapshot');

	LoadPatch(@patch);

	for i := opll_patch_1 to opll_patch_drum_5 do
		LoadPatch(@patches[i]);
end;

procedure TOPLL.SaveSnapshot;

	procedure SavePatch(P: Popll_patch);
	var
		i: Integer;
	begin
		Stream.WriteByte(P.tl);
		Stream.WriteByte(P.dc);
		Stream.WriteByte(P.dm);
		Stream.WriteByte(P.fb);
		for i := 0 to 1 do
		begin
			Stream.WriteByte(P.am[i]);
			Stream.WriteByte(P.vib[i]);
			Stream.WriteByte(P.et[i]);
			Stream.WriteByte(P.ksr[i]);
			Stream.WriteByte(P.multi[i]);
			Stream.WriteByte(P.ksl[i]);
			Stream.WriteByte(P.ar[i]);
			Stream.WriteByte(P.dr[i]);
			Stream.WriteByte(P.sl[i]);
			Stream.WriteByte(P.rr[i]);
		end;
	end;

var
	i: Integer;
begin
	inherited SaveSnapshot;
writeln('SaveSnapshot');

	SavePatch(@patch);

	for i := opll_patch_1 to opll_patch_drum_5 do
		SavePatch(@patches[i]);
end;

procedure TOPLL.Reset;
var
	i: Integer;
begin
	rhythm := $20;
	{$R-}rm_enable := Int8($80);

	patch := default(Topll_patch);

	for i := 0 to 17 do
	begin
		eg_state[i] := eg_num_release;
		eg_level[i] := $7f;
	end;

	eg_out := $7f;
	rm_select := rm_num_tc + 1;
end;

procedure TOPLL.DoIO;
begin
	// Write signal check
	write_a_en := (write_a and $03) = $01;
	write_d_en := (write_d and $03) = $01;
	{$R-}write_a := write_a << 1;
	{$R-}write_d := write_d << 1;
end;

procedure TOPLL.DoModeWrite;
var
	slot: Byte;
begin
	if (write_d_en) and ((write_mode_address and $10) <> 0) then
	begin
		slot := write_mode_address and $01;

		case (write_mode_address and $0F) of

			$00, $01:
			begin
				patch.multi[slot] :=  write_data       and $0f;
				patch.ksr[slot]   := (write_data >> 4) and $01;
				patch.et[slot]    := (write_data >> 5) and $01;
				patch.vib[slot]   := (write_data >> 6) and $01;
				patch.am[slot]    := (write_data >> 7) and $01;
			end;

			$02:
			begin
				patch.ksl[0] := (write_data >> 6) and $03;
				patch.tl     :=  write_data and $3f;
			end;

			$03:
			begin
				patch.ksl[1] := (write_data >> 6) and $03;
				patch.dc     := (write_data >> 4) and $01;
				patch.dm     := (write_data >> 3) and $01;
				patch.fb     :=  write_data       and $07;
			end;

			$04, $05:
			begin
				patch.dr[slot] := write_data and $0f;
				patch.ar[slot] := (write_data >> 4) and $0f;
			end;

			$06, $07:
			begin
				patch.rr[slot] := write_data and $0f;
				patch.sl[slot] := (write_data >> 4) and $0f;
			end;

			$0E:
			begin
				rhythm := (write_data and $3f) or $20;
				rm_enable := (rm_enable and $7f) or Byte((rhythm << 2) and $80);
			end;

			$0F:
				testmode := write_data and $0f;

		end;
	end;
end;

procedure TOPLL.DoRegWrite;
var
	channel: Cardinal;
begin
	// Address
	if write_a_en then
	begin
		if ((write_data and $c0) = $00) then
		begin
			// FM Write
			write_fm_address := True;
			address := write_data;
		end
		else
			write_fm_address := False;
	end;

	// Data
	if (write_fm_address and write_d_en) then
		data := write_data;

	// Update registers
	if (write_fm_data) and (not write_a_en) then
	begin
		if (cycles < 16) and ((address and $F) = Byte(cycles and $F)) then
		begin
			channel := cycles mod 9;

			case (address and $f0) of

				$10:
					fnum[channel] := (fnum[channel] and $100) or data;

				$20:
				begin
					fnum[channel] := (fnum[channel] and $ff) or ((data and $01) << 8);
					block[channel] := (data >> 1) and $07;
					kon[channel] := (data >> 4) and $01;
					son[channel] := (data >> 5) and $01;
				end;

				$30:
				begin
					vol[channel] := data and $0f;
					inst[channel] := (data >> 4) and $0f;
				end;

			end;
		end;
	end;

	if (write_a_en) then
		write_fm_data := False;

	if (write_fm_address and write_d_en) then
		write_fm_data := True;

	if (write_a_en) then
	begin
		if ((write_data and $f0) = $00) then
			write_mode_address := $10 or (write_data and $0f)
		else
			write_mode_address := $00;
	end;
end;

procedure TOPLL.PreparePatch1;
var
	instr: Byte;
	mcsel, instr_index, ch: Cardinal;
	_patch: Popll_patch;
begin
	mcsel := ((cycles + 1) div 3) and $01;
	ch := ch_offset[cycles];
	instr := inst[ch];

	if (instr > 0) then
		instr_index := opll_patch_1 + instr - 1
	else
		instr_index := 0;

	if (rm_select <= rm_num_tc) then
		instr_index := opll_patch_drum_0 + rm_select;

	if (rm_select <= rm_num_tc) or (instr > 0) then
		_patch := @patches[instr_index]
	else
		_patch := @patch;

	if (rm_select = rm_num_hh) or (rm_select = rm_num_tom) then
		c_tl := inst[ch] << 2
	else
	if (mcsel = 1) then
		c_tl := vol[ch] << 2
	else
		c_tl := _patch.tl;

	c_adrr[0] := _patch.ar[mcsel];
	c_adrr[1] := _patch.dr[mcsel];
	c_adrr[2] := _patch.rr[mcsel];
	c_et  := _patch.et[mcsel];
	c_ksr := _patch.ksr[mcsel];
	c_ksl := _patch.ksl[mcsel];
	c_ksr_freq := (block[ch] << 1) or (fnum[ch] >> 8);
	c_ksl_freq := (fnum[ch] >> 5);
	c_ksl_block := (block[ch]);
end;

procedure TOPLL.PreparePatch2;
var
	instr: Byte;
	mcsel, instr_index: Cardinal;
	_patch: Popll_patch;
begin
	mcsel := ((cycles + 1) div 3) and $01;
	instr := inst[ch_offset[cycles]];

	if (instr > 0) then
		instr_index := opll_patch_1 + instr - 1
	else
		instr_index := 0;

	if (rm_select <= rm_num_tc) then
		instr_index := opll_patch_drum_0 + rm_select;

	if (rm_select <= rm_num_tc) or (instr > 0) then
		_patch := @patches[instr_index]
	else
		_patch := @patch;

	c_fnum := fnum[ch_offset[cycles]];
	c_block := block[ch_offset[cycles]];

	c_multi := _patch.multi[mcsel];
	c_sl := _patch.sl[mcsel];
	c_fb := _patch.fb;
	c_vib:= _patch.vib[mcsel];
	c_am := _patch.am[mcsel];
	c_dc := c_dc << 1;
	c_dm := c_dm << 1;
	c_dc := c_dc or _patch.dc;
	c_dm := c_dm or _patch.dm;
end;

procedure TOPLL.PhaseGenerate;
var
	ismod, phase: Cardinal;
	rm_bit: Byte;
	pgout: Word = 0;
begin
	{$Q-}pg_phase[(cycles + 17) mod 18] := pg_phase_next + pg_inc;

	if (((rm_enable and $40) <> 0) and ((cycles = 13) or (cycles = 14))) then
		ismod := 0
	else
		ismod := ((cycles + 3) div 3) and 1;

	phase := pg_phase[cycles];

	// KeyOn event check
    if ( ((testmode and $04) <> 0) or
		 ((ismod <> 0) and ((eg_dokon and $8000) <> 0)) or
		 ((ismod  = 0) and ((eg_dokon and $0001) <> 0)) ) then
		pg_phase_next := 0
	else
		pg_phase_next := phase;

	// Rhythm mode
	if (cycles = 13) then
	begin
		rm_hh_bit2 := (phase >> (2 + 9)) and 1;
		rm_hh_bit3 := (phase >> (3 + 9)) and 1;
		rm_hh_bit7 := (phase >> (7 + 9)) and 1;
		rm_hh_bit8 := (phase >> (8 + 9)) and 1;
	end
	else
	if (cycles = 17) and ((rm_enable and $80) <> 0) then
	begin
		rm_tc_bit3 := (phase >> (3 + 9)) and 1;
		rm_tc_bit5 := (phase >> (5 + 9)) and 1;
	end;

	if (rm_enable and $80) <> 0 then
	case cycles of
		13: begin  // HH
			rm_bit := (rm_hh_bit2 xor rm_hh_bit7)
				   or (rm_hh_bit3 xor rm_tc_bit5)
				   or (rm_tc_bit3 xor rm_tc_bit5);
			pgout := rm_bit << 9;
			if (rm_bit xor (rm_noise and 1)) <> 0 then
				pgout := pgout or $d0
			else
				pgout := pgout or $34;
		end;

		16: begin  // SD
			pgout := (rm_hh_bit8 << 9)
				   or ((rm_hh_bit8 xor (rm_noise and 1)) << 8);
		end;

		17: begin  // TC
			rm_bit := (rm_hh_bit2 xor rm_hh_bit7)
				   or (rm_hh_bit3 xor rm_tc_bit5)
				   or (rm_tc_bit3 xor rm_tc_bit5);
			pgout := (rm_bit << 9) or $100;
		end;

		else
			pgout := phase >> 9;
	end
	else
		pgout := phase >> 9;

	pg_out := pgout;
end;

procedure TOPLL.PhaseCalcIncrement;
var
	freq: Cardinal;
begin
	freq := c_fnum << 1;

	// Apply vibrato
	if c_vib <> 0 then
	case lfo_vib_counter of
		1,3:	Inc(freq, freq >> 8);
		2:		Inc(freq, freq >> 7);
		5,7:	Dec(freq, freq >> 8);
		6:		Dec(freq, freq >> 7);
	end;

	// Apply block
	freq := (freq << c_block) >> 1;

	pg_inc := (freq * pg_multi[c_multi]) >> 1;
end;

procedure TOPLL.EnvelopeKSLTL;
var
	ksl: Integer;
begin
	ksl := eg_ksltable[c_ksl_freq] - ((8 - c_ksl_block) << 3);
	if ksl < 0 then
		ksl := 0;

	ksl := ksl << 1;

	if c_ksl <> 0 then
		ksl := ksl >> (3 - c_ksl)
	else
		ksl := 0;

	eg_ksltl := Byte(ksl and $FF) + (c_tl << 1);
end;

procedure TOPLL.EnvelopeOutput;
var
	level: Integer;
begin
	level := eg_level[(cycles+17) mod 18] + eg_ksltl;

	if c_am <> 0 then
		Inc(level, lfo_am_out);

	if level >= 128 then
		level := 127;

	if (testmode and 1) <> 0 then
		level := 0;

	eg_out := level;
end;

procedure TOPLL.EnvelopeGenerate;
var
	timer_inc, timer_bit, timer_low, rate, state_rate,
	ksr, sum, rate_hi, rate_lo, state, next_state: Byte;

	i0, i1: Boolean;

	level, next_level,
	step, sl, shift: Integer;

	mcsel: Cardinal;
	zero: Boolean;

	procedure DoI;
	begin
		i0 :=	 (eg_rate_hi = 15) or ((eg_rate_hi = 14) and (eg_inc_hi <> 0));
		i1 :=	((eg_rate_hi = 14) and (eg_inc_hi =0)) or
				((eg_rate_hi = 13) and (eg_inc_hi<>0)) or
				((eg_rate_hi = 13) and (eg_inc_hi =0) and ((eg_counter_state_prev and 1)<> 0)) or
				((eg_rate_hi = 12) and (eg_inc_hi<>0) and ((eg_counter_state_prev and 1)<> 0)) or
				((eg_rate_hi = 12) and (eg_inc_hi =0) and ((eg_counter_state_prev and 3) = 3)) or
				((eg_inc_lo <> 0) and ((eg_counter_state_prev and 3) = 3));
		step := (BoolVal[i0] << 1) or BoolVal[i1];
	end;

begin
	mcsel := ((cycles + 1) div 3) and $01;

	// EG timer
	if ((eg_counter_state and 3) <> 3) then
		timer_inc := 0
	else
	if (cycles = 0) then
		timer_inc := 1
	else
		timer_inc := eg_timer_carry;

	timer_low := eg_timer and 3;
	timer_bit := (eg_timer and 1) + timer_inc;
	eg_timer_carry := timer_bit >> 1;
	eg_timer := Cardinal((timer_bit and 1) << 17) or (eg_timer >> 1);

	if (testmode and $08) <> 0 then
		eg_timer := (eg_timer and $2ffff) or ((write_data << (16 - 2)) and $10000);

	if (eg_timer_shift_stop = 0) and (((eg_timer >> 16) and 1) <> 0) then
		eg_timer_shift := cycles;

	if (cycles = 0) and ((eg_counter_state_prev and 1) = 1) then
	begin
		eg_timer_low_lock := timer_low;
		eg_timer_shift_lock := eg_timer_shift;
		if (eg_timer_shift_lock > 13) then
			eg_timer_shift_lock := 0;
		eg_timer_shift := 0;
	end;

	eg_timer_shift_stop := eg_timer_shift_stop or ((eg_timer >> 16) and 1);
	if (cycles = 0) then
		eg_timer_shift_stop := 0;

	eg_counter_state_prev := eg_counter_state;
	if (cycles = 17) then
		Inc(eg_counter_state);

	level := eg_level[(cycles+16) mod 18];
	next_level := level;
	zero := (level = 0);
	eg_silent := (level = $7f);

    if (eg_state[(cycles+16) mod 18] <> eg_num_attack) and
		((eg_off and 2) <> 0) and ((eg_dokon and 2) = 0) then
			next_level := $7f;

	if (eg_maxrate) and ((eg_dokon and 2) <> 0) then
		next_level := $00;

	state := eg_state[(cycles+16) mod 18];
	next_state := eg_num_attack;

	step := 0;
	sl := eg_sl;

	case state of

		eg_num_attack:
		begin
			if (not eg_maxrate) and ((eg_kon and 2) <> 0) and (not zero) then
			begin
				shift := eg_rate_hi - 11 + eg_inc_hi;
				if (eg_inc_lo <> 0) then
					shift := 1;
				if shift > 0 then
				begin
					if shift > 4 then shift := 4;
					step := (not level) >> (5 - shift);
				end;
			end;
			if (zero) then
				next_state := eg_num_decay
			else
				next_state := eg_num_attack;
		end;

		eg_num_decay:
		begin
			if ((eg_off and 2) = 0) and ((eg_dokon and 2) = 0) and ((level >> 3) <> sl) then
				DoI;
			if ((level >> 3) = sl) then
				next_state := eg_num_sustain
			else
				next_state := eg_num_decay;
		end;

		eg_num_sustain,
		eg_num_release:
		begin
			if ((eg_off and 2) = 0) and ((eg_dokon and 2) = 0) then
				DoI;
			next_state := state;
		end;

	end;

	if (eg_kon and 2) = 0 then
		next_state := eg_num_release;

	if (eg_dokon and 2) <> 0 then
		next_state := eg_num_attack;

	eg_level[(cycles+16) mod 18] := next_level + step;
	eg_state[(cycles+16) mod 18] := next_state;

	rate_hi := eg_rate >> 2;
	rate_lo := eg_rate and 3;
	eg_inc_hi := eg_stephi[rate_lo][eg_timer_low_lock];
	sum := (eg_timer_shift_lock + rate_hi) and $0f;
	eg_inc_lo := 0;

	if (rate_hi < 12) and (not eg_zerorate) then
	case sum of
		12: eg_inc_lo := 1;
		13: eg_inc_lo := (rate_lo >> 1) and 1;
		14: eg_inc_lo := rate_lo and 1;
	end;

	eg_maxrate := (rate_hi = $0f);
	eg_rate_hi := rate_hi;

	eg_kon := (eg_kon << 1) or kon[ch_offset[cycles]];
	eg_off := (eg_off << 1) or BoolVal[(eg_level[cycles] >> 2) = $1f];

	case rm_select of
		rm_num_bd0,
		rm_num_bd1: eg_kon := eg_kon or ((rhythm >> 4) and 1);
		rm_num_sd:  eg_kon := eg_kon or ((rhythm >> 3) and 1);
		rm_num_tom: eg_kon := eg_kon or ((rhythm >> 2) and 1);
		rm_num_tc:  eg_kon := eg_kon or ((rhythm >> 1) and 1);
		rm_num_hh:  eg_kon := eg_kon or ( rhythm       and 1);
	end;

	// Calculate rate
	rate := 0;
	eg_dokon := eg_dokon << 1;
	state_rate := eg_state[cycles];
	if (state_rate = eg_num_release) and ((eg_kon and 1) <> 0) and ((eg_off and 1) <> 0) then
	begin
		state_rate := eg_num_attack;
		eg_dokon := eg_dokon or 1;
	end;

	case state_rate of

		eg_num_attack:
			rate := c_adrr[0];

		eg_num_decay:
			rate := c_adrr[1];

		eg_num_sustain:
			if c_et = 0 then
				rate := c_adrr[2];

		eg_num_release:
			if son[ch_offset[cycles]] <> 0 then
				rate := 5
			else
				rate := c_adrr[2];
	end;

	if ((eg_kon and 1) = 0) and (mcsel = 0) and
		(rm_select <> rm_num_tom) and (rm_select <> rm_num_hh) then
			rate := 0;

	if ((eg_kon and 1) <> 0) and (eg_state[cycles] = eg_num_release) and ((eg_off and 1) = 0) then
		rate := 12;

	if ((eg_kon and 1) = 0) and (son[ch_offset[cycles]] = 0) and (mcsel = 1) and (c_et = 0) then
		rate := 7;

	eg_zerorate := (rate = 0);
	ksr := c_ksr_freq;
	if (c_ksr = 0) then
		ksr := ksr >> 2;
	eg_rate := (rate << 2) + ksr;
	if (eg_rate and $40) <> 0 then
		eg_rate := $3c or (ksr and 3);
	eg_sl := c_sl;
end;

procedure TOPLL.DoChannel;
begin
	if (((cycles div 3) and 1) <> 0) or
		( ((rm_enable and $40) <> 0) and (((cycles + 15) mod 18) >= 12) ) then
			output_m := 0
	else
	begin
		output_m := ch_out;
		if output_m >= 0 then
			Inc(output_m);
	end;
end;

procedure TOPLL.DoOperator;
var
	ismod1, ismod2, ismod3: Boolean;
	exp_shift: Word;
	output: Int16;
	opmod: Integer;
	level, phase: Cardinal;
begin
	if ((rm_enable and $80) <> 0) and ((cycles = 15) or (cycles = 16)) then
		ismod1 := False
	else
		ismod1 := (((cycles + 1) div 3) and 1) <> 0;

	if ((rm_enable and $40) <> 0) and ((cycles = 13) or (cycles = 14)) then
		ismod2 := False
	else
		ismod2 := (((cycles + 3) div 3) and 1) <> 0;

	if ((rm_enable and $40) <> 0) and ((cycles = 16) or (cycles = 17)) then
		ismod3 := False
	else
		ismod3 := ((cycles div 3) and 1) <> 0;

	opmod := 0;

	if (ismod3) then
		opmod := op_mod << 1;

	if (ismod2) and (c_fb <> 0) then
		opmod := opmod or ((op_fbsum) >> (7 - c_fb));

	exp_shift := op_exp_s;
	if (eg_silent) or ( ((op_neg and 2) <> 0) and
		(IfThen(ismod1, c_dm and 4, c_dc and 4) <> 0) ) then
			exp_shift := exp_shift or 12;

	output := op_exp_m >> exp_shift;
	if (not eg_silent) and ((op_neg and 2) <> 0) then
		output := not output;

	level := Min(op_logsin + Cardinal(eg_out << 4), 4095);

	op_exp_m := exprom[level and $FF];
	op_exp_s := level >> 8;

	phase := (opmod + pg_out) and $3FF;
	if (phase and $100) <> 0 then
		phase := phase xor $FF;

	op_logsin := logsinrom[phase and $FF];
	op_neg := (op_neg << 1) or (phase >> 9);
	op_fbsum := (op_fb1[(cycles+3) mod 9] + op_fb2[(cycles+3) mod 9]) >> 1;

	if (ismod1) then
	begin
		op_fb2[cycles mod 9] := op_fb1[cycles mod 9];
		op_fb1[cycles mod 9] := output;
	end;

	op_mod := output and $1FF;

	// !!! TODO FIXME: output should be shifted right by only 3 bits instead of five,
	// otherwise it results in horrible clipping and distortion.
	// Have tried investigating but haven't found the error yet. 2021-08-13
	//
	ch_out := IfThen(ismod1, 0, output >> 5);
end;

procedure TOPLL.DoRhythm;
var
	nbit: Byte;
begin
	// Noise
	nbit := ((rm_noise xor (rm_noise >> 14)) and 1) or
		(BoolVal[rm_noise = 0] or ((testmode >> 1) and 1));
	rm_noise := (nbit << 22) or (rm_noise >> 1);
end;

procedure TOPLL.DoLFO;
var
	vib_step, am_bit: Byte;
	am_inc: Boolean = False;
begin
	// Update counter
	if (cycles = 17) then
	begin
		vib_step := ((lfo_counter and $3ff) + 1) >> 10;
		lfo_am_step := ((lfo_counter and $3f) + 1) >> 6;
		vib_step := vib_step or ((testmode >> 3) and $01);
		lfo_vib_counter := (lfo_vib_counter + vib_step) and 7;
		Inc(lfo_counter);
	end;

	// LFO AM
    if (cycles < 9) and ((lfo_am_step <> 0) or ((testmode and $08) <> 0)) then
		am_inc := (lfo_am_dir <> 0) or (cycles = 0);

	if (cycles >= 9) then
		lfo_am_car := 0;

	if (cycles = 0) then
	begin
		if (lfo_am_dir <> 0) and ((lfo_am_counter and $7f) = 0) then
			lfo_am_dir := 0
		else
		if (lfo_am_dir = 0) and ((lfo_am_counter and $69) = $69) then
			lfo_am_dir := 1;
	end;

	am_bit := (lfo_am_counter and $01) + BoolVal[am_inc] + lfo_am_car;
	lfo_am_car := am_bit >> 1;
	am_bit := am_bit and 1;
	lfo_am_counter := (am_bit << 8) or (lfo_am_counter >> 1);

	// Reset LFO
	if (testmode and $02) <> 0 then
	begin
		lfo_vib_counter := 0;
		lfo_counter := 0;
		lfo_am_dir := 0;
		lfo_am_counter := lfo_am_counter and $ff;
	end;
end;

function TOPLL.Clock: SmallInt;
begin
	Result := output_m;

	if (cycles = 0) then
		lfo_am_out := (lfo_am_counter >> 3) and $F;
	rm_enable := rm_enable >> 1;

	DoModeWrite;

	Inc(rm_select);
	if (rm_select > rm_num_tc) then
		rm_select := rm_num_tc + 1;

	if (cycles = 11) and ((rm_enable and $80) = $80) then
		rm_select := rm_num_bd0;

	PreparePatch1;
	DoChannel;
	PhaseGenerate;
	DoOperator;
	PhaseCalcIncrement;
	EnvelopeOutput;
	EnvelopeKSLTL;
	EnvelopeGenerate;
	DoLFO;
	DoRhythm;
	PreparePatch2;
	DoRegWrite;
	DoIO;

	cycles := (cycles + 1) mod 18;
end;

procedure TOPLL.Write(_port: Cardinal; _data: Byte);
begin
	write_data := _data;
	if (_port and 1) <> 0 then
		write_d := write_d or 1  // data
	else
		write_a := write_a or 1; // address
end;


end.

