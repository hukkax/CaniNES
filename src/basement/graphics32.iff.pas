unit Graphics32.IFF;

interface

uses
	Classes, SysUtils, Graphics32;

type
	TPalette = array[0..255] of TColor32;

	TPaletteObject = class
	public
		Palette: 	TPalette;
		First,
		Last:		Integer;

		procedure AddColorChange(data, register_offset: Cardinal);
	end;

	TIFFImage = class
	private
		cmap_bits:		Integer;
		cmap_length:	Integer;
		colormap:		TPalette;
		color_change_lists: array of TPaletteObject;

		left,
		top,
		bitplanes,
		masking,
		compress,
		padding,
		transparent_index,
		x_aspect,
		y_aspect,
		page_width,
		page_height:	Integer;

		flagPCHG,
		flagPBM,
		flagHAM,
		flagEHB,
		flagHires,
		flagLace:		Boolean;

		Data:			RawByteString;
		Filesize:		Cardinal;

		function 		ResolvePixels(value: Cardinal; previous_color: TColor32): Cardinal;

		function		IsCMAPScaled: Boolean;
		procedure		ScaleCMAP;

		procedure		SetError(const S: String);

		procedure 		ParsePCHG(p, size: Integer);
		procedure 		LineStart(line: Word);
	public
		Width,
		Height:			Integer;
		{$IFDEF DEBUG}
		Errors:			TStrings;
		{$ENDIF}

		Bitmap:			TBitmap32;

		function		LoadFromFile(const Filename: String): Boolean;
		function		LoadLBM(LoadBitmap: Boolean = True): Boolean;

		function 		LoadFromStream(Stream: TStream): Boolean;

		constructor		Create;
		destructor		Destroy; override;

		function		GetBitmap: TBitmap32;
		procedure 		DrawTo(var Buffer: TBitmap32);
	end;

	{$IFDEF DEBUG}
	function LoadIFF(const Filename: String; Debug: TStrings): TBitmap32; overload;
	{$ELSE}
	function LoadIFF(const Filename: String): TBitmap32; overload;
	{$ENDIF}
	function LoadIFF(const Stream: TStream; var Bitmap: TBitmap32): Boolean; overload;


implementation

uses
	Math,
	hkaFileUtils;


{$IFDEF DEBUG}
{$ENDIF}

{$IFDEF DEBUG}
function LoadIFF(const Filename: String; Debug: TStrings): TBitmap32;
{$ELSE}
function LoadIFF(const Filename: String): TBitmap32;
{$ENDIF}
var
	IFF: TIFFImage;
begin
	IFF := TIFFImage.Create;
	if IFF.LoadFromFile(Filename) then
		Result := IFF.GetBitmap
	else
		Result := nil;

	{$IFDEF DEBUG}
	if Debug <> nil then
	begin
		Debug.Clear;

		if IFF.Errors.Count > 0 then
		begin
			Debug.AddStrings(IFF.Errors);
			Debug.Add('');
		end;

		Debug.Add(Format('Dimensions: %d * %d', [IFF.Width, IFF.Height]));
		Debug.Add(Format('Bitplanes:  %d (%d colors)', [IFF.bitplanes, IFF.cmap_length]));
		Debug.Add(Format('Compression=%d  masking=%d', [IFF.compress, IFF.masking]));
		Debug.Add(Format('Page dimensions: %d * %d', [IFF.page_width, IFF.page_height]));
		Debug.Add(Format('Ratio x=%d  y=%d', [IFF.x_aspect, IFF.y_aspect]));

		if IFF.flagPBM then Debug.Add('<PBM>');
		if IFF.flagHAM then Debug.Add('<HAM>');
		if IFF.flagEHB then Debug.Add('<EHB>');
		if IFF.flagHires then Debug.Add('<Hires>');
		if IFF.flagLace  then Debug.Add('<Lace>');

		//Debug.Add(Format('', []));

{		Debug.Add('Palette:');
		for i := 0 to IFF.cmap_length-1 do
			Debug.Add(IntToHex(i, 2) + '=' + IntToHex(IFF.colormap[i], 8));

		Debug.Add('First scanline:');
		S := '';
		for i := 0 to IFF.Width-1 do
			S := S + IntToHex(IFF.Bitmap.Value[i,0], 2) + ' ';
		Debug.Add(S);}
	end;
	{$ENDIF}

	IFF.Free;
end;

function LoadIFF(const Stream: TStream; var Bitmap: TBitmap32): Boolean;
var
	IFF: TIFFImage;
begin
	IFF := TIFFImage.Create;
	Result := IFF.LoadFromStream(Stream);
	if Result then
	begin
		if Assigned(Bitmap) then
			FreeAndNil(Bitmap);
		Bitmap := IFF.GetBitmap;
	end;
	IFF.Free;
end;

constructor TIFFImage.Create;
begin
	inherited Create;
	{$IFDEF DEBUG}
	Errors := TStringList.Create;
	{$ENDIF}
end;

destructor TIFFImage.Destroy;
var
	i: Integer;
begin
	{$IFDEF DEBUG}
	Errors.Free;
	{$ENDIF}
	if Assigned(Bitmap) then
		Bitmap.Free;
	for i := 0 to Length(color_change_lists)-1 do
		if color_change_lists[i] <> nil then
			color_change_lists[i].Free;
	inherited Destroy;
end;

function TIFFImage.LoadFromFile(const Filename: String): Boolean;
begin
	Data := FileToString(Filename);
	Filesize := Length(Data);
	Result := LoadLBM(True);
end;

function PadTo8bits(value: Byte; bits: Integer): Byte;
var
	I: Integer;
begin
	Result := 0;
	I := 8 - bits;
	while I >= 0 do
	begin
		Result := Result or (value shl i);
		I := I - bits;
	end;
end;

function TIFFImage.IsCMAPScaled: Boolean;
var
	scale_mask, i: Integer;
	col: TColor32Entry;
begin
	scale_mask := (1 shl cmap_bits) - 1;
	for i := 0 to cmap_length-1 do
	begin
		col := TColor32Entry(colormap[i]);
		if (col.R and scale_mask) <> 0 then Exit(True);
		if (col.G and scale_mask) <> 0 then Exit(True);
		if (col.B and scale_mask) <> 0 then Exit(True);
	end;
	Result := False;
end;

procedure TIFFImage.ScaleCMAP;
var
	i: Integer;
begin
	for i := 0 to cmap_length-1 do
	begin
		with TColor32Entry(colormap[i]) do
		begin
			R := PadTo8bits(R shr (8 - cmap_bits), cmap_bits);
			G := PadTo8bits(G shr (8 - cmap_bits), cmap_bits);
			B := PadTo8bits(B shr (8 - cmap_bits), cmap_bits);
		end;
	end;
end;

procedure TIFFImage.SetError(const S: String);
begin
	{$IFDEF DEBUG}
	if S <> '' then
		Errors.Add(S);
	{$ENDIF}
end;

procedure TIFFImage.LineStart(line: Word);
var
	i: Integer;
	pal: TPaletteObject;
begin
	pal := color_change_lists[line];
	if (pal <> nil) then
	begin
		if not (pal.First in [0..255]) then Exit;
		for i := pal.First to pal.Last do
			if pal.Palette[i] <> 0 then
				colormap[i] := pal.Palette[i];
		{SetError(Format('CHG Line %d Range=%d-%d (%d colors)',
		[line, pal.First, pal.Last, pal.Last-pal.First+1]));}
	end;
end;

procedure TPaletteObject.AddColorChange(data, register_offset: Cardinal);
var
	Index, R, G, B: Integer;
begin
	Index := ((data and $F000) shr 12) + register_offset;
	if (Index > 255) then Exit;

	R := (data and $F00) shr 8;
	G := (data and $0F0) shr 4;
	B := (data and $00F);

	if (First < 0) or (Index < First) then
	begin
		if First < 0 then Last := Index;
		First := Index;
	end
	else
	if Index > Last then
		Last := Index;

	Palette[Index] := Color32(padTo8bits(R, 4), padTo8bits(G, 4), padTo8bits(B, 4));
end;

procedure TIFFImage.ParsePCHG(p, size: Integer);
var
	i, j, origp, idx, compression, flags, start_line, line_count,
	min_register, max_register: Integer;
	line_bitmap, mask: Cardinal;
	{$IFDEF DEBUG}
	change_filled_count: Integer;
	{$ENDIF}
	change_count, word_count, tree_size, full_size: Integer;
	pal: TPaletteObject;
begin
	origp := p;

	compression  := GetVal16R(data, p);
	flags        := GetVal16R(data, p);
	start_line   := GetVal16R(data, p);
	line_count   := GetVal16R(data, p);
	min_register := GetVal16R(data, p);
	max_register := GetVal16R(data, p);
	tree_size    := GetVal16R(data, p);
	Inc(p, 2);   // reserved word in between
	full_size    := GetVal32R(data, p);

	{$IFDEF DEBUG}
	SetError(Format('Compression: %d', [compression]));
	SetError(Format('Lines: %d (%d)', [start_line, line_count]));
	SetError(Format('Size: %d / %d', [tree_size, full_size]));
	{$ENDIF}

	word_count := (line_count + 31) shr 5;
	SetLength(color_change_lists, Height);
	for i := 0 to Height-1 do
		color_change_lists[i] := nil;

	change_count := 0;

	for i := 0 to word_count-1 do
	begin
		line_bitmap := Cardinal(GetVal32R(data, p));
		for j := 0 to 31 do
		begin
			mask := 1 shl (31 - j);
			if (mask and line_bitmap) <> 0 then
			begin
				idx := start_line + (i * 32) + j;
				if idx < Height then
				begin
					pal := TPaletteObject.Create;
					pal.First := -1;
					color_change_lists[idx] := pal;
					for idx := 0 to 255 do
						pal.Palette[idx] := 0;
					Inc(change_count);
				end;
			end;
		end;
	end;

	{$IFDEF DEBUG}
	SetError('Changes: ' + IntToStr(change_count));
	change_filled_count := 0;
	{$ENDIF}

	for i := 0 to Height-1 do
	begin
		if ((p - origp) >= size) then Break;

		pal := color_change_lists[i];
		if pal = nil then Continue;

		change_count := GetVal8(data, p);
		idx := GetVal8(data, p);

		if change_count > 0 then
		for j := 0 to change_count-1 do
			pal.AddColorChange(GetVal16R(data, p), 0);

		if idx > 0 then
		for j := 0 to idx-1 do
			pal.AddColorChange(GetVal16R(data, p), 16);

		{$IFDEF DEBUG}
		Inc(change_filled_count);
		{$ENDIF}
	end;

	{$IFDEF DEBUG}
	SetError('Filled-in changes: ' + IntToStr(change_filled_count));
	{$ENDIF}
end;

function TIFFImage.LoadLBM(LoadBitmap: Boolean = True): Boolean;
var
	bit_buffer: array of Cardinal;
	id: RawByteString;
	sb: ShortInt;
	b: Byte;
	i, j, x, y, h, size, bytesloaded, row_bytes, viewmodes, stencil: Uint32;
	p, iterations, planes, pixel_mask: Integer;
	plane_mask: Cardinal;
begin
	Result := False;
	p := 1;	// read position

	id := GetString(Data, p, 4);
	if id <> 'FORM' then
	begin
		SetError('Not a valid IFF file.');
		Exit;
	end;

	// Should be the size of the file minus 4+4 ('FORM' + size)
	size := GetVal32R(Data, p) + 8;
	if size <> Filesize then
	begin
		SetError('Invalid filesize in IFF header.');
		//Exit;
	end;

	// File format : PBM=Packed Bitmap, ILBM=Interleaved Bitmap
	id := Trim(GetString(Data, p, 4));
	flagPBM := (id = 'PBM');
	if (not flagPBM) and (id <> 'ILBM') then
	begin
		SetError('Not an image file.');
		Exit;
	end;

	flagHAM := False;
	flagEHB := False;
	flagPCHG := False;

	bitplanes := 0;
	cmap_length := 0;
	iterations := 0; // prevent infinite loop

	while (id <> 'BODY') and (iterations < 30) do
	begin
		if (p mod 2) = 0 then Inc(p);

		id := GetString(Data, p, 4);
		Inc(iterations);

		size := GetVal32R(Data, p);
		bytesloaded := 0;

		if (id = 'BMHD') then	// Bitmap header
		begin
			Width  := GetVal16R(Data, p);
			Height := GetVal16R(Data, p);
			left   := GetVal16R(Data, p);
			top    := GetVal16R(Data, p);
			bitplanes := GetVal8(Data, p);
			masking   := GetVal8(Data, p);
			compress  := GetVal8(Data, p);
			padding := GetVal8(Data, p);
			transparent_index := GetVal16R(Data, p);
			x_aspect := GetVal8(Data, p);
			y_aspect := GetVal8(Data, p);
			page_width  := GetVal16R(Data, p);
			page_height := GetVal16R(Data, p);
		end
		else
		if (id = 'CMAP') then	// Palette (Color Map)
		begin
			cmap_length := size div 3;
			for i := 0 to cmap_length-1 do
				with TColor32Entry(colormap[i]) do
				begin
					R := GetVal8(data, p);
					G := GetVal8(data, p);
					B := GetVal8(data, p);
				end;

			SetError('cmap_length=' + inttostr(cmap_length));

			if cmap_length in [9..15]  then
			begin
				cmap_length := 16;
				SetError('cmap_length fixed to ' + inttostr(cmap_length));
			end
			else
			if cmap_length in [17..31] then
			begin
				cmap_length := 32;
				SetError('cmap_length fixed to ' + inttostr(cmap_length));
			end
			else
			if cmap_length in [33..63] then
			begin
				cmap_length := 64;
				SetError('cmap_length fixed to ' + inttostr(cmap_length));
			end;

			cmap_bits := 0;
			while (cmap_length > (1 shl cmap_bits)) do
				Inc(cmap_bits);
			SetError('cmap_bits=' + inttostr(cmap_bits));

			if not IsCMAPScaled then
			begin
				SetError('* ScaleCMAP()');
				ScaleCMAP;
			end;
		end
		else
		if (id = 'CAMG') then	// Amiga ViewMode
		begin
			viewmodes := GetVal32R(Data, p);
			flagHires := (viewmodes and $8000) <> 0;
			flagHAM   := (viewmodes and $0800) <> 0;
			flagEHB   := (viewmodes and $0080) <> 0;
			flagLace  := (viewmodes and $0004) <> 0;

			if (flagEHB) and (cmap_bits = bitplanes) then
			begin
				Dec(cmap_bits);
				cmap_length := cmap_length shr 1;
			end;

			if (flagHAM) and (cmap_bits > (bitplanes - 2)) then
			begin
				j := (cmap_bits - bitplanes + 2);
				Dec(cmap_bits, j);
				cmap_length := cmap_length shr j;
			end;
		end
		else
		if (id = 'PCHG') then
		begin
			flagPCHG := True;
			ParsePCHG(p, size);
			if (size and 1) <> 0 then Inc(size);	// padding
			Dec(size, bytesloaded);
			// skip the remaining bytes of this chunk
			if (size <> 0) then Inc(p, size);
		end
		else
		if (id <> 'BODY') then
		begin
			if (size and 1) <> 0 then Inc(size);	// padding
			Dec(size, bytesloaded);
			// skip the remaining bytes of this chunk
			if (size <> 0) then Inc(p, size);
		end;

	end; // 'BODY'

	if (Width < 1) or (Height < 1) then
	begin
		SetError('Invalid size');
		Exit;
	end;

	stencil := (masking and 1);	// There is a mask ('stencil')
	if stencil <> 0 then
		SetError('stencil=' + IntToStr(stencil));


	if not LoadBitmap then Exit(False);


	row_bytes := ((Width + 15) div 16) * 2;

	SetLength(bit_buffer, Height * row_bytes * (bitplanes+1));

	Bitmap := TBitmap32.Create;
	Bitmap.SetSize(Width, Height);

	if flagPBM then	// File format = Packed Bitmap
	begin
		row_bytes := row_bytes * 8;
		bitplanes := 1;
		planes := 1;
	end
	else
	begin
		planes := bitplanes;
		if masking = 1 then Inc(planes);
	end;

	if compress = 0 then
	begin
		for i := 0 to size-1 do
			bit_buffer[i] := GetVal8(Data, p);
	end
	else
	begin
		// Unpack
		//
		j := p + size;
		y := 0;
		x := Length(bit_buffer);
		for i := 0 to x-1 do
			bit_buffer[i] := 0;

		while p < j do
		begin
			sb := ShortInt(GetVal8(Data, p));
			if sb > 0 then
			begin
				for h := 0 to sb do
				begin
					if (p >= j) or (y >= x) then
						Break; //return output_offset;
					bit_buffer[y] := GetVal8(Data, p);
					Inc(y);
				end;
			end
			else
			begin
				b := GetVal8(Data, p);
				for h := 0 to Abs(sb) do
				begin
					if y >= x then Break;
					bit_buffer[y] := b;
					Inc(y);
				end;
			end;
		end;

	end;


	// Planes to pixel map
	//
	for y := 0 to Height-1 do
	begin
		for x := 0 to Width-1 do
			Bitmap[x,y] := 0;

		if flagPBM then		// File format: Packed Bitmap
		begin
			for x := 0 to ((Width + 15) and $FFFFFFF0) do  // Width in pixels modulo 16
				Bitmap[x, y] := bit_buffer[(y * row_bytes) + x];
		end
		else
		if planes > 0 then
		for p := 0 to planes-1 do
		begin
			plane_mask := 1 shl p;
			for i := 0 to row_bytes-1 do
			begin
				h := bit_buffer[(y * planes * row_bytes) + (p * row_bytes) + i];
				for j := 0 to 7 do
				begin
					pixel_mask := 1 shl (7 - j);
					if (h and pixel_mask) <> 0 then
					begin
						x := (i * 8) + j;
						//if (x >= 0) and (x < Width) then
						Bitmap[x,y] := Bitmap[x,y] or plane_mask;
					end;
				end;
			end;
		end;
	end;

	Result := True;
end;

function TIFFImage.LoadFromStream(Stream: TStream): Boolean;
begin
	Filesize := Stream.Size;
	SetLength(Data, Filesize);
	SetCodePage(Data, 1252, False);
	Stream.Read(Data[1], Filesize);
	Result := LoadLBM(True);
end;

function TIFFImage.ResolvePixels(value: Cardinal; previous_color: TColor32): Cardinal;

	function ResolveHAMPixel: TColor32;
	var
		selector, data, index: Byte;
	begin
		selector := (value and (3 shl (bitplanes-2))) shr (bitplanes-2);
		index := value and not (3 shl (bitplanes-2));
		data := index shl (8 - bitplanes + 2);

		if selector = 0 then
			Result := colormap[value]
		else
		begin
			Result := previous_color;
			case selector of
				1: TColor32Entry(Result).B := data;
				2: TColor32Entry(Result).R := data;
				3: TColor32Entry(Result).G := data;
			end;
		end;
	end;

	// Resolves an EHB encoded value into the correct color.
	// This assumes the color-table has been properly culled.
	function ResolveEHBPixel: TColor32;
	begin
		Result := colormap[value mod cmap_length];
		with TColor32Entry(Result) do
		begin
			R := R shr 1;
			G := G shr 1;
			B := B shr 1;
		end;
	end;

begin
	{if (value = undefined) then
		value := transparent_index;}

	// This breaks some images.
	{if (masking = 2) and (value = transparent_index) then
		Exit(transparent_color);}

	if flagHAM then
		Result := ResolveHAMPixel
	else
	if value < cmap_length then
		Result := colormap[value]
	else
	if flagEHB then
		Result := ResolveEHBPixel
	else
	if (value >= cmap_length) then
	begin
		Dec(value, cmap_length);
		if value < cmap_length then
			Result := colormap[value]
		else
			Result := previous_color;
	end
	else
		Result := clBlack32; //transparent_color;
end;

procedure TIFFImage.DrawTo(var Buffer: TBitmap32);
var
	ratio: Single;
	x, y {, W, H}: Integer;
	//DoubleX, DoubleY: Boolean;
	Color: TColor32;
	Car: Cardinal;
//	ScaledBuffer: TBitmap32;
begin
	if not Assigned(Buffer) then
		Buffer := TBitmap32.Create;
	Buffer.SetSize(Width, Height);

	if bitplanes = 24 then
	begin
		for y := 0 to Height-1 do
		for x := 0 to Width-1 do
		begin
			Car := Bitmap.Pixel[x, y];
			Buffer.Pixel[x, y] := Color32(
				(Car and $0000FF),
				(Car and $00FF00) shr 8,
				(Car and $FF0000) shr 16);
		end;
	end
	else
	for y := 0 to Height-1 do
	begin
		if flagPCHG then
			LineStart(y);	// modify palette dynamically
		Color := colormap[0];
		for x := 0 to Width-1 do
		begin
			Color := ResolvePixels(Bitmap.Pixel[x,y], Color);
			Buffer.Pixel[x, y] := Color;
		end;
	end;

	if y_aspect <> 0 then // some Atari files do not set the aspect fields
		ratio := x_aspect / y_aspect
	else
		ratio := 1;

	if (ratio < 1.0) then
	begin
		if ratio > 0.5 then
			ratio := 1.0
		else
		if ratio < 0.25 then
			ratio := 0.25
		else
			ratio := 0.5;
	end
	else
	if (ratio > 1.0) then
	begin
		if ratio > 3.5 then
			ratio := 4.0
		else
		if ratio >= 1.5 then
			ratio := 2.0
		else
			ratio := 1.0;
	end;

	if (ratio = 1) then
	begin
		if (flagLace) and (not flagHires) then
			ratio := 2.0
		else
			Exit;
	end;

	SetError('Final ratio: ' + FloatToStr(Ratio));
	//ratio := 1.0;

{	W := Trunc(Width  * Max(ratio, 1));
	H := Trunc(Height / Min(ratio, 1));

	if (W <> Width) or (H <> Height) then
	begin
		ScaledBuffer := TBitmap32.Create;
		ScaledBuffer.SetSize(W, H);
		ScaledBuffer.Draw(ScaledBuffer.BoundsRect, Buffer.BoundsRect, Buffer);
		Buffer.Assign(ScaledBuffer);
		ScaledBuffer.Free;
	end;}
end;

function TIFFImage.GetBitmap: TBitmap32;
begin
	Result := nil;
	DrawTo(Result);
end;


end.

