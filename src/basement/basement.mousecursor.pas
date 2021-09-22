unit Basement.MouseCursor;

interface

uses
	Classes, Types, Graphics32;

type
	TMouseCursor = class
	private
		function 	ValidCoords(var X, Y: Integer): Boolean;
	public
		Show,
		InWindow: 	Boolean;
		Buffer,
		Bitmap,
		Background:	TBitmap32;
		Size,
		HotSpot,
		Scaling,
		OldPos,
		Pos: 		TPoint;

		procedure	Draw;
		procedure	Erase;
		procedure 	SetImage(const Filename: String);
		constructor	Create(const aBuffer: TBitmap32; const Filename: String);
		destructor	Destroy; override;
	end;

var
	Mouse: TMouseCursor;


implementation


constructor TMouseCursor.Create(const aBuffer: TBitmap32; const Filename: String);
begin
	inherited Create;

	Bitmap := TBitmap32.Create;
	Background := TBitmap32.Create;
	Buffer := aBuffer;

	SetImage(Filename);

	HotSpot.X := 0;
	HotSpot.Y := 0;

	Pos := Types.Point(-1, -1);
	InWindow := False;
	Show := False;
end;

destructor TMouseCursor.Destroy;
begin
	Bitmap.Free;
	Background.Free;

	inherited;
end;

procedure TMouseCursor.SetImage(const Filename: String);
begin
	Bitmap.LoadFromFile(Filename);

	Size.X := Bitmap.Width;
	Size.Y := Bitmap.Height;

	Background.SetSize(Size.X, Size.Y);
end;

function TMouseCursor.ValidCoords(var X, Y: Integer): Boolean;
begin
	X := Pos.X - HotSpot.X;
	Y := Pos.Y - HotSpot.Y;
	Result := (Show) and (InWindow) and
		(X >= 0) and (Y >= 0) and
		(X < Buffer.Width) and (Y < Buffer.Height);
end;

procedure TMouseCursor.Draw;
var
	X, Y: Integer;
begin
	if ValidCoords(X, Y) then
	begin
		Background.Draw(0, 0, Bounds(X, Y, Size.X, Size.Y), Buffer);
		Buffer.DrawColorKey(X, Y, $FFFF00FF, Bitmap);
	end;
end;

procedure TMouseCursor.Erase;
var
	X, Y: Integer;
begin
	if ValidCoords(X, Y) then
		Buffer.Draw(X, Y, Background);
end;

end.
