      {******************************************************************}
      { GDIPPathText                                                     }
      {                                                                  }
      { home page : http://www.mwcs.de                                   }
      { email     : martin.walter@mwcs.de                                }
      {                                                                  }
      { date      : 30-11-2007                                           }
      {                                                                  }
      { version   : 1.0                                                  }
      {                                                                  }
      { Use of this file is permitted for commercial and non-commercial  }
      { use, as long as the author is credited.                          }
      { This file (c) 2007 Martin Walter                                 }
      {                                                                  }
      { This Software is distributed on an "AS IS" basis, WITHOUT        }
      { WARRANTY OF ANY KIND, either express or implied.                 }
      {                                                                  }
      { *****************************************************************}

unit GDIPPathText;

interface

uses
  Winapi.GDIPAPI, Winapi.GDIPOBJ, GDIPKerning;

type
  TPathPosition = Single;
  
  TGPPathText = class(TObject)
  strict private
    FRotation: Single;
    FGuidePath: TGPGraphicsPath;

    FFamily: TGPFontFamily;
    FStyle: Integer;
    FSize: Single;
    FFormat: TGPStringFormat;
    FDistanceFactor: Single;
    FKerningFactor: Single;
    FAdditionalMatrix: TGPMatrix;

    function AddGlyphToPath(const Path: TGPGraphicsPath; const Char: WideChar;
      const Family: TGPFontFamily; const Style: Integer; const Size: Single;
      const Origin: TGPPointF; const Format: TGPStringFormat): TStatus;

    function AddCharacter(const Current, Next: WideChar;
      const Path: TGPGraphicsPath; const Position: TPathPosition): TPathPosition;

    function GetPathPoint(const Position: TPathPosition): TGPPointF;
    function GetPathPointLength(const Position: TPathPosition): Single;
    function GetPathPosition(Indent: Single): TPathPosition;
    function FindRightPosition(CenterPos: TPathPosition;
      const Radius: Single): TPathPosition;
  protected
  public
    constructor Create(const GuidePath: TGPGraphicsPath;
      const Flatness: Single = 10 * FlatnessDefault);

    destructor Destroy; override;

    function AddPathText(const Path: TGPGraphicsPath;
      const Text: string; const Indent: Single;
      const Family: TGPFontFamily; Style: Integer;
      const Size: Single; const Format: TGPStringFormat;
      const DistanceFactor: Single = 1; const KerningFactor: Single = 1): Single;

    class function GetPathLength(const Path: TGPGraphicsPath): Single;

    property Rotation: Single read FRotation write FRotation;
    property AdditionalMatrix: TGPMatrix read FAdditionalMatrix write FAdditionalMatrix;
  end;

implementation

uses
  System.Math, System.SysUtils;

function GetPoint(P: PGPPointF; Index: Integer): TGPPointF;
begin
  Inc(P, Index);
  Result := P^;
end;

function AddPoint(A, B: TGPPointF): TGPPointF;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

function SubPoint(A, B: TGPPointF): TGPPointF;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

function GetIntersectionFromCircle(PtA, PtB, Center: TGPPointF;
  const R2: Single): Single;
var
 Diff: TGPPointF;
 rA2, rB2: Single;
 A, B, C, D, T, T1, T2, SqrtD: Single;
begin
  PtA := SubPoint(PtA, Center);
  PtB := SubPoint(PtB, Center);

	rA2 := Sqr(PtA.X) + Sqr(PtA.Y);
	rB2 := Sqr(PtB.X) + Sqr(PtB.Y);

	if (rA2 > R2) and (rB2 > R2) then
  begin
    Result := -1;
    Exit;
  end;

	if (rA2 < R2) and (rB2 < R2) then
  begin
    Result := -1;
    Exit;
  end;

	Diff := SubPoint(PtB, PtA);

	A := Sqr(Diff.X) + Sqr(Diff.Y);
	B := 2 * (PtA.X * Diff.X + PtA.Y * Diff.Y);
	C := rA2 - R2;
  D := Sqr(B) - 4 * A * C;

	T := -1;

  A := 2 * A;
	if (D = 0) then
    T := -B / A
	else
    if (D > 0) then
	  begin
      SqrtD := Sqrt(D);
      T1 := (-B + SqrtD) / A;
      T2 := (-B - SqrtD) / A;

      if (T1 >= 0) and (T1 <= 1) then
      begin
        if (T2 > 0) and (T2 < T1) then
          T := T2
        else
          T := T1;
      end
      else
        if (T2 >= 0) and (T2 <= 1) then
          T := T2;
    end;
	Result := T;
end;


{ TPathText }

function TGPPathText.AddCharacter(const Current, Next: WideChar;
  const Path: TGPGraphicsPath; const Position: TPathPosition): TPathPosition;
var
  CharWidth: Single;
  GlyphPath: TGPGraphicsPath;
  Left, Right, Diff: TGPPointF;
  SinAngle, CosAngle: Single;
  Matrix: TGPMatrix;
  PosRight: TPathPosition;
begin
  GlyphPath := TGPGraphicsPath.Create;
  try
    CharWidth := KerningText.GetCellWidth(Word(Current), Word(Next),
      FDistanceFactor, FKerningFactor);

    if (CharWidth = 0) then
    begin
      Result := -1;
      Exit;
    end;

    PosRight := FindRightPosition(Position, CharWidth);
    if (PosRight < 0) then
    begin
      Result := PosRight;
      Exit;
    end;

    Left := GetPathPoint(Position);
    Right := GetPathPoint(PosRight);

    Diff := SubPoint(Right, Left);

    CosAngle := Diff.X / CharWidth;
    SinAngle := Diff.Y / CharWidth;

    AddGlyphToPath(GlyphPath, Current, FFamily, FStyle, FSize,
      MakePoint(0, -FSize), FFormat);

    if Assigned(FAdditionalMatrix) then
      GlyphPath.Transform(FAdditionalMatrix);

    Matrix := TGPMatrix.Create(CosAngle, SinAngle,
      - Rotation * SinAngle, 1 + Rotation * (CosAngle - 1),
      Left.X, Left.Y);
    try
      GlyphPath.Transform(Matrix);
    finally
      Matrix.Free;
    end;

    Path.AddPath(GlyphPath, False);
    Result := PosRight;
  finally
    GlyphPath.Free;
  end;
end;

function TGPPathText.AddGlyphToPath(const Path: TGPGraphicsPath;
  const Char: WideChar; const Family: TGPFontFamily; const Style: Integer;
  const Size: Single; const Origin: TGPPointF;
  const Format: TGPStringFormat): TStatus;
begin
  Result := Path.AddString(Char, -1, Family, Style, Size, Origin, Format);
end;

function TGPPathText.AddPathText(const Path: TGPGraphicsPath;
  const Text: string; const Indent: Single;
  const Family: TGPFontFamily; Style: Integer;
  const Size: Single; const Format: TGPStringFormat;
  const DistanceFactor: Single = 1; const KerningFactor: Single = 1): Single;
var
  IndentPosition, Position: TPathPosition;
  Current, Next: PWideChar;
begin
  Result := 0;
  Path.SetFillMode(FillModeWinding);

  IndentPosition := GetPathPosition(Indent);
  Position := IndentPosition;

  Current := PWideChar(Text);

  FFamily := Family;
  FStyle := Style;
  FSize := Size;
  FFormat := Format;
  FDistanceFactor := DistanceFactor;
  FKerningFactor := KerningFactor;

  KerningText.Prepare(FFamily, FStyle, FSize, FFormat);
  try
    while (Current^ <> #0) and (Position >= 0) do
    begin
      Next := Current + 1;
      Position := AddCharacter(Current^, Next^, Path, Position);
      if Position >= 0 then
        Result := Position;
      Inc(Current);
    end;
  finally
    KerningText.Unprepare;
  end;
  if Result > 0 then
    Result := GetPathPointLength(Result - IndentPosition);
end;

constructor TGPPathText.Create(const GuidePath: TGPGraphicsPath;
  const Flatness: Single);
begin
  if not Assigned(GuidePath) then
    Exception.Create('Path is invalid');

  inherited Create;

  FGuidePath := GuidePath.Clone;
  FRotation := 1;
  FGuidePath.Flatten(nil, Flatness);
end;

destructor TGPPathText.Destroy;
begin
  FGuidePath.Free;
  inherited;
end;

function TGPPathText.FindRightPosition(CenterPos: TPathPosition;
  const Radius: Single): TPathPosition;
var
  StartSegment: Integer;
  PD: TPathData;
  DistLeft, DistRight: Single;
  Start: TGPPointF;
  Diff: TGPPointF;
  P1, P2: TGPPointF;
  C, PointCount: Integer;
  Intersection: Single;
begin
	if (CenterPos < 0) then
  begin
    Result := -1;
    Exit;
  end;

	StartSegment := Floor(CenterPos);

  PointCount := FGuidePath.GetPointCount;
	if (StartSegment >= PointCount - 1) then
  begin
    Result := -1;
    Exit;
  end;

	PD := TPathData.Create;
  try
    if (FGuidePath.GetPathData(PD) = Ok) then
    begin
      Start := GetPathPoint(CenterPos);

      P1 := GetPoint(PD.Points, StartSegment + 1);

      Diff := SubPoint(Start, P1);
      DistRight := Sqrt(Sqr(Diff.X) + Sqr(Diff.Y));

      if (Radius < DistRight) then
      begin
        Diff := SubPoint(Start, GetPoint(PD.Points, StartSegment));
        DistLeft := Sqrt(Sqr(Diff.X) + Sqr(Diff.Y));

        Result := StartSegment + 1 - (DistRight - Radius) / (DistRight + DistLeft);
        Exit;
      end;

      for C := StartSegment + 1 to PointCount - 2 do
      begin
        P2 := GetPoint(PD.Points, C + 1);
        Intersection := GetIntersectionFromCircle(P1, P2, Start, Sqr(Radius));
        P1 := P2;

        if (Intersection >= 0) then
        begin
          Result := C + Intersection;
          Exit;
        end;
      end;
    end;
    Result := -1;
  finally
    PD.Free;
  end;
end;

class function TGPPathText.GetPathLength(const Path: TGPGraphicsPath): Single;
var
  P: TGPGraphicsPath;
  Count, C: Integer;
  PD: TPathData;
  P1, P2: TGPPointF;
begin
  Result := 0;
  P := Path.Clone;
  try
    P.Flatten(nil, 10 * FlatnessDefault);

    Count := P.GetPointCount;
    if Count > 0 then
    begin
      PD := TPathData.Create;
      try
        if (P.GetPathData(PD) = Ok) then
        begin
          P1 := GetPoint(PD.Points, 0);
          for C := 0 to Count - 2 do
          begin
            P2 := GetPoint(PD.Points, C + 1);
            P1 := SubPoint(P2, P1);
            Result := Result + Sqrt(Sqr(P1.X) + Sqr(P1.Y));
            P1 := P2;
          end;
        end;
      finally
        PD.Free;
      end;
    end;
  finally
    P.Free;
  end;
end;

function TGPPathText.GetPathPoint(const Position: TPathPosition): TGPPointF;
var
  R: TGPPointF;
  Segment, Count: Integer;
  PD: TPathData;
  Diff: TGPPointF;
  T: Single;
begin
  R := MakePoint(0.0, 0);

  if Position < 0 then
  begin
    Result := R;
    Exit;
  end;

  Segment := Floor(Position);

  Count := FGuidePath.GetPointCount;
	if (Segment < Count - 1) then
  begin
    PD := TPathData.Create;
		if (FGuidePath.GetPathData(PD) = Ok) then
		begin
			R := GetPoint(PD.Points, Segment);
			Diff := GetPoint(PD.Points, Segment + 1);

      Diff := SubPoint(Diff, R);

			T := Frac(Position);

			R.X := R.X + T * Diff.X;
			R.Y := R.Y + T * Diff.Y;
		end;
    PD.Free;
  end;

	Result := R;
end;

function TGPPathText.GetPathPointLength(const Position: TPathPosition): Single;
var
  P1, P2: TGPPointF;
  Diff: TGPPointF;
  C, Segment, Count: Integer;
  PD: TPathData;
begin
  if Position < 0 then
  begin
    Result := 0;
    Exit;
  end;

  Segment := Floor(Position);

  Result := 0;
  Count := FGuidePath.GetPointCount;
	if (Segment < Count - 1) then
  begin
    PD := TPathData.Create;
    try
      if (FGuidePath.GetPathData(PD) = Ok) then
      begin
        P1 := GetPoint(PD.Points, 0);
        for C := 0 to Segment - 1 do
        begin
          P2 := GetPoint(PD.Points, C + 1);
          Diff := SubPoint(P2, P1);
          Result := Result + Sqrt(Sqr(Diff.X) + Sqr(Diff.Y));
          P1 := P2;
        end;

        P2 := GetPoint(PD.Points, Segment + 1);
        Diff := SubPoint(P2, P1);

        Result := Result + Sqrt(Sqr(Diff.X) + Sqr(Diff.Y)) * Frac(Position);
      end;
    finally
      PD.Free;
    end;
  end;
end;

function TGPPathText.GetPathPosition(Indent: Single): TPathPosition;
var
  PD: TPathData;
  C, Count: Integer;
  A, B: TGPPointF;
  Distance: Single;
begin
  PD := TPathData.Create;
  try
    if (FGuidePath.GetPathData(PD) = Ok) then
    begin
      Count := FGuidePath.GetPointCount;
      A := GetPoint(PD.Points, 0);
      for C := 0 to Count - 2 do
      begin
        B := GetPoint(PD.Points, C + 1);

        Distance := Sqrt(Sqr(B.X - A.X) + Sqr(B.Y - A.Y));
        A := B;

        if (Indent < Distance) then
        begin
          Result := C + Indent / Distance;
          Exit;
        end;

        Indent := Indent - Distance;
      end;
    end;
  finally
    PD.Free;
  end;
  Result := -1;
end;

end.
