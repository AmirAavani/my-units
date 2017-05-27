unit GeomertyUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, fgl;

type

  { TPoint }

  TPoint = class(TObject)
  private
    Fx: Extended;
    Fy: Extended;
    public
      property x: Extended read Fx;
      property y: Extended read Fy;

      constructor Create(x_, y_: Extended);

      function GetDistance(const B: TPoint): Extended;
      // Returns the angle of ABC.
      function GetAngle(const A, C: TPoint): Extended;
      function Copy: TPoint;

      function ToString: AnsiString; override;
  end;

  TPoints = specialize TFPGList<TPoint>;

  { TLineSegment }

  TLineSegment = class(TObject)
    private
      Fa, Fb, Fc: Extended;
      FP, FQ: TPoint;

    public
      property a: Extended read Fa;
      property b: Extended read Fb;
      property c: Extended read Fc;
      property P : TPoint read FP;
      property Q: TPoint read FQ;

      constructor Create(const R, S: TPoint);
      destructor Destroy; override;

      function Evaluate(const R: TPoint): Extended;
      function Intersects(const L: TLineSegment): Boolean;
      // function Intersects(const L: TLineSegment; X: TPoint): Boolean;
      function ToString: AnsiString; override;
  end;

  { TCircle }

  TCircle = class(TObject)
  private
    FCenter: TPoint;
    FRadius: Extended;
    public
      property Radius: Extended read FRadius;
      property Center: TPoint read FCenter;

      constructor Create(const c: TPoint; r: Extended);
      constructor CirclePassing (const P1, P2, P3: TPoint);

  end;


  { TPolygon }

  TPolygon = class(TObject)
    private type
      TEdges = specialize TFPGList<TLineSegment>;

    private
      FVertices: TPoints;
      FEdges: TEdges;
      FMin, FMax: TPoint;

      function GetEdge(Index: Integer): TLineSegment;
      function GetVertex(Index: Integer): TPoint;
      function GetVertexCount: Integer;

    public
      property Edge[Index: Integer]: TLineSegment read GetEdge;
      property Vertex[Index: Integer]: TPoint read GetVertex;
      property VertexCount: Integer read GetVertexCount;
      property MinCoordinate: TPoint read FMin;
      property MaxCoordinate: TPoint read FMax;


      constructor Create(Str: String);
      constructor Create(Points: TPoints);
      destructor Destroy; override;

      function IsInside(const P: TPoint): Integer;
      function GetArea: Extended;
      function GetPerimeter: Extended;

      function ToString: AnsiString; override;
  end;

implementation
const
  Epsilon = 1e-5;

{ TPolygon }

function TPolygon.GetEdge(Index: Integer): TLineSegment;
begin
  Result := FEdges[Index];
end;

function TPolygon.GetVertex(Index: Integer): TPoint;
begin
  Result := FVertices.Items[Index] as TPoint;
end;

function TPolygon.GetVertexCount: Integer;
begin
  Result := FVertices.Count;

end;

constructor TPolygon.Create(Str: String);
var
  i: Integer;
  StrList: TStringList;

begin
  inherited Create;

  FVertices := TPoints.Create;
  FEdges := TEdges.Create;

  StrList := TStringList.Create;
  ExtractStrings([','], [], PChar(@Str[1]), StrList);
  for i := 0 to (StrList.Count div 2) - 1 do
    FVertices.Add(TPoint.Create(StrToFloat(StrList[2 * i]),
                           StrToFloat(StrList[2 * i + 1])));
  for i := 0 to StrList.Count div 2 - 2 do
    FEdges.Add(TLineSegment.Create(Vertex[i], Vertex[i + 1]));
  FEdges.Add(TLineSegment.Create(Vertex[VertexCount - 1], Vertex[0]));

  StrList.Free;

  FMin := Vertex[0].Copy;
  FMax := Vertex[0].Copy;

  for i := 1 to VertexCount - 1 do
  begin
    FMin.Fx := Math.Min(FMin.X, Vertex[i].x);
    FMin.Fy := Math.Min(FMin.y, Vertex[i].y);
    FMax.Fx := Math.Max(FMax.x, Vertex[i].x);
    FMax.Fy := Math.Max(FMax.y, Vertex[i].y);
  end;

end;

constructor TPolygon.Create(Points: TPoints);
var
  i: Integer;

begin
  inherited Create;

  FVertices := TPoints.Create;
  FEdges := TEdges.Create;

  for i := 0 to Points.Count - 1 do
    FVertices.Add(TPoint.Create(Points[i].x, Points[i].y));

  for i := 0 to Points.Count - 2 do
    FEdges.Add(TLineSegment.Create(Vertex[i], Vertex[i + 1]));
  FEdges.Add(TLineSegment.Create(Vertex[VertexCount - 1], Vertex[0]));

  FMin := Vertex[0].Copy;
  FMax := Vertex[0].Copy;

  for i := 1 to VertexCount - 1 do
  begin
    FMin.Fx := Math.Min(FMin.X, Vertex[i].x);
    FMin.Fy := Math.Min(FMin.y, Vertex[i].y);
    FMax.Fx := Math.Max(FMax.x, Vertex[i].x);
    FMax.Fy := Math.Max(FMax.y, Vertex[i].y);
  end;

end;

destructor TPolygon.Destroy;
var
  i: Integer;

begin
  for i := 0 to VertexCount - 1 do
  begin
    Vertex[i].Free;
    Edge[i].Free;
  end;
  FVertices.Clear;
  FVertices.Free;
  FEdges.Clear;
  FEdges.Free;

  FMin.Free;
  FMax.Free;

  inherited Destroy;
end;

function TPolygon.IsInside(const P: TPoint): Integer;
var
  Flag: Boolean;
  MaxX, MaxY: Extended;
  T: TPoint;
  Line: TLineSegment;
  i: Integer;
  IntersectionCount: Integer;

begin
  for i := 0 to VertexCount - 1 do
    if P.GetDistance(Vertex[i]) < Epsilon then
      Exit(0);

  MaxX := MaxCoordinate.x;
  MaxY := MaxCoordinate.y;
  Flag := True;

  while Flag do
  begin
    Flag := False;
    MaxX := MaxX + 1;
    T := TPoint.Create(MaxX,
                       MaxY + Random(10000));
    Line := TLineSegment.Create(P, T);
    T.Free;

    for i := 0 to VertexCount - 1 do
      if Abs(Line.Evaluate(Vertex[i])) <= Epsilon then
      begin
        WriteLn(Line.ToString);
        WriteLn(Vertex[i].ToString);

        Flag := True;
        Break;
      end;

    if not Flag then
      Break;
  end;

  IntersectionCount := 0;
  Result := -1;
  for i := 0 to VertexCount - 1 do
  begin
    if Edge[i].Intersects(Line) then
    begin
//      WriteLn(Edge[i].P.ToString,  Edge[i].Q.ToString, ' ',
//        Line.P.ToString, Line.Q.ToString);
      Inc(IntersectionCount);
    end;
  end;

  Line.Free;
  if IntersectionCount mod 2 = 1 then
    Result := +1
  else
    Result := -1;

end;

function TPolygon.GetArea: Extended;
var
  i: Integer;

begin
  Result := 0.0;

  for i := 0 to VertexCount - 2 do
  begin
    Result := Result + Vertex[i].x * Vertex[i + 1].y;
    Result := Result - Vertex[i].y * Vertex[i + 1].x;
  end;

  Result := Result + Vertex[VertexCount - 1].x * Vertex[0].y;
  Result := Result - Vertex[VertexCount - 1].y * Vertex[0].x;

  Result := Result / 2;

end;

function TPolygon.GetPerimeter: Extended;
var
  i: Integer;

begin
  Result := 0;
  for i := 0 to VertexCount - 1 do
    Result += Vertex[i].GetDistance(Vertex[i + 1]);
  Result += Vertex[0].GetDistance(Vertex[VertexCount - 1]);
end;

function TPolygon.ToString: AnsiString;
var
  i: Integer;

begin
  Result := '';

  for i := 0 to VertexCount - 1 do
    Result += Vertex[i].ToString + ':';
  Result := Copy(Result,  1, Length(Result) - 1);
end;

{ TCircle }

constructor TCircle.Create(const c: TPoint; r: Extended);
begin
  inherited Create;

  FCenter := c;
  FRadius := r;
end;

constructor TCircle.CirclePassing(const P1, P2, P3: TPoint);
var
  x, y: array [0..3] of Extended;
  x1, y1, dy1, dx1: Extended;
  x2, y2, dy2, dx2: Extended;
  x_, y_: Extended;

begin
  x [1]:= P1.x; x [2]:= P2.x; x [3]:= P3.x;
  y [1]:= P1.y; y [2]:= P2.y; y [3]:= P3.y;

  x1:= (x [2] + x [1]) / 2;
  y1:= (y [2] + y [1]) / 2;
  dy1:= x [2]- x [1];
  dx1:= -(y [2]- y [1]);

  x2:= (x [3] + x [2]) / 2;
  y2:= (y [3]+ y [2]) / 2;
  dy2:= x [3] - x [2];
  dx2:= -(y [3]- y [2]);

  x_:= (y1 * dx1 * dx2 + x2 * dx1 * dy2 - x1 * dy1 * dx2 - y2 * dx1 * dx2)/ (dx1 * dy2 - dy1 * dx2);
  y_:= (x_ - x1) * dy1 / dx1 + y1;

  FCenter := TPoint.Create(x_, y_);
  FRadius:= Center.GetDistance(P1);

end;

{ TPoint }

constructor TPoint.Create(x_, y_: Extended);
begin
  inherited Create;

  Fx := x_;
  Fy := y_;
end;

function TPoint.GetDistance(const B: TPoint): Extended;
begin
  Result := Sqrt(Sqr(Self.x - B.x) + Sqr(Self.y- B.y));
end;

function TPoint.GetAngle(const A, C: TPoint): Extended;

  function _GetAngle (x, y: Extended): Extended;{C, (0,0), (1, 0)}
  begin
    Result := ArcCos (x / Sqrt (Sqr (x) + Sqr (y)));

    if y < 0 then
      Result := 2 * Pi - Result;

  end;

  var
  aa, bb, cc: Extended;
  d: Extended;

begin
  Result := _GetAngle(A.x - Self.x, A.y - Self.y) -
           _GetAngle (C.x - Self.x, C.y - Self.y);
  if Result < 0 then
    Result += 2 * Pi;

{  cc:= Self.GetDistance (A, B);
  bb:= Self.GetDistance (A, C);
  aa:= Self.GetDistance (B, C);

  d:= arccos ((aa*aa+cc*cc-bb*bb)/(2*aa*cc));
  if (1e-6< Abs (Result- d)) and
     (1e-6< Abs (Result- 2* Pi+ d)) then
       WriteLn ('Error');

//  Result:= d;
}
end;

function TPoint.Copy: TPoint;
begin
  Result := TPoint.Create(x, y);
end;

function TPoint.ToString: AnsiString;
begin
  Result := FloatToStr(x) + ':' + FloatToStr(y);
end;

{ TLineSegment }

constructor TLineSegment.Create(const R, S: TPoint);
begin
  inherited Create;
  FP := R.Copy;
  FQ := S.Copy;
             //ax + by + c = 0
  Fa := P.y - Q.y; // Qx
  Fb := Q.x - P.x; // Qy
  Fc := -(Fa * P.x + Fb * P.y);

  if Fa < 0 then
  begin
    Fa := -Fa;
    Fb := -Fb;
    Fc := -Fc;
  end;

end;

destructor TLineSegment.Destroy;
begin
  P.Free;
  Q.Free;

  inherited Destroy;
end;

function TLineSegment.Evaluate(const R: TPoint): Extended;
begin
  Result := a * R.x + b * R.y + c;
end;

function TLineSegment.Intersects(const L: TLineSegment): Boolean;
  function OnSegment(const p, q, r: TPoint): Boolean;
  begin
    Result := (q.x <= max(p.x, r.x)) and (q.x >= min(p.x, r.x)) and
              (q.y <= max(p.y, r.y)) and (q.y >= min(p.y, r.y));
  end;

// To find orientation of ordered triplet (p, q, r).
// The function returns following values
// 0 --> p, q and r are colinear
// 1 --> Clockwise
// 2 --> Counterclockwise
  function Orientation(const p, q, r: TPoint): Integer;
  var
    val: Extended;
  begin
    val := (q.y - p.y) * (r.x - q.x) -
           (q.x - p.x) * (r.y - q.y);

    if val = 0 then
      Exit(0);  // Colinear

    if 0 < val then
      Result := 1 // Clockwise
    else
      Result := 2; // Counter Clockwise
  end;

var
  o1, o2, o3, o4: Integer;

begin
    o1 := Orientation(L.p, L.Q, Self.p);
    o2 := Orientation(L.p, L.q, Self.q);
    o3 := Orientation(Self.p, Self.q, L.p);
    o4 := Orientation(Self.p, Self.q, L.q);

    // General case
    if (o1 <> o2) and (o3 <> o4) then
        Exit(true);

    // Special Cases
    // p1, q1 and p2 are colinear and p2 lies on segment p1q1
    if (o1 = 0) and OnSegment(L.p, Self.p, L.q) then
      Exit(True);

    // p1, q1 and p2 are colinear and q2 lies on segment p1q1
    if (o2 = 0) and OnSegment(L.p, Self.q, L.q) then
      Exit(True);

    // p2, q2 and p1 are colinear and p1 lies on segment p2q2
    if (o3 = 0) and OnSegment(Self.p, L.p, Self.q) then
      Exit(True);

     // p2, q2 and q1 are colinear and q1 lies on segment p2q2
    if (o4 = 0) and OnSegment(Self.P, L.Q, Self.Q) then
      Exit(True);

    Result := False; // Doesn't fall in any of the above cases
end;

function TLineSegment.ToString: AnsiString;
begin
  Result := FloatToStr(a) + ' * x + ' +
            FloatToStr(b) + ' * y + ' +
            FloatToStr(c) + ' = 0' +
            ' ' + P.ToString + ' ' + Q.ToString;

end;

end.

