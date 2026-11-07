unit PatternUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TPattern - Encapsulates shard path pattern }
  TPattern = class(TObject)
  private
    FBasePath: ansistring;
    FNumShards: integer;
    FRemainder: integer;
    FModulo: integer;

    function GetFilteredNumShards: integer;
    function GetPhysicalShardIndex(LogicalIndex: integer): integer;
  public
    constructor Create(const Pattern: TPattern);
    constructor Create(const Pattern: ansistring);
    constructor Create(const ABasePath: ansistring; ANumShards: integer);

    function GetShardPath(ShardIndex: integer): ansistring;
    function WithModulo(Remainder, ModuloValue: integer): TPattern;

    property BasePath: ansistring read FBasePath;
    property NumShards: integer read GetFilteredNumShards;
  end;

  { Exception Classes }
  EZIOStreamException = class(Exception);
  EPatternException = class(EZIOStreamException);
  EShardException = class(EZIOStreamException);
  EStreamException = class(EZIOStreamException);

implementation

{ TPattern }

constructor TPattern.Create(const ABasePath: ansistring; ANumShards: integer);
begin
  inherited Create;
  FBasePath := ABasePath;
  FNumShards := ANumShards;
  FRemainder := 0;
  FModulo := 1;
end;

constructor TPattern.Create(const Pattern: ansistring);
var
  P: integer;
begin
  inherited Create;
  P := Pos('@', Pattern);
  if P > 0 then
  begin
    FBasePath := Copy(Pattern, 1, P - 1);
    FNumShards := StrToIntDef(Copy(Pattern, P + 1, Length(Pattern)), 1);
  end
  else
  begin
    FBasePath := Pattern;
    FNumShards := 1;
  end;
  FRemainder := 0;
  FModulo := 1;
end;

constructor TPattern.Create(const Pattern: TPattern);
begin
  inherited Create;
  FBasePath := Pattern.FBasePath;
  FNumShards := Pattern.FNumShards;
  FRemainder := Pattern.FRemainder;
  FModulo := Pattern.FModulo;
end;

function TPattern.GetFilteredNumShards: integer;
var
  i, Count: integer;
begin
  if FModulo <= 1 then
    Exit(FNumShards);

  {TODO: This is a naive implementation for this function. This could be improved. }

  Count := 0;
  for i := 0 to FNumShards - 1 do
  begin
    if (i mod FModulo) = FRemainder then
      Inc(Count);
  end;
  Result := Count;
end;



function TPattern.GetPhysicalShardIndex(LogicalIndex: integer): integer;
var
  i, Count: integer;
begin
  if FModulo <= 1 then
    Exit(LogicalIndex);

  {TODO: This is a naive implementation for this function. This could be improved. }
  Count := 0;
  for i := 0 to FNumShards - 1 do
  begin
    if (i mod FModulo) = FRemainder then
    begin
      if Count = LogicalIndex then
        Exit(i);
      Inc(Count);
    end;
  end;
  Result := LogicalIndex; // Fallback
end;

function TPattern.GetShardPath(ShardIndex: integer): ansistring;
var
  PhysicalIndex: integer;
begin
  // Map the local loop index (0 to FilteredNumShards-1) to the actual physical shard index
  PhysicalIndex := GetPhysicalShardIndex(ShardIndex);

  // Format uses the physical index for the directory name, while keeping total shards for naming consistency
  Result := Format('%s/shard-%.5d-of-%.5d', [FBasePath, PhysicalIndex, FNumShards]);
end;

function TPattern.WithModulo(Remainder, ModuloValue: integer): TPattern;
begin
  Result := TPattern.Create(Self);
  Result.FRemainder := Remainder;
  Result.FModulo := ModuloValue;
end;


end.
