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
  public
    constructor Create(const Pattern: TPattern);
    constructor Create(const Pattern: ansistring);
    constructor Create(const ABasePath: ansistring; ANumShards: integer);
    destructor Destroy; override;

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
  FBasePath := Pattern.BasePath;
  FNumShards := Pattern.NumShards;
  FRemainder := Pattern.FRemainder;
  FModulo := Pattern.FModulo;
end;

destructor TPattern.Destroy;
begin
  inherited Destroy;
end;

function TPattern.GetFilteredNumShards: integer;
begin
  Result := FNumShards;
end;

function TPattern.GetShardPath(ShardIndex: integer): ansistring;
begin
  // Example: data/shard-00000-of-00016
  Result := Format('%s/shard-%.5d-of-%.5d', [FBasePath, ShardIndex, FNumShards]);
end;

function TPattern.WithModulo(Remainder, ModuloValue: integer): TPattern;
begin
  Result := TPattern.Create(Self);
  Result.FRemainder := Remainder;
  Result.FModulo := ModuloValue;
end;


end.
