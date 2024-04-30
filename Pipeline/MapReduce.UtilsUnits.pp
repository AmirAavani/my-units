unit MapReduce.UtilsUnits;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;
type

    { TPattern }

    TPattern = class(TStringList)
    private
      function GetFilename(Index: integer): AnsiString;
    public
      property Filename[Index: integer]: AnsiString read GetFilename;

      constructor Create(constref StringPattern: AnsiString);
      destructor Destroy; override;

    end;

implementation

uses
    PathHelperUnit;


{ TPattern }

function TPattern.GetFilename(Index: integer): AnsiString;
begin
  Result := Self.Strings[Index];

end;

constructor TPattern.Create(constref StringPattern: AnsiString);
var
  PatternName: AnsiString;
  Shards: Integer;
  i: Integer;

begin
  inherited Create;

  i := Length(StringPattern);
  while i <> 0 do
  begin
    if StringPattern[i] = '@' then
      Break;
    Dec(i);

  end;
  if i = 0 then
  begin
    Self.Add(StringPattern);
    Exit;

  end;

  Shards := StrToInt(Copy(StringPattern, i + 1, Length(StringPattern)));
  PatternName := Copy(StringPattern, 1, i - 1);

  for i := 0 to Shards - 1 do
  begin
    Self.Add(
      PathHelperUnit.JoinPath(PatternName, Format('%05d-of-%05d', [i, Shards]))
    );

  end;
end;

destructor TPattern.Destroy;
begin
  inherited Destroy;
end;

end.

