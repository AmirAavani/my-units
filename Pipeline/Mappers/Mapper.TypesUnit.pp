unit Mapper.TypesUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PairUnit;

type
  TKeyValue = specialize TPair<AnsiString, TObject>;

  { TMapperBasicData }

  TMapperBasicData = class(TObject)
  private type
    TDataType = (dtAnsiString, dtInt64, dtUInt64, dtExtended, dtBoolean);

  private
    DataType: TDataType;
    Data: Pointer;

    function GetDataBoolean: Boolean;
    function GetDataExtended: Extended;
    function GetDataInt64: Int64;
    function GetDataString: AnsiString;
    function GetDataUInt64: UInt64;

  public
    property DataString: AnsiString read GetDataString;
    property DataInt64: Int64 read GetDataInt64;
    property DataUInt64: UInt64 read GetDataUInt64;
    property DataExtended: Extended read GetDataExtended;
    property DataBoolean: Boolean read GetDataBoolean;

    constructor Create(Str: AnsiString);
    constructor Create(n: Int64);
    constructor Create(u: UInt64);
    constructor Create(e: Extended);
    constructor Create(b: Boolean);

    destructor Destroy; override;
  end;

implementation

uses
  ALoggerUnit;

{ TMapperBasicData }

function TMapperBasicData.GetDataBoolean: Boolean;
begin
  Result := PBoolean(Data)^;

end;

function TMapperBasicData.GetDataExtended: Extended;
begin
  Result := PExtended(Data)^;

end;

function TMapperBasicData.GetDataInt64: Int64;
begin
  Result := PInt64(Data)^;

end;

function TMapperBasicData.GetDataString: AnsiString;
begin
  Result := PAnsiString(Data)^;

end;

function TMapperBasicData.GetDataUInt64: UInt64;
begin
  Result := PUInt64(Data)^;

end;

constructor TMapperBasicData.Create(Str: AnsiString);
begin
  inherited Create;

  Data := new(PAnsiString);
  DataType := dtAnsiString;
  PAnsiString(Data)^ := Str;

end;

constructor TMapperBasicData.Create(n: Int64);
begin
  inherited Create;

  Data := new(PInt64);
  DataType := dtInt64;
  PInt64(Data)^ := n;

end;

constructor TMapperBasicData.Create(u: UInt64);
begin
  inherited Create;

  Data := new(PUInt64);
  DataType := dtUInt64;
  PUInt64(Data)^ := u;

end;

constructor TMapperBasicData.Create(e: Extended);
begin
  inherited Create;

  Data := new(PExtended);
  DataType := dtExtended;
  PExtended(Data)^ := e;

end;

constructor TMapperBasicData.Create(b: Boolean);
begin
  inherited Create;

  Data := new(PBoolean);
  DataType := dtBoolean;
  PBoolean(Data)^ := b;

end;

destructor TMapperBasicData.Destroy;
begin
  case DataType of
    dtAnsiString:
      Dispose(PAnsiString(Data));
    dtInt64:
      Dispose(PInt64(Data));
    dtUInt64:
      Dispose(PUInt64(Data));
    dtExtended:
      Dispose(PExtended(Data));
    dtBoolean:
      Dispose(PBoolean(Data))
  end;

  inherited Destroy;
end;

end.

