unit DataUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TData }

  TData = class(TObject)
  type
    TDataType = (dtString = 1, dtInt64, dtUint64, dtExtended, dtPointer);
  var
    DataPtr: Pointer;
    DataType: TDataType;

  private
    function GetDataAsPointer: Pointer;
    function GetDataAsString: AnsiString;
  public
    property DataAsString: AnsiString read GetDataAsString;
    property DataAsPointer: Pointer read GetDataAsPointer;

    constructor CreatePointer(Ptr: Pointer);
    constructor CreateString(Str: AnsiString);
    constructor CreateInt64(i64: Int64);
    constructor CreateUInt64(u64: UInt64);
    constructor CreateExtended(e: Extended);
    constructor Create(D: Pointer; dt: TDataType);

    destructor Destroy; override;
  end;

implementation
uses
  ALoggerUnit;

{ TData }

function TData.GetDataAsPointer: Pointer;
begin
  Result := DataPtr;
end;

function TData.GetDataAsString: AnsiString;
begin
  Result := PAnsiString(DataPtr)^;

end;

constructor TData.CreatePointer(Ptr: Pointer);
begin
  Create(Ptr, dtPointer);

end;

constructor TData.CreateString(Str: AnsiString);
begin
  Create(Pointer(@Str), dtString);

end;

constructor TData.CreateInt64(i64: Int64);
begin
  Create(Pointer(@i64), dtInt64);

end;

constructor TData.CreateUInt64(u64: UInt64);
begin
  Create(Pointer(@u64), dtUint64);

end;

constructor TData.CreateExtended(e: Extended);
begin
  Create(Pointer(@e), dtExtended);

end;

constructor TData.Create(D: Pointer; dt: TDataType);
begin
  inherited Create;

  DataType:= dt;

  case dt of
    dtString:
    begin
      DataPtr := New(PAnsiString);
      PAnsiString(DataPtr)^ := (PAnsiString(D))^;

    end;
    dtPointer:
    begin
      DataPtr := d;

    end
    else
    begin
      FmtFatalLn('Invalid dt: %d', [Ord(dt)]);

    end;
  end;
end;

destructor TData.Destroy;
begin
  if DataType = dtString then
    Dispose(PAnsiString(DataPtr));

  inherited Destroy;
end;

end.

