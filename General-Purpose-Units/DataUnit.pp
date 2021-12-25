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
    function GetDataAsExtended: Extended;
    function GetDataAsInt64: Int64;
    function GetDataAsPointer: Pointer;
    function GetDataAsString: AnsiString;
    function GetDataAsUInt64: UInt64;
  public
    property DataAsString: AnsiString read GetDataAsString;
    property DataAsPointer: Pointer read GetDataAsPointer;
    property DataAsInt64: Int64 read GetDataAsInt64;
    property DataAsUInt64: UInt64 read GetDataAsUInt64;
    property DataAsExtended: Extended read GetDataAsExtended;

    constructor CreatePointer(Ptr: Pointer);
    constructor CreateString(constref Str: AnsiString);
    constructor CreateInt64(i64: Int64);
    constructor CreateUInt64(u64: UInt64);
    constructor CreateExtended(e: Extended);
    constructor Create(p: Pointer; dt: TDataType);

    destructor Destroy; override;
  end;

implementation
uses
  ALoggerUnit;

{ TData }

function TData.GetDataAsExtended: Extended;
begin
  Result := PExtended(DataPtr)^;
end;

function TData.GetDataAsInt64: Int64;
begin

end;

function TData.GetDataAsPointer: Pointer;
begin
  Result := DataPtr;
end;

function TData.GetDataAsString: AnsiString;
begin
  Result := PAnsiString(DataPtr)^;

end;

function TData.GetDataAsUInt64: UInt64;
begin
  Result := PUInt64(DataPtr)^;

end;

constructor TData.CreatePointer(Ptr: Pointer);
begin
  Create(Ptr, dtPointer);

end;

constructor TData.CreateString(constref Str: AnsiString);
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

constructor TData.Create(p: Pointer; dt: TDataType);
begin
  inherited Create;

  DataType:= dt;

  case dt of
    dtString:
    begin
      DataPtr := New(PAnsiString);
      PAnsiString(DataPtr)^ := (PAnsiString(p))^;

    end;
    dtExtended:
    begin
      DataPtr := New(PExtended);
      PExtended(DataPtr)^ := (PExtended(p))^;

    end;
    dtInt64:
    begin
      DataPtr := New(PInt64);
      PInt64(DataPtr)^ := (PInt64(p))^;

    end;
    dtUint64:
    begin
      DataPtr := New(PUInt64);
      PUInt64(DataPtr)^ := (PUInt64(p))^;

    end;
    dtPointer:
    begin
      DataPtr := p;

    end
    else
    begin
      FmtFatalLn('Invalid dt: %d', [Ord(dt)]);

    end;
  end;
end;

destructor TData.Destroy;
begin
  case DataType of
    dtString:
    begin
      Dispose(PAnsiString(DataPtr));

    end;
    dtExtended:
    begin
      Dispose(PExtended(DataPtr));

    end;
    dtInt64:
    begin
      Dispose(PInt64(DataPtr));

    end;
    dtUint64:
    begin
      Dispose(PUInt64(DataPtr));

    end;
    else
    begin
      FmtFatalLn('Invalid dt: %d', [Ord(DataType)]);

    end;

  end;


  inherited Destroy;
end;

end.

