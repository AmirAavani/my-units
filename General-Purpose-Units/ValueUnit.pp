unit ValueUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  {$M+}
  { TValue }

  TValue = class(TObject)
  public type
    TInputType = (itAnsiString, itInteger, itUInteger, itBoolean, itExtended);

    { EUninitializedValue }

    EUninitializedValue = class(Exception)
      constructor Create;
    end;
  protected
    ValuePtr: Pointer;
    InputType: TInputType;

    function GetAsAnsiString: AnsiString; virtual;
    function GetAsBoolean: Boolean;  virtual;
    function GetAsExtended: Extended; virtual;
    function GetAsInteger: Int64; virtual;
    function GetAsUInteger: uInt64;  virtual;

  public
    property AsAnsiString: AnsiString read GetAsAnsiString;
    property AsInteger: Int64 read GetAsInteger;
    property AsUInteger: uInt64 read GetAsUInteger;
    property AsBoolean: Boolean read GetAsBoolean;
    property AsExtended: Extended read GetAsExtended;


    constructor CreateAnsiString(Str: AnsiString);
    constructor CreateInteger(IntStr: AnsiString);
    constructor CreateInteger(Int: Int64);
    constructor CreateUInteger(uIntStr: AnsiString);
    constructor CreateUInteger(uInt: UInt64);
    constructor CreateBoolean(bStr: AnsiString);
    constructor CreateBoolean(b: Boolean);
    constructor CreateExtended(eStr: AnsiString);
    constructor CreateExtended(e: Extended);

    destructor Destroy; override;

    function AsAnsiStringOrDefault(Str: AnsiString): AnsiString;
    function AsBooleanOrDefault(b: Boolean): Boolean;
    function AsExtendedOrDefault(e: Extended): Extended;
    function AsIntegerOrDefault(i: Int64): Int64;
    function AsUIntegerOrDefault(ui: UInt64): uInt64;

    procedure UpdateValue(NewValue: AnsiString); virtual;
  end;

implementation
uses
  ALoggerUnit;

{ TValue.EUninitializedValue }

constructor TValue.EUninitializedValue.Create;
begin
  inherited Create('Value object has not been appropriately initialized!');
end;

{ TValue }

function TValue.GetAsAnsiString: AnsiString;
begin
  if Self = nil then
    Exit('');

  Result := PAnsiString(ValuePtr)^;

end;

function TValue.GetAsBoolean: Boolean;
begin
  if Self = nil then
    Exit(False);

  Result := PBoolean(ValuePtr)^;

end;

function TValue.GetAsExtended: Extended;
begin
  if Self = nil then
    Exit(0.0);

  Result := PExtended(ValuePtr)^;

end;

function TValue.GetAsInteger: Int64;
begin
  if Self = nil then
    Exit(0);

  Result := PInt64(ValuePtr)^;

end;

function TValue.GetAsUInteger: uInt64;
begin
  if Self = nil then
    Exit(0);

  Result := PUInt64(ValuePtr)^;

end;

constructor TValue.CreateAnsiString(Str: AnsiString);
begin
  inherited Create;

  InputType := itAnsiString;

  ValuePtr:= new(PAnsiString);
  PAnsiString(ValuePtr)^ := Str;

end;

constructor TValue.CreateInteger(IntStr: AnsiString);
begin
  inherited Create;

  InputType := itInteger;

  ValuePtr:= new(PInt64);
  PInt64(ValuePtr)^ := StrToInt64(IntStr);

end;

constructor TValue.CreateInteger(Int: Int64);
begin
  inherited Create;

  InputType := itInteger;

  ValuePtr:= new(PInt64);
  PInt64(ValuePtr)^ := Int;

end;

constructor TValue.CreateUInteger(uIntStr: AnsiString);
begin
  inherited Create;

  InputType := itUInteger;

  ValuePtr:= new(PUInt64);
  PUInt64(ValuePtr)^ := StrToQWord(uIntStr);

end;

constructor TValue.CreateUInteger(uInt: UInt64);
begin
  inherited Create;

  InputType := itUInteger;

  ValuePtr:= new(PUInt64);
  PUInt64(ValuePtr)^ := uInt;


end;

constructor TValue.CreateBoolean(bStr: AnsiString);
begin
  inherited Create;

  InputType := itBoolean;

  ValuePtr:= new(PBoolean);
  PBoolean(ValuePtr)^ := StrToBool(bStr);

end;

constructor TValue.CreateBoolean(b: Boolean);
begin
  inherited Create;

  InputType := itBoolean;

  ValuePtr:= new(PBoolean);
  PBoolean(ValuePtr)^ := b;


end;

constructor TValue.CreateExtended(eStr: AnsiString);
begin
  inherited Create;

  InputType := itExtended;

  ValuePtr:= new(PExtended);
  PExtended(ValuePtr)^ := StrToFloat(eStr);

end;

constructor TValue.CreateExtended(e: Extended);
begin
  inherited Create;

  InputType := itExtended;

  ValuePtr:= new(PExtended);
  PExtended(ValuePtr)^ := e;

end;

destructor TValue.Destroy;
begin
  case InputType of
    itAnsiString:
      Dispose(PAnsiString(ValuePtr));
    itBoolean:
      Dispose(PBoolean(ValuePtr));
    itExtended:
      Dispose(PExtended(ValuePtr));
    itInteger:
      Dispose(PInt64(ValuePtr));
    itUInteger:
      Dispose(PUInt64(ValuePtr))
    else
      ALoggerUnit.GetLogger.FmtFatalLn(
        'Unknown Type %d',
        [Ord(InputType)]
      );

  end;

  inherited Destroy;
end;

function TValue.AsAnsiStringOrDefault(Str: AnsiString): AnsiString;
begin
  if Self = nil then
    Exit(Str);

  Result := AsAnsiString;

end;

function TValue.AsBooleanOrDefault(b: Boolean): Boolean;
begin
  if Self = nil then
    Exit(b);

  Result := AsBoolean;

end;

function TValue.AsExtendedOrDefault(e: Extended): Extended;
begin
  if Self = nil then
    Exit(e);

  Result := AsExtended;

end;

function TValue.AsIntegerOrDefault(i: Int64): Int64;
begin
  if Self = nil then
    Exit(i);

  Result := AsInteger;

end;

function TValue.AsUIntegerOrDefault(ui: UInt64): uInt64;
begin
  if Self = nil then
    Exit(ui);

  Result := AsUInteger;

end;

procedure TValue.UpdateValue(NewValue: AnsiString);
begin
  if Self = nil then
  begin
    case InputType of
      itAnsiString:
      begin
        Self.ValuePtr := New(PAnsiString);

      end;
      itBoolean:
      begin
        Self.ValuePtr := New(PBoolean);
        PBoolean(ValuePtr)^ := StrToBool(NewValue);
      end;
      itExtended:
      begin
        Self.ValuePtr := New(PExtended);
        PExtended(ValuePtr)^ := StrToFloat(NewValue);
      end;
      itInteger:
      begin
        Self.ValuePtr := New(PInt64);
        PInt64(ValuePtr)^ := StrToInt64(NewValue);
      end;
      itUInteger:
      begin
        Self.ValuePtr := New(PUInt64);
        PUInt64(ValuePtr)^ := StrToQWord(NewValue);
      end;
    end;
  end;

  case InputType of
    itAnsiString:
      PAnsiString(ValuePtr)^ := NewValue;
    itBoolean:
      PBoolean(ValuePtr)^ := StrToBool(NewValue);
    itExtended:
      PExtended(ValuePtr)^ := StrToFloat(NewValue);
    itInteger:
      PInt64(ValuePtr)^ := StrToInt64(NewValue);
    itUInteger:
      PUInt64(ValuePtr)^ := StrToQWord(NewValue);
  end;

end;

end.

