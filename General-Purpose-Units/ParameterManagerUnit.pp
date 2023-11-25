unit ParameterManagerUnit;

{$mode objfpc}{$H+}

interface

uses
  ValueUnit, Classes, SysUtils, GenericCollectionUnit;

type
  { TRunTimeParameterManager }

  TRunTimeParameterManager = class(TObject)
  public type

    { EUndefinedArgument }

    EUndefinedArgument = class(Exception)
      constructor Create(ArgName: AnsiString);

    end;
  public type
    TNameValueMap = specialize TMapSimpleKeyObjectValue<AnsiString, TValue>;

  protected
    Values: TNameValueMap;

    function GetVerbosity: Integer;
    function GetValueByName(Name: AnsiString): TValue;

  public
    property Verbosity: Integer read GetVerbosity;
    property ValueByName[Name: AnsiString]: TValue read GetValueByName;

    constructor Create;
    destructor Destroy; override;

    class function GetInstance: TRunTimeParameterManager;
  end;

procedure Initialize;
procedure Finalize;
function GetRunTimeParameterManager: TRunTimeParameterManager;
function RunTimeParameterManager: TRunTimeParameterManager;

implementation
uses
  StringUnit;

var
  _RunTimeParameterManager: TRunTimeParameterManager;

procedure Initialize;
begin
  if _RunTimeParameterManager = nil then
    _RunTimeParameterManager := TRunTimeParameterManager.Create;

end;

procedure Finalize;
begin
  _RunTimeParameterManager.Free;

end;

function GetRunTimeParameterManager: TRunTimeParameterManager;
begin
  Result := _RunTimeParameterManager;

end;

function RunTimeParameterManager: TRunTimeParameterManager;
begin
  Result := GetRunTimeParameterManager;
end;

{ TRunTimeParameterManager.EUndefinedArgument }

constructor TRunTimeParameterManager.EUndefinedArgument.Create(
  ArgName: AnsiString);
begin
  inherited Create(Format('Undefined argument: no entry with name %s exists in ValidArguments.inc', [ArgName]));
end;

{ TRunTimeParameterManager }

function TRunTimeParameterManager.GetVerbosity: Integer;
begin
  Result := GetValueByName('--Verbosity').AsInteger

end;

constructor TRunTimeParameterManager.Create;
const
{$i ValidArguments.inc }

  function GetNameFromArgInfo(ArgInfo: AnsiString): AnsiString;
  begin
    Result :=  Copy(ArgInfo, 1, Pos(':', ArgInfo + ':') - 1);

  end;

  function GetArgumentType(const TypeString: AnsiString): TValue.TInputType;
  begin
    case UpperCase(TypeString) of
    'ANSISTRING':
      Exit(itAnsiString);
    'BOOLEAN':
      Exit(itBoolean);
    'EXTENDED':
      Exit(itExtended);
    'INTEGER':
      Exit(itInteger);
    'UINTEGER':
      Exit(itUInteger);
    end;

    Result := itAnsiString;
  end;

  function GetArgumentTypeByName(ArgName: AnsiString): TValue.TInputType;
  var
    ArgInfo: AnsiString;
    ArgType: AnsiString;

  begin
    ArgType := '';
    for ArgInfo in ValidArgumentsInfo do
      if UpperCase(GetNameFromArgInfo(ArgInfo)) = UpperCase(ArgName) then
      begin
        ArgType := Copy(ArgInfo, Pos(':', ArgInfo) + 1, Length(ArgInfo));
        Break;
      end;

    if ArgType = '' then
      raise EUndefinedArgument.Create(ArgName);

    Result := GetArgumentType(ArgType);
  end;

  procedure CheckParameter(Name, Value: AnsiString);
  var
    i: Integer;

  begin
    for i := Low(ValidArgumentsValues) to High(ValidArgumentsValues) do
    begin

      if UpperCase(Name) = UpperCase(GetNameFromArgInfo(ValidArgumentsInfo[i])) then
      begin
        if ValidArgumentsValues[i] = '' then
          Exit;
        if Pos(':' + UpperCase(Value) + ':',
          ':' + UpperCase(ValidArgumentsValues[i]) + ':') <> 0 then
           Exit;

        WriteLn(Format('Value "%s" is not a valid value for %s.', [Name, Value]));
        WriteLn(Format('Valid Arguments for %s are (%s)', [Name, ValidArgumentsValues[i]]));
        Halt(1);
      end;

    end;

    for i := Low(ValidArgumentsValues) to High(ValidArgumentsValues) do
      if UpperCase(Name) = UpperCase(ValidArgumentsInfo[i]) then
        Exit;

    WriteLn('Invalid Name :', Name, '.');
    WriteLn('Valid Parameters are: ');
    for i := Low(ValidArgumentsInfo) to High(ValidArgumentsInfo)  do
      Write(GetNameFromArgInfo(ValidArgumentsInfo[i]), ' , ');
    Halt(1);

  end;

  function GetValueObject(aType: TValue.TInputType; aValue: AnsiString): TValue;
  begin
    case aType of
      itAnsiString: Exit(TValue.CreateAnsiString(aValue));
      itBoolean: Exit(TValue.CreateBoolean(aValue));
      itExtended: Exit(TValue.CreateExtended(aValue));
      itInteger: Exit(TValue.CreateInteger(aValue));
      itUInteger: Exit(TValue.CreateUInteger(aValue));
    end;
    Result := nil;
  end;

var
  i: Integer;
  ArgInfo: AnsiString;
  Name, V: AnsiString;
  aVal: TValue;

begin
  inherited;

  Values := TNameValueMap.Create;

  i := 1;
  while i <= Paramcount do
  begin
    Name := ParamStr(i);
    Inc(i);

    if not IsPrefix('--', Name) then
      Continue;
    if ParamCount < i then
      Continue;

    V := ParamStr(i);
    Values.Add(UpperCase(Name), GetValueObject(GetArgumentTypeByName(Name), V));

    Inc(i);

  end;

  for i := Low(ValidArgumentsInfo) to High(ValidArgumentsInfo) do
  begin
    ArgInfo := ValidArgumentsInfo[i];
    aVal := Values.Find(UpperCase(GetNameFromArgInfo(ArgInfo)));
    if aVal = nil then
    begin
      Values.Add(UpperCase(GetNameFromArgInfo(ArgInfo)),
         GetValueObject(GetArgumentTypeByName(GetNameFromArgInfo(ArgInfo)),
           ValidArgumentsValues[i]));
    end;
  end;

end;

destructor TRunTimeParameterManager.Destroy;
begin
  Values.Free;

  inherited Destroy;

end;

class function TRunTimeParameterManager.GetInstance: TRunTimeParameterManager;
begin
  Result := GetRunTimeParameterManager;
end;

function TRunTimeParameterManager.GetValueByName(Name: AnsiString): TValue;
begin
  Result := nil;
  Values.TryGetData(UpperCase(Name), Result);


end;

initialization
  _RunTimeParameterManager := nil;
  Initialize;

finalization
  Finalize;

end.

