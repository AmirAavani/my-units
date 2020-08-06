unit ParameterManagerUnit;

{$mode objfpc}{$H+}

interface

uses
  ValueUnit, Classes, SysUtils, fgl;

type

  { TRunTimeParameterManager }

  TRunTimeParameterManager = class(TObject)
  public type

    { EUndefinedArgument }

    EUndefinedArgument = class(Exception)
      constructor Create(ArgName: AnsiString);

    end;
  private type
    TNameValueMap = specialize TFPGMap<AnsiString, TValue>;

  private
    Values: TNameValueMap;

    function GetValueByNameOrDefault(Name, DefaultValue: AnsiString
      ): AnsiString;
    function GetVerbosity: Integer;
    function GetValueByName(Name: AnsiString): TValue;

  public
    property Verbosity: Integer read GetVerbosity;
    property ValueByName[Name: AnsiString]: TValue read GetValueByName;
    property ValueByNameOrDefault[Name, DefaultValue: AnsiString]: AnsiString read GetValueByNameOrDefault;

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

function TRunTimeParameterManager.GetValueByNameOrDefault(Name,
  DefaultValue: AnsiString): AnsiString;
begin

end;

function TRunTimeParameterManager.GetVerbosity: Integer;
begin
  Result := GetValueByName('--Verbosity').AsInteger

end;

constructor TRunTimeParameterManager.Create;
const
{$i ValidArguments.inc }

  function GetNameFromArgInfo(ArgInfo: AnsiString): AnsiString;
  begin
    Result :=  Copy( ArgInfo, 1, Pos(':', ArgInfo) - 1);

  end;

  function GetArgumentTypeByName(ArgName: AnsiString): TValue.TInputType;
  var
    ArgInfo: AnsiString;
    ArgType: AnsiString;

  begin
    ArgType := '';
    for ArgInfo in ValidArgumentsInfo do
      if Copy(ArgInfo, 1, Pos(':', ArgInfo) - 1) = ArgName then
      begin
        ArgType := Copy(ArgInfo, Pos(':', ArgInfo) + 1, Length(ArgInfo));
        Break;
      end;

    if ArgType = '' then
      raise EUndefinedArgument.Create(ArgName);

    case ArgType of
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

  function GetValueObjectForName(aName: AnsiString; aValue: AnsiString): TValue;
  begin
    case GetArgumentTypeByName(aName) of
      itAnsiString: Exit(TValue.CreateAnsiString(aValue));
      itBoolean: Exit(TValue.CreateBoolean(aValue));
      itExtended: Exit(TValue.CreateExtended(aValue));
      itInteger: Exit(TValue.CreateExtended(aValue));
      itUInteger: Exit(TValue.CreateExtended(aValue));
    end;
    Result := nil;
  end;

var
  i: Integer;
  ArgInfo: AnsiString;
  Name, V: AnsiString;

begin
  inherited;

  Values := TNameValueMap.Create;
  Values.Sorted := True;

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
    Values[UpperCase(Name)] := GetValueObjectForName(Name, V);

    Inc(i);

  end;

  for i := Low(ValidArgumentsInfo) to High(ValidArgumentsInfo) do
  begin
    ArgInfo := ValidArgumentsInfo[i];
    if Values.IndexOf(UpperCase(GetNameFromArgInfo(ArgInfo))) < 0 then
    begin
      Values[UpperCase(GetNameFromArgInfo(ArgInfo))] :=
         GetValueObjectForName(GetNameFromArgInfo(ArgInfo),
           Copy(ValidArgumentsValues[i],
             1,
             Pos(':', ValidArgumentsValues[i] + ':') - 1
            ));
    end;
  end;

end;

destructor TRunTimeParameterManager.Destroy;
begin
  inherited Destroy;

end;

class function TRunTimeParameterManager.GetInstance: TRunTimeParameterManager;
begin
  Result := GetRunTimeParameterManager;
end;

function TRunTimeParameterManager.GetValueByName(Name: AnsiString): TValue;
begin
  Values.TryGetData(UpperCase(Name), Result);


end;

initialization
  _RunTimeParameterManager := nil;
  Initialize;

finalization
  Finalize;

end.

