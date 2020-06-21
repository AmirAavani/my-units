unit ParameterManagerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  { TRunTimeParameterManager }

  TRunTimeParameterManager = class(TObject)
  private type
    TNameValueMap = specialize TFPGMap<AnsiString, AnsiString>;

  private
    Values: TNameValueMap;

    function GetBoolValue(Name: AnsiString): Boolean;
    function GetBoolValueOrDefault(Name: AnsiString; DefaultValue: Boolean
      ): Boolean;
    function GetFloatValue(Name: AnsiString): Extended;
    function GetIntegerValue(Name: AnsiString): Int64;
    function GetStringValue(Name: AnsiString): AnsiString;
    function GetUIntegerValue(Name: AnsiString): UInt64;
    function GetVerbosity: Integer;
    function GetValueByName(Name: AnsiString): AnsiString;
    function GetValueByNameOrDefault(Name: AnsiString; DefaultValue: AnsiString): AnsiString;

  public
    property Verbosity: Integer read GetVerbosity;
    property StringValue[Name: AnsiString]: AnsiString read GetStringValue;
    property IntegerValue[Name: AnsiString]: Int64 read GetIntegerValue;
    property UIntegerValue[Name: AnsiString]: UInt64 read GetUIntegerValue;
    property BoolValue[Name: AnsiString]: Boolean read GetBoolValue;
    property BoolValueOrDefault[Name: AnsiString; DefaultValue: Boolean]: Boolean read GetBoolValueOrDefault;
    property FloatValue[Name: AnsiString]: Extended read GetFloatValue;
    property ValueByName[Name: AnsiString]: AnsiString read GetValueByName;
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

{ TRunTimeParameterManager }

function TRunTimeParameterManager.GetBoolValue(Name: AnsiString): Boolean;
begin
  Result := StrToBoolDef(GetValueByName(Name), False);
end;

function TRunTimeParameterManager.GetBoolValueOrDefault(Name: AnsiString;
  DefaultValue: Boolean): Boolean;
begin
  Result := StrToBoolDef(GetValueByName(Name), DefaultValue);
end;

function TRunTimeParameterManager.GetFloatValue(Name: AnsiString): Extended;
begin
  Result := StrToFloatDef(GetValueByName(Name), 0.0);
end;

function TRunTimeParameterManager.GetIntegerValue(Name: AnsiString): Int64;
begin
  Result := StrToInt64Def(GetValueByName(Name), 0);
end;

function TRunTimeParameterManager.GetStringValue(Name: AnsiString): AnsiString;
begin
  Result := GetValueByNameOrDefault(Name, '');
end;

function TRunTimeParameterManager.GetUIntegerValue(Name: AnsiString): UInt64;
begin
  Result := StrToDWordDef(GetValueByName(Name), 0);
end;

function TRunTimeParameterManager.GetVerbosity: Integer;
begin
  if GetValueByName('--Verbosity')<> '' then
    Exit(StrToInt(GetValueByName('--Verbosity')))
  else
    Exit(0);

end;

constructor TRunTimeParameterManager.Create;
const
{$i ValidArguments.inc }

  procedure CheckParameter(Name, Value: AnsiString);
  var
    i: Integer;
  begin
    for i := Low(ValidArguments) to High(ValidArguments) do
    begin

      if UpperCase(Name) = UpperCase(ValidArguments[i]) then
      begin
        if ValidArgumentsValues[i] = '' then
          Exit;
        // For backward compatibility.
        if Pos('NONE', UpperCase(ValidArgumentsValues[i])) <> 0 then
          Exit;

        if Pos(':' + UpperCase(Value) + ':',
          ':' + UpperCase(ValidArgumentsValues[i]) + ':') <> 0 then
           Exit;

        WriteLn(Format('Value "%s" is not a valid value for %s.', [Name, Value]));
        WriteLn(Format('Valid Arguments for %s are (%s)', [Name, ValidArgumentsValues[i]]));
        Halt(1);

      end;

    end;

    for i := Low(ValidArguments) to High(ValidArguments) do
      if UpperCase(Name) = UpperCase(ValidArguments [i]) then
        Exit;

    WriteLn('Invalid Name :', Name, '.');
    WriteLn('Valid Parameters are: ');
    for i := Low(ValidArguments) to High(ValidArguments)  do
      Write(ValidArguments [i], ' , ');
    Halt(1);
  end;


var
  i: Integer;
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
    Values[UpperCase(Name)] := V;

    Inc(i);

  end;

  for i := Low(ValidArguments) to High(ValidArguments) do
    if Values.IndexOf(UpperCase(ValidArguments[i])) < 0 then
    begin
      Values[UpperCase(ValidArguments[i])] :=
        Copy(ValidArgumentsValues[i],
             1,
             Pos(':', ValidArgumentsValues[i] + ':') - 1
            );
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

function TRunTimeParameterManager.GetValueByName(Name: AnsiString): AnsiString;
begin
  Result := Values[UpperCase(Name)]

end;

function TRunTimeParameterManager.GetValueByNameOrDefault(Name: AnsiString;
  DefaultValue: AnsiString): AnsiString;
begin
  if Values.IndexOf(UpperCase(Name)) < 0 then
    Exit(DefaultValue);
  Result := Values[UpperCase(Name)];
end;

initialization
  _RunTimeParameterManager := nil;
  Initialize;

finalization
  Finalize;

end.

