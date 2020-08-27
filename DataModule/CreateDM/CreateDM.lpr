program CreateDM;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  sysutils, Classes, ParameterManagerUnit, MySQLDBConnectorUnit, StringUnit,
  QueryResponeUnit, UtilsUnit, BaseDataModuleUnit, StreamUnit,
  TemplateEngineUnit, ALoggerUnit;

const
  SingleQuote : AnsiString = Chr(39);
  UnitHeader: AnsiString =
    'unit {{@UnitName}};' + sLineBreak
    + sLineBreak
    + '{$mode objfpc}{$H+}' + sLineBreak
    + sLineBreak
    + 'interface' + sLineBreak
    + sLineBreak
    + 'uses' + sLineBreak
    + '  BaseDataModuleUnit, fgl;' + sLineBreak
    + sLineBreak
    + 'type' + sLineBreak
    + sLineBreak;

  DataCollectionDefinition: AnsiString =
  '  {{@ClassName}} = class;'+ sLineBreak
  + sLineBreak
  + '{ {{@ClassName}}List }'
  + sLineBreak
  + sLineBreak
  + '  {{@ClassName}}List = specialize TFPGList<{{@ClassName}}>;'
  + sLineBreak
  + sLineBreak;

  DataClassDefinition: AnsiString =
  '{ {{@ClassName}} }'
  + sLineBreak
  + sLineBreak
  + '  {{@ClassName}} = class(TBaseDataModule)' + sLineBreak
  + '  public' + sLineBreak
  + '    class function TableName: AnsiString; override;' + sLineBreak
  + '    class function NumFields: Integer; override;' + sLineBreak
  + '    class function ColumnNameByIndex(Index: Integer): AnsiString; override;' + sLineBreak
  + '    class function MySQLColumnTypeByIndex(Index: Integer): AnsiString; override;' + sLineBreak
  + '    class function FPCColumnTypeByIndex(Index: Integer): AnsiString; override;' + sLineBreak
  + '    class function ColumnIndexByName(const aName: AnsiString): Integer; override;' + sLineBreak
  + sLineBreak + sLineBreak;

  procedure ImplementStaticFuctions(ClassName: AnsiString; TheTableName: AnsiString;
    ColumnNames, ColumnTypes: TStringList; OutputStream: TMyTextStream);
  begin
    OutputStream.WriteLine(Format('class function %s.TableName: AnsiString;', [ClassName]));
    OutputStream.WriteLine('begin');
    OutputStream.WriteLine(Format('  Exit(' + SingleQuote + '%s' + SingleQuote + ')', [TheTableName]));
    OutputStream.WriteLine('');
    OutputStream.WriteLine('end;');
    OutputStream.WriteLine('');

    OutputStream.WriteLine(Format('class function %s.NumFields: Integer;', [ClassName]));
    OutputStream.WriteLine('begin');
    OutputStream.WriteLine(Format('  Result := %d;', [ColumnNames.Count]));
    OutputStream.WriteLine('end;');
    OutputStream.WriteLine('');

  end;

procedure ImplementCreate(ClassName, TheTableName: AnsiString;
    ColumnNames, ColumnTypes: TStringList; OutputStream: TMyTextStream);
var
  i: Integer;
  ColName, ColType, FieldName, FieldType: AnsiString;

begin
  OutputStream.WriteLine(Format('constructor %s.Create;', [ClassName]));
  OutputStream.WriteLine('begin');
  OutputStream.WriteLine(Format('  inherited Create(%d);', [ColumnNames.Count]));
  OutputStream.WriteLine;
  for i := 0 to ColumnNames.Count - 1 do
  begin
    ColName := ColumnNames[i];
    ColType := ColumnTypes[i];

    FieldName := TransformName(ColName);
    FieldType := TranslateType(ColType);

    case FieldType of
    'AnsiString':
      OutputStream.WriteLine(Format('  FValues[%d] := TDMValue.CreateAnsiString(' +
        SingleQuote + SingleQuote + ');', [i]));
    'Integer':
      OutputStream.WriteLine(Format('  FValues[%d] := TDMValue.CreateInteger(0);', [i]));
    'Int64':
      OutputStream.WriteLine(Format('  FValues[%d] := TDMValue.CreateInteger(0);', [i]));
    'Extended':
      OutputStream.WriteLine(Format('  FValues[%d] := TDMValue.CreateExtended(0.0);', [i]));
    'TDate':
      OutputStream.WriteLine(Format('  FValues[%d] := TDMValue.CreateTDate(Now);', [i]));
    'TTime':
      OutputStream.WriteLine(Format('  FValues[%d] := TDMValue.CreateTTime(Now);', [i]));
    else
      raise Exception.Create(Format('Invalid Type %s', [FieldType]));
    end;

  end;

  OutputStream.WriteLine;
  OutputStream.WriteLine(sLineBreak + 'end;');

  OutputStream.WriteStr(Format('constructor %s.Create(', [ClassName]));
  for i := 0 to ColumnNames.Count - 1 do
  begin
    ColName := ColumnNames[i];
    ColType := ColumnTypes[i];

    FieldName := TransformName(ColName);
    FieldType := TranslateType(ColType);
    if i <> 0 then
      OutputStream.WriteStr('; ');
    OutputStream.WriteStr(Format('_%s: %s', [FieldName, FieldType]));
  end;
  OutputStream.WriteLine(');');

  OutputStream.WriteLine('begin');
  OutputStream.WriteLine(Format('  inherited Create(%d);', [ColumnNames.Count]));
  OutputStream.WriteLine;
  for i := 0 to ColumnNames.Count - 1 do
  begin
    ColName := ColumnNames[i];
    ColType := ColumnTypes[i];

    FieldName := TransformName(ColName);
    FieldType := TranslateType(ColType);

    case FieldType of
    'AnsiString':
      OutputStream.WriteLine(Format('  FValues[%d] := TDMValue.CreateAnsiString(_%s);', [i, FieldName]));
    'Integer':
      OutputStream.WriteLine(Format('  FValues[%d] := TDMValue.CreateInteger(_%s);', [i, FieldName]));
    'Int64':
      OutputStream.WriteLine(Format('  FValues[%d] := TDMValue.CreateInteger(_%s);', [i, FieldName]));
    'Extended':
      OutputStream.WriteLine(Format('  FValues[%d] := TDMValue.CreateExtended(_%s);', [i, FieldName]));
    'TDate':
      OutputStream.WriteLine(Format('  FValues[%d] := TDMValue.CreateTDate(_%s);', [i, FieldName]));
    'TTime':
      OutputStream.WriteLine(Format('  FValues[%d] := TDMValue.CreateTTime(_%s);', [i, FieldName]));
    else
      raise Exception.Create(Format('Invalid Type %s', [FieldType]));
    end;

  end;

  OutputStream.WriteLine;
  OutputStream.WriteLine(sLineBreak + 'end;');

end;

procedure ImplementColumnNameByIndex(aClassName: String; aTheTableName: String;
  aColumnNames: TStringList; aColumnTypes: TStringList;
  OutputStream: TMyTextStream);
var
  i: Integer;

begin
  OutputStream.WriteLine(Format('class function %s.ColumnNameByIndex(Index: Integer): AnsiString;',
    [aClassName]));

  OutputStream.WriteLine('begin');
  OutputStream.WriteLine(Format('  Result := ' + Chr(39) + Chr(39) + ';', []));
  OutputStream.WriteLine(Format('  case Index of', []));

  for i := 0 to aColumnNames.Count - 1 do
    OutputStream.WriteLine(Format('    %d: Exit(' + Chr(39) + '%s' + Chr(39) + ');', [i, aColumnNames[i]]));
  OutputStream.WriteLine('  end;');
  OutputStream.WriteLine;
  OutputStream.WriteLine(Format('  raise EInvalidColumnIndex.Create(Index, %d);', [aColumnNames.Count]));

  OutputStream.WriteLine;
  OutputStream.WriteLine('end;');

end;

procedure ImplementMySQLColumnTypeByIndex(aClassName: String; aTheTableName: String;
  aColumnNames: TStringList; aColumnTypes: TStringList;
  OutputStream: TMyTextStream);
var
  i: Integer;

begin
  OutputStream.WriteLine(Format('class function %s.MySQLColumnTypeByIndex(Index: Integer): AnsiString;',
    [aClassName]));

  OutputStream.WriteLine('begin');
  OutputStream.WriteLine(Format('  Result := ' + Chr(39) + Chr(39) + ';', []));
  OutputStream.WriteLine(Format('  case Index of', []));

  for i := 0 to aColumnNames.Count - 1 do
    OutputStream.WriteLine(Format('    %d: Exit(' + Chr(39) + '%s' + Chr(39) + ');', [i, aColumnTypes[i]]));
  OutputStream.WriteLine('  end;');
  OutputStream.WriteLine;
  OutputStream.WriteLine(Format('  raise EInvalidColumnIndex.Create(Index, %d);', [aColumnNames.Count]));

  OutputStream.WriteLine;
  OutputStream.WriteLine('end;');

end;

procedure ImplementFPCColumnTypeByIndex(aClassName: String; aTheTableName: String;
  aColumnNames: TStringList; aColumnTypes: TStringList;
  OutputStream: TMyTextStream);
var
  i: Integer;

begin
  OutputStream.WriteLine(Format('class function %s.FPCColumnTypeByIndex(Index: Integer): AnsiString;',
    [aClassName]));

  OutputStream.WriteLine('begin');
  OutputStream.WriteLine(Format('  Result := ' + Chr(39) + Chr(39) + ';', []));
  OutputStream.WriteLine(Format('  case Index of', []));

  for i := 0 to aColumnNames.Count - 1 do
    OutputStream.WriteLine(Format('    %d: Exit(' + Chr(39) + '%s' + Chr(39) + ');', [i, TranslateType(aColumnTypes[i])]));
  OutputStream.WriteLine('  end;');
  OutputStream.WriteLine;
  OutputStream.WriteLine(Format('  raise EInvalidColumnIndex.Create(Index, %d);', [aColumnNames.Count]));

  OutputStream.WriteLine;
  OutputStream.WriteLine('end;');

end;

procedure ImplementColumnIndexByName(aClassName: String; aTheTableName: String;
  aColumnNames: TStringList; aColumnTypes: TStringList;
  OutputStream: TMyTextStream);
var
  i: Integer;

begin
  OutputStream.WriteLine(Format('class function %s.ColumnIndexByName(const aName: AnsiString): Integer;',
    [aClassName]));

  OutputStream.WriteLine('begin');
  OutputStream.WriteLine(Format('  Result := -1;', []));
  OutputStream.WriteLine(Format('  case aName of', []));

  for i := 0 to aColumnNames.Count - 1 do
    OutputStream.WriteLine(Format('    ' + SingleQuote + '%s' + SingleQuote + ': Exit(%d);', [aColumnNames[i], i]));

  OutputStream.WriteLine('  end;');
  OutputStream.WriteLine;
  OutputStream.WriteLine(Format('  raise EInvalidColumnName.Create(aName)', []));

  OutputStream.WriteLine;
  OutputStream.WriteLine('end;');

end;

procedure ImplementToString(ClassName: String; TheTableName: String;
  ColumnNames: TStringList; ColumnTypes: TStringList;
  OutputStream: TMyTextStream);
var
  i: Integer;
  ColName, ColType: AnsiString;
  FieldName, FieldType: AnsiString;

begin
  OutputStream.WriteLine(Format('function %s.ToString: AnsiString;',
    [ClassName]));

  OutputStream.WriteLine('begin');
  OutputStream.WriteLine('  Result := ' + chr(39) + Chr(39) + ';');

  for i := 0 to ColumnNames.Count - 1 do
  begin
    ColName := ColumnNames[i];
    ColType := ColumnTypes[i];

    FieldName := TransformName(ColName);
    FieldType := TranslateType(ColType);

    OutputStream.WriteStr(Format('  Result += (' + Chr(39) + '%s: ' + Chr(39) + ' + ', [ColName]));
    if FieldType = 'AnsiString' then
      OutputStream.WriteStr(Format('%s', [FieldName]))
    else if FieldType = 'Integer' then
      OutputStream.WriteStr(Format('IntToStr(%s)', [FieldName]))
    else if FieldType = 'Int64' then
      OutputStream.WriteStr(Format('IntToStr(%s)', [FieldName]))
    else if FieldType = 'Double' then
      OutputStream.WriteStr(Format('FloatToStr(%s)', [FieldName]))
    else if FieldType = 'Extended' then
      OutputStream.WriteStr(Format('FloatToStr(%s)', [FieldName]))
    else if FieldType = 'Boolean' then
      OutputStream.WriteStr(Format('BoolToStr(%s)', [FieldName]))
    else if FieldType = 'TDate' then
      OutputStream.WriteStr(Format('DateToStr(%s)', [FieldName]))
    else if FieldType = 'TTime' then
      OutputStream.WriteStr(Format('TimeToStr(%s)', [FieldName]))
    else if FieldType = 'TDateTime' then
      OutputStream.WriteStr(Format('DateTimeToStr(%s)', [FieldName]));
    OutputStream.WriteLine(') + sLineBreak;')

  end;

  OutputStream.WriteLine('');
  OutputStream.WriteLine('end;');

end;

procedure ImplementGetters(ClassName: String; TheTableName: String;
  ColumnNames: TStringList; ColumnTypes: TStringList;
  OutputStream: TMyTextStream);
var
  i: Integer;
  ColName, ColType: AnsiString;
  FieldName, FieldType: AnsiString;

begin
  for i := 0 to ColumnNames.Count - 1 do
  begin
    ColName := ColumnNames[i];
    ColType := ColumnTypes[i];

    FieldName := TransformName(ColName);
    FieldType := TranslateType(ColType);

    OutputStream.WriteLine(Format('function %s.Get%s: %s;', [ClassName, FieldName, FieldType]));
    OutputStream.WriteLine('begin');
    OutputStream.WriteStr('  Result := ');
    case FieldType of
    'AnsiString':
      OutputStream.WriteStr(Format('FValues[%d].AsAnsiString;', [i]));
    'Integer':
      OutputStream.WriteStr(Format('FValues[%d].AsInteger;', [i]));
    'Int64':
      OutputStream.WriteStr(Format('FValues[%d].AsInteger;', [i]));
    'Double':
      OutputStream.WriteStr(Format('FValues[%d].AsExtended;', [i]));
    'Extended':
      OutputStream.WriteStr(Format('FValues[%d].AsExtended;', [i]));
    'Boolean':
      OutputStream.WriteStr(Format('FValues[%d].AsBoolean;', [i]));
    'TDate':
      OutputStream.WriteStr(Format('FValues[%d].AsTDate;', [i]));
    'TTime':
      OutputStream.WriteStr(Format('FValues[%d].AsTTime;', [i]));
    'TDateTime':
      OutputStream.WriteStr(Format('FValues[%d].AsTDateTime;', [i]));
    else
      raise Exception.Create('Unknown Field Type '+ FieldType);
    end;

    OutputStream.WriteLine('');
    OutputStream.WriteLine('end;');
    OutputStream.WriteLine('');

  end;

end;

procedure ImplementSetters(ClassName: String; TheTableName: String;
  ColumnNames: TStringList; ColumnTypes: TStringList;
  OutputStream: TMyTextStream);
var
  i: Integer;
  ColName, ColType: AnsiString;
  FieldName, FieldType: AnsiString;

begin
  for i := 0 to ColumnNames.Count - 1 do
  begin
    ColName := ColumnNames[i];
    ColType := ColumnTypes[i];

    FieldName := TransformName(ColName);
    FieldType := TranslateType(ColType);

    OutputStream.WriteLine(Format('procedure %s.Set%s(_%s: %s);', [ClassName, FieldName, FieldName, FieldType]));
    OutputStream.WriteLine('begin');
    OutputStream.WriteLine(Format('  FValues[%d].Free;', [i]));
    OutputStream.WriteLine;
    OutputStream.WriteStr(Format('  FValues[%d] := TDMValue.', [i]));

    case FieldType of
    'AnsiString':
      OutputStream.WriteStr(Format('CreateAnsiString(_%s);', [FieldName]));
    'Integer':
      OutputStream.WriteStr(Format('CreateInteger(_%s);', [FieldName]));
    'Int64':
      OutputStream.WriteStr(Format('CreateInteger(_%s);', [FieldName]));
    'Extended':
      OutputStream.WriteStr(Format('CreateExtended(_%s);', [FieldName]));
    'TDate':
      OutputStream.WriteStr(Format('CreateTDate(_%s);', [FieldName]));
    'TTime':
      OutputStream.WriteStr(Format('CreateTTime(_%s);', [FieldName]));
    else
      raise Exception.Create(Format('Invalid Type %s', [FieldType]));
    end;

    OutputStream.WriteLine('');
    OutputStream.WriteLine('end;');
    OutputStream.WriteLine('');

  end;

end;

procedure GenerateCode(DBConnection: TMySQLDatabaseConnection;
  TheTableName: AnsiString; OutputDir: AnsiString);
var
  Mapper: TName2ValueMapper;
  Template: TTemplateEngine;
  Response: TQueryResponse;
  i, NumElements: Integer;
  ARow: TStringList;
  ColumnNames, ColumnTypes: TStringList;
  ColName, FieldName: AnsiString;
  ColType, FieldType: AnsiString;
  OutputFile: AnsiString;
  OutputStream: TMyTextStream;
  UnitName, ClassName: AnsiString;

begin
  Mapper := TName2ValueMapper.Create;

  UnitName:= TransformName(TheTableName) + 'Unit';
  UnitName[1] := UpCase(UnitName[1]);
  OutputFile := OutputDir + '/' + UnitName + '.pp';
  OutputStream := TMyTextStream.Create(TFileStream.Create(OutputFile, fmCreate), True);
  Response := DBConnection.RunQuery(Format('Explain %s', [TheTableName]));

  if Response.NumRows = 0 then
  begin
    Response.Free;
    Exit;
  end;

  OutputStream.WriteLine('// This file is generated automatically using the following command:');
  OutputStream.WriteStr('//');
  for i := 0 to ParamCount  do
    OutputStream.WriteStr(Format('%s ', [ParamStr(i)]));
  OutputStream.WriteLine('');
  OutputStream.WriteLine('');

  Template := TTemplateEngine.CreateFromText(UnitHeader);
  Mapper.AddNameValue('UnitName', UnitName);
  OutputStream.WriteStr(Template.Map(Mapper));
  Template.Free;

  Template := TTemplateEngine.CreateFromText(DataCollectionDefinition);
  ClassName := 'T' + TransformName(TheTableName);
  Mapper.AddNameValue('ClassName', ClassName);
  OutputStream.WriteStr(Template.Map(Mapper));
  Template.Free;

  Template := TTemplateEngine.CreateFromText(DataClassDefinition);
  ClassName := 'T' + TransformName(TheTableName);
  Mapper.AddNameValue('ClassName', ClassName);
  OutputStream.WriteStr(Template.Map(Mapper));
  Template.Free;

  ColumnNames := TStringList.Create;
  ColumnTypes := TStringList.Create;

  NumElements := Response.NumRows;
  for i := 1 to Response.NumRows do
  begin
    ARow :=  Response.Row;
    ColName := ARow[0];
    ColType := ARow[1];
    FieldName := TransformName(ColName);
    FieldType := TransformName(ColType);

    ColumnNames.Add(ColName);
    ColumnTypes.Add(ColType);
    Response.Next;

    WriteLn(Format('%s: %s -> %s: %s', [ColName, ColType, FieldName, FieldType]));
  end;
  Response.Free;

  for i := 0 to NumElements - 1 do
  begin
    ColName := ColumnNames[i];
    ColType := ColumnTypes[i];

    FieldName := TransformName(ColName);
    FieldType := TranslateType(ColType);

    OutputStream.WriteLine(Format('    function Get%s: %s;', [FieldName, FieldType]));
    OutputStream.WriteLine(Format('    procedure Set%s(_%s: %s);', [FieldName, FieldName, FieldType]));

  end;
  OutputStream.WriteLine('');
  OutputStream.WriteLine('  public');

  for i := 0 to NumElements - 1 do
  begin
    ColName := ColumnNames[i];
    ColType := ColumnTypes[i];

    FieldName := TransformName(ColName);
    FieldType := TranslateType(ColType);

    OutputStream.WriteLine(Format('    // %s: %s', [ColName, ColType]));
    OutputStream.WriteLine(Format('    property %s: %s read Get%s write Set%s;',
       [FieldName, FieldType, FieldName, FieldName]));

    ColName := ColumnNames[i];
    ColType := ColumnTypes[i];
    FieldName := TransformName(ColName);
    FieldType := TranslateType(ColType);

  end;

  OutputStream.WriteLine('');
  OutputStream.WriteLine('    function ToString: AnsiString; override;');
  OutputStream.WriteLine('    constructor Create;');
  OutputStream.WriteStr('    constructor Create(');
  for i := 0 to NumElements - 1 do
  begin
    ColName := ColumnNames[i];
    ColType := ColumnTypes[i];

    FieldName := TransformName(ColName);
    FieldType := TranslateType(ColType);
    if i <> 0 then
      OutputStream.WriteStr('; ');
    OutputStream.WriteStr(Format('_%s: %s', [FieldName, FieldType]));
  end;
  OutputStream.WriteStr(');');

  OutputStream.WriteLine('');
  OutputStream.WriteLine('');
  OutputStream.WriteLine('  end;');

  OutputStream.WriteLine('');
  OutputStream.WriteLine('implementation');
  OutputStream.WriteLine('uses');
  OutputStream.WriteLine('  SysUtils;');

  OutputStream.WriteLine('');
  ImplementStaticFuctions(ClassName, TheTableName, ColumnNames, ColumnTypes, OutputStream);
  OutputStream.WriteLine('');
  ImplementCreate(ClassName, TheTableName, ColumnNames, ColumnTypes, OutputStream);
  OutputStream.WriteLine('');
  ImplementColumnNameByIndex(ClassName, TheTableName, ColumnNames, ColumnTypes, OutputStream);
  OutputStream.WriteLine('');
  ImplementMySQLColumnTypeByIndex(ClassName, TheTableName, ColumnNames, ColumnTypes, OutputStream);
  OutputStream.WriteLine('');
  ImplementFPCColumnTypeByIndex(ClassName, TheTableName, ColumnNames, ColumnTypes, OutputStream);
  OutputStream.WriteLine('');
  ImplementColumnIndexByName(ClassName, TheTableName, ColumnNames, ColumnTypes, OutputStream);
  OutputStream.WriteLine('');
  ImplementToString(ClassName, TheTableName, ColumnNames, ColumnTypes, OutputStream);
  OutputStream.WriteLine('');
  ImplementGetters(ClassName, TheTableName, ColumnNames, ColumnTypes, OutputStream);
  OutputStream.WriteLine('');
  ImplementSetters(ClassName, TheTableName, ColumnNames, ColumnTypes, OutputStream);
  OutputStream.WriteLine('');

  OutputStream.WriteLine(sLineBreak + sLineBreak);
  OutputStream.WriteLine('end.');

  OutputStream.Free;
end;

var
  DBConnection: TMySQLDatabaseConnection;
  Tables: TStringList;
  TableName: AnsiString;
  i: Integer;

begin
  DBConnection := TMySQLDatabaseConnection.Create(
    GetRunTimeParameterManager.ValueByName['--DBUsername'].AsAnsiString,
    GetRunTimeParameterManager.ValueByName['--DBPassword'].AsAnsiString,
    GetRunTimeParameterManager.ValueByName['--DBHost'].AsAnsiString);
  DBConnection.Connect;
  DBConnection.ActiveDB := GetRunTimeParameterManager.ValueByName['--DBName'].AsAnsiString;

  Tables := Split(GetRunTimeParameterManager.ValueByName['--Tables'].AsAnsiString, ',');

  for TableName in Tables do
    GenerateCode(DBConnection, TableName,
      GetRunTimeParameterManager.ValueByName['--Output-Dir'].AsAnsiString);

//  DBConnection.Free;
end.

