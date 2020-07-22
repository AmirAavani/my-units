program CreateDM;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  sysutils, Classes, ParameterManagerUnit, MySQLDBConnectorUnit, StringUnit,
  QueryResponeUnit, UtilsUnit, BaseDataModuleUnit, StreamUnit,
  TemplateEngineUnit;

const
  UnitHeader: AnsiString =
    'unit {{@UnitName}};' + sLineBreak
    + sLineBreak
    + '{$mode objfpc}{$H+}' + sLineBreak
    + sLineBreak
    + 'interface' + sLineBreak
    + sLineBreak
    + 'uses' + sLineBreak
    + '  BaseDataModuleUnit;' + sLineBreak
    + sLineBreak
    + 'type' + sLineBreak
    + sLineBreak;

  ClassDefinition: AnsiString =
  '{ {{@ClassName}} }'
  + sLineBreak
  + sLineBreak
  + '  {{@ClassName}} = class(TBaseDataModule)' + sLineBreak
  + '  public' + sLineBreak
  + '    class function TableName: AnsiString; override;' + sLineBreak
  + '    class function NumFields: Integer; override;' + sLineBreak
  + '  private' + sLineBreak;

  procedure ImplementStaticFuctions(ClassName: AnsiString; TheTableName: AnsiString;
    ColumnNames, ColumnTypes: TStringList; OutputStream: TMyTextStream);
  begin
    OutputStream.WriteLine(Format('class function %s.TableName: AnsiString;', [ClassName]));
    OutputStream.WriteLine('begin');
    OutputStream.WriteLine(Format('  Exit(' + Chr(39) + '%s' + Chr(39) + ')', [TheTableName]));
    OutputStream.WriteLine('');
    OutputStream.WriteLine('end;');
    OutputStream.WriteLine('');

    OutputStream.WriteLine(Format('class function %s.NumFields: Integer;', [ClassName]));
    OutputStream.WriteLine('begin');
    OutputStream.WriteLine(Format('  Result := %d;', [ColumnNames.Count]));
    OutputStream.WriteLine('end;');
    OutputStream.WriteLine('');

  end;

function ImplementCreate(ClassName, TheTableName: AnsiString;
    ColumnNames, ColumnTypes: TStringList; OutputStream: TMyTextStream): AnsiString;
var
  i: Integer;
  ColName, ColType, FieldName, FieldType: AnsiString;

begin

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
  OutputStream.WriteLine('  inherited Create;' + sLineBreak);
  for i := 0 to ColumnNames.Count - 1 do
  begin
    ColName := ColumnNames[i];
    ColType := ColumnTypes[i];

    FieldName := TransformName(ColName);
    FieldType := TranslateType(ColType);

    OutputStream.WriteLine(Format('  F%s := _%s;', [FieldName, FieldName]));
  end;
  OutputStream.WriteLine(sLineBreak + 'end;');

end;

function ImplementSetValueByColumnName(ClassName, TheTableName: AnsiString;
    ColumnNames, ColumnTypes: TStringList; OutputStream: TMyTextStream): AnsiString;
var
  i: Integer;
  ColName, ColType, FieldName, FieldType: AnsiString;

begin
  OutputStream.WriteLine(Format('procedure %s.SetValueByColumnName(ColumnName: AnsiString; StrValue: AnsiString);',
    [ClassName]));
  OutputStream.WriteLine('begin');

  for i := 0 to ColumnNames.Count - 1 do
  begin
    ColName := ColumnNames[i];
    ColType := ColumnTypes[i];

    FieldName := TransformName(ColName);
    FieldType := TranslateType(ColType);

    if i <> 0 then
      OutputStream.WriteStr('  else ')
    else
      OutputStream.WriteStr('  ');
    OutputStream.WriteLine(Format('if ColumnName = ' + Chr(39) + '%s' + Chr(39) + ' then', [ColName]));
    if FieldType = 'AnsiString' then
      OutputStream.WriteStr(Format('    F%s := StrValue', [FieldName]))
    else if FieldType = 'Integer' then
      OutputStream.WriteStr(Format('    F%s := StrToInt(StrValue)', [FieldName]))
    else if FieldType = 'Int64' then
      OutputStream.WriteStr(Format('    F%s := StrToInt64(StrValue)', [FieldName]))
    else if FieldType = 'Double' then
      OutputStream.WriteStr(Format('    F%s := StrToFloat(StrValue)', [FieldName]))
    else if FieldType = 'Extended' then
      OutputStream.WriteStr(Format('    F%s := StrToFloat(StrValue)', [FieldName]))
    else if FieldType = 'Boolean' then
      OutputStream.WriteStr(Format('    F%s := StrToBool(StrValue)', [FieldName]))
    else if FieldType = 'TDate' then
      OutputStream.WriteStr(Format('    F%s := StrToDate(StrValue)', [FieldName]))
    else if FieldType = 'TTime' then
      OutputStream.WriteStr(Format('    F%s := StrToTime(StrValue)', [FieldName]))
    else if FieldType = 'TDateTime' then
      OutputStream.WriteStr(Format('    F%s := StrToDateTime(StrValue)', [FieldName]));

    if i <> ColumnNames.Count - 1 then
      OutputStream.WriteLine('')
    else
      OutputStream.WriteLine(';')

  end;

  OutputStream.WriteLine('end;');

end;

procedure ImplementGetColumnByValue(aClassName: String; aTheTableName: String;
  aColumnNames: TStringList; aColumnTypes: TStringList;
  OutputStream: TMyTextStream);
var
  i: Integer;

begin
  OutputStream.WriteLine(Format('class function %s.GetColumnNameByIndex(Index: Integer): AnsiString;',
    [aClassName]));

  OutputStream.WriteLine('begin');

  for i := 0 to aColumnNames.Count - 1 do
  begin
    if i <> 0 then
      OutputStream.WriteStr('  else');

    OutputStream.WriteLine(Format('  if Index = %d then Exit(' + Chr(39) + '%s' + Chr(39) + ')', [i, aColumnNames[i]]));
  end;

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
  VAlueStr: AnsiString;

begin
  OutputStream.WriteLine(Format('function %s.ToString: AnsiString;',
    [ClassName]));

  OutputStream.WriteLine('begin');
  OutputStream.WriteLine('Result := ' + chr(39) + Chr(39) + ';');

  for i := 0 to ColumnNames.Count - 1 do
  begin
    ColName := ColumnNames[i];
    ColType := ColumnTypes[i];

    FieldName := TransformName(ColName);
    FieldType := TranslateType(ColType);

    OutputStream.WriteStr(Format('  Result += ' + Chr(39) + '(%s: ' + Chr(39), [ColName]));
    if FieldType = 'AnsiString' then
      OutputStream.WriteStr(Format('    F%s', [FieldName]))
    else if FieldType = 'Integer' then
      OutputStream.WriteStr(Format('    IntToStr(F%s)', [FieldName]))
    else if FieldType = 'Int64' then
      OutputStream.WriteStr(Format('    Int64ToStr(F%s)', [FieldName]))
    else if FieldType = 'Double' then
      OutputStream.WriteStr(Format('    FloatToStr(F%s)', [FieldName]))
    else if FieldType = 'Extended' then
      OutputStream.WriteStr(Format('    FloatToStr(F%s)', [FieldName]))
    else if FieldType = 'Boolean' then
      OutputStream.WriteStr(Format('    BoolToStr(F%s)', [FieldName]))
    else if FieldType = 'TDate' then
      OutputStream.WriteStr(Format('    DateToStr(F%s)', [FieldName]))
    else if FieldType = 'TTime' then
      OutputStream.WriteStr(Format('    TimeToStr(F%s)', [FieldName]))
    else if FieldType = 'TDateTime' then
      OutputStream.WriteStr(Format('    DateTimeToStr(F%s)', [FieldName]));
    OutputStream.WriteLine(' + sLineBreak;')

  end;

  OutputStream.WriteLine('');
  OutputStream.WriteLine('end;');

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
  for i := 0 to ParamCount - 1 do
    OutputStream.WriteStr(Format('%s ', [ParamStr(i)]));
  OutputStream.WriteLine('');
  OutputStream.WriteLine('');

  Template := TTemplateEngine.CreateFromText(UnitHeader);
  Mapper.AddNameValue('UnitName', UnitName);
  OutputStream.WriteStr(Template.Map(Mapper));
  Template.Free;

  Template := TTemplateEngine.CreateFromText(ClassDefinition);
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

    OutputStream.WriteLine(Format('    F%s: %s;', [FieldName, FieldType]));

  end;
  OutputStream.WriteLine('');
  OutputStream.WriteLine('  protected');
  OutputStream.WriteLine('    procedure SetValueByColumnName(ColumnName: AnsiString; StrValue: AnsiString); override;');
  OutputStream.WriteLine('    class function GetColumnNameByIndex(Index: Integer): AnsiString; override;');
  OutputStream.WriteLine('');
  OutputStream.WriteLine('  public');

  for i := 0 to NumElements - 1 do
  begin
    ColName := ColumnNames[i];
    ColType := ColumnTypes[i];

    FieldName := TransformName(ColName);
    FieldType := TranslateType(ColType);

    OutputStream.WriteLine(Format('    // %s: %s', [ColName, ColType]));
    OutputStream.WriteLine(Format('    property %s: %s read F%s write F%s;',
       [FieldName, FieldType, FieldName, FieldName]));

    ColName := ColumnNames[i];
    ColType := ColumnTypes[i];
    FieldName := TransformName(ColName);
    FieldType := TranslateType(ColType);

  end;

  OutputStream.WriteLine('');
  OutputStream.WriteLine('    function ToString: AnsiString; override;');
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
  ImplementSetValueByColumnName(ClassName, TheTableName, ColumnNames, ColumnTypes, OutputStream);
  OutputStream.WriteLine('');
  ImplementGetColumnByValue(ClassName, TheTableName, ColumnNames, ColumnTypes, OutputStream);
  OutputStream.WriteLine('');
  ImplementToString(ClassName, TheTableName, ColumnNames, ColumnTypes, OutputStream);
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
    GetRunTimeParameterManager.ValueByName['--DBUsername'],
    GetRunTimeParameterManager.ValueByName['--DBPassword'],
    GetRunTimeParameterManager.ValueByName['--DBHost']);
  DBConnection.Connect;
  DBConnection.ActiveDB := GetRunTimeParameterManager.ValueByName['--DBName'];

  Tables := Split(GetRunTimeParameterManager.ValueByName['--Tables'], ',');

  for TableName in Tables do
    GenerateCode(DBConnection, TableName,
      GetRunTimeParameterManager.ValueByName['--Output-Dir']);

  DBConnection.Free;
end.

