unit UtilsUnit;

{$mode objfpc}{$H+}

interface

uses
  StringUnit, Classes, SysUtils;

function TransformName(ColumnName: AnsiString): AnsiString;
function TranslateType(ColumnType: AnsiString): AnsiString;


type
  ENotSupportedYet = class(Exception);
  ENotSupported = class(Exception);

implementation

function TransformName(ColumnName: AnsiString): AnsiString;
var
  Parts: TStringList;
  i: Integer;
  Part: AnsiString;

begin
  Parts := Split(ColumnName, '_');

  for i := 0 to Parts.Count - 1 do
  begin
    Part := Parts[i];
    if Part <> '' then
      Part[1] := upCase(Part[1]);
    Parts[i] := Part;
  end;

  Result := JoinStrings(Parts, '');
end;

const
  TranslateTypeTable: array of array[1..2] of AnsiString =
      (
      // String Types
        ('CHAR', 'AnsiString'),
        ('VARCHAR', 'AnsiString'),
        ('NVARCHAR', 'AnsiString'),
        ('BINARY', 'AnsiString'),
        ('VARBINARY', 'AnsiString'),
        ('TINYBLOB', 'AnsiString'),
        ('BLOB', 'AnsiString'),
        ('MEDIUMBLOB', 'AnsiString'),
        ('LONGBLOB', 'AnsiString'),
        ('TINYTEXT', 'AnsiString'),
        ('TEXT', 'AnsiString'),
        ('MEDIUMTEXT', 'AnsiString'),
        ('LONGTEXT', 'AnsiString'),
        ('JSON', 'AnsiString'),

        // Numeric Types
        ('TINYINT', 'Integer'),
        ('SMALLINT', 'Integer'),
        ('INT', 'Integer'),
        ('MEDIUMINT', 'Integer'),
        ('BIGINT', 'Int64'),
        ('FLOAT', 'Extended'),
        ('DOUBLE', 'Extended'),
        ('DECIMAL', 'Extended'),
        ('BIT', 'Integer'),
        ('BOOLEAN', 'Boolean'),

        // Date and Time Types.
        ('DATE', 'TDate'),
        ('TIME', 'TTime'),
        ('DATETIME', 'TDateTime'),
        ('TIMESTAMP', ''),
        ('YEAR', 'Integer'),

        // Special Data Types.
        ('ENUM', ''),
        ('SET', ''),
        ('GEOMETRY', ''),
        ('POINT', ''),
        ('LINESTRING', ''),
        ('POLYGON', ''),
        ('GEOMETRYCOLLECTION', ''),
        ('MULTILINESTRING', ''),
        ('MULTIPOINT', ''),
        ('MULTIPOLYGON', ''),
        ('MULTIPOLYGON', '')
      );
function TranslateType(ColumnType: AnsiString): AnsiString;
var
  i: Integer;
  ColumnTypePrefix: AnsiString;

begin
  ColumnTypePrefix := ColumnType;
  if Pos('(', ColumnType) <> 0 then
    ColumnTypePrefix := Copy(ColumnType, 1, Pos('(', ColumnType) - 1);

  for i := Low(TranslateTypeTable) to High(TranslateTypeTable) do
    if UpperCase(ColumnTypePrefix) = TranslateTypeTable[i][1] then
    begin
      Result := TranslateTypeTable[i][2];

      if Result = '' then
        raise ENotSupportedYet.Create(Format('MySQL Type %s is not supported (yet)!', [ColumnTypePrefix]));
      Exit;
    end;


  raise ENotSupportedYet.Create(Format('MySQL Type %s is unknown to me!', [ColumnTypePrefix]));

end;

end.

