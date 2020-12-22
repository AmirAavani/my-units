unit Pipeline.Utils;

{$mode objfpc}{$H+}

interface

uses
  Pipeline.TypesUnit, Classes, SysUtils;

function ExpandPattern(aPattern: AnsiString): TAnsiStringList;

implementation
uses
  PathHelperUnit, RegExpr;

function ExpandPattern(aPattern: AnsiString): TAnsiStringList;
var
  ExpandableRE: TRegExpr;

  procedure RecGenerate(Index: Integer; Prefix: AnsiString; Segments: TStringList;
    Result: TAnsiStringList);
  var
    i: Integer;
    Count: Integer;
    CurSegment, CurPrefix: AnsiString;

  begin
    if Index = Segments.Count then
    begin
      Result.Add(Prefix);
      Exit;

    end;

    CurSegment := Segments[Index];
    if ExpandableRE.Exec(CurSegment) then
    begin
      Count := StrToInt(ExpandableRE.Match[1]);
      CurPrefix := Copy(CurSegment, 1,
        Length(CurSegment) - Length(ExpandableRE.Match[1]) - 1);

      for i := 0 to Count - 1 do
      begin
        RecGenerate(Index + 1,
          JoinPath(Prefix, Format('/%s-%.5d-of-%.5d', [CurPrefix, i, Count])),
          Segments, Result);
      end;

      Exit;
    end;

    RecGenerate(Index + 1, JoinPath(Prefix, CurSegment), Segments, Result);
  end;

var
  Segments: TStringList;

begin
  ExpandableRE := TRegExpr.Create('.*@([0-9]+?)$');
  Segments := TStringList.Create;
  Segments.Delimiter := '/';
  Segments.DelimitedText := aPattern;

  Result := TAnsiStringList.Create;
  RecGenerate(0, '', Segments, Result);

  Segments.Free;
  ExpandableRE.Free;

end;

end.
