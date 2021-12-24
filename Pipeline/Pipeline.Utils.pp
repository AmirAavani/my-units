unit Pipeline.Utils;

{$mode objfpc}{$H+}

interface

uses
  Pipeline.TypesUnit, PipelineUnit;

type

  { TFilePattern }

  TFilePattern = class(TObject)
  private
    FAllFiles: TAnsiStringList;
    FPattern: AnsiString;

  public
    property AllFiles: TAnsiStringList read FAllFiles;
    property Pattern: AnsiString read FPattern;

    constructor Create(constref PatternStr: AnsiString);
    destructor Destroy; override;

    function FilterFilesModule(TaskID, NumTasks: Integer): TAnsiStringList;
    function FilterFilesDiv(TaskID, NumTasks: Integer): TAnsiStringList;
    function FilterMyFilesModule(Task: TTask): TAnsiStringList;
    function FilterMyFilesDiv(Task: TTask): TAnsiStringList;

  end;


implementation
uses
  Classes, SysUtils, PathHelperUnit, RegExpr;

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

{ TFilePattern }

constructor TFilePattern.Create(constref PatternStr: AnsiString);
begin
  inherited Create;

  FPattern := PatternStr;
  FAllFiles := ExpandPattern(PatternStr);

end;

destructor TFilePattern.Destroy;
begin
  FAllFiles.Free;

  inherited Destroy;
end;

function TFilePattern.FilterFilesDiv(TaskID, NumTasks: Integer
  ): TAnsiStringList;
var
  i, Len: Integer;

begin
  Result := TAnsiStringList.Create;

  Len := FAllFiles.Count div NumTasks;

  for i := (TaskID - 1) * Len to TaskID * Len - 1 do
    Result.Add(FAllFiles[i]);

end;

function TFilePattern.FilterMyFilesModule(Task: TTask): TAnsiStringList;
begin
  Result := Self.FilterFilesModule(Task.ID, Task.Count);

end;

function TFilePattern.FilterMyFilesDiv(Task: TTask): TAnsiStringList;
begin
  Result := Self.FilterFilesDiv(Task.ID, Task.Count);

end;

function TFilePattern.FilterFilesModule(TaskID, NumTasks: Integer
  ): TAnsiStringList;
var
  i: Integer;

begin
  Result := TAnsiStringList.Create;

  i := TaskID;
  while i < FAllFiles.Count do
  begin
    Result.Add(FAllFiles[i]);
    Inc(i, NumTasks);

  end;

end;

end.
