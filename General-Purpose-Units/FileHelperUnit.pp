unit FileHelperUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TMatcherFunc = function (constref Path, FileName: AnsiString; constref Params: array of AnsiString): Boolean;

function GetAllFiles(MatcherFunc: TMatcherFunc; Params: array of AnsiString; Path: AnsiString): TStringList;
function DeleteDir(constref DirPath: AnsiString): Boolean;
function GetMatcherByExtension(constref Path, FileName: AnsiString; Params: array of AnsiString): Boolean;
function CreateDir(constref Path: AnsiString; Recursive: Boolean): Boolean;
function GetFileSize(constref Filename: AnsiString): Int64;

implementation
uses
  StringUnit, ALoggerUnit, PathHelperUnit;

function GetAllFiles(MatcherFunc: TMatcherFunc;
  Params: array of AnsiString;
  Path: AnsiString): TStringList;
  procedure RecGetAllFiles(Path: AnsiString);
  var
    Info : TSearchRec;

  begin
    if FindFirst(Path + '*', faAnyFile, Info) = 0 then
    begin
      repeat
        with Info do
          if not IsPrefix('.', Name) then
          begin
            If (Attr and faDirectory) = faDirectory then
              RecGetAllFiles(Path + Name + '/')
            else if MatcherFunc(Path, Name, Params) then
              Result.Add(Path + Name);

          end;
      until FindNext(info)<>0;
      FindClose(Info);
    end;
  end;

begin
  Result := TStringList.Create;

  if not IsSuffix('/', Path) then
    Path := Path + '/';

  RecGetAllFiles(Path);
end;

function DeleteDir(constref DirPath: AnsiString): Boolean;
var
  Info : TSearchRec;

begin
  Result := False;
  if FindFirst (DirPath, faAnyFile, Info) <> 0 then
  begin
    Exit;

  end;

  repeat
    With Info do
    begin
      If (Attr and faDirectory) <> faDirectory then
      begin
        if not DeleteFile(JoinPath(DirPath, Info.Name)) then
        begin
          Exit;

        end;

      end;

      DeleteDir(JoinPath(DirPath, Info.Name));
    end;
  until FindNext(info)<>0;
  FindClose(Info);

end;

function GetMatcherByExtension(constref Path, FileName: AnsiString;
  Params: array of AnsiString): Boolean;
begin
  Result := ExtractFileExt(FileName) = Params[0];

end;

function CreateDir(constref Path: AnsiString; Recursive: Boolean): Boolean;
var
  Parts: TStringList;
  CurPath: AnsiString;
  Part: AnsiString;

begin
  if not Recursive then
  begin
    Exit(SysUtils.CreateDir(Path));

  end;

  Parts := TStringList.Create;
  Parts.Delimiter := '/';
  Parts.DelimitedText := Path;


  CurPath := '';
  Result := False;
  for Part in Parts do
  begin
    CurPath += Part;
    if CurPath = '' then
    begin
      CurPath += '/';
      Continue;

    end;

    if not DirectoryExists(CurPath) and not CreateDir(CurPath, False) then
      Exit;

    CurPath += '/';
  end;

  Result := True;
  Parts.Free;


end;

function GetFileSize(constref Filename: AnsiString): Int64;
var
  Info: TRawbyteSearchRec;

begin
  if FindFirst(Filename, faAnyFile, Info) <> 0 then
    Exit(0);

  Result := Info.Size;
end;

end.

