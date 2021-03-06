unit FileHelperUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TMatcherFunc = function (Path, FileName: AnsiString; Params: array of AnsiString): Boolean;

function GetAllFiles(MatcherFunc: TMatcherFunc; Params: array of AnsiString; Path: AnsiString): TStringList;

function GetMatcherByExtension(Path, FileName: AnsiString; Params: array of AnsiString): Boolean;

implementation
uses
  StringUnit, ALoggerUnit;

function GetAllFiles(MatcherFunc: TMatcherFunc; Params: array of AnsiString;
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

function GetMatcherByExtension(Path, FileName: AnsiString;
  Params: array of AnsiString): Boolean;
begin
  Result := ExtractFileExt(FileName) = Params[0];

end;

end.

