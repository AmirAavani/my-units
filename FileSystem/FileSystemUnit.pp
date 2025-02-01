unit FileSystemUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;


function FileExists(const Filename: AnsiString): Boolean;
function MustReadFile(const Filename: AnsiString): AnsiString;
procedure MustReadFile(const Filename: AnsiString; var Result: AnsiString);
procedure MustCreateDir(const Path: AnsiString);

implementation
uses
  FileSystemLinuxUnit, StringUnit;

function FileExists(const Filename: AnsiString): Boolean;
begin
  if FileSystemLinuxUnit.IsLinuxFile(Filename) then
    Exit(FileSystemLinuxUnit.FileExists(Filename));

  WriteLn('Invalid File type');
  Halt(1);

end;

function MustReadFile(const Filename: AnsiString): AnsiString;
begin
  Result := '';
  MustReadFile(Filename, Result);

end;

procedure MustReadFile(const Filename: AnsiString; var Result: AnsiString);
begin
  if FileSystemLinuxUnit.IsLinuxFile(Filename) then
  begin
    FileSystemLinuxUnit.MustReadFile(Filename, Result);
    Exit;
  end;

  WriteLn('Invalid File type');
  Halt(1);

end;

procedure MustCreateDir(const Path: AnsiString);
begin
  if FileSystemLinuxUnit.IsLinuxFile(Path) then
  begin
      FileSystemLinuxUnit.MustCreateDir(Path);
      Exit;
  end;
  WriteLn('Invalid File type');
  Halt(1);

end;

end.

