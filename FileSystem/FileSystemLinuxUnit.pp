unit FileSystemLinuxUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

procedure MustReadFile(const Filename: AnsiString; var Result: AnsiString);
// A Linux file starts with /.
function IsLinuxFile(const Filename: AnsiString): Boolean;
function FileExists(const FileName: AnsiString): Boolean;
function MustCreateDir(const Path: AnsiString): Boolean;

implementation

procedure MustReadFile(const Filename: AnsiString; var Result: AnsiString);
var
  Stream: TFileStream;
  InputFileSize: Int64;

begin
  Stream := TFileStream.Create(Filename, fmOpenRead);
  InputFileSize := Stream.Size;
  SetLength(Result, InputFileSize);
  Stream.Position := 0;

  Stream.Read(Result[1], InputFileSize);

  Stream.Free;
end;

function IsLinuxFile(const Filename: AnsiString): Boolean;
begin
  if Length(Filename) = 0 then
    Exit(False);

  if Filename[1] = '/' then
    Exit(True);
  if Filename[1] = '.' then
    Exit(True);

  Exit(False);
end;

function FileExists(const FileName: AnsiString): Boolean;
begin
  Result := SysUtils.FileExists(FileName);
end;

function MustCreateDir(const Path: AnsiString): Boolean;
var
  Parts: TStringArray;
  Current: AnsiString;
  i: Integer;

begin
  if DirectoryExists(Path) then
    Exit;
  Parts := Path.Split(['/']);
  Current := Parts[0];
  for i := 1 to High(Parts) do
  begin
    if not DirectoryExists(Current) then
      if not CreateDir(Current) then
      begin
        SetLength(Parts, 0);
        Exit(False);
      end;

    Current += '/' + Parts[i];

  end;
  if not DirectoryExists(Current) then
    if not CreateDir(Current) then
    begin
      SetLength(Parts, 0);
      Exit(False);
    end;

  SetLength(Parts, 0);
end;

end.

