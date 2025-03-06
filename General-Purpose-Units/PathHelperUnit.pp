unit PathHelperUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit;

function JoinPath(const Parent, Rest: AnsiString; DirSeparator: AnsiString = '/'): AnsiString;
function JoinPath(const Parts: array of AnsiString; DirSeparator: AnsiString = '/'): AnsiString;
function GetAllFiles(APath: AnsiString): TAnsiStrings;

implementation
uses
  StringUnit;

function JoinPath(const Parent, Rest: AnsiString; DirSeparator: AnsiString
  ): AnsiString;
begin
  Result := Parent;
  if not IsSuffix(DirSeparator, Parent) and not IsPrefix(DirSeparator, Rest) then
    Result += DirSeparator;
  Result += Rest;

end;

function JoinPath(const Parts: array of AnsiString; DirSeparator: AnsiString
  ): AnsiString;
var
  i: Integer;

begin
  Result := '';
  if Length(Parts) = 0 then
    Exit;
  Result := Parts[0];

  for i := 1 to High(Parts) do
  begin
    Result += DirSeparator;
    Result += Parts[i];

  end;

end;

function GetAllFiles(APath: AnsiString): TAnsiStrings;
var
  SearchResult: TSearchRec;

begin
  Result := TAnsiStrings.Create;

   if FindFirst(APath + '/*', 0, SearchResult) <> 0 then
   begin
     Exit;

   end;

   Result.Add(JoinPath(APath, SearchResult.Name));
   while FindNext(SearchResult) = 0 do
    Result.Add(JoinPath(APath, SearchResult.Name));

end;

end.

