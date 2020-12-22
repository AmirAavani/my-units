unit PathHelperUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function JoinPath(Parent, Rest: AnsiString; DirSeparator: AnsiString = '/'): AnsiString;

implementation
uses
  StringUnit;

function JoinPath(Parent, Rest: AnsiString; DirSeparator: AnsiString
  ): AnsiString;
begin
  Result := Parent;
  if not IsSuffix(DirSeparator, Parent) and not IsPrefix(DirSeparator, Rest) then
    Result += DirSeparator;
  Result += Rest;

end;

end.

