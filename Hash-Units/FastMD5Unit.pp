unit FastMD5Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, md5;

function MD5Sum(constref S: AnsiString): TMD5Digest; inline;

implementation

function MD5Sum(constref S: AnsiString): TMD5Digest;
begin
  Result := MD5String(S);

end;

end.

