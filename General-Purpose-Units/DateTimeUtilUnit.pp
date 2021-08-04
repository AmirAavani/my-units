unit DateTimeUtilUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function CurrentTimestamp: Int32;
function CurrentTimestampInMS: Int64;

implementation

function CurrentTimestamp: Int32;
begin
  Result := DateTimeToTimeStamp(Now).Time div 1000;

end;

function CurrentTimestampInMS: Int64;
begin
  Result := DateTimeToTimeStamp(Now).Time;

end;

end.

