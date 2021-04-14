unit MapperOptionUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;
type
  TSharder = function (constref Key: AnsiString): UInt64;

  TMappingOptions = record
    NumShards: Integer;
    Sharder: TSharder;
    ThreadCount: Integer;

  end;

function DefaultMappingOptions: TMappingOptions;

implementation
uses
  HlpHashFactory,
  HlpIHash,
  HlpIHashResult;

function DefaultSharder(constref Key: AnsiString): UInt64;
begin
  Result := THashFactory.THash64.CreateXXHash64().ComputeString(Key, TEncoding.UTF8).
    GetUInt64();

end;

function DefaultMappingOptions: TMappingOptions;
begin
  Result.NumShards := 1;
  Result.Sharder := @DefaultSharder;
  Result.ThreadCount := 1;

end;

end.

