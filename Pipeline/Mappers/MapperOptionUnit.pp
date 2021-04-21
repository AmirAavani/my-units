unit MapperOptionUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;
type
  TSharder = function (constref Key: AnsiString): UInt64;

  { TMappingOptions }

  TMappingOptions = class(TObject)
  private
    FNumShards: Integer;
    FSharder: TSharder;
    FThreadCount: Integer;
  public
    property NumShards: Integer read FNumShards;
    property Sharder: TSharder read FSharder;
    property ThreadCount: Integer read FThreadCount;

  public
    constructor _Create;
    class function Create: TMappingOptions;

    function SetNumShards(_NumShards: Integer): TMappingOptions;
    function SetSharder(_Sharder: TSharder): TMappingOptions;
    function SetThreadCount(_ThreadCount: Integer): TMappingOptions;
  end;


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

{ TMappingOptions }

constructor TMappingOptions._Create;
begin
  inherited Create;

  FNumShards := 1;
  FSharder := @DefaultSharder;
  FThreadCount := 1;

end;

class function TMappingOptions.Create: TMappingOptions;
begin
  Result := TMappingOptions._Create;

end;

function TMappingOptions.SetNumShards(_NumShards: Integer): TMappingOptions;
begin
  Result := Self;
  Result.FNumShards := _NumShards;

end;

function TMappingOptions.SetSharder(_Sharder: TSharder): TMappingOptions;
begin
  Result := Self;
  Result.FSharder := _Sharder;


end;

function TMappingOptions.SetThreadCount(_ThreadCount: Integer): TMappingOptions;
begin
  Result := Self;
  Result.FThreadCount := _ThreadCount;

end;

end.

