unit SampleUnit;
{$Mode objfpc}
interface

uses 
    classes, fgl, sysutils, ProtoHelperUnit, ProtoHelperListsUnit, ProtoStreamUnit;

type


  TTestID = Class(TObject)
  // Declarations for uint64 uid64 = 5;
  private
    FUid64: UInt64;
  public
    property Uid64: UInt64 read FUid64 write FUid64;

  // Declarations for uint32 uid32 = 6;
  private
    FUid32: UInt32;
  public
    property Uid32: UInt32 read FUid32 write FUid32;

  // Declarations for int64 id64 = 7;
  private
    FId64: Int64;
  public
    property Id64: Int64 read FId64 write FId64;

  // Declarations for int32 id32 = 8;
  private
    FId32: Int32;
  public
    property Id32: Int32 read FId32 write FId32;

  // Declarations for string idStr = 10;
  private
    FIdStr: AnsiString;
  public
    property IdStr: AnsiString read FIdStr write FIdStr;

  private 
    procedure SaveToStream(Stream: TProtoStreamWriter);
  public 
    constructor Create;
    constructor Create(aUid64: UInt64; aUid32: UInt32; aId64: Int64; aId32: Int32; aIdStr: AnsiString);
    destructor Destroy; override;
    function ToString: AnsiString; override;
    procedure SaveToStream(Stream: TStream);
 
  end;


implementation

uses strutils;

 { TTestID }

constructor TTestID.Create;
begin
  inherited Create;

  FUid64 := 0;
  FUid32 := 0;
  FId64 := 0;
  FId32 := 0;
  FIdStr := '';
end;

constructor TTestID.Create(aUid64: UInt64; aUid32: UInt32; aId64: Int64; aId32: Int32; aIdStr: AnsiString);
begin
  inherited Create;

  FUid64 := aUid64; 
  FUid32 := aUid32; 
  FId64 := aId64; 
  FId32 := aId32; 
  FIdStr := aIdStr; 

end;

destructor TTestID.Destroy;
begin

  inherited;
end;

function TTestID.ToString: AnsiString;
begin
  Result := '';

  if FUid64 <> 0 then
  begin
    Result += Format('uid64: %d ', [FUid64]);
    Result += sLineBreak;
  end;

  if FUid32 <> 0 then
  begin
    Result += Format('uid32: %d ', [FUid32]);
    Result += sLineBreak;
  end;

  if FId64 <> 0 then
  begin
    Result += Format('id64: %d ', [FId64]);
    Result += sLineBreak;
  end;

  if FId32 <> 0 then
  begin
    Result += Format('id32: %d ', [FId32]);
    Result += sLineBreak;
  end;

  if FIdStr <> '' then
  begin
    Result += Format('idStr: %s ', [FIdStr]);
    Result += sLineBreak;
  end;


end;

procedure TTestID.SaveToStream(Stream: TStream);
var
  ProtoStream: TProtoStreamWriter;

begin
  ProtoStream := TProtoStreamWriter.Create;
  Self.SaveToStream(ProtoStream);

  ProtoStream.WriteToStream(Stream);
  ProtoStream.Free;
end;

procedure TTestID.SaveToStream(Stream: TProtoStreamWriter);
begin
  if FUid64 <> 0 then
    Stream.WriteUInt64(5, FUid64);

  if FUid32 <> 0 then
    Stream.WriteUInt32(6, FUid32);

  if FId64 <> 0 then
    Stream.WriteInt64(7, FId64);

  if FId32 <> 0 then
    Stream.WriteInt32(8, FId32);

  if FIdStr <> '' then
    Stream.WriteString(10, FIdStr);

end;

end.
