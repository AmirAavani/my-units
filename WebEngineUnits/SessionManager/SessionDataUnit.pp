unit SessionDataUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, ValueUnit;

type
  TSessionID = AnsiString;

  TSessionValue = class(TValue)

  end;

  TNameValueMap = specialize TFPGMap<AnsiString, TSessionValue>;

  { TSessionData }

  TSessionData = class(TObject)
  private
    FNameValueMap: TNameValueMap;
    FCreationTime: Integer;
    FLastActivity: Integer;
    FSessionID: TSessionID;
    function GetValueByName(aName: AnsiString): TSessionValue;
    procedure SetValueByName(aName: AnsiString; AValue: TSessionValue);

  public
    property SessionID: TSessionID read FSessionID;
    property ValueByName[aName: AnsiString]: TSessionValue read GetValueByName write SetValueByName;
    property NameValueMap: TNameValueMap read FNameValueMap;
    property CreationTime: Integer read FCreationTime;
    property LastActivity: Integer read FLastActivity;


    constructor Create(SID: TSessionID; nvMap: TNameValueMap = nil; CTimestamp: Integer = 0; LATimestamp: Integer = 0);
    destructor Destroy; override;

    procedure UpdateLastActivity;
  end;

implementation

{ TSessionData }

function TSessionData.GetValueByName(aName: AnsiString): TSessionValue;
begin

end;

procedure TSessionData.SetValueByName(aName: AnsiString; AValue: TSessionValue);
begin

end;

constructor TSessionData.Create(SID: TSessionID; nvMap: TNameValueMap;
  CTimestamp: Integer; LATimestamp: Integer);
begin
  inherited Create;

  FSessionID := SID;
  FNameValueMap := nvMap ;
  FCreationTime := CTimestamp;
  FLastActivity := LATimestamp;

  if FNameValueMap = nil then
  begin
    FNameValueMap := TNameValueMap.Create;
    FNameValueMap.Sorted := True;
  end;

  if CreationTime = 0 then
    FCreationTime := DateTimeToTimeStamp(Now).Time;
  if LastActivity = 0 then
    FLastActivity := DateTimeToTimeStamp(Now).Time;

end;

destructor TSessionData.Destroy;
begin
  FNameValueMap.Free;

  inherited Destroy;
end;

procedure TSessionData.UpdateLastActivity;
begin
  FLastActivity := DateTimeToTimeStamp(Now).Time;
end;

end.

