unit BaseDataModuleUnit;

{$mode objfpc}{$H+}

interface

uses
  DBConnectorUnit, QueryResponeUnit, Classes, SysUtils, fgl;

type
  { EInvalidColumnName }

  EInvalidColumnName = class(Exception)
  public
    constructor Create(ColumnName: AnsiString);
  end;

  { TBaseDataModule }

  TBaseDataModule = class(TObject)
  protected

    function FillFromResponse(Row, Column: TStringList): Boolean; virtual;
    procedure SetValueByColumnName(ColumnName: AnsiString; StrValue: AnsiString); virtual; abstract;

  public
    class function TableName: AnsiString; virtual; abstract;
    class function NumFields: Integer; virtual; abstract;
    class function GetColumnNameByIndex(Index: Integer): AnsiString; virtual; abstract;
    class function GetDataByIndex(Index: Integer): AnsiString; virtual; abstract;

    function ToString: AnsiString; override;
    constructor Create;
  end;


  { TBaseDataList }

  generic TBaseDataList<T> = class(specialize TFPGList<T>)
  public
    procedure PrintAll; virtual;

  end;

  { TBaseDataModuleManager }

   generic TBaseDataModuleManager<TData> =  class(TObject)
   public type
     TDataList = specialize TBaseDataList<TData>;

   private

   protected
     FDB: TDatabaseConnection;
     CS: TRTLCriticalSection;

     procedure Lock; virtual;
     procedure Unlock; virtual;
   public
    // TBaseDataModule will not free DBConnection object.
    constructor Create(aBD: TDatabaseConnection);
    destructor Destroy; override;

    // Returns the number of elements added or raise EInvalidColumnName Expection.
    function ExtractFromResponse(aResponse: TQueryResponse): TDataList;

  end;

procedure ToInt(Source: AnsiString; Target: Pointer);
procedure ToString(Source: AnsiString; Target: Pointer);

implementation

procedure ToInt(Source: AnsiString; Target: Pointer);
begin
  PInteger(Target)^ := StrToInt(source);

end;

procedure ToString(Source: AnsiString; Target: Pointer);
begin
  PString(Target)^ := Source;

end;

{ TBaseDataList }

procedure TBaseDataList.PrintAll;
var
  Obj: T;

begin
  for Obj in Self do
    WriteLn(T.ToString);
end;

{ EInvalidColumnName }

constructor EInvalidColumnName.Create(ColumnName: AnsiString);
begin
  inherited Create(Format('Column "%s" has not found', [ColumnName]));

end;

{ TBaseDataModuleManager }

procedure TBaseDataModuleManager.Lock;
begin
  EnterCriticalsection(CS);
end;

procedure TBaseDataModuleManager.Unlock;
begin
  LeaveCriticalsection(CS);
end;

constructor TBaseDataModuleManager.Create(aBD: TDatabaseConnection);
begin
  inherited Create;

  FDB := aBD;

  InitCriticalSection(CS);

end;

destructor TBaseDataModuleManager.Destroy;
begin
  DoneCriticalsection(CS);

  inherited Destroy;
end;

function TBaseDataModuleManager.ExtractFromResponse(aResponse: TQueryResponse
  ): TDataList;
var
  Obj: TData;
  i: Integer;

begin
  Result := TDataList.Create;

  for i := 1 to aResponse.NumRows do
  begin
    Obj := TData.Create;
    Obj.FillFromResponse(aResponse.Row, aResponse.Columns);
    Result.Add(Obj);

  end;
end;

{ TBaseDataModule }

function TBaseDataModule.FillFromResponse(Row, Column: TStringList): Boolean;
var
  i: Integer;

begin
  Result := True;

  for i := 0 to Row.Count - 1 do
    Self.SetValueByColumnName(Column[i], Row[i]);

end;

function TBaseDataModule.ToString: AnsiString;
var
  i: Integer;

begin
  Result := '';

  for i := 0 to NumFields do
    Result += Format('%s: %s' + sLineBreak, [GetColumnNameByIndex(i), GetDataByIndex(i)]);

end;

constructor TBaseDataModule.Create;
begin
  inherited Create;

end;

end.

