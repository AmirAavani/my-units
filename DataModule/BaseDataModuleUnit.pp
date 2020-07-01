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

    constructor Create;
  end;

  { TBaseDataModuleManager }

   generic TBaseDataModuleManager<T> =  class(specialize TFPGList<T>)
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
    function AddFromResponse(aResponse: TQueryResponse): Integer;
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

  InitCriticalSection(CS);

end;

destructor TBaseDataModuleManager.Destroy;
var
  Obj: T;
begin
  EnterCriticalsection(CS);

  for Obj in Self do
    Obj.Free;

  LeaveCriticalsection(CS);

  DoneCriticalsection(CS);

  inherited Destroy;
end;

function TBaseDataModuleManager.AddFromResponse(aResponse: TQueryResponse
  ): Integer;
var
  Obj: T;
  i: Integer;

begin
  for i := 1 to aResponse.NumRows do
  begin
    Obj := T.Create;
    Obj.FillFromResponse(aResponse.Row, aResponse.Columns);
    Self.Add(Obj);

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

constructor TBaseDataModule.Create;
begin
  inherited Create;

end;

end.

