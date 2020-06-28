unit BaseDataModuleUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TImportFunction = procedure (Source: AnsiString; Target: Pointer);

  TColumnInfo = record
    ColumnName: AnsiString;
    Target: Pointer;
    ImportFunction: TImportFunction;
  end;

  TColumnInfoArray = array of TColumnInfo;
  { TBaseDataModule }

  TBaseDataModule = class(TObject)
  protected

  public
    class function TableName: AnsiString; virtual; abstract;
    class function NumFields: Integer; virtual; abstract;
    class function GetColumnNameByIndex(Index: Integer): AnsiString; virtual; abstract;

    constructor Create(Columns: TColumnInfoArray);
    constructor Create;

  end;

  { TBaseDataModuleManager }

  TBaseDataModuleManager = class(TObject)
   private

   protected
     CS: TRTLCriticalSection;

     procedure Lock; virtual;
     procedure Unlock; virtual;
   public
    // TBaseDataModule will not free DBConnection object.
    constructor Create;
    destructor Destroy; override;
  end;


function CreateColumnInfo (const ColumnName: AnsiString; Target: Pointer; ImportFunction: TImportFunction): TColumnInfo;

procedure ToInt(Source: AnsiString; Target: Pointer);
procedure ToString(Source: AnsiString; Target: Pointer);

implementation

function CreateColumnInfo(const ColumnName: AnsiString; Target: Pointer;
  ImportFunction: TImportFunction): TColumnInfo;
begin
  Result.ColumnName := ColumnName;
  Result.Target := Target;
  Result.ImportFunction := ImportFunction;

end;

procedure ToInt(Source: AnsiString; Target: Pointer);
begin
  PInteger(Target)^ := StrToInt(source);

end;

procedure ToString(Source: AnsiString; Target: Pointer);
begin
  PString(Target)^ := Source;

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

constructor TBaseDataModuleManager.Create;
begin
  inherited Create;

  InitCriticalSection(CS);

end;

destructor TBaseDataModuleManager.Destroy;
begin
  DoneCriticalsection(CS);

  inherited Destroy;
end;

{ TBaseDataModule }

constructor TBaseDataModule.Create(Columns: TColumnInfoArray);
begin
  inherited Create;

end;

constructor TBaseDataModule.Create;
begin
  inherited Create;

end;

end.

