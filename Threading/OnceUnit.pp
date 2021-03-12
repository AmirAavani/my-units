unit OnceUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPtrArray = array of Pointer;
  TOnceProc = procedure (Arguments: TPtrArray);

  { TOnce }

  TOnce = class(TObject)
  private
    FBeenCalled: Boolean;
    FOnceProc: TOnceProc;
    FObj: TPtrArray;
    CS: TRTLCriticalSection;

  public
    property BeenCalled: Boolean read FBeenCalled;
    constructor Create(OnceProc: TOnceProc; Obj: TPtrArray);
    destructor Destroy; override;

    function Run: Boolean;

  end;

implementation

{ TOnce }

constructor TOnce.Create(OnceProc: TOnceProc; Obj: TPtrArray);
begin
  inherited Create;

  FOnceProc := OnceProc;
  FObj := Obj;
  FBeenCalled := False;

  InitCriticalSection(CS);
end;

destructor TOnce.Destroy;
begin
   DoneCriticalsection(CS);

  inherited Destroy;
end;

function TOnce.Run: Boolean;
begin
  EnterCriticalsection(CS);

  if FBeenCalled then
  begin
    LeaveCriticalsection(CS);

    Exit(False);
  end;

  FOnceProc(FObj);
  FBeenCalled := True;
  LeaveCriticalsection(CS);

  Result := True;
end;

end.

