unit StepHanderUnit;

{$mode objfpc}{$H+}

interface

uses
  StreamUnit, Classes, SysUtils;

type

  { TStepHandler }

  TStepHandler = class(TObject)
  private
    FStepIndex: Integer;
    FStepName: AnsiString;

  public
    property StepName: AnsiString read FStepName;
    property StepIndex: Integer read FStepIndex;

    constructor Create(_StepIndex: Integer; _StepName: AnsiString);
    destructor Destroy; override;

    function Run(InputStream, OutputStream: TMyBinStream): Boolean; virtual; abstract;
  end;

implementation

{ TStepHandler }

constructor TStepHandler.Create(_StepIndex: Integer; _StepName: AnsiString);
begin
  inherited Create;


  FStepIndex:= _StepIndex;
  FStepName:= _StepName;

end;

destructor TStepHandler.Destroy;
begin

  inherited Destroy;
end;

end.

