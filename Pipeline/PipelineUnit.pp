unit PipelineUnit;

{$mode objfpc}{$H+}

interface

uses
  StepHanderUnit, fgl, Classes, SysUtils;

type

  { TPipeline }

  TPipeline = class(TObject)
  private type
    // TStepHandlers =
  private
    FName: AnsiString;

  public
    property Name: AnsiString read FName;

    constructor Create(Name: AnsiString);
    destructor Destroy; override;

    function AddNewStep(InputPattern, OutputPattern: AnsiString;
        Handler: TStepHandler; NumTasks: Integer): Boolean;
  end;

implementation

{ TPipeline }

constructor TPipeline.Create(Name: AnsiString);
begin
  inherited Create;

  FName := _Name;

end;

destructor TPipeline.Destroy;
begin
  inherited Destroy;
end;

function TPipeline.AddNewStep(InputPattern, OutputPattern: AnsiString;
  Handler: TStepHandler; NumTasks: Integer): Boolean;
begin

end;

end.

