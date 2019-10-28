unit ThreadUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TThreadFunctionPtr = function (Args: array of TObject): Boolean;

function RunInThread(F: TThreadFunctionPtr; Args: array of TObject): Boolean;

implementation

type

  { TRunnerThread }

  TRunnerThread = class(TThread)
  private
    Arguments: array of TObject;
    F: TThreadFunctionPtr;
    Result: PBoolean;

  public
    constructor Create(FToRun: TThreadFunctionPtr; Args: array of TObject; Res: PBoolean);

    procedure Execute; override;
  end;

function RunInThread(F: TThreadFunctionPtr; Args: array of TObject): Boolean;
var
  Thread: TRunnerThread;

begin
  Thread := TRunnerThread.Create(F, Args, @Result);
  Thread.FreeOnTerminate := True;
  Thread.Suspended := False;

end;

{ TRunnerThread }

constructor TRunnerThread.Create(FToRun: TThreadFunctionPtr;
  Args: array of TObject; Res: PBoolean);
var
  i: Integer;

begin
  inherited Create(True);

  F := FToRun;
  SetLength(Arguments, Length(Args));
  for i := 0 to High(Args) do
    Arguments[i] := Args[i];

  Result := Res;

end;

procedure TRunnerThread.Execute;
begin
  Result^ := F(Arguments);

end;

end.

