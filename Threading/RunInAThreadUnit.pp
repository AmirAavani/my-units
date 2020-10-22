unit RunInAThreadUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TThreadFunctionPtr = function (Args: array of Pointer): Boolean;
  TArrayOfPointer = array of Pointer;

procedure RunInThread(F: TThreadFunctionPtr; Args: TArrayOfPointer;
  OutputResult: PBoolean);

implementation

type
  { TRunnerThread }

  TRunnerThread = class(TThread)
  private
    Arguments: array of Pointer;
    F: TThreadFunctionPtr;
    Result: PBoolean;

  public
    constructor Create(FToRun: TThreadFunctionPtr; Args: TArrayOfPointer; Res: PBoolean);
    destructor Destroy; override;

    procedure Execute; override;
  end;

procedure RunInThread(F: TThreadFunctionPtr; Args: TArrayOfPointer;
  OutputResult: PBoolean);
var
  Thread: TThread;

begin
  Thread := TRunnerThread.Create(F, Args, OutputResult);
  Thread.FreeOnTerminate := True;
  Thread.Suspended := False;

end;

{ TRunnerThread }

constructor TRunnerThread.Create(FToRun: TThreadFunctionPtr;
  Args: TArrayOfPointer; Res: PBoolean);
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

destructor TRunnerThread.Destroy;
begin
  SetLength(Arguments, 0);

  inherited Destroy;
end;

procedure TRunnerThread.Execute;
begin
  Result^ := F(Arguments);

end;

end.

