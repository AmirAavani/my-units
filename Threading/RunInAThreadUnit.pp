unit RunInAThreadUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TObjectList = specialize TList<TObject>;
  TThreadFunctionPtr = function (Args: TObjectList): Boolean;

procedure RunInThread(F: TThreadFunctionPtr; Args: TObjectList;
  OutputResult: PBoolean);

implementation
uses
  ALoggerUnit;

type
  { TRunnerThread }

  TRunnerThread = class(TThread)
  private
    Arguments: TObjectList;
    F: TThreadFunctionPtr;
    Result: PBoolean;

  public
    // Caller is responsibe for freeing the memory for Args.
    constructor Create(FToRun: TThreadFunctionPtr; Args: TObjectList;
       Res: PBoolean);
    destructor Destroy; override;

    procedure Execute; override;
  end;

procedure RunInThread(F: TThreadFunctionPtr; Args: TObjectList;
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
  Args: TObjectList; Res: PBoolean);
begin
  inherited Create(True);

  F := FToRun;
  Arguments := Args;
  Result := Res;

end;

destructor TRunnerThread.Destroy;
begin

  inherited Destroy;
end;

procedure TRunnerThread.Execute;
var
  _Result: Boolean;

begin
  _Result := F(Arguments);

  if Self.Result <> nil then
    Self.Result^ := _Result;

end;

end.

