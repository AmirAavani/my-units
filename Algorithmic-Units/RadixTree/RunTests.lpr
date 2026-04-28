program RunTests;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  RadixTreeTests;

type
  TTestFailureAccess = class(TTestFailure);

procedure RunAllTests;
var
  TestResult: TTestResult;
  i: Integer;
  Failure: TTestFailure;
begin
  WriteLn('RadixTree Unit Tests');
  WriteLn('====================');
  WriteLn;
  
  TestResult := TTestResult.Create;
  try
    // Run all registered tests
    GetTestRegistry.Run(TestResult);
    
    // Display summary
    WriteLn;
    WriteLn('Test Results:');
    WriteLn('-------------');
    WriteLn(Format('Tests run:    %d', [TestResult.RunTests]));
    WriteLn(Format('Passed:       %d', [TestResult.RunTests - TestResult.NumberOfErrors - TestResult.NumberOfFailures]));
    WriteLn(Format('Failures:     %d', [TestResult.NumberOfFailures]));
    WriteLn(Format('Errors:       %d', [TestResult.NumberOfErrors]));
    WriteLn;
    
    // Show failures with details
    if TestResult.NumberOfFailures > 0 then
    begin
      WriteLn('FAILURES:');
      WriteLn('---------');
      for i := 0 to TestResult.Failures.Count - 1 do
      begin
        Failure := TTestFailure(TestResult.Failures[i]);
        WriteLn(Format('[%d] Test: %s', [i + 1, Failure.AsString]));
        WriteLn(Format('    Message: %s', [Failure.ExceptionMessage]));
        WriteLn;
      end;
    end;
    
    // Show errors with details
    if TestResult.NumberOfErrors > 0 then
    begin
      WriteLn('ERRORS:');
      WriteLn('-------');
      for i := 0 to TestResult.Errors.Count - 1 do
      begin
        Failure := TTestFailure(TestResult.Errors[i]);
        WriteLn(Format('[%d] Test: %s', [i + 1, Failure.AsString]));
        WriteLn(Format('    Message: %s', [Failure.ExceptionMessage]));
        WriteLn;
      end;
    end;
    
    // Final message
    if (TestResult.NumberOfErrors = 0) and (TestResult.NumberOfFailures = 0) then
      WriteLn('*** All tests passed! ***')
    else
      WriteLn('*** Some tests failed ***');
      
  finally
    TestResult.Free;
  end;
end;

begin
  RunAllTests;
end.
