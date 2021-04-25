program Sample01;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, sysutils, ALoggerUnit, SyncUnit, ElfHashUnit, DataLineUnit,
  BaseMapperUnit, Mapper.OptionUnit
  { you can add units after this };

var
  dp: TDataPoint;

begin
  dp := TDataPoint.Start.
    Map('GenerateRandomNumbers', TBaseMapper.Create,
      TMappingOptions.Create.SetNumShards(16).SetThreadCount(10)).
    Map('SumThemUp', TBaseMapper.Create,
      TMappingOptions.Create.SetNumShards(16).SetThreadCount(10));
  dp.Summary;
  if not dp.Run(False, True) then
    FmtFatalLn('Failed To Run', []);
  FMTDebugLn('Dl.Wait: %s', [BoolToStr(dp.Wait, 'T', 'F')]);
end.

