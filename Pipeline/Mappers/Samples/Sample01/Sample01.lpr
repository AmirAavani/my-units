program Sample01;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, sysutils, ALoggerUnit, SyncUnit, ElfHashUnit, DataLineUnit,
Mapper.BaseMapperUnit, Mapper.OptionUnit, Mapper.ChannelUnit, MapperFunctionUnit,
  Mapper.LibraryUnit, ParameterManagerUnit
  { you can add units after this };

var
  dp: TDataPoint;

begin
  dp := TDataPoint.Start.
    Map('Reader',
    TNewLineReaderMapper.Create(
      TFileStream.Create(GetRunTimeParameterManager.ValueByName['--FileName'].AsAnsiString,
        fmOpenRead)),
      TMappingOptions.Create.SetNumShards(16).SetThreadCount(10)).
    Map('SumThemUp',
     TBaseMapper.MapperFromFunction(@SumThemUpMapper),
      TMappingOptions.Create.SetNumShards(16).SetThreadCount(10));

  dp.Summary;

  FmtFatalLnIFFalse(dp.Run(True, True), 'Failed To Run', []);
  FMTDebugLn('Dl.Wait: %s', [BoolToStr(dp.Wait, 'True', 'False')]);
end.

