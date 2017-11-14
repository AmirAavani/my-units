program MNIST;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, ParameterManagerUnit, MNISTUnit,
  sysutils;
var
  TrainImages, TestImages: TMNISTImages;
  TrainLStream, TrainIStream: TStream;
  TestLStream, TestIStream: TStream;

begin
  TrainIStream := TFileStream.Create(
    GetRunTimeParameterManager.ValueByName['--ImageTrainFile'], fmOpenRead);
  TrainLStream := TFileStream.Create(
    GetRunTimeParameterManager.ValueByName['--LabelTrainFile'], fmOpenRead);

  TestIStream := TFileStream.Create(
    GetRunTimeParameterManager.ValueByName['--ImageTestFile'], fmOpenRead);
  TestLStream := TFileStream.Create(
    GetRunTimeParameterManager.ValueByName['--LabelTestFile'], fmOpenRead);


  TrainImages := TMNISTImages.Create(TrainLStream, TrainIStream,
    StrToInt(GetRunTimeParameterManager.ValueByName['--TrainImageCount']));
  TestImages := TMNISTImages.Create(TestLStream, TestIStream,
    StrToInt(GetRunTimeParameterManager.ValueByName['--TestImageCount']));

  TrainLStream.Free; TrainIStream.Free;
  TestIStream.Free; TestLStream.Free;

  TrainImages.Free;
  TestImages.Free;

end.

