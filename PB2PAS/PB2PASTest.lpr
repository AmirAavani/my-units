program PB2PASTest;

{$mode objfpc}{$H+}
{$ASSERTIONS on}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, heaptrc, fgl, SampleUnit, ProtoHelperUnit
  { you can add units after this };

{
function EmptyProto: Boolean;
var
  Document: TDocument;
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create('Sample.bin', fmCreate);

  Document := TDocument.Create;
  // Document.SaveToStream(FileStream);
  FileStream.Free;
  Document.Free;

  Document := TDocument.Create;
  FileStream := TFileStream.Create('Sample.bin', fmOpenRead);
  // Document.LoadFromStream(FileStream);
  Assert(Document.ToString = '');
  Document.Free;
  FileStream.Free;

  Result := True;
end;


function DocWithTitle: Boolean;
var
  Document: TDocument;
  FileStream: TFileStream;

begin
  { Empty Title}
  Document := TDocument.Create;
  Assert(Document.Title.ConstAllTokens.Count = 0);
  Document.Title := TTitle.Create;
  Assert(Document.Title.ConstAllTokens.Count = 0);
  Assert(Document.ToString = '');

  Assert(Document.Title.ConstAllTokens = nil);

  Document.Title.AllTokens.Add(TToken.Create(1, 'a', nil, 1));
  Document.Free;

  Result := True;

end;


begin
  EmptyProto;
  DocwithTitle;

end.
}

var
  IDList: TTestID;

begin
  IDList := TTestID.Create;

  IDList.Uid64 := 1;
  IDList.Uid32 := 2;
  IDList.id64 := 3;
  IDList.id32 := 4;
  IDList.IdStr := 'amir';
  // IDList.Idd := 5.0;

{  IDList.AllId64s.Add(1);
  IDList.AllId64s.Add(2);
  IDList.AllId64s.Add(127);
  IDList.AllId64s.Add(128);
  IDList.AllId64s.Add(130);
  Assert(IDList.ConstAllId64s.Count = 5);
  Assert(IDList.ConstAllId32s.Count = 0);
  Assert(IDList.ConstAllId32s = nil);
  //;
 }
  WriteLn(IDList.ToString);
  IDList.SaveToStream(TFileStream.Create('/tmp/t1.out', fmCreate));
  IDList.Free;
end.
