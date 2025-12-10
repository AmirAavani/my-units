unit StorageUnitTest;
                             {$push}{$warn 5024 off}//some code that causes hint note or warning{$pop}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestStorage }

  TTestStorage = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestSharding;
  end;

implementation

uses
  StorageUnit;

procedure TTestStorage.TestCreate;
var
  Files: TFiles;

begin
  Files := TFiles.Create('/a/b/c-en@100');
  AssertEquals(Files.Count, 100);
  Files.Free;

  Files := TFiles.Create('/a/b/c-en@128');
  AssertEquals(Files.Count, 128);
  Files.Free;
  Files := TFiles.Create('/a/b/c-en@0');
  AssertEquals(Files.Count, 0);
  Files.Free;

  Files := TFiles.Create('/a/b/c-en@100[1:10]');
  AssertEquals(Files.Count, 10);
  Files.Free;

  Files := TFiles.Create('/a/b/c-en@100[1:120]');
  AssertEquals(Files.Count, 100);
  Files.Free;


end;

procedure TTestStorage.TestSharding;
const
  Pattern: AnsiString = '/ab/c@100';
var
  Files, AllFiles: TFiles;
  Parts: TStringList;

  i: Integer;
  f: AnsiString;

begin
  AllFiles := TFiles.Create(Pattern);
  Parts := TStringList.Create;
  for i := 1 to 7 do
  begin
    Files := TFiles.Create(Pattern);
    AssertEquals(12, Files.Shard(i, 8).Count);

    for f in Files do
      Parts.Add(f);
    Files.Free;

  end;
  Files := TFiles.Create(Pattern);
  AssertEquals(16, Files.Shard(8, 8).Count);
  for f in Files do
    Parts.Add(f);
  Files.Free;

  AssertEquals(AllFiles.Count, Parts.Count);
  for i := 0 to AllFiles.Count - 1 do
    AssertEquals(AllFiles[i], Parts[i]);

  AllFiles.Free;
  Parts.Free;
end;

procedure TTestStorage.SetUp;
begin

end;

procedure TTestStorage.TearDown;
begin

end;

initialization

  RegisterTest(TTestStorage);
end.

