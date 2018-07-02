unit GZipCompressionUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, zstream;

type

  { TGZStream }

  { TGUnzipStream }

  TGUnzipStream = class(TObject)
  protected
    FStream: TStream;
  public
    // Will Own the Stream Object.
    constructor Create(Stream: TStream);
    function Read(var buffer; Count:longint): LongInt;
    function Seek(offset:longint;origin:word): LongInt;
    destructor Destroy; override;

  end;

implementation
uses
  zbase;

{ TGUnzipStream }

constructor TGUnzipStream.Create(Stream: TStream);
begin
  inherited Create;

  FStream := Stream;

end;


const
  Z_EOF = -1;         { same value as in STDIO.H }
  Z_BUFSIZE = 16384;
  { Z_PRINTF_BUFSIZE = 4096; }


  gz_magic : array[0..1] of byte = ($1F, $8B); { gzip magic header }

  { gzip flag byte }

  ASCII_FLAG  = $01; { bit 0 set: file probably ascii text }
  HEAD_CRC    = $02; { bit 1 set: header CRC present }
  EXTRA_FIELD = $04; { bit 2 set: extra field present }
  ORIG_NAME   = $08; { bit 3 set: original file name present }
  COMMENT     = $10; { bit 4 set: file comment present }
  RESERVED    = $E0; { bits 5..7: reserved }

type gz_stream = record
  stream      : z_stream;
  z_err       : integer;      { error code for last stream operation }
  z_eof       : boolean;  { set if end of input file }
  gzfile      : file;     { .gz file }
  inbuf       : Pbyte;   { input buffer }
  outbuf      : Pbyte;   { output buffer }
  crc         : cardinal;    { crc32 of uncompressed data }
  msg,                    { error message - limit 79 chars }
  path        : string[79];   { path name for debugging only - limit 79 chars }
  transparent : boolean;  { true if input file is not a .gz file }
  mode        : char;     { 'w' or 'r' }
  startpos    : longint;     { start of compressed data in file (header skipped) }
end;

type gz_streamp = ^gz_stream;

function destroy (var s:gz_streamp) : integer; forward;
procedure check_header(s:gz_streamp); forward;

{Copied from gzio.pp gzread}
function TGUnzipStream.Read(var buffer; Count: longint): LongInt;
{ GZREAD ====================================================================

  Reads the given number of uncompressed bytes from the compressed file.
  If the input file was not in gzip format, gzread copies the given number
  of bytes into the buffer.

  gzread returns the number of uncompressed bytes actually read
  (0 for end of file, -1 for error).

============================================================================}
var
  s         : gz_streamp;
  start     : Pbyte;
  n         : cardinal;
  crclen    : cardinal;  { Buffer length to update CRC32 }
  filecrc   : cardinal; { CRC32 stored in GZIP'ed file }
  filelen   : cardinal; { Total lenght of uncompressed file }
  bytes     : integer;  { bytes actually read in I/O blockread }
  total_in  : Qword;
  total_out : Qword;
{$ifndef pointer_arith}
  next_out  : Pbyte;
{$endif}

begin

  s := gz_streamp(f);
  start := Pbyte(buf); { starting point for crc computation }

  if (s = nil) or (s^.mode <> 'r') then begin
    gzread := Z_STREAM_ERROR;
    exit;
  end;

  if (s^.z_err = Z_DATA_ERROR) or (s^.z_err = Z_ERRNO) then begin
    gzread := -1;
    exit;
  end;

  if (s^.z_err = Z_STREAM_END) then begin
    gzread := 0;  { EOF }
    exit;
  end;

  s^.stream.next_out := Pbyte(buf);
  s^.stream.avail_out := len;

  while (s^.stream.avail_out <> 0) do begin

    if (s^.transparent = true) then begin
      { Copy first the lookahead bytes: }
      n := s^.stream.avail_in;
      if (n > s^.stream.avail_out) then n := s^.stream.avail_out;
      if (n > 0) then begin
        move(s^.stream.next_in^,s^.stream.next_out^,n);
        inc (s^.stream.next_out, n);
        inc (s^.stream.next_in, n);
        dec (s^.stream.avail_out, n);
        dec (s^.stream.avail_in, n);
      end;
      if (s^.stream.avail_out > 0) then begin
        blockread (s^.gzfile, s^.stream.next_out^, s^.stream.avail_out, bytes);
        dec (s^.stream.avail_out, cardinal(bytes));
      end;
      dec (len, s^.stream.avail_out);
      inc (s^.stream.total_in, cardinal(len));
      inc (s^.stream.total_out, cardinal(len));
      gzread := integer(len);
      exit;
    end; { IF transparent }

    if (s^.stream.avail_in = 0) and (s^.z_eof = false) then begin
      {$push}{$I-}
      blockread (s^.gzfile, s^.inbuf^, Z_BUFSIZE, s^.stream.avail_in);
      {$pop}
      if (s^.stream.avail_in = 0) then begin
        s^.z_eof := true;
      if (IOResult <> 0) then begin
  	s^.z_err := Z_ERRNO;
  	break;
        end;
      end;
      s^.stream.next_in := s^.inbuf;
    end;

    s^.z_err := inflate(s^.stream, Z_NO_FLUSH);

    if (s^.z_err = Z_STREAM_END) then begin
    {$ifdef pointer_arith}
      crclen := 0;
      crclen:=s^.stream.next_out-start;
    {$else}
      next_out := s^.stream.next_out;
      while (next_out <> start ) do begin
        dec (next_out);
        inc (crclen);   { Hack because Pascal cannot substract pointers }
      end;
    {$endif}
      { Check CRC and original size }
      s^.crc := crc32(s^.crc, start, crclen);
      start := s^.stream.next_out;

      filecrc := getLong (s);
      filelen := getLong (s);

      if (s^.crc <> filecrc) or (s^.stream.total_out <> filelen)
        then s^.z_err := Z_DATA_ERROR
      else begin
  	{ Check for concatenated .gz files: }
  	check_header(s);
  	if (s^.z_err = Z_OK) then begin
            total_in := s^.stream.total_in;
            total_out := s^.stream.total_out;

  	  inflateReset (s^.stream);
  	  s^.stream.total_in := total_in;
  	  s^.stream.total_out := total_out;
  	  s^.crc := crc32 (0, nil, 0);
  	end;
      end; {IF-THEN-ELSE}
    end;

    if (s^.z_err <> Z_OK) or (s^.z_eof = true) then break;

  end; {WHILE}

{$ifdef pointer_arith}
  crclen:=s^.stream.next_out-start;
{$else}
  crclen := 0;
  next_out := s^.stream.next_out;
  while (next_out <> start ) do begin
    dec (next_out);
    inc (crclen);   { Hack because Pascal cannot substract pointers }
  end;
{$endif}
  s^.crc := crc32 (s^.crc, start, crclen);
  gzread := integer(len - s^.stream.avail_out);

end;

end;

function TGUnzipStream.Seek(offset: longint; origin: word): LongInt;
begin

end;

destructor TGUnzipStream.Destroy;
begin
  FStream.Free;

  inherited Destroy;
end;

end.

