unit MeiPack;

interface

uses
  System.Classes, System.Generics.Collections;

type
  TMeiPack = class
    FPackFile: TFileStream;
    FMap: TDictionary<string,Integer>;

    function findRes(resName: string): Integer;
  public
    constructor Create(fileName: string);
    destructor Destroy; override;
    class procedure packFiles(root: string; fileList: TStrings; packFileName: string);
    procedure loadPackFile(fileName: string);
    function readResFile(resName: string; resStream: TStream): Boolean;
  end;
implementation

uses
  System.SysUtils, MeiStream;

{ TMeiPack }

constructor TMeiPack.Create(fileName: string);
begin
  FMap := TDictionary<string,Integer>.Create();
  loadPackFile(fileName);
end;

destructor TMeiPack.Destroy;
begin
  if Assigned(FPackFile) then
    FPackFile.Free;
  if Assigned(FMap) then
    FMap.Free;
  inherited;
end;

function TMeiPack.findRes(resName: string): Integer;
begin
  if not FMap.ContainsKey(resName) then
    Exit(-1);

  Result := FMap[resName];
end;

procedure TMeiPack.loadPackFile(fileName: string);
var
  i: Integer;
  ms: TMeiStream;
  key: string;
  v, size: Integer;
begin
  if Assigned(FPackFile) then
    FPackFile.Free;
  FPackFile := TFileStream.Create(fileName, fmOpenRead);
  FPackFile.Seek(-4, soFromEnd);
  FPackFile.Read(i, 4);
  if (i <= 0) or (i > FPackFile.Size) then
  begin
    FreeAndNil(FPackFile);
    raise Exception.Create('TMeiPack map size illegal!');
    Exit;
  end;

  FMap.Clear;
  ms := TMeiStream.Create;
  ms.SetSize(i);
  FPackFile.Seek(-i - 4, soFromEnd);
  FPackFile.Read(ms.Memory^, i);
  ms.Seek(0, soFromBeginning);
  while not ms.EOF do
  begin
    key := ms.ReadWideStringWithSize();
    v := ms.ReadInteger;
    FMap.Add(key, v);
  end;
  ms.Free;
end;

class procedure TMeiPack.packFiles(root: string; fileList: TStrings; packFileName: string);
var
  idxMs, ms: TMeiStream;
  packFs, fs: TFileStream;
  size, id: Integer;
  s: string;
  i, p: Integer;
begin

  ms := TMeiStream.Create;
  idxMs := TMeiStream.Create;
  packFs := TFileStream.Create(packFileName, fmCreate);
  for i := 0 to fileList.Count - 1 do
  begin
    s := fileList[i];
    ms.LoadFromFile(root + s);
    p := packFs.Position;
    size := ms.Size;
    packFs.Write(size, 4);
    packFs.Write(ms.Memory^, ms.Size);
    idxMs.WriteWideStringWithSize(s);
    idxMs.WriteInteger(p);
  end;
  packFs.Write(idxMs.Memory^, idxMs.Size);
  i := idxMs.Size;
  packFs.Write(i, 4);

  ms.Free; idxMs.Free;
  packFs.Free;
end;

function TMeiPack.readResFile;
var
  p: Integer;
  m: Pointer;
begin
  Result := False;
  if not Assigned(FPackFile) then
    Exit;
  p := findRes(resName);
  if p < 0 then
    Exit;

  FPackFile.Seek(p, soFromBeginning);
  FPackFile.Read(p, 4);
  m := GetMemory(p);
  FPackFile.Read(m^, p);
  resStream.Write(m^, p);
  resStream.Seek(0, 0);
  FreeMem(m);
  Result := True;
end;

end.
