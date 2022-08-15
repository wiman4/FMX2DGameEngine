unit gameSpriteUnit;
//{$DEFINE sprdebug}
interface

uses
  FMX.Graphics, gameAniUnit, FMX.Types, System.Classes,
  System.Generics.Collections, System.Types, System.Rtti, FMX.MultiResBitmap,
  System.SysUtils, FMX.Utils, System.Math.Vectors, System.UITypes,
  System.Generics.Defaults, TypeUnit, MeiPack;

const
  Orient2Index: array[0..7] of Integer = (0, 4, 1, 5, 2, 6, 3, 7);
  Index2Orient: array[0..7] of Integer = (0, 2, 4, 6, 1, 3, 5, 7);
  Dir2Orient: array[0..7] of Integer = (0, 7, 3, 6, 2, 5, 1, 4);

type
  TGameObjState = (osStand, osWalk, osGuard, osAtk, osDefence, osHit, osMagic, osDie);
  TGameWeaponType = (wtNone, wtBlade, wtSword, wtSpear, wtFan, wtWhip, wtStick, wtBall, wtRing);

const
  sGameObjState: array[TGameObjState] of string = (
    'stand', 'walk', 'guard', 'attack', 'defend', 'hit', 'magic', 'die' );

  DefaultSpriteDuration: array[TGameObjState] of Single = (1, 0.5, 0.8, 0.5, 0.5, 0.5, 0.5, 1);

type
{ TGameSpriteBmp }
  TGameSprFrame = packed record
    rt: TRectF;
    xoff, yoff: Integer;
  end;
  TGameSprBmp = class(TBitmap)
  private
    FFrameList: TList<TGameSprFrame>;
    FFrameCount, FDirCount, FXOff, FYOff: Integer;
  public
    constructor Create(ms: TStream);
    destructor Destroy; override;

    procedure parseFrames(ms: TStream);
    procedure setOneFrame;

    property FrameCount: Integer read FFrameCount;
    property FrameList: TList<TGameSprFrame> read FFrameList;
  end;

  TGameSprBmpPack = array[0..8] of TGameSprBmp;
  PGameSprBmpPack = ^TGameSprBmpPack;
  TGameSpriteBmpList = array[TGameWeaponType] of array[TGameObjState] of TGameSprBmpPack;

{ TGameSpritemation }

  TGameSprite = class(TSpriteAnimation)
  public class var
    SpriteCompare: IComparer<TGameAnimation>;
    GameRes: TMeiPack;
  private//    FCanvas: TCanvas;
    FFrameCount, FCurrentIndex: Integer;
    FSprBmpList: TGameSpriteBmpList;
    FLastAnimationStep: Integer;
    FAnimationRowCount: Integer;
    FAnimationLookup: string;
    FSprState: TGameObjState;
    FSprOrientation: Byte;
    FVisible: Boolean;
    FX, FY, FScale, FOpacity: Single;
    FGameObjId: Integer;
    FGameName: string;
    FGameGuid: Integer;
    FWeaponType: TGameWeaponType;
    FWeaponShape: Byte;

    function GetSprBmpPack(const State: TGameObjState): PGameSprBmpPack;
    procedure SetAnimationRowCount(const Value: Integer);
    procedure SetSprState(const Value: TGameObjState);
    procedure SetSprOrientation(const Value: Byte);
//    procedure SetAnimationRowCount(const Value: Integer);
//    procedure SetAnimationLookup(const Value: string);
    function getBounds: TRectF;
    procedure setOpacity(const Value: Single);
    function getSprOrientation: Byte;
    procedure loadGameWeaponSprBmp;
    procedure setWeaponType(const Value: TGameWeaponType);
  protected
    procedure ProcessAnimation; override;
//    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(id: Integer);
    destructor Destroy; override;

    procedure testDraw(idx: Integer);
    procedure clear;
    procedure refAppearance(var app: TGameAppearance);

//    property Canvas: TCanvas write FCanvas;
    property SprBmpList[const State: TGameObjState]: PGameSprBmpPack read GetSprBmpPack;
    property SprState: TGameObjState read FSprState write SetSprState;
    property X: Single read FX write FX;
    property Y: Single read FY write FY;
    property SprOrientation: Byte read getSprOrientation write SetSprOrientation;
    property Scale: Single read FScale write FScale;
    property Opacity: Single read FOpacity write setOpacity;
    property Bounds: TRectF read getBounds;
    property WeaponType: TGameWeaponType read FWeaponType write setWeaponType;
    property GameObjId: Integer read FGameObjId write FGameObjId;
//    property GameWeaponId: Integer read FGameWeaponId write setGameWeaponId;
    property GameGuid: Integer read FGameGuid write FGameguid;
    property Visible: Boolean read FVisible write FVisible;
  published
    property FrameCount: Integer read FFrameCount write FFrameCount;
    property AnimationRowCount: Integer read FAnimationRowCount write SetAnimationRowCount default 1;
    property AnimationType default TAnimationType.in;
    property AutoReverse default False;
    property Enabled default False;
    property Delay;
    property Duration nodefault;
    property Interpolation default TInterpolationType.Linear;
    property Inverse default False;
    property Loop default True;
    property OnProcess;
    property OnFinish;
    property Trigger;
    property TriggerInverse;
  end;

  function getGameSprite(id, guid: Integer; name: string): TGameSprite;
  function getGameSprBmp(id: Integer; wt: TGameWeaponType; ss: TGameObjState): TGameSprBmpPack;
  function loadSprBmp(fname: string): TGameSprBmp;

implementation

uses
  System.Math;

var
  srcSpriteList: TDictionary<Integer,TGameSprite>;
//  gameSpriteList: TList;
  allSprBmpList: TDictionary<string,TGameSprBmp>;

function myBoundsF(x, y, w, h: Single): TRectF;
begin
  Result.Top := y; Result.Left := x;
  Result.Width := w; Result.Height := h;
end;

function loadSprBmp(fname: string): TGameSprBmp;
var
  fs: TFileStream;
  ms: TMemoryStream;
begin
  if allSprBmpList.TryGetValue(fname, Result) then
    Exit;
  Result := nil;
  {$IFDEF mswindows}
  if FileExists('res\' + fname) then
  begin
    fs := TFileStream.Create('res\' + fname, fmOpenRead);
    Result := TGameSprBmp.Create(fs);
    fs.Free;
    allSprBmpList.Add(fname, Result);
    Exit;
  end;
  {$ENDIF}
  if Assigned(TGameSprite.GameRes) then
  begin
    ms := TMemoryStream.Create;
    TGameSprite.GameRes.readResFile(fname, ms);
    if ms.Size > 0 then
    begin
      Result := TGameSprBmp.Create(ms);
      allSprBmpList.Add(fname, Result);
    end;
    ms.Free;
  end;
  if (Result = nil) and (fname.IndexOf('char\') < 0) then
//    FileExists('interface\蓝色风格\杂图\不可用.png') then
    Result := loadSprBmp('interface\蓝色风格\杂图\不可用.png');
end;

function getGameSprite(id, guid: Integer; name: string): TGameSprite;
var
  ss: TStrings;
  ms: TMemoryStream;
  s: string;
  sprBmp: TGameSprBmp;
  objState: TGameObjState;
  i: Integer;
  spr: TGameSprite;
  label lb001;
begin
  if not srcSpriteList.TryGetValue(id, spr) then
  begin
    spr := TGameSprite.Create(id);
    srcSpriteList.Add(id, spr);

  end;
lb001:
  Result := TGameSprite.Create(id);
//  gameSpriteList.Add(Result);
  Result.FGameName := name;
  Result.FGameGuid := guid;
  Result.FSprBmpList := spr.FSprBmpList;
  Result.SprOrientation := 0;
  Result.SprState := osStand;
  Result.Start;
end;

function getGameSprBmp(id: Integer; wt: TGameWeaponType; ss: TGameObjState): TGameSprBmpPack;
var
  spr: TGameSprite;
  bmpPack: PGameSprBmpPack;
  i: Integer;
begin
  if not srcSpriteList.TryGetValue(id, spr) then
  begin
    spr := TGameSprite.Create(id);
    srcSpriteList.Add(id, spr);
  end;
  bmpPack := @spr.FSprBmpList[wt, ss];
  if bmpPack[0] <> nil then
    Exit(bmpPack^);

  if id >= 1000 then
  begin
    bmpPack[0] := loadSprBmp(Format('char\%d\%s.png', [id, sGameObjState[ss]]));
    if not Assigned(bmpPack[0]) and (ss = osStand) then
      bmpPack[0] :=  loadSprBmp('char\1005\stand.png');
    Exit(bmpPack^);
  end;

  bmpPack[0] := loadSprBmp(Format('char\hero\%.4d\%.2d\%s.png',
    [id, Ord(wt), sGameObjState[ss]]));
  for i := 5 to 7 do
    bmpPack[i] := loadSprBmp(Format('char\hero\%.4d\%.2d\%.3d\%s.png',
      [id, Ord(wt), i, sGameObjState[ss]]));
  Result := bmpPack^;
end;



{ TGameSprite }

procedure TGameSprite.clear;
begin
  FillChar(FSprBmpList, SizeOf(FSprBmpList), 0);
  FWeaponType := wtNone;
  FrameCount := 0;
  FGameGuid := 0;
  FGameObjId := 0;
  FGameName := '';
end;

constructor TGameSprite.Create(id: Integer);
begin
  inherited Create(nil);
  FVisible := True;
  FGameObjId := id;
  FOpacity := 1;
  FScale := 1;
  Loop := True;
  Duration := DefaultSpriteDuration[osStand];
  AutoRemove := False;
  if id < 1000 then
    case FGameObjId of
      111,131,132: FWeaponType := wtBlade;
      121,112: FWeaponType := wtSword;
      122: FWeaponType := wtSpear;
      212,231,232: FWeaponType := wtWhip;
      211,221: FWeaponType := wtStick;
      222: FWeaponType := wtBall;
    end;
end;

destructor TGameSprite.Destroy;
begin
  inherited;
end;

function TGameSprite.GetSprBmpPack(const State: TGameObjState): PGameSprBmpPack;
begin
  Result := @FSprBmpList[FWeaponType, State];
end;

function TGameSprite.getSprOrientation: Byte;
begin
  Result := Index2Orient[FSprOrientation];
end;

procedure TGameSprite.loadGameWeaponSprBmp;
var
  bmpPack: PGameSprBmpPack;
  i: Integer;
begin
  bmpPack := @FSprBmpList[FWeaponType, FSprState];
  if FWeaponType = wtNone then
  begin
    bmpPack[1] := nil;
    Exit;
  end;
  if bmpPack[1] <> nil then
    Exit;
//  if FGameWeaponId > 1000000 then
//    i := FGameWeaponId div 100
//  else
//    i := FGameWeaponId;
//  i := (i - 11500) mod 30;
//  case i of
//    2..4: i := 2;
//    5..8: i := 3;
//    9..13: i := i - 5;
//  end;
  bmpPack[1] := loadSprBmp(Format('char\hero\weapon\%.4d\%.2d\%.3d\%s.png',
    [FGameObjId, Ord(FWeaponType), FWeaponShape, sGameObjState[FSprState]]));

end;

function TGameSprite.getBounds: TRectF;
var
  CurrentIndex: Integer;
  aniBmps: PGameSprBmpPack;
  aniBmp: TGameSprBmp;
  rt, rt2: TRectF;
  frame: TGameSprFrame;
begin
  Result := myBoundsF(FX, FY, 0, 0);
  begin
    if FFrameCount = 0 then
      Exit;
    aniBmps  := @FSprBmpList[FWeaponType, FSprState];
    CurrentIndex := FCurrentIndex;
    for aniBmp in aniBmps^ do
    begin
      if not Assigned(aniBmp) then
        Continue;

      frame := aniBmp.FFrameList[CurrentIndex];
      rt := frame.rt;
      rt2 := TRectF.Create(TPointF.Create(FX - frame.xoff * FScale, FY - frame.yoff * FScale), rt.Width * FScale, rt.Height * FScale);
      Result := Result + rt2;
    end;
  end;
end;

procedure TGameSprite.ProcessAnimation;
var
  CurrentIndex: Integer;
  NowValue: Single;
  aniBmps: PGameSprBmpPack;
  aniBmp: TGameSprBmp;
  rt, rt2: TRectF;
  frame: TGameSprFrame;
  i: Integer;
begin
  if Assigned(AniCanvas) and FVisible then
  begin
    aniBmps  := @FSprBmpList[FWeaponType, FSprState];
    if FFrameCount = 0 then
      Exit;

    NowValue := InterpolateSingle(0, FFrameCount, NormalizedTime);
    for i := High(aniBmps^) downto 0 do
    begin
      aniBmp := aniBmps[i];
      if not Assigned(aniBmp) then
        Continue;

      CurrentIndex := Trunc(NowValue);
      if CurrentIndex > FFrameCount - 1 then
        CurrentIndex := FFrameCount - 1;
      Inc(CurrentIndex, FSprOrientation * FFrameCount);
      if CurrentIndex > aniBmp.FFrameList.Count - 1 then
        CurrentIndex := aniBmp.FFrameList.Count - 1;
      FCurrentIndex := CurrentIndex;
      frame := aniBmp.FFrameList[CurrentIndex];
      rt := frame.rt;
      rt2 := myBoundsF(FX - frame.xoff * FScale, FY - frame.yoff * FScale, rt.Width * FScale, rt.Height * FScale);
      AniCanvas.DrawBitmap(aniBmp, rt, rt2, FOpacity);
    end;

    {$IFDEF sprdebug}
    AniCanvas.Stroke.Color := TAlphaColorRec.Fuchsia;
    AniCanvas.DrawRect(getBounds, 1);
    {$ENDIF}
  end;
end;

procedure TGameSprite.SetAnimationRowCount(const Value: Integer);
begin
  FAnimationRowCount := Value;
end;

procedure TGameSprite.refAppearance;
var
  wt: TGameWeaponType;
  ss: TGameObjState;
begin
  if app._weapon2[0] <> 0 then
  begin
    wt := TGameWeaponType(app._weapon2[0]);
    FWeaponShape := app._weapon2[1];
  end
  else begin
    wt := TGameWeaponType(app._weapon1[0]);
    FWeaponShape := app._weapon1[1];
  end;
  setWeaponType(wt);

  for ss := Low(TGameObjState) to High(TGameObjState) do
    FSprBmpList[FWeaponType, ss][1] := nil;
  loadGameWeaponSprBmp;
end;

procedure TGameSprite.setOpacity(const Value: Single);
begin
  FOpacity := Value;
  if FOpacity < 0.01 then
    FVisible := False
  else
    FVisible := True;
end;

procedure TGameSprite.SetSprOrientation(const Value: Byte);
var
  bmp: TGameSprBmp;
begin
  FSprOrientation := Value;
  if FSprOrientation > High(Orient2Index) then
    FSprOrientation := 0;

  FSprOrientation := Orient2Index[FSprOrientation];
  bmp := FSprBmpList[FWeaponType, FSprState][0];
  if not Assigned(bmp) or (FSprOrientation > bmp.FDirCount - 1) then
    FSprOrientation := 0;
end;

procedure TGameSprite.SetSprState(const Value: TGameObjState);
var
  aniBmp: TGameSprBmp;
begin
  FSprState := Value;
  FFrameCount := 0;

  aniBmp := FSprBmpList[FWeaponType, Value][0];
//  {$IFDEF gameclient}
  if not Assigned(aniBmp) then
  begin
    FSprBmpList[FWeaponType, Value] := getGameSprBmp(FGameObjId, FWeaponType, FSprState);
    aniBmp := FSprBmpList[FWeaponType, Value][0];
  end;
//  {$ENDIF}
  if Assigned(aniBmp) then
    FFrameCount := aniBmp.FrameCount
  else if Value <> osStand then
    SetSprState(osStand)
  else
    FFrameCount := 0;
  loadGameWeaponSprBmp;
  Duration := DefaultSpriteDuration[Value];
end;

procedure TGameSprite.setWeaponType(const Value: TGameWeaponType);
begin
  FWeaponType := Value;
  if Value = wtNone then
    case FGameObjId of
      111,131,132: FWeaponType := wtBlade;
      121,112: FWeaponType := wtSword;
      122: FWeaponType := wtSpear;
      212,231,232: FWeaponType := wtWhip;
      211,221: FWeaponType := wtStick;
      222: FWeaponType := wtBall;
    end;
  SetSprState(FSprState);
end;

procedure TGameSprite.testDraw(idx: Integer);
var
  NowValue: Single;
  CurrentIndex: Integer;
begin
  if not Assigned(TGameAnimation.AniCanvas) then
    Exit;
  CurrentTime := CurrentTime + 1 / FFrameCount;
  if CurrentTime >= Duration then
    CurrentTime := 0;
  NowValue := InterpolateSingle(0, FFrameCount, NormalizedTime);
  CurrentIndex := Trunc(NowValue);
  if CurrentIndex > FFrameCount - 1 then
    CurrentIndex := FFrameCount - 1;
  Inc(CurrentIndex, FSprOrientation * FFrameCount);
  with TGameAnimation.AniCanvas do
  begin
    BeginScene();
    ClearRect(TRectF.Create(0,0,80,40), 0);
    FillText(TRectF.Create(0,0,80,40), CurrentIndex.ToString, False, 1, [], TTextAlign.Center);
    ClearRect(Bounds, 0);
    ProcessAnimation;
    EndScene;
  end;
end;

{ TGameSpriteBmp }

constructor TGameSprBmp.Create(ms: tstream);
begin
  inherited Create;
  LoadFromStream(ms);
  FFrameList := TList<TGameSprFrame>.Create;
  parseFrames(ms);
end;

destructor TGameSprBmp.Destroy;
begin
  FFrameList.Free;
  inherited;
end;

procedure TGameSprBmp.parseFrames;
var
  fs: TFileStream;
  i, cnt: Cardinal;
  frame: TGameSprFrame;
  rt: TRect;
begin
  ms.Seek(-20, 2);
  ms.Read(i, 4);
  if i > 10000 then
  begin
    setOneFrame;
    Exit;
  end;
  ms.Seek(-20 - i, 2);
  ms.Read(i, 4);  //dirCnt
  FDirCount := i;
  ms.Read(i, 4);  //frameCnt
  FFrameCount := i;
  ms.Read(cnt, 4);  //allFrameCnt
  for i := 1 to cnt do
  begin
    ms.Read(rt, SizeOf(rt));
    frame.rt := rt;
    ms.Read(frame.xoff, 4);
    ms.Read(frame.yoff, 4);
    FFrameList.Add(frame);
  end;

end;

procedure TGameSprBmp.setOneFrame;
var
  frame: TGameSprFrame;
begin
  FDirCount := 1;
  FFrameCount := 1;
  frame.rt := RectF(0, 0, Width, Height);
  frame.xoff := 0;
  frame.yoff := 0;
  FFrameList.Clear;
  FFrameList.Add(frame);
end;

procedure finalSprites;
var
  sp: TPair<Integer,TGameSprite>;
  bmp: TGameSprBmp;
//  spr: TObject;
  sb: TPair<string,TGameSprBmp>;
begin
  for sp in srcSpriteList do
    sp.Value.Free;

  srcSpriteList.Free;

//  for spr in gameSpriteList do
//  begin
//    spr.Free;
//  end;
//  gameSpriteList.Free;

  for sb in allSprBmpList do
    sb.Value.Free;
  allSprBmpList.Free;
end;

function compSpr(const spr1, spr2: TGameAnimation): Integer;
begin
  if TGameSprite(spr1).Y = TGameSprite(spr2).Y then
    Result := 0
  else if TGameSprite(spr1).Y > TGameSprite(spr2).Y then
    Result := -1
  else
    Result := 1;
end;

initialization
  srcSpriteList := TDictionary<Integer,TGameSprite>.Create;
//  gameSpriteList := TList.Create;
  allSprBmpList := TDictionary<string,TGameSprBmp>.Create;
  TGameSprite.SpriteCompare := TComparer<TGameAnimation>.Construct(compSpr);
finalization
  finalSprites;
  TGameSprite.SpriteCompare := nil;

end.

