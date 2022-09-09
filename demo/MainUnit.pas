unit MainUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, gameAniUnit,
  FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.Layouts,
  FMX.ListBox, System.Generics.Collections, FMX.Gestures;

type
  TForm1 = class(TForm)
    btnTest: TButton;
    btnDrawAniBmp: TButton;
    btnStressTest: TButton;
    btnTest2: TButton;
    edtFileName: TEdit;
    lblFileName: TLabel;
    btnPlayStop: TButton;
    edtFrameCount: TEdit;
    lblFrameCount: TLabel;
    cbbState: TComboBox;
    cbbCharState: TComboBox;
    edtUI: TEdit;
    tmrMain: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure btnDrawAniBmpClick(Sender: TObject);
    procedure btnStressTestClick(Sender: TObject);
    procedure btnTest2Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure edtFileNameKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure btnPlayStopClick(Sender: TObject);
    procedure cbbStateChange(Sender: TObject);
    procedure cbbCharStateChange(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure gameBtnTestClick(sender: TObject);
    procedure imgMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
    procedure tmrMainTimer(Sender: TObject);
    procedure FormResize(Sender: TObject);

  private    { Private declarations }
    FOffSet: TPointF;
    procedure paintUI(Sender: TObject);
  protected
//    procedure PaintRects(const UpdateRects: array of TRectF); override;
  public    { Public declarations }
    FDrawScale: Single;

    procedure OnGridClick(sender: TObject; gridIndex: Integer);
    procedure tvMarketClickItem(sender: TObject);
    procedure showMsg(s: string);

//    property Controls: TControlList read FControls;
  end;

var
  Form1: TForm1;

const
  // Player
  PLAYER_HEALTH = 3;
  PLAYER_LIVES = 3;
  // Stages
  MAIN_MENU = 0;
  INSTRUCTIONS = 1;
  GAMEPLAY = 2;
  LEVEL_COMPLETE = 3;
  GAME_OVER = 4;
  HIGH_SCORES = 5;
  SETTINGS = 6;
  GAMEPAD = 7;
  // Sounds
  FIRE_SFX = 0;
  EXPLOSION_SFX = 1;
  FAIL_SFX = 2;
  WIN_SFX = 3;
  ALIEN_SFX = 4;
  COLLECT_SFX = 5;


var
  CurrentStage: Integer;


implementation

uses
  gameSpriteUnit, System.Math, System.Math.Vectors,
  UIunit, RichTextUnit, stringUnit, TypeUnit, MeiPack, pubUnit;

{$R *.fmx}
const
  MaxBattleMember = 24;
  MaxSideMember = MaxBattleMember div 2;

  BtlPosToIdx: array[0..1] of array[1..MaxSideMember] of Integer = (
    (14, 13, 15, 12, 16, 20, 19, 21, 18, 22, 23, 17),
    (2, 3, 1, 4, 0, 8, 9, 7, 10, 6, 5, 11)
  );
  dmgHeight = 30;
  dmgFontSize = 26;
  imgSize = 110;
  hpRctWidth = 50;
  hpRctHeight = 10;
  marginSize = 10;
  buffFontSize = 16;
  nameFontSize = 18;
  nameWidth = 160;
  objHeight = imgSize + marginSize + nameFontSize;
  BtlSlotWidth = 80;
  BtlSlotHeight = 40;
  BtlPetXOff = -121;
  BtlPetYOff = -60;
  BtlPetXOff1 = 111;
  BtlPetYOff1 = 55;
  BtlSideXOff = -388;
  BtlSideYOff = -238;
  BtlAtkXOff = BtlPetXOff div 2;
  BtlAtkYOff = BtlPetYOff div 2;
  BtlAtkXOff1 = BtlPetXOff1 div 2;
  BtlAtkYOff1 = BtlPetYOff1 div 2;
  BtlAtkMoveTime = 0.5;

  sBattleStart = '进入战斗';
  sAttack = '普攻';
  sReady = '就绪';
  sDodge = '闪避';
  sParry = '招架';
  sCrit = '！';
  sDead = '死亡';
  sOut = '场外';

type
  TOpenControl = class(TControl);

  TMyEdit = class(TEdit)
  protected
    procedure DoPaint; override;
  end;

  TBattleUIMsgId = (bumNone, bumAction, bumHp, bumMp, bumBuff, bumDebuff, bumWors,
    bumDie, bumOut);
  TBattleUIMsg = packed record
    _id: TBattleUIMsgId;
    _srcIdx, _dstIdx: Byte;
    _value: Integer;
    _text: string;
  end;
  PBattleUIMsg = ^TBattleUIMsg;
  TGameBattleSprite = class(TGameSprite)
  private
    FBtlSide, FBtlIdx: Integer;
    function getAniState: Integer;
    procedure SetAniState(const Value: Integer);
  public
    lblWords: TUILabel;
    lblDmgs: array[0..7] of TUILabel;
    rctHpBar: TUIRectangle;
    lblBuff, lblDebuff: TUILabel;
    lblName: TUILabel;
    FPos0: TPoint;

    procedure show;
    procedure posChange(dx, dy: Single; delay, duration: Single; atype: TAnimationType = TAnimationType.In;
      ainterpolation: TInterpolationType = TInterpolationType.Quadratic);
    procedure moveTo(x, y: Single; delay: Single; duration: Single = BtlAtkMoveTime;
      atype: TAnimationType = TAnimationType.In;
      ainterpolation: TInterpolationType = TInterpolationType.Quadratic);
    procedure stateChange(state: TGameObjState; delay: Single);
    procedure orienteTo(ort: Byte; delay: Single);

    property BtlSide: Integer read FBtlSide write FBtlSide;
    property BtlIdx: Integer read FBtlIdx write FBtlIdx;
    property AniState: Integer read getAniState write SetAniState;
  end;

  TBattleObjUI = class
  public
    FParent: TUIElement;
    FObjSprites: array[0..MaxBattleMember - 1] of TGameBattleSprite;

    FDmgY0, FDmgY1: Integer;
    FMsgCnt: Integer;
    FMsgListData: array[0..511] of TBattleUIMsg;
    FMsgList: array[0..511] of PBattleUIMsg;
    FActStartTime, FActOverTime, FActDuration: Single;
    FActFinishedTime: Int64;

    constructor Create(AOwner: TUIElement);
    destructor Destroy; override;


    procedure show(idx: Integer; b: Boolean);
    procedure addMsg(id: TBattleUIMsgId; srcIdx, dstIdx: Byte; value: Integer; text: string = '');
    procedure delMsg(idx, cnt: Integer);
    procedure doMsg;
    procedure clearMsg;
  end;


var
  gameSpr: TGameSprite;
  gameRes: TMeiPack;
  sprBmp: TGameSprBmp;
  AllsprBmpList: TDictionary<Integer, TGameSprBmp>;
  AllAniList: TList<TGameSprite>;
  sprPos: TPoint;
  fileList: TStrings;
  testEdit: TMyEdit;
  battleUI: TBattleObjUI;
  lblTest: TUILabel;
  rvMsg: TUIRichView;

procedure TForm1.btnDrawAniBmpClick(Sender: TObject);
var
  bmp: TGameSprBmp;
begin

  gameSpr.testDraw(Tag);
  Tag := Tag + 1;
  if Tag > 7 then
    Tag := 0;
end;

procedure TForm1.btnPlayStopClick(Sender: TObject);
begin
  gameSpr.AniThread.Enabled := not gameSpr.AniThread.Enabled;
  if not gameSpr.AniThread.Enabled then
    PaintTo(Canvas);
end;

procedure TForm1.btnStressTestClick(Sender: TObject);
var
  ani: TGameSprite;
  i: Integer;
  rt: TRectangle;
begin
  for i := 1 to 100 do
  begin
    ani := TGameSprite.Create(1005);
    ani.X := sprPos.X;
    ani.Y := sprPos.Y;
//    ani.Parent := rt;
//    ani.SprBmpList[osWalk] := SprBmp;
    ani.SprState := osWalk;
    ani.Name := 'testAni' + AllAniList.Count.ToString;
    ani.SprOrientation := Random(8);
    ani.start;
    AllAniList.Add(ani);
    sprPos.X := sprPos.X + 20;
    if sprPos.X > Self.Width - 20 then
    begin
      sprPos.X := 0;
      sprPos.Y := sprPos.Y + 30;
    end;
  end;

end;

procedure TForm1.btnTest2Click(Sender: TObject);
var
  i: Integer;
  rt: TRectF;
begin
  Canvas.BeginScene;
  try
    Canvas.Clear(TAlphaColors.Gray);
    Canvas.DrawBitmap(SprBmp, SprBmp.Bounds, SprBmp.Bounds, 1);
    i := 0;
    for i := 0 to SprBmp.FrameList.Count - 1 do
    begin
      rt := SprBmp.FrameList[i].rt;
//      Canvas.Stroke.Color := TAlphaColorRec.White;
      Canvas.Fill.Color :=TAlphaColorRec.White;
      Canvas.FillText(TRectF.Create(rt.TopLeft, 18, 18), i.ToString, False, 1, [], TTextAlign.Leading);
      Canvas.DrawRect(rt, 0, 0, [], 1);
    end;

  finally
    Canvas.EndScene;
  end;
//  TGameAnimator.AnimateIntDelay(lblTest, 'Y', 800, 1, 0, TAnimationType.In, TInterpolationType.Quadratic);
//  TGameAnimator.AnimateIntDelay(lblTest, 'Y', 200, 1, 1, TAnimationType.out, TInterpolationType.Quadratic);
end;

procedure TForm1.btnTestClick(Sender: TObject);
begin
  showMsg(TUIElement(Sender).Name + ' click');
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  ms: TMemoryStream;
  fid: Integer;
  spath0, spath: string;
  aniCnt, id, osIdx: Integer;
  obs: TGameObjState;
  s: string;
  i: Integer;
  btn: TUIButton;
  pnl: TUIPanel;
  rv: TUIRichView;
  lbl: TUILabel;
  rct: TUIRectangle;
  ui: TUIElement;
  grid: TUIGrid;
  edt: TUIEdit;
  ss: TStringList;
  tv: TUITreeView;
  node: TUITreeViewNode;
  lst: TUIListbox;
  lv: TUIListView;
  li: TUIListItem;
  fs: TFileStream;
  app: TGameAppearance;
const
  dirNames: array[0..3] of string = ('\', '\005\', '\006\', '\007\');

begin
  initAppPath;
//  fs := TFileStream.Create('D:\projs\xyol\swxy\bin\res\char\hero\weapon\0222\08\001\die.png', fmOpenRead);
//  sprBmp := TGameSprBmp.Create(fs);
//  fs.Free;
//  btnTestClick(nil);
//  Exit;
//  SearchFileAll('D:\projs\xyol\tools\MeiFilePacker\Win32\Debug\res\sprite\', '', fileList);
  gameRes := TMeiPack.Create(AppPath + 'res.mpk');
  TGameSprite.GameRes := gameRes;
  CurrentStage := GAMEPLAY;
  TGameAnimation.AniCanvas := Canvas;
  TGameAnimation.OnPaintUI := paintUI;
  TGameAnimation.Matrix := TMatrix.CreateScaling(1, 1);

  gameSpr := TGameSprite.Create(1005);
  FillChar(app, SizeOf(app), 0);
  app._weapon1[0] := 3;
  app._weapon1[1] := 1;
  gameSpr.refAppearance(app);
  gameSpr.X := 800;
  gameSpr.Y := 450;
  AllAniList.Add(gameSpr);
  gameSpr.SprState := osStand;
  gameSpr.Start;
//  gameSpr.GameWeaponId := 1159908;

  ms := TMemoryStream.Create;

  gameUI := TUIScene.Create('GameScene', nil);
  gameUI.FCanvas := Canvas;
  gameUI.FmxEdit := edtUI;
  TGameAnimation.Active := True;
//  edtUI.Font := Canvas.Font;
//  edtUI.OnChange := gameUI.uiEditChange;
//  edtUI.OnKeyUp := gameUI.uiEditKeyUp;
  gameUI.setSize(1600, 900);

  edt := TUIEdit.Create('edtTest', gameUI, 520, 200, 400, 30);
  rvMsg := TUIRichView.Create('rvMsg', gameUI, 600, 350, 400, 200, TAlphaColorRec.White);
  rvMsg.clear;
  rvMsg.defFontSize := 32;
  rvMsg.BgColor := 0; //$B2000000;
  rvMsg.FCanHit := False;


  pnl := TUIPanel.Create('pnlMall', gameUI, 650, 400, True, 'Mall');
  pnl.setPostion(upLeftCenter);
  pnl.show;

//  lst := TUIListbox.Create('lvPropsLeft', pnl, 6, 2, 8, 40, 225, 250);
//  lst.ColWidths[0] := 50;
//  for i := 0 to High(sBltProps) do
//  begin
//    lst.Items.Add(sBltProps[i]);
//    lst.Items.Add(IntToStr(i * 10000));
//  end;
  lbl := TUILabel.Create('', pnl, 'BuyAmt', 410, 200, 100, 30);
  lbl := TUILabel.Create('', pnl, 'GoodsPrice', 410, 240, 100, 30);
  lbl := TUILabel.Create('', pnl, 'NowJinbi', 410, 280, 100, 30);
  edt := TUIEdit.Create('edtMallBuyAmt', pnl, 520, 200, 120, 30);
  lbl := TUILabel.Create('lblMallBuyPrice', pnl, '99', 520, 240, 120, 30, TTextAlign.Leading);
  lbl := TUILabel.Create('lblMallJinbi', pnl, '999999', 520, 280, 120, 30, TTextAlign.Leading);
  btn := TUIButton.Create('btnMallBuy', pnl, 490, 320, 0, 0, 'Buy', 0);
  btn.OnClick := btntestclick;
//  lst := TUIListbox.Create('lstMall', pnl, 5, 2, 5, 40, 400, 320);
//  for i := 1 to 20 do
//    lst.Items.Add('TextTextTextTextText_' + i.ToString);
  lv := TUIListView.Create('lvtMall', pnl, 5, 2, 5, 40, 400, 320);
  for i := 10090 to 10106 do
  begin
    li := TUIListItem.Create(lv);
    li.loadItemBmp(i);
    li.Text := '物品_' + i.ToString;
    li.Value := '元宝：' + i.ToString;
    lv.Items.Add(li);
  end;

//  grid[0].loadItemBmp(12283);
//  TUIListItem(grid[0]).Value := '飞行旗补充包';
//  TUIListItem(grid[0]).Text := '元宝：4';



//  TGameAnimator.AnimateInt(pnl, 'FY', 500, 10.0);


  ms.Free;

//  gameAni.Pause := True;
//  btnStressTestClick(nil);
//  TStrings.Values
//  AnimateEnemy1.Start;
end;

procedure TForm1.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  Caption := Integer(EventInfo.GestureID).ToString;
end;

var
  keyDownList: array[vkLeft..vkDown] of Boolean;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  case CurrentStage of
    GAMEPLAY:
      begin
        case KeyChar of
          'Z', 'z', ' ':
            begin
//              UpDownEvent(Sender);
            end;
          'W', 'w':
            begin
//              UpDownEvent(Sender);
            end;
          'A', 'a':
            begin
//              LeftDownEvent(Sender);
            end;
          'D', 'd':
            begin
//              RightDownEvent(Sender);
            end;
        end;
        case Key of
          vkLeft..vkDown:
            begin
              keyDownList[Key] := True;
              case Key of
                vkRight:
                  begin
                    gameSpr.SprOrientation := 7;
                    if keyDownList[vkDown] then
                      gameSpr.SprOrientation := 0;
                    if keyDownList[vkUp] then
                      gameSpr.SprOrientation := 3;
                  end;
                vkLeft:
                  begin
                    gameSpr.SprOrientation := 5;
                    if keyDownList[vkDown] then
                      gameSpr.SprOrientation := 1;
                    if keyDownList[vkUp] then
                      gameSpr.SprOrientation := 2;
                  end;
                vkUp:
                  begin
                    gameSpr.SprOrientation := 6;
                    if keyDownList[vkLeft] then
                      gameSpr.SprOrientation := 2;
                    if keyDownList[vkRight] then
                      gameSpr.SprOrientation := 3;
                  end;
                vkDown:
                  begin
                    gameSpr.SprOrientation := 4;
                    if keyDownList[vkRight] then
                      gameSpr.SprOrientation := 0;
                    if keyDownList[vkLeft] then
                      gameSpr.SprOrientation := 1;
                  end;
              end;
            end;
        end;
        if Key <> vkHardwareBack then
          Key := 0;
      end;
    GAMEPAD:
      //




  end;

  case Key of
    vkHardwareBack:
      begin
        case CurrentStage of
          MAIN_MENU:
            begin
            // allow default functionality
            end;
          INSTRUCTIONS:
            begin
              Key := 0;
//            ShowMainMenu;
            end;
          GAMEPLAY, LEVEL_COMPLETE:
            begin
              Key := 0;
//            ExitDialog(Sender);
            end;
          GAME_OVER:
            begin
              Key := 0;
//            CleanupGame(False);
//            ShowMainMenu;
            end;
          HIGH_SCORES:
            begin
              Key := 0;
//            CloseHighScores;
            end;
        end;
      end;
  end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
//  gameUI.uiKeyUp(Sender, Key, KeyChar, Shift);
  if Key = 0 then
    Exit;
  case CurrentStage of
    GAMEPLAY:
      begin
        case KeyChar of
          'Z', 'z', ' ':
            begin
//              UpDownEvent(Sender);
            end;
          'W', 'w':
            begin
//              UpDownEvent(Sender);
            end;
          'A', 'a':
            begin
//              LeftDownEvent(Sender);
            end;
          'D', 'd':
            begin
//              RightDownEvent(Sender);
            end;
        end;
        case Key of
          vkLeft..vkDown:
            begin
              keyDownList[Key] := False;
//              case Key of
//                vkRight:
//                  begin
//                    if gameSpr.SprOrientation = 0 then
//                      gameSpr.SprOrientation := 4
//                    else if gameSpr.SprOrientation = 3 then
//                      gameSpr.SprOrientation := 6;
//                  end;
//                vkLeft:
//                  begin
//                    if gameSpr.SprOrientation = 1 then
//                      gameSpr.SprOrientation := 4
//                    else if gameSpr.SprOrientation = 2 then
//                      gameSpr.SprOrientation := 6;
//                  end;
//                vkUp:
//                  begin
//                    if gameSpr.SprOrientation = 2 then
//                      gameSpr.SprOrientation := 5
//                    else if gameSpr.SprOrientation = 3 then
//                      gameSpr.SprOrientation := 7;
//                  end;
//                vkDown:
//                  begin
//                    if gameSpr.SprOrientation = 0 then
//                      gameSpr.SprOrientation := 7
//                    else if gameSpr.SprOrientation = 1 then
//                      gameSpr.SprOrientation := 5;
//                  end;
//              end;
            end;
        end;
        if Key <> vkHardwareBack then
          Key := 0;
      end;
    GAMEPAD:
      //




  end;

  case Key of
    vkHardwareBack:
      begin
        case CurrentStage of
          MAIN_MENU:
            begin
            // allow default functionality
            end;
          INSTRUCTIONS:
            begin
              Key := 0;
//            ShowMainMenu;
            end;
          GAMEPLAY, LEVEL_COMPLETE:
            begin
              Key := 0;
//            ExitDialog(Sender);
            end;
          GAME_OVER:
            begin
              Key := 0;
//            CleanupGame(False);
//            ShowMainMenu;
            end;
          HIGH_SCORES:
            begin
              Key := 0;
//            CloseHighScores;
            end;
        end;
      end;
  end;

end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  x0, y0: Single;
  a: Single;
  d, ideg: Integer;

begin
  x := (X - FOffSet.X) / FDrawScale;
  y := (Y - FOffSet.Y) / FDrawScale;
  gameUI.uiMouseDown(Button, Shift, X, Y);

  x0 := Width / 2;
  y0 := Height / 2;

  Y := Height - Y;
  if x <> x0 then
    a := ArcTan2(Y - y0, X - x0)
  else if y > y0 then
    a := Pi / 2
  else
    a := - Pi / 2;
  a := a + Pi / 4;
  if a < 0 then
    a := Pi * 2 + a;
  ideg := Trunc(a / (Pi * 2 / 360));
  d := Trunc(a / Pi * 4);
  if a > d * Pi / 4 + Pi / 8 then
    Inc(d);
  d := d mod 8;

  Caption := Format('%d : %d', [ideg, d]);
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  x := (X - FOffSet.X) / FDrawScale;
  y := (Y - FOffSet.Y) / FDrawScale;
  gameUI.uiMouseMove(shift, X, Y);
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  x := (X - FOffSet.X) / FDrawScale;
  y := (Y - FOffSet.Y) / FDrawScale;
  gameUI.uiMouseUp(Button, Shift, X, Y);
end;

procedure TForm1.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
//  btnTestClick(nil);
end;

procedure TForm1.FormResize(Sender: TObject);
var
  kx, ky: Single;
begin
  kx := ClientRect.Width / ScreenWidth;
  ky := ClientRect.Height / ScreenHeight;
  if kx <= ky then
  begin
    FOffSet := TPointF.Create(0, ClientRect.Height * (1 - kx / ky) / 2);
    TGameAnimation.Matrix := TMatrix.CreateScaling(kx, kx) * TMatrix.CreateTranslation(FOffSet.X, FOffSet.Y);
    FDrawScale := kx;
  end
  else
  begin
    FOffSet := TPointF.Create(ClientRect.Width * (1 - ky / kx) / 2, 0);
    TGameAnimation.Matrix := TMatrix.CreateScaling(ky, ky) * TMatrix.CreateTranslation(FOffSet.X, FOffSet.Y);
    FDrawScale := ky;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
//
end;

procedure TForm1.gameBtnTestClick(sender: TObject);
begin
  ShowMessage('btnTest click!');
end;

procedure TForm1.imgMapMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
Caption := Format('%f,%f', [x, y]);
end;

procedure TForm1.OnGridClick;
begin
  ShowMessage(gridIndex.ToString);
end;

//procedure TForm1.PaintRects(const UpdateRects: array of TRectF);
//var
//  I, J: Integer;
//  R: TRectF;
//  CallOnPaint, AllowPaint: Boolean;
//  Control: TControl;
//begin
//  btnTest.PaintTo(Canvas, btnTest.ClipRect);
//  if Canvas = nil then
//    Exit;
//
//  begin
//
//        if Controls <> nil then
//          for I := 0 to Controls.Count - 1 do
//            if FControls[I].Visible or FControls[I].ShouldTestMouseHits then
//            begin
//              Control := FControls[I];
//              if Control = FResourceLink then
//              begin
//                if Self.Transparency then
//                  Continue;
//                if Self.Fill.Kind <> TBrushKind.None then
//                  Continue;
//                if (Self.Fill.Kind = TBrushKind.Solid) and (Self.Fill.Color <> Fill.DefaultColor) then
//                  Continue;
//              end;
//              if Control.UpdateRect.IsEmpty then
//                Continue;
//              AllowPaint := False;
//              if Control.InPaintTo then
//                AllowPaint := True;
//              if not AllowPaint then
//              begin
//                R := UnionRect(Control.ChildrenRect, Control.UpdateRect);
//                for J := 0 to High(FUpdateRects) do
//                  if IntersectRect(FUpdateRects[J], R) then
//                  begin
//                    AllowPaint := True;
//                    Break;
//                  end;
//              end;
//              if AllowPaint then
//                TOpenControl(Control).PaintInternal;
//
//              if Control = FResourceLink then
//              begin
//                Canvas.SetMatrix(TMatrix.Identity);
//                DoPaint(Self.Canvas, ClientRect);
//
//                {$IFDEF MSWINDOWS}
//                if (csDesigning in ComponentState) and (Designer <> nil) then
//                begin
//                  Canvas.SetMatrix(TMatrix.Identity);
//                  Designer.PaintGrid;
//                end;
//                {$ENDIF}
//
//                CallOnPaint := True;
//              end;
//            end;
//
//
//  end;
//end;

procedure TForm1.paintUI(Sender: TObject);
begin
  gameUI.draw;
  btnTest.PaintTo(TGameAnimation.AniCanvas, btnTest.BoundsRect);
//  TOpenControl(btnTest).PaintInternal;
end;

procedure TForm1.showMsg(s: string);
begin
  TGameAnimator.StopAnimation(rvMsg, 'Opacity');
  rvMsg.clear;
  addGameText(rvMsg, s);
  rvMsg.Opacity := 1;
  rvMsg.show;
  rvMsg.BringToFront;
  TGameAnimator.AnimateFloatDelay(rvMsg, 'Opacity', 0, 2, 1);
end;

procedure TForm1.tmrMainTimer(Sender: TObject);
begin
  gameUI.uiCheckLongTap;
end;

procedure TForm1.tvMarketClickItem(sender: TObject);
var
  tvi: TUITreeViewItem;
begin
  tvi := TUITreeViewItem(sender);
  ShowMessage(tvi.FData);
end;

var
  fileIdx: Integer;


procedure TForm1.edtFileNameKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
var
  idx, i: Integer;
begin
  if Key <> 13 then
    Exit;
  idx := Max(0, fileIdx);
  for i := idx to fileList.Count - 1 do
    if Pos(edtFileName.Text, fileList[i]) > 0 then
    begin
      fileIdx := i;
      lblFileName.Text := fileList[i];
      Exit;
    end;
  if idx > 0 then
    for i := 0 to idx - 1 do
      if Pos(edtFileName.Text, fileList[i]) > 0 then
      begin
        fileIdx := i;
        lblFileName.Text := fileList[i];
        Exit;
      end;

  ShowMessage('找不到文件！');
end;

procedure TForm1.cbbCharStateChange(Sender: TObject);
begin
  gameSpr.WeaponType := TGameWeaponType(cbbCharState.ItemIndex);
  Focused := nil
end;

procedure TForm1.cbbStateChange(Sender: TObject);
begin
  gameSpr.SprState := TGameObjState(cbbState.ItemIndex);
  Focused := nil
end;

procedure finalObjs;
var
  abp: TPair<Integer, TGameSprBmp>;
  spr: TGameSprite;
begin
  for abp in AllSprBmpList do
    abp.Value.Free;
  AllSprBmpList.Free;
  for spr in AllAniList do
    spr.Free;
  AllAniList.Free;
  fileList.Free;
  gameUI.Free;
  if Assigned(battleUI) then
    battleUI.Free;
  if sprBmp <> nil then
    sprBmp.Free;
  gameRes.Free;
end;

{ TMyEdit }

procedure TMyEdit.DoPaint;
begin
  Form1.Canvas.FillRect(BoundsRect, 1);
end;

{ TBattleObjUI }

procedure TBattleObjUI.addMsg(id: TBattleUIMsgId; srcIdx, dstIdx: Byte;
  value: Integer; text: string);
var
  i: Integer;
begin
  for i := 0 to High(FMsgListData) do
    if FMsgListData[i]._id = bumNone then
    begin
      FmsgList[FmsgCnt] := @FMsgListData[i];
      Break;
    end;

  with FmsgList[FmsgCnt]^ do
  begin
    _srcIdx:= srcIdx;
    _dstIdx := dstIdx;
    _id := id;
    _value := value;
    _text := text;
  end;
  Inc(FmsgCnt);
end;

constructor TBattleObjUI.Create(AOwner: TUIElement);
var
  i, idx, side: Integer;
  x0, x, y, xspace: Integer;
  k: Integer;
  pt0: TPoint;
  spr: TGameBattleSprite;
begin
  FParent := Pointer(AOwner);
  pt0.X := 560;
  pt0.Y := 650;

  x0 := marginSize;
  xspace := (AOwner.Width - marginSize * 2 - MaxSideMember div 2 * imgSize) div (MaxSideMember div 2 - 1);
  for i := 0 to MaxBattleMember - 1 do
  begin
    spr := TGameBattleSprite.Create(0);
    FObjSprites[i] := spr;
    if i < MaxSideMember div 2 then
    begin
      x := pt0.X + i * BtlSlotWidth + FParent.X;
      y := pt0.Y - i * BtlSlotHeight + FParent.Y;
    end else
    if i < MaxSideMember then
    begin
      x := FObjSprites[i - MaxSideMember div 2].FPos0.x + BtlPetXOff;
      y := FObjSprites[i - MaxSideMember div 2].FPos0.y + BtlPetYOff;
    end else
    if i < MaxSideMember + MaxSideMember div 2 then
    begin
      x := FObjSprites[i - MaxSideMember].FPos0.x + BtlSideXOff;
      y := FObjSprites[i - MaxSideMember].FPos0.y + BtlSideYOff;
    end else
    begin
      x := FObjSprites[i - MaxSideMember div 2].FPos0.x + BtlPetXOff1;
      y := FObjSprites[i - MaxSideMember div 2].FPos0.y + BtlPetYOff1;
    end;
    spr.X := x; spr.Y := y;
    spr.FPos0.X := x;
    spr.FPos0.Y := y;
    x := x - FParent.X; y := y - FParent.Y;
    spr.Name := 'btlSpr_' + i.ToString;
    spr.Start;
    //hp条
    spr.rctHpBar := TUIRectangle.Create('bm_hp_'+i.ToString, FParent, x - hpRctWidth div 2,
      y + 3, hpRctWidth, hpRctHeight, TAlphaColorRec.Crimson);
    spr.rctHpBar.hide;
//    FHpBar[i].FDrawType := 1;

    //增益显示
    spr.lblBuff := TUILabel.Create('bm_buf_'+i.ToString, spr.rctHpBar, '增益', 0, 0,
      BtlSlotWidth, imgSize div 2, TTextAlign.Center, buffFontSize, [TFontStyle.fsBold], TAlphaColorRec.Blue);
    spr.lblbuff.Left := -(BtlSlotWidth - hpRctWidth) div 2;
    spr.lblBuff.Top := -imgSize;
    spr.lblBuff.VTextAlign := TTextAlign.Leading;
    spr.lblBuff.hide;

    //减益显示
    spr.lblDebuff := TUILabel.Create('bm_dbuf_'+i.ToString, spr.rctHpBar, '减益', 0, 0,
      BtlSlotWidth, imgSize div 2, TTextAlign.Center, buffFontSize, [TFontStyle.fsBold], TAlphaColorRec.Red);
    spr.lblDebuff.hide;
    spr.lblDebuff.Left := -(BtlSlotWidth - hpRctWidth) div 2;
    spr.lblDebuff.Top := - imgSize div 2;
    spr.lblDebuff.VTextAlign := TTextAlign.Leading;
//    FDebuff[idx].Text := '减益10001'#13'减益10001'#13'减益10001'#13'减益10001'#13;

    //伤害数字
    FDmgY0 := -imgSize div 2;
    FDmgY1 := -imgSize - dmgHeight;
    for k := 0 to High(spr.lblDmgs) do
    begin
      spr.lblDmgs[k] := TUILabel.Create(Format('dmg_%d_%d', [i, k]), spr.rctHpBar, '9999', 0, 0,
        imgSize, dmgHeight, TTextAlign.Center, dmgFontSize, [TFontStyle.fsBold], TAlphaColorRec.White);
      spr.lblDmgs[k].Left := -(imgSize - hpRctWidth) div 2;
      spr.lblDmgs[k].Top := - dmgHeight;
      spr.lblDmgs[k].Visible := False;
    end;

    //动作文字
    spr.lblWords := TUILabel.Create('words_'+i.ToString, spr.rctHpBar, '横扫千军', 0, 0,
      imgSize, imgSize div 2, TTextAlign.Center, dmgFontSize, [TFontStyle.fsBold], TAlphaColorRec.White);
    spr.lblWords.Left := -(imgSize - hpRctWidth) div 2;
    spr.lblWords.Top := -dmgFontSize - imgSize;
    spr.lblWords.Visible := False;
    spr.lblWords.VTextAlign := TTextAlign.Leading;
    spr.lblWords.hide;

    //名称
    spr.lblName := TUILabel.Create('bm_name_'+i.ToString, spr.rctHpBar, '测试名称', 0,
      hpRctHeight + 5, nameWidth, 30, TTextAlign.Center, nameFontSize, [], TAlphaColorRec.Silver);
    spr.lblName.Left := -(nameWidth - hpRctWidth) div 2;
    spr.lblName.hide;
  end;
end;

procedure TBattleObjUI.delMsg(idx, cnt: Integer);
var
  i: Integer;
begin
  if not InRange(idx, 0, FMsgCnt - 1) then
    Exit;
  cnt := Min(FMsgCnt - idx, cnt);
  for i := idx to idx + cnt - 1 do
    FMsgList[i]._id := bumNone;

  Move(FMsgList[idx + cnt], FMsgList[idx], (FMsgCnt - idx - cnt) * SizeOf(Pointer));
  Dec(FMsgCnt, cnt);
end;

destructor TBattleObjUI.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FObjSprites) do
    FObjSprites[i].Free;
  inherited;
end;

procedure TBattleObjUI.clearMsg;
begin
  FMsgCnt := 0;
end;

procedure TBattleObjUI.doMsg;
var
  lb: TUILabel;
  cnt, idx, i: Integer;
  v1, v2, k: Integer;
  s: string;
  x, y: Integer;
  spr: TGameBattleSprite;
  state0: TGameObjState;
  t: Single;
begin
  if FMsgCnt <= 0 then
    Exit;
  if TThread.GetTickCount64 <= FActFinishedTime then
    Exit;
  FActFinishedTime := 0;

  cnt := FMsgCnt;
  for i := 0 to FMsgCnt - 1 do
  begin
    for k := High(FObjSprites) downto 0 do
      if FObjSprites[k].GameGuid = FMsgList[i]._srcIdx then
        Break;
    if k < 0 then
      Continue;
    spr := FObjSprites[k];
    case FMsgList[i]._id of
      bumAction:
        begin
          if FActFinishedTime <> 0 then
          begin
            cnt := i;
            Break;
          end;
          spr.lblWords.StopPropertyAnimation('Opacity');
          spr.lblWords.Visible := True;
          spr.lblWords.Opacity := 1;
          spr.lblWords.Text := fmsglist[i]._text;
          TGameAnimator.AnimateFloatDelay(spr.lblWords, 'Opacity', 0, 0.5, 0.5);


          state0 := spr.SprState;
          if  FMsgList[i]._text = sAttack then
          begin
            v1 := FMsgList[i]._dstIdx;
            for k := 0 to MaxBattleMember - 1 do
              if FObjSprites[k].GameGuid = v1 then
                Break;

            //物理攻击动画
            if k < MaxBattleMember then
            begin
              if k < MaxSideMember then
              begin
                x := BtlAtkXOff; y := BtlAtkYOff;
              end else
              begin
                x := BtlAtkXOff1; y := BtlAtkYOff1;
              end;
              FActDuration := spr.Duration;
              FActStartTime := BtlAtkMoveTime + FActDuration / 2;
              FActOverTime := BtlAtkMoveTime + FActDuration;
              t := BtlAtkMoveTime * 2 + FActDuration;
              TGameAnimator.AnimateIntDelay(spr, 'SprOrientation', spr.SprOrientation, 0.01, t);
              TGameAnimator.AnimateIntDelay(spr, 'AniState', ord(state0), 0.01, t);
              FActFinishedTime := TThread.GetTickCount64 + Trunc(t * 1000);

              spr.moveTo(spr.FPos0.X, spr.fpos0.Y, FActOverTime);
              TGameAnimator.AnimateIntDelay(spr, 'SprOrientation', (spr.SprOrientation + 4) mod 8, 0.01, FActOverTime);

              spr.stateChange(osWalk, FActOverTime);
              spr.stateChange(osAtk, BtlAtkMoveTime);
              spr.orienteTo(IfThen(k < MaxSideMember, 0, 4), BtlAtkMoveTime);

              spr.SprState := osWalk;
              spr.moveTo(FObjSprites[k].X + x, FObjSprites[k].Y + y, 0);
            end;
          end else
          begin
            spr.SprState := osMagic;
            FActOverTime := spr.Duration;
            TGameAnimator.AnimateIntDelay(spr, 'AniState', Ord(state0), 0.01, FActOverTime);
            FActFinishedTime := TThread.GetTickCount64 + Trunc(FActOverTime * 1000);
          end;

        end;
      bumHp:
        begin
          for k := High(spr.lblDmgs) downto 0 do
            if (spr.lblDmgs[k].Top = FDmgY0) then
              Break;
          Inc(k);
          if k > High(spr.lblDmgs) then
            k := 0;

          lb := spr.lblDmgs[k];
          if FMsgList[i]._value > 0  then
            lb.FontColor := TAlphaColorRec.Red
          else
            lb.FontColor := TAlphaColorRec.Green;

          if FMsgList[i]._value > 0 then
          begin
            v1 := FMsgList[i]._value and $FFFFFF;
            v2 := FMsgList[i]._value shr 24;
            case v2 of
              0,1,2,9,10: s := v1.ToString();
              3: s := sDodge;
              4: s := sParry;
              17,18,25,26: s := v1.ToString() + sCrit;
            else
              s := v1.ToString();
            end;
          end else
          begin
            v1 := -FMsgList[i]._value;
            s := v1.ToString();
          end;

          lb.Text := s;
          lb.Top := FDmgY0;
          TGameAnimator.AnimateFloatDelay(lb, 'Opacity', 0, 0.5, 0.5 + FActOverTime,
            TAnimationType.Out, TInterpolationType.Sinusoidal);
          TGameAnimator.AnimateFloatDelay(lb, 'Opacity', 1, 0.01, FActOverTime);
          TGameAnimator.AnimateIntDelay(lb, 'Top', FDmgY1, 0.5, FActOverTime,
            TAnimationType.Out, TInterpolationType.Elastic);

          if s = sDodge then
          begin
            spr.moveTo(spr.FPos0.X, spr.FPos0.Y, FActOverTime, 0.2);
            spr.posChange(IfThen(spr.FBtlSide = 1, -BtlPetXOff div 2, -BtlPetXOff1 div 2),
              IfThen(spr.FBtlSide = 1, -BtlPetYOff div 2, -BtlPetYOff1 div 2), FActStartTime, 0.2);
          end
          else if s = sParry then
          begin
            state0 := spr.SprState;
            spr.stateChange(state0, FActOverTime);
            spr.stateChange(osDefence, FActStartTime);
          end else
          begin
            state0 := spr.SprState;
            spr.stateChange(state0, FActStartTime + spr.Duration);
            spr.stateChange(osHit, FActStartTime);
          end;
//          FObjHeads[idx].AnimateFloat('Top', fheady0[idx div maxsidemember div 2] + 4,
//            0.5, TAnimationType.Out, TInterpolationType.Elastic);
        end;
      bumMp:
        begin
          for k := High(spr.lblDmgs) downto 0 do
            if (spr.lblDmgs[k].Visible) then
              Break;
          Inc(k);
          if k > High(spr.lblDmgs) then
            k := 0;

          lb := spr.lblDmgs[k];
          lb.Visible := True;
          lb.Opacity := 1;
          if FMsgList[i]._value > 0  then
            lb.FontColor := TAlphaColorRec.Purple
          else if FMsgList[i]._value = 0 then
            lb.FontColor := TAlphaColorRec.Blue
          else
            lb.FontColor := TAlphaColorRec.Aqua;

          lb.Text := FMsgList[i]._value.ToString();

          lb.Top := FDmgY0;
//          TGameAnimator.AnimateInt(lb, 'Top', 0, 0.5, TAnimationType.Out, TInterpolationType.Elastic);
          TGameAnimator.AnimateFloatDelay(lb, 'Opacity', 0, 1, 0.5, TAnimationType.Out, TInterpolationType.Sinusoidal);
        end;
      bumBuff:
        spr.lblBuff.Text := fmsglist[i]._text;
      bumDebuff:
        spr.lblDebuff.Text := fmsglist[i]._text;
      bumWors:
        begin
          spr.lblWords.StopPropertyAnimation('Opacity');
          spr.lblWords.Visible := True;
          spr.lblWords.Opacity := 1;
          spr.lblWords.Text := fmsglist[i]._text;
          TGameAnimator.AnimateFloatDelay(spr.lblWords, 'Opacity', 0, 0.5, 0.5);
        end;
      bumDie:
        begin
          spr.lblWords.Text := sDead;
          spr.lblWords.Opacity := 1;
          spr.lblWords.Visible := True;
        end;
      bumOut:
        begin
//          if idx >= MaxSideMember then
//            y := FParent.Height + hpRctSize + 3
//          else
//            y := -imgSize - 3 - nameFontSize;
//
          TGameAnimator.AnimateFloat(spr, 'Opacity', 0, 2);
//          TGameAnimator.AnimateIntDelay(FObjHeads[idx], 'Top', y, 1, 0.5, TAnimationType.Out,
//            TInterpolationType.Bounce);
        end;
    end;
  end;

  delMsg(0, cnt);
end;

procedure TBattleObjUI.show;
begin

end;

{ TGameBattleSprite }

function TGameBattleSprite.getAniState: Integer;
begin
  Result := Ord(SprState);
end;

procedure TGameBattleSprite.moveTo;
begin
  TGameAnimator.AnimateFloatDelay(Self, 'X', x, duration, delay, atype, ainterpolation);
  TGameAnimator.AnimateFloatDelay(Self, 'Y', y, duration, delay, atype, ainterpolation);
end;

procedure TGameBattleSprite.orienteTo(ort: Byte; delay: Single);
begin
  TGameAnimator.AnimateIntDelay(Self, 'SprOrientation', ort, 0.01, delay);
end;

procedure TGameBattleSprite.posChange;
begin
  TGameAnimator.AnimateFloatDelay(Self, 'X', X + dx, duration, delay, atype, ainterpolation);
  TGameAnimator.AnimateFloatDelay(Self, 'Y', Y + dy, duration, delay, atype, ainterpolation);
end;

procedure TGameBattleSprite.SetAniState(const Value: Integer);
begin
  SprState := TGameObjState(Value);
end;

procedure TGameBattleSprite.show;
begin
  Visible := True;
  lblName.show;
  rctHpBar.show;
end;

procedure TGameBattleSprite.stateChange(state: TGameObjState; delay: Single);
begin
  TGameAnimator.AnimateIntDelay(Self, 'AniState', ord(state), 0.01, delay);
end;

initialization
  AllSprBmpList := TDictionary<Integer, TGameSprBmp>.Create;
  AllAniList := TList<TGameSprite>.Create;
  fileList := TStringList.Create;


finalization
  finalObjs;
end.

